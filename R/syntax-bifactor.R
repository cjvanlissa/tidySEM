
# TODO: remove this to do proper importing of functions. Don't rely on tidyverse
library(tidyverse)

#' @title Generate syntax for a bifactor model
#' @description Generate syntax for a bifactor model for latent variables.
#' @param x An object for which a method exists, including \code{tidy_sem}
#' (generated using \code{\link[tidySEM]{dictionary}}, or \code{data.frame} (for
#' which \code{\link[tidySEM]{dictionary}} will be run first).
#' @param ... Additional parameters passed to \code{\link{measurement}}.
#' @return An object of class \code{tidy_sem_bifactor} and \code{tidy_sem} which can be passed to \code{\link{run_bifactor}}.
#' @examples
#' dict <- tidy_sem(c("a_1", "a_2", "a_3", "b_4", "b_5"))
#' bifactor(dict)
#' @rdname bifactor
#' @export
bifactor <- function(x, ...) {
  UseMethod("bifactor")
}

#' @method bifactor tidy_sem
#' @export
bifactor.tidy_sem <- function(x, ...) {
  x$dictionary %>% filter(type == "observed") %>% pull(name) -> ids

  if ("G" %in% ids) {
    stop("Illegal factor name: G. G is a reserved name representing the general factor.")
  }

  x$dictionary <- rbind(x$dictionary, data.frame(
    name = ids,
    scale = "G",
    type = "observed",
    label = ids
  ))

  m <- measurement(x,
                   std.lv = T,
                   auto.fix.single = T,
                   auto.fix.first = F,
                   orthogonal = T,
                   int.ov.free = F,
                   int.lv.free = F,
                   meanstructure = F,
                    ...
                   )

  # auto.fix.single doesn't work because each item loads on the general factor and (at least) another latent variable. Doing it manual here in two steps
  # Step 1
  m$dictionary %>% group_by(scale) %>% mutate(n = n()) %>% ungroup() %>% filter(n == 1, type == "indicator") -> singles

  # Step 2
  # Can be out of the sapply loop I guess, haven't checked all edge cases so I am using this safer version:
  for(i in seq_len(nrow(singles))) {
    m$syntax[m$syntax$lhs == singles$name[[i]] & m$syntax$op == "~~" & m$syntax$rhs == singles$name[[i]],c("free", "ustart")] <- 0
  }

  class(m) <- c("tidy_sem_bifactor", class(m))

  m
}

#' @title Run a bifactor model
#' @description Run a bifactor model using lavaan.
#' This function relies on \code{\link{run_lavaan}} to run the model.
#' @param x An object of class  \code{tidy_sem_bifactor}
#' (generated using \code{\link[tidySEM]{bifactor}}).
#' @param total_variance whether to compute total variances with the
#' model implied or the observed correlation matrix.
#' Default: "modelled"
#' @param ... Additional parameters passed to \code{\link{run_lavaan}}.
#' @return A list containing a \code{lavaan} fit object (\code{fit}) and omega
#' statistics (\code{omega}, \code{omega_subfactor},
#' \code{omegah}, \code{omegah_subfactor}).
#' @references
#' 10.1080/00223891.2015.1089249
#' 10.1080/00223891.2015.1117928
#' @rdname run_bifactor
#' @export
run_bifactor <- function(x,
                         total_variance = c("modelled", "observed"),
                         ...) {
  UseMethod("run_bifactor")
}

#' @method run_bifactor tidy_sem_bifactor
#' @export
run_bifactor.tidy_sem_bifactor <- function(x,
                                           total_variance_calculation = c("modelled", "observed"),
                                           ...) {
  total_variance_calculation <- match.arg(total_variance_calculation,
                              choices = c("modelled", "observed"),
                              several.ok = F)

  f <- run_lavaan(x, ...)
  p <- lavaan::standardizedSolution(f, type = "std.all") %>%
    left_join(x$syntax, by = c("lhs", "op", "rhs"))

  subfactors <- x$dictionary %>%
    filter(type == "latent", name != "G") %>%
    pull(name)

  subfactors_dictionary <- x$dictionary %>%
    filter(type == "indicator", name != "G", scale != "G")

  factor_loading_variances <- p %>%
    filter(op == "=~") %>%
    mutate(est.std = abs(est.std)) %>%
    group_by(lhs) %>%
    summarise(variance = sum(est.std)^2) %>%
    rename(latent = lhs) %>% {
      setNames(pull(., variance), pull(., latent))
    }

  subfactor_G_loading_variances <- p %>%
    filter(op == "=~", lhs == "G") %>%
    left_join(subfactors_dictionary,
              by = c("rhs" = "name"),
              suffix = c("", "_subfactors")) %>%
    mutate(est.std = abs(est.std)) %>%
    group_by(scale) %>%
    summarise(variance = sum(est.std)^2) %>%
    rename(subfactor = scale) %>% {
      setNames(pull(., variance), pull(., subfactor))
    }

  item_error_variances_by_subfactor <- p %>%
    filter(op == "~~", lhs == rhs, lhs %in% subfactors_dictionary$name) %>%
    left_join(subfactors_dictionary,
              by = c("rhs" = "name"),
              suffix = c("", "_subfactors")) %>%
    mutate(est.std = abs(est.std)) %>%
    group_by(scale) %>%
    # No squaring because they are already variances
    summarise(variance = sum(est.std)) %>%
    rename(subfactor = scale) %>% {
      setNames(pull(., variance), pull(., subfactor))
    }

  explained_variance <- sum(factor_loading_variances)

  error_variance <- sum(item_error_variances_by_subfactor)

  # This total variance depends slightly on model fit.
  # A more conservative way is to base it on the cor matrix, see below
  # lavaan::lavInspect(f, "cor.ov") %>% sum()
  if (total_variance_calculation == "modelled") {
    total_variance <- explained_variance + error_variance
  } else {
    # TODO: This breaks with missing values...
    # does lavaan give the cor matrix with missing values imputed ?
    total_variance <- lavaan::lavInspect(f, "data") %>% cor() %>% sum()
  }

  if (total_variance_calculation == "modelled") {
    total_variance_subfactor <- sapply(X = subfactors, FUN = function(sf) {
      vg <- subfactor_G_loading_variances[sf]
      vs <- factor_loading_variances[sf]
      ve <- item_error_variances_by_subfactor[sf]

      vg + vs + ve
    }) %>% setNames(subfactors)

    ## We could also do:
    # r <- lavaan::lavInspect(f, "cor.ov") # Model implied correlation matrix
    # total_variance_subfactor <- sapply(X = subfactors, FUN = function(sf) {
    #   sf_indicators <- subfactors_dictionary %>% filter(scale == sf) %>%
    #     pull(name)
    #   sum(r[sf_indicators, sf_indicators])
    # }) %>% setNames(subfactors)
  } else {
    r <- lavaan::lavInspect(f, "data") %>% cor()
    total_variance_subfactor <- sapply(X = subfactors, FUN = function(sf) {
      sf_indicators <- subfactors_dictionary %>% filter(scale == sf) %>%
        pull(name)
      sum(r[sf_indicators, sf_indicators])
    }) %>% setNames(subfactors)
  }



  # For formulas see this publication and its corrections:
  #

  omega_G <- (explained_variance / total_variance) %>% setNames("G")

  omega_subfactor <- sapply(X = subfactors, FUN = function(sf) {
    vg <- subfactor_G_loading_variances[sf]
    vs <- factor_loading_variances[sf]
    ve <- item_error_variances_by_subfactor[sf]

    (vg + vs) / total_variance_subfactor[sf]
  }) %>% setNames(subfactors)

  omegah_G <- (factor_loading_variances[["G"]] / total_variance) %>% setNames("G")

  omegah_subfactor <- sapply(X = subfactors, FUN = function(sf) {
    vg <- subfactor_G_loading_variances[sf]
    vs <- factor_loading_variances[sf]
    ve <- item_error_variances_by_subfactor[sf]

    (vs) / total_variance_subfactor[sf]
  }) %>% setNames(subfactors)

  # The interpretability of this one is disputed and not returning it.
  # Can be informative if residual error in a set of items is high.
  # Or if a single item loads on a subfactor.
  omegah_group_subfactor <- sapply(X = subfactors, FUN = function(sf) {
    vg <- subfactor_G_loading_variances[sf]
    vs <- factor_loading_variances[sf]

    (vs) / (vg + vs)
  }) %>% setNames(subfactors)


  list(
    lavaan_fit = f,
    omegas = list(
      omega = omega_G,
      omega_subfactor = omega_subfactor,
      omegah = omegah_G,
      omegah_subfactor = omegah_subfactor,
      omegah_group_subfactor = omegah_group_subfactor
    )
  )

}

