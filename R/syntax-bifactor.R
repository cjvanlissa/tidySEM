
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
#' @note Currently does not support cross loadings.
#' @rdname bifactor
#' @export
bifactor <- function(x, ...) {
  UseMethod("bifactor")
}

#' @method bifactor tidy_sem
#' @export
bifactor.tidy_sem <- function(x, ...) {

  # Pull the indicator names from the dictionary of `x`
  ## Tidyverse code:
  ### x$dictionary %>% filter(type == "observed") %>% pull(name) -> ids
  with(x, {
    dictionary[dictionary$type == "observed","name"]
  }) -> ids

  ## G is not an allowed scale name
  with(x, {
    dictionary[dictionary$name == "G" | dictionary$scale == "G",]
  }) -> Gs

  if (nrow(Gs) > 0) {
    stop("Illegal indicator or factor name: 'G'. G is a reserved name representing the general factor.")
  }

  ## Add latent variable G to the dictionary such that
  ## the `measurement` function picks up on it.
  xG <- x
  xG$dictionary <- rbind(data.frame(
    name = ids,
    scale = "G",
    type = "observed",
    label = ids
  ), xG$dictionary)

  ## Measurement function picks up on the new G latent variable.
  ## However, it does not pick up on auto.fix.single because the x$dictionary
  ## has the G factor added, leading to two latent factors per observed variable
  m <- measurement(xG,
                   std.lv = T,
                   auto.fix.single = T,
                   auto.fix.first = F,
                   orthogonal = T,
                   int.ov.free = F,
                   int.lv.free = F,
                   meanstructure = F,
                    ...
                   )

  ## Because auto.fix.single doesn't work because each item loads on the general factor
  ## and (at least) another latent variable. I am fixing it manually here in two steps
  ### Step 1: find the factors that only have one indicator variable
  ### Tidyverse code:
  ### m$dictionary %>% group_by(scale) %>% mutate(n = n()) %>% ungroup() %>% filter(n == 1, type == "indicator") -> singles
  with(xG, {
    table(dictionary$scale) -> scales_tabs
    dictionary[scales_tabs[dictionary$scale] == 1,]
  }) -> singles

  ### Step 2: set the error variance of these items to 0
  m_mod <- m
  within(m, {
    singles_covs <- with(syntax, lhs == singles$name & op == "~~" & rhs == singles$name)
    syntax[singles_covs, c("free", "ustart")] <- 0
    rm(singles_covs)
  }) -> m_mod

  ### This class is used by run_bifactor
  class(m_mod) <- c("tidy_sem_bifactor", class(m_mod))

  m_mod
}

#' @title Run a bifactor model
#' @description Run a bifactor model using lavaan.
#' This function relies on \code{\link{run_lavaan}} to run the model.
#' @param x An object of class  \code{tidy_sem_bifactor}
#' (generated using \code{\link[tidySEM]{bifactor}}).
#' @param total_variance_calculation whether to compute total variances with the
#' model implied or the observed correlation matrix.
#' Default: "observed"
#' @param standardization_method whether to base omega on the completely standardized
#' solution or the solution in which latent variables are standardized (see lavaan)
#' Default: "std.all"
#' @param ... Additional parameters passed to \code{\link{run_lavaan}}.
#' @return A list containing a \code{lavaan} fit object (\code{fit}) and omega
#' statistics (\code{omega}, \code{omega_subfactor},
#' \code{omegah}, \code{omegah_subfactor}).
#' @references
#' 10.1080/00223891.2015.1089249
#' 10.1080/00223891.2015.1117928
#' @note If setting total_variance_calculation to "observed" and standardization_method
#' to "std.all", there is a
#' discrepancy in results with psych::omegaFromSem if you do not feed
#' lavaan a correlation matrix, because omegaFromSem extracts unstandardized
#' loadings when computing omega, and does not take into
#' account that loadings can exceed 1.0 if the model is
#' based on anything else than a
#' correlation matrix. The current implementation avoids this issue by using
#' the covariance matrix when standardization_method is "std.lv".
#'
#' @rdname run_bifactor
#' @export
run_bifactor <- function(x,
                         total_variance_calculation = c("observed", "modelled"),
                         standardization_method = c("std.all", "std.lv"),
                         ...) {
  UseMethod("run_bifactor")
}

#' @method run_bifactor tidy_sem_bifactor
#' @export
run_bifactor.tidy_sem_bifactor <- function(x,
                                           total_variance_calculation = c("observed", "modelled"),
                                           standardization_method = c("std.all", "std.lv"),
                                           ...) {

  # A discrepancy in results can occur if model fit is not adequate (unmodelled error variance).
  # It is more conservative to use the observed variance for omega calculation.
  total_variance_calculation <- match.arg(total_variance_calculation,
                              choices = c("observed", "modelled"),
                              several.ok = F)

  # A discrepancy in results compared to psych::omegaFromSem occurs for "std.all".
  # I think std.all is still more appropriate given the computation formulas used.
  # Note this is not important for when the total variance calculation is based on the modelled variance.
  standardization_method <- match.arg(standardization_method,
                                          choices = c("std.all", "std.lv"),
                                          several.ok = F)

  # Run the lavaan model. TODO: check as_lavaan and as_ram output
  f <- run_lavaan(x, ...)

  list(
    lavaan_fit = f,
    omega = compute_omega(x,
                          f,
                          total_variance_calculation = total_variance_calculation,
                          standardization_method = standardization_method)
  )

}

# Computes omega given an appropriate lavaan fit object and a tidysem object
compute_omega <- function(x,
                          f,
                          total_variance_calculation = c("observed", "modelled"),
                          standardization_method = c("std.all", "std.lv")) {

  # A discrepancy in results can occur if model fit is not adequate (unmodelled error variance).
  # It is more conservative to use the observed variance for omega calculation.
  total_variance_calculation <- match.arg(total_variance_calculation,
                                          choices = c("observed", "modelled"),
                                          several.ok = F)

  # A discrepancy in results compared to psych::omegaFromSem occurs for "std.all".
  # I think std.all is still more appropriate given the computation formulas used.
  # Note this is not important for when the total variance calculation is based on the modelled variance.
  standardization_method <- match.arg(standardization_method,
                                      choices = c("std.all", "std.lv"),
                                      several.ok = F)

  # List the parameter estimates
  ## I use std.lv here because we restrict the variances of the latent variables to be 1
  p <- lavaan::standardizedSolution(f, type = standardization_method) %>%
    left_join(x$syntax, by = c("lhs", "op", "rhs"))

  # Store the subfactors in a lookup table
  subfactors <- x$dictionary %>%
    filter(type == "latent", name != "G") %>%
    pull(name)

  # Store a dictionary of the subfactor indicators without the G factor
  subfactors_dictionary <- x$dictionary %>%
    filter(type == "indicator", name != "G", scale != "G")

  # Compute the variances explained by each factor by summing and squaring loadings
  factor_loading_variances <- p %>%
    filter(op == "=~") %>%
    mutate(est.std = abs(est.std)) %>%
    group_by(lhs) %>%
    summarise(variance = sum(est.std)^2) %>%
    rename(latent = lhs) %>% {
      setNames(pull(., variance), pull(., latent))
    }

  # Compute the variances explained by the general factor for each subfactor
  # by summing and squaring loadings
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

  # Compute the error variances for each subfactor by summing error variances
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

  # The explained and total variances depend on model fit.
  # A more conservative way is to base it on the observed cor matrix, see below
  if (total_variance_calculation == "modelled") {
    explained_variance <- sum(factor_loading_variances)

    error_variance <- sum(item_error_variances_by_subfactor)

    total_variance <- explained_variance + error_variance

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
    if (standardization_method == "std.lv") {
      r <- lavInspect(f, "sampstat")$cov
    } else {
      # std.all, so we use a correlation matrix.
      # Note: I have no idea why psych::omegaFromSem uses "std.lv" or
      # even unstandardized indicator parameters and still does a cov2cor...
      r <- cov2cor(lavInspect(f, "sampstat")$cov)
    }

    total_variance <- sum(r)

    error_variance <- p %>%
      filter(op == "=~") %>%
      mutate(est.std = abs(est.std)) %>%
      group_by(rhs) %>%
      summarise(variance = sum(est.std^2)) %>%
      rename(indicator = rhs) %>% {
        setNames(pull(., variance), pull(., indicator))
      } %>% sum() %>% {tr(r) - .}

    explained_variance <- total_variance - error_variance

    total_variance_subfactor <- sapply(X = subfactors, FUN = function(sf) {
      sf_indicators <- subfactors_dictionary %>% filter(scale == sf) %>%
        pull(name)
      sum(r[sf_indicators, sf_indicators])
    }) %>% setNames(subfactors)
  }

  # For formulas see references of this function

  omega_G <- (explained_variance / total_variance) %>% setNames("G")

  omegaS_subfactor <- sapply(X = subfactors, FUN = function(sf) {
    vg <- subfactor_G_loading_variances[sf]
    vs <- factor_loading_variances[sf]
    ve <- item_error_variances_by_subfactor[sf]

    (vg + vs) / total_variance_subfactor[sf]
  }) %>% setNames(subfactors)

  omegaH_G <- (factor_loading_variances[["G"]] / total_variance) %>% setNames("G")

  omegaH_subfactor <- sapply(X = subfactors, FUN = function(sf) {
    vg <- subfactor_G_loading_variances[sf]
    vs <- factor_loading_variances[sf]
    ve <- item_error_variances_by_subfactor[sf]

    (vg) / total_variance_subfactor[sf]
  }) %>% setNames(subfactors)

  omegaHS_G <- (sum(factor_loading_variances[-which(names(factor_loading_variances) == "G")]) / total_variance) %>% setNames("G")

  omegaHS_subfactor <- sapply(X = subfactors, FUN = function(sf) {
    vg <- subfactor_G_loading_variances[sf]
    vs <- factor_loading_variances[sf]
    ve <- item_error_variances_by_subfactor[sf]

    (vs) / total_variance_subfactor[sf]
  }) %>% setNames(subfactors)

  ecv_G <- (factor_loading_variances[["G"]] / explained_variance) %>% setNames("G")
  ecv_subfactors <- sapply(X = subfactors, FUN = function(sf) {
    vg <- subfactor_G_loading_variances[sf]
    vs <- factor_loading_variances[sf]

    (vg) / (vg + vs)
  }) %>% setNames(subfactors)

  o_df <- data.frame(
    omega = c(omega_G, omegaS_subfactor),
    omegaH = c(omegaH_G, omegaH_subfactor),
    omegaHS = c(omegaHS_G, omegaHS_subfactor),
    ECV = c(ecv_G, ecv_subfactors),

    row.names = c("G", subfactors),
    stringsAsFactors = F
  )

  attr(o_df$omega, "label") <- "For G: the proportion of variance in all the items that is explained by the general factor and subfactors (explained variance). For subfactors: the proportion of variance in the subfactor's items explained by the general factor (the items loadings on the general factor) and subfactors (loadings of the items on the subfactor)."
  attr(o_df$omegaH, "label") <- "For G: The proportion of variance in all items that is explained by the general factor. For subfactors: The proportion of variance in subfactor's items that is explained by the general factor."
  attr(o_df$omegaHS, "label") <- "For G: the proportion of the variance in all items that is explained by the subfactors. For subfactors: The proportion of the variance in subfactor's items that is explained by the subfactor."
  attr(o_df$ECV, "label") <- "The proportion of explained variance in items (i.e. excluding error variance) explained by the general factor."

  o_df
}

