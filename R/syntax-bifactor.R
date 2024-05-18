

#' @title Generate syntax for a bifactor model
#' @description Generate syntax for a bifactor model for latent variables.
#' @param x An object for which a method exists, including \code{tidy_sem}
#' (generated using \code{\link[tidySEM]{dictionary}}, or \code{data.frame} (for
#' which \code{\link[tidySEM]{dictionary}} will be run first).
#' @param G The name to use for the general factor. Default: "G".
#' @param ... Additional parameters passed to \code{\link{measurement}}.
#' @return An object of class \code{tidy_sem_bifactor} and \code{tidy_sem} which can be passed to \code{\link{run_bifactor}}.
#' @examples
#' dict <- tidy_sem(c("a_1", "a_2", "a_3", "b_4", "b_5"))
#' bifactor(dict)
#' @note Currently does not support cross loadings.
#' @rdname bifactor
#' @export
bifactor <- function(x,
                     generalFactorName = "G", ...) {
  UseMethod("bifactor")
}

#' @method bifactor tidy_sem
#' @export
bifactor.tidy_sem <- function(x,
                              generalFactorName = "G", ...) {

  if (anyNA(names(x$data))) {
    stop("Some variable names in x$data are missing")
  }

  if (anyNA(x$dictionary$name)) {
    stop("Some variables in tidy_sem model or data don't load on any factor")
  }

  lav_syntax <- c()

  single_indicators <- c()


  # G
  synt <- paste0(generalFactorName, "=~", x$dictionary$name)
  lav_syntax <- c(lav_syntax, synt)

  # Subscales
  for(scale in unique(c(x$dictionary$scale))) {
    sub_dict <- with(x, {
      dictionary[dictionary$scale == scale,]
    })

    synt <- paste0(sub_dict$scale, "=~", sub_dict$name)

    if (nrow(sub_dict) < 3) {
      if (nrow(sub_dict) < 2) {
        # Fix error to zero, auto.fix.single doesn't do the trick because of G
        # Do this later
        single_indicators <- c(single_indicators, sub_dict$name)
      } else {
        # Fix loadings to equality
        loading_name <- paste0(scale, "_eq")
        synt <- paste0(sub_dict$scale, "=~", loading_name, "*", sub_dict$name)

      }
    } else {

    }


    lav_syntax <- c(lav_syntax, synt)
  }



  syntax <- add_paths(NULL, lav_syntax,
                      std.lv = T,
                      auto.fix.single = T,
                      auto.fix.first = F,
                      orthogonal = T,
                      int.ov.free = F,
                      int.lv.free = F,
                      meanstructure = T)

  ## Because auto.fix.single doesn't work because each item loads on the general factor
  ## and (at least) another latent variable. I am fixing it (to zero) manually here
  single_rows <- with(syntax, lhs %in% single_indicators & op == "~~" & rhs %in% single_indicators)
  syntax[single_rows, c("free", "ustart")] <- 0

  x$syntax <- syntax

  x
}


#' @title Compute omega
#' @description Compute omega based on a model fit object
#' @param f A model fit object
#' (fit object generated using \code{\link[tidySEM]{bifactor}}).
#' @param total_variance_calculation whether to compute total variances with the
#' model implied or the observed correlation matrix.
#' Default: "observed"
#' @param standardization_method whether to base omega on the completely standardized
#' solution or the solution in which latent variables are standardized (see lavaan)
#' Default: "std.all"
#' @return A dataframe containing omega statistics for the general factor
#' and the subfactors
#' (\code{omega} and \code{omegaS}, \code{omegaH} and \code{omegaHS}, and \code{ECV}).
#' @references
#' 10.1080/00223891.2015.1089249
#' 10.1080/00223891.2015.1117928

#'
#' @rdname omega
#' @export
omega <- function(f,
                  total_variance_calculation = c("observed", "modelled"),
                  standardization_method = c("std.all", "std.lv"),
                  generalFactorName = "G") {
  UseMethod("omega", f)
}

#' @param parameters A data.frame containing columns 'lhs', 'op', 'rhs', and an 'estimate'
#' @param covariance_matrix The covariance matrix
#' @method omega default
#' @export
omega.default <- function(parameters,
                          covariance_matrix,
                          total_variance_calculation = c("observed", "modelled"),
                          generalFactorName = generalFactorName) {

  # A discrepancy in results can occur if model fit is not adequate (unmodelled error variance).
  # It is more conservative to use the observed variance for omega calculation.
  total_variance_calculation <- match.arg(total_variance_calculation,
                                          choices = c("observed", "modelled"),
                                          several.ok = F)

  subfactors <- with(parameters, {
    sort(unique(lhs[op == "=~" & lhs != generalFactorName]))
  })

  p_complete <- parameters

  if (total_variance_calculation == "modelled") {
    p <- parameters



  } else {

    # This logic is required because when cross loadings are modelled,
    # they do count towards error variance of an item (or all items for a subfactor)
    # but they only count towards the variance of one subfactor.
    # This stuff shows a downside of basing the computation on
    # observed covariances in items.
    # source: psych function implementation

    subfactor_loadings <- with(parameters, {
      parameters[op == "=~" & lhs != generalFactorName,]
    })
    subfactor_loadings$estimate <- abs(subfactor_loadings$estimate)
    subfactor_loadings$is_max <- sapply(1:nrow(subfactor_loadings), function(i){
      subfactor_loadings$estimate[i] == max(with(subfactor_loadings, {estimate[rhs == rhs[i]]}))
    })

    subfactor_loadings$key = with(subfactor_loadings, paste0(lhs, op, rhs))

    pa <- parameters
    pa$key <- with(pa, paste0(lhs, op, rhs))

    pa$is_max <- subfactor_loadings[match(pa$key, subfactor_loadings$key),]$is_max

    p <- pa[is.na(pa$is_max) | pa$is_max,]

  }

  r <- covariance_matrix

  # Store a dictionary of the subfactor indicators without the G factor
  subfactors_dictionary <- p[p$op == "=~" & p$lhs != generalFactorName,c("lhs", "rhs")]
  names(subfactors_dictionary) <- c("scale", "name")

  # Compute the variances explained by each factor by summing and squaring loadings
  p_loadings_abs <- within(p[p$op == "=~",], {
    estimate <- abs(estimate)
  })

  factor_loading_variances <- sapply(unique(p_loadings_abs$lhs), function(l) {
    l_est <- with(p_loadings_abs, {
      estimate[lhs == l]
    })
    sum(l_est)^2
  })



  # Compute the variances explained by the general factor for each subfactor
  # by summing and squaring loadings
  p_loadings_general_abs <- within(p[p$op == "=~" & p$lhs == generalFactorName,], {
    estimate <- abs(estimate)
  })

  lookup <- match(p_loadings_general_abs$rhs, table = subfactors_dictionary$name)
  p_loadings_general_abs_scale <- cbind(p_loadings_general_abs, scale = subfactors_dictionary[lookup,"scale"])

  subfactor_G_loading_variances <- sapply(unique(p_loadings_general_abs_scale$scale), function(l) {
    l_est <- with(p_loadings_general_abs_scale, {
      estimate[scale == l]
    })
    sum(l_est)^2
  })


  # Compute the error variances for each subfactor by summing error variances
  p_error <- within(p[p$op == "~~" & p$lhs == p$rhs & p$lhs %in% subfactors_dictionary$name,], {
    estimate <- abs(estimate)
  })

  lookup <- match(p_error$rhs, table = subfactors_dictionary$name)
  p_error_scale <- cbind(p_error, scale = subfactors_dictionary[lookup,"scale"])

  item_error_variances_by_subfactor <- sapply(unique(p_error_scale$scale), function(l) {
    l_est <- with(p_error_scale, {
      estimate[scale == l]
    })
    sum(l_est)
  })

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
    })
    names(total_variance_subfactor) <- subfactors

    ## We could also do:
    # r <- lavaan::lavInspect(f, "cor.ov") # Model implied correlation matrix
    # total_variance_subfactor <- sapply(X = subfactors, FUN = function(sf) {
    #   sf_indicators <- subfactors_dictionary %>% filter(scale == sf) %>%
    #     pull(name)
    #   sum(r[sf_indicators, sf_indicators])
    # }) %>% setNames(subfactors)

  } else {

    total_variance <- sum(r)

    # Note the usage of p_complete here to include cross loadings
    p_complete_loadings_abs <- within(p_complete[p_complete$op == "=~",], {
      estimate <- abs(estimate)
    })

    error_variance_indicators <- sapply(unique(p_complete_loadings_abs$rhs), function(l) {
      l_est <- with(p_complete_loadings_abs, {
        estimate[rhs == l]^2
      })
      sum(l_est)
    })

    error_variance <- tr(r) - sum(error_variance_indicators)

    explained_variance <- total_variance - error_variance

    # This code requires cross loadings to be excluded from the subfactors_dictionary
    # or else we are counting the variance of that item twice
    total_variance_subfactor <- sapply(X = subfactors, FUN = function(sf) {
      sf_indicators <- with(subfactors_dictionary, {
        name[scale == sf]
      })

      sum(r[sf_indicators, sf_indicators])
    })

  }

  # For formulas see references of this function

  omega_G <- (explained_variance / total_variance)
  names(omega_G) <- generalFactorName

  omegaS_subfactor <- sapply(X = subfactors, FUN = function(sf) {
    vg <- subfactor_G_loading_variances[sf]
    vs <- factor_loading_variances[sf]
    ve <- item_error_variances_by_subfactor[sf]

    (vg + vs) / total_variance_subfactor[sf]
  })
  names(omegaS_subfactor) <- subfactors

  omegaH_G <- (factor_loading_variances[[generalFactorName]] / total_variance)
  names(omegaH_G) <- generalFactorName

  omegaH_subfactor <- sapply(X = subfactors, FUN = function(sf) {
    vg <- subfactor_G_loading_variances[sf]
    vs <- factor_loading_variances[sf]
    ve <- item_error_variances_by_subfactor[sf]

    (vg) / total_variance_subfactor[sf]
  })
  names(omegaH_subfactor) <- subfactors

  omegaHS_G <- (sum(factor_loading_variances[-which(names(factor_loading_variances) == generalFactorName)]) / total_variance)
  names(omegaHS_G) <- generalFactorName

  omegaHS_subfactor <- sapply(X = subfactors, FUN = function(sf) {
    vg <- subfactor_G_loading_variances[sf]
    vs <- factor_loading_variances[sf]
    ve <- item_error_variances_by_subfactor[sf]

    (vs) / total_variance_subfactor[sf]
  })
  names(omegaHS_subfactor) <- subfactors

  ecv_G <- (factor_loading_variances[[generalFactorName]] / explained_variance)
  names(ecv_G) <- generalFactorName

  ecv_subfactors <- sapply(X = subfactors, FUN = function(sf) {
    vg <- subfactor_G_loading_variances[sf]
    vs <- factor_loading_variances[sf]

    (vg) / (vg + vs)
  })
  names(ecv_subfactors) <- subfactors

  o_df <- data.frame(
    omega = c(omega_G, omegaS_subfactor),
    omegaH = c(omegaH_G, omegaH_subfactor),
    omegaHS = c(omegaHS_G, omegaHS_subfactor),
    ECV = c(ecv_G, ecv_subfactors),

    row.names = c(generalFactorName, subfactors),
    stringsAsFactors = F
  )

  attr(o_df$omega, "label") <- "For G: the proportion of variance in all the items that is explained by the general factor and subfactors (explained variance). For subfactors: the proportion of variance in the subfactor's items explained by the general factor (the items loadings on the general factor) and subfactors (loadings of the items on the subfactor)."
  attr(o_df$omegaH, "label") <- "For G: The proportion of variance in all items that is explained by the general factor. For subfactors: The proportion of variance in subfactor's items that is explained by the general factor."
  attr(o_df$omegaHS, "label") <- "For G: the proportion of the variance in all items that is explained by the subfactors. For subfactors: The proportion of the variance in subfactor's items that is explained by the subfactor."
  attr(o_df$ECV, "label") <- "The proportion of explained variance in items (i.e. excluding error variance) explained by the general factor."

  o_df

}

#' @title Compute omega (lavaan)
#' @description Compute omega based on a lavaan model fit object
#' @note If setting total_variance_calculation to "observed" and standardization_method
#' to "std.all", there is a
#' discrepancy in results with psych::omegaFromSem if you do not feed
#' \code{lavaan()} a correlation matrix, because omegaFromSem extracts unstandardized
#' loadings when computing omega and uses the correlation matrix for variance calculations.
#'
#' psych::omegaFromSem also does not take into
#' account that loadings can exceed 1.0 if the model is
#' based on anything else than a correlation matrix.
#' The current implementation avoids this issue by using
#' the covariance matrix when standardization_method is "std.lv".
#'
#' Also note that standardization_method "psych" was implemented to produce identical results
#' to psych::omegaFromSem (when lavaan was given a correlation matrix)
#' @method omega lavaan
#' @export
omega.lavaan <- function(f,
                  total_variance_calculation = c("observed", "modelled"),
                  standardization_method = c("std.all", "std.lv", "psych"),
                  generalFactorName = "G") {

  # A discrepancy in results can occur if model fit is not adequate (unmodelled error variance).
  # It is more conservative to use the observed variance for omega calculation.
  total_variance_calculation <- match.arg(total_variance_calculation,
                                          choices = c("observed", "modelled"),
                                          several.ok = F)

  # A discrepancy in results compared to psych::omegaFromSem occurs for "std.all".
  # I think std.all is still more appropriate given the computation formulas used.
  # Note this is not important for when the total variance calculation is based on the modelled variance.
  standardization_method <- match.arg(standardization_method,
                                      choices = c("std.all", "std.lv", "psych"),
                                      several.ok = F)



  if (total_variance_calculation == "observed") {
    # std.lv is valid with a covariance matrix because the model standardized latent variable variances.
    # needs testing with real data
    if (standardization_method == "std.lv") {
      # This option primarily exists to compare results with other packages.

      # List the parameter estimates
      p <- lavaan::standardizedSolution(f, type = standardization_method)[,c("lhs", "op", "rhs", "est.std")]
      colnames(p) <- c("lhs", "op", "rhs", "estimate")

      # This cov matrix is under the assumption that f was built with a correlation matrix!
      # This assumption is in place because bifactor standardizes latent variances to 1
      r <- lavInspect(f, "sampstat")$cov
    } else if(standardization_method == "psych") {
      # List the parameter estimates
      ## I use std.lv here because we restrict the variances of the latent variables to be 1
      p <- lavaan::standardizedSolution(f, type = "std.lv")[,c("lhs", "op", "rhs", "est.std")]
      colnames(p) <- c("lhs", "op", "rhs", "estimate")

      # This is the odd thing that psych::omegaFromSem does...
      r <- cov2cor(lavInspect(f, "sampstat")$cov)
    } else {
      # List the parameter estimates
      ## I use std.lv here because we restrict the variances of the latent variables to be 1
      p <- lavaan::standardizedSolution(f, type = standardization_method)[,c("lhs", "op", "rhs", "est.std")]
      colnames(p) <- c("lhs", "op", "rhs", "estimate")

      # std.all, so we use a correlation matrix.
      # Note: I have no idea why psych::omegaFromSem uses "std.lv" or
      # even unstandardized indicator parameters and still does a cov2cor...
      r <- cov2cor(lavInspect(f, "sampstat")$cov)
    }
  } else {
    # Model implied correlation matrix
    r <- lavaan::lavInspect(f, "cor.ov")
  }

  omega.default(parameters = p,
                covariance_matrix = r,
                total_variance_calculation = total_variance_calculation,
                generalFactorName = generalFactorName)

}

#' @title Compute omega (openMx)
#' @description Compute omega based on a openMx model
#' @method omega MxRAMModel
#' @export
omega.MxRAMModel <- function(f,
                             total_variance_calculation = c("observed", "modelled"),
                             generalFactorName = "G"
                             ) {

  # Does openmx provide different standardization methods like lavaan?
  p <- mxStandardizeRAMpaths(f)[,c("col", "matrix", "row", "Std.Value")]
  colnames(p) <- c("lhs", "op", "rhs", "estimate")

  p$op <- ifelse(p$op == "A", "=~", p$op)
  p$op <- ifelse(p$op == "S", "~~", p$op)


  # A discrepancy in results can occur if model fit is not adequate (unmodelled error variance).
  # It is more conservative to use the observed variance for omega calculation.
  total_variance_calculation <- match.arg(total_variance_calculation,
                                          choices = c("observed", "modelled"),
                                          several.ok = F)

  if (total_variance_calculation == "observed") {
    # This fails with missing data. Does openMx provide a non-missing cov matrix
    # like lavaan does?
    r <- cor(f$data$observed)
  } else if (total_variance_calculation == "modelled") {
    r <- cov2cor(f$expectation$output$covariance)
    rownames(r) <- names(f$data$observed)
    colnames(r) <- names(f$data$observed)
  }

  omega.default(parameters = p,
                covariance_matrix = r,
                total_variance_calculation = total_variance_calculation,
                generalFactorName = generalFactorName)

}
