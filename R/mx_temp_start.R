simple_starts <- function (model, type = c("ULS", "DWLS"))
{
  type <- type[1]
  isMultiGroupModel <- (is.null(model$expectation) && inherits(model$fitfunction, "MxFitFunctionMultigroup"))
  if (isMultiGroupModel) {
    submNames <- names(model@submodels)
    wmodel <- model
    for (amod in submNames) {
      wmodel[[amod]] <- OpenMx::mxModel(model[[amod]], asdhelper(model,
                                                                   subname = amod, type = type))
    }
    wmodel <- OpenMx::mxModel(wmodel, OpenMx::mxFitFunctionMultigroup(submNames))
  }
  else {
    wmodel <- OpenMx::mxModel(model, asdhelper(model,
                                                 type = type))
  }
  wmodel <- OpenMx::mxOption(wmodel, "Calculate Hessian", "No")
  wmodel <- OpenMx::mxOption(wmodel, "Standard Errors", "No")
  # End omxBuildAutoStartModel
  wmodel <- OpenMx::mxRun(wmodel, silent = TRUE)
  newparams <- coef(wmodel)
  oldparams <- coef(model)
  model <- OpenMx::omxSetParameters(model, values = newparams, labels = names(oldparams))
  model <- fix_x(wmodel = wmodel, model = model, multigroup = isMultiGroupModel)
  return(model)
}


#' @importFrom stats coef cov
asdhelper <- function (model, subname = model@name, type)
{
  exps <-
    OpenMx::mxGetExpected(model, c("covariance", "means", "thresholds"),
                  subname = subname)
  useVars <- colnames(exps$means)
  data <- model[[subname]]$data$observed
  data <- data[, useVars, drop = FALSE]

  if (type == "ULS") {
    if(!any(sapply(data, is.ordered))){
      os <- list(cov = cov(data, use = "pair"))
      if (length(exps$means) > 0)
        os$means <- colMeans(data, na.rm = TRUE)
      return(list(
        OpenMx::mxData(
          data,
          type = "raw",
          numObs = nrow(data),
          observedStats = os
        ),
        OpenMx::mxFitFunctionWLS("ULS",
                         fullWeight = FALSE)
      ))
    }
    return(list(
      OpenMx::mxData(
        data,
        type = "raw"
      ),
      OpenMx::mxFitFunctionWLS("ULS",
                       fullWeight = FALSE)
    ))
  }
  mdata <- OpenMx::mxData(
    data,
    type = "raw"
  )
  return(list(mdata, OpenMx::mxFitFunctionWLS(
    type,
    ifelse(length(exps$means) >
             0, "marginals", "cumulants"),
    type != "ULS"
  )))
}


fix_x <- function(wmodel, model, multigroup){
  if(multigroup){
    submNames <- names(model@submodels)
    for(n in submNames){
      model[[n]] <- fix_x(wmodel = wmodel[[n]], model = model[[n]], multigroup = FALSE)
    }
  } else {
    obsvars <- wmodel$manifestVars
    fixedvar <- !diag(wmodel$S$free)[obsvars]
    zerovar <- diag(wmodel$S$values)[obsvars] == 0
    if(any(fixedvar & zerovar)){
      diag(model$S$values)[which(fixedvar & zerovar)] <- diag(wmodel$data$observedStats$cov)[which(fixedvar & zerovar)]
    }
    # fixedvcov <- !wmodel$S$free[obsvars, obsvars]
    # zerovcov <- wmodel$S$values[obsvars, obsvars] == 0
    # if(any(fixedvcov & zerovcov)){
    #   model$S$values[fixedvcov & zerovcov] <- wmodel$data$observedStats$cov[fixedvcov & zerovcov]
    # }
    # fixedmeans <- !wmodel$M$free[1, obsvars]
    # zeromeans <- wmodel$M$values[1, obsvars] == 0
    # if(any(fixedmeans & zeromeans)){
    #   model$M$values[1, fixedmeans & zeromeans] <- wmodel$data$observedStats$means[fixedmeans & zeromeans]
    # }
  }
  return(model)
}


#' @importFrom stats lm.wfit weighted.mean
mx_starts_auxiliary <- function(model, data, weights,
                             alpha = 0.5,
                             min_variance = 1e-3) {

  stopifnot(
    inherits(model, "MxModel"),
    is.data.frame(data),
    length(weights) == nrow(data)
  )

  weights <- as.numeric(weights)
  weights[!is.finite(weights) | weights < 0] <- 0

  if (sum(weights) <= 0) {
    weights[] <- 1
  }

  # All manifest variables
  vars <- intersect(model@manifestVars, names(data))

  continuous <- vars[vapply(
    data[vars],
    function(z) is.numeric(z) && !is.factor(z),
    logical(1)
  )]

  ordinal <- vars[vapply(
    data[vars],
    is.ordered,
    logical(1)
  )]

  # Initialize ordinal thresholds
  if(length(ordinal) > 0){
    thrsh <- mx_data_quantiles(data[, ordinal, drop = FALSE], weights = weights, alpha = alpha)
    for (v in ordinal) {

      thresholds <- thrsh[[v]]

      if ("Thresholds" %in% names(model@matrices)) {

        nr <- min(length(thresholds), nrow(model$Thresholds$values))

        free <- model$Thresholds$free[seq_len(nr), v]

        model$Thresholds$values[seq_len(nr)[free], v] <- thresholds[seq_len(nr)][free]
      }

      if ("mat_dev" %in% names(model@matrices)) {

        deviations <- c(thresholds[1], diff(thresholds))

        nr <- min(length(deviations), nrow(model$mat_dev$values))

        free <- model$mat_dev$free[seq_len(nr), v ]

        model$mat_dev$values[seq_len(nr)[free], v] <- deviations[seq_len(nr)][free]
      }
    }
  }

  # Initialize continuous means, regressions, and residual variances
  residuals <- list()
  for (v in continuous) {

    # Predictors of v, based on RAM A matrix
    predictors <- continuous[
      vapply(
        continuous,
        function(p) isTRUE(model$A$free[v, p]),
        logical(1)
      )
    ]

    if (!length(predictors)) {

      # Intercept-only model
      mom <- weighted_moments(
        data[v], weights
      )

      if (is.null(mom)) next

      if (isTRUE(model$M$free[1, v])) {
        model$M$values[1, v] <- mom$mean[v]
      }

      if (isTRUE(model$S$free[v, v])) {
        model$S$values[v, v] <- max(
          mom$cov[v, v], min_variance
        )
      }

      residuals[[v]] <- data[[v]] -
        mom$mean[v]

    } else {

      fit <- weighted_regression(
        y = data[[v]],
        X = data[predictors],
        w = weights,
        min_variance = min_variance)

      if (is.null(fit)) next

      # Regression coefficients
      for (p in predictors) {

        if (isTRUE(model$A$free[v, p]) &&
            is.finite(fit$coefficients[p])) {

          model$A$values[v, p] <-
            fit$coefficients[p]
        }
      }

      # Intercept
      if (isTRUE(model$M$free[1, v]) &&
          is.finite(fit$coefficients["(Intercept)"])) {

        model$M$values[1, v] <-
          fit$coefficients["(Intercept)"]
      }

      # Residual variance
      if (isTRUE(model$S$free[v, v])) {
        model$S$values[v, v] <- fit$variance
      }

      # Residuals for all observations
      b <- fit$coefficients[predictors]
      b[is.na(b)] <- 0

      residuals[[v]] <- data[[v]] -
        fit$coefficients["(Intercept)"] -
        as.matrix(data[predictors]) %*% b
    }
  }

  # Initialize residual covariances
  if ("S" %in% names(model@matrices)) {

    for (i in seq_along(continuous)) {
      for (j in seq_len(i - 1L)) {

        v1 <- continuous[i]
        v2 <- continuous[j]

        if (!isTRUE(model$S$free[v1, v2]) &&
            !isTRUE(model$S$free[v2, v1])) {
          next
        }

        if (is.null(residuals[[v1]]) ||
            is.null(residuals[[v2]])) {
          next
        }

        r1 <- as.numeric(residuals[[v1]])
        r2 <- as.numeric(residuals[[v2]])

        keep <- is.finite(r1) &
          is.finite(r2) &
          weights > 0

        if (sum(keep) < 2L) next

        w <- weights[keep]

        mu1 <- stats::weighted.mean(r1[keep], w)
        mu2 <- stats::weighted.mean(r2[keep], w)

        covariance <- sum(
          w *
            (r1[keep] - mu1) *
            (r2[keep] - mu2)
        ) / sum(w)

        if (isTRUE(model$S$free[v1, v2])) {
          model$S$values[v1, v2] <- covariance
        }

        if (isTRUE(model$S$free[v2, v1])) {
          model$S$values[v2, v1] <- covariance
        }
      }
    }
  }

  model
}


# Weighted mean and covariance, using complete cases
weighted_moments <- function(dat, w) {

  keep <- complete.cases(dat) & w > 0
  dat <- as.matrix(dat[keep, , drop = FALSE])
  w <- w[keep]

  if (!nrow(dat) || sum(w) <= 0) {
    return(NULL)
  }

  mu <- colSums(dat * w) / sum(w)

  centered <- sweep(dat, 2, mu, "-")

  cov <- crossprod(
    centered * sqrt(w),
    centered * sqrt(w)
  ) / sum(w)

  list(mean = mu, cov = cov)
}

# Weighted least squares for one dependent variable
weighted_regression <- function(y, X, w, min_variance = 1e-3) {

  dat <- data.frame(y = y, X, check.names = FALSE)

  keep <- complete.cases(dat) & w > 0

  if (sum(keep) < 2L) {
    return(NULL)
  }

  y <- y[keep]
  X <- as.matrix(X[keep, , drop = FALSE])
  w <- w[keep]

  # Intercept and predictors
  X <- cbind("(Intercept)" = 1, X)

  fit <- tryCatch(
    stats::lm.wfit(
      x = X,
      y = y,
      w = w
    ),
    error = function(e) NULL
  )

  if (is.null(fit)) {
    return(NULL)
  }

  coefs <- fit$coefficients
  names(coefs) <- colnames(X)

  residuals <- as.numeric(y - X %*% replace(coefs, is.na(coefs), 0))

  variance <- sum(w * residuals^2) / sum(w)

  list(
    coefficients = coefs,
    residuals = residuals,
    variance = max(variance, min_variance)
  )
}
