
#' @references
#' Amelia::rubin_rules and mice::pool served as inspirations for this function
#' @importFrom stats pt
pool_data <- function(x, df_complete = NULL) {
  # Rubin rules

  estimates <- x[,"est"]
  standard_errors <- x[,"se"]

  m <- length(estimates)
  estimate <- mean(estimates)

  var_w <- mean(standard_errors^2)
  var_b <- var(estimates)
  var_t <- var_w + var_b + (var_b / m)

  lambda <- (1 + 1/m) * var_b/var_t
  lambda[lambda < 1e-04] <- 1e-04
  dfold <- (m - 1)/lambda^2
  dfobs <- (df_complete + 1)/(df_complete + 3) * df_complete * (1 - lambda)
  df <- ifelse(is.infinite(df_complete), dfold, dfold * dfobs/(dfold + dfobs))

  riv <- (1 + 1/m) * var_b/var_w
  lambda <- (1 + 1/m) * var_b/var_t
  fmi <- (riv + 2/(df + 3))/(riv + 1)

  se <- sqrt(var_t)
  statistic <- estimate / se
  pvalue <- 2 * (stats::pt(abs(statistic), pmax(df, 0.001), lower.tail = FALSE))

  list(
    estimate = estimate,
    se = se,
    statistic = statistic,
    df = df,
    df_complete = df_complete,
    pvalue = pvalue#,
    # var_w = var_w,
    # var_b = var_b,
    # var_t = var_t,
    # riv = riv,
    # lambda = lambda,
    # fmi = fmi
  )
}

do_pool <- function(parameters, parameter_names, df_complete = NULL) {

  m <- array(dim = c(
    length(parameter_names),
    length(parameters),
    2
  ), dimnames = list(parameter_names, NULL, c("est", "se")))

  for (j in seq_len(length(parameters))) {
    for(i in seq_len(length(parameter_names))) {
      m[i, j, ] <- unlist(parameters[[j]][i, c("est", "se"), drop = TRUE])
    }
  }

  results <- lapply(parameter_names, function(pn) {
    if ( all( m[pn,,"se"] == 0 ) ) {
      warning(paste("Standard errors of parameter '", pn,"' are all zero. This can happen for non-free parameters. Don't interpret the SE for such parameters."))
    }
    pool_data(m[pn,,], df_complete)
  })

  a <- as.data.frame(do.call(rbind, results))
  results <- as.data.frame(lapply(a, unlist))

  results$term <- parameter_names

  results[,unique(c("term",colnames(results)))]
}


pseudo_class_pool <- function(fits, df_complete = NULL, ...) {

  if (!is.list(fits)) {
    fits <- list(fits)
  }

  cls <- sapply(fits, function(x) head(class(x)))

  cl <- unique(cls)

  if ( length(cl) > 1 ) {
    stop(paste0("'fits' consists of objects with different classes: ", paste(cl, collapse = ', ')))
  }

  UseMethod("pseudo_class_pool", fits[[1]])

}

#' @export
pseudo_class_pool.default <- function(fits, df_complete = NULL, ...) {
  if ( ! requireNamespace("mice", quietly = TRUE) ) {
    stop("Cannot pool fit objects, because package 'mice' is not installed")
  }

  summary(mice::pool(object = fits, dfcom = df_complete, ...))
}

#' @export
pseudo_class_pool.MxModel <- function(fits, df_complete = NULL, std = FALSE, ...) {

  modelType <- imxTypeName(fits[[1]])

  if ( modelType != "RAM" && std == TRUE) {
    stop(paste("Don't know how to standardize a MxModel of type:", modelType))
  }

  parameters <- lapply(fits, function(fit) {

    if (std == TRUE) {

      # table_results() is used to get the prettier lhs, op, and rhs
      parameters_info <- table_results(fit, format_numeric = F, columns = c("lhs", "op", "rhs", "est", "se", "name"))

      parameters_standardized <- mxStandardizeRAMpaths(fit, SE = TRUE)[,c("name", "Std.Value", "Std.SE")]

      parameters <- merge(parameters_info, parameters_standardized)

      if(nrow(parameters) != nrow(parameters_standardized)) {
        stop("merge() result is unexpected")
      }

      parameters$rhs <- ifelse(parameters$op == "~1", "", parameters$rhs)

      parameters$name <- with(parameters, paste(lhs, op, rhs))

      parameters$est <- parameters$Std.Value
      parameters$se <- parameters$Std.SE

      parameters <- parameters[,c("name", "est", "se")]

    } else {

      parameters <- table_results(fit, format_numeric = F, columns = c("lhs", "op", "rhs", "est", "se"))

      parameters$rhs <- ifelse(parameters$op == "~1", "", parameters$rhs)

      parameters$name <- with(parameters, paste(lhs, op, rhs))

      parameters <- parameters[,-(1:3)][,c("name", "est", "se")]

    }

    parameters
  })

  example_fit <- fits[[1]]
  example_parameters <- parameters[[1]]

  if (is.null(df_complete)) {
    nparam <- length(omxGetParameters(example_fit, free = TRUE))
    ntotal <- example_fit@data@numObs

    message(paste("The degrees of freedom are assumed to be equal to the total number of observations used in the model (", ntotal, ") minus the number of parameters estimated (", nparam, "). This may not be correct. If necessary, provide a better value via the 'df_complete' argument"))

    df_complete <- max(1, ntotal - nparam)

  }


  parameter_names <- example_parameters$name

  do_pool(parameters, parameter_names, df_complete)
}

#' @export
pseudo_class_pool.lavaan <- function(fits, df_complete = NULL, std.all = FALSE, ...) {

  example_fit <- fits[[1]]

  # Extract free parameters only, because the SE of fixed parameters is 0, while their estimate can differ across fits
  # Applying Rubin's Rules to SE values of 0 appears invalid.
  # If they needed to be included in the pooling process, they should be included in the lavaan model.
  fit_meta <- lavaan::parameterTable(example_fit)
  fit_meta <- fit_meta[fit_meta$free != 0, c("lhs", "op", "rhs")]

  free_parameter_names <- with(fit_meta, paste(lhs, op, rhs))

  parameters <- lapply(fits, function(fit) {

    # I use this implementation rather than coef() because it is more flexible in terms of options



    if (std.all == TRUE) {
      #
      ss <- lavaan::standardizedSolution(fit, type = "std.all", se = TRUE, zstat = F, pvalue = F, ci = F)

      cns <- colnames(ss)
      colnames(ss)[which(cns == "est.std")] <- "est"

      parameters <- ss
    } else {


      parameters <- lavaan::parameterEstimates(fit, se = TRUE, zstat = F, pvalue = F, ci = F)
    }

    parameters$name <- with(parameters, paste(lhs, op, rhs))

    parameters <- parameters[,c("name", "est", "se")]

    parameters <- parameters[parameters$name %in% free_parameter_names,]

    parameters <- parameters[order(parameters$name),]

    parameters
  })

  example_parameters <- parameters[[1]]

  if (is.null(df_complete)) {
    nparam <- length(lavaan::coef(example_fit))
    ntotal <- lavaan::lavInspect(example_fit, "ntotal")

    message(paste("The degrees of freedom are assumed to be equal to the total number of observations used in the model (", ntotal, ") minus the number of free parameters estimated (", nparam, "). This may not be correct. If necessary, provide a better value via the 'df_complete' argument"))

    df_complete <- max(1, ntotal - nparam)
  }

  parameter_names <- example_parameters$name

  do_pool(parameters, parameter_names, df_complete)
}

probabilistic_assignment <- function(probs) {

  ps <- nrow(probs)
  class_count <- ncol(probs)

  # For every row class membership is selected based on the class probabilities
  sapply(seq_len(ps), function(i) {
    sample(x = seq_len(class_count), size = 1, replace = F, prob = probs[i,])
  })

}

pseudo_class_analysis <- function(dfs, model, enclos = parent.frame()) {

  # Expose the imputed data frame as "data"
    lapply(X = dfs, FUN = function(df) {

      eval(expr = model, envir = list(data = df), enclos = enclos)

    })
}

pseudo_class_analysis_cb <- function(dfs, func) {
  lapply(X = dfs, FUN = func)
}


#' @title Append Pseudo-class Draws
#' @description
#' Generates `m` datasets with random draws of a variable named `class`, with
#' probability for these draws based on each case's probability of belonging to
#' that class according to the model in `x`.
#' @param x An object for which a method exists, usually a `mx_mixture` model.
#' @param data A data.frame which the `class` variable is appended to. Note
#' that the row order must be identical to that of the data used to fit `x`,
#' as these data will be augmented with a pseudo-class draw for that specific
#' individual.
#' @param m Integer. Number of datasets to generate. Default is 10.
#' @returns A data.frame of class `class_draws`.
#' @examples
#' dat <- iris[c(1:5, 50:55, 100:105),1:3]
#' colnames(dat) <- letters[1:3]
#' fit <- mx_profiles(data = dat, classes = 2)
#'
#' append_class_draws(fit, data = iris[c(1:5, 50:55, 100:105), 4, drop = FALSE])
#' @export
append_class_draws <- function(x, data = NULL, m = 20) {
  if (isTRUE(is.null(attr(x, "tidySEM")) | length(attr(x, "tidySEM") == "mixture") == 0)){
    stop("Argument 'x' is not a valid mixture model.")
  }
  if(any(names(data) == "id_dataset")){
    warning("Argument 'data' has a column called 'id_dataset'; this column will be renamed because append_class_draws() adds a column of the same name.")
    names(data)[which(names(data) == "id_dataset")] <- "id_dataset.1"
  }
  probabilities <- as.data.frame(class_prob(x, type = "individual")[["individual"]])
  probabilities <- probabilities[, -ncol(probabilities), drop = FALSE]

  if (is.null(data)) {
      data <- x@data@observed
  }


    if (nrow(data) != nrow(probabilities)) {
      stop("Cannot assign class membership variable to data.frame 'data'. Ensure the same rows in 'data' were used to fit the model.")
    }

    x_imputed <- lapply(seq_len(m), function(i) {
      data.frame(id_dataset = i, data, class = probabilistic_assignment(probabilities))
    })
    x_imputed <- data.frame(do.call(rbind, x_imputed))
    class(x_imputed) <- c("class_draws", class(x_imputed))
    return(x_imputed)
}

#' @title Estimate an Auxiliary Model using the Pseudo-Class Method
#' @description
#' Estimate an auxiliary model based on multiple datasets, randomly drawing
#' latent class values based on the estimated probability of belonging to each
#' class. The pseudo class variable is treated as an observed variable within
#' each dataset, and results are pooled across datasets to account for
#' classification uncertainty.
#'
#' @param x An object for which a method exists, typically either a fitted
#' `mx_mixture` model or `class_draws` object.
#' @param model Either an expression to execute on every generated dataset,
#' or a function that performs the analysis on every generated dataset,
#' or a character that can be interpreted as a structural equation model using
#' \code{\link[tidySEM]{as_ram}}. This `model` can explicitly refer to `data`.
#' @param df_complete Integer. Degrees of freedom of the complete-data analysis.
#' @param ... Additional arguments passed to other functions.
#' @returns An object of class \code{\link[base]{data.frame}} containing pooled
#' estimates.
#'
#'
#' @examples
#' set.seed(2)
#' dat <- iris[c(1:5, 50:55, 100:105), 1:4]
#' colnames(dat) <- c("SL", "SW", "PL", "PW")
#' fit <- suppressWarnings(mx_profiles(data = dat, classes = 3))
#'
#' pct_mx <- pseudo_class(x = fit,
#'                        model = "SL ~ class",
#'                        data = dat,
#'                        m = 2)
#'
#' pct_lm <- pseudo_class(x = fit,
#'              model = lm( SL ~ class, data = data),
#'              data = dat,
#'              m = 2)
#'
#'
#' pcte <- pseudo_class(x = fit,
#'                      model = lm(SL ~ class, data = data),
#'                      data = dat,
#'                      m = 2)
#'
#' pct_func <- pseudo_class(x = fit,
#'                          model = function(data){lm(SL ~ class, data = data)},
#'                          data = dat,
#'                          m = 2)
#'
#'
# pseudo_class(x = fit,
#              model = nnet::multinom( class ~ SL + SW + PL ) ) -> membership_prediction
#'
#' @references
#' Pseudo-class technique:
#' Wang C-P, Brown CH, Bandeen-Roche K (2005). Residual Diagnostics for Growth
#' Mixture Models: Examining the Impact of a Preventive Intervention on
#' Multiple Trajectories of Aggressive Behavior. Journal of the American
#' Statistical Association 100(3):1054-1076. \doi{10.1198/016214505000000501}
#'
#' Pooling results across samples:
#' Van Buuren, S. 2018. Flexible Imputation of Missing Data. Second Edition.
#' Boca Raton, FL: Chapman & Hall/CRC. \doi{10.1201/9780429492259}
#'
#'
#' @export
pseudo_class <- function(x, model, df_complete = NULL, ...) {
  UseMethod("pseudo_class", x)
}


#' @method pseudo_class MxModel
#' @param data A data.frame on which the auxiliary model can be evaluated. Note
#' that the row order must be identical to that of the data used to fit `x`,
#' as these data will be augmented with a pseudo-class draw for that specific
#' individual.
#' @param m Integer. Number of datasets to generate. Default is 20.
#' @rdname pseudo_class
#' @export
pseudo_class.MxModel <- function(x, model, df_complete = NULL, data = NULL, m = 20, ...) {
  if (isTRUE(is.null(attr(x, "tidySEM")) | length(attr(x, "tidySEM") == "mixture") == 0)){
    stop("Argument 'fit' is not a valid mixture model.")
  }
  if(inherits(data, what = "data.frame")){

    Args <- list(
      x = x,
      data = data,
      m = m
    )

    # Generate the data, uses sample(). Since model might also use the seed,
    # all data is generated first in order to be more reproducible.
    data <- do.call(append_class_draws, Args)
  } else {
    stop("Function 'pseudo_class()' requires a 'data' argument when called on a 'mx_mixture' model.")
  }
  cl <- match.call()
  cl[[1]] <- str2lang("tidySEM::pseudo_class")
  cl[["x"]] <- data
  cl <- cl[c(1, which(names(cl) %in% c("x", "model", "df_complete")))]
  eval.parent(cl)
}

#' @method pseudo_class class_draws
#' @export
#' @importFrom methods is
pseudo_class.class_draws <- function(x, model, df_complete = NULL, ...) {
  enclos <- parent.frame()

  expr <- substitute(model)

  if (methods::is(expr, "name")) {

    # Assume names are okay to evaluate
    if ( is.function(model) ) {
      analysis_type <- "function"
    } else {
      analysis_type <- class(model)
    }

  } else if (methods::is(expr, "call")) {
    if (length(expr) > 1 && deparse(expr[[1]]) == "function"  ) {
      # expr is an anonymous function passed as an argument
      analysis_type <- "function"
    } else {
      analysis_type <- "call"
    }
  } else if (is.character(expr)) {
    # Treat as a tidy_sem

    ramModel <- as_ram(model)

    model <- function(df) {

      model <- mxModel(ramModel,
              data = mxData(observed = df, type = "raw"),
              fitfunction = mxFitFunctionML())

      out <- try(run_mx(model, silent = TRUE), silent = TRUE)
      if(!inherits(out, "try-error")){
        return(out)
      }

      out
    }

    analysis_type <- "function"


  } else {
    stop(paste0("Unknown expression type for 'model'. It should be a 'call', a 'function' or 'name' of a function. Received: ", as.character(expr)))
  }

  class(x) <- "data.frame"
  dfs <- split(x[, -which(names(x) == "id_dataset"), drop = FALSE], f = factor(x$id_dataset))

  # Run the models
  if ( analysis_type == "function" ) {

    if ( !is.function(model)  ) {
      stop(paste("model '", expr, "' is not a function. The argument 'model' should be a callback function, a call, or a character vector"))
    }

    if ( length( formalArgs(model) ) != 1 ) {
      stop("'model' is a function but it does not accept an argument. The callback function should accept one argument.")
    }

    # Analysis is a function so the callback variant can be used
    fits <- pseudo_class_analysis_cb(dfs, model)

  } else if (analysis_type == "call" ) {

    # Analysis is a call which can be evaluated
    fits <- pseudo_class_analysis(dfs, expr, enclos)

  } else {
    stop(paste("Unknown analysis_type:", analysis_type))
  }

  # Pool the results
  return(pseudo_class_pool(fits, ...))
}
