

#' @references
#' Amelia::rubin_rules and mice::pool served as inspirations for this function
pool_data <- function(estimates, standard_errors, df_complete) {
  # Rubin rules

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
  pvalue <- 2 * (pt(abs(statistic), pmax(df, 0.001), lower.tail = FALSE))

  list(
    estimate = estimate,
    se = se,
    statistic = statistic,
    df = df,
    pvalue = pvalue#,
    # var_w = var_w,
    # var_b = var_b,
    # var_t = var_t,
    # riv = riv,
    # lambda = lambda,
    # fmi = fmi
  )
}

do_pool <- function(parameters, parameter_names, df_complete) {

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
    pool_data(m[pn,,"est"], m[pn,,"se"], df_complete)
  })

  results <- as.data.frame(do.call(rbind, results))

  results$term <- parameter_names

  results[,unique(c("term",colnames(results)))]
}

#' @export
pseudo_class_pool <- function(fits, df_complete = NULL, ...) {

  if (!is.list(fits)) {
    fits <- list(fits)
  }

  cls <- sapply(fits, function(x) head(class(x)))

  cl <- unique(cls)

  if ( length(cl) > 1 ) {
    stop(paste0("'fits' consists of objects with different classes: ", paste(cl, sep = ', ')))
  }

  UseMethod("pseudo_class_pool", fits[[1]])

}

#' @export
pseudo_class_pool.default <- function(fits, df_complete = NULL, ...) {
  if ( ! requireNamespace("mice") ) {
    stop("Cannot pool fit objects, because package 'mice' is not installed")
  }

  summary(mice::pool(object = fits, dfcom = df_complete, ...))
}

#' @export
pseudo_class_pool.MxModel <- function(fits, df_complete = NULL, ...) {

  parameters <- lapply(fits, function(fit) {

    parameters <- summary(fit)$parameters

    ns <- colnames(parameters)
    colnames(parameters)[which(ns == "Estimate")] <- "est"
    colnames(parameters)[which(ns == "Std.Error")] <- "se"

    parameters[,c("name", "est", "se")]
  })

  example_fit <- fits[[1]]
  example_parameters <- parameters[[1]]

  if (is.null(df_complete)) {
    warning(paste("degrees of freedom is assumed to be equal to the number of observations used in the analysis. Provide a better value via the 'df_complete' argument"))
    df_complete <- example_fit@data@numObs
  }


  parameter_names <- example_parameters$name

  do_pool(parameters, parameter_names, df_complete)
}

#' @export
pseudo_class_pool.lavaan <- function(fits, std.all = FALSE, ...) {

  parameters <- lapply(fits, function(fit) {

    if (std.all == TRUE) {
      ss <- lavaan::standardizedSolution(fit, type = "std.all", se = TRUE, zstat = F, pvalue = F, ci = F)

      cns <- colnames(ss)
      colnames(ss)[which(cns == "est.std")] <- "est"

      parameters <- ss
    } else {
      parameters <- lavaan::parameterEstimates(fit, se = TRUE, zstat = F, pvalue = F, ci = F)
    }

    parameters
  })

  example_fit <- fits[[1]]
  example_parameters <- parameters[[1]]

  if (is.null(df_complete)) {
    warning(paste("degrees of freedom is assumed to be equal to the number of observations used in the analysis. Provide a better value via the 'df_complete' argument"))
    df_complete <- lavaan::lavInspect(example_fit, "ntotal")
  }

  parameter_names <- with(example_parameters, paste(lhs, op, rhs))



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

pseudo_class_analysis <- function(dfs, analysis, enclos = parent.frame(), expose_data = FALSE) {

  if ( expose_data ) {
    # Expose the imputed data frame as "data"
    lapply(X = dfs, FUN = function(df) {

      result <- eval(expr = analysis, envir = list(data = df), enclos = enclos)

      result
    })

  } else {

    lapply(X = dfs, FUN = function(df) {

      result <- eval(expr = analysis, envir = df, enclos = enclos)

      result
    })

  }

}

pseudo_class_analysis_cb <- function(dfs, func) {
  lapply(X = dfs, FUN = func)
}


#' @title Sample class membership based on class membership probabilities
#' @description
#' Adds a variable named "class" to "m" datasets. The variable contains the
#' random class membership assignment based on the probabilities of "fit"
#'
#' @param fit A fitted mx_mixture model
#' @param x A dataset to which to add "class", if NULL, takes data from fit object
#' @param m How many datasets to generate. Default is 10.
#' @param output_type Either 'list' (default), 'long', or 'class_only'. If 'list', the datasets are returned as a list.
#' If "long", the datasets are combined into a long dataset containing ".imp" to indicate the separate datasets.
#' If "class_only", class assignments are returned as a matrix with "m" columns.
#'
#' @returns Returns a list of size "m" containing data.frame's with variable "class",
#' or if output_type is "long", one long format data.frame with variable "class".
#' If output_type is "class_only" a matrix with "m" columns containing class assignments.
#'
#' @examplesIf FALSE
#' ## Not run:
#' x <- iris[,c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")]
#' colnames(x) <- c("SL", "SW", "PL", "PW")
#' tidySEM::mx_profiles(data = x, classes = 3) -> fit
#'
#' class_assignments <- pseudo_class_data(fit)
#' ## End(Not)
#'
#' @author Frank Gootjes
#'
#' @export
pseudo_class_data <- function(fit, x = NULL, m = 20, output_type = "list") {

  output_type <- match.arg(output_type, c("list", "long", "class_only"))

  cs <- seq_along(fit@submodels)

  probabilities <- as.data.frame(class_prob(fit)$individual)[,cs]

  if ( output_type != "class_only") {
    if (is.null(x)) {
      x <- fit@data@observed
    }

    if (nrow(x) != nrow(probabilities)) {
      stop("Cannot assign class membership variable to data.frame 'x'. Ensure the same rows in 'x' were used to fit the model.")
    }

    x_imputed <- lapply(seq_len(m), function(i) {

      x$class <- probabilistic_assignment(probabilities)

      x
    })

    if (output_type == "long") {

      x_imputed <- lapply(seq_along(x_imputed), function(i) {
        x_imputed[[i]]$.id <- rownames(x_imputed[[i]])
        x_imputed[[i]]$.imp <- i

        x_imputed[[i]]
      })

      do.call(rbind, x_imputed)
    } else {
      x_imputed
    }
  } else {

    sapply(seq_len(m), function(i) {

      probabilistic_assignment(probabilities)

    })
  }


}

#' @title Analyze cluster membership using the pseudo-class technique
#' @description
#' Given a fitted tidySEM mixture model "fit", and the corresponding dataset "x",
#' this functions executes "analysis" on "m" datasets. Each of these datasets
#' contains a "class" variable representing random class assignment based on
#' the class probabilities of "fit".
#'
#' @param fit A fitted mx_mixture model
#' @param analysis Either an expression to execute on every generated dataset,
#' or a function that performs the analysis on every generated dataset,
#' or a character that can be converted with \code{\link[tidySEM]{as_ram}}.
#' @param x The corresponding dataset on which the model "fit" was fitted. If NULL (default)
#' the dataset is taken from "fit".
#' @param m The amount of datasets to generate. Default is 10.
#' @param pool_results Whether to pool the results of the analyses using \code{\link[mice]{pool}}, default is FALSE.
#' @param expose_data Whether the expression explicitly refers to the generated dataset using "data".
#' @param ... Arguments passed to \code{\link[tidySEM]{pseudo_class_pool}}
#'
#' @note
#' Note that if you provide "x", the function assumes rows of "x" correspond to the same
#' rows of the dataset on which "fit" is based.
#'
#' @returns If pool_results is FALSE, returns a list of size "m" containing fitted models as returned by "analysis".
#' If pool_results is TRUE, returns an object of class \code{\link[mice]{mipo}}
#'
#' @author Frank Gootjes
#'
#' @examplesIf FALSE
#' ## Not run:
#' x <- iris[,c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")]
#' colnames(x) <- c("SL", "SW", "PL", "PW")
#' tidySEM::mx_profiles(data = x, classes = 3) -> fit
#'
#' pseudo_class_technique( fit = fit,
#'                         analysis = "SL ~ class",
#'                         pool_results = TRUE ) -> pct_mx
#'
#' summary(pct_mx)
#'
#' pseudo_class_technique( fit = fit,
#'                         analysis = lm( SL ~ class ),
#'                         pool_results = TRUE  ) -> pct_lm
#'
#' summary(pct_lm)
#'
#'
#' pseudo_class_technique(fit = fit,
#'                        analysis = lm(Sepal.Length ~ class, data = data),
#'                        x = iris,
#'                        m = 10,
#'                        pool_results = TRUE,
#'                        expose_data = TRUE) -> pcte
#'
#' summary(pcte)
#'
#' pseudo_class_technique(fit = fit,
#'                        analysis = function(df) lm(Sepal.Length ~ class, data = df),
#'                        x = iris,
#'                        m = 10,
#'                        pool_results = TRUE,
#'                        analysis_method = "function") -> pct_func
#'
#' summary(pct_func)
#'
#' pseudo_class_technique( fit = fit,
#'                         analysis = nnet::multinom( class ~ SL + SW + PL ) ) -> membership_prediction
#'
#'
#' ## End(Not run)
#'
#' @references
#' - Paper recommending, among others, the pseudo-class technique.
#'   The GRoLTS-Checklist: Guidelines for Reporting on Latent Trajectory Studies. https://doi.org/10.1080/10705511.2016.1247646
#' - Paper describing the pseudo-class technique.
#'   Residual Diagnostics for Growth Mixture Models. https://doi.org/10.1198/016214505000000501
#' - Paper describing pooling results.
#'   Applied missing data analysis with SPSS and (R) Studio. Chapter Rubin's Rules. https://bookdown.org/mwheymans/bookmi/rubins-rules.html
#' - Source for the default of 20 draws.
#'   Wald Test of Mean Equality for Potential Latent Class Predictors in Mixture Modeling. https://www.statmodel.com/download/MeanTest1.pdf
#' @md
#'
#' @export
pseudo_class_technique <- function(fit, analysis, x = NULL, m = 20, pool_results = FALSE, expose_data = FALSE, ...) {

  enclos <- parent.frame()

  expr <- substitute(analysis)

  if (is(expr, "name")) {
    analysis_type <- "function"
  } else if (is(expr, "call")) {
    if (length(expr) > 1 && deparse(expr[[1]]) == "function"  ) {
      analysis_type <- "function"
    } else {
      analysis_type <- "expression"
    }
  } else if (is.character(expr)) {
    # Treat as a tidy_sem
    analysis_type <- "function"

    ramModel <- as_ram(analysis)

    analysis <- function(df) {

      model <- mxModel(ramModel,
              data = mxData(observed = df, type = "raw"),
              fitfunction = mxFitFunctionML())

      out <- try(run_mx(model, silent = TRUE), silent = TRUE)
      if(!inherits(out, "try-error")){
        return(out)
      }

      out
    }
  } else {
    stop(paste0("Unknown expression type for 'analysis'. Should be a 'call' or 'name' of a function. 'analysis': ", as.character(expr)))
  }

  # Generate the data, uses sample(). Since analysis might also use the seed,
  # data is generated first in order to be more reproducible.

  if ( is.null(x) ) {
    x <- fit@data@observed
  }

  dfs <- pseudo_class_data(fit = fit, x = x, m = m, output_type = "list")

  # Run the models
  if ( analysis_type == "function" ) {

    # Analysis is a function so the callback variant can be used
    fits <- pseudo_class_analysis_cb(dfs, analysis)

  } else {

    # Analysis is a call which can be evaluated
    fits <- pseudo_class_analysis(dfs, expr, enclos, expose_data = expose_data)

  }

  # Pool the results?
  if (pool_results) {

    pseudo_class_pool(fits, ...)

  } else {

    # Return a list of fit objects
    fits
  }

}
