
probabilistic_assignment <- function(probs) {

  ps <- nrow(probs)
  class_count <- ncol(probs)

  # For every row class membership is selected based on the class probabilities
  sapply(seq_len(ps), function(i) {
    sample(x = seq_len(class_count), size = 1, replace = F, prob = probs[i,])
  })

}

#' @importFrom rlang eval_tidy
pseudo_class_analysis <- function(dfs, quosure, enclos, expose_data = FALSE) {

  if ( expose_data ) {
    # Expose the imputed data frame as "data"
    lapply(X = dfs, FUN = function(df) {

      eval_tidy(expr = quosure, data = list(data = df), env = enclos)

    })

  } else {

    lapply(X = dfs, FUN = function(df) {

      eval_tidy(expr = quosure, data = df, env = enclos)

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
pseudo_class_data <- function(fit, x = NULL, m = 10, output_type = "list") {

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
#' or a function that performs the analysis on every generated dataset.
#' @param x The corresponding dataset on which the model "fit" was fitted. If NULL (default)
#' the dataset is taken from "fit".
#' @param m The amount of datasets to generate. Default is 10.
#' @param pool_results Whether to pool the results of the analyses using \code{\link[mice]{pool}}, default is TRUE.
#' @param expose_data Whether the expression explicitly refers to the generated dataset using "data".
#' @param ... Arguments passed to \code{\link[mice]{pool}}
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
#'                         analysis = lm( SL ~ class ) ) -> pct
#'
#' summary(pct)
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
#' @export
#' @importFrom mice pool
#' @importFrom rlang get_expr
#' @importFrom rlang enquo
pseudo_class_technique <- function(fit, analysis, x = NULL, m = 10, pool_results = TRUE, expose_data = FALSE, ...) {

  enclos <- parent.frame()

  quosure <- enquo(analysis)

  expr <- get_expr(quosure)

  if (is(expr, "name")) {
    analysis_type <- "function"
  } else if (is(expr, "call")) {
    analysis_type <- "expression"
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
    fits <- pseudo_class_analysis(dfs, quosure, enclos, expose_data = expose_data)

  }

  # Pool the results?
  if (pool_results) {
    # Leverage mice::pool
    pool(object = fits, ...)
  } else {

    # Return a list of fit objects
    fits
  }

}
