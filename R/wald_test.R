#' @importFrom utils getFromNamespace
parse_hypothesis <- function(varnames, hyp){
  if(requireNamespace("bain", quietly = TRUE)){
    cl <- match.call()
    cl[[1]] <- str2lang("bain:::parse_hypothesis")
    eval.parent(cl)
  } else {
    message('The `bain` package is not installed; hypotheses cannot be parsed. Run `install.packages("bain")` to be able to parse complex hypotheses.')
    return(NULL)
  }
}

is_simple_hypothesis <- function(x){
  grepl("^[^<>&;()]+$", x) &
      lengths(regmatches(x, gregexpr("=", x, fixed = TRUE))) <= 1
}

#' @title Wald Test for Linear Hypotheses
#' @description This function is a wrapper for the function
#' [car::linearHypothesis()], but which uses the [bain::bain()] syntax to parse
#' equality constrained hypotheses.
#' @param x An object for which a method exists.
#' @param hypothesis A character string with equality constrained hypotheses,
#' specified according to the [bain::bain()] syntax.
#' @param ... Additional arguments passed to [car::linearHypothesis()].
#' @return A `data.frame` of class `wald_test`.
#' @examples
#' mod <- lm(Sepal.Length ~ Sepal.Width, data = iris)
#' coef(mod)
#' wald_test(mod, "Sepal.Width = 0")
#' @seealso
#'  \code{\link[car]{linearHypothesis}}
#' @rdname wald_test
#' @export
#' @importFrom car linearHypothesis
#' @importFrom utils packageVersion
wald_test <- function(x, hypothesis, ...){
  if(grepl("[><]", hypothesis)) stop("Can only evaluate equality constrained hypotheses. Hypotheses with '>' or '<' are not valid.")
  # Check if any hypotheses are complex
  simple_hypothesis <- is_simple_hypothesis(hypothesis)
  if(any(!simple_hypothesis)){
    can_run <- isTRUE(requireNamespace("bain", quietly = TRUE))
    if(can_run){
      if(!isTRUE(utils::packageVersion("bain") >= package_version("0.2.12"))){
        can_run <- FALSE
      }
    }
    if(!can_run){
      message('You are attempting to test a complex hypothesis which requires the `bain` package to be parsed. Run `install.packages("bain")` to be able to parse complex hypotheses.')
      return(NULL)
    }
  }

  if(all(simple_hypothesis)){
    hyps <- hypothesis
    test_res <- do.call(rbind, lapply(hyps, function(h){
      as.data.frame(lapply(suppressWarnings(car::linearHypothesis(x, hypothesis.matrix = h)), `[[`, 2))
    }))
  } else {
    varnames_orig <- varnames <- names(coef(x))
    hyp_orig <- hypothesis
    hyps <- parse_hypothesis(varnames = varnames, hyp = hypothesis)
    test_res <- do.call(rbind, lapply(hyps$hyp_mat, function(h){
      as.data.frame(lapply(car::linearHypothesis(x, hypothesis.matrix = h[, -ncol(h), drop = FALSE], rhs = h[, ncol(h), drop = TRUE]), `[[`, 2))
    }))
  }
  if(!is.null(test_res)){
    if(!"F" %in% names(test_res)){
      names(test_res) <- c("df", "chisq", "p")
    } else {
      names(test_res)[match("Pr..F.", names(test_res))] <- c("p")
    }
  }
  out <- data.frame(Hypothesis = tryCatch(hyps$original_hypothesis, error = function(e){hypothesis}), test_res)
  class(out) <- c("wald_test", class(out))
  out
}

#' @method print wald_test
#' @export
print.wald_test <- function(x, ...){
  cat("Wald tests for linear hypotheses:\n")
  print.data.frame(x, ..., row.names = FALSE)
}

#' @title Get estimates from a model object
#' @description Get estimates from a model object.
#' This convenience function allows you to see that coefficients are properly
#' extracted, note how their names will be parsed, and inspect their values.
#' @param x A model object.
#' @param ... Parameters passed to and from other functions.
#' @return An object of class 'model_estimates'
#' @rdname get_estimates
#' @export
#' @keywords internal
get_estimates <- function(x, ...){
  UseMethod("get_estimates", x)
}

#' @method get_estimates MxModel
#' @export
get_estimates.MxModel <- function(x, ...){
  coef(x)
}
