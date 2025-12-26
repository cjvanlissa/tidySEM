#' @importFrom utils getFromNamespace
if(requireNamespace("bain", quietly = TRUE)){
  parse_hypothesis <- getFromNamespace("parse_hypothesis", "bain")
} else {
  parse_hypothesis <- function(varnames, hyp){
    message('The `bain` package is not installed; hypotheses cannot be parsed. Run `install.packages("bain")` to be able to parse complex hypotheses.')
    return(NULL)
  }

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
wald_test <- function(x, hypothesis, ...){
  if(grepl("[><]", hypothesis)) stop("Can only evaluate equality constrained hypotheses. Hypotheses with '>' or '<' are not valid.")
  if(requireNamespace("bain", quietly = FALSE)){
    varnames_orig <- varnames <- names(coef(x))
    hyp_orig <- hypothesis
    if(any(grepl("\\[\\d+,\\d+\\]", varnames))){
      for(i in seq_along(varnames_orig)){
        names(varnames_orig)[i] <- varnames[i] <- paste0("xxxxxx", i)
        hypothesis <- gsub(varnames_orig[i], paste0("xxxxxx", i), hypothesis, fixed = TRUE)
      }
    }
    hyps <- parse_hypothesis(varnames = varnames, hyp = hypothesis)
    test_res <- do.call(rbind, lapply(hyps$hyp_mat, function(h){
      as.data.frame(lapply(car::linearHypothesis(x, hypothesis.matrix = h[, -ncol(h), drop = FALSE], rhs = h[, ncol(h), drop = TRUE]), `[[`, 2))
    }))
  } else {
    message('The `bain` package is not installed; hypotheses cannot be parsed. Run `install.packages("bain")` to be able to parse complex hypotheses.')
    hyps <- hypothesis
    test_res <- do.call(rbind, lapply(hyps, function(h){
      as.data.frame(lapply(car::linearHypothesis(x, hypothesis.matrix = h), `[[`, 2))
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
  if(any(grepl("xxxxxx\\d{1,}", out$Hypothesis))){
    for(i in seq_along(varnames_orig)){
      repthis <- paste0("xxxxxx", i)
      if(!any(grepl(repthis, out$Hypothesis), fixed = TRUE)) next
      out$Hypothesis <- gsub(paste0(repthis, "\\b"), varnames_orig[repthis], out$Hypothesis)
    }
  }
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
