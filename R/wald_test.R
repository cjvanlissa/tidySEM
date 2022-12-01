#' @importFrom utils getFromNamespace
parse_hypothesis <- getFromNamespace("parse_hypothesis", "bain")

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
  varnames <- names(coef(x))
  cl <- match.call()
  cl[[1L]] <- quote(parse_hypothesis)
  cl[["x"]] <- NULL
  names(cl)[names(cl) == "hypothesis"] <- "hyp"
  cl[["varnames"]] <- varnames
  hyps <- eval(cl)
  test_res <- do.call(rbind, lapply(hyps$hyp_mat, function(h){
    as.data.frame(lapply(car::linearHypothesis(x, hypothesis.matrix = h[, -ncol(h), drop = FALSE], rhs = h[, ncol(h), drop = TRUE]), `[[`, 2))
  }))
  names(test_res) <- c("df", "chisq", "p")
  out <- data.frame(Hypothesis = hyps$original_hypothesis, test_res)
  class(out) <- c("wald_test", class(out))
  out
}

#' @method print wald_test
#' @export
print.wald_test <- function(x, ...){
  cat("Wald chi-square tests for linear hypotheses:\n")
  print.data.frame(x, ..., row.names = FALSE)
}

#' @method get_estimates MxModel
#' @export
#' @importFrom bain get_estimates
get_estimates.MxModel <- function(x, ...){
  coef(x)
}
