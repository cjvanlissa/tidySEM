#' Compare Information Criteria
#'
#' IC weights quantify the evidence in favor of different models in a set.
#' This function normalizes the IC values to obtain IC weights, which sum to 1.
#' The model with the highest weight is most supported by the data.
#' The ratio of different weights gives the relative support in favor of one
#' model over another.
#' @param x An object for which a method exists.
#' @param ... Additional arguments.
#' @return A `list` of class `ic_weights` with elements `$weights`, which
#' contains the model weights, and `$comparison`, which contains the relative
#' support in favor of each model over the others.
#' @export
#' @examples
#' ics <- c(100, 200, 102, 300)
#' ic_weights(ics)
#' @references Wagenmakers, E. J., & Farrell, S. (2004). AIC model selection
#' using Akaike weights. Psychonomic bulletin & review, 11(1), 192-196.
#' \doi{10.3758/BF03206482}
ic_weights <- function(x, ...){
  UseMethod("ic_weights", x)
}

#' @method ic_weights tidy_fit
#' @export
ic_weights.tidy_fit <- function(x, ic = "BIC", ...){
  if(!ic %in% names(x)){
    stop("The 'ic = ", ic, "' was not found in the tidy_fit table.")
  }
  x <- x[!is.na(x[[ic]]), , drop = FALSE]
  if(nrow(x) < 2){
    stop("Fewer than two models with valid ", ic, ". Could not calculate weights.")
  }
  out <- x[[ic]]
  if("Name" %in% names(x)) names(out) <- x[["Name"]]
  ic_weights(out)
}

#' @method ic_weights default
#' @export
ic_weights.default <- function(x, ...){
  if(!all(x > 0)) message("Negative IC values found; this may indicate a problem.")
  if(is.null(names(x))) names(x) <- paste0("Model ", 1:length(x))
  ic_min <- min(x)
  ic_diff <- x - ic_min
  rel_likelihood <- exp(-0.5*ic_diff)
  weights <- rel_likelihood / sum(rel_likelihood)
  comparison <- outer(weights, 1/weights)
  out <- list(weights = weights, comparison = comparison)
  class(out) <- c("ic_weights", class(out))
  return(out)
}

