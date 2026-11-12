#' Compute Scale Scores
#'
#' Uses the dictionary of a \code{tidy_sem} object to compute total scores for
#' each scale.
#' @param x A \code{tidy_sem} object containing data and a dictionary with
#' item names and scale assignments.
#' @param type Character string specifying the scoring method. One of
#' \code{"mean"} (default), \code{"median"}, or \code{"sum"}.
#' @param ... Additional arguments passed to the aggregation function,
#' such as \code{na.rm = TRUE}.
#'
#' @return
#' A data frame with one row per observation and one column per scale.
#' Column names correspond to the scale names in the dictionary.
#'
#' @examples
#' df <- iris[1:4]
#' names(df) <- c("x1", "x2", "y1", "y2")
#' model <- tidy_sem(df)
#' dictionary(model)$scale <- c("X", "X", "Y", "Y")
#' score_items(model)
#' score_items(model, type = "sum")
#' score_items(model, type = "median", na.rm = TRUE)
#' @export
score_items <- function(x, type = c("mean", "median", "sum"), ...){
  UseMethod("score_items", x)
}

make_scaleslist <- function(x) {
  out <- split(
    x$dictionary$name,
    x$dictionary$scale
  )
 if (!length(out)) {
    stop("Object does not contain valid scales.", call. = FALSE)
  }

  out
}

#' @method score_items tidy_sem
#' @export
score_items.tidy_sem <- function(x, type = c("mean", "median", "sum"), ...) {
  type <- match.arg(type)
  scales_list <- make_scaleslist(x)

  fun <- switch(
    type,
    mean = function(d) rowMeans(d, ...),
    median = function(d) apply(d, 1L, stats::median, ...),
    sum = function(d) rowSums(d, ...)
  )
  out <- lapply(scales_list, function(items) {
    fun(x$data[, items, drop = FALSE])
  })
  as.data.frame(out, check.names = FALSE)
}
