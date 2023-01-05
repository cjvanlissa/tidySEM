#' @title Switch LCA Class Labels
#' @description The order of class labels in LCA is arbitrary. This can
#' result in a phenomenon called 'label switching', where classes change
#' places between replications of an analysis. This function attempts to
#' re-order classes in a substantively meaningful way.
#' @param x An `MxModel` estimated by `mx_mixture` or one of its wrappers.
#' @param param The parameter by which to order the classes,
#' defaults to `'weights'`, which orders classes based on their sample size.
#' @param decreasing logical. Should the classes be sorted in increasing
#' or decreasing order? Default: TRUE
#' @param order Integer, indicating the ordering of classes. Ignored
#' when NULL (default).
#' @return An `MxModel` with `"tidySEM"` attribute: `"mixture"`
#' @details The argument `param` can accept either:
#'
#' 1. The default string "weights", in which classes are sorted by size.
#' 2. The `OpenMx` matrix indicator for a specific model parameter; e.g.,
#' the first mean is indicated by `"M[1,1]"`. These indicators can be viewed
#' by running `table_results(x, columns = NULL)`.
#' 3. The letter indicating an `OpenMx` model matrix, e.g., `"M"` refers to
#' the matrix of means. To account for all elements of the matrix, Euclidean
#' distance to the origin is used.
#' @examples
#' \dontrun{
#' df <- iris[1:4]
#' names(df) <- letters[1:4]
#' res1 <- mx_profiles(data = df, classes = 2)
#' mx_switch_labels(res1, decreasing = FALSE)
#' }
#' @rdname mx_switch_labels
#' @export
mx_switch_labels <- function(x, param = "weights", decreasing = TRUE, order = NULL){
  UseMethod("mx_switch_labels", x)
}

#' @export
#' @method mx_switch_labels MxModel
mx_switch_labels.MxModel <- function(x, param = "weights", decreasing = TRUE, order = NULL){
  if(is.null(order)){
    if(param == "weights"){
      coefs <- x$weights$values
      ordr <- order(coefs, decreasing = decreasing)
    } else {
      tabres <- table_results(x, columns = NULL)
      tabres$c <- gsub("^(class\\d+)\\..+$", "\\1", tabres$name)
      if(grepl("\\[\\d+,\\d+\\]", param)){
        tabres$p <- gsub("^class\\d+\\.(.+)$", "\\1", tabres$name)
      } else {
        tabres$p <- tabres$matrix
      }
      tabres <- tabres[, c("c", "p", "est")]
      tabres <- tabres[grepl("^class", tabres$c), ]
      tabres <- tabres[tabres$p %in% param, ]
      if(!any(duplicated(tabres$c))){
        ordr <- order(tabres$est, decreasing = decreasing)
      } else {
        dists <- tapply(tabres$est, factor(tabres$c), FUN = function(x){
          as.matrix(dist(
            t(matrix(c(rep(0, length(x)), x), ncol = 2))
            , method = "euclidean"))[1,2]
        })
        ordr <- order(dists, decreasing = decreasing)
      }
    }
  } else {
    ordr <- order
  }
  # Insert start values into mixture model
  model <- x
  if(!all(ordr == seq_along(ordr))){
    for(i in seq_along(ordr)){
      dest_class <- names(model@submodels)[i]
      orig_class <- names(model@submodels)[ordr[i]]
      for(mtx in names(model[[dest_class]]@matrices)){
        model[[dest_class]][[mtx]]$values <- x[[orig_class]][[mtx]]$values
      }
    }
    model$weights$values <- (model$weights$values / model$weights$values[ordr[1]])[ordr]
    model <- mxRun(model)
  }
  return(model)
}
