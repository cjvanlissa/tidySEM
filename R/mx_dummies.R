#' @title Dummy Code Factor Variables
#' @description For each variable *v* that inherits
#' `factor`, create a number of new variables equal
#' to `levels(v)` to indicate group membership (1)
#' or non-membership (0) of that level.
#' The resulting dummies have class `mxFactor`.
#' @param x An object for which a method exists.
#' @param ... Arguments
#' @return A `data.frame`.
#' @examples
#' mx_dummies(iris[1:5,])
#' @rdname mx_dummies
#' @export
#' @importFrom stats model.matrix
mx_dummies <- function(x, ...){
  code_me <- sapply(x, inherits, what = "factor")
  if(!any(code_me)) return(x)
  na_action <- getOption("na.action")
  options(na.action = "na.pass")
  out <- x[, !code_me, drop = FALSE]
  coded <- lapply(x[which(code_me)], function(c){
    mat <- data.frame(model.matrix(~.-1, data = as.data.frame(c)))
    mat[] <- lapply(mat, mxFactor, levels = c(0, 1))
    mat
  })
  options(na.action = na_action)
  coded <- do.call(cbind, coded)
  is_unique <- sapply(coded, function(c){
    tb <- table(na.omit(c))
    length(tb[!tb == 0]) < 2
    })
  if(any(!is_unique)){
    out <- cbind(out, coded[, !is_unique, drop = FALSE])
  }
  return(out)
}
