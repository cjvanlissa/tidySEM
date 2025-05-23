#' @title Dummy Code Factor Variables
#' @description For each variable *v* that inherits
#' `factor`, create a number of new variables equal
#' to `levels(v)` to indicate group membership (1)
#' or non-membership (0) of that level.
#' The resulting dummies have class `mxFactor`.
#' @param x An object for which a method exists.
#' @param classes Character vector, indicating which classes to dummy code.
#' Defaults to `c("factor", "character")`.
#' @param ... Arguments
#' @return A `data.frame`.
#' @examples
#' if(requireNamespace("OpenMx", quietly = TRUE)){
#' mx_dummies(iris[1:5,])
#' }
#' @rdname mx_dummies
#' @export
#' @importFrom stats model.matrix
mx_dummies <- function(x, classes = c("factor", "character"), ...){
  UseMethod("mx_dummies", x)
}

#' @method mx_dummies data.frame
#' @export
mx_dummies.data.frame <- function(x, classes = c("factor", "character"), ...){
  if(any(!(classes == "factor"))){
    for(c in classes[!(classes == "factor")]){
      code_me <- sapply(x, inherits, what = c)
      if(any(code_me)){
        x[code_me] <- lapply(x[code_me], as.factor)
      }
    }
  }
  code_me <- sapply(x, inherits, what = "factor")
  if(!any(code_me)) return(x)
  na_action <- getOption("na.action")
  options(na.action = "na.pass")
  out <- x[, !code_me, drop = FALSE]
  coded <- lapply(x[which(code_me)], function(c){
    mat <- data.frame(model.matrix(~.-1, data = as.data.frame(c)))
    mat[] <- lapply(mat, OpenMx::mxFactor, levels = c(0, 1))
    mat
  })
  options(na.action = na_action)
  coded <- do.call(cbind, coded)
  uniqvals <- sapply(coded, function(i){min(prop.table(table(i)))})
  if(any(uniqvals < .05)) warning("Some categories have fewer than 5% of cases: ", paste0(names(coded)[uniqvals < .05], collapse = ", "))
  is_unique <- sapply(coded, function(c){
    tb <- table(na.omit(c))
    length(tb[!tb == 0]) < 2
    })
  if(any(!is_unique)){
    out <- cbind(out, coded[, !is_unique, drop = FALSE])
  }
  return(out)
}

#' @method mx_dummies factor
#' @export
mx_dummies.factor <- function(x, ...){
  vnam <- deparse(substitute(x))
  is_dollar <- grepl("\\$\\b[a-zA-Z_\\.1-9]+\\b$", vnam)
  is_bracket <- grepl("\\[\\[[\'\"]\\b[a-zA-Z_\\.1-9]+\\b[\'\"]\\]\\]$", vnam)
  if(is_dollar | is_bracket){
    if(is_dollar){
      vnam <- gsub("^.+?\\$", "", vnam)
    }
    if(is_bracket){
      vnam <- gsub("^.+?\\[\\[[\'\"]\\b([a-zA-Z_\\.1-9]+)\\b[\'\"]\\]\\]$", "\\1", vnam)
    }
  } else {
    vnam <- "xxxremovemexxx"
  }
  na_action <- getOption("na.action")
  options(na.action = "na.pass")
  tmp <- data.frame(x)
  names(tmp) <- vnam
  mat <- data.frame(model.matrix(~.-1, data = tmp))
  mat[] <- lapply(mat, OpenMx::mxFactor, levels = c(0, 1))
  options(na.action = na_action)
  names(mat) <- make.names(gsub("xxxremovemexxx", "", names(mat), fixed = TRUE))
  return(mat)
}
