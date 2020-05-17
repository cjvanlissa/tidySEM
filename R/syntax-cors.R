#' @title Generate syntax for correlations
#' @description Generate syntax for correlations between variables.
#' @param x Object for which a method exists. If \code{x} is an object of class
#'  \code{tidy_sem}, then correlations between all
#'  observed and latent variables in the data dictionary of that object are
#'  computed, by default. If
#' \code{x} is a character vector, all elements of the vector are used.
#' @param ... Optional additional character vectors of variables to be
#' correlated. If \code{x} is an object of class \code{tidy_sem}, then up to two
#' vectors can be provided. If \code{x} is a
#' vector, then one more optional vector can be provided.
#' When no additional vectors of variable names are provided, only
#' intercorrelations between elements of \code{x} are returned.
#' @return An object of class \code{tidy_sem}.
#' @examples
#' dict <- tidy_sem(c("bfi_1", "bfi_2", "bfi_3", "bfi_4", "bfi_5"))
#' cors(dict, c("bfi_1", "bfi_2"))
#' @rdname cors
#' @export
cors <- function(x, ...){
  UseMethod("cors")
}

#' @method cors tidy_sem
#' @export
cors.tidy_sem <- function(x, ...){
  out <- x
  dots <- list(...)
  if(!length(dots)){
    Args <- list(x = x$dictionary$name[x$dictionary$type %in% c("observed", "latent")])
  } else {
    Args <- check_dots_cors(dots)
  }
  syntax <- do.call(cors_table, Args)
  out$syntax <- rbind(out$syntax, syntax)
  out
}


# @method cors tidy_sem
# @export
# cors.tidy_sem <- function(x, ...){
#   dots <- list(...)
#   x <- force(x)
#   if(!length(dots)){
#     Args <- list(x = x$dictionary$name[x$dictionary$type %in% c("observed", "latent")])
#   } else {
#     Args <- check_dots_cors(dots)
#   }
#   out <- list(dictionary = x,
#               syntax = do.call(cors_table, Args))
#   class(out) <- c("tidy_sem", class(out))
#   out
# }

#' @method cors data.frame
#' @export
cors.data.frame <- function(x, ...){
  Args <- as.list(match.call()[-1])
  Args$x <- do.call(dictionary, Args[1])
  do.call(cors, Args)
}

# cors_table(c("a", "b", "c"))
# cors_table(c("a", "b", "c"), c("x", "y"))
cors_table <- function(x, y = NULL){
  if(is.null(y)){
    mod_mat <- matrix(1:(length(x)*length(x)), nrow = length(x))
    out <- cbind(expand.grid(x, "~~", x)[upper.tri(mod_mat), ], "TRUE", "", "covariance", "covariance")
  } else {
    out <- cbind(expand.grid(x, "~~", y), "TRUE", "", "covariance", "covariance")
  }
  names(out) <- c("lhs", "op", "rhs", "free", "value", "category", "aspect")
  check_lav_tab(out)
}


# cors_mplus(c("a", "b", "c"))
# cors_mplus(c("a", "b", "c"), c("x", "y"))
# cors_mplus <- function(x, y = NULL){
#   if(is.null(y)){
#     m <- matrix(paste0(rep(x, each = length(x)), " WITH ", rep(x, length(x)), ";"), ncol = length(x))
#     m[lower.tri(m)]
#   } else {
#     paste0(rep(x, length(x)), " WITH ", rep(y, each = length(x)), ";")
#   }
# }


# cors_lavaan(c("a", "b", "c"))
# cors_lavaan(c("a", "b", "c"), c("x", "y"))
# @importFrom lavaan mplus2lavaan.modelSyntax
# cors_lavaan <- function(x, y = NULL){
#   Args <- as.list(match.call()[-1])
#   out <- do.call(cors_mplus, Args)
#   strsplit(mplus2lavaan.modelSyntax(paste0(out, collapse = "\n")), "\n")[[1]]
# }


#syntax_cor_lavaan(c("a", "b", "c"))
#syntax_cor_mplus(c("a", "b", "c"))

# syntax_cor_lavaan <- function(x, y = NULL, all = TRUE, label = TRUE){
#   if(all){
#     cors <- expand.grid(x, " ~~ ", y)
#     if(label){
#       cors <- cbind(cors, " ~~ c", "*")[, c(1, 4, 1,3,5,3)]
#     }
#     cors <- cors[!cors$Var1==cors$Var3, ]
#     unname(apply(cors, 1, paste0, collapse = ""))
#   } else {
#     if(label){
#       paste0(x, " ~~ c", x, y, "*", y)
#     } else {
#       paste0(x, " ~~ ", y)
#     }
#   }
# }

check_dots_cors <- function(dots){
  if(length(dots) > 3){
    stop("Tried to pass more than two additional arguments to function cors.tidy_sem(). Cors accepts an object, and up to two character vectors of variable names.", call. = FALSE)
  }
  if(!all(sapply(dots, inherits, what = "character"))){
    stop("Cors accepts up to two character vectors of variable names as optional arguments. The optional arguments are not all character vectors.", call. = FALSE)
  }
  names(dots) <- c("x", "y")[1:length(dots)]
  dots
}
