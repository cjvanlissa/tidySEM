#' Results table in probability scale
#'
#' Returns thresholds for ordinal
#' dependent variables in probability scale.
#' @param x An object for which a method exists.
# @param columns A character vector of columns to retain from the results
# section. If this is set to \code{NULL}, all available columns are returned.
# Defaults to \code{c("label", "est_sig", "se", "pval", "confint", "group",
# "level")}. These correspond to 1) the parameter label, 2) estimate column
# with significance asterisks appended
# (* <.05, ** < .01, *** < .001); 3) standard error, 4) p-value, 5) a
# formatted confidence interval, 6) grouping variable (if available), 7) level
# variable for multilevel models, if available.
# @param digits Number of digits to round to when formatting numeric columns.
#' @param ... Arguments passed to other functions.
#' @family Reporting tools
#' @keywords reporting
#' @return A data.frame with results in probability scale.
#' @export
#' @examples
#' \dontrun{
#' df <- data_mix_ordinal
#' df[1:4] <- lapply(df, ordered)
#' mx_lca(data = df,
#'        classes = 2) -> res
#' }
# mx_lca(data = df,
#        classes = 2, run = FALSE) -> res
# res$class1 <- mxModel(model = res$class1,
#                       mxAlgebra(pnorm(Thresholds), name = "Probscale"))
table_prob <- function(x,
                       ...){
  UseMethod("table_prob", x)
}

#' @method table_prob MxModel
#' @export
table_prob.MxModel <- function(x, ...){
  submods <- names(x@submodels)
  if(length(submods) == 0){
    if(!is.null(x[["Thresholds"]])){
      out <- x$Thresholds$result
      out <- pnorm(out)
      out <- rbind(rep(0, ncol(out)), out, rep(1, ncol(out)))
      out <- apply(out, 2, diff)
      rownames(out) <- 1:nrow(out)
      out <- as.data.frame.table(out)
      names(out) <- c("Category", "Variable", "Probability")
      return(out[, c("Variable", "Category", "Probability")])
    } else {
      return(NULL)
    }
  } else {
    out <- lapply(submods, function(nam){
      tmp <- table_prob(x[[nam]])
      if(!is.null(tmp)){
        tmp$group <- nam
      }
      tmp
    })
    out <- bind_list(out)
  }
  out
}


# table_prob.MxModel <- function(x,
#                                ...){
#
#
#
#   cl[[1L]] <- quote(table_results)
#   cl["columns"] <- list(NULL)
#   out <- eval.parent(cl)
#   thresh <- out[which(out$Category == "Thresholds"), , drop = FALSE]
#   probs <- data.frame(do.call(rbind, strsplit(thresh$name, split = "(\\[|,|\\])")))
#   probs[2:3] <- lapply(probs[2:3], as.integer)
#   probs$value <- thresh$est
#   probs$vnam <- thresh$rwn
#   lapply(unique(probs$X1), function(m){
#     tmp <- probs[probs$X1 == m, , drop = FALSE]
#     out <- as.matrix(Matrix::sparseMatrix(i = tmp$X2, j = tmp$X3, x = tmp$value))
#     out <- pnorm(out)
#     out <- rbind(rep(0, ncol(out)), out, rep(1, ncol(out)))
#     out <- apply(out, 2, diff)
#     cns <- tmp[,c("X3", "vnam")]
#     colnames(out) <- tmp$rwn
#     tmp
#   })
#
#   thresh$matnam <- gsub(thresh$name, "\\")
#   browser()
# }
# table_prob.MxModel <- function(x,
#                        columns = c("label", "est_sig", "se", "pval", "confint", "group", "level"),
#                        digits = 2,
#                        ...){
#   cl <- match.call()
#   cl[[1L]] <- quote(table_results)
#   cl["columns"] <- list(NULL)
#   out <- eval.parent(cl)
#   thresh <- out[which(out$Category == "Thresholds"), , drop = FALSE]
#   probs <- data.frame(do.call(rbind, strsplit(thresh$name, split = "(\\[|,|\\])")))
#   probs[2:3] <- lapply(probs[2:3], as.integer)
#   probs$value <- thresh$est
#   probs$vnam <- thresh$rwn
#   lapply(unique(probs$X1), function(m){
#     tmp <- probs[probs$X1 == m, , drop = FALSE]
#     out <- as.matrix(Matrix::sparseMatrix(i = tmp$X2, j = tmp$X3, x = tmp$value))
#     out <- pnorm(out)
#     out <- rbind(rep(0, ncol(out)), out, rep(1, ncol(out)))
#     out <- apply(out, 2, diff)
#     cns <- tmp[,c("X3", "vnam")]
#     colnames(out) <- tmp$rwn
#     tmp
#   })
#
#   thresh$matnam <- gsub(thresh$name, "\\")
#  browser()
# }
