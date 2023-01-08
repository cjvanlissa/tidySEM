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
      trsh <- x$Thresholds$values
      if(is.null(trsh)){
        trsh <- x$Thresholds$result
      }
      out <- do.call(rbind, lapply(1:ncol(trsh), function(i){
        thiscol <- pnorm(trsh[x$mat_dev$free[, i],i])
        thiscol <- c(0, thiscol, 1)
        thiscol <- diff(thiscol)#[-1]
        data.frame(Variable = colnames(trsh)[i],
                   Category = 1:length(thiscol),
                   Probability = thiscol)
      }))
      rownames(out) <- NULL
      return(out)
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
