#' @method table_results MxModel
#' @export
#' @importFrom stats pnorm
table_results.MxModel <- function (x, columns = c("label", "est_sig", "se", "pval", "confint", "group", "level"), digits = 2, ...)
{
  # Multigroup:
  # attr(attr(fit,"runstate")$fitfunctions$mg.fitfunction, "groups")
  Args <- list(x = x)
  digits <- force(digits)
  sum_x <- summary(x)
  results <- sum_x$parameters
  results$est <- results$Estimate
  results$lhs <- results$row
  results$op <- NA
  results$op[results$matrix == "A"] <- "~"
  results$op[results$matrix == "S"] <- "~~"
  results$op[results$matrix == "M"] <- "~1"
  results$rhs <- results$col
  fac_load <- results$op == "~" & results$rhs %in% x$latentVars
  results$rhs[fac_load] <- results$row[fac_load]
  results$lhs[fac_load] <- results$col[fac_load]
  results$op[fac_load] <- "=~"

  results$confint <- conf_int(results$est, se = results$Std.Error)
  if (!is.null(sum_x[["CI"]])) {
    if (!all(is.na(sum_x[["CI"]]))) {
      ci_x <- data.frame(name = rownames(sum_x$CI),
                         sum_x$CI)
      ci_x$CI <- NA
      ci_x$CI[!is.na(ci_x$lbound) & !is.na(ci_x$ubound)] <- conf_int(x= NULL, lb = ci_x$lbound[!is.na(ci_x$lbound) & !is.na(ci_x$ubound)], ub = ci_x$ubound[!is.na(ci_x$lbound) & !is.na(ci_x$ubound)])
      ci_x <- ci_x[!is.na(ci_x$CI), ]

      results$confint[match(ci_x$name, results$name)] <- ci_x$CI
    }
  }
  results$pvalue <- 2*pnorm(abs(results$Estimate)/results$Std.Error, lower.tail = FALSE)
  results$est_sig <- est_sig(results$est, sig = results$pvalue)
  results[c("estimate", "Estimate")] <- NULL
  names(results)[match(c("Std.Error", "name"), names(results))] <- c("se", "label")
  names(results) <- tolower(names(results))
  if(!is.null(columns)) {
    results[, na.omit(match(columns, names(results)))]
  }
  else {
    order_cols <- c("label")
    order_cols <- order_cols[order_cols %in% names(results)]
    remaining_cols <- names(results)[(length(order_cols)):(ncol(results))]
    remaining_cols <- remaining_cols[!remaining_cols %in%
                                       order_cols]
    order_cols <- c(match(order_cols, names(results)), match(remaining_cols,
                                                             names(results)))
    class(results) <- c("tidy_results", class(results))
    results[, order_cols]
  }
}
