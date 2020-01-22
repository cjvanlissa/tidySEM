#' @method table_results MxModel
#' @export
#' @importFrom stats pnorm
table_results.MxModel <- function (x, standardized = TRUE, columns = c("label", "est_sig", "se", "pval", "confint", "group", "level"), digits = 2, ...)
{
  Args <- list(x = x)
  digits <- force(digits)
  sum_x <- summary(x)
  use_ci <- FALSE
  if (!is.null(x$output[["confidenceIntervals"]])) {
    if (!all(is.na(x$output[["confidenceIntervals"]]))) {
      ci_x <- data.frame(name = rownames(x$output$confidenceIntervals),
                         x$output$confidenceIntervals)
      rownames(ci_x) <- NULL
      use_ci <- TRUE
    }
  }
  results <- sum_x$parameters
  results$est <- results$Estimate
  if (use_ci) {
    if (!nrow(ci_x) == nrow(results)) {
      results <- merge(results, ci_x, by = "name", all.y = TRUE)
      results$label <- gsub("^.+\\.(.+)\\[.+\\]$", "\\1",
                            results$name)
      results$est[is.na(results$est)] <- results$estimate[is.na(results$est)]
      results$ubound <- results$ubound.x
      results$ubound[is.na(results$ubound)] <- results$ubound.y
      results$lbound <- results$lbound.x
      results$lbound[is.na(results$lbound)] <- results$lbound.y
      results <- results[, !grepl("\\.[xy]$", names(results))]
    }
  } else{
    results$label <- gsub("^.+\\.(.+)\\[.+\\]$", "\\1",
                          results$name)
  }
  results$pvalue <- 2 * pnorm(-abs(results$est), mean = 0,
                              sd = results$Std.Error)
  results$est_sig <- est_sig(results$est, sig = results$pvalue)
  results$confint <- conf_int(lb = results$lbound, ub = results$ubound)
  results[c("estimate", "Estimate")] <- NULL
  names(results)[match("Std.Error", names(results))] <- "se"
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
