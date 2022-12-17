#' @title Lo-Mendell-Rubin Likelihood Ratio Test
#' @description Implements the ad-hoc adjusted likelihood ratio test (LRT)
#' described in Formula 15 of Lo, Mendell, & Rubin (2001), or LMR LRT.
#' @param x An object for which a method exists.
#' @param ... Additional arguments.
#' @return A numeric vector containing the likelihood ratio LR, the ad-hoc
#' corrected LMR, degrees of freedom, and the LMR p-value.
#' @references Lo Y, Mendell NR, Rubin DB. Testing the number of components in a
#' normal mixture. Biometrika. 2001;88(3):767–778.
#' <doi:10.1093/biomet/88.3.767>
#' @examples
#' lr_lmr(150L, -741.02, 8, 1, -488.91, 13, 2)
#' @rdname lr_lmr
#' @export
lr_lmr <- function(x, ...){
  UseMethod("lr_lmr", x)
}

#' @method lr_lmr tidy_fit
#' @export
lr_lmr.tidy_fit <- function(x, ...){
  if(!all(grepl("^mix\\d+$", x$modelName))) stop("Lo-Mendell-Rubin likelihood ratio test is only suitable for latent class analyses.")
  numclass <- as.integer(gsub("mix", "", x$modelName, fixed = TRUE))
  if(!all(diff(numclass) > 0)){
    stop("Lo-Mendell-Rubin likelihood ratio test requires a strictly increasing number of classes.")
  }
  if(!length(unique(x$n)) == 1){
    stop("Number of participants is not identical across latent class analyses.")
  }

  out <- data.frame(do.call(rbind, lapply(2:length(numclass), function(i){
    calc_lrt_internal(
      null = c(x$LL[i-1],
               x$Parameters[i-1],
               x$n[1],
               numclass[i-1]),
      alt  = c(x$LL[i],
               x$Parameters[i],
               x$n[1],
               numclass[i])
    )
  })))
  out <- rbind(rep(NA, ncol(out)), out)
  out
}

#' @importFrom stats pchisq
calc_lrt_internal <- function(null, alt){
  # c(ll, parameters, n, class)
  if(!alt[3] == null[3]){
    warning("Null and alt models estimated on different number of cases, and might not be nested. Used lowest number of cases.")
  }
  n <- min(c(alt[3], null[3]))
  if(!alt[4]>null[4]){
    stop("Alternative model does not have more classes than null model.")
  }
  lr_test_stat = 2 * (alt[1] - null[1])
  modlr_test_stat <- lr_test_stat / (1 + (((3 * alt[4] - 1) - (3 * null[4] - 1)) * log(n))^-1)
  df <- (alt[2] - null[2])
  lmrt_p <- pchisq(q = modlr_test_stat, df = df, lower.tail = FALSE)
  out <- c(lr = lr_test_stat, lmr_lr = modlr_test_stat, df = df, lmr_p = lmrt_p)
  class(out) <- c("LRT", class(out))
  out
}

#' @method print LRT
#' @export
print.LRT <- function(x,
                      digits = 3,
                      na.print = "",
                      ...) {
  cat("Lo-Mendell-Rubin ad-hoc adjusted likelihood ratio rest:\n\n")
  cat(
    "LR = ",
    formatC(x[["lr"]], digits = digits, format = "f"),
    ", LMR LR (df = ",
    as.integer(x[["df"]]),
    ") = ",
    formatC(x[["lmr_lr"]], digits = digits, format = "f"),
    ", p ",
    ifelse(x[["lmr_p"]] < 1/10^digits,
           paste0("< ", 1/10^digits),
           paste0("= ", formatC(x[["lmr_p"]], digits = digits, format = "f")))
    , sep = "")
}
