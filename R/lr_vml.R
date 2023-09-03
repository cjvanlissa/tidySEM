#' @title Lo-Mendell-Rubin Likelihood Ratio Test
#' @description A likelihood ratio test for class enumeration in latent class
#' analysis, proposed by Lo, Mendell, & Rubin (2001) based on work by Vuong
#' (1989). See Details for important clarification.
#' @details The likelihood ratio test for non-nested models, based on
#' work by Vuong (1989), is often used for class enumeration in latent class
#' analysis (see Lo, Mendell, & Rubin, 2001). Following work by Merkle,
#' You, & Preacher (2016), the models to be compared must first be tested for
#' distinguishability in the population, using the `w2` test. The null
#' hypothesis is that the models are indistinguishable. If this null hypothesis
#' is not rejected, there is no point in statistical model comparison, either
#' using the LMR LRT or other statistics. If the null hypothesis is rejected,
#' the LMR LRT can be evaluated using a Z-test. This function wraps
#' `\link[nonnest2]{vuongtest}` to perform that test.
#' @param x An object for which a method exists.
#' @param ... Additional arguments.
#' @return A `data.frame` containing the Z-value for the likelihood ratio test,
#' its p-value, df (which indicates the difference in number of parameters, not
#' true degrees of freedom, which may be zero), w2 (omega squared) statistic for
#' the test of distinguishability, an its p-value.
#' @references Lo Y, Mendell NR, Rubin DB. Testing the number of components in a
#' normal mixture. Biometrika. 2001;88(3):767–778.
#' \doi{10.1093/biomet/88.3.767}
#' @references Vuong, Q. H. (1989). Likelihood ratio tests for model selection
#' and non-nested hypotheses. Econometrica, 57, 307-333. \doi{10.2307/1912557}
#' @references Merkle, E. C., You, D., & Preacher, K. (2016). Testing non-nested
#' structural equation models. Psychological Methods, 21, 151-163.
#' \doi{10.1037/met0000038}
#' @examples
#' df <- iris[c(1:5, 100:105), 1:3]
#' names(df) <- letters[1:3]
#' res <- mx_profiles(df, classes = 1:2)
#' lr_lmr(res)
#' @rdname lr_lmr
#' @export
lr_lmr <- function(x, ...){
  UseMethod("lr_lmr", x)
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

#' @method lr_lmr MxModel
#' @importFrom nonnest2 vuongtest
#' @export
lr_lmr.MxModel <- function(x, ...){
  dots <- list(...)
  object1 <- x
  object2 <- dots[[which(sapply(dots, inherits, what = "MxModel"))[1]]]
  lr_res <- vuongtest(object1, object2,
            score1 = function(x)mixgrads(x) * -.5,
            score2 = function(x)mixgrads(x) * -.5,
            nested = FALSE)
  return(data.frame(lr = -1*lr_res$LRTstat, df = length(omxGetParameters(object2)) - length(omxGetParameters(object1)), p = lr_res$p_LRT$B, w2 = lr_res$omega, p_w2 = lr_res$p_omega))
}

#' @method lr_lmr mixture_list
#' @importFrom nonnest2 vuongtest
#' @export
lr_lmr.mixture_list <- function(x, ...){
  df_empty <- data.frame(lr = NA, df = NA, p = NA, w2 = NA, p_w2 = NA)
  if(length(x) > 1){
    out <- mapply(function(smaller, bigger){
      tryCatch({
        lr_lmr.MxModel(smaller, bigger)
      },
      error = function(e){
        df_empty })
    }, bigger = x[-1], smaller = x[-length(x)], SIMPLIFY = FALSE)
    out <- do.call(rbind, append(out, list(df_empty), 0))
  } else {
    out <- df_empty
  }
  out <- data.frame(null = c(NA, sapply(x[-length(x)], function(x){x@name})),
                    alt = c(NA, sapply(x[-1], function(x){x@name})),
                    out)[-1, , drop = FALSE]
  rownames(out) <- NULL
  return(out)
}


mixgrads <- function(model){
  if(!isTRUE("mixture" %in% attr(model, "tidySEM")) & length(names(model@submodels)) > 1){
    return(imxRowGradients(model))
  }
  paramLabels <- names(omxGetParameters(model))
  numParam <- length(paramLabels)
  custom.compute <-
    mxComputeSequence(list(
      mxComputeNumericDeriv(checkGradient = FALSE,
                            hessian = FALSE),
      mxComputeReportDeriv()
    ))
  grads <- do.call(rbind, lapply(1:nrow(model@data$observed), function(i) {
    tryCatch({
      mxRun(mxModel(model, custom.compute, mxData(model@data$observed[i, , drop = FALSE], "raw")), silent = TRUE)$output$gradient
    },
    error = function(e) {
      rep(NA, length(numParam))
    })
  }))
  return(grads)
}
