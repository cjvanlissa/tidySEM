# df <- iris[, 1:2]
# names(df) <- c("x", "y")
# set.seed(1)
# mix <- mx_profiles(df, classes = 2)

mx_mixture_gradients <- function(x, ...){
  paramLabels <- names(OpenMx::omxGetParameters(x))
  numParam <- length(paramLabels)
  custom.compute <- OpenMx::mxComputeSequence(list(OpenMx::mxComputeNumericDeriv(checkGradient = FALSE,
                                                                 hessian = FALSE), OpenMx::mxComputeReportDeriv()))
  do.call(rbind, lapply(1:nrow(x@data$observed), function(i) {
    tryCatch({
      OpenMx::mxRun(OpenMx::mxModel(
        x, custom.compute, OpenMx::mxData(x@data$observed[i, , drop = FALSE], "raw")
      ), silent = TRUE)$output$gradient
    },
    error = function(e) {
        rep(NA, length(numParam))
    })
  }))
}
#   #x <- mix
#   model <- x
#   cprobs <- class_prob(x)
#   Hmat <- cprobs$mostlikely.class
#     Hmatinv <- solve(Hmat)
#     mostlikely <- cprobs$individual[,"predicted"]
#     bchweights <- data.frame(Hmatinv[mostlikely, ])
#     names(bchweights) <- paste0("w", cprobs$sum.posterior$class)
#     df <- cbind(x@data$observed, bchweights)
#     grp_names <- cprobs$sum.posterior$class
#     grps <- lapply(1:ncol(bchweights), function(i){
#       mxModel(model[[grp_names[i]]],
#               name = grp_names[i],
#               data = mxData(observed = df, type = "raw", weight = names(bchweights)[i]),
#               fitfunction = mxFitFunctionML())
#     })
#     grps <- do.call(mxModel, c(list(model = "mg", mxFitFunctionMultigroup(grp_names), grps)))
#
#     out <- try(run_mx(grps), silent = TRUE)
#     attr(out, "tidySEM") <- "BCH"
#     if(!inherits(out, "try-error")){
#       return(out)
#     }
#     NULL
#   }
# }
#
#
#
#
