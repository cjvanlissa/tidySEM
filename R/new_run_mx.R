new_run_mx.MxModel <- function(x, ...){
  if(isTRUE(x@.wasRun)){
    x@.wasRun <- FALSE
  }
  dots <- list(...)
  run_fun <- "OpenMx::mxRun"
  run_args <- list()
  # Determine type of model and what elements are available
  if(!is.null(dots[["data"]])){
    cl <- match.call()
    cl[[1L]] <- str2lang("tidySEM:::mx_add_data")
    x <- eval.parent(cl)
    dots[["data"]] <- NULL
  }
  if(length(x@intervals) > 0){
    run_args[["intervals"]] <- TRUE
  }

  # Different approaches based on model type --------------------------------
  if(!is.null(attr(x, "tidySEM"))){
    if(attr(x, "tidySEM") == "mixture" & length(names(x@submodels)) > 0){
      mix_method <- "annealing"
      if(!is.null(dots[["method"]])){
        mix_method <- dots[["method"]]
      }
      switch(mix_method,
             "hard" = {
               # tryhard
               run_fun <- "OpenMx::mxTryHard"
               run_args <- c(run_args,
                             list(
                               extraTries = 100,
                               intervals=TRUE,
                               silent = TRUE,
                               verbose = FALSE,
                               bestInitsOutput = FALSE,
                               exhaustive = TRUE))
             },
             {
               x <- OpenMx::mxModel(x, OpenMx::mxComputeSimAnnealing())
               res <- suppressWarnings(try(OpenMx::mxRun(x), silent = TRUE))
               # if(tryCatch({res$output[['maxRelativeOrdinalError']] > OpenMx::mxOption(res, "mvnRelEps")}, error = function(e){FALSE})){
               #   message("Model finished with a large ordinal error. Trying `mxTryHardOrdinal()`.")
               #   res <- OpenMx::mxTryHardOrdinal(res)
               # }
               if(inherits(res, "try-error")){
                 message("Simulated annealing failed, suggesting bad starting values or an overly complex model. Trying `mxTryHard()`.")
                 x <- OpenMx::mxTryHard(x)
               } else {
                 x <- res
               }
               x@compute <- NULL
             })
    }
  } else {
    x <- simple_starts(x, type = "ULS")
  }
  if(isFALSE(x@.wasRun)){
    run_args <- c(
      list(
        "name" = str2lang(run_fun),
        "model" = x
      ),
      run_args,
      dots[which(names(dots) %in% formalArgs(fun_from_pack(run_fun)))])
    cl <- as.call(run_args)
    res <- eval(cl)
  } else {
    res <- x
  }
  if(tryCatch({res$output[['maxRelativeOrdinalError']] > OpenMx::mxOption(res, "mvnRelEps")}, error = function(e){FALSE})){
    message("Larger ordinal error than expected. Trying `mxTryHardOrdinal()`.")
    res <- try(OpenMx::mxTryHardOrdinal(x), silent = TRUE)
  }
  return(res)
}
