#' @title Run as OpenMx model with sensible defaults
#' @description This convenience function runs objects for which a method exists
#' using OpenMx, with sensible defaults. It is intended for use with
#' \code{tidySEM}. For instance, it will convert a \code{tidySEM} object to
#' a \code{mxModel} and run it, and it will try to ensure convergence for
#' mixture models created using \code{\link{mx_mixture}}.
#' Knowledgeable users may want to run models manually.
#' @param x An object for which a method exists.
#' @param ... Parameters passed on to other functions.
#' @return Returns an \code{\link[OpenMx]{mxModel}} with free parameters updated
#' to their final values.
#' @examples
#' df <- iris[1:3]
#' names(df) <- paste0("X_", 1:3)
#' run_mx(measurement(tidy_sem(df), meanstructure = TRUE))
#' @rdname run_mx
#' @export
# @importFrom OpenMx mxAutoStart mxData mxExpectationMixture mxPath
# @importFrom OpenMx mxFitFunctionML mxMatrix mxModel mxRun mxTryHard
# @importFrom OpenMx omxAssignFirstParameters mxCompare mxFitFunctionMultigroup
# @importFrom lavaan mplus2lavaan.modelSyntax
# @importFrom stats cutree dist hclust
# @importFrom OpenMx omxDefaultComputePlan mxComputeSimAnnealing
run_mx <- function(x, ...){
  s <- search()
  if(!any(grepl("OpenMx", s, fixed = TRUE))){
    message("Please run `library(OpenMx) before running any OpenMx models.`")
    return(NULL)
  }
  UseMethod("run_mx", x)
}

#' @method run_mx tidy_sem
#' @export
run_mx.tidy_sem <- function(x, ...){
  cl <- match.call()
  cl[[1L]] <- str2lang("tidySEM::as_ram")
  cl[["x"]] <- eval.parent(cl)
  cl[[1L]] <- str2lang("tidySEM::run_mx")
  cl[["data"]] <- x$data
  eval.parent(cl)
}

#' @method run_mx MxModel
#' @export
run_mx.MxModel <- function(x, ...){
  # Check if the model has already been run. run_mx will always re-run models
  if(isTRUE(x@.wasRun)){
    x@.wasRun <- FALSE
  }

  # Capture additional arguments
  dots <- list(...)

  # Append data to model if provided in dots
  if(!is.null(dots[["data"]])){
    x <- mx_add_data(x, ...)
  }

  # Default run function and arguments
  run_fun <- "OpenMx::mxRun"
  run_args <- list()
  if(!"silent" %in% names(dots)) run_args$silent <- TRUE

  # Check if confidence intervals are required
  if(length(x@intervals) > 0){
    run_args[["intervals"]] <- TRUE
  }

  # If this is a mixture model with >1 class, use simulated annealing
  if(isTRUE(attr(x, "tidySEM") == "mixture") & length(names(x@submodels)) > 0){
    # Has to be re-run again later, to get $expectation$output$weights
    x <- OpenMx::mxModel(x, OpenMx::mxComputeSimAnnealing())
    x <- suppressWarnings(try(OpenMx::mxRun(x), silent = TRUE))
    # Reset simulated annealing
    x@compute <- NULL
  }

  # Check if the model has start values (this could be improved)
  if(!has_startvalues(x)){
    x <- simple_starts(x, type = "ULS")
  }

  # Now run the model
  x <- run_with_args(x = x, run_fun = run_fun, run_args = run_args, ...)
  # Use mxTryHard if conversion failed
  if(inherits(x, "try-error")){
    message("Model estimation failed. Trying `mxTryHard()`.")
    x <- run_with_args(x = x, run_fun = "OpenMx::mxTryHard", run_args = run_args, ...)
  }
  # Use mxTryHardOrdinal if ordinal error is the problem
  if(tryCatch({x$output[['maxRelativeOrdinalError']] > OpenMx::mxOption(x, "mvnRelEps")}, error = function(e){FALSE})){
    message("Larger ordinal error than expected. Trying `mxTryHardOrdinal()`.")
    x <- run_with_args(x = x, run_fun = "OpenMx::mxTryHardOrdinal", run_args = run_args, ...)
  }
  return(x)
}

has_startvalues <- function(x){
  isFALSE(all(OpenMx::omxGetParameters(x) == 0))
}

run_with_args <- function(x, run_fun, run_args, ...){
  dots <- list(...)
  run_args <- c(
    list(
      "name" = str2lang(run_fun),
      "model" = x
    ),
    run_args,
    dots[which(names(dots) %in% formalArgs(fun_from_pack(run_fun)))])
  cl <- as.call(run_args)
  suppressWarnings(eval(cl))
}


mx_add_data <- function(x, data, ...){
  dots <- list(...)
  if(inherits(x$fitfunction, "MxFitFunctionMultigroup")){
    if("groups" %in% names(dots)){
      if(length(dots[["groups"]]) != 1 | !(dots[["groups"]][1] %in% names(data))){
        stop("The argument 'groups' should contain the name of a column in 'data'.")
      }
      groupnames <- as.character(unique(data[[dots[["groups"]]]]))
      if(!length(groupnames) == length(x$fitfunction$groups)){
        stop("The column indicated by 'groups' contains ", length(groupnames), " unique values, but the model was specified for ", length(x$fitfunction$groups), " groups.")
      }
      if(all(groupnames %in% x$fitfunction$groups)){
        groupnames <- x$fitfunction$groups # To get correct order
      }
      for(i in 1:length(groupnames)){
        x[[x$fitfunction$groups[i]]] <-
          OpenMx::mxModel(x[[x$fitfunction$groups[i]]],
                  OpenMx::mxData(data[data[[dots[["groups"]]]] == groupnames[i], -which(names(data) == dots[["groups"]]), drop = FALSE],
                         type = "raw"))
      }
    }
  } else {
    x <- OpenMx::mxModel(x, OpenMx::mxData(data, type = "raw"))
  }
  x
}


#' @title Run as lavaan model
#' @description This convenience function runs objects for which a method exists
#' using lavaan. It is intended for use with
#' \code{tidySEM}, and passes the \code{$syntax} and \code{$data} elements of a
#' \code{tidy_sem} object on to \code{\link[lavaan]{lavaan}}.
#' @param x An object for which a method exists.
#' @param ... Parameters passed on to other functions.
#' @return Returns a lavaan object.
#' @examples
#' df <- iris[1:3]
#' names(df) <- paste0("X_", 1:3)
#' run_lavaan(measurement(tidy_sem(df), meanstructure = TRUE))
#' @rdname run_lavaan
#' @export
# @importFrom OpenMx mxAutoStart mxData mxExpectationMixture mxPath
# @importFrom OpenMx mxFitFunctionML mxMatrix mxModel mxRun mxTryHard
# @importFrom OpenMx omxAssignFirstParameters mxCompare mxFitFunctionMultigroup
# @importFrom lavaan mplus2lavaan.modelSyntax
# @importFrom stats cutree dist hclust
# @importFrom utils capture.output
run_lavaan <- function(x, ...){
  UseMethod("run_lavaan", x)
}

#' @method run_lavaan tidy_sem
#' @export
run_lavaan.tidy_sem <- function(x, ...){
  cl <- match.call()
  cl[[1L]] <- str2lang("lavaan::lavaan")
  cl[["model"]] <- as_lavaan(x)
  cl[["x"]] <- NULL
  cl[["data"]] <- x$data
  eval.parent(cl)
}
