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
# @importFrom utils capture.output
run_mx <- function(x, ...){
  UseMethod("run_mx", x)
}

#' @method run_mx tidy_sem
#' @export
run_mx.tidy_sem <- function(x, ...){
  cl <- match.call()
  cl[[1L]] <- quote(run_mx)
  cl[["x"]] <- as_ram(x$syntax)
  cl[["data"]] <- x$data
  eval.parent(cl)
}
#mxRun(mxModel(tmp, mxData(dat, type = "raw"), mxFitFunctionML())) -> test

#' @method run_mx MxModel
#' @export
run_mx.MxModel <- function(x, ...){
  dots <- list(...)
  # Determine type of model and what elements are available
  if(!is.null(dots[["data"]])){
    x <- mxModel(x, mxData(dots[["data"]], type = "raw"))
  }

  # Different approaches based on model type --------------------------------
  if(!is.null(attr(x, "tidySEM"))){
    if(attr(x, "tidySEM") == "mixture"){
      # maybe also try simulated annealing
      return(mxTryHard(x,
                       extraTries = 100,
                       intervals=TRUE,
                       silent = TRUE,
                       verbose = FALSE,
                       bestInitsOutput = FALSE,
                       exhaustive = TRUE))
    }
  }
  mxRun(x)
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
  cl[[1L]] <- quote(lavaan)
  cl[["model"]] <- as_lavaan(x$syntax)
  cl[["x"]] <- NULL
  cl[["data"]] <- x$data
  eval.parent(cl)
}
