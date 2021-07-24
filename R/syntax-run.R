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
#' @importFrom OpenMx omxDefaultComputePlan mxComputeSimAnnealing
run_mx <- function(x, ...){
  UseMethod("run_mx", x)
}

#' @method run_mx tidy_sem
#' @export
run_mx.tidy_sem <- function(x, ...){
  cl <- match.call()
  cl[[1L]] <- quote(as_ram)
  cl[["x"]] <- eval.parent(cl)
  cl[[1L]] <- quote(run_mx)
  cl[["data"]] <- x$data
  eval.parent(cl)
}

#' @method run_mx MxModel
#' @export
run_mx.MxModel <- function(x, ...){
  dots <- list(...)
  run_fun <- "mxRun"
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
    if(attr(x, "tidySEM") == "mixture"){
      mix_method <- "annealing"
      if(!is.null(dots[["method"]])){
        mix_method <- dots[["method"]]
      }
      switch(mix_method,
             "hard" = {
               # tryhard
               run_fun <- "mxTryHard"
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
               x <- mxModel(x, mxComputeSimAnnealing())
               x <- mxRun(x)
               x@compute <- NULL
             })
    }
  } else {
    x <- mxAutoStart(x)
  }
  run_args <- c(
    list(
      "name" = str2lang(run_fun),
      "model" = x
    ),
    run_args,
    dots[which(names(dots) %in% formalArgs(run_fun))])
  cl <- as.call(run_args)
  eval(cl)
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
          mxModel(x[[x$fitfunction$groups[i]]],
                  mxData(data[data[[dots[["groups"]]]] == groupnames[i], -which(names(data) == dots[["groups"]]), drop = FALSE],
                         type = "raw"))
      }
    }
  } else {
    x <- mxModel(x, mxData(data, type = "raw"))
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
