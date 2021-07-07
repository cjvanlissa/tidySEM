#' @title Apply pattern replacement over a vector
#' @description \code{lsub} returns a list of the same length as
#' \code{replacement}, each element of which is the result of applying
#' \code{\link[base:grep]{gsub}} to \code{x} using \code{\link{lapply}}.
#' @param x A character vector where matches are sought.
#' @param replacement a character vector of length 1 or more. Each element is
#' applied to \code{x} in turn. Default: NULL
#' @param pattern A character string containing a regular expression (or
#' character string when \code{fixed = TRUE}). Default: \code{'{C}'}.
#' @param fixed logical. If TRUE, pattern is a string to be matched as is.
#' Default: TRUE
#' @param ... Parameters passed on to \code{\link[base:grep]{gsub}}.
#' @return A list of results returned by \code{\link[base:grep]{gsub}}.
#' @examples
#' lsub("a{C}", 1:3)
#' @rdname lsub
#' @export
lsub <- function(x, replacement = NULL, pattern = "{C}", fixed = TRUE, ...){
  lapply(replacement, gsub, pattern = pattern, x = x, fixed = fixed, ...)
}


#' @importFrom utils getFromNamespace
vnames <- getFromNamespace("vnames", "lavaan")


#' @title Convert lavaan syntax to RAM specification
#' @description Converts SEM models to RAM models for
#' \code{OpenMx}.
#' @details For models specified using lavaan syntax, the procedure is as
#' follows:
#' \enumerate{
#'  \item Apply \code{\link[lavaan]{lavaanify}} to the \code{model}. The default
#'  arguments to \code{\link[lavaan]{lavaanify}} correspond to those of the
#'  \code{\link[lavaan]{sem}} function.
#'  \item Convert each row of the resulting lavaan parameter table to a
#'  \code{\link[OpenMx]{mxPath}}.
#'  \item Apply \code{\link[OpenMx]{mxModel}} to the \code{mxPath}s to create
#'  an \code{OpenMx} model using RAM specification
#' }
#' @param x An object for which a method exists, such as a \code{tidy_sem}
#' object, or character vector describing the user-specified model using
#' the lavaan model syntax.
#' @param ... Parameters passed on to other functions.
#' @return Returns an \code{\link[OpenMx]{mxModel}}.
#' @examples
#' as_ram("y ~ x")
#' @rdname as_ram
#' @export
#' @importFrom lavaan lavaanify
#' @importFrom OpenMx mxModel
#' @importFrom OpenMx mxAutoStart mxData mxExpectationMixture mxPath
#' @importFrom OpenMx mxFitFunctionML mxMatrix mxModel mxRun mxTryHard
#' @importFrom OpenMx omxAssignFirstParameters mxCompare mxFitFunctionMultigroup
#' @importFrom lavaan mplus2lavaan.modelSyntax
#' @importFrom stats cutree dist hclust
#' @importFrom methods formalArgs
as_ram <- function(x, ...){
  UseMethod("as_ram", x)
}

#' @method as_ram character
#' @export
as_ram.character <- function(x, ...){
  defaults <- list(int.ov.free = TRUE, int.lv.free = FALSE, auto.fix.first = FALSE,
                   auto.fix.single = TRUE, auto.var = TRUE, auto.cov.lv.x = TRUE,
                   auto.efa = TRUE, auto.th = TRUE, auto.delta = TRUE, auto.cov.y = TRUE)
  browser()
  dots <- list(...)
  cl <- match.call()
  cl[names(defaults)[!names(defaults) %in% names(cl)]] <- defaults[!names(defaults) %in% names(cl)]
  lavaan_dots <- formalArgs(lavaan::lavaanify)
  lavaan_dots <- lavaan_dots[!lavaan_dots == "model"]
  lavaan_dots <- names(dots)[names(dots) %in% lavaan_dots]
  if(isFALSE(is.null(lavaan_dots))){
    cl[lavaan_dots] <- dots[lavaan_dots]
  }
  cl[["model"]] <- x
  cl[["x"]] <- NULL
  cl[[1L]] <- str2lang("lavaan::lavaanify")
  x <- eval.parent(cl)
  cl <- match.call()
  cl[[1L]] <- quote(as_ram)
  cl[["x"]] <- x
  eval.parent(cl)
}

#' @method as_ram tidy_sem
#' @export
as_ram.tidy_sem <- function(x, ...){
  cl <- match.call()
  cl[[1L]] <- quote(as_ram)
  cl[["x"]] <- x$syntax
  eval.parent(cl)
}

#' @method as_ram data.frame
#' @export
as_ram.data.frame <- function(x, ...){
  if(!all(c("lhs", "rhs", "op", "free", "ustart") %in% names(x))){
    stop("Not a valid lavaan parameter table.")
  }
  dots <- list(...)
  lavtab <- x
  # Remove defined parameters
  # if(any(lavtab$group == 0)){
  #   stop("Develop")
  # }
  defined <- NULL
  defined_parameters <- which(lavtab$block == 0 & lavtab$plabel == "")
  if(length(defined_parameters) > 0){
    defined <- lavtab[defined_parameters, , drop = FALSE]
    lavtab <- lavtab[-defined_parameters, ]

  }
  # Starting values
  #lavtab$ustart[lavtab$op == "~1"] <- 0
  #lavtab$ustart[lavtab$op == "~~"] <- .5
  # Identify observed and latent
  vnames <- vnames(partable = lavtab, type = "all")
  latent <- unlist(vnames[["lv"]])
  obs <- unlist(vnames[["ov"]])
  # Intercept needs rhs
  lavtab$rhs[lavtab$op == "~1"] <- "one"
  lavtab$op[lavtab$op == "~1"] <- "~"
  # Convert lavtab to paths
  mxpath_dots <- names(dots)[names(dots) %in% formalArgs(OpenMx::mxPath)]
  mxpath_dots <- dots[mxpath_dots]
  path_list <- lapply(1:nrow(lavtab), function(i){
    path <- lavtab[i, ]
    Args <- c(list(
      name = "mxPath",
      from = switch(path[["op"]],
                    "=~" = path[["lhs"]],
                    path[["rhs"]]),
      to = switch(path[["op"]],
                  "=~" = path[["rhs"]],
                  path[["lhs"]]),
      connect = "single",
      arrows = switch(path[["op"]],
                      `~~` = 2,
                      1),
      free = !(path[["free"]] == 0),
      values = path[["ustart"]]
    ), mxpath_dots)
    if(!path[["label"]] == "") Args$labels <- path[["label"]]
    do.call(call, Args)
  })
  if(length(defined) > 0){
    path_list <- c(
      path_list,
      lapply(1:nrow(defined), function(i){
        path <- defined[i, ]
        cl <- call(name = "mxAlgebra")
        cl[["expression"]] <- str2lang(path$rhs)
        cl[["name"]] <- path$lhs
        eval(cl)
      })
    )
  }
  # mxModel-specific arguments
  mxmodel_args <- list(
    model = "model",
    type='RAM',
    manifestVars = obs,
    latentVars = latent)
  mxmodel_dots <- names(dots)[names(dots) %in% formalArgs(OpenMx::mxModel)]
  if(isFALSE(is.null(mxmodel_dots))){
    mxmodel_args[mxmodel_dots] <- dots[mxmodel_dots]
  }
  do.call(mxModel, c(mxmodel_args,
                     path_list))
}

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
