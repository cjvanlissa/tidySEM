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
#' @description Converts models specified using lavaan syntax to RAM models for
#' \code{OpenMx}.
#' @details The procedure is as follows:
#' \enumerate{
#'  \item Apply \code{\link[lavaan]{lavaanify}} to the \code{model}. The default
#'  arguments to \code{\link[lavaan]{lavaanify}} correspond to those of the
#'  \code{\link[lavaan]{sem}} function.
#'  \item Convert each row of the resulting lavaan parameter table to a
#'  \code{\link[OpenMx]{mxPath}}.
#'  \item Apply \code{\link[OpenMx]{mxModel}} to the \code{mxPath}s to create
#'  an \code{OpenMx} model using RAM specification
#' }
#' @param x A character vector, describing the user-specified model using
#' the lavaan model syntax.
#' @param ... Parameters passed on to other functions.
#' @return Returns a new MxModel object \code{\link[OpenMx]{mxModel}}.
#' @examples
#' lsub("a{C}", 1:3)
#' @rdname lsub
#' @export
#' @importFrom lavaan lavaanify
#' @importFrom OpenMx mxModel
# @importFrom OpenMx mxAutoStart mxData mxExpectationMixture mxPath
# @importFrom OpenMx mxFitFunctionML mxMatrix mxModel mxRun mxTryHard
# @importFrom OpenMx omxAssignFirstParameters mxCompare mxFitFunctionMultigroup
# @importFrom lavaan mplus2lavaan.modelSyntax
# @importFrom stats cutree dist hclust
# @importFrom utils capture.output
as_ram <- function(x, ...){
  defaults <- list(int.ov.free = TRUE, int.lv.free = FALSE, auto.fix.first = FALSE,
                   auto.fix.single = TRUE, auto.var = TRUE, auto.cov.lv.x = TRUE,
                   auto.efa = TRUE, auto.th = TRUE, auto.delta = TRUE, auto.cov.y = TRUE)
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
  lavtab <- eval.parent(cl)
  # Remove defined parameters
  lavtab <- lavtab[!(lavtab$group == 0), ]
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
