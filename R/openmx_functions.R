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


# @importFrom utils getFromNamespace
#vnames <- getFromNamespace("vnames", "lavaan")


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
# @param groups Optional character vector for multi-group models, containing
# either group names, or if
# \code{x} is an object that contains data, the name of the column that
# contains a grouping
# variable.
# @param data Optional data.frame to include in the model.
#' @param ... Parameters passed on to other functions.
#' @return Returns an \code{\link[OpenMx]{mxModel}}.
#' @examples
#' as_ram("y ~ x")
#' @rdname as_ram
#' @export
#' @importFrom lavaan lavaanify
# @importFrom OpenMx mxModel imxReportProgress
# @importFrom OpenMx mxAutoStart mxData mxExpectationMixture mxPath
# @importFrom OpenMx mxFitFunctionML mxMatrix mxModel mxRun mxTryHard
# @importFrom OpenMx omxAssignFirstParameters mxCompare mxFitFunctionMultigroup
#' @importFrom lavaan mplus2lavaan.modelSyntax
#' @importFrom stats cutree dist hclust
#' @importFrom methods formalArgs
# @import OpenMx
as_ram <- function(x, ...){
  if(!isTRUE(requireNamespace("OpenMx", quietly = TRUE))) {
    return(NULL)
  }
  UseMethod("as_ram", x)
}

#' @method as_ram character
#' @export
as_ram.character <- function(x, groups = NULL, data = NULL, ...){
  # defaults <- list(int.ov.free = TRUE, int.lv.free = FALSE, auto.fix.first = FALSE,
  #                  auto.fix.single = TRUE, auto.var = TRUE, auto.cov.lv.x = TRUE,
  #                  auto.efa = TRUE, auto.th = TRUE, auto.delta = TRUE, auto.cov.y = TRUE,
  #                  meanstructure = TRUE)
  dots <- list(...)
  Args_lav <- lav_from_dots(...)
  cl <- match.call()
  cl[names(Args_lav)] <- Args_lav
  cl[["model"]] <- x
  if(!is.null(groups)){
    if(length(groups) == 1 & !is.null(data)){
      cl[["ngroups"]] <- length(unique(data[[groups]]))
    } else {
      cl[["ngroups"]] <- length(groups)
    }
  }
  cl <- cl[c(1L, which(names(cl) %in% c("model", "ngroups", "data", names(Args_lav))))]#lavaan_dots, names(defaults))))]
  cl[[1L]] <- str2lang("tidySEM:::tidysem_lavaanify")
  x <- eval.parent(cl)
  cl <- match.call()
  cl[[1L]] <- str2lang("tidySEM::as_ram")
  cl[["x"]] <- x
  eval.parent(cl)
}

#' @method as_ram tidy_sem
#' @export
as_ram.tidy_sem <- function(x, ...){
  cl <- match.call()
  cl[[1L]] <- str2lang("tidySEM::as_ram")
  cl[["x"]] <- x$syntax
  if(is.null(cl[["data"]])) cl[["data"]] <- x$data
  gv <- group_var(x)
  if(!is.null(gv)){
    cl[["groups"]] <- gv
  }
  eval.parent(cl)
}

#' @method as_ram data.frame
#' @export
as_ram.data.frame <- function(x, groups = NULL, data = NULL, ...){
  if(!all(c("lhs", "rhs", "op", "free", "ustart") %in% names(x))){
    stop("Not a valid lavaan parameter table.")
  }
  groupnames <- groups
  usedata <- !is.null(data)
  if(length(groups) == 1 & usedata){
    groupnames <- as.character(unique(data[[groups]]))
  }
  if(!is.null(x[["group"]])){
    x <- x[!(x$group == 0), ]
    if(length(unique(x[["group"]])) > 1){
      cl <- match.call()
      grps <- lapply(1:length(groupnames), function(i){
        cl[["x"]] <- x[x$group == i, -which(names(x) == "group"), drop = FALSE]
        if(usedata){
          cl[["data"]] <- data[data[[groups]] == groupnames[i], -which(names(data) == groups), drop = FALSE]
        }
        cl[[1L]] <- str2lang("tidySEM::as_ram")
        out <- eval.parent(cl)
        Args <- list(
          out,
          name = groupnames[i],
          OpenMx::mxFitFunctionML()
        )
        # if(usedata) {
        #   Args <-
        #     c(Args, list(mxData(data[data[[groups]] == groupnames[i], -which(names(data) == groups), drop = FALSE], type = "raw")))
        # }
        do.call(OpenMx::mxModel, Args)
      })
      grps <- do.call(OpenMx::mxModel, c(list(model = "mg", OpenMx::mxFitFunctionMultigroup(groupnames), grps)))
      return(grps)
    }
  }
  dots <- list(...)
  if("threshold_method" %in% names(dots)){
    threshold_method <- dots[["threshold_method"]]
    dots[["threshold_method"]] <- NULL
  } else {
    threshold_method <- "mx_threshold"
  }
  if("threshold_data" %in% names(dots)){
    threshold_data <- dots[["threshold_data"]]
    dots[["threshold_data"]] <- NULL
  }
  lavtab <- x
  # Parse categorical variables
  cats <- which(lavtab$op %in% c("|", "~*~"))
  catlist <- NULL
  if(length(cats) > 0){
    cattab <- lavtab[cats, , drop = FALSE]
    cattab$label[cattab$label == ""] <- NA
    lavtab <- lavtab[-cats, ]
    #if(threshold_method == "mx_threshold"){
      threshtab <- cattab[cattab$op == "|", , drop = FALSE]
      catvars <- unique(threshtab$lhs)
      num_thresholds <- sapply(catvars, function(v){length(unique(threshtab$rhs[threshtab$lhs == v]))})
      maxthresh <- max(num_thresholds)
      if(any(!is.na(threshtab$ustart)) & !all(!is.na(threshtab$ustart))){
        message("User starts for categorical variable thresholds were ignored; either specify ALL thresholds by hand, or remove constraints. You can apply constraints to the `mxModel` object returned by `as_ram()`.")
      }
      if(all(!is.na(threshtab$ustart))){
        tvalues <- unlist(lapply(catvars, function(v){
          tmp <- threshtab[threshtab$lhs == v, ]
          tmp$ustart[order(tmp$rhs)]
          }))

      } else {
        if(usedata){
          tvalues <- mx_data_quantiles(data[, catvars, drop = FALSE])
        } else {
          tvalues <- OpenMx::mxNormalQuantiles(nBreaks = num_thresholds)
        }
      }
      free <- matrix(FALSE, nrow = maxthresh, ncol = length(catvars))
      for(v in seq_along(catvars)){
        free[1:num_thresholds[v], v] <- TRUE
      }
      catlist <- mx_threshold(vars = catvars, nThresh = num_thresholds, free = free, values = tvalues, labels = threshtab$label)

    # } else {
    #   thresh <- mx_thresholds(threshold_data)
    #   catlist <- thresh
    #     #list(
    #     #OpenMx::mxPath(from = "one", to = names(data), free = FALSE, values = 0),
    #     #OpenMx::mxPath(from = names(data), to = names(data), free = FALSE, values = 1, arrows = 2),
    #
    #   #)
    #   # do this: c1$expectation$thresholds <- "Thresholds"
    # }


  }
  # Parse defined parameters
  defined <- NULL
  defined_parameters <- which(lavtab$block == 0 & lavtab$plabel == "")
  if(length(defined_parameters) > 0){
    defined <- lavtab[defined_parameters, , drop = FALSE]
    lavtab <- lavtab[-defined_parameters, ]

  }
  # Identify observed and latent
  vnames <- lavaan::lavNames(object = lavtab, type = "all")

  if(!is.null(vnames[["lv"]])){
    latent <- unlist(vnames[["lv"]])
  } else {
    latent <- vector("character")
  }
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
    do.call(fun_from_pack("OpenMx::mxPath"), Args)
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
  if(!is.null(catlist)){
    path_list <- c(path_list, catlist)
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
  out <- do.call(OpenMx::mxModel, c(mxmodel_args,
                     path_list))
  if(!is.null(catlist)){
    if(!threshold_method == "mxThreshold"){
      out$expectation$thresholds <- "Thresholds"
    }
  }
  # Add data if available
  if(usedata){
    cl <- match.call()
    cl[[1L]] <- str2lang("tidySEM:::mx_add_data")
    cl[["x"]] <- out
    out <- eval.parent(cl)
  }
  out
}


.mx_thresholds_internal <- function(df, ...){
  ord_vars <- sapply(df, inherits, what = "ordered")
  ord_var_nam <- names(df)[ord_vars]
  lapply(ord_var_nam, function(nam){
    tab <- table(df[[nam]])
    startvals <- qnorm(cumsum(prop.table(tab)[-length(tab)]))
    OpenMx::mxThreshold(vars = nam, nThresh = (length(tab)-1), free = TRUE, values = startvals)
  })
}
