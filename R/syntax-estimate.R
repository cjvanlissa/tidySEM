#' @title Estimate tidy_sem using 'OpenMx'
#' @description This function is a wrapper for the \code{\link{as_ram}}
#' and \code{\link{run_mx}} functions.
#' @param x An object of class \code{tidy_sem}.
#' @param ... Additional parameters passed to the estimating function.
#' @return An object of class \code{MxModel}.
#' @examples
#' df <- iris[1:4]
#' names(df) <- paste0("x_", 1:4)
#' model <- tidy_sem(df)
#' model <- measurement(model)
#' res <- estimate_mx(model)
#' summary(res)
#' @rdname estimate_mx
#' @export
estimate_mx <- function(x, ...){
  if(!has_data(x)) return(NULL)
  if(!has_syntax(x)) return(NULL)
  Args <- c(list(
    x = as_ram(x$syntax),
    data = x$data),
    list(...))
  do.call(run_mx, Args)
}


#' @title Estimate tidy_sem using 'lavaan'
#' @description This function is a wrapper for the \code{\link[lavaan]{lavaan}}
#' estimating functions. By default, the wrapper uses \code{\link[lavaan]{sem}},
#' but users can also specify \code{\link[lavaan]{lavaan}},
#' \code{\link[lavaan]{cfa}}, or \code{\link[lavaan]{growth}}.
#' @param x An object of class \code{tidy_sem}.
#' @param func The \code{\link[lavaan]{lavaan}} modeling function to invoke,
#' Default: 'sem'.
#' @param ... Additional parameters passed to the estimating function.
#' @return An object of class \code{lavaan}.
#' @examples
#' library(lavaan)
#' model <- tidy_sem(iris, "\\.")
#' model <- measurement(model)
#' res <- estimate_lavaan(model)
#' summary(res)
#' @rdname estimate_lavaan
#' @importFrom lavaan sem
#' @export
estimate_lavaan <- function(x, func = "sem", ...){
  if(!has_data(x)) return(NULL)
  if(!has_syntax(x)) return(NULL)
  Args <- c(list(
    model = x$syntax, #do.call(as_lavaan, list(x = x))
    data = x$data),
    list(...))
  do.call(func, Args)
}

#' @title Estimate tidy_sem using 'Mplus'
#' @description This function is a wrapper for the functions
#' \code{\link[MplusAutomation]{mplusObject}} and
#' \code{\link[MplusAutomation]{mplusModeler}}. Using this function requires
#' 'Mplus' to be installed.
#' @param x An object of class \code{tidy_sem}.
#' @param ... Additional parameters passed to
#' \code{\link[MplusAutomation]{mplusObject}} and
#' \code{\link[MplusAutomation]{mplusModeler}}. These arguments are matched to
#' the correct function by name. The arguments \code{rdata}, and \code{MODEL}
#' cannot be edited, as they are determined from the \code{tidy_sem} object.
#' @details The arguments \code{dataout}, \code{modelout}, and \code{run} are
#' optional. If these are not specified, the model will be run in
#' \code{\link{tempdir}}.
#' @return An object of class \code{mplusObject}.
#' @examples
#' library(MplusAutomation)
#' model <- tidy_sem(iris, "\\.")
#' model <- measurement(model)
#' \dontrun{
#'   estimate_mplus(model, run = 0L)
#' }
#' @rdname estimate_mplus
#' @export
#' @importFrom MplusAutomation mplusObject mplusModeler
estimate_mplus <- function(x, ...){
  if(!has_data(x)) return(NULL)
  if(!has_syntax(x)) return(NULL)
  dots <- list(...)
  Args_mplusobject <- dots[which(names(dots) %in% c("TITLE", "DATA", "VARIABLE", "DEFINE", "MONTECARLO", "MODELPOPULATION", "MODELMISSING", "ANALYSIS", "MODELINDIRECT", "MODELCONSTRAINT", "MODELTEST", "MODELPRIORS", "OUTPUT", "SAVEDATA", "PLOT", "usevariables", "autov", "imputed"))]
  Args_mplusobject$rdata <- x$data
  Args_mplusobject$MODEL <- do.call(as_mplus, list(x = x))
  Args_mplusmodeler <- dots[which(names(dots) %in% c("dataout", "modelout", "run", "check", "varwarnings", "Mplus_command", "writeData", "hashfilename"))]
  Args_mplusmodeler$object <- do.call(mplusObject, Args_mplusobject)
  run_in_temp <- FALSE
  if(is.null(Args_mplusmodeler[["dataout"]]) & is.null(Args_mplusmodeler[["modelout"]])){
    run_in_temp <- TRUE
    old_wd <- getwd()
    tmpdir <- file.path(tempdir(), "mplus")
    dir.create(tmpdir)
    setwd(tmpdir)
    on.exit(setwd(old_wd))
    Args_mplusmodeler$modelout <- "mplus_model.inp"
  }
  if(is.null(Args_mplusmodeler[["run"]])){
    Args_mplusmodeler$run <- 1L
  }
  out <- tryCatch({do.call(mplusModeler, Args_mplusmodeler)}, error = function(e){return(NULL)})
  if(run_in_temp){
    setwd(old_wd)
    unlink(tmpdir, recursive = TRUE)
  }
  out
}
