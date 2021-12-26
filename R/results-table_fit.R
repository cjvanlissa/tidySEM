#' Print model fit table formatted for publication
#'
#' Takes a model object, extracts model fit information, and formats it as a
#' publication-ready table.
#' @param x A model object for which a method exists.
# @param columns A character vector of columns to retain from the results
# section. If this is set to \code{NULL}, all available columns are returned.
# @param digits Number of digits to round to when formatting numeric columns.
#' @param ... Arguments passed to other functions.
#' @return A data.frame of formatted results.
#' @author Caspar J. van Lissa
#' @family Reporting tools
#' @keywords reporting
#' @export
#' @examples
#' library(lavaan)
#' HS.model <- '  visual =~ x1 + x2 + x3
#'                textual =~ x4 + x5 + x6
#'                speed   =~ x7 + x8 + x9 '
#' fit <- cfa(HS.model,
#'            data = HolzingerSwineford1939,
#'            group = "school")
#' table_fit(fit)
table_fit <- function(x, ...){
  UseMethod("table_fit")
}

#' @method table_fit mixture_list
#' @export
table_fit.mixture_list <- function(x, ...) {
  out <- lapply(x, table_fit)
  out <- bind_list(out)
  if(!is.null(rownames(out))){
    out <- cbind(Name = rownames(out), out)
    rownames(out) <- NULL
  }
  out <- .renamefits(out, "mx")
  class(out) <- c("tidy_fit", class(out))
  out
}

#' @importFrom utils getFromNamespace
mplus_as_list <- getFromNamespace("mplus_as_list", "MplusAutomation")

#' @method table_fit mplusObject
#' @export
table_fit.mplusObject <- function(x, ...) {
  cl <- match.call()
  modelList <- mplus_as_list(x)
  mixtures <- sapply(modelList, function(x) {
    !is.null(x$input$analysis[["type"]])
  })
  if(any(mixtures)){
    mixtures[mixtures] <- sapply(modelList[mixtures], function(x) {
      grepl("mixture", tolower(x$input$analysis$type))
    })
  }
  out <- vector("list", length = length(modelList))
  #if(MplusAutomation:::check_mixtures(MplusAutomation:::mplus_as_list(x)))
  cl[["x"]] <- NULL
  if(!hasArg("keepCols")){
    cl["keepCols"] <- list(NULL)
  }
  if(any(!mixtures)){
    cl[[1L]] <- quote(SummaryTable)
    out[which(!mixtures)] <- lapply(which(!mixtures), function(i){
      cl[["modelList"]] <- modelList[[i]]
      eval.parent(cl)
    })
  }
  if(any(mixtures)){
    cl[[1L]] <- quote(mixtureSummaryTable)
    out[which(mixtures)] <- lapply(which(mixtures), function(i){
      cl[["modelList"]] <- modelList[[i]]
      eval.parent(cl)
    })
  }
  if(length(out) > 1){
    out <- bind_list(out)
  } else {
    out <- out[[1]]
  }
  out <- out[, !colSums(is.na(out)) == nrow(out)]
  out <- .renamefits(out, "mplus")
  class(out) <- c("tidy_fit", class(out))
  out
}

#' @method table_fit mplus.model.list
#' @export
table_fit.mplus.model.list <- table_fit.mplusObject
#' @method table_fit mplus.model
#' @export
table_fit.mplus.model <- table_fit.mplusObject
#' @method table_fit mplusObject
#' @export
table_fit.mplusObject <- table_fit.mplusObject
#' @method table_fit mixture.list
#' @export
table_fit.mixture.list <- table_fit.mplusObject
#' @method table_fit model.list
#' @export
table_fit.model.list <- table_fit.mplusObject

#' @method table_fit MxModel
#' @export
table_fit.MxModel <- function(x, ...) {
  if(is.null(attr(x, "tidySEM"))) attr(x, "tidySEM") <- "default"
  out <- switch(attr(x, "tidySEM")[1],
                "list" = sapply(x, function(i){ table_fit(i, ...)}),
                "mixture" = calc_fitindices(x, type = "mixture", ...),
                calc_fitindices(x, ...))
  out <- as.data.frame(out)
  out <- out[, !colSums(is.na(out)) == nrow(out)]
  if(!"LL" %in% names(out) & "Minus2LogLikelihood" %in% names(out)) out$LL <- out$Minus2LogLikelihood/-2
  out <- .renamefits(out, "mx")
  class(out) <- c("tidy_fit", class(out))
  out
}

.table_fit_mx <- function(x){
  out <- tryCatch({
    suppressMessages({
    satmod <- mxRefModels(x, run = TRUE)
    summary(x, refModels = satmod)
    })
  },
  error = function(e){
    summary(x)
  },
  warning = function(w){
    summary(x)
  })
  classes <- sapply(out, function(i){class(i)[1]})
  is_num <- sapply(out, inherits, what = "numeric")
  is_char <- sapply(out, inherits, what = "character")
  atomvect <- sapply(out, function(i){ is.atomic(i) & length(i) == 1})
  fits <- out[atomvect]

  if(!is.null(out[["RMSEACI"]])){
    if(any(!is.na(out[["RMSEACI"]]))){
      names(out[["RMSEACI"]]) <- paste0("RMSEA.", names(out[["RMSEACI"]]))
      fits <- c(fits, out[["RMSEACI"]])
    }
  }
  if(any(endsWith(names(fits), "Message"))) fits[["warning"]] <- TRUE
  dropthese <- c("wasRun", "stale", "infoDefinite", "conditionNumber", "maxAbsGradient",
                 "fitUnits", "fit", "CI.Requested", "statusCode", "maxRelativeOrdinalError",
                 "timestamp", "frontendTime", "backendTime", "independentTime",
                 "wallTime", "cpuTime", "optimizerEngine", "verbose", "npsolMessage"
  )
  fits <- fits[!names(fits) %in% dropthese]
  out <- as.data.frame(fits)
  rownames(out) <- NULL
  out
}

#' @importFrom lavaan fitmeasures
#' @method table_fit lavaan
#' @export
table_fit.lavaan <- function(x, ...){
  out <- data.frame(Name = as.character(substitute(x)), t(as.data.frame(fitmeasures(x))))
  rownames(out) <- NULL
  out <- .renamefits(out)
  class(out) <- c("tidy_fit", class(out))
  out
}

.fitmeasuretable <- data.frame(
  newname = c("Parameters", "LL", "n", "AIC", "BIC", "Name", "df"),
  lavaan = c("npar", "logl", "ntotal", "AIC", "BIC", "name", "df"),
  mx = c("estimatedParameters", "LogLik", "numObs", "AIC.Mx", "BIC.Mx", "Name", "degreesOfFreedom"),
  mplus = c("Parameters", "LL", "Observations", "AIC", "BIC", "Title", "DF")
)

.renamefits <- function(x, from = "lavaan"){
  replacethese <- which(.fitmeasuretable[[from]] %in% names(x))
  ftab <- .fitmeasuretable[replacethese, , drop = FALSE]
  names(x)[match(ftab[[from]], names(x))] <- ftab$newname
  x
}
