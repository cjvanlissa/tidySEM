#' @method print tidy_sem
#' @export
print.tidy_sem <- function(x, ...){
  has_element <- sapply(c("dictionary", "data", "syntax"), function(i){
    c("\033[0;37mo  \033[0m  ", "\033[0;32mv  \033[0m  ")[(!is.null(x[[i]]))+1]
    })
  cat("A tidy_sem object\n")
  colmsg(has_element["dictionary"], "Has a data dictionary")
  colmsg(has_element["data"], "Has a data.frame attached")
  colmsg(success = has_element["syntax"], "Has syntax")
  #if(!is.null(x[["data"]])) msg <- paste0(msg, " ")
  #has_code <- !is.null(x[["syntax"]])
  #cat(msg)
  #print(x[["dictionary"]])
}

colmsg <- function(..., col = 30) {
  txt <- do.call(paste0, list(...))
  cat(paste0("\033[0;",
             col,
             "m",
             txt,
             "\033[0m",
             "\n"
             ))
}

#' @method print mplusObject
#' @export
print.mplusObject <- function(x, ...){
  if(!is.null(x[["summary"]][["ChiSqM_DF"]]))
  print(summary(x))
}

#' @method print mplus.model
#' @export
print.mplus.model <- print.mplusObject

#' @method summary mplus.model
#' @export
summary.mplus.model <- function(object, verbose = FALSE, ...)
{
  stopifnot(!is.null(object$summaries))
  Args <- as.list(match.call()[-1])
  Args$object <- list(results = object)
  class(Args$object) <- c("mplusObject", class(Args$object))
  do.call(summary, Args)
}

