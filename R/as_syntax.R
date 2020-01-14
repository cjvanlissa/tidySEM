#' @title Convert sem_syntax to Mplus syntax
#' @description Final stage in the tidySEM workflow for syntax generation:
#' Convert the sem_syntax object to Mplus syntax.
#' @param x An object of class \code{sem_syntax}
#' @param ... Additional parameters to be passed to and from functions.
#' @return Character vector.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname as.mplus
#' @export
as.mplus <- function(x, ...){
  UseMethod("as.mplus")
}

#' @method as.mplus sem_syntax
#' @export
as.mplus.sem_syntax <- function(x, ...){
  if("group" %in% names(x$syntax)){
    stop("Develop")
  }
  if("level" %in% names(x$syntax)){
    stop("Develop")
  }
  x$syntax$label <- paste0("lab", 1:nrow(x$syntax))
  browser()
  x$syntax$value[is.na(x$syntax$value)] <- ""
  apply(x$syntax, 1, function(this_row){
    switch(this_row[["op"]],
           "=~" = {
             paste0(this_row[["lhs"]], " BY ", this_row[["rhs"]], ifelse(this_row[["free"]], "*", "@"), this_row[["value"]], " (", this_row[["label"]], ")", ";")
           },
           "~1" = {
             paste0("[", this_row[["lhs"]], ifelse(this_row[["free"]], "*", "@"), this_row[["value"]], "] (", this_row[["label"]], ")", ";")
           },
           "~~" = {
             if(this_row[["lhs"]] == this_row[["rhs"]]){
               paste0(this_row[["lhs"]], ifelse(this_row[["free"]], "*", "@"), this_row[["value"]], " (", this_row[["label"]], ")", ";")
             } else {
               paste0(this_row[["lhs"]], " WITH ", this_row[["rhs"]], ifelse(this_row[["free"]], "*", "@"), this_row[["value"]], " (", this_row[["label"]], ")", ";")
             }
           },
           "~" = {
             paste0(this_row[["lhs"]], " ON ", this_row[["rhs"]], ifelse(this_row[["free"]], "*", "@"), this_row[["value"]], " (", this_row[["label"]], ")", ";")
           }
           )
  })

}

#' @title Convert sem_syntax to lavaan syntax
#' @description Final stage in the tidySEM workflow for syntax generation:
#' Convert the sem_syntax object to lavaan syntax.
#' @param x An object of class \code{sem_syntax}
#' @param ... Additional parameters to be passed to and from functions.
#' @return Character vector.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname as.lavaan
#' @export
as.lavaan <- function(x, ...){
  UseMethod("as.lavaan")
}

#' @method as.lavaan sem_syntax
#' @export
#' @importFrom lavaan mplus2lavaan.modelSyntax
as.lavaan.sem_syntax <- function(x, ...){
  tab <- mod$syntax
  tab$free <- as.numeric(as.logical(tab$free))
  names(tab)[match("value", names(tab))] <- "ustart"
  if(any(c("label", "group", "level") %in% names(tab))){
    stop("Develop")
  }
  tab[, 1:5]
}

as.lavaan.sem_syntax3 <- function(x, ...){
  if("group" %in% names(x$syntax)){
    stop("Develop")
  }
  if("level" %in% names(x$syntax)){
    stop("Develop")
  }
  x$syntax$label <- paste0("lab", 1:nrow(x$syntax))
  x$syntax$rhs[x$syntax$op == "~1"] <- 1
  x$syntax$op[x$syntax$op == "~1"] <- "~"
  paste0(x$syntax[["lhs"]], " ", x$syntax[["op"]], " ", ifelse(x$syntax[["free"]], x$syntax[["label"]], x$syntax[["value"]]), " * ", x$syntax[["rhs"]])
}

as.lavaan.sem_syntax2 <- function(x, ...){
  Args <- as.list(match.call()[-1])
  mplus_syntax <- do.call(as.mplus, Args)
  out <- paste0(mplus_syntax, collapse = "\n")
  out <- mplus2lavaan.modelSyntax(out)
  out <- strsplit(out, "\n")[[1]]
  means <- grepl("^0 ~", out)
  if(any(means)){
    mplus_syntax[means]
    out[means] <- gsub("^0", gsub("^\\[(.+)[@\\*].*$", "\\1", mplus_syntax[means]), out[means])
  }
  out
}
