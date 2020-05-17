#' @title Convert tidy_sem to Mplus syntax
#' @description Final stage in the tidySEM workflow for syntax generation:
#' Convert the tidy_sem object to Mplus syntax.
#' @param x An object of class \code{tidy_sem}
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

#' @method as.mplus tidy_sem
#' @export
as.mplus.tidy_sem <- function(x, ...){
  if(!has_syntax(x)) return(NULL)
  if("group" %in% names(x$syntax)){
    stop("Develop")
  }
  if("level" %in% names(x$syntax)){
    stop("Develop")
  }
  x$syntax$free <- as.logical(x$syntax$free)
  x$syntax$lhs <- gsub(".", "_", x$syntax$lhs, fixed = TRUE)
  x$syntax$rhs <- gsub(".", "_", x$syntax$rhs, fixed = TRUE)
  #x$syntax$label <- paste0("lab", 1:nrow(x$syntax))
  x$syntax$value[is.na(x$syntax$value)] <- ""
  apply(x$syntax, 1, function(this_row){
    this_free <- as.logical(this_row[["free"]])
    switch(this_row[["op"]],
           "=~" = {
             paste0(this_row[["lhs"]],
                    " BY ",
                    this_row[["rhs"]],
                    if(!this_free){paste0("@", this_row[["value"]])},
                    if(!this_row[["label"]] == ""){paste0(" (", this_row[["label"]], ")")},
                    ";")
           },
           "~1" = {
             paste0("[",
                    this_row[["lhs"]],
                    if(!this_free){paste0("@", this_row[["value"]])},
                    "]",
                    if(!this_row[["label"]] == ""){paste0("(", this_row[["label"]], ")")},
                    ";")
           },
           "~~" = {
             if(this_row[["lhs"]] == this_row[["rhs"]]){
               paste0(this_row[["lhs"]],
                      if(!this_free){paste0("@", this_row[["value"]])},
                      if(!this_row[["label"]] == ""){paste0(" (", this_row[["label"]], ")")},
                      ";")
             } else {
               paste0(this_row[["lhs"]],
                      " WITH ",
                      this_row[["rhs"]],
                      if(!this_free){paste0("@", this_row[["value"]])},
                      if(!this_row[["label"]] == ""){paste0(" (", this_row[["label"]], ")")},
                      ";")
             }
           },
           "~" = {
             paste0(this_row[["lhs"]],
                    " ON ",
                    this_row[["rhs"]],
                    if(!this_free){paste0("@", this_row[["value"]])},
                    if(!this_row[["label"]] == ""){paste0(" (", this_row[["label"]], ")")},
                    ";")
           }
           )
  })

}

#' @title Convert tidy_sem to lavaan syntax
#' @description Final stage in the tidySEM workflow for syntax generation:
#' Convert the tidy_sem object to lavaan syntax.
#' @param x An object of class \code{tidy_sem}
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

#' @method as.lavaan tidy_sem
#' @export
#' @importFrom lavaan mplus2lavaan.modelSyntax
as.lavaan.tidy_sem <- function(x, ...){
  if(!has_syntax(x)) return(NULL)
  tab <- x$syntax
  free_values <- rep(0, nrow(tab))
  tab$free <- as.logical(tab$free)
  free_values[tab$free] <- 1:sum(tab$free)
  names(tab)[match("value", names(tab))] <- "ustart"
  tab$ustart[tab$free] <- NA
  tab$free <- free_values
  if(any(c("group", "level") %in% names(tab))){
    stop("Develop")
  }
  drop_args <- eval(formals(check_lav_tab)$lav_names)
  drop_args <- drop_args[!names(drop_args) == ""]
  drop_args <- which(names(tab) %in% drop_args)
  tab[, -drop_args]
}

as.lavaan.tidy_sem3 <- function(x, ...){
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

as.lavaan.tidy_sem2 <- function(x, ...){
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
