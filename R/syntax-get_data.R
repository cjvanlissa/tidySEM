#' @title Get data from model object
#' @description Extracts data from an object for which a method exists.
#' @param x An object for which a method exists.
#' @param ... Additional arguments passed to and from functions.
#' @return A \code{data.frame}.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname get_data
#' @export
get_data <- function(x, ...){
  UseMethod("get_data", x)
}

#' @method get_data mplusObject
#' @export
get_data.mplusObject <- function(x, ...){
  Args <- as.list(match.call()[-1])
  Args$x <- x$results
  do.call(get_data, Args)
}

#' @method get_data mplus.model
#' @export
#' @importFrom utils read.table
get_data.mplus.model <- function(x, ...){
  df <- read.table(x$input$data$file, stringsAsFactors = FALSE)
  names(df) <- mplus_expand_names(x$input$variable$names) #strsplit(x$input$variable$names, split = "\\s+")[[1]]
  df
}

#' @method get_data data.frame
#' @export
get_data.data.frame <- function(x, ...){
  return(x)
}

