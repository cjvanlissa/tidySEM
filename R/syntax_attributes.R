#' @title Extract data_dict from sem_syntax
#' @description Provides access to the \code{data_dict} element of a
#' \code{sem_syntax} object. This can be used to return or assign to the
#' \code{data_dict} element.
#' @param x Object of class sem_syntax.
#' @return data.frame
#' @rdname get_dict
#' @export
get_dict <- function(x) UseMethod("get_dict")

#' @method get_dict sem_syntax
#' @export
get_dict.sem_syntax <- function(x) x[["data_dict"]]

#' @rdname get_dict
#' @param value A valid value for \code{get_dict(x)}.
#' @export
`get_dict<-` <- function(x, value){
  UseMethod("get_dict<-")
}

#' @method get_dict<- sem_syntax
#' @export
`get_dict<-.sem_syntax` <- function(x, value)
{
  x$data_dict <- value
  x
}
