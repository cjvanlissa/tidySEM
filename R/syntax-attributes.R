#' @title Extract dictionary from sem_syntax
#' @description Provides access to the \code{dictionary} element of a
#' \code{sem_syntax} object. This can be used to return or assign to the
#' \code{dictionary} element.
#' @param x Object of class sem_syntax.
#' @return data.frame
#' @rdname dictionary
#' @export
dictionary <- function(x) UseMethod("dictionary")

#' @method dictionary sem_syntax
#' @export
dictionary.sem_syntax <- function(x) x[["dictionary"]]

#' @rdname dictionary
#' @param value A valid value for \code{dictionary(x)}.
#' @export
`dictionary<-` <- function(x, value){
  UseMethod("dictionary<-")
}

#' @method dictionary<- sem_syntax
#' @export
`dictionary<-.sem_syntax` <- function(x, value)
{
  x$dictionary <- value
  x
}

#' @title Extract syntax from sem_syntax
#' @description Provides access to the \code{syntax} element of a
#' \code{sem_syntax} object. This can be used to return or assign to the
#' \code{syntax} element.
#' @param x Object of class sem_syntax.
#' @return data.frame
#' @rdname syntax
#' @export
syntax <- function(x) UseMethod("syntax")

#' @method syntax sem_syntax
#' @export
syntax.sem_syntax <- function(x) x[["syntax"]]

#' @rdname syntax
#' @param value A valid value for \code{syntax(x)}.
#' @export
`syntax<-` <- function(x, value){
  UseMethod("syntax<-")
}

#' @method syntax<- sem_syntax
#' @export
`syntax<-.sem_syntax` <- function(x, value)
{
  x$syntax <- value
  x
}
