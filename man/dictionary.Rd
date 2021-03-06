% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/syntax-attributes.R
\name{dictionary}
\alias{dictionary}
\alias{dictionary<-}
\title{Extract dictionary from tidy_sem}
\usage{
dictionary(x)

dictionary(x) <- value
}
\arguments{
\item{x}{Object of class tidy_sem.}

\item{value}{A valid value for \code{dictionary(x)}.}
}
\value{
data.frame
}
\description{
Provides access to the \code{dictionary} element of a
\code{tidy_sem} object. This can be used to return or assign to the
\code{dictionary} element.
}
\examples{
dict <- tidy_sem(iris, split = "\\\\.")
dictionary(dict)
}
