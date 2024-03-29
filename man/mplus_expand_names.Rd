% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{mplus_expand_names}
\alias{mplus_expand_names}
\title{Expand abbreviated Mplus variable names}
\usage{
mplus_expand_names(x)
}
\arguments{
\item{x}{Atomic character string containing the variable names section of an
Mplus syntax file.}
}
\value{
Character vector of names.
}
\description{
Expand the Mplus syntax for abbreviating lists of variable
names.
}
\examples{
mplus_expand_names("test1-test12")
mplus_expand_names("testa-testb")
}
\keyword{mplus}
\keyword{utilities}
