% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/syntax-run.R
\name{run_mx}
\alias{run_mx}
\title{Run as OpenMx model with sensible defaults}
\usage{
run_mx(x, ...)
}
\arguments{
\item{x}{An object for which a method exists.}

\item{...}{Parameters passed on to other functions.}
}
\value{
Returns an \code{\link[OpenMx]{mxModel}} with free parameters updated
to their final values.
}
\description{
This convenience function runs objects for which a method exists
using OpenMx, with sensible defaults. It is intended for use with
\code{tidySEM}. For instance, it will convert a \code{tidySEM} object to
a \code{mxModel} and run it, and it will try to ensure convergence for
mixture models created using \code{\link{mx_mixture}}.
Knowledgeable users may want to run models manually.
}
\examples{
df <- iris[1:3]
names(df) <- paste0("X_", 1:3)
run_mx(measurement(tidy_sem(df), meanstructure = TRUE))
}
