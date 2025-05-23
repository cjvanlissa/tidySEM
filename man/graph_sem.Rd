% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dagitty.R, R/plot-plot_sem.R
\name{graph_sem.dagitty}
\alias{graph_sem.dagitty}
\alias{graph_sem}
\alias{graph_sem.default}
\alias{graph_sem.lavaan}
\alias{graph_sem.MxModel}
\alias{graph_sem.mplus.model}
\alias{graph_sem.igraph}
\alias{graph_sem.character}
\alias{graph_sem.mplusObject}
\title{Render a graph}
\usage{
\method{graph_sem}{dagitty}(model, ...)

graph_sem(...)

\method{graph_sem}{default}(
  edges = NULL,
  layout = NULL,
  nodes = NULL,
  rect_width = 1.2,
  rect_height = 0.8,
  ellipses_width = 1,
  ellipses_height = 1,
  variance_diameter = 0.8,
  spacing_x = 2,
  spacing_y = 2,
  text_size = 4,
  curvature = 60,
  angle = NULL,
  fix_coord = FALSE,
  ...
)

\method{graph_sem}{lavaan}(model, edges = NULL, layout = NULL, nodes = NULL, ...)

\method{graph_sem}{MxModel}(model, edges = NULL, layout = NULL, nodes = NULL, ...)

\method{graph_sem}{mplus.model}(model, edges = NULL, layout = NULL, nodes = NULL, ...)

\method{graph_sem}{igraph}(model, edges = NULL, layout = NULL, nodes = NULL, ...)

\method{graph_sem}{character}(...)

\method{graph_sem}{mplusObject}(model, edges = NULL, layout = NULL, nodes = NULL, ...)
}
\arguments{
\item{model}{Instead of the edges argument, it is also possible to use the
model argument and pass an object for which a method exists (e.g.,
\code{mplus.model} or \code{lavaan}).}

\item{...}{Additional arguments passed to and from functions.}

\item{edges}{Object of class 'tidy_edges', or a \code{data.frame} with  (at
least) the columns \code{c("from", "to")}, and optionally, \code{c("arrow",
"label", "connect_from", "connect_to", "curvature")}.}

\item{layout}{A matrix (or data.frame) that describes the layout; see
\code{\link[tidySEM]{get_layout}}.}

\item{nodes}{Optional, object of class 'tidy_nodes', created with the
\code{\link[tidySEM]{get_nodes}} function, or a \code{data.frame} with (at
least) the column \code{c("name")}, and optionally, \code{c("shape",
"label")}. If set to \code{NULL} (the default), nodes are inferred from the
\code{layout} and \code{edges} arguments.}

\item{rect_width}{Width of rectangles (used to display observed variables),
Default: 1.2}

\item{rect_height}{Height of rectangles (used to display observed variables),
Default: 0.8}

\item{ellipses_width}{Width of ellipses (used to display latent variables),
Default: 1}

\item{ellipses_height}{Height of ellipses (used to display latent variables),
Default: 1}

\item{variance_diameter}{Diameter of variance circles,
Default: .8}

\item{spacing_x}{Spacing between columns of the graph, Default: 1}

\item{spacing_y}{Spacing between rows of the graph, Default: 1}

\item{text_size}{Point size of text, Default: 4}

\item{curvature}{Curvature of curved edges. The curve is a circle segment
originating in a point that forms a triangle with the two connected points,
with angles at the two connected points equal to \code{curvature}.
To flip a curved edge, use a negative value for curvature. Default: 60}

\item{angle}{Angle used to connect nodes by the top and bottom. Defaults to
NULL, which means Euclidean distance is used to determine the shortest
distance between node sides. A numeric value between 0-180 can be provided,
where 0 means that only nodes with the same x-coordinates are connected
top-to-bottom, and 180 means that all nodes are connected top-to-bottom.}

\item{fix_coord}{Whether or not to fix the aspect ratio of the graph.
Does not work with multi-group or multilevel models.
Default: FALSE.}
}
\value{
Object of class 'sem_graph'
}
\description{
Render a graph based on a layout, and either nodes and edges, or
a model object.
}
\details{
The default interface simply Runs the functions
\code{\link[tidySEM]{prepare_graph}} and
\code{plot}. The alternative interface first runs
\code{\link[tidySEM]{get_nodes}} and \code{\link[tidySEM]{get_edges}} on
the \code{model} argument.
}
\examples{
library(lavaan)
res <- sem("dist ~ speed", cars)
graph_sem(res)
}
\keyword{tidy_graph}
