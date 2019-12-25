#' @title Extract edges from sem_graph
#' @description Provides access to the \code{edges} element of a
#' \code{sem_graph} object. This can be used to return or assign to the
#' \code{edges} element.
#' @param x Object of class sem_graph.
#' @return data.frame
#' @rdname edges
#' @export
edges <- function(x) UseMethod("edges")

#' @method edges sem_graph
#' @export
edges.sem_graph <- function(x) x[["edges"]]

#' @rdname edges
#' @param value A valid value for \code{edges(x)}.
#' @export
`edges<-` <- function(x, value){
  UseMethod("edges<-")
}

#' @method edges<- sem_graph
#' @export
`edges<-.sem_graph` <- function(x, value)
{
  # if(!inherits(value, "data.frame")){
  #   value <- tryCatch({data.frame(value)}, error = function(e){
  #     stop("Can only assign a data.frame to edges(). The value you are trying to assign to edges() is not a data.frame, and cannot be coerced to data.frame.", call. = FALSE)
  #   })
  # }
  # if(!all(names(value) == c("from", "to", "arrow", "label", "connector", "connect_from", "connect_to", "curvature"))){
  #   stop("The names of the data.frame you are trying to assign to edges() do not match the expected names, which are: from, to, arrow, label, connector, connect_from, connect_to, curvature", call. = FALSE)
  # }
  x$edges <- value
  x
}

#' @title Extract nodes from sem_graph
#' @description Provides access to the \code{nodes} element of a
#' \code{sem_graph} object. This can be used to return or assign to the
#' \code{nodes} element.
#' @param x Object of class sem_graph.
#' @return data.frame
#' @rdname nodes
#' @export
nodes <- function(x) UseMethod("nodes")

#' @method nodes sem_graph
#' @export
nodes.sem_graph <- function(x) x[["nodes"]]

#' @rdname nodes
#' @param value A valid value for \code{nodes(x)}.
#' @export
`nodes<-` <- function(x, value){
  UseMethod("nodes<-")
}

#' @method nodes<- sem_graph
#' @export
`nodes<-.sem_graph` <- function(x, value)
{
  # if(!inherits(value, "data.frame")){
  #   value <- tryCatch({data.frame(value)}, error = function(e){
  #     stop("Can only assign a data.frame to nodes(). The value you are trying to assign to nodes() is not a data.frame, and cannot be coerced to data.frame.", call. = FALSE)
  #   })
  # }
  # if(!all(names(value) == c("node_id", "param", "shape", "label", "x", "y", "node_xmin", "node_xmax", "node_ymin", "node_ymax"))){
  #   stop("The names of the data.frame you are trying to assign to nodes() do not match the expected names, which are: node_id, param, shape, label, x, y, node_xmin, node_xmax, node_ymin, node_ymax", call. = FALSE)
  # }
  x$nodes <- value
  x
}
