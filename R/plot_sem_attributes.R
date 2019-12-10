x <- list(val = "a", edges = "test")
class(x) <- "sem_graph"
#attr(x, "edges") <- "bla"
#x$edges <- "test"
#edges(x) <- "duh"
x <- prep
edges <- function(x) UseMethod("edges")
edges.default <- function(x) x[["edges"]]

`edges<-` <- function(x, value){
  UseMethod("edges<-")
}

`edges<-.sem_graph` <- function(x, value)
{
  if(!inherits(value, "data.frame")){
    value <- tryCatch({data.frame(value)}, error = function(e){
      stop("Can only assign a data.frame to edges(). The value you are trying to assign to edges() is not a data.frame, and cannot be coerced to data.frame.", call. = FALSE)
    })
  }
  if(!all(names(value) == c("from", "to", "arrow", "label", "connector", "connect_from", "connect_to", "curvature"))){
    stop("The names of the data.frame you are trying to assign to edges() do not match the expected names, which are: from, to, arrow, label, connector, connect_from, connect_to, curvature", call. = FALSE)
  }
  x$edges <- value
  x
}

nodes <- function(x) UseMethod("nodes")
nodes.default <- function(x) x[["nodes"]]

`nodes<-` <- function(x, value){
  UseMethod("nodes<-")
}

`nodes<-.sem_graph` <- function(x, value)
{
  if(!inherits(value, "data.frame")){
    value <- tryCatch({data.frame(value)}, error = function(e){
      stop("Can only assign a data.frame to nodes(). The value you are trying to assign to nodes() is not a data.frame, and cannot be coerced to data.frame.", call. = FALSE)
    })
  }
  if(!all(names(value) == c("node_id", "param", "shape", "label", "x", "y", "node_xmin", "node_xmax", "node_ymin", "node_ymax"))){
    stop("The names of the data.frame you are trying to assign to nodes() do not match the expected names, which are: node_id, param, shape, label, x, y, node_xmin, node_xmax, node_ymin, node_ymax", call. = FALSE)
  }
  x$nodes <- value
  x
}
