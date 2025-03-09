# g <- make_ring(10)
# edge_attr(g) <- list(
#   name = LETTERS[1:10],
#   color = rep("green", gsize(g))
# )
# edge_attr(g, "label") <- E(g)$name
# plot(g)
# get_nodes.igraph(g)
#
# g <- graph.data.frame(data.frame(one=1:2,two=2:3))
# g <- set.vertex.attribute(g,'id',1,'first_id')
# vertex_attr(g,'id',1)
# plot(g)
# get_nodes.igraph(g)
#
# names <- c('A','B','C,','D')
# from <- c(113,115,112,114,113)
# to <- c(112,112,115,113,114)
# structure <- data.frame("from" = from, "to" = to)
# g <- graph.data.frame(structure)
# #V(g)$label <- names
# V(g)$nonsense <- letters[seq_along(names)]
# plot(g)
# get_nodes(g)
# unclass(g)
# unclass(as_undirected(g))
# as_directed(graph, mode = c("mutual", "arbitrary", "random", "acyclic"))

# as_undirected(
#   graph,
#   mode = c("collapse", "each", "mutual"),
#   edge.attr.comb = igraph_opt("edge.attr.comb")
# )

#' @method get_nodes igraph
#' @export
get_nodes.igraph <- function(x, label = name, ...){
  label <- substitute(label)
  nod <- igraph::as_data_frame(x, what = "vertices")
  nod$shape <- "none"
  if(is.null(nod[["label"]])){
    nod[["label"]] <- eval(label, envir = nod)
  }
  class(nod) <- c("tidy_nodes", class(nod))
  return(nod)
}

#' @method get_edges igraph
#' @export
get_edges.igraph <- function(x, label = NULL, ...){
  label <- substitute(label)
  edg <- igraph::as_data_frame(g, "edges")
  edg$arrow <- tryCatch({c("none", "last")[unclass(g)[[2]]+1L]}, error = function(e){
    message("Could not identify edge direction from object of class `igraph`.")
    "none"})

  if(is.null(edg$label)){
    edg[["label"]] <- eval(label, envir = edg)
  }
  class(edg) <- c("tidy_edges", class(edg))
  return(edg)
}

#' @method get_layout igraph
#' @export
get_layout.igraph <- function(x, ..., layout_algorithm = "layout_nicely"){
  Args <- list("graph" = x, "dim" = 2)
  layout_algorithm <- paste0("igraph::", layout_algorithm)
  lo <- data.frame(do.call(eval(parse(text=layout_algorithm)), Args))
  names(lo) <- c("x", "y")
  nod <- get_nodes(x)
  lo$name <- nod$name
  class(lo) <- c("tidy_layout", class(lo))
  return(lo)
}

#' @method prepare_graph igraph
#' @export
prepare_graph.igraph <- function(model, ...){
  cl <- match.call()
  if(!"edges" %in% names(cl)) cl[["edges"]] <- get_edges(model)
  if(!"nodes" %in% names(cl)) cl[["nodes"]] <- get_nodes(model)
  if(!"layout" %in% names(cl)) cl[["layout"]] <- get_layout(model)
  cl[["model"]] <- NULL
  cl[[1]] <- str2lang("tidySEM::prepare_graph")
  eval.parent(cl)
}

