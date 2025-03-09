#' @method get_nodes igraph
#' @importFrom igraph V as_data_frame
#' @export
get_nodes.igraph <- function(x, label = name, ...){
  label <- substitute(label)
  nod <- igraph::as_data_frame(x, what = "vertices")
  if(ncol(nod) == 0){
    nod <- data.frame(name = as.character(unclass(igraph::V(x))))
  }
  # att_nams <- vertex_attr_names(x)
  if(is.null(nod[["shape"]])) nod[["shape"]] <- "none"
  if(is.null(nod[["label"]])) nod[["label"]] <- eval(label, envir = nod)

  class(nod) <- c("tidy_nodes", class(nod))
  return(nod)
}

#' @method get_edges igraph
#' @importFrom igraph as_data_frame
#' @export
get_edges.igraph <- function(x, label = NULL, ...){
  label <- substitute(label)
  edg <- igraph::as_data_frame(x, "edges")
  edg$arrow <- tryCatch({c("none", "last")[unclass(x)[[2]]+1L]}, error = function(e){
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

