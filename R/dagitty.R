#' @method get_layout dagitty
#' @export
get_layout.dagitty <- function(x, ..., rows = NULL){
  lo <- dagitty::coordinates(x)
  if(!diff(sapply(lo, length)) == 0) stop("Could not extract layout from object of class 'dagitty'.")
  if(anyNA(lo$x) | anyNA(lo$y)){
    edg <- get_edges(x)
    out <- get_layout(edg)
  } else {
    out <- matrix(nrow = max(lo$y) + 1, ncol = max(lo$x) + 1)
    for(v in names(lo$x)){
      out[lo$y[v] + 1, lo$x[v] + 1] <- v
    }
    class(out) <- c("layout_matrix", class(out))
  }
  return(out)
}

# @importFrom utils getFromNamespace
#.edgeAttributes <- utils::getFromNamespace(".edgeAttributes", "dagitty")

#' @method get_edges dagitty
#' @export
#' @importFrom dagitty edges
get_edges.dagitty <- function(x, label = "est", ...){
  edg <- dagitty::edges(x)
  cl <- match.call()
  cl[[1]] <- str2lang("dagitty:::.edgeAttributes")
  cl <- cl[c(1, which(names(cl) == "x"))]
  cl[["a"]] <- "beta"
  labs <- try(eval.parent(cl))
  if(!inherits(labs, "try-error")){
    if(!all(is.na(labs$a))){
      edg$label <- labs$a
    }
  }
  names(edg)[1:2] <- c("from", "to")
  edg$arrow <- "last"
  edg$arrow[edg$e == "<->"] <- "both"
  edg$arrow[edg$e == "--"] <- "none"
  edg$color <- "gray80"
  if(any(edg$e == "--")){
    edg$linewidth <- .5
    edg$linewidth[edg$e == "--"] <- 1
    edg$color[edg$e == "--"] <- "black"
  }
  if(any(edg$e == "<->")){
    edg$curvature <- NA
    edg$curvature[edg$e == "<->"] <- 60
  }
  edg <- edg[, names(edg)[names(edg) %in% c("from", "to", "arrow", "curvature", "linewidth", "color")], drop = FALSE]
  class(edg) <- c("tidy_edges", class(edg))
  return(edg)
}


#' @method get_nodes dagitty
#' @export
#' @importFrom dagitty coordinates
get_nodes.dagitty <- function(x, label = "est", ...){
  nods <- dagitty::coordinates(x)
  nams <- labs <- names(nods$x)
  if(!is.null(attr(x, "labels"))){
    attrlab <- attr(x, "labels")
    if(any(labs %in% names(attrlab))){
      labs[labs %in% names(attrlab)] <- attrlab[labs[labs %in% names(attrlab)]]
    }
  }
  nods <- data.frame(
    name = nams,
    shape = "none",
    label = labs
  )
  class(nods) <- c("tidy_nodes", class(nods))
  return(nods)
}

#' @method prepare_graph dagitty
#' @rdname prepare_graph
#' @export
prepare_graph.dagitty <- function(model,
                                  rect_height = .5,
                                  rect_width = .5,
                                  ...){
  cl <- match.call()
  if(!"edges" %in% names(cl)) cl[["edges"]] <- get_edges(model)
  if(!"nodes" %in% names(cl)) cl[["nodes"]] <- get_nodes(model)
  if(!"layout" %in% names(cl)) cl[["layout"]] <- get_layout(model)
  cl[["rect_height"]] <- rect_height
  cl[["rect_width"]] <- rect_width
  cl[["model"]] <- NULL
  cl[[1]] <- quote(prepare_graph)
  eval.parent(cl)
}

#' @method graph_sem dagitty
#' @rdname graph_sem
#' @export
graph_sem.dagitty <- function(model,
                              ...){
  cl <- match.call()
  cl[[1L]] <- quote(prepare_graph)
  out <- eval.parent(cl)
  return(plot(out))
}
