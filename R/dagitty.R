#' @method get_layout dagitty
#' @export
get_layout.dagitty <- function(x, ..., rows = NULL){
  if (requireNamespace("dagitty", quietly = TRUE)) {
    lo <- dagitty::coordinates(x)
    if(!diff(sapply(lo, length)) == 0) stop("Could not extract layout from object of class 'dagitty'.")
    if(anyNA(lo$x) | anyNA(lo$y)){
      lo_graph <- dagitty::coordinates(dagitty::graphLayout(x))
      lo$x[is.na(lo$x)] <- lo_graph$x[is.na(lo$x)]
      lo$y[is.na(lo$y)] <- lo_graph$y[is.na(lo$y)]
      out <- data.frame(name = names(lo$x), do.call(cbind, lo))
      rownames(out) <- NULL
      class(out) <- c("tidy_layout", class(out))
    } else {
      out <- matrix(nrow = max(lo$y) + 1, ncol = max(lo$x) + 1)
      for(v in names(lo$x)){
        out[lo$y[v] + 1, lo$x[v] + 1] <- v
      }
      class(out) <- c("layout_matrix", class(out))
    }
    return(out)
  } else {
    message("Dependency 'dagitty' is not available.")
  }
}


#' @method get_edges dagitty
#' @export
get_edges.dagitty <- function(x, label = "est", ...){
  if (requireNamespace("dagitty", quietly = TRUE)) {
    edg <- dagitty::edges(x)
    txt <- as.character(x)
    if(grepl("[", txt, fixed = TRUE)){
      attrbts <- strsplit(txt, split = "\n")[[1]]
      attrbts <- attrbts[grepl("(<-|->|--)", attrbts)]
      attrbts <- attrbts[grep("[", attrbts, fixed = TRUE)]
      attrbts <- lapply(attrbts, function(at){
        out <- trimws(strsplit(at, split = "[", fixed = TRUE)[[1]])
        nam = out[1]
        out <- gsub("]", "", out[2], fixed = TRUE)
        out <- strsplit(out, split = ",", fixed = TRUE)[[1]]
        out <- lapply(out, function(i){
          gsub('"', "", strsplit(i, split = "=", fixed = TRUE)[[1]], fixed = TRUE)
        })
        nams <- sapply(out, `[`, 1)
        out <- data.frame(name = nam, lapply(out, `[`, 2))
        names(out) <- c("name", nams)
        if(any(names(out) %in% c("exposure", "outcome"))) out[(names(out) %in% c("exposure", "outcome"))] <- TRUE
        out
      })
      attrbts <- bind_list(attrbts)
      if(!is.null(attrbts)){
        edge_atts <- attrbts[, !(colSums(is.na(attrbts)) == nrow(attrbts)), drop = FALSE]
        # Merge
        edge_atts$v <- gsub("\\s.*$", "", edge_atts$name)
        edge_atts$w <- gsub("^.+\\s", "", edge_atts$name)
        edge_atts$e <- gsub("^.+?\\s(.+)\\s.+$", "\\1", edge_atts$name)
        edg <- merge(edg, edge_atts, by = c("v", "w", "e"), all = TRUE)
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
    edg <- edg[, !names(edg) %in% c("x", "y"), drop = FALSE]
    if("label" %in% names(edg)){
      edg$label[is.na(edg$label)] <- edg$name[is.na(edg$label)]
    }
    class(edg) <- c("tidy_edges", class(edg))
    return(edg)
  } else {
    message("Dependency 'dagitty' is not available.")
  }
}

#' @method get_nodes dagitty
#' @export
get_nodes.dagitty <- function(x, label = "est", ...){
  if (requireNamespace("dagitty", quietly = TRUE)) {

      nods <- names(dagitty::coordinates(x)$x)
      nods <- data.frame(name = nods)
      txt <- as.character(x)
      if(grepl("[", txt, fixed = TRUE)){
        attrbts <- strsplit(txt, split = "\n")[[1]]
        attrbts <- attrbts[!grepl("(<-|->|--|\\{|\\})", attrbts)]
        attrbts <- attrbts[grep("[", attrbts, fixed = TRUE)]
        attrbts <- lapply(attrbts, function(at){
          out <- trimws(strsplit(at, split = "[", fixed = TRUE)[[1]])
          nam = out[1]
          out <- gsub("]", "", out[2], fixed = TRUE)
          out <- strsplit(out, split = ",", fixed = TRUE)[[1]]
          out <- lapply(out, function(i){
            gsub('"', "", strsplit(i, split = "=", fixed = TRUE)[[1]], fixed = TRUE)
          })
          nams <- sapply(out, `[`, 1)
          out <- data.frame(name = nam, lapply(out, `[`, 2))
          names(out) <- c("name", nams)
          if(any(names(out) %in% c("exposure", "outcome"))) out[(names(out) %in% c("exposure", "outcome"))] <- TRUE
          out
        })
        attrbts <- bind_list(attrbts)
        if(!is.null(attrbts)){
          node_atts <- attrbts[, !c(colSums(is.na(attrbts)) == nrow(attrbts)), drop = FALSE]
          nods <- merge(nods, node_atts, by = "name", all = TRUE)
        }
      }
      if("label" %in% names(nods)){
        nods$label[is.na(nods$label)] <- nods$name[is.na(nods$label)]
      }
      nods$shape <- "none"
    class(nods) <- c("tidy_nodes", class(nods))
    return(nods)
  } else {
    message("Dependency 'dagitty' is not available.")
  }
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
  cl[[1]] <- str2lang("tidySEM::prepare_graph")
  eval.parent(cl)
}

#' @method graph_sem dagitty
#' @rdname graph_sem
#' @export
graph_sem.dagitty <- function(model,
                              ...){
  cl <- match.call()
  cl[[1L]] <- str2lang("tidySEM::prepare_graph")
  out <- eval.parent(cl)
  return(plot(out))
}
