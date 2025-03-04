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
      atts <- strsplit(txt, split = "\n")[[1]]
      atts <- atts[grepl("(<-|->|--)", atts)]
      atts <- atts[grep("[", atts, fixed = TRUE)]
      if(length(atts) > 0){
        atts <- parse_dag_properties(atts)
        # Merge
        atts$v <- gsub("\\s.*$", "", atts$name)
        atts$w <- gsub("^.+\\s", "", atts$name)
        atts$e <- gsub("^.+?\\s(.+)\\s.+$", "\\1", atts$name)
        edg <- merge(edg, atts, by = c("v", "w", "e"), all = TRUE)
        edg[["name"]] <- NULL
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
    # if("label" %in% names(edg)){
    #   if(any(is.na(edg$label))) edg$label[is.na(edg$label)] <- edg$name[is.na(edg$label)]
    # }
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
        atts <- strsplit(txt, split = "\n")[[1]]
        atts <- atts[!grepl("(<-|->|--|\\{|\\})", atts)]
        atts <- atts[grep("[", atts, fixed = TRUE)]
        if(length(atts) > 0){
          atts <- parse_dag_properties(atts)
          if(!is.null(atts)){
            nods <- merge(nods, atts, by = "name", all = TRUE)
          }
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

#' @importFrom utils read.csv
parse_dag_properties <- function(x){
  if(length(x) > 1){
    out <- lapply(x, function(thisx){
      parse_dag_properties(thisx)
    })
    out <- bind_list(out)
    out <- out[, !c(colSums(is.na(out)) == nrow(out)), drop = FALSE]
    return(out)
  }
  nam <- trimws(gsub("\\[.*", "", x))
  x <- gsub("^.+?\\[(.+?)\\].{0,}$", "\\1", x)
  sects <- unname(unlist(utils::read.csv(text=x, header = FALSE)))
  sects <- lapply(sects, function(i){
    splt <- regmatches(i, regexpr("=", i), invert = TRUE)[[1]]
    nm <- splt[1]
    # Catch tags
    if(nm %in% c("exposure", "outcome", "unobserved", "latent")){
      out <- data.frame(TRUE)
      names(out) <- nm
      return(out)
    }
    if(nm == "pos"){
      out <- as.data.frame(t(as.numeric(strsplit(splt[2], split = ",", fixed = TRUE)[[1]])))
      names(out) <- c("x", "y")
      return(out)
    }
    out <- data.frame(splt[-1])
    names(out) <- nm
    return(out)
  })
  data.frame(name = nam, do.call(cbind, sects))
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
