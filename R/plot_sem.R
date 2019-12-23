#' @title Render a graph
#' @description Render a graph based on a layout, and either nodes and edges, or
#' a model object.
#' @param ... Additional arguments passed to and from functions.
#@usage ## Default S3 method:
#graph(edges, layout, nodes = NULL,  rect_width = 1.2, rect_height = .8,
#  ellipses_width = 1, ellipses_height = 1, spacing_x = 2, spacing_y = 2,
#  text_size = 4, curvature = .1, angle = NULL, ...)
#
### Alternative interface:
#graph(model, layout, ...)
# Default: "euclidean", but could be set to "manhattan".
#' @return Object of class 'sem_graph'
#' @details The default interface simply Runs the functions
#' \code{\link[tidySEM]{prepare_graph}} and
#' \code{plot}. The alternative interface first runs
#' \code{\link[tidySEM]{get_nodes}} and \code{\link[tidySEM]{get_edges}} on
#' the \code{model} argument.
#'
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname graph
#' @keywords tidy_graph
#' @export
graph <- function(...){

  UseMethod("graph")
}

#' @method graph default
#' @param edges Object of class 'tidy_edges', or a \code{data.frame} with  (at
#' least) the columns \code{c("from", "to")}, and optionally, \code{c("arrow",
#' "label", "connector", "connect_from", "connect_to", "curvature")}.
#' @param layout An object of class \code{tidy_layout}, or a matrix with the
#' layout of the graph that can be converted using
#' \code{\link[tidySEM]{as.layout}}.
#' @param nodes Optional, object of class 'tidy_nodes', created with the
#' \code{\link[tidySEM]{get_nodes}} function, or a \code{data.frame} with (at
#' least) the column \code{c("name")}, and optionally, \code{c("shape",
#' "label")}. If set to \code{NULL} (the default), nodes are inferred from the
#' \code{layout} and \code{edges} arguments.
#' @param rect_width Width of rectangles (used to display observed variables),
#' Default: 1.2
#' @param rect_height Height of rectangles (used to display observed variables),
#' Default: 0.8
#' @param ellipses_width Width of ellipses (used to display latent variables),
#' Default: 1
#' @param ellipses_height Height of ellipses (used to display latent variables),
#' Default: 1
#' @param variance_diameter Diameter of variance circles,
#' Default: .8
#' @param spacing_x Spacing between columns of the graph, Default: 1
#' @param spacing_y Spacing between rows of the graph, Default: 1
#' @param text_size Point size of text, Default: 4
#' @param curvature Curvature of curved connectors. To flip connectors, use
#' negative values. Default: .1
#' @param angle Angle used to connect nodes by the top and bottom. Defaults to
#' NULL, which means Euclidean distance is used to determine the shortest
#' distance between node sides. A numeric value between 0-180 can be provided,
#' where 0 means that only nodes with the same x-coordinates are connected
#' top-to-bottom, and 180 means that all nodes are connected top-to-bottom.
#' @rdname graph
#' @export
graph.default <- function(edges,
                          layout,
                          nodes = NULL,
                         rect_width = 1.2,
                         rect_height = .8,
                         ellipses_width = 1,
                         ellipses_height = 1,
                         variance_diameter = .8,
                         spacing_x = 2,
                         spacing_y = 2,
                         text_size = 4,
                         curvature = .1,
                         angle = NULL,
                         ...){
  Args <- as.list(match.call()[-1])
  Args$layout <- force(layout)
  Args$edges <- force(edges)
  Args$nodes <- force(nodes)
  Args[["model"]] <- NULL
  prep <- do.call(prepare_graph, Args)
  plot(prep)
}

#' @method graph lavaan
#' @param model Instead of the edges argument, it is also possible to use the
#' model argument and pass an object for which a method exists (e.g.,
#' \code{mplus.model} or \code{lavaan}).
#' @rdname graph
#' @export
graph.lavaan <- function(model,
                         layout,
                         ...){
  Args <- as.list(match.call()[-1])
  do.call(graph_model, Args)
}

#' @method graph mplus.model
#' @rdname graph
#' @export
graph.mplus.model <- graph.lavaan

graph_model <- function(model,
                        layout,
                        ...) {
  Args <- list(x = model)
  edges <- do.call(get_edges, Args)
  nodes <- do.call(get_nodes, Args)
  Args <- as.list(match.call()[-1])
  Args$layout <- layout
  Args$edges <- edges
  Args$nodes <- nodes
  Args[["model"]] <- NULL
  do.call(graph.default, Args)
}

#' @title Prepare graph data
#' @description Prepare an object of class \code{sem_graph}, containing
#' data objects that can be rendered into a SEM graph. Using this function
#' allows
#' users to manually change the default graph specification before plotting it.
#' Input consists of (at least) a layout, and either nodes and edges, or
#' a model object.
#' @param ... Additional arguments passed to and from functions.
# @usage ## Default S3 method:
# prepare_graph(edges, layout, nodes = NULL,  rect_width = 1.2,
#   rect_height = .8, ellipses_width = 1, ellipses_height = 1, spacing_x = 2,
#   spacing_y = 2, text_size = 4, curvature = .1, angle = NULL, ...)
#
# ## Alternative interface:
# prepare_graph(model, layout, ...)
# Default: "euclidean", but could be set to "manhattan".
# Default: "euclidean", but could be set to "manhattan".
#' @return Object of class 'sem_graph'
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname prepare_graph
#' @export
prepare_graph <- function(...){
  UseMethod("prepare_graph")
}

#' @method prepare_graph default
#' @param edges Object of class 'tidy_edges', or a \code{data.frame} with  (at
#' least) the columns \code{c("from", "to")}, and optionally, \code{c("arrow",
#' "label", "connector", "connect_from", "connect_to", "curvature")}.
#' @param layout An object of class \code{tidy_layout}, or a matrix with the
#' layout of the graph that can be converted using
#' \code{\link[tidySEM]{as.layout}}.
#' @param nodes Optional, object of class 'tidy_nodes', created with the
#' \code{\link[tidySEM]{get_nodes}} function, or a \code{data.frame} with (at
#' least) the column \code{c("name")}, and optionally, \code{c("shape",
#' "label")}. If set to \code{NULL} (the default), nodes are inferred from the
#' \code{layout} and \code{edges} arguments.
#' @param rect_width Width of rectangles (used to display observed variables),
#' Default: 1.2
#' @param rect_height Height of rectangles (used to display observed variables),
#' Default: 0.8
#' @param ellipses_width Width of ellipses (used to display latent variables),
#' Default: 1
#' @param ellipses_height Height of ellipses (used to display latent variables),
#' Default: 1
#' @param variance_diameter Diameter of variance circles,
#' Default: .8
#' @param spacing_x Spacing between columns of the graph, Default: 1
#' @param spacing_y Spacing between rows of the graph, Default: 1
#' @param text_size Point size of text, Default: 4
#' @param curvature Curvature of curved connectors. To flip connectors, use
#' negative values. Default: .1
#' @param angle Angle used to connect nodes by the top and bottom. Defaults to
#' NULL, which means Euclidean distance is used to determine the shortest
#' distance between node sides. A numeric value between 0-180 can be provided,
#' where 0 means that only nodes with the same x-coordinates are connected
#' top-to-bottom, and 180 means that all nodes are connected top-to-bottom.
#' @rdname prepare_graph
#' @export
prepare_graph.default <- function(edges,
                                 layout,
                                 nodes = NULL,
                                 rect_width = 1.2,
                                 rect_height = .8,
                                 ellipses_width = 1,
                                 ellipses_height = 1,
                                 variance_diameter = .8,
                                 spacing_x = 2,
                                 spacing_y = 2,
                                 text_size = 4,
                                 curvature = .1,
                                 angle = NULL,
                                 ...
){
  Args <- as.list(match.call())[-1]
  myfor <- formals(prepare_graph.default)
  for ( v in names(myfor)){
    if (!(v %in% names(Args)))
      Args <- append(Args,myfor[v])
  }

  # Check if nodes exist in edges and layout --------------------------------

  df_edges <- edges
  fac_vars <- sapply(df_edges, inherits, what = "factor")
  if(any(fac_vars)){
    df_edges[which(fac_vars)] <- lapply(df_edges[which(fac_vars)], as.character)
  }
  fac_vars <- sapply(layout, inherits, what = "factor")
  if(any(fac_vars)){
    layout[which(fac_vars)] <- lapply(layout[which(fac_vars)], as.character)
  }
  if(!("from" %in% names(df_edges) & "to" %in% names(df_edges))){
    stop("Argument 'edges' must have columns 'from' and 'to'.")
  }

  if(!all((df_edges$from %in% layout$name) & (df_edges$to %in% layout$name))){
    warning("Some edges involve nodes not in layout. These were dropped.")
    df_edges <- df_edges[(df_edges$from %in% layout$name) & (df_edges$to %in% layout$name), ]
  }

  # Infer nodes if argument is null -----------------------------------------

  if(is.null(nodes)){
    nodes <- data.frame(name = unique(c("", NA, unlist(layout), df_edges$from, df_edges$to))[-c(1:2)])
    nodes$shape <- "rect"
    nodes$label <- nodes$name
  }
  fac_vars <- sapply(nodes, inherits, what = "factor")
  if(any(fac_vars)){
    nodes[which(fac_vars)] <- lapply(nodes[which(fac_vars)], as.character)
  }

  # Check edges df ----------------------------------------------------------

  if(!"arrow" %in% names(df_edges)){
    df_edges$arrow <- "last"
  }
  if(!"label" %in% names(df_edges)){
    df_edges$label <- ""
  }
  if(!"connector" %in% names(df_edges)){
    df_edges$connector <- "line"
  }
  if(!"connect_from" %in% names(df_edges)){
    df_edges$connect_from <- "left"
  }
  if(!"connect_to" %in% names(df_edges)){
    df_edges$connect_to <- "right"
  }
  if(!"curvature" %in% names(df_edges)){
    df_edges$curvature <- NA
  }

  # Defaults for missing columns --------------------------------------------

  if(!("name" %in% names(nodes) & "name" %in% names(layout))){
    stop("Arguments 'nodes' and 'layout' must both have a 'name' column.")
  }
  df_nodes <- merge(nodes, layout, by = "name")

  if(!"label" %in% names(df_nodes)){
    df_nodes$label <- df_nodes$name
  }
  if(!"shape" %in% names(df_nodes)){
    df_nodes$shape <- "rect"
  }
  if("group" %in% names(df_edges) & !"group" %in% names(df_nodes)){
    df_nodes <- do.call(rbind, lapply(unique(df_edges$group), function(this_group){
      df_nodes$group <- this_group
      df_nodes
    }))
  }
  if("group" %in% names(df_nodes) & !"group" %in% names(df_edges)){
    df_edges <- do.call(rbind, lapply(unique(df_nodes$group), function(this_group){
      df_edges$group <- this_group
      df_edges
    }))
  }

# Order nodes and edges ---------------------------------------------------
#df_edges$level <- rep(c("within", "between", "within"), each = 3)
  if("group" %in% names(df_edges)){
    if("level" %in% names(df_edges)){
      df_edges <- df_edges[with(df_edges, order(group, level)), ]
    } else {
      df_edges <- df_edges[with(df_edges, order(group)), ]
    }
  }

# Compute x, y coordinates ------------------------------------------------

  df_nodes$x <- df_nodes$x * spacing_x
  df_nodes$y <- df_nodes$y * spacing_y

  df_nodes$node_xmin <- NA
  df_nodes$node_xmax <- NA
  df_nodes$node_ymin <- NA
  df_nodes$node_ymax <- NA
  if(any(df_nodes$shape == "rect")){
    df_nodes[df_nodes$shape == "rect", c("node_xmin", "node_xmax")] <- cbind(df_nodes[df_nodes$shape == "rect", ]$x-.5*rect_width,
                                                                             df_nodes[df_nodes$shape == "rect", ]$x+.5*rect_width)
    df_nodes[df_nodes$shape == "rect", c("node_ymin", "node_ymax")] <- cbind(df_nodes[df_nodes$shape == "rect", ]$y-.5*rect_height,
                                                                             df_nodes[df_nodes$shape == "rect", ]$y+.5*rect_height)
  }

  if(any(df_nodes$shape == "oval")){
    df_nodes[df_nodes$shape == "oval", c("node_xmin", "node_xmax")] <- cbind(df_nodes[df_nodes$shape == "oval", ]$x-.5*ellipses_width,
                                                                             df_nodes[df_nodes$shape == "oval", ]$x+.5*ellipses_width)
    df_nodes[df_nodes$shape == "oval", c("node_ymin", "node_ymax")] <- cbind(df_nodes[df_nodes$shape == "oval", ]$y-.5*ellipses_height,
                                                                             df_nodes[df_nodes$shape == "oval", ]$y+.5*ellipses_height)
  }

# Determine where best to connect nodes -----------------------------------

  connect_cols <- .determine_connections(df_nodes, df_edges, angle)
  # assign by groups to prevent arrows from ending up in the wrong place
  # Sort by group first, then by level
  df_edges$connect_from <- connect_cols[, 1]
  df_edges$connect_to <- connect_cols[, 2]

  has_curve <- which(df_edges$curvature > 0)
  df_edges$curvature[has_curve][df_edges$connect_to[has_curve] == df_edges$connect_from[has_curve] & df_edges$connect_from[has_curve] %in% c("top", "right") & df_edges$curvature[has_curve] > 0] <- -1 * df_edges$curvature[has_curve][df_edges$connect_to[has_curve] == df_edges$connect_from[has_curve] & df_edges$connect_from[has_curve] %in% c("top", "right") & df_edges$curvature[has_curve] > 0]

  out <- Args

  if(is.null(df_nodes[["label"]])) df_nodes$label <- df_nodes$name
  out$nodes <- df_nodes[, c("name", "shape", "label","x", "y", "node_xmin", "node_xmax", "node_ymin", "node_ymax", "group", "level")[na.omit(which(c("name", "shape", "label","x", "y", "node_xmin", "node_xmax", "node_ymin", "node_ymax", "group", "level") %in% names(df_nodes)))]]
  out$edges <- df_edges
  out$layout <- NULL
  class(out) <- "sem_graph"
  out
}

#' @method prepare_graph lavaan
#' @param model Instead of the edges argument, it is also possible to use the
#' model argument and pass an object for which a method exists (e.g.,
#' \code{mplus.model} or \code{lavaan}).
#' @rdname prepare_graph
#' @export
prepare_graph.lavaan <- function(model, layout, ...){
  Args <- as.list(match.call()[-1])
  do.call(prepare_graph_model, Args)
}

#' @method prepare_graph mplus.model
#' @rdname prepare_graph
#' @export
prepare_graph.mplus.model <- prepare_graph.lavaan

prepare_graph_model <- function(model, layout, ...) {
  Args <- list(x = model)
  edges <- do.call(get_edges, Args)
  nodes <- do.call(get_nodes, Args)
  Args <- as.list(match.call()[-1])
  Args$layout <- layout
  Args$edges <- edges
  Args$nodes <- nodes
  Args[["model"]] <- NULL
  do.call(prepare_graph.default, Args)
}

#' @export
#' @importFrom stats setNames
#' @importFrom ggplot2 aes_string arrow element_blank facet_grid geom_label
#' @importFrom ggplot2 geom_rect geom_segment geom_text geom_vline ggplot labs
#' @importFrom ggplot2 theme theme_bw unit facet_wrap facet_grid
#' @importFrom graphics plot
#' @method plot sem_graph
plot.sem_graph <- function(x, y, ...){
  df_nodes <- x$nodes
  df_edges <- x$edges
  rect_width <- x$rect_width
  rect_height <- x$rect_height
  ellipses_width <- x$ellipses_width
  ellipses_height <- x$ellipses_height
  variance_diameter <- x$variance_diameter
  spacing_x <- x$spacing_x
  spacing_y <- x$spacing_y
  text_size <- x$text_size
  connect_points <- .connect_points(df_nodes, df_edges)

  df_edges <- cbind(df_edges, connect_points)

  df_edges <- cbind(df_edges, setNames(data.frame(t(apply(connect_points, 1, function(x){(x[1:2]+x[3:4])/2}))), c("text_x", "text_y")))


# Make plot ---------------------------------------------------------------

  p <- ggplot(NULL)
  if(any(df_edges$connector == "curve")){
    if(any(df_edges$from == df_edges$to)){
      p <- .plot_variances(p, df = df_edges[df_edges$connector == "curve" & df_edges$from == df_edges$to, ], text_size = text_size, diameter = variance_diameter)
    }
    if(any(!df_edges$from == df_edges$to)){
      p <- .plot_curves(p, df = df_edges[df_edges$connector == "curve" & !df_edges$from == df_edges$to, ], text_size = text_size)
    }

  }
  if(any(df_edges$connector == "line")){
    p <- .plot_lines(p, df_edges[df_edges$connector == "line", ], text_size)
  }


  p <- .plot_nodes(p, df = df_nodes, text_size = text_size, ellipses_width = ellipses_width, ellipses_height = ellipses_height)

  if("level" %in% names(df_nodes) & "level" %in% names(df_edges)){
    if("group" %in% names(df_nodes) & "group" %in% names(df_edges)){
      p <- p + facet_grid(level~group, scales = "free")
    } else {
      p <- p + facet_wrap(~level, scales = "free")
    }
  } else {
    if("group" %in% names(df_nodes) & "group" %in% names(df_edges)){
      p <- p + facet_wrap(~group, scales = "free")
    }
  }

  p + theme(axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(), legend.position = "none",
          panel.background = element_blank(), panel.border = element_blank(),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          plot.background = element_blank())

}

.connect_points <- function(df_nodes, df_edges){
  node_id <- do.call(paste0, df_nodes[, na.omit(match(c("name", "group", "level"), names(df_nodes))), drop = FALSE])

  edge_id <- do.call(paste0, df_edges[, na.omit(match(c("from", "group", "level"), names(df_edges))), drop = FALSE])

  loc <- list(
    c("node_xmin", "y"),
    c("node_xmax", "y"),
    c("x", "node_ymax"),
    c("x", "node_ymin"))[match(df_edges$connect_from, c("left", "right", "top", "bottom"))]
  from_loc <- t(mapply(function(x,y){unlist(df_nodes[node_id == x, y])}, x = edge_id, y = loc, USE.NAMES = FALSE))
  edge_id <- do.call(paste0, df_edges[, na.omit(match(c("to", "group", "level"), names(df_edges))), drop = FALSE])
  loc <- list(
    c("node_xmin", "y"),
    c("node_xmax", "y"),
    c("x", "node_ymax"),
    c("x", "node_ymin"))[match(df_edges$connect_to, c("left", "right", "top", "bottom"))]
  to_loc <- t(mapply(function(x,y){unlist(df_nodes[node_id == x, y])}, x = edge_id, y = loc, USE.NAMES = FALSE))

  setNames(data.frame(cbind(from_loc, to_loc)), c("edge_xmin", "edge_ymin", "edge_xmax", "edge_ymax"))
}

matrix_to_nodes <- function(nodes, shape){
  nodes_long <- setNames(as.data.frame.table(nodes), c("y", "x", "label"))
  nodes_long[1:2] <- lapply(nodes_long[1:2], as.numeric)
  nodes_long$y <- (max(nodes_long$y)+1)-nodes_long$y
  nodes_long$label <- as.character(nodes_long$label)
  nodes_long$shape <- as.vector(shape)
  nodes_long <- nodes_long[!nodes_long$label == "", ]
  #nodes_long$node_id <- 1:nrow(nodes_long)
  nodes_long
}


#' @title Extract nodes from a SEM model object
#' @description Attempts to extract nodes from a SEM model object, where nodes
#' are defined as observed or latent variables.
#' @param x A model object of class 'mplusObject' or 'lavaan'.
#' @param label Character, indicating which column to use for node labels. Nodes
#' are labeled with mean values of the observed/latent variables they represent.
#' Defaults to 'est_sig', which consists of the estimate value with significance
#' asterisks.
#' @param ... Additional parameters to be passed to and from other functions.
#' @return An object of class 'tidy_nodes'
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname get_nodes
#' @keywords tidy_graph
#' @export
get_nodes <- function(x, label = "est_sig", ...){
  UseMethod("get_nodes", x)
}

#' @method get_nodes mplus.model
#' @export
get_nodes.mplus.model <- function(x, label = "est_sig", ...){
  Args <- as.list(match.call()[-1])
  Args$x <- table_results(x, all = TRUE)
  do.call(get_nodes, Args)
}

#' @method get_nodes lavaan
#' @export
#' @importFrom lavaan parameterTable lavInspect
get_nodes.lavaan <- function(x, label = "est_sig", ...){
  Args <- as.list(match.call()[-1])
  Args$x <- table_results(x, all = TRUE)
  do.call(get_nodes, Args)
}

#' @method get_nodes tidy_results
#' @export
get_nodes.tidy_results <- function(x, label = "est_sig", ...){
  if("group" %in% names(x)){
    x_list <- lapply(unique(x$group), function(i){
      tmp <- get_nodes(x = x[x$group == i, -which(names(x) == "group")])
      tmp$group <- i
      tmp
    })
    return(do.call(rbind, x_list))
  }
  if("level" %in% names(x)){
    x_list <- lapply(unique(x$level), function(i){
      tmp <- get_nodes(x = x[x$level == i, -which(names(x) == "level")])
      tmp$name <- paste0(tmp$name, ".", i)
      tmp$level <- i
      tmp
      })
    return(do.call(rbind, x_list))
  }
  latent <- unique(x$lhs[x$op == "=~"])
  obs <- unique(x$lhs[x$op %in% c("~~", "~", "~1")])
  nodes <- unique(c(latent, obs))
  nodes <- data.frame(name = unique(nodes), shape = c("rect", "oval")[(unique(nodes) %in% latent)+1], stringsAsFactors = FALSE)

  if(!is.null(label) & label %in% names(x)){
    labelz <- x[x$op == "~1", ]
    labelz <- labelz[match(nodes$name, labelz$lhs), ][[label]]
    nodes$label <- paste0(nodes$name, "\n", labelz)
  } else {
    nodes$label <- nodes$name
  }
  class(nodes) <- c("tidy_nodes", class(nodes))
  nodes
}

#' @title Extract edges from a SEM model object
#' @description Attempts to extract edges from a SEM model object, where edges
#' are defined as regression paths and covariances between variables (nodes).
#' @param x A model object of class 'mplusObject' or 'lavaan'.
#' @param label Character, indicating which column to use for edge labels.
#' Defaults to 'est_sig', which consists of the estimate value with significance
#' asterisks.
#' @param ... Additional parameters to be passed to and from other functions.
#' @return An object of class 'tidy_edges'
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname get_edges
#' @keywords tidy_graph
#' @export
get_edges <- function(x, label = "est_sig", ...){
  UseMethod("get_edges", x)
}

# #' @method get_edges mplusObject
# #' @export
# get_edges.mplusObject <- function(x, label = "est_sig", ...){
#   estimate <- table_results(x, ...)
#   estimate <- estimate[grepl("\\.(ON|WITH|BY)\\.", estimate$label), ]
#   estimate$from <- estimate$to <- NA
#   tmp <- do.call(rbind, strsplit(estimate$label, "\\."))
#   tmp[tmp[, 2] == "ON", ] <- tmp[tmp[, 2] == "ON", 3:1]
#   tmp <- cbind(tmp, "last")
#   tmp[tmp[, 2] == "WITH", 4] <- "none"
#   tmp <- setNames(data.frame(tmp[, -2], label = estimate[[label]]), c("from", "to", "arrow", "label"))
#   class(tmp) <- c("tidy_edges", class(tmp))
#   row.names(tmp) <- NULL
#   tmp
# }


#' @method get_edges lavaan
#' @export
get_edges.lavaan <- function(x, label = "est_sig_std", ...){
    Args <- as.list(match.call()[-1])
    Args$x <- table_results(x, all = TRUE)
    do.call(get_edges, Args)
}

#' @method get_edges mplus.object
#' @export
get_edges.mplus.object <- get_edges.lavaan



#' @method get_edges tidy_results
#' @export
get_edges.tidy_results <- function(x, label = "est_sig_std", ...){
  Args <- as.list(match.call())[-1]
  if("group" %in% names(x)){
    x_list <- lapply(unique(x$group), function(i){
      Args$x <- x[x$group == i, -which(names(x) == "group")]
      tmp <- do.call(get_edges, Args)
      tmp$group <- i
      tmp
    })
    return(do.call(rbind, x_list))
  }
  if("level" %in% names(x)){
    x_list <- lapply(unique(x$level), function(i){
      Args$x <- x[x$level == i, -which(names(x) == "level")]
      tmp <- do.call(get_edges, Args)
      tmp$from <- paste0(tmp$from, ".", i)
      tmp$to <- paste0(tmp$to, ".", i)
      tmp$level <- i
      tmp
    })
    return(do.call(rbind, x_list))
  }
  x <- x[x$op %in% c("~", "~~", "=~"), ]
  x$from <- x$to <- NA
  x$arrow <- "last"
  x$arrow[x$op == "~~"] <- "none" # Covariances
  x$arrow[x$op == "~~" & x$lhs == x$rhs] <- "both" # Variances
  x$arrow[x$op == "~"] <- "first"
  tmp <- x[, c("lhs", "rhs", "arrow", label)]
  tmp <- setNames(tmp, c("from", "to", "arrow", "label"))
  tmp$connector <- "line"
  tmp$connector[x$op == "~~"] <- "curve"
  #tmp$connector[x$op == "~~" & x$lhs == x$rhs] <- "var"
  tmp$curvature <- tmp$connect_to <- tmp$connect_from <- NA
  tmp$curvature[tmp$connector == "curve"] <- .1
  class(tmp) <- c("tidy_edges", class(tmp))
  attr(tmp, "which_label") <- label
  row.names(tmp) <- NULL
  tmp
}


#' @title Convert object to layout
#' @description Convert an object to a tidy_layout for a SEM graph.
#' @param x Object to convert to a tidy_layout. The default argument reads a
#' selected matrix from the clipboard.
#' To use this functionality, specify your layout in a spreadsheet program,
#' select the block of cells, and copy it to the clipboard.
#' @return Object of class 'tidy_layout'
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname as.layout
#' @keywords tidy_graph
#' @export
as.layout <- function(x){
  UseMethod("as.layout")
}

#' @method as.layout data.frame
#' @export
as.layout.data.frame <- function(x){
  Args <- as.list(match.call()[-1])
  Args$x <- as.matrix(x)
  do.call(as.layout, Args)
}

#' @method as.layout matrix
#' @export
as.layout.matrix <- function(x){
  mat <- x
  mat[is.na(mat)] <- ""
  nodes_long <- setNames(as.data.frame.table(mat), c("y", "x", "name"))
  nodes_long[1:2] <- lapply(nodes_long[1:2], as.numeric)
  nodes_long$y <- (max(nodes_long$y)+1)-nodes_long$y
  nodes_long$name <- as.character(nodes_long$name)
  #nodes_long$shape <- as.vector(shape)
  nodes_long <- nodes_long[!nodes_long$name == "", ]
  row.names(nodes_long) <- NULL
  class(nodes_long) <- c("tidy_layout", class(nodes_long))
  nodes_long
}

#' @title Generate graph layout
#' @description Generate a tidy_layout for a SEM graph by specifying node names,
#' and empty strings or \code{NA} values for spaces.
#' @param ... Character arguments corresponding to layout elements. Use node
#' names, empty strings (""), or NA values.
#' @param rows Numeric, indicating the number of rows of the graph.
#' @return Object of class 'tidy_layout'
#' @examples
#' get_layout("c", "",  "d",
#'            "",  "e", "", rows = 2)
#' @rdname layout
#' @keywords tidy_graph
#' @seealso as.layout
#' @export
get_layout <- function(..., rows = NULL){
  Args <- as.list(match.call()[-1])
  if("rows" %in% names(Args)){
    Args$rows <- NULL
  } else {
    if(length(sapply(Args, is.numeric)) == 1){
      Args[which(sapply(Args, is.numeric))] <- NULL
    } else {
      stop("Provide 'rows' argument.", call. = FALSE)
    }
  }
  if(!(length(Args) %% rows == 0)){
    stop("Number of arguments is not a multiple of rows = ", rows, call. = FALSE)
  }
  vec <- do.call(c, Args)

  Args <- list(x = do.call(matrix, list(
    data = vec,
    nrow = rows,
    byrow = TRUE
  )))
  out <- do.call(as.layout, Args)
  class(out) <- c("tidy_layout", class(out))
  out
}



.euclidean_distance <- function(p, q){
  sqrt(sum((p - q)^2))
}

.manhattan_distance <- function(p, q){
  sum(abs(p-q))
}

match.call.defaults <- function(...) {
  call <- evalq(match.call(expand.dots = FALSE), parent.frame(1))
  formals <- evalq(formals(), parent.frame(1))

  for(i in setdiff(names(formals), names(call)))
    call[i] <- list( formals[[i]] )


  match.call(sys.function(sys.parent()), call)
}

#' @importFrom ggplot2 geom_curve
.plot_curves2 <- function(p, df, text_size, ...){
  an = 90
  #curvature = df$curvature#Flip curvature to mirror the curve
  ncp = 1
  df_edges <- data.frame(lapply(df[, c("edge_xmin", "edge_ymin", "edge_xmax", "edge_ymax", "curvature")], as.numeric))
  df_curves <- data.frame(df, t(apply(df_edges, 1, function(x){unlist(calcControlPoints(x[[1]], x[[2]], x[[3]], x[[4]], angle = an, curvature = x[[5]], ncp = ncp))})))
  for(cur in unique(df$curvature)){
    p <- p + geom_curve(data = df_curves[df_curves$curvature == cur, ], aes_string(x = "edge_xmin", y = "edge_ymin", xend = "x", yend = "y"), curvature = cur,
                        angle = an, linetype = 2) +
      geom_curve(data = df_curves[df_curves$curvature == cur, ], aes_string(x = "x", y = "y", xend = "edge_xmax", yend = "edge_ymax"), curvature = cur,
                 angle = an, linetype = 2)
  }
  p + geom_label(data = df_curves,
               aes_string(x = "x", y = "y", label = "label"),
               size = text_size, fill = "white", label.size = NA)

}

.plot_variances <- function(p, df, text_size, diameter, ...) {
  npoints <- 20
  radius <- diameter / 2
  xlabel <- xcenter <- df$edge_xmin
  ylabel <- ycenter <- df$edge_ymin
  offset <- rep(1.5, length(xcenter))
  ycenter[df$connect_from == "top"] <-
    ycenter[df$connect_from == "top"] + radius
  ylabel[df$connect_from == "top"] <-
    ylabel[df$connect_from == "top"] + diameter
  ycenter[df$connect_from == "bottom"] <-
    ycenter[df$connect_from == "bottom"] - radius
  ylabel[df$connect_from == "bottom"] <-
    ylabel[df$connect_from == "bottom"] - diameter
  offset[df$connect_from == "bottom"] <- .5
  xcenter[df$connect_from == "left"] <-
    xcenter[df$connect_from == "left"] + radius
  xlabel[df$connect_from == "left"] <-
    xlabel[df$connect_from == "left"] + diameter
  offset[df$connect_from == "left"] <- 0
  xcenter[df$connect_from == "right"] <-
    xcenter[df$connect_from == "right"] - radius
  xlabel[df$connect_from == "right"] <-
    xlabel[df$connect_from == "right"] - diameter
  offset[df$connect_from == "right"] <- 1

  df_label <- data.frame(x = xlabel,
                         y = ylabel,
                         label = df$label)

  df_ellipse <-
    data.frame(do.call(rbind, lapply(1:length(xcenter), function(this_var) {
      point_seq <-
        seq((offset[[this_var]] * pi), (2 + offset[[this_var]]) * pi, length.out = npoints) %% (2 *
                                                                                                  pi)
      matrix(
        c(
          xcenter[[this_var]] + radius * cos(point_seq),
          ycenter[[this_var]] + radius * sin(point_seq),
          rep(this_var, npoints)
        ),
        nrow = npoints,
        ncol = 3,
        dimnames = list(NULL, c("x", "y", "var"))
      )
    })))

  p + geom_path(
    data = df_ellipse,
    aes_string(x = "x", y = "y", group = "var"),
    linetype = 1,
    arrow = arrow(
      angle = 25,
      length = unit(.1, "inches"),
      ends = "both",
      type = "closed"
    )
  ) +
    geom_label(data = df_label, aes_string(x = "x", y = "y", label = "label"),
               size = text_size, fill = "white", label.size = NA)
}

.plot_lines <- function(p, df, text_size, ...){
  if(any(df$arrow != "none")){
    p <- p + geom_segment(data = df[!df$arrow == "none", ],
                          aes_string(
                            x = "edge_xmin",
                            xend = "edge_xmax",
                            y = "edge_ymin",
                            yend = "edge_ymax"),
                          arrow = arrow(angle = 25, length = unit(.1, "inches"), ends = df$arrow[!df$arrow == "none"], type = "closed"), arrow.fill = "black")
  }
  if(any(df$arrow == "none")){
    p <- p + geom_segment(data = df[df$arrow == "none", ],
                          aes_string(
                            x = "edge_xmin",
                            xend = "edge_xmax",
                            y = "edge_ymin",
                            yend = "edge_ymax"))
  }
  p <- p + geom_label(data = df,
                      aes_string(x = "text_x", y = "text_y", label = "label"),
                      size = text_size, fill = "white", label.size = NA)
}

.plot_nodes <- function(p, df, text_size, ellipses_width, ellipses_height){
  if(any(df$shape == "rect")){
    p <- p + geom_rect(data = df[df$shape == "rect", ], aes_string(xmin = "node_xmin", xmax = "node_xmax", ymin = "node_ymin", ymax = "node_ymax"), fill = "white", colour = "black")
  }
  if(any(df$shape == "oval")){
    p <- .oval_node(p, x = df[df$shape == "oval", ]$x, y = df[df$shape == "oval", ]$y, oval_width = ellipses_width, oval_height = ellipses_height, npoints = 360)
  }
  p + geom_text(data = df, aes_string(x = "x", y = "y", label = "label"), size = text_size)
}

.determine_connections <- function(df_nodes, df_edges, angle){
  # Begin recursion
  if("group" %in% names(df_nodes)){
    x_list <- lapply(unique(df_nodes$group), function(i){
      .determine_connections(df_nodes = df_nodes[df_nodes$group == i, -which(names(df_nodes) == "group")],
                             df_edges = df_edges[df_edges$group == i, -which(names(df_edges) == "group")],
      angle = angle)
    })
    return(do.call(rbind, x_list))
  }
  if("level" %in% names(df_nodes)){
    x_list <- lapply(unique(df_nodes$level), function(i){
      .determine_connections(df_nodes = df_nodes[df_nodes$level == i, -which(names(df_nodes) == "level")],
                             df_edges = df_edges[df_edges$level == i, -which(names(df_edges) == "level")],
                             angle = angle)
    })
    return(do.call(rbind, x_list))
  }
  # End recursion
  connector_sides <-
    cbind(c("left", "right", "bottom", "top")[rep(1:4, each = 4)],
          c("left", "right", "bottom", "top")[rep(1:4, 4)])
  out <- matrix(nrow = nrow(df_edges), ncol = 2)
  #df_nodes <- df_nodes[order(df_nodes$node_id), ]

  # Instead of left and right col, split graph down the middle (and split 5 col like 3,2)
  # Apply left-side connections to all same-col cors in left side of plot
  same_column <- df_nodes$x[match(df_edges$from, df_nodes$name)] == df_nodes$x[match(df_edges$to, df_nodes$name)]
  #left_col <- df_nodes$x[match(df_edges$from, df_nodes$name)] == min(df_nodes$x)
  #right_col <- df_nodes$x[match(df_edges$from, df_nodes$name)] == max(df_nodes$x)
  midpoint_df <- mean(range(df_nodes$x))
  left_half <- df_nodes$x[match(df_edges$from, df_nodes$name)] < midpoint_df
  out[same_column & left_half & df_edges$connector == "curve", ] <- c("left", "left")
  out[same_column & !left_half & df_edges$connector == "curve", ] <- c("right", "right")
  # For rows
  same_row <- df_nodes$y[match(df_edges$from, df_nodes$name)] == df_nodes$y[match(df_edges$to, df_nodes$name)]
  #bottom_row <- df_nodes$y[match(df_edges$from, df_nodes$name)] == min(df_nodes$y)
  #top_row <- df_nodes$y[match(df_edges$from, df_nodes$name)] == max(df_nodes$y)
  midpoint_df <- mean(range(df_nodes$y))
  top_half <- df_nodes$y[match(df_edges$from, df_nodes$name)] > midpoint_df
  out[same_row & !top_half & df_edges$connector == "curve", ] <- c("bottom", "bottom")
  out[same_row & top_half & df_edges$connector == "curve", ] <- c("top", "top")
  incomplete <- is.na(out[,1])
  df_edges <- df_edges[incomplete, ]

  if(!is.null(angle)){
    out[incomplete, ] <- t(mapply(function(from, to){
      if(angle > 180) angle <- 180
      if(angle < 0) angle <- 0
      fx <- df_nodes$x[df_nodes$name == from]
      tx <- df_nodes$x[df_nodes$name == to]
      fy <- df_nodes$y[df_nodes$name == from]
      ty <- df_nodes$y[df_nodes$name == to]

      if(!(fx == tx | fy == ty)){

        dx <- tx-fx
        dy <- ty-fy
        an <- (atan2(dy,dx)*(180/pi)) %% 360
        if(an > 90+.5*angle & an < 270-.5*angle){
          return(c("left", "right"))
        }
        if(an <= 90+(.5*angle) & an >= 90-(.5*angle)){
          return(c("top", "bottom"))
        }
        if(an <  90-.5*angle | an > 270+.5*angle){
          return(c("right", "left"))
        }
        return(c("bottom", "top"))
      } else {
        if(fx == tx){
          list(c("bottom", "top"), c("top", "bottom"))[[(((ty-fy)>0)+1)]]
        } else {
          list(c("left", "right"), c("right", "left"))[[(((tx-fx)>0)+1)]]
        }
      }

    }, from = df_edges$from, to = df_edges$to))
  } else {
    out[incomplete, ] <- t(mapply(function(from, to) {
      from_mat <-
        as.matrix(rbind(
          expand.grid(x = unlist(df_nodes[df_nodes$name == from, c("node_xmin", "node_xmax")]),
                      y = unlist(df_nodes[df_nodes$name == from, "y"])),
          expand.grid(x = unlist(df_nodes[df_nodes$name == from, "x"]),
                      y = unlist(df_nodes[df_nodes$name == from, c("node_ymin", "node_ymax")]))
        ))

      to_mat <-
        as.matrix(rbind(
          expand.grid(x = unlist(df_nodes[df_nodes$name == to, c("node_xmin", "node_xmax")]),
                      y = unlist(df_nodes[df_nodes$name == to, "y"])),
          expand.grid(x = unlist(df_nodes[df_nodes$name == to, "x"]),
                      y = unlist(df_nodes[df_nodes$name == to, c("node_ymin", "node_ymax")]))
        ))

      connector_sides[which.min(mapply(
        function(from, to) {
          do.call(.euclidean_distance, list(p = from_mat[from, ], q = to_mat[to, ]))
        },
        from = rep(1:4, each = 4),
        to = rep(1:4, 4)
      )), ]

    }, from = df_edges$from, to = df_edges$to))
  }
  out
}

#' @importFrom stats dist
.plot_curves <- function(p, df, text_size, npoints = 101, ...) {
  point_seq <- seq(pi, 2*pi,length.out = npoints) #%% (2*pi)
  df_curves <- data.frame(do.call(rbind, lapply(1:nrow(df), function(rownum){
    this_row <- df[rownum, ]
    x <- c(this_row[["edge_xmin"]], this_row[["edge_xmax"]])
    y <- c(this_row[["edge_ymin"]], this_row[["edge_ymax"]])
    Rx = dist(cbind(x, y))/2
    Ry = this_row[["curvature"]] * Rx
    Cx <- mean(x)
    Cy <- mean(y)
    angles <- atan2(y = y - Cy, x = x - Cx)
    theta = angles[2]
    matrix(
      c(Rx * cos(point_seq)*cos(theta) - Ry * sin(point_seq) * sin(theta)+Cx,
        Rx * cos(point_seq) * sin(theta) + Ry * sin(point_seq) * cos(theta)+Cy,
        rep(rownum, npoints)),
      nrow = npoints, ncol = 3, dimnames = list(NULL, c("x", "y", "id"))
    )
  })))
  df_label <- df_curves[seq(ceiling(npoints/2), nrow(df_curves), by = npoints), ]
  df_label$label <- df$label
  p + geom_path(
    data = df_curves,
    aes_string(x = "x", y = "y", group = "id"),
    linetype = 2
  ) +
    geom_label(
      data = df_label,
      aes_string(x = "x", y = "y", label = "label"),
      size = text_size,
      fill = "white",
      label.size = NA
    )
}
