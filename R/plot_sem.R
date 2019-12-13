#' @title Prepare to plot a SEM graph
#' @description Starting with nodes, a layout, and edges, prepare several data
#' objects that can be rendered into a SEM graph. Using this function allows
#' users to manually change the default graph specification before plotting it.
#' @param layout A matrix with the layout of the graph, using the same names
#' for nodes as in the \code{edges} argument, or an object of class
#' 'tidy_layout', created with the \code{\link[tidySEM]{get_layout}} function.
#' @param edges Object of class 'tidy_edges', Default: NULL
#' @param nodes Object of class 'tidy_nodes', created with the
#' \code{\link[tidySEM]{get_nodes}} function.
#If this argument is NULL, the nodes are
# inferred from the \code{layout} argument. The advantage of using
#' @param rect_width Width of rectangles (used to display observed variables),
#' Default: 1.2
#' @param rect_height Height of rectangles (used to display observed variables),
#' Default: 0.8
#' @param ellipses_a Width of ellipses (used to display latent variables),
#' Default: 1
#' @param ellipses_b Height of ellipses (used to display latent variables),
#' Default: 1
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
# Default: "euclidean", but could be set to "manhattan".
#' @return Object of class 'sem_graph'
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname prepare_sem_graph
#' @export
prepare_sem_graph <- function(layout,
                     edges = NULL,
                     nodes = NULL,
                     rect_width = 1.2,
                     rect_height = .8,
                     ellipses_a = 1,
                     ellipses_b = 1,
                     spacing_x = 2,
                     spacing_y = 2,
                     text_size = 4,
                     curvature = .1,
                     angle = NULL
                     ){
  browser()
  Args <- as.list(match.call())[-1]
  myfor <- formals(prepare_sem_graph)
  for ( v in names(myfor)){
    if (!(v %in% names(Args)))
      Args <- append(Args,myfor[v])
  }

# Extract nodes -----------------------------------------------------------

  # pars <- table_results_lavaan(x)
  # latent <- unique(pars$lhs[pars$op == "=~"])
  # nodes <- c(latent, pars$lhs[pars$free != 0])
  # nodes <- data.frame(node_id = 1:length(unique(nodes)), name = unique(nodes), shape = c("rect", "oval")[(unique(nodes) %in% latent)+1])
  # class(nodes) <- c("tidy_nodes", class(nodes))
  # nodes


# Check if nodes exist in edges and layout --------------------------------

  df_edges <- edges
  if(!all((df_edges$from %in% layout$name) & (df_edges$to %in% layout$name))){
    warning("Some edges involve nodes not in layout. These were dropped.")
    df_edges <- df_edges[(df_edges$from %in% layout$name) & (df_edges$to %in% layout$name), ]
  }

  df_nodes <- merge(layout, nodes, by = "name")

  df_edges$from <- df_nodes$node_id[match(df_edges$from, df_nodes$name)]
  df_edges$to <- df_nodes$node_id[match(df_edges$to, df_nodes$name)]


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
    df_nodes[df_nodes$shape == "oval", c("node_xmin", "node_xmax")] <- cbind(df_nodes[df_nodes$shape == "oval", ]$x-.5*ellipses_a,
                                                                             df_nodes[df_nodes$shape == "oval", ]$x+.5*ellipses_a)
    df_nodes[df_nodes$shape == "oval", c("node_ymin", "node_ymax")] <- cbind(df_nodes[df_nodes$shape == "oval", ]$y-.5*ellipses_b,
                                                                             df_nodes[df_nodes$shape == "oval", ]$y+.5*ellipses_b)
  }

  connect_cols <- .determine_connections(df_nodes, df_edges, angle)

  #df_edges <- setNames(data.frame(t(mapply(function(from, to){c(df_nodes$node_xmax[from], df_nodes$node_xmin[to], df_nodes$y[from], df_nodes$y[to])}, from = edges[, 1], to = edges[, 2]))), c("edge_xmin", "edge_xmax", "edge_ymin", "edge_ymax"))
  df_edges$connect_from <- connect_cols[, 1]
  df_edges$connect_to <- connect_cols[, 2]

  has_curve <- which(df_edges$curvature > 0)
  df_edges$curvature[has_curve][df_edges$connect_to[has_curve] == df_edges$connect_from[has_curve] & df_edges$connect_from[has_curve] %in% c("top", "right") & df_edges$curvature[has_curve] > 0] <- -1 * df_edges$curvature[has_curve][df_edges$connect_to[has_curve] == df_edges$connect_from[has_curve] & df_edges$connect_from[has_curve] %in% c("top", "right") & df_edges$curvature[has_curve] > 0]

  out <- Args
  if(is.null(df_nodes[["label"]])) df_nodes$label <- df_nodes$name
  out$nodes <- df_nodes[, c("node_id", "name", "shape", "label","x", "y", "node_xmin", "node_xmax", "node_ymin", "node_ymax")]
  out$edges <- df_edges
  out$layout <- NULL
  class(out) <- "sem_graph"
  out
}

#' @export
#' @importFrom stats setNames
#' @importFrom ggplot2 aes_string arrow element_blank facet_grid geom_label
#' @importFrom ggplot2 geom_rect geom_segment geom_text geom_vline ggplot labs
#' @importFrom ggplot2 theme theme_bw unit
#' @importFrom ggforce geom_ellipse
plot.sem_graph <- function(x, y, ...){

  df_nodes <- x$nodes
  df_edges <- x$edges
  rect_width <- x$rect_width
  rect_height <- x$rect_height
  ellipses_a <- x$ellipses_a
  ellipses_b <- x$ellipses_b
  spacing_x <- x$spacing_x
  spacing_y <- x$spacing_y
  text_size <- x$text_size

  connect_points <- setNames(data.frame(t(
    mapply(function(from, to, startpoint, endpoint){
      c(
        switch(startpoint,
               right = df_nodes$node_xmax[which(df_nodes$node_id == from)],
               left =  df_nodes$node_xmin[which(df_nodes$node_id == from)],
               df_nodes$x[which(df_nodes$node_id == from)]),
        switch(startpoint,
               top = df_nodes$node_ymax[which(df_nodes$node_id == from)],
               bottom =  df_nodes$node_ymin[which(df_nodes$node_id == from)],
               df_nodes$y[which(df_nodes$node_id == from)]),
        switch(endpoint,
               right = df_nodes$node_xmax[which(df_nodes$node_id == to)],
               left =  df_nodes$node_xmin[which(df_nodes$node_id == to)],
               df_nodes$x[which(df_nodes$node_id == to)]),
        switch(endpoint,
               top = df_nodes$node_ymax[which(df_nodes$node_id == to)],
               bottom =  df_nodes$node_ymin[which(df_nodes$node_id == to)],
               df_nodes$y[which(df_nodes$node_id == to)])
      )},
      from = df_edges$from,
      to = df_edges$to,
      startpoint = df_edges$connect_from,
      endpoint = df_edges$connect_to
    )
  )), c("edge_xmin", "edge_ymin", "edge_xmax", "edge_ymax"))
  df_edges <- cbind(df_edges, connect_points)

  df_edges <- cbind(df_edges, setNames(data.frame(t(apply(connect_points, 1, function(x){(x[1:2]+x[3:4])/2}))), c("text_x", "text_y")))


# Make plot ---------------------------------------------------------------

  p <- ggplot(NULL)
  if(any(df_edges$connector == "line")){
    p <- .plot_lines(p, df_edges[df_edges$connector == "line", ], text_size)
  }
  if(any(df_edges$connector == "curve")){
    p <- .plot_curves(p, df = df_edges[df_edges$connector == "curve", ], text_size = text_size)
  }
  p <- .plot_nodes(p, df = df_nodes, text_size = text_size, ellipses_a = ellipses_a, ellipses_b = ellipses_b)


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


matrix_to_nodes <- function(nodes, shape){
  nodes_long <- setNames(as.data.frame.table(nodes), c("y", "x", "label"))
  nodes_long[1:2] <- lapply(nodes_long[1:2], as.numeric)
  nodes_long$y <- (max(nodes_long$y)+1)-nodes_long$y
  nodes_long$label <- as.character(nodes_long$label)
  nodes_long$shape <- as.vector(shape)
  nodes_long <- nodes_long[!nodes_long$label == "", ]
  nodes_long$node_id <- 1:nrow(nodes_long)
  nodes_long
}

#' @title Prepare graph data
#' @description Extracts nodes and edges from a SEM model object, where nodes
#' are defined as observed or latent variables, and edges
#' are defined as regression paths and covariances between variables (nodes).
#' @param x A model object of class 'mplusObject' or 'lavaan'.
#' @param ... Additional parameters to be passed to and from other functions.
#' @return An object of class 'graph_data'
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname prepare_graph
#' @keywords tidy_graph
#' @export
prepare_graph <- function(x, ...){
  UseMethod("get_nodes", x)
}

#' @method prepare_graph mplusObject
#' @export
prepare_graph.mplusObject <- function(x, ...){
}

#' @method prepare_graph mplusObject
#' @export
prepare_graph.lavaan <- function(x, ...){

}

#' @title Extract nodes from a SEM model object
#' @description Attempts to extract nodes from a SEM model object, where nodes
#' are defined as observed or latent variables.
#' @param x A model object of class 'mplusObject' or 'lavaan'.
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
get_nodes <- function(x, ...){
  UseMethod("get_nodes", x)
}

#' @method get_nodes mplus.model
#' @export
get_nodes.mplus.model <- function(x, ...){
  Args <- as.list(match.call()[-1])
  Args$x <- table_results(x, all = TRUE)
  do.call(get_nodes, Args)
  # latent <- x$parameters$unstandardized$paramHeader
  # latent <- latent[grepl("\\.BY$", latent)]
  # latent <- unique(gsub("\\.BY$", "", latent))
  #
  # nodes <- x$parameters$unstandardized$param
  # nodes <- nodes[!grepl("\\$\\d+$", nodes)]
  # nodes <- data.frame(node_id = 1:length(unique(nodes)), name = unique(nodes), shape = c("rect", "oval")[(unique(nodes) %in% latent)+1])
  # nodes$label <- nodes$name
  # class(nodes) <- c("tidy_nodes", class(nodes))
  # nodes
}

#' @method get_nodes lavaan
#' @export
#' @importFrom lavaan parameterTable lavInspect
get_nodes.lavaan <- function(x, ...){
  Args <- as.list(match.call()[-1])
  Args$x <- table_results(x, all = TRUE)
  do.call(get_nodes, Args)
}

#' @method get_nodes tidy_results
#' @export
get_nodes.tidy_results <- function(x, ...){
  if("level" %in% names(x)){
    x_list <- lapply(unique(x$level), function(i){
      tmp <- get_nodes(x = x[x$level == i, -which(names(x) == "level")])
      tmp$name <- paste0(tmp$name, ".", i)
      tmp
      })
    return(do.call(rbind, x_list))
  }
  latent <- unique(x$lhs[x$op == "=~"])
  obs <- unique(x$lhs[x$op %in% c("~~", "~", "~1")])
  nodes <- unique(c(latent, obs))
  nodes <- data.frame(node_id = 1:length(unique(nodes)), name = unique(nodes), shape = c("rect", "oval")[(unique(nodes) %in% latent)+1], stringsAsFactors = FALSE)
  if(FALSE){# any(x$lhs %in% nodes$name & x$op == "~1")
    es <- est_sig(x = x$est, sig = x$pvalue)
    x[x$lhs %in% nodes$name & x$op == "~1", ]
    match(nodes$name, x$lhs)
    nodes$label <- paste0(nodes$name, "\n", es)
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

#' @method get_edges mplusObject
#' @export
get_edges.mplusObject <- function(x, label = "est_sig", ...){
  #par_spec <- x$tech1$parameterSpecification
  estimate <- table_results(x, ...)
  estimate <- estimate[grepl("\\.(ON|WITH|BY)\\.", estimate$label), ]
  estimate$from <- estimate$to <- NA
  tmp <- do.call(rbind, strsplit(estimate$label, "\\."))
  tmp[tmp[, 2] == "ON", ] <- tmp[tmp[, 2] == "ON", 3:1]
  tmp <- cbind(tmp, "last")
  tmp[tmp[, 2] == "WITH", 4] <- "none"
  tmp <- setNames(data.frame(tmp[, -2], label = estimate[[label]]), c("from", "to", "arrow", "label"))
  class(tmp) <- c("tidy_edges", class(tmp))
  row.names(tmp) <- NULL
  tmp
}


#' @method get_edges lavaan
#' @export
get_edges.lavaan <- function(x, label = "est_sig_std", ...){
    Args <- as.list(match.call()[-1])
    Args$x <- table_results(x, all = TRUE)
    do.call(get_edges, Args)
  }


#' @method get_edges tidy_results
#' @export
get_edges.tidy_results <- function(x, label = "est_sig_std", ...){
  Args <- as.list(match.call())[-1]
  if("level" %in% names(x)){
    x_list <- lapply(unique(x$level), function(i){
      Args$x <- x[x$level == i, -which(names(x) == "level")]
      tmp <- do.call(get_edges, Args)
      tmp$from <- paste0(tmp$from, ".", i)
      tmp$to <- paste0(tmp$to, ".", i)
      tmp
    })
    return(do.call(rbind, x_list))
  }
  x <- x[x$op %in% c("~", "~~", "=~") & !x$lhs == x$rhs, ]
  x$from <- x$to <- NA
  x$arrow <- "last"
  x$arrow[x$op == "~~"] <- "none"
  x$arrow[x$op == "~"] <- "first"
  tmp <- x[, c("lhs", "rhs", "arrow", label)]
  tmp <- setNames(tmp, c("from", "to", "arrow", "label"))
  tmp$connector <- "line"
  tmp$connector[x$op == "~~"] <- "curve"
  tmp$curvature <- tmp$connect_to <- tmp$connect_from <- NA
  tmp$curvature[tmp$connector == "curve"] <- .1
  class(tmp) <- c("tidy_edges", class(tmp))
  attr(tmp, "which_label") <- label
  row.names(tmp) <- NULL
  tmp
}


#' @title Generate graph layout from a matrix
#' @description Starting with a matrix, generate a tidy_layout for a SEM graph.
#' @param mat Matrix. The default reads a selected matrix from the clipboard.
#' To use this functionality, specify your layout in a spreadsheet program,
#' select the block of cells, and copy it to the clipboard.
#' @return Object of class 'tidy_layout'
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname get_layout
#' @keywords tidy_graph
#' @export
get_layout <- function(mat = read.table("clipboard", sep = "\t", stringsAsFactors = FALSE)){
  mat <- as.matrix(mat)
  mat[is.na(mat)] <- ""
  nodes_long <- setNames(as.data.frame.table(mat), c("y", "x", "name"))
  nodes_long[1:2] <- lapply(nodes_long[1:2], as.numeric)
  nodes_long$y <- (max(nodes_long$y)+1)-nodes_long$y
  nodes_long$name <- as.character(nodes_long$name)
  #nodes_long$shape <- as.vector(shape)
  nodes_long <- nodes_long[!nodes_long$name == "", ]
  class(nodes_long) <- c("tidy_layout", class(nodes_long))
  nodes_long
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
.plot_curves <- function(p, df, text_size, ...){
  an = 90
  #curvature = df$curvature#Flip curvature to mirror the curve
  ncp = 1
  df_curves <- data.frame(df, t(apply(df[, c("edge_xmin", "edge_ymin", "edge_xmax", "edge_ymax", "curvature")], 1, function(x){unlist(calcControlPoints(x[[1]], x[[2]], x[[3]], x[[4]], angle = an, curvature = x[[5]], ncp = ncp))})))
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

.plot_nodes <- function(p, df, text_size, ellipses_a, ellipses_b){
  if(any(df$shape == "rect")){
    p <- p + geom_rect(data = df[df$shape == "rect", ], aes_string(xmin = "node_xmin", xmax = "node_xmax", ymin = "node_ymin", ymax = "node_ymax"), fill = "white", colour = "black")
  }
  if(any(df$shape == "oval")){
    p <- p + geom_ellipse(data = df[df$shape == "oval", ], aes_string(x0 = "x", y0 = "y", a = .5*ellipses_a, b = .5*ellipses_b, angle = 0), fill = "white", colour = "black")
  }
  p + geom_text(data = df, aes_string(x = "x", y = "y", label = "label"), size = text_size)
}

.determine_connections <- function(df_nodes, df_edges, angle){
  connector_sides <-
    cbind(c("left", "right", "bottom", "top")[rep(1:4, each = 4)],
          c("left", "right", "bottom", "top")[rep(1:4, 4)])
  out <- matrix(nrow = nrow(df_edges), ncol = 2)
  #df_nodes <- df_nodes[order(df_nodes$node_id), ]
  same_column <- df_nodes$x[match(df_edges$from, df_nodes$node_id)] == df_nodes$x[match(df_edges$to, df_nodes$node_id)]
  left_col <- df_nodes$x[match(df_edges$from, df_nodes$node_id)] == min(df_nodes$x)
  right_col <- df_nodes$x[match(df_edges$from, df_nodes$node_id)] == max(df_nodes$x)
  same_row <- df_nodes$y[match(df_edges$from, df_nodes$node_id)] == df_nodes$y[match(df_edges$to, df_nodes$node_id)]
  bottom_row <- df_nodes$y[match(df_edges$from, df_nodes$node_id)] == min(df_nodes$y)
  top_row <- df_nodes$y[match(df_edges$from, df_nodes$node_id)] == max(df_nodes$y)
  out[same_column & left_col & df_edges$connector == "curve", ] <- c("left", "left")
  out[same_column & right_col & df_edges$connector == "curve", ] <- c("right", "right")
  out[same_row & bottom_row & df_edges$connector == "curve", ] <- c("bottom", "bottom")
  out[same_row & top_row & df_edges$connector == "curve", ] <- c("top", "top")
  incomplete <- is.na(out[,1])
  df_edges <- df_edges[incomplete, ]

  if(!is.null(angle)){
    out[incomplete, ] <- t(mapply(function(from, to){
      if(angle > 180) angle <- 180
      fx <- df_nodes$x[df_nodes$node_id == from]
      tx <- df_nodes$x[df_nodes$node_id == to]
      fy <- df_nodes$y[df_nodes$node_id == from]
      ty <- df_nodes$y[df_nodes$node_id == to]
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
          expand.grid(x = unlist(df_nodes[df_nodes$node_id == from, c("node_xmin", "node_xmax")]),
                      y = unlist(df_nodes[df_nodes$node_id == from, "y"])),
          expand.grid(x = unlist(df_nodes[df_nodes$node_id == from, "x"]),
                      y = unlist(df_nodes[df_nodes$node_id == from, c("node_ymin", "node_ymax")]))
        ))

      to_mat <-
        as.matrix(rbind(
          expand.grid(x = unlist(df_nodes[df_nodes$node_id == to, c("node_xmin", "node_xmax")]),
                      y = unlist(df_nodes[df_nodes$node_id == to, "y"])),
          expand.grid(x = unlist(df_nodes[df_nodes$node_id == to, "x"]),
                      y = unlist(df_nodes[df_nodes$node_id == to, c("node_ymin", "node_ymax")]))
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
