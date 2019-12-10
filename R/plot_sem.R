#' @title Prepare to plot a SEM graph
#' @description Starting with nodes, a layout, and edges, prepare several data
#' objects that can be rendered into a SEM graph. Using this function allows
#' users to manually change the default graph specification before plotting it.
#' @param nodes Object of class 'tidy_nodes'
#' @param layout Object of class 'tidy_layout'
#' @param edges Object of class 'tidy_edges', Default: NULL
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
#' @return Object of class 'sem_graph'
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname prepare_sem_graph
#' @export
prepare_sem_graph <- function(nodes,
                     layout,
                     edges = NULL,
                     rect_width = 1.2,
                     rect_height = .8,
                     ellipses_a = 1,
                     ellipses_b = 1,
                     spacing_x = 1,
                     spacing_y = 1,
                     text_size = 4
                     ){
  args <- as.list(match.call())[-1]
  myfor <- formals(prepare_sem_graph)
  for ( v in names(myfor)){
    if (!(v %in% names(args)))
      args <- append(args,myfor[v])
  }

  df_edges <- edges
  if(!all((df_edges$from %in% layout$param) & (df_edges$to %in% layout$param))){
    warning("Some edges involve nodes not in layout. These were dropped.")
    df_edges <- df_edges[(df_edges$from %in% layout$param) & (df_edges$to %in% layout$param), ]
  }

  df_nodes <- merge(layout, nodes, by = "param")

  df_edges$from <- df_nodes$node_id[match(df_edges$from, df_nodes$param)]
  df_edges$to <- df_nodes$node_id[match(df_edges$to, df_nodes$param)]


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

  connector_sides <-
    cbind(c("left", "right", "bottom", "top")[rep(1:4, each = 4)],
          c("left", "right", "bottom", "top")[rep(1:4, 4)])

  connect_cols <- t(mapply(function(from, to) {
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
        euclidean_distance(from_mat[from, ], to_mat[to, ])
      },
      from = rep(1:4, each = 4),
      to = rep(1:4, 4)
    )), ]

  }, from = df_edges$from, to = df_edges$to))
  #df_edges <- setNames(data.frame(t(mapply(function(from, to){c(df_nodes$node_xmax[from], df_nodes$node_xmin[to], df_nodes$y[from], df_nodes$y[to])}, from = edges[, 1], to = edges[, 2]))), c("edge_xmin", "edge_xmax", "edge_ymin", "edge_ymax"))
  df_edges$connect_from <- connect_cols[, 1]
  df_edges$connect_to <- connect_cols[, 2]

  out <- args
  out$nodes <- df_nodes[, c("node_id", "param", "shape", "label","x", "y", "node_xmin", "node_xmax", "node_ymin", "node_ymax")]
  out$edges <- df_edges
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
  #browser()
  p <- ggplot(NULL)
  if(any(df_nodes$shape == "rect")){
    p <- p + geom_rect(data = df_nodes[df_nodes$shape == "rect", ], aes_string(xmin = "node_xmin", xmax = "node_xmax", ymin = "node_ymin", ymax = "node_ymax"), fill = "white", colour = "black")
  }
  if(any(df_nodes$shape == "oval")){

    p <- p + geom_ellipse(data = df_nodes[df_nodes$shape == "oval", ], aes_string(x0 = "x", y0 = "y", a = .5*ellipses_a, b = .5*ellipses_b, angle = 0), fill = "white", colour = "black")
  }
  p <- p + geom_text(data = df_nodes, aes_string(x = "x", y = "y", label = "label"), size = text_size) +
    geom_segment(data = df_edges[!df_edges$arrow %in% c("none", "curve"), ], aes_string(x = "edge_xmin",
                                      xend = "edge_xmax",
                                      y = "edge_ymin",
                                      yend = "edge_ymax"
    ), arrow = arrow(angle = 25, length = unit(.1, "inches"), ends = df_edges[!df_edges$arrow == "none", ]$arrow, type = "closed"), arrow.fill = "black") +
    geom_segment(data = df_edges[df_edges$arrow == "none", ], aes_string(x = "edge_xmin",
                                                                   xend = "edge_xmax",
                                                                   y = "edge_ymin",
                                                                   yend = "edge_ymax"
    )) +
#
# # Part segment, part curve ------------------------------------------------
#     browser()
#     geom_curve(data = df_edges[df_edges$arrow == "curve", ],
#                aes_string(x = edge_xmin,
#                    xend = edge_xmax,
#                    y = edge_ymin,
#                    yend = edge_ymax)) +
   geom_label(data = df_edges[!df_edges$arrow == "curve", ],
              aes_string(x = "text_x", y = "text_y", label = "label"), size = text_size, fill = "white", label.size = NA)+

# # End part segment, part curve --------------------------------------------
#

    theme(axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(), legend.position = "none",
          panel.background = element_blank(), panel.border = element_blank(),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          plot.background = element_blank())
  p
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

#' @method get_nodes mplusObject
#' @export
get_nodes.mplusObject <- function(x, ...){
  latent <- x$parameters$unstandardized$paramHeader
  latent <- latent[grepl("\\.BY$", latent)]
  latent <- unique(gsub("\\.BY$", "", latent))

  nodes <- x$parameters$unstandardized$param
  nodes <- nodes[!grepl("\\$\\d+$", nodes)]
  nodes <- data.frame(node_id = 1:length(unique(nodes)), param = unique(nodes), shape = c("rect", "oval")[(unique(nodes) %in% latent)+1])
  class(nodes) <- c("tidy_nodes", class(nodes))
  nodes
}

#' @method get_nodes lavaan
#' @export
#' @importFrom lavaan parameterTable lavInspect
get_nodes.lavaan <- function(x, ...){
  pars <- parameterTable(x)
  latent <- unique(pars$lhs[pars$op == "=~"])
  nodes <- c(latent, unlist(lapply(lavInspect(x, "free"), rownames)))
  nodes <- data.frame(node_id = 1:length(unique(nodes)), param = unique(nodes), shape = c("rect", "oval")[(unique(nodes) %in% latent)+1])
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
  estimate <- printResultsTable(x, ...)
  estimate <- estimate[grepl("\\.(ON|WITH|BY)\\.", estimate$label), ]
  estimate$from <- estimate$to <- NA
  tmp <- do.call(rbind, strsplit(estimate$label, "\\."))
  tmp[tmp[, 2] == "ON", ] <- tmp[tmp[, 2] == "ON", 3:1]
  tmp <- cbind(tmp, "last")
  tmp[tmp[, 2] == "WITH", 4] <- "none"
  tmp <- setNames(data.frame(tmp[, -2], label = estimate[[label]]), c("from", "to", "arrow", "label"))
  class(tmp) <- c("tidy_edges", class(tmp))
  tmp
}


#' @method get_edges lavaan.data.frame
#' @export
get_edges.lavaan.data.frame <- function(x, label = "est_sig", ...){

  if(label == "est_sig"){
    x$est_sig <- paste0(formatC(x$est.std, digits = 2, format = "f"), ifelse(x$pvalue<.05, "*", ""), ifelse(x$pvalue<.01, "*", ""), ifelse(x$pvalue<.001, "*", ""))
    x$est_sig <- gsub("NA", "", x$est_sig)
  }
  estimate <- x
  estimate <- estimate[grepl("^(~|~~|=~)$", estimate$op), ]
  estimate$from <- estimate$to <- NA
  estimate$arrow <- "last"
  estimate$arrow[estimate$op == "~~"] <- "none"
  estimate$arrow[estimate$op == "~"] <- "first"
  #tmp <- do.call(rbind, strsplit(estimate$label, "\\."))
  #tmp[tmp[, 2] == "ON", ] <- tmp[tmp[, 2] == "ON", 3:1]
  #tmp <- cbind(tmp, "last")
  #tmp[tmp[, 2] == "WITH", 4] <- "none"
  tmp <- estimate[, c("lhs", "rhs", "arrow", label)]
  tmp <- setNames(tmp, c("from", "to", "arrow", "label"))
  class(tmp) <- c("tidy_edges", class(tmp))
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
  nodes_long <- setNames(as.data.frame.table(mat), c("y", "x", "param"))
  nodes_long[1:2] <- lapply(nodes_long[1:2], as.numeric)
  nodes_long$y <- (max(nodes_long$y)+1)-nodes_long$y
  nodes_long$param <- as.character(nodes_long$param)
  #nodes_long$shape <- as.vector(shape)
  nodes_long <- nodes_long[!nodes_long$param == "", ]
  class(nodes_long) <- c("tidy_layout", class(nodes_long))
  nodes_long
}

euclidean_distance <- function(p,q){
  sqrt(sum((p - q)^2))
}

match.call.defaults <- function(...) {
  call <- evalq(match.call(expand.dots = FALSE), parent.frame(1))
  formals <- evalq(formals(), parent.frame(1))

  for(i in setdiff(names(formals), names(call)))
    call[i] <- list( formals[[i]] )


  match.call(sys.function(sys.parent()), call)
}
