.sem_node_geometry <- function(df, ids = seq_len(nrow(df))) {
  if (nrow(df)) {
    coords <- as.matrix(df[c('x', 'y', 'node_xmin', 'node_xmax',
                             'node_ymin', 'node_ymax')])
  }
  df$.sem_id <- ids
  df$width <- df$node_xmax - df$node_xmin
  df$height <- df$node_ymax - df$node_ymin
  df$rx <- df$width / 2
  df$ry <- df$height / 2
  labels <- df
  labels$label_x <- df$x
  labels$label_y <- df$y
  list(shapes = df, labels = labels)
}

.sem_edge_geometry <- function(df, npoints = 101L,
                               ids = seq_len(nrow(df))) {
  if (!nrow(df)) {
    df$label_x <- numeric(0)
    df$label_y <- numeric(0)
    return(list(paths = data.frame(x = numeric(), y = numeric(),
                                   .sem_id = ids, .sem_t = numeric()), labels = df))
  }
  if (!'curvature' %in% names(df)) df$curvature <- 0
  if (!'label_location' %in% names(df)) df$label_location <- 0.5
  if(anyNA(df$label_location)) df$label_location[is.na(df$label_location)] <- 0.5
  df$.sem_id <- ids
  paths <- vector('list', nrow(df))
  labels <- df
  labels$label_x <- NA_real_
  labels$label_y <- NA_real_
  for (i in seq_len(nrow(df))) {
    A <- c(df$edge_xmin[i], df$edge_ymin[i])
    B <- c(df$edge_xmax[i], df$edge_ymax[i])
    if (any(!is.finite(c(A, B)))) stop('Nonfinite edge endpoints at row ', i)
    angle <- df$curvature[i]
    # NA is treated as straight for backwards compatibility. In tidySEM 0.3.1
    # and up, NA typically means it should be optimized - but that should happen
    # before this function is called.
    t <- if (is.na(angle) || angle == 0) c(0, 0.5, 1) else seq(0, 1, length.out = npoints)
    xy <- .circular_arc(A[1], A[2], B[1], B[2], angle = angle, t = t)
    xy$.sem_id <- rep(ids[i], nrow(xy))
    xy$.sem_t <- t
    paths[[i]] <- xy
    lab <- .circular_arc(A[1], A[2], B[1], B[2], angle = angle,
                         t = df$label_location[i])
    labels$label_x[i] <- lab$x[1]
    labels$label_y[i] <- lab$y[1]
  }
  list(paths = do.call(rbind, paths), labels = labels)
}

.sem_variance_geometry <- function(
    df,
    diameter = 0.8,
    npoints = 101L,
    ids = seq_len(nrow(df)),
    gap = 20
) {

  if (nrow(df) == 0L) {

    df$.sem_id <- ids
    df$label_x <- numeric(0)
    df$label_y <- numeric(0)

    if (!"label_location" %in% names(df)) {
      df$label_location <- numeric(0)
    }

    return(list(
      paths = data.frame(
        x = numeric(0),
        y = numeric(0),
        .sem_id = ids,
        .sem_t = numeric(0)
      ),
      labels = df
    ))
  }
  if (!"label_location" %in% names(df)) {
    df$label_location <- 0.5
  }

  df$label_location[is.na(df$label_location)] <- 0.5

  df$.sem_id <- ids

  labels <- df
  labels$label_x <- numeric(nrow(df))
  labels$label_y <- numeric(nrow(df))

  sides <- c("top", "bottom", "left", "right")

  if (anyNA(df$connect_from) ||
      any(!df$connect_from %in% sides)) {
    stop("Invalid variance connection side.")
  }

  if (any(!is.finite(df$edge_xmin)) ||
      any(!is.finite(df$edge_ymin))) {
    stop("Variance endpoints must be finite.")
  }

  radius <- diameter / 2

  # Angle pointing from the circle center
  # towards the node attachment point.
  start_angle <- c(
    top = 3 * pi / 2,
    bottom = pi / 2,
    left = 0,
    right = pi
  )

  # Half of the opening around the attachment point.
  half_gap <- gap * pi / 360

  paths <- vector("list", nrow(df))

  for (i in seq_len(nrow(df))) {

    side <- as.character(df$connect_from[i])

    anchor <- c(
      df$edge_xmin[i],
      df$edge_ymin[i]
    )

    theta0 <- start_angle[[side]]

    # Circle center is displaced outward
    # from the node by one radius.
    center <- anchor - radius * c(
      cos(theta0),
      sin(theta0)
    )

    # Travel almost one full revolution,
    # leaving a small opening at the node.
    t <- seq(0, 1, length.out = npoints)

    theta <- theta0 + half_gap +
      t * (2 * pi - 2 * half_gap)

    paths[[i]] <- data.frame(
      x = center[1] + radius * cos(theta),
      y = center[2] + radius * sin(theta),
      .sem_id = rep(ids[i], npoints),
      .sem_t = t
    )

    # Label follows the same open circular path.
    label_t <- df$label_location[i]

    label_theta <- theta0 + half_gap +
      label_t * (2 * pi - 2 * half_gap)

    labels$label_x[i] <-
      center[1] + radius * cos(label_theta)

    labels$label_y[i] <-
      center[2] + radius * sin(label_theta)
  }

  list(
    paths = do.call(rbind, paths),
    labels = labels
  )
}

.sem_graph_geometry <- function(graph) {

  nodes <- graph$nodes
  edges <- graph$edges

  node_ids <- seq_len(nrow(nodes))
  edge_ids <- seq_len(nrow(edges))

  # Filter invisible nodes
  if ("show" %in% names(nodes)) {
    keep <- !is.na(nodes$show) & nodes$show
    nodes <- nodes[keep, , drop = FALSE]
    node_ids <- node_ids[keep]
  }

  if (!nrow(nodes)) {
    stop("No nodes left to plot.")
  }

  # Filter invisible edges
  if ("show" %in% names(edges)) {
    keep <- !is.na(edges$show) & edges$show
    edges <- edges[keep, , drop = FALSE]
    edge_ids <- edge_ids[keep]
  }

  # Match nodes and edges within their respective panels
  if (nrow(edges)) {

    panel_cols <- intersect(c("group", "level"), intersect(names(nodes), names(edges)))
    node_keys <- do.call(paste, c(nodes[c("name", panel_cols)], sep = "|"))
    # Reject ambiguous node identities
    if (anyDuplicated(node_keys)) {
      stop("Duplicate node identities within a panel.")
    }

    from_keys <- do.call(paste, c(edges[c("from", panel_cols)], sep = "|"))
    to_keys <- do.call(paste, c(edges[c("to", panel_cols)], sep = "|"))
    keep <- from_keys %in% node_keys & to_keys %in% node_keys
    if (any(!keep)) {
      message("Some edges involve nodes not shown in the layout. These were dropped.")
    }
    edges <- edges[keep, , drop = FALSE]
    edge_ids <- edge_ids[keep]
  }

  # Calculate node geometry
  node_geometry <- .sem_node_geometry(
    nodes,
    ids = node_ids
  )

  # Calculate edge attachment points
  if (nrow(edges)) {

    points <- .connect_points(nodes, edges)

    # Replace existing endpoint coordinates, if any
    edges[names(points)] <- points

    if ("curvature" %in% names(edges)) {
      edges$curvature <- as.numeric(
        as.character(edges$curvature)
      )
    }
  }

  is_variance <- edges$from == edges$to
  edge_geometry <- .sem_edge_geometry(
    edges[!is_variance, , drop = FALSE],
    ids = edge_ids[!is_variance]
  )
  variance_geometry <- .sem_variance_geometry(
    edges[is_variance, , drop = FALSE],
    diameter = graph$variance_diameter,
    ids = edge_ids[is_variance]
  )
  edge_geometry$paths <- rbind(
    edge_geometry$paths,
    variance_geometry$paths
  )
  edge_geometry$labels <- rbind(
    edge_geometry$labels,
    variance_geometry$labels
  )
  list(
    nodes = node_geometry,
    edges = edge_geometry
  )
}

.plot_sem_geometry_edges <- function(p, geometry, text_size) {

  paths <- geometry$paths
  labels <- geometry$labels

  if (!nrow(paths)) return(p)

  # Match each path point to its original edge
  idx <- match(paths$.sem_id, labels$.sem_id)

  # Copy edge aesthetics and metadata onto path points
  metadata <- labels[
    idx,
    setdiff(
      names(labels),
      c(
        "label_x", "label_y",
        ".sem_id", ".sem_t", "id"
      )
    ),
    drop = FALSE
  ]

  df_paths <- cbind(paths, metadata)
  df_paths <- df_paths[
    order(df_paths$.sem_id, df_paths$.sem_t),
    ,
    drop = FALSE
  ]
  rownames(df_paths) <- NULL
  p <- .plot_edges_internal(p, df_paths)

  # Draw labels at the explicit geometry coordinates
  if ("label" %in% names(labels)) {

    keep <- !is.na(labels$label) &
      nzchar(as.character(labels$label))

    df_labels <- labels[keep, , drop = FALSE]

    if (nrow(df_labels)) {
      p <- .plot_label_internal(
        p,
        df_labels,
        text_size
      )
    }
  }

  p
}
