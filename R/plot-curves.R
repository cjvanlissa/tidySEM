.circular_arc <- function(
    x0, y0, x1, y1,
    angle = 30,
    t = seq(0, 1, length.out = 101)
) {
  A <- c(x0, y0)
  B <- c(x1, y1)

  delta <- B - A
  d <- sqrt(sum(delta^2))

  if (d < 1e-10) {
    return(data.frame(
      x = rep(x0, length(t)),
      y = rep(y0, length(t))
    ))
  }

  if (is.na(angle) || abs(angle) < 1e-10) {
    return(data.frame(
      x = x0 + t * delta[1],
      y = y0 + t * delta[2]
    ))
  }

  theta <- angle * pi / 180

  if (abs(theta) >= pi) {
    stop("Absolute curvature angle must be below 180.")
  }

  # Unit vector perpendicular to the chord
  normal <- c(-delta[2], delta[1]) / d

  # Signed distance from chord midpoint to circle center
  h <- d / (2 * tan(theta / 2))

  center <- (A + B) / 2 + h * normal

  radius <- d / (2 * sin(abs(theta) / 2))

  start <- atan2(
    A[2] - center[2],
    A[1] - center[1]
  )

  # Consistent signed angular sweep
  phi <- start + theta * t

  data.frame(
    x = center[1] + radius * cos(phi),
    y = center[2] + radius * sin(phi)
  )
}


.curvature_from_distance <- function(
    x0, y0, x1, y1,
    bend = 0.4,
    min_angle = 5,
    max_angle = 90
) {

  d <- sqrt(
    (x1 - x0)^2 +
      (y1 - y0)^2
  )

  if (d < 1e-10) {
    return(NA_real_)
  }

  angle <- 4 * atan(2 * bend / d) * 180 / pi

  pmin(max_angle, pmax(min_angle, angle))
}

.curve_node_penalty <- function(
    curve,
    df_nodes,
    exclude = character(),
    margin = 0.15
) {

  nodes <- df_nodes[
    !df_nodes$name %in% exclude,
    ,
    drop = FALSE
  ]

  penalty <- 0

  for (i in seq_len(nrow(nodes))) {

    node <- nodes[i, ]

    xmin <- node$node_xmin - margin
    xmax <- node$node_xmax + margin
    ymin <- node$node_ymin - margin
    ymax <- node$node_ymax + margin

    inside <- (
      curve$x >= xmin &
        curve$x <= xmax &
        curve$y >= ymin &
        curve$y <= ymax
    )

    penalty <- penalty + sum(inside)
  }

  penalty
}

.optimize_edge_curvature <- function(
    edge,
    df_nodes,
    df_edges = NULL,
    candidate_angles = c(
      -75, -60, -45, -30, -20, -10,
      10, 20, 30, 45, 60, 75
    ),
    npoints = 101,
    node_weight = 10,
    edge_weight = 5,
    bend_weight = 0.5
) {

  from_node <- df_nodes[
    match(edge$from, df_nodes$name),
    ,
    drop = FALSE
  ]

  to_node <- df_nodes[
    match(edge$to, df_nodes$name),
    ,
    drop = FALSE
  ]

  A <- .edge_anchor(
    from_node,
    edge$connect_from
  )

  B <- .edge_anchor(
    to_node,
    edge$connect_to
  )

  preferred <- .curvature_from_distance(
    A[1], A[2], B[1], B[2]
  )

  scores <- vapply(candidate_angles, function(angle) {

    curve <- .circular_arc(
      A[1], A[2],
      B[1], B[2],
      angle = angle,
      t = seq(0, 1, length.out = npoints)
    )

    node_penalty <- .curve_side_penalty(
      curve,
      edge,
      df_nodes
    )

    edge_penalty <- if (!is.null(df_edges)) {
      .curve_edge_penalty(
        curve,
        edge,
        df_edges
      )
    } else {
      0
    }

    bend_penalty <- (
      abs(angle) - preferred
    )^2 / 100

    node_weight * node_penalty +
      edge_weight * edge_penalty +
      bend_weight * bend_penalty

  }, numeric(1))

  candidate_angles[which.min(scores)]
}

.curve_side_penalty <- function(
    curve,
    edge,
    df_nodes,
    margin = 0.3
) {

  other_nodes <- df_nodes[
    !df_nodes$name %in% c(edge$from, edge$to),
    ,
    drop = FALSE
  ]

  if (!nrow(other_nodes)) {
    return(0)
  }

  # Penalize proximity to all other nodes.
  penalty <- 0

  for (i in seq_len(nrow(other_nodes))) {

    node <- other_nodes[i, ]

    dx <- pmax(
      node$node_xmin - curve$x,
      0,
      curve$x - node$node_xmax
    )

    dy <- pmax(
      node$node_ymin - curve$y,
      0,
      curve$y - node$node_ymax
    )

    distance <- sqrt(dx^2 + dy^2)

    penalty <- penalty + mean(
      1 / (distance + margin)^2
    )
  }

  penalty
}

.points_to_segment_distance <- function(
    px, py,
    ax, ay,
    bx, by
) {

  dx <- bx - ax
  dy <- by - ay

  length_squared <- dx^2 + dy^2

  if (length_squared < 1e-12) {
    return(sqrt((px - ax)^2 + (py - ay)^2))
  }

  t <- (
    (px - ax) * dx +
      (py - ay) * dy
  ) / length_squared

  t <- pmax(0, pmin(1, t))

  closest_x <- ax + t * dx
  closest_y <- ay + t * dy

  sqrt(
    (px - closest_x)^2 +
      (py - closest_y)^2
  )
}

.curve_edge_penalty <- function(
    curve,
    edge,
    df_edges,
    clearance = 0.25
) {

  penalty <- 0

  for (i in seq_len(nrow(df_edges))) {

    other <- df_edges[i, ]

    # Ignore the edge itself and edges sharing endpoints.
    if (other$from %in% c(edge$from, edge$to) ||
        other$to %in% c(edge$from, edge$to)) {
      next
    }

    required <- c(
      "edge_xmin", "edge_ymin",
      "edge_xmax", "edge_ymax"
    )

    if (!all(required %in% names(other))) {
      next
    }

    d <- .points_to_segment_distance(
      curve$x,
      curve$y,
      other$edge_xmin,
      other$edge_ymin,
      other$edge_xmax,
      other$edge_ymax
    )

    penalty <- penalty + mean(
      pmax(0, clearance - d)^2
    )
  }

  penalty
}
