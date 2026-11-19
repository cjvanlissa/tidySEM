.determine_connections <- function(
    df_nodes,
    df_edges,
    angle = NULL
) {
  # Preserve group / level recursion.
  for (column in c("group", "level")) {

    if (!column %in% names(df_nodes)) next

    groups <- unique(df_nodes[[column]])

    result <- lapply(groups, function(g) {

      node_idx <- !is.na(df_nodes[[column]]) &
        df_nodes[[column]] == g

      edge_idx <- !is.na(df_edges[[column]]) &
        df_edges[[column]] == g

      .determine_connections(
        df_nodes = df_nodes[
          node_idx,
          setdiff(names(df_nodes), column),
          drop = FALSE
        ],
        df_edges = df_edges[
          edge_idx,
          setdiff(names(df_edges), column),
          drop = FALSE
        ],
        angle = angle
      )
    })

    return(do.call(rbind, result))
  }

  .connect_sem_optimized(
    df_nodes = df_nodes,
    df_edges = df_edges,
    angle = angle
  )
}


.connect_sem_optimized <- function(
    df_nodes,
    df_edges,
    angle = NULL,
    weights = c(
      distance = 1,
      alignment = 2,
      semantic = 5,
      obstruction = 100,
      congestion = 1.5
    )
) {
  sides <- c("left", "right", "bottom", "top")

  n <- nrow(df_edges)

  out <- matrix(
    NA_character_,
    nrow = n,
    ncol = 3,
    dimnames = list(
      NULL,
      c("connect_from", "connect_to", "curvature")
    )
  )

  if (n == 0L) return(out)

  out[, 3] <- as.character(df_edges$curvature)

  # ---------------------------------------------------------
  # Helpers
  # ---------------------------------------------------------

  get_node <- function(name) {
    idx <- match(name, df_nodes$name)

    if (is.na(idx)) {
      stop("Unknown node: ", name)
    }

    df_nodes[idx, , drop = FALSE]
  }

  anchor <- function(node, side) {
    switch(
      side,
      left = c(node$node_xmin, node$y),
      right = c(node$node_xmax, node$y),
      bottom = c(node$x, node$node_ymin),
      top = c(node$x, node$node_ymax)
    )
  }

  normal <- function(side) {
    switch(
      side,
      left = c(-1, 0),
      right = c(1, 0),
      bottom = c(0, -1),
      top = c(0, 1)
    )
  }

  # Test whether a line segment crosses the interior
  # of an axis-aligned rectangle.
  segment_intersects_rect <- function(p, q, node) {

    xmin <- node$node_xmin
    xmax <- node$node_xmax
    ymin <- node$node_ymin
    ymax <- node$node_ymax

    d <- q - p

    tmin <- 0
    tmax <- 1

    for (j in 1:2) {

      bounds <- if (j == 1) {
        c(xmin, xmax)
      } else {
        c(ymin, ymax)
      }

      if (abs(d[j]) < 1e-12) {

        if (p[j] <= bounds[1] ||
            p[j] >= bounds[2]) {
          return(FALSE)
        }

      } else {

        t1 <- (bounds[1] - p[j]) / d[j]
        t2 <- (bounds[2] - p[j]) / d[j]

        lo <- min(t1, t2)
        hi <- max(t1, t2)

        tmin <- max(tmin, lo)
        tmax <- min(tmax, hi)

        if (tmin >= tmax) return(FALSE)
      }
    }

    tmin < 1 && tmax > 0
  }

  # ---------------------------------------------------------
  # Semantic preference
  # ---------------------------------------------------------

  preferred_sides <- function(edge, from_node, to_node) {

    dx <- to_node$x - from_node$x
    dy <- to_node$y - from_node$y

    if (abs(dx) >= abs(dy)) {
      preferred <- if (dx >= 0) {
        c("right", "left")
      } else {
        c("left", "right")
      }
    } else {
      preferred <- if (dy >= 0) {
        c("top", "bottom")
      } else {
        c("bottom", "top")
      }
    }

    if (!"op" %in% names(edge)) {
      return(preferred)
    }

    # Measurement edges:
    # prefer the direction of the measurement layer.
    if (edge$op == "=~") {

      if (abs(dy) > 0) {
        return(if (dy < 0) {
          c("bottom", "top")
        } else {
          c("top", "bottom")
        })
      }
    }

    preferred
  }

  # ---------------------------------------------------------
  # Evaluate all 16 connection combinations
  # ---------------------------------------------------------

  candidates <- expand.grid(
    from = sides,
    to = sides,
    stringsAsFactors = FALSE
  )

  usage <- matrix(
    0L,
    nrow = nrow(df_nodes),
    ncol = length(sides),
    dimnames = list(df_nodes$name, sides)
  )

  is_variance <- df_edges$from == df_edges$to

  # Measurement edges first, then regressions,
  # covariances, and other edges.
  priority <- rep(3L, n)

  if ("op" %in% names(df_edges)) {
    priority[df_edges$op == "=~"] <- 1L
    priority[df_edges$op == "~"] <- 2L
    priority[df_edges$op == "~~"] <- 3L
  }

  priority[is_variance] <- 4L

  edge_order <- order(priority, seq_len(n))

  # ---------------------------------------------------------
  # Non-self edges
  # ---------------------------------------------------------

  for (i in edge_order[!is_variance[edge_order]]) {

    edge <- df_edges[i, , drop = FALSE]

    from_node <- get_node(edge$from)
    to_node <- get_node(edge$to)

    preferred <- preferred_sides(
      edge, from_node, to_node
    )

    scores <- numeric(nrow(candidates))

    for (j in seq_len(nrow(candidates))) {

      sf <- candidates$from[j]
      st <- candidates$to[j]

      p <- anchor(from_node, sf)
      q <- anchor(to_node, st)

      delta <- q - p
      distance <- sqrt(sum(delta^2))

      # Direction should leave the source outward
      # and approach the target from outside.
      nf <- normal(sf)
      nt <- normal(st)

      if (distance > 0) {
        direction <- delta / distance

        alignment <- (
          1 - sum(nf * direction)
        ) + (
          1 + sum(nt * direction)
        )
      } else {
        alignment <- 4
      }

      semantic <- sum(
        c(sf, st) != preferred
      )

      # Count node intersections, including source
      # and target interiors. Valid straight connections
      # should not cross either interior.
      obstruction <- 0L

      for (k in seq_len(nrow(df_nodes))) {
        if (segment_intersects_rect(
          p, q, df_nodes[k, ]
        )) {
          obstruction <- obstruction + 1L
        }
      }

      congestion <- (
        usage[edge$from, sf] +
          usage[edge$to, st]
      )

      scores[j] <-
        weights["distance"] * distance +
        weights["alignment"] * alignment +
        weights["semantic"] * semantic +
        weights["obstruction"] * obstruction +
        weights["congestion"] * congestion
    }

    best <- which.min(scores)

    sf <- candidates$from[best]
    st <- candidates$to[best]

    out[i, 1:2] <- c(sf, st)

    usage[edge$from, sf] <-
      usage[edge$from, sf] + 1L

    usage[edge$to, st] <-
      usage[edge$to, st] + 1L
  }

  # ---------------------------------------------------------
  # Self-variances
  #
  # Choose a side facing away from the diagram center,
  # while penalizing already occupied sides.
  # ---------------------------------------------------------

  center <- c(
    mean(range(df_nodes$x)),
    mean(range(df_nodes$y))
  )

  for (i in edge_order[is_variance[edge_order]]) {

    node <- get_node(df_edges$from[i])

    displacement <- c(
      node$x - center[1],
      node$y - center[2]
    )

    scores <- vapply(sides, function(side) {

      outward <- sum(
        normal(side) * displacement
      )

      congestion <- usage[node$name, side]

      -outward +
        weights["congestion"] * congestion

    }, numeric(1))

    best <- sides[which.min(scores)]

    out[i, 1:2] <- best

    usage[node$name, best] <-
      usage[node$name, best] + 1L
  }

  out
}
