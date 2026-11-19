reposition_variances <- function(
    df_edges,
    df_nodes = NULL,
    congestion_weight = 3,
    outward_weight = 2,
    proximity_weight = 5,
    semantic_weight = 10,
    clearance = 0.5
) {

  sides <- c("top", "bottom", "left", "right")

  variances <- (
    df_edges$from == df_edges$to &
      df_edges$arrow == "both"
  )

  variances[is.na(variances)] <- FALSE

  if (!any(variances)) {
    return(df_edges)
  }

  # ---------------------------------------------------------
  # 1. Count existing connections on each node side
  # ---------------------------------------------------------

  count_connections <- function(node) {

    from_sides <- df_edges$connect_from[
      df_edges$from == node & !variances
    ]

    to_sides <- df_edges$connect_to[
      df_edges$to == node & !variances
    ]

    occupied <- c(from_sides, to_sides)
    occupied <- occupied[!is.na(occupied)]

    tabulate(
      match(occupied, sides),
      nbins = length(sides)
    )
  }

  # ---------------------------------------------------------
  # 2. Calculate diagram center and scale
  # ---------------------------------------------------------

  if (!is.null(df_nodes)) {

    required <- c(
      "name", "x", "y",
      "node_xmin", "node_xmax",
      "node_ymin", "node_ymax"
    )

    if (!all(required %in% names(df_nodes))) {
      stop(
        "`df_nodes` must contain: ",
        paste(required, collapse = ", ")
      )
    }

    center <- c(
      mean(range(df_nodes$x)),
      mean(range(df_nodes$y))
    )

    diagram_scale <- max(
      diff(range(df_nodes$x)),
      diff(range(df_nodes$y)),
      1
    )
  }

  # ---------------------------------------------------------
  # 3. Outward-facing preference
  # ---------------------------------------------------------

  outward_scores <- function(node) {

    if (is.null(df_nodes)) {
      return(setNames(rep(0, 4), sides))
    }

    idx <- match(node, df_nodes$name)

    if (is.na(idx)) {
      return(setNames(rep(0, 4), sides))
    }

    dx <- (df_nodes$x[idx] - center[1]) /
      diagram_scale

    dy <- (df_nodes$y[idx] - center[2]) /
      diagram_scale

    # Positive values indicate outward-facing sides.
    c(
      top = dy,
      bottom = -dy,
      left = -dx,
      right = dx
    )
  }

  # ---------------------------------------------------------
  # 4. Estimate clearance from nearby nodes
  #
  # Each side is evaluated using a point just outside
  # the corresponding node boundary.
  #
  # Nearby nodes in that direction receive a penalty.
  # ---------------------------------------------------------

  proximity_scores <- function(node) {

    scores <- setNames(rep(0, 4), sides)

    if (is.null(df_nodes)) {
      return(scores)
    }

    idx <- match(node, df_nodes$name)

    if (is.na(idx)) {
      return(scores)
    }

    current <- df_nodes[idx, , drop = FALSE]

    others <- df_nodes[
      df_nodes$name != node,
      ,
      drop = FALSE
    ]

    if (nrow(others) == 0L) {
      return(scores)
    }

    # Distance from a point to an axis-aligned rectangle.
    point_rect_distance <- function(px, py, rect) {

      dx <- max(
        rect$node_xmin - px,
        0,
        px - rect$node_xmax
      )

      dy <- max(
        rect$node_ymin - py,
        0,
        py - rect$node_ymax
      )

      sqrt(dx^2 + dy^2)
    }

    anchors <- list(
      top = c(
        current$x,
        current$node_ymax + clearance
      ),
      bottom = c(
        current$x,
        current$node_ymin - clearance
      ),
      left = c(
        current$node_xmin - clearance,
        current$y
      ),
      right = c(
        current$node_xmax + clearance,
        current$y
      )
    )

    for (side in sides) {

      p <- anchors[[side]]

      distances <- vapply(
        seq_len(nrow(others)),
        function(j) {
          point_rect_distance(
            p[1],
            p[2],
            others[j, ]
          )
        },
        numeric(1)
      )

      # Larger penalty when another node is nearby.
      scores[side] <- sum(
        1 / (1 + distances)^2
      )
    }

    scores
  }

  # ---------------------------------------------------------
  # 5. Assign variance sides
  # ---------------------------------------------------------

  variance_indices <- which(variances)

  # Keep track of already assigned variance loops.
  assigned <- list()

  for (i in variance_indices) {

    node <- df_edges$from[i]

    congestion <- count_connections(node)
    names(congestion) <- sides

    # Account for previously placed variance loops.
    if (!is.null(assigned[[node]])) {

      previous <- assigned[[node]]

      congestion <- congestion +
        tabulate(
          match(previous, sides),
          nbins = length(sides)
        )
    }

    outward <- outward_scores(node)

    proximity <- proximity_scores(node)

    # ---------------------------------------------------------
    # Semantic preference for measurement error variances
    # ---------------------------------------------------------

    semantic_penalty <- setNames(rep(0, 4), sides)

    if (!is.null(df_nodes)) {

      preferred <- variance_semantic_preference(
        node = node,
        df_nodes = df_nodes,
        df_edges = df_edges
      )

      if (!is.null(preferred)) {
        semantic_penalty[sides != preferred] <- 1
      }
    }

    # ---------------------------------------------------------
    # Combined score (lower is better)
    # ---------------------------------------------------------

    scores <-
      congestion_weight * congestion -
      outward_weight * outward +
      proximity_weight * proximity +
      semantic_weight * semantic_penalty

    # Prefer the original side when scores tie.
    original <- df_edges$connect_from[i]

    tie_order <- sides

    if (!is.na(original) && original %in% sides) {
      tie_order <- c(
        original,
        setdiff(sides, original)
      )
    }

    best <- tie_order[
      which.min(scores[tie_order])
    ]

    df_edges$connect_from[i] <- best
    df_edges$connect_to[i] <- best

    assigned[[node]] <- c(
      assigned[[node]],
      best
    )
  }

  df_edges
}


variance_semantic_preference <- function(
    node,
    df_nodes,
    df_edges
) {
  measurement <- df_edges[
    df_edges$op == "=~" &
      df_edges$from != df_edges$to,
    ,
    drop = FALSE
  ]

  # Measurement edges run from latent to observed.
  parents <- measurement$from[
    measurement$to == node
  ]

  if (length(parents) == 0L) {
    return(NULL)
  }

  child_idx <- match(node, df_nodes$name)
  parent_idx <- match(parents, df_nodes$name)

  parent_idx <- parent_idx[!is.na(parent_idx)]

  if (length(parent_idx) == 0L) {
    return(NULL)
  }

  dx <- mean(df_nodes$x[parent_idx]) -
    df_nodes$x[child_idx]

  dy <- mean(df_nodes$y[parent_idx]) -
    df_nodes$y[child_idx]

  if (abs(dy) >= abs(dx)) {
    if (dy > 0) "bottom" else "top"
  } else {
    if (dx > 0) "left" else "right"
  }
}
