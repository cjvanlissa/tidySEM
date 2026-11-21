sem_layout_integer <- function(
    nodes,
    edges,
    indicator_gap = 2L,
    group_gap = 3L,
    measurement_gap = 2L,
    structural_gap = 2L,
    orientation = c("bottom", "top"),
    order = c("input", "alphabetical")
) {
  orientation <- match.arg(orientation)
  order <- match.arg(order)

  nodes <- as.character(nodes)

  if (anyDuplicated(nodes)) {
    stop("`nodes` must contain unique names.")
  }

  if (!all(c("lhs", "rhs", "op") %in% names(edges))) {
    stop("`edges` must contain lhs, rhs, and op.")
  }

  edges <- as.data.frame(edges)

  edges$lhs <- as.character(edges$lhs)
  edges$rhs <- as.character(edges$rhs)
  edges$op  <- as.character(edges$op)

  edges <- edges[
    !is.na(edges$lhs) &
      !is.na(edges$rhs) &
      !is.na(edges$op),
    ,
    drop = FALSE
  ]

  if (any(!edges$lhs %in% nodes |
          !edges$rhs %in% nodes)) {
    stop("All edge endpoints must occur in `nodes`.")
  }

  n <- length(nodes)

  empty <- matrix(
    integer(0),
    ncol = 2,
    dimnames = list(NULL, c("x", "y"))
  )

  if (n == 0L) {

    out <- data.frame(
      x = integer(),
      y = integer(),
      name = character()
    )

    attr(out, "grid_size") <- c(
      width = 0L,
      height = 0L
    )

    class(out) <- c("tidy_layout", "data.frame")

    return(out)
  }

  original_nodes <- nodes

  if (order == "alphabetical") {
    nodes <- sort(nodes)
  }

  # ---------------------------------------------------------
  # 1. Identify measurement relationships
  # ---------------------------------------------------------

  measurement <- edges[
    edges$op == "=~" &
      edges$lhs != edges$rhs,
    c("lhs", "rhs"),
    drop = FALSE
  ]

  measurement <- unique(measurement)

  latent <- nodes[nodes %in% measurement$lhs]

  # A variable can be an indicator of multiple factors.
  # Choose one primary factor for positioning.
  # All relationships remain in the original edge table.

  primary_factor <- setNames(
    rep(NA_character_, n),
    nodes
  )

  for (i in seq_len(nrow(measurement))) {
    child <- measurement$rhs[i]
    parent <- measurement$lhs[i]

    if (child %in% latent) next

    if (is.na(primary_factor[child])) {
      primary_factor[child] <- parent
    }
  }

  indicators <- nodes[
    !is.na(primary_factor[nodes])
  ]

  # Every latent variable is a group.
  # Observed variables without a primary factor
  # become singleton groups.

  roots <- nodes[
    nodes %in% latent |
      !(nodes %in% indicators)
  ]

  if (length(roots) == 0L) {
    roots <- nodes
  }

  members <- setNames(
    vector("list", length(roots)),
    roots
  )

  for (root in roots) {
    members[[root]] <- nodes[
      primary_factor[nodes] %in% root &
        !is.na(primary_factor[nodes])
    ]
  }

  # ---------------------------------------------------------
  # 2. Map every node to its layout group
  # ---------------------------------------------------------

  node_group <- setNames(nodes, nodes)

  for (root in roots) {
    if (length(members[[root]]) > 0L) {
      node_group[members[[root]]] <- root
    }
  }

  # ---------------------------------------------------------
  # 3. Extract directed structural relationships
  #
  # lavaan: lhs ~ rhs means rhs -> lhs
  # ---------------------------------------------------------

  structural <- edges[
    edges$op == "~" &
      edges$lhs != edges$rhs,
    c("lhs", "rhs"),
    drop = FALSE
  ]

  structural <- unique(structural)

  if (nrow(structural) > 0L) {
    structural$from <- unname(
      node_group[structural$rhs]
    )
    structural$to <- unname(
      node_group[structural$lhs]
    )

    structural <- unique(
      structural[, c("from", "to")]
    )

    structural <- structural[
      structural$from != structural$to,
      ,
      drop = FALSE
    ]
  } else {
    structural <- data.frame(
      from = character(),
      to = character()
    )
  }

  # ---------------------------------------------------------
  # 4. Assign structural layers
  #
  # Kahn-style topological traversal.
  #
  # For acyclic graphs:
  #   layer(child) >= layer(parent) + 1
  #
  # For cycles:
  #   break the cycle deterministically by choosing
  #   the first remaining group.
  # ---------------------------------------------------------

  layers <- setNames(
    integer(length(roots)),
    roots
  )

  remaining <- roots
  processed <- character()

  while (length(remaining) > 0L) {

    eligible <- remaining[
      vapply(remaining, function(v) {
        parents <- structural$from[
          structural$to == v
        ]

        all(parents %in% processed)
      }, logical(1))
    ]

    if (length(eligible) == 0L) {
      # A cycle exists. Break it deterministically.
      eligible <- remaining[1L]
    }

    v <- eligible[1L]

    parents <- structural$from[
      structural$to == v
    ]

    parents <- intersect(parents, processed)

    if (length(parents) > 0L) {
      layers[v] <- max(layers[parents]) + 1L
    }

    processed <- c(processed, v)
    remaining <- setdiff(remaining, v)
  }

  # ---------------------------------------------------------
  # 5. Order groups within structural layers
  #
  # Prefer input order, with a barycentric adjustment
  # for groups having already-positioned predecessors.
  # ---------------------------------------------------------

  ordered_roots <- character()

  for (lev in sort(unique(layers))) {

    current <- roots[layers[roots] == lev]

    if (length(current) > 1L &&
        length(ordered_roots) > 0L) {

      barycenter <- vapply(current, function(v) {
        parents <- structural$from[
          structural$to == v
        ]

        idx <- match(parents, ordered_roots)
        idx <- idx[!is.na(idx)]

        if (length(idx) == 0L) {
          return(Inf)
        }

        mean(idx)
      }, numeric(1))

      current <- current[
        order(barycenter, seq_along(current))
      ]
    }

    ordered_roots <- c(ordered_roots, current)
  }

  # ---------------------------------------------------------
  # 6. Calculate horizontal group widths
  #
  # Indicator groups are laid out symmetrically:
  #
  #     L
  #   a b c
  #
  # The width depends on the number of indicators.
  # ---------------------------------------------------------

  group_width <- setNames(
    integer(length(roots)),
    roots
  )

  for (root in roots) {
    k <- length(members[[root]])

    group_width[root] <- if (k <= 1L) {
      1L
    } else {
      as.integer((k - 1L) * indicator_gap + 1L)
    }
  }

  # ---------------------------------------------------------
  # 7. Position groups on the horizontal axis
  #
  # Structural layers are ordered left to right.
  # Within each layer, groups are placed sequentially.
  # ---------------------------------------------------------

  group_x <- setNames(
    integer(length(roots)),
    roots
  )

  cursor <- 0L
  previous_layer <- NA_integer_

  for (root in ordered_roots) {

    lev <- layers[root]

    if (!is.na(previous_layer)) {
      if (lev == previous_layer) {
        cursor <- cursor + group_gap
      } else {
        cursor <- cursor + structural_gap
      }
    }

    width <- group_width[root]

    group_x[root] <- cursor + (width - 1L) %/% 2L

    cursor <- cursor + width
    previous_layer <- lev
  }

  # ---------------------------------------------------------
  # 8. Position latent and observed nodes
  #
  # All roots occupy the structural baseline.
  # Indicators occupy a separate measurement layer.
  # ---------------------------------------------------------

  pos <- matrix(
    NA_integer_,
    nrow = n,
    ncol = 2,
    dimnames = list(nodes, c("x", "y"))
  )

  root_y <- as.integer(measurement_gap)

  indicator_y <- if (orientation == "bottom") {
    0L
  } else {
    as.integer(2L * measurement_gap)
  }

  for (root in ordered_roots) {

    cx <- group_x[root]

    pos[root, ] <- c(cx, root_y)

    children <- members[[root]]
    k <- length(children)

    if (k == 0L) next

    # Center indicators around the root.
    # For even k, use an integer approximation.

    offsets <- (
      seq_len(k) - (k + 1) / 2
    ) * indicator_gap

    child_x <- as.integer(
      round(cx + offsets)
    )

    pos[children, 1] <- child_x
    pos[children, 2] <- indicator_y
  }

  # ---------------------------------------------------------
  # 9. Fallback for any unassigned nodes
  # ---------------------------------------------------------

  missing <- which(is.na(pos[, 1]))

  if (length(missing) > 0L) {
    start <- max(pos[, 1], na.rm = TRUE) +
      group_gap + 1L

    pos[missing, 1] <- start +
      seq_along(missing) - 1L

    pos[missing, 2] <- root_y
  }

  # ---------------------------------------------------------
  # 10. Normalize coordinates to start at zero
  # ---------------------------------------------------------

  pos[, 1] <- pos[, 1] - min(pos[, 1])
  pos[, 2] <- pos[, 2] - min(pos[, 2])

  storage.mode(pos) <- "integer"

  pos <- pos[original_nodes, , drop = FALSE]

  # Check the integer lattice invariant.
  keys <- paste(pos[, 1], pos[, 2], sep = ":")

  if (anyDuplicated(keys)) {
    stop("Internal error: duplicate layout coordinates.")
  }

  attr(pos, "grid_size") <- c(
    width = as.integer(max(pos[, 1]) + 1L),
    height = as.integer(max(pos[, 2]) + 1L)
  )
  out <- data.frame(
    x = pos[, 1],
    y = pos[, 2],
    name = original_nodes,
    row.names = NULL
  )
  rownames(out) <- NULL
  class(out) <- c("tidy_layout", class(out))
  out
}
