fr_layout <- function(nodes, edges, niter = 500, width = 1, height = 1,
                      seed = NULL) {
  # Validate input
  nodes <- as.character(nodes)
  if (anyDuplicated(nodes)) {
    stop("`nodes` must contain unique node names.")
  }

  edges <- as.matrix(edges)
  if (ncol(edges) != 2) {
    stop("`edges` must be a two-column matrix: from, to.")
  }

  edges <- matrix(as.character(edges), ncol = 2)

  if (length(nodes) == 0) {
    return(matrix(numeric(0), ncol = 2,
                  dimnames = list(NULL, c("x", "y"))))
  }

  if (length(edges) > 0 &&
      any(!edges[, 1] %in% nodes | !edges[, 2] %in% nodes)) {
    stop("All nodes appearing in `edges` must occur in `nodes`.")
  }

  if (!is.null(seed)) {
    set.seed(seed)
  }

  n <- length(nodes)

  # Initial random positions
  pos <- cbind(
    x = runif(n, -width / 2,  width / 2),
    y = runif(n, -height / 2, height / 2)
  )
  rownames(pos) <- nodes

  if (n == 1) {
    pos[1, ] <- c(0, 0)
    return(pos)
  }

  # Convert node names in edge list to integer indices
  if (nrow(edges) > 0) {
    edge_idx <- cbind(
      match(edges[, 1], nodes),
      match(edges[, 2], nodes)
    )

    # Self-loops do not affect an FR layout
    edge_idx <- edge_idx[edge_idx[, 1] != edge_idx[, 2], ,
                         drop = FALSE]
  } else {
    edge_idx <- matrix(integer(0), ncol = 2)
  }

  # Fruchterman-Reingold constants
  area <- width * height
  k <- sqrt(area / n)

  # Starting "temperature": limits how far a node may move
  temperature <- min(width, height) / 10

  eps <- .Machine$double.eps^0.25

  for (iter in seq_len(niter)) {
    disp <- matrix(0, nrow = n, ncol = 2)

    # ---------------------------------------------------------------
    # Repulsive forces between every pair of vertices
    #
    #     f_r(d) = k^2 / d
    # ---------------------------------------------------------------
    if (n >= 2) {
      for (v in seq_len(n - 1)) {
        for (u in (v + 1):n) {
          delta <- pos[v, ] - pos[u, ]
          dist <- sqrt(sum(delta^2))

          # Prevent division by zero if two vertices coincide
          if (dist < eps) {
            delta <- runif(2, -eps, eps)
            dist <- sqrt(sum(delta^2))
          }

          force <- k^2 / dist
          direction <- delta / dist
          force_vec <- direction * force

          disp[v, ] <- disp[v, ] + force_vec
          disp[u, ] <- disp[u, ] - force_vec
        }
      }
    }

    # ---------------------------------------------------------------
    # Attractive forces along edges
    #
    #     f_a(d) = d^2 / k
    # ---------------------------------------------------------------
    if (nrow(edge_idx) > 0) {
      for (e in seq_len(nrow(edge_idx))) {
        v <- edge_idx[e, 1]
        u <- edge_idx[e, 2]

        delta <- pos[v, ] - pos[u, ]
        dist <- sqrt(sum(delta^2))

        if (dist > eps) {
          force <- dist^2 / k
          direction <- delta / dist
          force_vec <- direction * force

          disp[v, ] <- disp[v, ] - force_vec
          disp[u, ] <- disp[u, ] + force_vec
        }
      }
    }

    # ---------------------------------------------------------------
    # Move vertices, limiting each displacement by temperature
    # ---------------------------------------------------------------
    for (v in seq_len(n)) {
      disp_len <- sqrt(sum(disp[v, ]^2))

      if (disp_len > 0) {
        step <- disp[v, ] / disp_len * min(disp_len, temperature)
        pos[v, ] <- pos[v, ] + step
      }
    }

    # Linear cooling schedule
    temperature <- min(width, height) / 10 * (1 - iter / niter)
  }

  # Center the finished layout at the origin
  pos[, 1] <- pos[, 1] - mean(pos[, 1])
  pos[, 2] <- pos[, 2] - mean(pos[, 2])

  colnames(pos) <- c("x", "y")
  rownames(pos) <- nodes

  pos
}


fr_layout_integer <- function(nodes, edges, niter = 500,
                              grid_width = 20, grid_height = 20,
                              seed = NULL) {
  nodes <- as.character(nodes)

  if (anyDuplicated(nodes)) {
    stop("`nodes` must contain unique node names.")
  }

  edges <- as.matrix(edges)

  if (ncol(edges) != 2) {
    stop("`edges` must be a two-column matrix: from, to.")
  }

  edges <- matrix(as.character(edges), ncol = 2)

  if (length(nodes) == 0) {
    return(matrix(
      integer(0),
      ncol = 2,
      dimnames = list(NULL, c("x", "y"))
    ))
  }

  if (length(edges) > 0 &&
      any(!edges[, 1] %in% nodes | !edges[, 2] %in% nodes)) {
    stop("All nodes appearing in `edges` must occur in `nodes`.")
  }

  n <- length(nodes)

  # Number of available lattice points
  n_cells <- (2 * grid_width + 1) * (2 * grid_height + 1)

  if (n > n_cells) {
    stop("Grid is too small to assign a unique position to every node.")
  }

  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Convert edges to integer node indices
  if (nrow(edges) > 0) {
    edge_idx <- cbind(
      match(edges[, 1], nodes),
      match(edges[, 2], nodes)
    )

    # Ignore self-loops
    edge_idx <- edge_idx[
      edge_idx[, 1] != edge_idx[, 2],
      ,
      drop = FALSE
    ]
  } else {
    edge_idx <- matrix(integer(0), ncol = 2)
  }

  # ------------------------------------------------------------
  # Generate unique random integer starting locations
  # ------------------------------------------------------------

  grid <- expand.grid(
    x = -grid_width:grid_width,
    y = -grid_height:grid_height
  )

  start <- sample(seq_len(nrow(grid)), n)

  pos <- as.matrix(grid[start, ])
  storage.mode(pos) <- "double"

  rownames(pos) <- nodes
  colnames(pos) <- c("x", "y")

  if (n == 1) {
    pos[1, ] <- c(0, 0)
    storage.mode(pos) <- "integer"
    return(pos)
  }

  # Character key for detecting occupied grid points
  point_key <- function(xy) {
    paste(xy[1], xy[2], sep = ",")
  }

  # ------------------------------------------------------------
  # Find closest unoccupied integer coordinate
  # ------------------------------------------------------------

  nearest_free_point <- function(target, occupied,
                                 max_x, max_y) {
    target <- round(target)

    target[1] <- max(-max_x, min(max_x, target[1]))
    target[2] <- max(-max_y, min(max_y, target[2]))

    if (!(point_key(target) %in% occupied)) {
      return(target)
    }

    max_radius <- max(2 * max_x + 1, 2 * max_y + 1)

    for (r in seq_len(max_radius)) {
      candidates <- expand.grid(
        x = (target[1] - r):(target[1] + r),
        y = (target[2] - r):(target[2] + r)
      )

      # Keep only the perimeter of this search square
      candidates <- candidates[
        abs(candidates$x - target[1]) == r |
          abs(candidates$y - target[2]) == r,
        ,
        drop = FALSE
      ]

      # Restrict to the allowed grid
      candidates <- candidates[
        candidates$x >= -max_x &
          candidates$x <=  max_x &
          candidates$y >= -max_y &
          candidates$y <=  max_y,
        ,
        drop = FALSE
      ]

      if (nrow(candidates) == 0) {
        next
      }

      keys <- paste(candidates$x, candidates$y, sep = ",")
      candidates <- candidates[!(keys %in% occupied), , drop = FALSE]

      if (nrow(candidates) > 0) {
        # Choose the candidate closest to the continuous target
        d2 <- (candidates$x - target[1])^2 +
          (candidates$y - target[2])^2

        best <- which.min(d2)

        return(c(
          candidates$x[best],
          candidates$y[best]
        ))
      }
    }

    stop("Could not find an unoccupied lattice point.")
  }

  # FR natural edge length
  area <- (2 * grid_width + 1) *
    (2 * grid_height + 1)

  k <- sqrt(area / n)

  # Temperature is measured in grid cells
  temperature <- max(1, min(grid_width, grid_height) / 2)

  eps <- 1e-9

  for (iter in seq_len(niter)) {

    disp <- matrix(0, nrow = n, ncol = 2)

    # ----------------------------------------------------------
    # Repulsive forces
    #
    #       f_r(d) = k^2 / d
    # ----------------------------------------------------------

    for (v in seq_len(n - 1)) {
      for (u in (v + 1):n) {

        delta <- pos[v, ] - pos[u, ]
        dist <- sqrt(sum(delta^2))

        if (dist < eps) {
          # Should normally be impossible because positions
          # are unique, but retain this for numerical safety.
          delta <- runif(2, -1, 1)
          dist <- sqrt(sum(delta^2))
        }

        force <- k^2 / dist
        direction <- delta / dist
        f <- direction * force

        disp[v, ] <- disp[v, ] + f
        disp[u, ] <- disp[u, ] - f
      }
    }

    # ----------------------------------------------------------
    # Attractive edge forces
    #
    #       f_a(d) = d^2 / k
    # ----------------------------------------------------------

    if (nrow(edge_idx) > 0) {
      for (e in seq_len(nrow(edge_idx))) {

        v <- edge_idx[e, 1]
        u <- edge_idx[e, 2]

        delta <- pos[v, ] - pos[u, ]
        dist <- sqrt(sum(delta^2))

        if (dist > eps) {
          force <- dist^2 / k
          direction <- delta / dist
          f <- direction * force

          disp[v, ] <- disp[v, ] - f
          disp[u, ] <- disp[u, ] + f
        }
      }
    }

    # ----------------------------------------------------------
    # Compute continuous proposed positions
    # ----------------------------------------------------------

    proposed <- pos

    for (v in seq_len(n)) {

      d <- sqrt(sum(disp[v, ]^2))

      if (d > eps) {
        step <- disp[v, ] / d * min(d, temperature)

        proposed[v, ] <- pos[v, ] + step
      }
    }

    # ----------------------------------------------------------
    # Project onto integer grid
    #
    # Process vertices in random order so lower-indexed vertices
    # do not always win collisions.
    # ----------------------------------------------------------

    new_pos <- matrix(NA_real_, nrow = n, ncol = 2)

    occupied <- character(0)

    order_v <- sample(seq_len(n))

    for (v in order_v) {

      p <- nearest_free_point(
        proposed[v, ],
        occupied,
        grid_width,
        grid_height
      )

      new_pos[v, ] <- p
      occupied <- c(occupied, point_key(p))
    }

    pos <- new_pos

    # Linear cooling, but do not drop below one grid cell
    temperature <- max(
      1,
      min(grid_width, grid_height) / 2 *
        (1 - iter / niter)
    )
  }

  storage.mode(pos) <- "integer"

  rownames(pos) <- nodes
  colnames(pos) <- c("x", "y")

  pos
}

fr_layout_integer_compact <- function(nodes, edges, niter = 1000,
                                      seed = NULL) {
  nodes <- as.character(nodes)

  if (anyDuplicated(nodes)) {
    stop("`nodes` must contain unique node names.")
  }

  edges <- as.matrix(edges)

  if (ncol(edges) != 2) {
    stop("`edges` must be a two-column matrix: from, to.")
  }

  edges <- matrix(as.character(edges), ncol = 2)

  n <- length(nodes)

  if (n == 0) {
    return(matrix(
      integer(0),
      ncol = 2,
      dimnames = list(NULL, c("x", "y"))
    ))
  }

  if (length(edges) > 0 &&
      any(!edges[, 1] %in% nodes | !edges[, 2] %in% nodes)) {
    stop("All nodes appearing in `edges` must occur in `nodes`.")
  }

  if (!is.null(seed)) {
    set.seed(seed)
  }

  # ------------------------------------------------------------
  # Find the smallest approximately-square integer rectangle
  # containing at least n lattice points.
  #
  # Coordinates will be:
  #
  #       x = 0, ..., nx - 1
  #       y = 0, ..., ny - 1
  #
  # We first minimize area nx * ny, then choose the shape
  # closest to square.
  # ------------------------------------------------------------

  possible_nx <- seq_len(n)

  possible_ny <- ceiling(n / possible_nx)
  areas <- possible_nx * possible_ny

  min_area <- min(areas)

  candidates <- which(areas == min_area)

  # Among minimum-area rectangles, prefer the most square one
  best <- candidates[
    which.min(abs(possible_nx[candidates] -
                    possible_ny[candidates]))
  ]

  nx <- possible_nx[best]
  ny <- possible_ny[best]

  # Put the longer dimension on x
  if (ny > nx) {
    tmp <- nx
    nx <- ny
    ny <- tmp
  }

  # ------------------------------------------------------------
  # All possible lattice positions
  # ------------------------------------------------------------

  grid <- as.matrix(expand.grid(
    x = 0:(nx - 1),
    y = 0:(ny - 1)
  ))

  storage.mode(grid) <- "integer"

  # ------------------------------------------------------------
  # Edge indices
  # ------------------------------------------------------------

  if (nrow(edges) > 0) {
    edge_idx <- cbind(
      match(edges[, 1], nodes),
      match(edges[, 2], nodes)
    )

    # Self-loops do not affect the layout
    edge_idx <- edge_idx[
      edge_idx[, 1] != edge_idx[, 2],
      ,
      drop = FALSE
    ]
  } else {
    edge_idx <- matrix(integer(0), ncol = 2)
  }

  # ------------------------------------------------------------
  # Initial assignment: random distinct grid cells
  # ------------------------------------------------------------

  pos <- grid[sample(seq_len(nrow(grid)), n), , drop = FALSE]

  rownames(pos) <- nodes
  colnames(pos) <- c("x", "y")

  if (n == 1) {
    pos[1, ] <- c(0L, 0L)
    return(pos)
  }

  # ------------------------------------------------------------
  # FR energy
  #
  # Instead of allowing continuous movement, we optimize the
  # assignment of nodes to lattice cells.
  #
  # Lower energy is better.
  # ------------------------------------------------------------

  k <- sqrt((nx * ny) / n)

  layout_energy <- function(pos) {
    energy <- 0

    # Repulsive potential.
    #
    # A potential whose derivative corresponds approximately
    # to the standard FR repulsive force k^2 / d.
    for (v in seq_len(n - 1)) {
      for (u in (v + 1):n) {
        delta <- pos[v, ] - pos[u, ]
        d <- sqrt(sum(delta^2))

        # Positions are distinct, so d cannot be zero.
        energy <- energy - k^2 * log(d)
      }
    }

    # Attractive potential.
    #
    # Derivative of d^3 / (3k) is d^2 / k,
    # the standard FR attractive force.
    if (nrow(edge_idx) > 0) {
      for (e in seq_len(nrow(edge_idx))) {
        v <- edge_idx[e, 1]
        u <- edge_idx[e, 2]

        delta <- pos[v, ] - pos[u, ]
        d <- sqrt(sum(delta^2))

        energy <- energy + d^3 / (3 * k)
      }
    }

    energy
  }

  current_energy <- layout_energy(pos)

  # ------------------------------------------------------------
  # Simulated annealing over discrete grid assignments
  #
  # Two possible moves:
  #
  #   1. Move a node into an unused grid cell.
  #   2. Swap the positions of two nodes.
  #
  # This lets the nodes explore the complete fixed-size lattice
  # without ever leaving the minimum grid.
  # ------------------------------------------------------------

  temperature <- max(1, sqrt(nx^2 + ny^2))

  for (iter in seq_len(niter)) {

    proposal <- pos

    # Which grid cells are currently occupied?
    occupied_key <- paste(pos[, 1], pos[, 2], sep = ",")
    grid_key <- paste(grid[, 1], grid[, 2], sep = ",")

    free <- which(!(grid_key %in% occupied_key))

    # If the minimum rectangle contains spare cells, sometimes
    # move a node into a free cell. Otherwise swap two nodes.
    if (length(free) > 0 && runif(1) < 0.5) {

      v <- sample.int(n, 1)
      g <- sample(free, 1)

      proposal[v, ] <- grid[g, ]

    } else {

      pair <- sample.int(n, 2)

      tmp <- proposal[pair[1], ]
      proposal[pair[1], ] <- proposal[pair[2], ]
      proposal[pair[2], ] <- tmp
    }

    proposal_energy <- layout_energy(proposal)

    delta <- proposal_energy - current_energy

    # Accept improvements, plus occasional worse moves early on
    # to avoid becoming trapped in a poor local optimum.
    if (delta <= 0 ||
        runif(1) < exp(-delta / max(temperature, 1e-12))) {

      pos <- proposal
      current_energy <- proposal_energy
    }

    # Exponential cooling
    temperature <- temperature * 0.995
  }

  storage.mode(pos) <- "integer"

  rownames(pos) <- nodes
  colnames(pos) <- c("x", "y")

  attr(pos, "grid_size") <- c(
    width = nx,
    height = ny
  )

  pos
}
