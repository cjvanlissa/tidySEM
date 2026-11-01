## Merge RAM models

# Block-diagonal matrix helper
block_diag <- function(a, b, pad) {

  nr <- nrow(a) + nrow(b)
  nc <- ncol(a) + ncol(b)

  out <- matrix(pad, nr, nc)

  out[
    seq_len(nrow(a)),
    seq_len(ncol(a))
  ] <- a

  out[
    nrow(a) + seq_len(nrow(b)),
    ncol(a) + seq_len(ncol(b))
  ] <- b

  out
}
# Helper: pad matrices to equal row count and cbind
pad_cbind <- function(a, b, pad = NA) {

  nr <- max(nrow(a), nrow(b))

  pad_rows <- function(m) {
    if (nrow(m) == nr) return(m)

    rbind(
      m,
      matrix(
        pad,
        nrow = nr - nrow(m),
        ncol = ncol(m),
        dimnames = list(
          NULL,
          colnames(m)
        )
      )
    )
  }

  cbind(pad_rows(a), pad_rows(b))
}

merge_square <- function(x, y, name, N, ramvars) {

  a <- x[[name]]
  b <- y[[name]]

  OpenMx::mxMatrix(
    type = "Full",
    name = name,
    nrow = N,
    ncol = N,

    values = block_diag(
      a$values, b$values, 0
    ),

    free = block_diag(
      a$free, b$free, FALSE
    ),

    labels = block_diag(
      a$labels, b$labels, NA_character_
    ),

    lbound = block_diag(
      a$lbound, b$lbound, NA_real_
    ),

    ubound = block_diag(
      a$ubound, b$ubound, NA_real_
    ),

    dimnames = list(ramvars, ramvars)
  )
}

has_ordinal <- function(mod) {
  ordinal_algs <- c("Thresholds")
  ordinal_mats <- c(
    "mat_dev",
    "mat_ones"
  )
  any(ordinal_mats %in% names(mod@matrices)) || any(ordinal_algs %in% names(mod@algebras))
}

# Identify threshold parameterization
threshold_type <- function(m) {

  if (!has_ordinal(m)) {
    return("none")
  }

  if ("Thresholds" %in% names(m@algebras)) {
    return("mx_deviances")
  }

  if ("Thresholds" %in% names(m@matrices)) {
    return("mxThreshold")
  }

  stop("Ordinal model has no recognized threshold specification.")
}


merge_ordinal_thresholds <- function(x, y) {
  # Check parametrization
  type_x <- threshold_type(x)
  type_y <- threshold_type(y)

  # Neither model contains ordinal variables
  if (type_x == "none" && type_y == "none") {
    return(list())
  }
  # Only one model contains ordinal variables
  if (type_x == "none" || type_y == "none") {
    m <- if (type_x != "none") x else y
    if (type_x == "mxThreshold" || type_y == "mxThreshold") {
      return(list(m[["Thresholds"]]))
    }
    return(list(
      m[["mat_dev"]],
      m[["mat_ones"]],
      m[["Thresholds"]]
    ))
  }

  # Both ordinal: parameterizations must agree
  if (type_x != type_y) {
    stop(
      "Cannot merge ordinal models with different threshold ",
      "parameterizations ('", type_x, "' and '", type_y, "')."
    )
  }

  # mxThreshold parameterization
  if (type_x == "mxThreshold") {

    a <- x$Thresholds
    b <- y$Thresholds

    vals <- pad_cbind(a$values, b$values, NA_real_)
    free <- pad_cbind(a$free, b$free, FALSE)
    labs <- pad_cbind(a$labels, b$labels, NA_character_)
    lb <- pad_cbind(a$lbound, b$lbound, NA_real_)
    ub <- pad_cbind(a$ubound, b$ubound, NA_real_)

    return(list(
      OpenMx::mxMatrix(
        type = "Full",
        name = "Thresholds",
        values = vals,
        free = free,
        labels = labs,
        lbound = lb,
        ubound = ub,
        dimnames = list(
          paste0("th_", seq_len(nrow(vals))),
          colnames(vals)
        )
      )
    ))
  }

  # mx_deviation parameterization
  a <- x$mat_dev
  b <- y$mat_dev

  vals <- pad_cbind(a$values, b$values, 0)
  free <- pad_cbind(a$free, b$free, FALSE)
  labs <- pad_cbind(a$labels, b$labels, NA_character_)
  lb <- pad_cbind(a$lbound, b$lbound, NA_real_)
  ub <- pad_cbind(a$ubound, b$ubound, NA_real_)

  nr <- nrow(vals)

  L <- matrix(0, nr, nr)
  L[lower.tri(L, diag = TRUE)] <- 1

  list(
    OpenMx::mxMatrix(
      type = "Full",
      name = "mat_dev",
      values = vals,
      free = free,
      labels = labs,
      lbound = lb,
      ubound = ub,
      dimnames = list(
        paste0("dev_", seq_len(nr)),
        colnames(vals)
      )
    ),

    OpenMx::mxMatrix(
      type = "Lower",
      name = "mat_ones",
      free = FALSE,
      values = L
    ),

    OpenMx::mxAlgebra(
      expression = str2lang("mat_ones %*% mat_dev"),
      name = "Thresholds")
  )
}


mx_merge_matrices <- function(x, y) {
  if (!methods::is(x, "MxRAMModel") || !methods::is(y, "MxRAMModel")) {
    stop("Both models must be OpenMx RAM models.")
  }

  if (length(x@submodels) || length(y@submodels)) {
    stop("Models containing submodels cannot be merged.")
  }

  required <- c("A", "S", "F", "M")

  if (!all(required %in% names(x@matrices)) || !all(required %in% names(y@matrices))) {
    stop("Both models must contain A, S, F, and M matrices.")
  }

  # Variable names

  manifest <- c(x@manifestVars, y@manifestVars)
  latent <- c(x@latentVars, y@latentVars)
  # use the ordering of the original A matrices:
  vars_x <- colnames(x$A$values)
  vars_y <- colnames(y$A$values)
  ramvars <- c(vars_x, vars_y)
  if (anyDuplicated(ramvars)) {
    stop("The models contain overlapping variable names.")
  }

  nx <- length(vars_x)
  ny <- length(vars_y)
  N <- nx + ny


  # Merge A and S

  A <- merge_square(x, y, name = "A", N, ramvars)
  S <- merge_square(x, y, name = "S", N, ramvars)

  # Merge F

  F <- OpenMx::mxMatrix(
    type = "Full",
    name = "F",

    nrow = length(manifest),
    ncol = N,

    values = block_diag(
      x$F$values,
      y$F$values,
      0
    ),

    free = block_diag(
      x$F$free,
      y$F$free,
      FALSE
    ),

    labels = block_diag(
      x$F$labels,
      y$F$labels,
      NA_character_
    ),

    dimnames = list(
      manifest,
      ramvars
    )
  )

  # Merge M
  M <- OpenMx::mxMatrix(
    type = "Full",
    name = "M",
    nrow = 1,
    ncol = N,

    values = c(
      x$M$values,
      y$M$values
    ),

    free = c(
      x$M$free,
      y$M$free
    ),

    labels = c(
      x$M$labels,
      y$M$labels
    ),

    lbound = c(
      x$M$lbound,
      y$M$lbound
    ),

    ubound = c(
      x$M$ubound,
      y$M$ubound
    ),

    dimnames = list(NULL, ramvars)
  )

  ordinal_objects <- merge_ordinal_thresholds(x, y)

  # Construct combined model
  objects <- c(list(A, S, F, M), ordinal_objects)

  # Configure expectation
  expectation <- if (length(ordinal_objects) > 0L) {
    OpenMx::mxExpectationRAM(
      A = "A",
      S = "S",
      F = "F",
      M = "M",
      thresholds = "Thresholds"
    )
  } else {
    OpenMx::mxExpectationRAM(
      A = "A",
      S = "S",
      F = "F",
      M = "M"
    )
  }

  # Construct combined model
  model <- do.call(
    OpenMx::mxModel,
    c(
      list(
        model = "combined",
        type = "RAM",
        manifestVars = manifest,
        latentVars = latent
      ),
      list(A, S, F, M),
      ordinal_objects,
      list(expectation)
    )
  )

  return(model)
}


