# For ordinal data: The real beauty of the 0/1 fixed threshold approach is that the rest of the model is entirely equivalent to the continuous case, containing variances, covariances and means. Thus, e.g., a latent growth curve model is straightforward to modify for use with ordinal data.

# For binary data, this is not possible. Make sure to fix the mean and variance for binary variables to 0 and 1

mx_thresholds <- function(df, variables = names(df),
                          output = "mx") {
  df <- df[, variables, drop = FALSE]
  if (!length(variables)) {
    stop("At least one variable must be supplied.")
  }
  isordered <- vapply(df, is.ordered, logical(1))
  if (any(!isordered)) {
    stop("All variables must be ordered categorical.")
  }
  numcats <- vapply(df, nlevels, integer(1))
  if (any(numcats < 2L)) {
    stop("Ordinal variables must have at least two levels.")
  }
  maxthres <- max(numcats) - 1L
  nvars <- length(variables)

  # Number of valid thresholds per variable
  nthres <- numcats - 1L

  # Identify genuine thresholds
  free <- outer(seq_len(maxthres), nthres, `<=`)

  dimnames(free) <- list(paste0("th_", seq_len(maxthres)), variables)

  # Starting values for thresholds
  values_thresh <- matrix(
    0,
    nrow = maxthres,
    ncol = nvars,
    dimnames = dimnames(free)
  )

  for (i in seq_along(variables)) {
    tab <- table(df[[i]])
    prop <- as.numeric(tab) / sum(tab)
    zscore <- stats::qnorm(
      cumsum(prop)[seq_len(nthres[i])]
    )
    if (any(!is.finite(zscore))) {
      zscore[!is.finite(zscore)] <- NA_real_
      zscore <- update_thresholds(zscore)
    }

    if (any(!is.finite(zscore))) {
      stop("Could not construct finite thresholds for ", variables[i])
    }

    values_thresh[seq_len(nthres[i]), i] <- zscore
  }

  # Threshold specification
  mat_thresh <- list(
    name = "Thresholds",
    type = "Full",
    nrow = maxthres,
    ncol = nvars,
    free = free,
    values = values_thresh,
    lbound = matrix(
      NA_real_, maxthres, nvars
    ),
    ubound = matrix(
      NA_real_, maxthres, nvars
    ),
    dimnames = dimnames(values_thresh)
  )

  # Convert to deviation parameterization
  values_dev <- values_thresh

  if (maxthres > 1L) {
    values_dev[-1L, ] <- values_thresh[-1L, , drop = FALSE] - values_thresh[-maxthres, , drop = FALSE]
  }
  # Set padding to zero
  values_dev[!free] <- 0

  # Mark padded cells usinglabels
  labels_dev <- matrix(
    NA_character_,
    nrow = maxthres,
    ncol = nvars,
    dimnames = dimnames(free)
  )
  labels_dev[!free] <- "XXXpaddingXXX"

  # Bounds on positive deviations
  lbound_dev <- matrix(
    NA_real_,
    nrow = maxthres,
    ncol = nvars
  )

  if (maxthres > 1L) {
    lbound_dev[-1L, ] <- 1e-3
  }

  # Padding is fixed, so bounds are unnecessary
  lbound_dev[!free] <- NA_real_

  mat_dev <- list(
    name = "mat_dev",
    type = "Full",
    nrow = maxthres,
    ncol = nvars,
    free = free,
    values = values_dev,
    labels = labels_dev,
    lbound = lbound_dev,
    ubound = matrix(
      NA_real_, maxthres, nvars
    ),
    dimnames = list(
      paste0("dev_", seq_len(maxthres)),
      variables
    )
  )

  # Lower-triangular accumulation matrix
  mat_ones <- list(
    name = "mat_ones",
    type = "Lower",
    nrow = maxthres,
    free = FALSE,
    values = 1
  )

  # Threshold algebra
  algebra <- list(
    name = "Thresholds",
    str2lang("mat_ones %*% mat_dev"),
    dimnames = mat_thresh$dimnames
  )

  out <- list(
    mat_dev = mat_dev,
    mat_ones = mat_ones,
    alg_thres = algebra
  )

  if (output == "list") {
    return(out)
  }

  if (output == "mx") {
    return(mx_threshold_matrices(out))
  }

  stop("'output' must be 'list' or 'mx'.")
}


mx_threshold_matrices <- function(x, ...){
  mats <- startsWith(names(x), "mat_")
  algs <- startsWith(names(x), "alg_")
  cnst <- startsWith(names(x), "const_")
  if(any(mats)){
    x[which(mats)] <- lapply(x[which(mats)], function(l){
      do.call(OpenMx::mxMatrix, l)
    })
  }
  if(any(algs)){
    x[which(algs)] <- lapply(x[which(algs)], function(l){
      do.call(OpenMx::mxAlgebra, l)
    })
  }
  if(any(cnst)){
    x[which(cnst)] <- lapply(x[which(cnst)], function(l){
      do.call(OpenMx::mxConstraint, l)
    })
  }
  return(x)
}


#' @title Create Thresholds Based on Deviances
#' @description This function creates a list of mxMatrices to specify thresholds
#' for ordinal variables as a function of strictly positive deviances, see
#' Details.
#' @param vars Character vector. Names of variables for which thresholds should
#' be specified.
#' @param nThresh Numeric vector. Number of thresholds for each of variable in
#' `vars`.
#' @param free Logical vector, used to fill the `free` matrix which indicates
#' whether the deviance will be freely estimated or fixed.
#' @param values Numeric vector of starting values. Default uses
#' `mxNormalQuantiles()`.
#' @param labels Character vector. Note that constraints specified using these
#' labels are applied using `mxConstraint()`, because the thresholds are derived
#' quantities, not parameters of the model (the deviances are).
#' @param lbound PARAM_DESCRIPTION, Default: NULL
#' @param ubound PARAM_DESCRIPTION, Default: NULL
#' @return A list of `mxMatrix` objects, to be included in an `mxModel`.
#' @details Specifying thresholds as a function of strictly positive deviances
#' is an efficient way to get models with ordinal indicators to converge, if
#' they tend to arrive at impossible solutions where the thresholds are not
#' monotonously increasing (e.g., in mixture models). The parameters of a model
#' specified this way are deviances (i.e., the differences between threshold
#' values), which have a lower bound of a small positive number to ensure that
#' thresholds are always increasing. The thresholds are computed by adding
#' subsequent deviances.
#' @rdname mx_deviances
#' @export
mx_deviances <- function(
    vars,
    nThresh = NA,
    free = NULL,
    values = NULL,
    labels = NA,
    lbound = NULL,
    ubound = NULL
) {

  if (!requireNamespace("OpenMx", quietly = TRUE)) {
    stop("Package 'OpenMx' is required.")
  }

  if (!is.character(vars) || !length(vars) || anyNA(vars) || anyDuplicated(vars)) {
    stop("'vars' must contain unique variable names.")
  }

  if (length(nThresh) == 1L) {
    nThresh <- rep(nThresh, length(vars))
  }

  if (length(nThresh) != length(vars) || anyNA(nThresh) || any(!is.finite(nThresh)) || any(nThresh < 0) || any(nThresh != as.integer(nThresh))) {
    stop("'nThresh' must contain one nonnegative integer per variable.")
  }

  nThresh <- as.integer(nThresh)
  maxthres <- max(nThresh)

  if (maxthres < 1L) {
    stop("At least one threshold is required.")
  }

  nvars <- length(vars)

  dims <- list(
    paste0("dev_", seq_len(maxthres)),
    vars
  )

  # Identify valid thresholds and padding

  active <- outer(seq_len(maxthres), nThresh, `<=`)

  padding <- !active

  # Helper: expand scalar/vector/matrix arguments
  expand_arg <- function(z, name, default) {

    if (is.null(z)) {
      return(matrix(default, maxthres, nvars))
    }

    if (is.matrix(z)) {
      if (!identical(dim(z), c(maxthres, nvars))) {
        stop("Incorrect dimensions for '", name, "'.")
      }
      return(z)
    }

    if (length(z) == 1L) {
      return(matrix(z, maxthres, nvars))
    }

    if (length(z) == sum(active)) {
      out <- matrix(default, maxthres, nvars)
      out[active] <- z
      return(out)
    }

    if (length(z) == maxthres * nvars) {
      return(matrix(z, maxthres, nvars))
    }

    stop("Incorrect length for '", name, "'.")
  }

  # Free parameter pattern

  free_mat <- expand_arg(free, "free", TRUE)

  if (!is.logical(free_mat) || anyNA(free_mat[active])) {
    stop("'free' must contain logical values.")
  }

  # Padding is always fixed
  free_mat[padding] <- FALSE

  # Threshold starting values
  if (is.null(values)) {

    thresh_mat <- matrix(
      0,
      maxthres,
      nvars
    )

    for (j in seq_len(nvars)) {

      if (nThresh[j] > 0L) {

        thresh_mat[
          seq_len(nThresh[j]), j
        ] <- OpenMx::mxNormalQuantiles(
          nBreaks = nThresh[j]
        )
      }
    }

  } else {

    thresh_mat <- expand_arg(
      values,
      "values",
      0
    )
  }

  if (!is.numeric(thresh_mat) ||
      any(!is.finite(thresh_mat[active]))) {
    stop("Threshold starting values must be finite.")
  }

  # Padding repeats the last valid threshold
  # in the algebra, so its deviation is zero.
  # For variables with no thresholds, all values are zero.
  for (j in seq_len(nvars)) {

    if (nThresh[j] < maxthres) {

      idx <- seq.int(nThresh[j] + 1L, maxthres)

      thresh_mat[idx, j] <- if (nThresh[j] > 0L) {
        thresh_mat[nThresh[j], j]
      } else {
        0
      }
    }
  }

  # Convert thresholds to deviations
  values_dev <- thresh_mat

  if (maxthres > 1L) {
    values_dev[-1L, ] <-
      thresh_mat[-1L, , drop = FALSE] -
      thresh_mat[-maxthres, , drop = FALSE]
  }

  values_dev[padding] <- 0

  # Labels supplied by the user refer to thresholds,
  # not necessarily to the underlying deviations.
  labels_mat <- expand_arg(
    labels,
    "labels",
    NA_character_
  )

  labels_mat <- matrix(
    as.character(labels_mat),
    nrow = maxthres,
    ncol = nvars
  )

  # Reserve XXXpaddingXXX for unused entries
  if (any(labels_mat[active] == "XXXpaddingXXX",
          na.rm = TRUE)) {
    stop("Label name 'XXXpaddingXXX' is exclusively reserved for unused thresholds.")
  }

  # Deviations are not directly equality-constrained by threshold labels.
  labels_dev <- matrix(
    NA_character_,
    maxthres,
    nvars
  )

  labels_dev[padding] <- "XXXpaddingXXX"

  # Bounds on deviations
  values_lbound <- matrix(
    NA_real_,
    maxthres,
    nvars
  )

  if (maxthres > 1L) {
    values_lbound[-1L, ] <- 1e-3
  }

  if (!is.null(lbound)) {
    values_lbound <- expand_arg(
      lbound,
      "lbound",
      NA_real_
    )
  }

  values_ubound <- expand_arg(
    ubound,
    "ubound",
    NA_real_
  )

  # Padding has no bounds
  values_lbound[padding] <- NA_real_
  values_ubound[padding] <- NA_real_

  # Check valid starting values against bounds
  bad_lower <- active & !is.na(values_lbound) &
    values_dev < values_lbound

  bad_upper <- active & !is.na(values_ubound) &
    values_dev > values_ubound

  if (any(bad_lower) || any(bad_upper)) {
    stop("Starting deviations violate parameter bounds.")
  }

  # Construct threshold matrices
  mat_dev <- list(
    name = "mat_dev",
    type = "Full",
    nrow = maxthres,
    ncol = nvars,
    free = free_mat,
    values = values_dev,
    labels = labels_dev,
    lbound = values_lbound,
    ubound = values_ubound,
    dimnames = dims
  )

  mat_ones <- list(
    name = "mat_ones",
    type = "Lower",
    nrow = maxthres,
    free = FALSE,
    values = 1
  )

  algebra <- list(
    name = "Thresholds",
    str2lang("mat_ones %*% mat_dev"),
    dimnames = list(
      paste0("th_", seq_len(maxthres)),
      vars
    )
  )

  # Equality constraints on actual thresholds
  cnsts <- list()

  valid_labels <- unique(
    labels_mat[active & !is.na(labels_mat)]
  )

  for (lab in valid_labels) {

    idx <- which(
      active & !is.na(labels_mat) & labels_mat == lab,
      arr.ind = TRUE
    )

    if (nrow(idx) < 2L) {
      next
    }

    # Use first occurrence as reference
    ref <- paste0(
      "Thresholds[",
      idx[1, 1], ",",
      idx[1, 2], "]"
    )

    for (i in seq.int(2L, nrow(idx))) {

      target <- paste0(
        "Thresholds[",
        idx[i, 1], ",",
        idx[i, 2], "]"
      )

      cname <- paste0(
        "const_threshold_",
        length(cnsts) + 1L
      )

      cnsts[[cname]] <- list(
        name = cname,
        str2lang(
          paste0(ref, " == ", target)
        )
      )
    }
  }

  # Return OpenMx objects
  out <- c(
    list(
      mat_dev = mat_dev,
      mat_ones = mat_ones,
      alg_thres = algebra
    ),
    cnsts
  )

  return(mx_threshold_matrices(out))
}


#' @title Data Quantiles
#' @description Get quantiles based on empirical normal distribution of data.
#' @param df A `data.frame` with only columns of class `ordered`.
#' @return A matrix with the appropriate dimensions for the threshold matrix for
#' `df`, see \code{\link[OpenMx:mxThreshold]{OpenMx::mxThreshold()}}.
#' @examples
#' set.seed(1)
#' df <- data.frame(X = ordered(sample(c(1:4), size = 100, replace = TRUE,
#' prob = c(.1, .2, .5, .2))))
#' mx_data_quantiles(df)
#' @export
mx_data_quantiles <- function(df){
  isordered <- sapply(df, inherits, what = "ordered")
  if(any(!isordered)) stop("All variables provided to mx_data_quantiles() must be ordered categorical.")
  numcats <- sapply(df, function(x){length(levels(x))})
  maxthres <- max(numcats)-1L
  vals <- matrix(1e-3, nrow = maxthres, ncol = ncol(df))
  for(v in seq_along(numcats)){
    vals[1:(numcats[v]-1L), v] <- qnorm(p = cumsum(prop.table(table(df[[v]]))[1:(numcats[v]-1L)]), lower.tail = TRUE)
  }
  colnames(vals) <- names(df)
  return(vals)
}



