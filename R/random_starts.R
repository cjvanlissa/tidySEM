#' Generate Random Starting Values for OpenMx Models
#'
#' Generates plausible random starting values for OpenMx models.
#' Starting values are scaled, where possible, to the observed data.
#' Multiple sets of starting values can be evaluated to identify
#' the solution with the lowest minus twice log-likelihood.
#'
#' @param x An OpenMx `MxModel` object.
#' @param nstarts Integer. Number of sets of starting values to generate.
#' @param tolerance_minus2ll Numeric. Maximum difference from the smallest minus
#' twice log-likelihood for solutions to be considered equivalent.
#' @param verbose Logical. Whether to print information about starting values
#' and model fit.
#' @param run Logical. If `TRUE`, estimate the models and return the
#' best-fitting solution. If `FALSE`, return a list of models with randomized
#' starting values.
#' @param ... Additional arguments passed to the model-specific implementation.
#' @return If `run = TRUE`, the best-fitting OpenMx model with an attribute
#' `tab_ll` containing information about the starting values and their fit
#' statistics. If `run = FALSE`, a list of OpenMx models.
#' @details Random starting values are generated using R's random-number
#' generator. Use `set.seed()` for reproducibility.
#'
#' @examples
#' \dontrun{
#' set.seed(123)
#' model <- mx_profiles(data = data.frame(x = iris$Sepal.Length),
#' classes = 2, run = FALSE)
#' fit <- random_starts(model, nstarts = 3)
#' attr(fit, "tab_ll")
#' }
#'
#' @export
random_starts <- function(x,
                          nstarts = 20,
                          tolerance_minus2ll = 1e-6,
                          verbose = TRUE,
                          run = TRUE,
                          ...){

  if(is.null(.Random.seed)){
    warning("First initialize the random number generator")
    invisible(stats::rnorm(1))
  }
  out <- seed_values <- vector("list", length = nstarts)
  for(i in seq_len(nstarts)){
    seed_values[[i]] <- .Random.seed
    out[[i]] <- random_starts_internal(x, ...)
  }
  if(!run) return(out)
  out <- lapply(out, OpenMx::mxRun, intervals = FALSE, silent = TRUE)
  tab_ll <- data.frame(seed = seq_along(seed_values), Minus2LogLikelihood = sapply(out, function(x){x$output$Minus2LogLikelihood}))
  if(sum(tab_ll$Minus2LogLikelihood - min(tab_ll$Minus2LogLikelihood) < tolerance_minus2ll) < 2){
    warning("The smallest Minus2LogLikelihood was not replicated.")
  }
  if(verbose) print(tab_ll)
  out <- out[[which.min(tab_ll$Minus2LogLikelihood)]]
  attr(out, "tab_ll") <- tab_ll
  attr(out, "seeds") <- seed_values
  return(out)
}

random_starts_internal <- function(x, ...) {
  dispatch_method(x, "random_starts_internal", ...)
}




#' @rdname random_starts
#' @section Additional arguments for RAM mixture models:
#'
#' When `attr(x, "tidySEM") == "mixture"`, the following
#' arguments can be passed through `...`:
#'
#' * `data`: Optional observed data.
#' * `scale_A`: Lower and upper multipliers for free
#'   elements of the A matrix. Default: `c(-0.5, 0.5)`.
#' * `scale_mat_dev`: Sampling range for threshold
#'   deviations. Default: `c(0.001, 1.5)`.
#' * `jitter_thresholds`: Magnitude of random jitter
#'   applied to thresholds. Default: `0.25`.
#'
#' @usage NULL
#'
#' @details
#' **RAM mixture models**
#'
#' Starting values are derived from the observed data and the
#' model's RAM matrices. The function supports free parameters
#' in matrices named `A`, `S`, `M`, `mat_dev`, and `Thresholds`.
#' The `F` matrix may be present but is not randomized.
#'
#' For continuous variables, observed ranges and standard
#' deviations are used to determine plausible starting values.
#' Fallback scales are used when the observed range or standard
#' deviation is zero or undefined.
#'
#' Free paths in `A` are sampled uniformly after scaling by
#' the ratio of the standard deviations of the dependent and
#' predictor variables:
#' \deqn{A_{ij} \sim U(a,b)\frac{sd_i}{sd_j}}
#' where `c(a, b) = scale_A`.
#'
#' Starting values for `S` are generated using a random
#' lower-triangular Cholesky factor \eqn{L}, giving
#' \deqn{S = LL^\top.}
#' Diagonal elements of \eqn{L} are sampled between 0.5 and
#' 1 times the corresponding observed standard deviation.
#' Off-diagonal elements are sampled between -0.25 and 0.25
#' times that standard deviation.
#'
#' Free means in `M` are sampled uniformly between the
#' observed minimum and maximum of the corresponding variable.
#'
#' Free elements of `mat_dev` are sampled uniformly from
#' `scale_mat_dev`. For ordinal variables, empirical
#' latent-normal thresholds are obtained from cumulative
#' category proportions using `qnorm()`, perturbed by
#' uniform jitter, and sorted to preserve their ordering.
#'
#' For models containing submodels, initialization is applied
#' recursively. Observed data stored in the model take
#' precedence over externally supplied data.
#'
#' After random values have been assigned,
#' `OpenMx::omxAssignFirstParameters()` ensures that
#' parameters sharing labels have consistent starting values.
#'
#' @importFrom stats rnorm
random_starts_internal_mixture <- function(
    x,
    data = NULL,
    scale_A = c(-.5, .5),
    scale_mat_dev = c(.001, 1.5),
    jitter_thresholds = .25
) {

  if(!is.null(x@data$observed)){
    data <- x@data$observed
  }
  submods <- names(x@submodels)
  if(!is.null(submods)){
    for(s in submods){
      x[[s]] <- random_starts_internal_mixture(
        x[[s]],
        data = data,
        scale_A = scale_A,
        scale_mat_dev = scale_mat_dev,
        jitter_thresholds = jitter_thresholds
      )
    }
    x <- OpenMx::omxAssignFirstParameters(x)
    return(x)
  }
  if(is.null(data)) stop("No data available to construct random starts.")
  X <- as.data.frame(data)
  is_num <- sapply(data, inherits, what = c("numeric", "integer"))
  is_ord <- sapply(data, inherits, what = "ordered")
  if (!all(is_num | is_ord))
    stop("All columns of X must be numeric or ordinal.")

  p <- ncol(X)
  # Set default values
  xmax <- xmin <- x$M$values[1, ]
  xsd <- sqrt(diag(x$S$values))
  # Plug in real values for numeric variables
  if(any(is_num)){
    these_num <- which(is_num)
    X_num <- X[, these_num, drop = FALSE]
    xmin[these_num] <- vapply(X_num, min, numeric(1), na.rm = TRUE)
    xmax[these_num] <- vapply(X_num, max, numeric(1), na.rm = TRUE)
    xsd[these_num]  <- vapply(X_num, sd, numeric(1), na.rm = TRUE)
  }
  xrng <- xmax - xmin
  xrng[!is.finite(xrng) | xrng == 0] <- 1
  bad_sd <- !is.finite(xsd) | xsd <= 0
  xsd[bad_sd] <- xrng[bad_sd] / 4

  # Get matrices
  mats <- names(x@matrices)
  mats <- mats[sapply(mats, function(m){
    any(x[[m]]$free)
  })]
  if(length(mats) == 0){
    return(x)
  }
  supported <- c("A", "S", "F", "M", "mat_dev", "Thresholds")
  unsupported <- setdiff(mats, supported)
  if (length(unsupported)) {
    stop("Cannot generate random starts for matrix ", paste(unsupported, collapse = ", "), ".")
  }

  ## A matrix
  if("A" %in% mats){
    idx <- which(x$A$free, arr.ind = TRUE)

    if (nrow(idx)) {
      for (r in seq_len(nrow(idx))) {
        i <- idx[r, 1]
        j <- idx[r, 2]

        scale <- xsd[i] / xsd[j]

        x$A$values[i, j] <- runif(
          1,
          min = scale_A[1] * scale,
          max = scale_A[2] * scale
        )
      }
    }
  }

  if("S" %in% mats){
    ## S matrix via random Cholesky factor
    L <- matrix(0, p, p)

    for (i in seq_len(p)) {

      # Positive diagonal, scaled to observed SD
      L[i, i] <- runif(
        1,
        min = 0.5 * xsd[i],
        max = 1.0 * xsd[i]
      )

      if (i > 1L) {
        for (j in seq_len(i - 1L)) {

          # Modest cross-variable dependence
          L[i, j] <- runif(
            1,
            min = -0.25 * xsd[i],
            max =  0.25 * xsd[i]
          )
        }
      }
    }

    x$S$values[x$S$free] <- (L %*% t(L))[x$S$free]
    S <- x$S$values
    if (!all(is.finite(S)) ||
        !isTRUE(all.equal(S, t(S))) ||
        min(eigen(S, symmetric = TRUE,
                  only.values = TRUE)$values) <= 0) {
      warning("Generated S matrix is not positive definite.")
    }
  }


  if("M" %in% mats){
    idx <- which(x$M$free[1, ])

    if (length(idx)) {
      x$M$values[1, idx] <- stats::runif(
        length(idx),
        min = xmin[idx],
        max = xmax[idx]
      )
    }
  }

  if("mat_dev" %in% mats){
    x$mat_dev$values[x$mat_dev$free] <- runif(
      sum(x$mat_dev$free),
      min = scale_mat_dev[1],
      max = scale_mat_dev[2]
    )
  }

  if("Thresholds" %in% mats){
    random_threshold_matrix <- function(X, jitter = 0.25) {

      Xord <- X[, which(is_ord), drop = FALSE]

      nthr <- vapply(
        Xord,
        function(x) nlevels(x) - 1L,
        integer(1)
      )

      Tmat <- matrix(
        NA_real_,
        nrow = max(nthr),
        ncol = length(Xord),
        dimnames = list(
          paste0("thr", seq_len(max(nthr))),
          names(Xord)
        )
      )

      for (j in seq_along(Xord)) {

        x <- Xord[[j]]

        tab <- table(x)
        prop <- as.numeric(tab) / sum(tab)
        cumprop <- cumsum(prop)
        cumprop <- cumprop[-length(cumprop)]
        eps <- 1e-6
        cumprop <- pmin(pmax(cumprop, eps), 1 - eps)
        base <- stats::qnorm(cumprop)

        vals <- base + runif(
          length(base),
          -jitter,
          jitter
        )

        Tmat[seq_along(vals), j] <- sort(vals)
      }

      Tmat
    }
    Tmat <- random_threshold_matrix(X, jitter = jitter_thresholds)
    idx <- x$Thresholds$free
    x$Thresholds$values[idx] <- Tmat[idx]
  }
  x <- OpenMx::omxAssignFirstParameters(x)
  return(x)
}
