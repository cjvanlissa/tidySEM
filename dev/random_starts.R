#' Generate random starting values for OpenMx model
#'
#' Generates plausible random starting values for free parameters in selected
#' matrices of an OpenMx RAM model. Starting values are scaled, where possible,
#' to the data used to construct the model.
#'
#' @param scale_A Numeric vector of length two giving the lower and upper
#'   multipliers used when drawing free elements of the \code{A} matrix.
#'   Defaults to \code{c(-0.5, 0.5)}. For an element \code{A[i, j]}, these
#'   multipliers are applied to \code{xsd[i] / xsd[j]}.
#'
#' @param scale_mat_dev Numeric vector of length two giving the lower and upper
#'   limits of the uniform distribution used for free elements of
#'   \code{mat_dev}. Defaults to \code{c(0.001, 1.5)}. The lower bound should
#'   generally be strictly positive when \code{mat_dev} represents positive
#'   threshold deviations.
#'
#' @param jitter_thresholds Non-negative numeric value controlling the magnitude
#'   of uniform random perturbation applied to empirical starting thresholds.
#'   Thresholds are perturbed by values drawn from
#'   \code{runif(n, -jitter_thresholds, jitter_thresholds)}. Defaults to
#'   \code{0.25}.
#'
#' @return If `run = TRUE`, returns the best-fitting OpenMx model. This model
#' has `attr(x, "tab_ll")`, a table with random seeds and minus 2 log likelihood
#' values. You can use these seeds to reproduce the models.
#' If `run = FALSE`, returns a list of OpenMX models.
#'
#' @details
#' Columns of the observed data must be numeric, integer, or ordered factors.
#' For numeric variables with zero or undefined observed range or standard
#' deviation, fallback scaling values are constructed to avoid division by
#' zero or non-finite random-start ranges.
#'
#' Only free matrices named \code{A}, \code{S}, \code{F}, \code{M},
#' \code{mat_dev}, and \code{Thresholds} are supported. The \code{F} matrix may
#' be present, although this function does not randomize its elements.
#'
#' The function uses R's global random-number generator. Use \code{set.seed()}
#' before calling this function when reproducible random starting values are
#' required.
#'
#' For models containing submodels, the function is applied recursively to each
#' submodel. If observed data are stored in \code{x@data$observed}, those data
#' take precedence over the value supplied to \code{data}.
#'
#' For continuous variables, the observed minimum, maximum, and standard
#' deviation are used to determine plausible starting-value scales. For ordinal
#' variables, values already present in the model matrices are used where
#' appropriate.
#'
#' Free paths in the \code{A} matrix are sampled uniformly after scaling by the
#' ratio of the standard deviations of the dependent and predictor variables.
#' The resulting interval for element \eqn{A_{ij}} is
#' \code{scale_A * (sd[i] / sd[j])}.
#'
#' Starting values for the \code{S} matrix are generated from a random
#' lower-triangular Cholesky factor \eqn{L}, and the covariance matrix is formed
#' as \eqn{S = L L^\top}. Diagonal elements of \eqn{L} are sampled uniformly
#' between 0.5 and 1 times the corresponding observed standard deviation, while
#' off-diagonal elements are sampled between -0.25 and 0.25 times that standard
#' deviation. This construction produces a positive-definite covariance matrix
#' before any restrictions imposed by the pattern of free parameters in
#' \code{S} are applied.
#'
#' Free means in the \code{M} matrix are sampled uniformly between the observed
#' minimum and maximum of the corresponding numeric variable.
#'
#' Free elements of \code{mat_dev} are sampled uniformly from
#' \code{scale_mat_dev}. These elements are intended for parameterizations in
#' which strictly positive deviations are accumulated to construct ordered
#' thresholds.
#'
#' For a \code{Thresholds} matrix, empirical latent-normal thresholds are based
#' on the cumulative observed category proportions of each ordered variable,
#' using \code{qnorm()}. Uniform random jitter is added to these values and the
#' resulting thresholds are sorted to preserve their ordering.
#'
#' After random values have been assigned,
#' \code{\link[OpenMx]{omxAssignFirstParameters}} is called so that parameters
#' sharing labels receive consistent starting values.
#' #' @examples
#' \dontrun{
#' set.seed(1)
#' df <- iris[1:4]
#' names(df) <- letters[1:4]
#' mod <- mx_profiles(data = df, classes = 2, run = FALSE)
#' res <- random_starts(mod, nstarts = 2)
#' # Get the table of seeds and minus 2 loglikelihoods:
#' tab_ll <- attr(res, "tab_ll")
#' # Seed for smallest -2LL:
#' seed <- tab_ll$seed[which.min(tab_ll$Minus2LogLikelihood)]
#' # Set random seed to that seed:
#' .Random.seed <- seed
#' # Rerun random starts just for that best seed:
#' res <- random_starts(mod, nstarts = 1)
#' }
random_starts <- function(x,
                              nstarts = 20,
                              scale_A = c(-.5, .5),
                              scale_mat_dev = c(.001, 1.5),
                              jitter_thresholds = .25,
                              tolerance_minus2ll = 1e-6,
                              verbose = TRUE,
                              run = TRUE
) {
  if(is.null(.Random.seed)){
    warning("First initialize the random number generator")
    invisible(rnorm(1))
  }
  seed_values <- .Random.seed
  if(nstarts > length(seed_values)) stop("Too many `nstarts` for the number of random seeds.")

  cl <- match.call()
  cl[[1]] <- quote(random_ram_starts_internal)
  cl[["nstarts"]] <- NULL
  out <- lapply(seq_len(nstarts), function(i){
    .Random.seed <- seed_values[i]
    eval(cl)
  })
  if(!run) return(out)
  out <- lapply(out, OpenMx::mxRun)
  tab_ll <- data.frame(seed = seed_values[1:nstarts], Minus2LogLikelihood = sapply(out, function(x){x$output$Minus2LogLikelihood}))
  if(sum(tab_ll$Minus2LogLikelihood - min(tab_ll$Minus2LogLikelihood) < tolerance_minus2ll) < 2){
    warning("The smallest Minus2LogLikelihood was not replicated.")
  }
  if(verbose) print(tab_ll)
  out <- out[[which.min(tab_ll$Minus2LogLikelihood)]]
  attr(out, "tab_ll") <- tab_ll
  return(out)
}

random_ram_starts_internal <- function(x,
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
      x[[s]] <- random_ram_starts_internal(x[[s]], data = data, scale_A = scale_A)
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
  xsd[!is.finite(xsd) | xsd == 0] <- xrng[!is.finite(xsd) | xrng == 0] / 4

  # Get matrices
  mats <- names(x@matrices)
  mats <- mats[sapply(mats, function(m){
    any(x[[m]]$free)
  })]
  if(length(mats) == 0){
    return(x)
  }
  if(any(!mats %in% c("A", "S", "F", "M", "mat_dev", "Thresholds"))){
    stop("Cannot generate random starts for ", paste0(mats[which(!mats %in% c("A", "S", "F", "M"))], collapse = ", "), ".")
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
  }


  if("M" %in% mats){
    these_ms <- names(xmin)[x$M$free]
    x$M$values[1, these_ms] <- sapply(these_ms, function(n){runif(n = 1, min = xmin[n], max = xmax[n])})
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

      Xord <- X_num <- X[, which(is_ord), drop = FALSE]

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
        prop <- tab / sum(tab)

        base <- qnorm(
          cumsum(prop)[-length(prop)]
        )

        vals <- base + runif(
          length(base),
          -jitter,
          jitter
        )

        Tmat[seq_along(vals), j] <- sort(vals)
      }

      Tmat
    }
  }
  x <- OpenMx::omxAssignFirstParameters(x)
  return(x)
}
