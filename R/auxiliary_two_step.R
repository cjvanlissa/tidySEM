#' Estimate an Auxiliary Model Using Two-Step Maximum Likelihood
#'
#' Estimates an auxiliary model based on a previously fitted latent class
#' model, using a two-step maximum likelihood procedure.
#'
#' In the first step, the latent class model is estimated using the
#' measurement indicators. In the second step, its parameter estimates
#' (including the mixing proportions) are fixed, and the auxiliary model
#' is added to each latent class. Only the parameters of the auxiliary
#' model are freely estimated.
#'
#' Unlike the one-step approach, the auxiliary variables cannot influence
#' the measurement model parameters. However, auxiliary variables can
#' influence posterior class membership probabilities because they
#' contribute to the joint likelihood. Classification uncertainty is
#' accounted for by integrating over latent class membership rather
#' than assigning observations to their most likely class.
#'
#' Note that conventional standard errors obtained from this procedure
#' treat the first-step parameter estimates as known and therefore do
#' not account for uncertainty in the estimation of the latent class
#' model. Corrected standard errors can be obtained using an appropriate
#' two-step sandwich estimator or a bootstrap that repeats both steps.
#'
#' @param x A fitted latent class model of class `MxModel`.
#' @param model An auxiliary model specified as an OpenMx RAM model or
#' a character string that can be converted to a RAM model using
#' [tidySEM::as_ram()]. May be omitted when `data` is a single numeric,
#'   integer, or factor variable.
#' @param data A `data.frame` containing the auxiliary variables,
#'   or a single numeric, integer, or factor variable. Observations
#'   must be in the same order as in the original latent class model.
#' @param ... Additional arguments passed to [OpenMx::mxRun()].
#'
#' @return An `MxModel` containing the original latent class model
#'   with fixed measurement parameters and freely estimated
#'   auxiliary model parameters.
#'
#' @details
#' The auxiliary model is incorporated into each class-specific RAM
#' model. By default, its parameters are freely estimated across
#' latent classes, allowing class-specific means, variances, and
#' regression coefficients.
#'
#' The measurement indicators and auxiliary variables are assumed
#' conditionally independent given latent class membership, unless
#' additional relationships are explicitly specified.
#'
#' @examples
#' \dontrun{
#' library(OpenMx)
#'
#' # Estimate a latent profile model
#' dat <- iris[, 1:4]
#' names(dat) <- c("SL", "SW", "PL", "PW")
#'
#' set.seed(123)
#' fit <- mx_profiles(
#'   data = dat[, c("PL", "PW")],
#'   classes = 2
#' )
#'
#' # Continuous distal outcome
#' res <- two_step(
#'   x = fit,
#'   data = dat$SL
#' )
#' table_results(res)
#'
#' # Equivalent specification using RAM syntax
#' res <- two_step(
#'   x = fit,
#'   model = "SL ~ 1",
#'   data = dat["SL"]
#' )
#'
#' # Ordinal distal outcome
#' ordinal <- ordered(
#'   cut(dat$SL, breaks = 3)
#' )
#'
#' res_ord <- two_step(
#'   x = fit,
#'   data = ordinal
#' )
#'
#' # Custom auxiliary model with two continuous variables
#' res_custom <- two_step(
#'   x = fit,
#'   model = "
#'     SL ~ 1
#'     SW ~ 1
#'     SL ~~ SW
#'   ",
#'   data = dat[, c("SL", "SW")]
#' )
#' }
#'
#' @references
#' Bakk, Z., & Kuha, J. (2018). Two-step estimation of models between latent
#' classes and external variables. *Psychometrika, 83*, 871–892.
#' \doi{10.1007/s11336-017-9592-7}
#'
#' @export
two_step <- function(x, model = NULL, data, ...) {
  if (!inherits(x, "MxModel") || !isTRUE(x@.wasRun)) {
    stop("'x' must be a fitted OpenMx model.")
  }

  if (num_classes(x) < 2L) {
    stop("Must have multiple classes.")
  }

  prep_model <- prep_auxiliary_model(model, data)
  model <- prep_model[["model"]]
  data <- prep_model[["data"]]

  if (length(model@submodels)) {
    stop("The auxiliary model cannot contain submodels.")
  }
  # Label first-stage parameters before fixing them
  x <- label_free_parameters(x)
  # Check number of observations
  n <- if (is_remix(x)) {
    nrow(x$Y$values)
  } else {
    nrow(x@data$observed)
  }

  if (n != nrow(data)) {
    stop(
      "Auxiliary data must have the same number of rows ",
      "as the data used to estimate the mixture model."
    )
  }

  # Fix first-stage parameters
  first_stage <- OpenMx::omxGetParameters(
    x, free = TRUE
  )

  fixed <- x

  if (length(first_stage)) {
    fixed <- OpenMx::omxSetParameters(
      fixed,
      labels = names(first_stage),
      free = FALSE
    )
  }

  # Construct and estimate model-specific second step
  out <- auxiliary_internal(x = fixed, model = model, data = data, ...)

  # Calculate corrected standard errors
  labels_first <- names(first_stage)

  labels_second <- names(
    OpenMx::omxGetParameters(out, free = TRUE)
  )

  V1 <- x@output$vcov
  V2 <- out@output$vcov
  H12 <- cross_information(
    out,
    labels_first,
    labels_second
  )

  ses <- data.frame(
    label = labels_second,
    se = two_step_vcov(V1, V2, H12)$se,
    type = "adjusted")
  attr(out, "se_robust") <- ses
  attr(out, "tidySEM") <- "two_step"
  out
}

auxiliary_internal <- function(x, ...) {
  dispatch_method(x, "auxiliary_internal", ...)
}


auxiliary_internal_mixture <- function(x, model, data, ...) {

  K <- num_classes(x)

  post_probs <- extract_postprob(x)

  original_data <- x@data$observed

  overlap <- intersect(
    names(original_data),
    names(data)
  )

  if (length(overlap)) {
    stop(
      "Auxiliary variable names overlap with measurement variables: ",
      paste(overlap, collapse = ", ")
    )
  }

  combined_data <- cbind(original_data, data)

  grp_names <- names(x@submodels)

  for (k in seq_len(K)) {

    class_name <- grp_names[k]

    measurement_model <- x[[class_name]]

    # Explicit auxiliary labels are preserved.
    # Unlabeled free parameters become class-specific.
    auxiliary_model <- label_auxiliary_parameters(
      model, k
    )

    # Posterior-weighted starting values
    auxiliary_model <- mx_starts_auxiliary(model = auxiliary_model, data = data, weights = post_probs[, k])

    combined <- mx_merge_matrices(
      measurement_model,
      auxiliary_model
    )

    combined@name <- class_name

    combined <- OpenMx::mxModel(
      combined,
      OpenMx::mxFitFunctionML(vector = TRUE)
    )

    x <- OpenMx::mxModel(x, combined)
  }

  # Reconcile starting values for equality constraints
  x <- OpenMx::omxAssignFirstParameters(x)

  x <- OpenMx::mxModel(
    x,
    OpenMx::mxData(
      observed = combined_data,
      type = "raw"
    )
  )

  OpenMx::mxRun(
    x,
    intervals = FALSE,
    silent = TRUE,
    ...
  )
}


label_auxiliary_parameters <- function(model, k) {

  for (mat_name in names(model@matrices)) {

    mat <- model@matrices[[mat_name]]

    free <- mat$free
    labs <- mat$labels

    if (!any(free)) next

    if (is.null(labs)) {
      labs <- matrix(
        NA_character_,
        nrow = nrow(free),
        ncol = ncol(free)
      )
    }

    # Only label previously unlabeled free parameters
    missing <- free & is.na(labs)

    if (!any(missing)) next

    idx <- which(missing, arr.ind = TRUE)

    for (i in seq_len(nrow(idx))) {

      r <- idx[i, 1]
      c <- idx[i, 2]

      # S is symmetric in RAM models
      if (mat_name == "S") {
        ij <- sort(c(r, c))
        r_label <- ij[1]
        c_label <- ij[2]
      } else {
        r_label <- r
        c_label <- c
      }

      labs[r, c] <- paste0(
        "aux", k, "_",
        mat_name, "_",
        r_label, "_", c_label
      )
    }

    model@matrices[[mat_name]]$labels <- labs
  }

  model
}



evaluate_m2ll <- function(model, labels, values) {

  mod <- OpenMx::omxSetParameters(
    model,
    labels = labels,
    values = values
  )

  mod <- OpenMx::mxModel(
    mod,
    OpenMx::mxComputeSequence(list(
      OpenMx::mxComputeOnce("fitfunction", "fit")
    ))
  )

  mod <- OpenMx::mxRun(
    mod,
    silent = TRUE,
    intervals = FALSE
  )

  as.numeric(mod@output$fit)
}


cross_information <- function(
    model,
    labels_first,
    labels_second,
    rel_step = 1e-4
) {

  labels <- c(labels_first, labels_second)

  joint <- OpenMx::omxSetParameters(
    model,
    labels = labels,
    free = TRUE
  )

  theta <- OpenMx::omxGetParameters(joint)
  theta <- theta[labels]

  p1 <- length(labels_first)
  p2 <- length(labels_second)

  objective <- function(par) {
    evaluate_m2ll(joint, labels, par)
  }

  H12 <- matrix(
    NA_real_,
    p1, p2,
    dimnames = list(labels_first, labels_second)
  )

  for (i in seq_len(p1)) {
    for (j in seq_len(p2)) {

      a <- i
      b <- p1 + j

      hi <- rel_step * max(1, abs(theta[a]))
      hj <- rel_step * max(1, abs(theta[b]))

      pp <- pm <- mp <- mm <- theta

      pp[c(a, b)] <- pp[c(a, b)] + c(hi, hj)
      pm[c(a, b)] <- pm[c(a, b)] + c(hi, -hj)
      mp[c(a, b)] <- mp[c(a, b)] + c(-hi, hj)
      mm[c(a, b)] <- mm[c(a, b)] - c(hi, hj)

      H12[i, j] <- (
        objective(pp) -
          objective(pm) -
          objective(mp) +
          objective(mm)
      ) / (8 * hi * hj)
    }
  }

  H12
}


two_step_vcov <- function(
    V1,
    V2,
    H12
) {

  V1 <- as.matrix(V1)
  V2 <- as.matrix(V2)
  H12 <- as.matrix(H12)

  p1 <- nrow(V1)
  p2 <- nrow(V2)

  stopifnot(
    ncol(V1) == p1,
    ncol(V2) == p2,
    all(dim(H12) == c(p1, p2))
  )

  # Additional uncertainty from step 1
  correction <- V2 %*%
    t(H12) %*%
    V1 %*%
    H12 %*%
    V2

  V <- V2 + correction

  # Numerical symmetry
  V <- (V + t(V)) / 2

  list(
    vcov = V,
    se = sqrt(pmax(0, diag(V))),
    vcov_conditional = V2,
    vcov_correction = correction
  )
}

label_free_parameters <- function(model) {

  for (nm in names(model@matrices)) {
    mat <- model@matrices[[nm]]

    idx <- which(mat$free & is.na(mat$labels))

    if (length(idx)) {
      labs <- mat$labels

      labs[idx] <- paste0(
        model@name, "_", nm, "_", idx
      )

      model@matrices[[nm]]$labels <- labs
    }
  }

  for (nm in names(model@submodels)) {
    model@submodels[[nm]] <-
      label_free_parameters(model@submodels[[nm]])
  }

  model
}

