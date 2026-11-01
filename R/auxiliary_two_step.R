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
#'   a character string that can be converted to a RAM model using
#'   [as_ram()]. May be omitted when `data` is a single numeric,
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
#' res <- tidySEM:::two_step.MxModel(
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
#' @seealso [BCH()], [pseudo_class()]
#'
#' @export
two_step <- function(x, model = NULL, data, ...) {

  if (!inherits(x$expectation, "MxExpectationMixture")) {
    stop("'x' must be a fitted OpenMx mixture model.")
  }
  K <- num_classes(x)

  if (K < 2L) {
    stop("Must have multiple classes.")
  }
  x <- label_free_parameters(x)
  # Prepare auxiliary outcome
  if (is.factor(data) || is.numeric(data)) {
    if (is.factor(data)) {
      if (is.ordered(data)) {
        cats <- nlevels(data)
        if (cats < 2L) {
          stop("Ordinal outcome must have at least two categories.")
        }
        model <- paste0("y | t", seq_len(cats - 1L), collapse = "\n")

        data <- data.frame(y = OpenMx::mxFactor(data, levels = levels(data)))
      } else {
        # Dummy-code nominal outcome
        data <- mx_dummies(data)
        model <- paste0(names(data), " | t1", collapse = "\n")
      }
    } else {
      # Continuous outcome
      model <- "y ~ 1"
      data <- data.frame(y = as.numeric(data))
    }
  }

  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame or a single outcome vector.")
  }

  # Construct auxiliary RAM model
  if (is.character(model)) {
    model <- as_ram(model, meanstructure = TRUE)
  }

  if (length(model@submodels)) {
    stop("The auxiliary model cannot contain submodels.")
  }

  # Check observed data
  original_data <- x@data$observed
  if (nrow(original_data) != nrow(data)) {
    stop("Auxiliary data must have the same number of rows as the data used to estimate the mixture model.")
  }

  overlap <- intersect(names(original_data), names(data))

  if (length(overlap)) {
    stop("Auxiliary variable names overlap with measurement variables: ", paste(overlap, collapse = ", "))
  }

  combined_data <- cbind(original_data, data)

  # Fix first-stage parameters
  first_stage <- OpenMx::omxGetParameters(x, free = TRUE)

  if(length(first_stage)){
    fixed <- OpenMx::omxSetParameters(x, labels = names(first_stage), free = FALSE)
  }

  # Merge auxiliary model into each class
  grp_names <- names(fixed@submodels)

  for (k in seq_len(K)) {
    class_name <- grp_names[k]
    measurement_model <- fixed[[class_name]]
    # Give auxiliary parameters class-specific labels
    auxiliary_model <- model
    for (mat_name in names(auxiliary_model@matrices)) {
      mat <- auxiliary_model[[mat_name]]
      if (any(mat$free)) {
        labs <- mat$labels
        missing_labels <- mat$free & is.na(labs)
        if (any(missing_labels)) {
          labs[missing_labels] <- paste0(
            mat_name, "_",
            which(missing_labels)
          )
        }
        # Prefix free parameter labels with class
        labs[mat$free] <- paste0("aux", k, "_", labs[mat$free])
        auxiliary_model[[mat_name]]$labels <- labs
      }
    }

    # Combine measurement and auxiliary RAM models
    combined <- mx_merge_matrices(measurement_model, auxiliary_model)

    # Restore class name
    combined@name <- class_name

    # Add class-specific fit function
    combined <- OpenMx::mxModel(combined, OpenMx::mxFitFunctionML(vector = TRUE))

    fixed <- OpenMx::mxModel(fixed, combined)
  }

  # Replace observed data
  fixed <- OpenMx::mxModel(fixed, OpenMx::mxData(observed = combined_data, type = "raw"))

  # Estimate auxiliary parameters
  out <- OpenMx::mxRun(fixed, intervals = FALSE, silent = TRUE, ...)
  # First-stage parameter labels
  labels_first <- names(OpenMx::omxGetParameters(x, free = TRUE))

  # Second-stage parameter labels
  labels_second <- names(OpenMx::omxGetParameters(out, free = TRUE))

  # First-stage covariance
  V1 <- x@output$vcov

  # Conditional second-stage covariance
  V2 <- out@output$vcov

  # Cross-information
  H12 <- cross_information(out, labels_first, labels_second)
  ses <- two_step_vcov(V1, V2, H12)$se
  names(ses) <- labels_second
  attr(out, "se_robust") <- ses
  attr(out, "tidySEM") <- "two_step"
  out
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

