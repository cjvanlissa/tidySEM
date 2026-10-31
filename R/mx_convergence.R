#' Check Convergence of an `OpenMx` Model
#'
#' Evaluates convergence and numerical diagnostics for an `OpenMx` model.
#'
#' @param model A fitted [OpenMx::mxModel()] object.
#' @param check Character vector specifying which diagnostics to perform out of
#' `c("convergence", "fit", "parameters", "standard_errors", "boundaries",
#' "hessian")`. Default: all diagnostics.
#' @param tol Numeric atomic vector, indicating the tolerance for checking
#' whether estimates are near their boundaries.
#' @return A list containing:
#' \describe{
#'   \item{checks_pass}{Logical indicating whether all requested checks pass.}
#'   \item{convergence_ok}{Logical indicating acceptable optimizer convergence.}
#'   \item{fit_ok}{Logical indicating a finite model fit value.}
#'   \item{parameters_ok}{Logical indicating finite parameter estimates.}
#'   \item{boundaries_ok}{Logical indicating whether parameter boundary
#'     checks pass.}
#'   \item{standard_errors_ok}{Logical indicating whether standard errors
#'     are available and valid.}
#'   \item{hessian_ok}{Logical indicating whether the Hessian satisfies
#'     the implemented diagnostic criteria.}
#'   \item{warnings}{Character vector describing detected problems.}
#' }
#'
#' @details
#' Convergence diagnostics do not guarantee that the global maximum of the
#' likelihood has been found. Boundary estimates and non-positive-definite
#' Hessians may indicate weak identification. Boundary estimates do not
#' necessarily imply optimization failure.
#'
#' @examples
#' \dontrun{
#' library(OpenMx)
#' library(metamixture)
#'
#' set.seed(123)
#' dat <- sim_mix_rma(n = 100)
#'
#' fit <- remix(dat$yi, dat$vi, classes = 2)
#'
#' # Run all convergence checks
#' mx_convergence(fit)
#'
#' # Check only optimizer convergence and model fit
#' mx_convergence(fit, check = c("convergence", "fit"))
#'
#' # Overall result
#' mx_convergence(fit)$checks_pass
#' }
#' @export
mx_convergence <- function(model, check = c("convergence", "fit", "parameters", "standard_errors", "boundaries", "hessian"), tol = 1e-6) {
  # If the model was wrapped in try() and that failed, return checks_pass = FALSE
  if (inherits(model, what = "try-error")) {
    return(list(checks_pass = FALSE))
  }
  if (!inherits(model, what = "MxModel")) {
    stop("'model' must be an OpenMx MxModel object.")
  }
  status <- model$output$status$code

  # OpenMx status codes 0 and 1 are generally acceptable.
  convergence_ok <- tryCatch(all(status %in% c(0L, 1L)), error = function(e)FALSE)

  # Fit value
  fit <- model$output$fit
  fit_ok <- length(fit) == 1L && is.finite(fit)

  # Parameter estimates
  pars <- OpenMx::omxGetParameters(model, free = TRUE)
  parameters_ok <- all(is.finite(pars))

  # Boundary estimates
  boundaries <- tryCatch(
    mx_boundary_estimates(model, tol = tol),
    error = function(e) NULL
  )

  boundaries_ok <- !is.null(boundaries) &&
    !any(boundaries$boundary)

  # Standard errors, if available
  se <- tryCatch(
    summary(model)$parameters[, "Std.Error"],
    error = function(e) NULL
  )

  standard_errors_ok <- !is.null(se) &&
    length(se) > 0L &&
    all(is.finite(se))

  # Hessian diagnostics, if available
  hessian <- model$output$hessian

  hessian_ok <- NA

  if (!is.null(hessian) &&
      is.matrix(hessian) &&
      all(is.finite(hessian))) {

    eigenvalues <- eigen(
      (hessian + t(hessian)) / 2,
      symmetric = TRUE,
      only.values = TRUE
    )$values

    hessian_ok <- all(eigenvalues > 0)
  }

  # Overall convergence:
  checks_pass <- eval(parse(text = paste0(paste0(check, "_ok"), collapse = " && ")))

  # Additional diagnostics
  warnings <- character()

  if (!convergence_ok) {
    warnings <- c(warnings,
                  paste("Optimizer status:", status))
  }

  if (!fit_ok) {
    warnings <- c(warnings, "Non-finite fit value.")
  }

  if (!parameters_ok) {
    warnings <- c(warnings, "Non-finite parameter estimates.")
  }

  if (!boundaries_ok) {
    warnings <- c(warnings,
                  "One or more parameters are at their bounds.")
  }

  if (!standard_errors_ok) {
    warnings <- c(warnings,
                  "Standard errors are unavailable or non-finite.")
  }

  if (identical(hessian_ok, FALSE)) {
    warnings <- c(warnings,
                  "Hessian is not positive definite.")
  }

  out <- list(
    checks_pass = checks_pass,
    convergence_ok = convergence_ok,
    fit_ok = fit_ok,
    parameters_ok = parameters_ok,
    boundaries_ok = boundaries_ok,
    standard_errors_ok = standard_errors_ok,
    hessian_ok = hessian_ok,
    warnings = warnings
  )
  class(out) <- c("mx_convergence", class(out))
  return(out)
}

#' @export
print.mx_convergence <- function(x, ...) {

  cat("OpenMx convergence diagnostics\n")
  cat("------------------------------\n")

  checks <- c(
    "Optimizer convergence" = x$convergence_ok,
    "Model fit" = x$fit_ok,
    "Parameter estimates" = x$parameters_ok,
    "Parameter boundaries" = x$boundaries_ok,
    "Standard errors" = x$standard_errors_ok,
    "Hessian" = x$hessian_ok
  )

  for (i in seq_along(checks)) {
    status <- if (is.na(checks[i])) {
      "Not checked"
    } else if (checks[i]) {
      "OK"
    } else {
      "FAILED"
    }
    cat(sprintf("  %-24s %s\n", names(checks)[i], status))
  }
  cat("------------------------------\n")
  cat(sprintf("  %-24s %s\n", "Overall", if (isTRUE(x$checks_pass)) "OK" else "FAILED"))

  if (length(x$warnings)) {
    cat("\nWarnings:\n")
    for (w in x$warnings) {
      cat(" -", w, "\n")
    }
  }

  invisible(x)
}
