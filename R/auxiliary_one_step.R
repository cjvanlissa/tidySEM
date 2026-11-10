#' One-Step Method for Auxiliary Analysis
#'
#' Estimates a joint latent class and auxiliary-variable model using
#' one-step maximum likelihood. The measurement-model parameters,
#' mixing proportions, and auxiliary-model parameters are estimated
#' simultaneously.
#'
#' @details
#' The function starts from a previously fitted latent class model
#' and adds an auxiliary model to each latent class.
#'
#' Unlike [tidySEM::two_step()], the measurement-model parameters and mixing
#' proportions are not fixed. Consequently, auxiliary variables can
#' influence the estimated latent class solution.
#'
#' Classification uncertainty is accounted for by integrating over
#' latent class membership in the joint likelihood.
#'
#' By default, unlabeled auxiliary parameters are estimated separately
#' across classes. Explicit parameter labels are preserved, allowing
#' users to specify equality constraints within and across classes.
#'
#' The measurement indicators and auxiliary variables are assumed
#' conditionally independent given latent class membership, unless
#' additional relationships are explicitly specified.
#'
#' @param x A fitted latent class model of class `MxModel`.
#' @param model An auxiliary model specified as an OpenMx RAM model
#' or a character string that can be converted using [tidySEM::as_ram()].
#' May be omitted when `data` is a single numeric, integer, or
#' factor variable.
#' @param data A `data.frame` containing the auxiliary variables,
#'   or a single numeric, integer, or factor variable. Observations
#'   must be in the same order as in the original latent class model.
#' @param ... Additional arguments passed to [OpenMx::mxRun()].
#'
#' @return An `MxModel` containing the jointly estimated measurement
#'   and auxiliary models.
#'
#' @examples
#' \dontrun{
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
#' res <- one_step(
#'   x = fit,
#'   data = dat$SL
#' )
#' table_results(res)
#'
#' # Equivalent specification using RAM syntax
#' res <- one_step(
#'   x = fit,
#'   model = "SL ~ 1",
#'   data = dat["SL"]
#' )
#'
#' # Ordinal distal outcome
#' ordinal <- ordered(cut(dat$SL, breaks = 3))
#' res_ord <- one_step(
#'   x = fit,
#'   data = ordinal
#' )
#'
#' # Multivariate continuous auxiliary model
#' res_custom <- one_step(
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
#' Bakk, Z., & Kuha, J. (2018). Two-step estimation of models
#' between latent classes and external variables. *Psychometrika, 83*, 871–892.
#' \doi{10.1007/s11336-017-9592-7}
#' @export
one_step <- function(x, model = NULL, data, ...) {

  if (!inherits(x, "MxModel") || !isTRUE(x@.wasRun)) {
    stop("'x' must be a fitted OpenMx model.")
  }

  if (num_classes(x) < 2L) {
    stop("Must have multiple classes.")
  }

  prep <- prep_auxiliary_model(model, data)

  model <- prep[["model"]]
  data <- prep[["data"]]

  if (length(model@submodels)) {
    stop("The auxiliary model cannot contain submodels.")
  }

  # Ensure unlabeled measurement parameters have stable names
  x <- label_free_parameters(x)

  # Validate number of observations
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

  # Keep all first-stage parameters free
  out <- auxiliary_internal(
    x = x,
    model = model,
    data = data,
    ...
  )

  attr(out, "tidySEM") <- "one_step"

  out
}
