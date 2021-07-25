#' @title Estimate an Auxiliary Model using the BCH Method
#' @description Estimate an auxiliary model based on a latent classification
#' by means of mixture modeling (see \code{\link{mx_mixture}}).
#'
#' The auxiliary model is treated as a multi-group model. All cases are used in
#' all groups, but they are weighted by group-specific BCH weights as described
#' in Bakk, Tekle, & Vermunt, 2013.
#' @param x An object for which a method exists.
#' @param model An object that can be converted to an \code{OpenMx} model
#' using \code{\link{as_ram}}.
#' @param data A data.frame on which the auxiliary model can be evaluated.
#' @param ... further arguments to be passed to or from other methods.
#' @return An MxModel.
#' @examples
#' dat <- data.frame(x = iris$Petal.Length)
#' mixmod <- mx_profiles(dat,
#'                       classes = 2)
#' res <- BCH(mixmod, "y ~ 1", data = data.frame(y = iris$Sepal.Length))
#' @references Bakk, Z., Tekle, F.B., & Vermunt, J.K. (2013). Estimating the
#' association between latent class membership and external variables using bias
#' adjusted three-step approaches. In T.F. Liao (ed.), Sociological Methodology.
#' Thousand Oaks, CA: SAGE publications.
#' @export
BCH <- function(x, model, data, ...){
  UseMethod("BCH", x)
}

#' @method BCH MxModel
#' @export
BCH.MxModel <- function(x, model, data, ...){
  cprobs <- class_prob(x)
  Hmat <- cprobs$mostlikely.class
  Hmatinv <- solve(Hmat)
  mostlikely <- apply(cprobs$individual, 1, which.max)
  bchweights <- data.frame(Hmatinv[mostlikely, ])
  names(bchweights) <- paste0("w", cprobs$sum.posterior$class)
  data <- cbind(data, bchweights)
  model <- as_ram(model, meanstructure = TRUE)
  grp_names <- paste("aux", cprobs$sum.posterior$class)
  grps <- lapply(1:ncol(bchweights), function(i){
    mxModel(model,
            name = grp_names[i],
            mxData(data, type = "raw", weight = names(bchweights)[i]),
            mxFitFunctionML())
  })
  grps <- do.call(mxModel, c(list(model = "aux", mxFitFunctionMultigroup(grp_names), grps)))
  run_mx(grps)
}

