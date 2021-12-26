simple_starts <- function (model, type = c("ULS", "DWLS"))
{
  # From omxBuildAutoStartModel
  isMultiGroupModel <- is.null(model$expectation) && (class(model$fitfunction) %in%
                                                        "MxFitFunctionMultigroup")
  if (isMultiGroupModel) {
    submNames <- sapply(strsplit(model$fitfunction$groups,
                                 ".", fixed = TRUE), "[", 1)
    wmodel <- model
    for (amod in submNames) {
      wmodel[[amod]] <- mxModel(model[[amod]], asdhelper(model,
                                                                   subname = amod, type = type))
    }
    wmodel <- mxModel(wmodel, mxFitFunctionMultigroup(submNames))
  }
  else {
    wmodel <- mxModel(model, asdhelper(model,
                                                 type = type))
  }
  wmodel <- mxOption(wmodel, "Calculate Hessian", "No")
  wmodel <- mxOption(wmodel, "Standard Errors", "No")
  # End omxBuildAutoStartModel
  wmodel <- mxRun(wmodel, silent = TRUE)
  newparams <- coef(wmodel)
  oldparams <- coef(model)
  model <- omxSetParameters(model, values = newparams, labels = names(oldparams))
  return(model)
}


#' @importFrom stats coef cov
asdhelper <- function (model, subname = model@name, type)
{
  exps <-
    mxGetExpected(model, c("covariance", "means", "thresholds"),
                  subname = subname)
  useVars <- dimnames(exps$covariance)[[1]]
  data <- model[[subname]]$data$observed
  data <- data[, useVars, drop = FALSE]
  if (type == "ULS" && !any(sapply(data, is.ordered))) {
    os <- list(cov = cov(data, use = "pair"))
    if (length(exps$means) > 0)
      os$means <- colMeans(data, na.rm = TRUE)
    return(list(
      mxData(
        data,
        type = "raw",
        numObs = nrow(data),
        observedStats = os
      ),
      mxFitFunctionWLS("ULS",
                       fullWeight = FALSE)
    ))
  }
  mdata <- mxData(data, "raw")
  list(mdata, mxFitFunctionWLS(
    type,
    ifelse(length(exps$means) >
             0, "marginals", "cumulants"),
    type != "ULS"
  ))
}
