simple_starts <- function (model, type = c("ULS", "DWLS"))
{

  isMultiGroupModel <- (is.null(model$expectation) && inherits(model$fitfunction, "MxFitFunctionMultigroup"))
  if (isMultiGroupModel) {
    submNames <- names(model@submodels)
    wmodel <- model
    for (amod in submNames) {
      wmodel[[amod]] <- OpenMx::mxModel(model[[amod]], asdhelper(model,
                                                                   subname = amod, type = type))
    }
    wmodel <- OpenMx::mxModel(wmodel, OpenMx::mxFitFunctionMultigroup(submNames))
  }
  else {
    wmodel <- OpenMx::mxModel(model, asdhelper(model,
                                                 type = type))
  }
  wmodel <- OpenMx::mxOption(wmodel, "Calculate Hessian", "No")
  wmodel <- OpenMx::mxOption(wmodel, "Standard Errors", "No")
  # End omxBuildAutoStartModel
  wmodel <- OpenMx::mxRun(wmodel, silent = TRUE)
  newparams <- coef(wmodel)
  oldparams <- coef(model)
  model <- OpenMx::omxSetParameters(model, values = newparams, labels = names(oldparams))
  model <- fix_x(wmodel = wmodel, model = model, multigroup = isMultiGroupModel)
  return(model)
}


#' @importFrom stats coef cov
asdhelper <- function (model, subname = model@name, type)
{
  exps <-
    OpenMx::mxGetExpected(model, c("covariance", "means", "thresholds"),
                  subname = subname)
  useVars <- colnames(exps$means)
  data <- model[[subname]]$data$observed
  data <- data[, useVars, drop = FALSE]

  if (type == "ULS") {
    if(!any(sapply(data, is.ordered))){
      os <- list(cov = cov(data, use = "pair"))
      if (length(exps$means) > 0)
        os$means <- colMeans(data, na.rm = TRUE)
      return(list(
        OpenMx::mxData(
          data,
          type = "raw",
          numObs = nrow(data),
          observedStats = os
        ),
        OpenMx::mxFitFunctionWLS("ULS",
                         fullWeight = FALSE)
      ))
    }
    return(list(
      OpenMx::mxData(
        data,
        type = "raw"
      ),
      OpenMx::mxFitFunctionWLS("ULS",
                       fullWeight = FALSE)
    ))
  }
  mdata <- OpenMx::mxData(
    data,
    type = "raw"
  )
  return(list(mdata, OpenMx::mxFitFunctionWLS(
    type,
    ifelse(length(exps$means) >
             0, "marginals", "cumulants"),
    type != "ULS"
  )))
}


fix_x <- function(wmodel, model, multigroup){
  if(multigroup){
    submNames <- names(model@submodels)
    for(n in submNames){
      model[[n]] <- fix_x(wmodel = wmodel[[n]], model = model[[n]], multigroup = FALSE)
    }
  } else {
    obsvars <- wmodel$manifestVars
    fixedvar <- !diag(wmodel$S$free)[obsvars]
    zerovar <- diag(wmodel$S$values)[obsvars] == 0
    if(any(fixedvar & zerovar)){
      diag(model$S$values)[which(fixedvar & zerovar)] <- diag(wmodel$data$observedStats$cov)[which(fixedvar & zerovar)]
    }
    # fixedvcov <- !wmodel$S$free[obsvars, obsvars]
    # zerovcov <- wmodel$S$values[obsvars, obsvars] == 0
    # if(any(fixedvcov & zerovcov)){
    #   model$S$values[fixedvcov & zerovcov] <- wmodel$data$observedStats$cov[fixedvcov & zerovcov]
    # }
    # fixedmeans <- !wmodel$M$free[1, obsvars]
    # zeromeans <- wmodel$M$values[1, obsvars] == 0
    # if(any(fixedmeans & zeromeans)){
    #   model$M$values[1, fixedmeans & zeromeans] <- wmodel$data$observedStats$means[fixedmeans & zeromeans]
    # }
  }
  return(model)
}
