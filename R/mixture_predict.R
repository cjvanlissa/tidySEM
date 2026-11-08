#' @title Predict Class Membership
#' @description Predict class membership for latent class analyses conducted
#' with `tidySEM` in `OpenMx`.
#' @param object An `MxModel` object with attribute `tidySEM = "mixture"`.
#' @param newdata A `data.frame` with the same column names and data types as
#' the one used to estimate the model.
#' @param ... Other arguments passed to `predict.MxModel()`.
#' @return A `data.frame`.
#' @rdname predict_class
#' @export
predict_class <- function(object,
                            newdata,
                            ...){
  return(dispatch_method(object, "predict_class", ...))
}

predict_class_MxModel <-  function(object,
                                   newdata,
                                   ...){
  cl <- match.call()
  cl[[1L]] <- str2lang("OpenMx:::predict.MxModel")
  eval.parent(cl)
}

predict_mixture <- function(object, newdata = NULL, ...){
  if(is.null(newdata)){
    return(class_prob(object, "individual")[[1]][,"predicted"])
  } else {
    mod_fix <- object
    parnams <- OpenMx::omxGetParameters(mod_fix, free = TRUE)
    mod_fix <- OpenMx::omxSetParameters(mod_fix, labels = names(parnams), free = FALSE)
    mod_fix$data$observed <- rbind(newdata, mod_fix$data$observed)
    mod_fix <- OpenMx::mxRun(mod_fix)
    return(class_prob(mod_fix, "individual")[[1]][1:nrow(newdata), , drop = FALSE])
  }
}
