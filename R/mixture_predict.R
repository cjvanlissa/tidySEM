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
  cl <- match.call()
  cl[[1L]] <- str2lang("OpenMx:::predict.MxModel")
  if(!is.null(attr(object, "tidySEM"))){
    if("mixture" %in% attr(object, "tidySEM")){
             cl[[1L]] <- str2lang("tidySEM:::predict_mxmodel_mixture")
    }
  }
  eval.parent(cl)
}

predict_mxmodel_mixture <- function(object, newdata = NULL, ...){
  if(is.null(newdata)){
    return(class_prob(object, "individual")[[1]][,"predicted"])
  } else {
    mod_fix <- object
    for(c in names(object@submodels)){
      for(m in names(object[[c]]@matrices)){
        mod_fix[[c]][[m]]$free[,] <- FALSE
      }
    }
    mod_fix$data$observed <- rbind(newdata, mod_fix$data$observed)
    mod_fix@matrices$weights$free[,] <- FALSE
    mod_fix <- OpenMx::mxRun(mod_fix)
    return(class_prob(mod_fix, "individual")[[1]][1:nrow(newdata), , drop = FALSE])
  }
}
