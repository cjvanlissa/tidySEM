#' @method predict MxModel
#' @export
predict.MxModel <- function(object,
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
    mod_fix <- mxRun(mod_fix)
    return(class_prob(mod_fix, "individual")[[1]][1:nrow(newdata), , drop = FALSE])
  }
}
