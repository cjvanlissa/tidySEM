# fix_threshold <- function(model, threshold = 5, value = 5, free = TRUE){
#   out <- model
#   classes <- names(model@submodels)
#   for(c in classes){
#     large <- abs(model[[c]]$mat_dev$values) > threshold & model[[c]]$mat_dev$free
#     if(any(large)){
#       out[[c]]$mat_dev$values[which(large)] <- value
#       out[[c]]$mat_dev$lbound[which(large)] <- value-1e-3
#       out[[c]]$mat_dev$ubound[which(large)] <- value+1e-3
#       if(!free){
#         out[[c]]$mat_dev$free[which(large)] <- FALSE
#       }
#     }
#   }
#   out
# }

fix_threshold <- function(model, threshold = 5, value = 5){
  out <- model
  classes <- names(model@submodels)
  if(is.null(classes)){
    large <- abs(model$Thresholds$result) > threshold & model$mat_dev$free
    if(any(large)){
      for(j in 1:ncol(model$Thresholds$result)){
        thiscol <- large[, j]
        if(any(thiscol)){

          out$mat_dev$values[(which(thiscol)[1]+1L):length(thiscol), j] <- 1e-3
          out$mat_dev$lbound[(which(thiscol)[1]+1L):length(thiscol), j] <- 1e-4
          out$mat_dev$free[which(thiscol)[1]:length(thiscol), j] <- FALSE
        }
      }
    }
    return(out)
  } else {
    for(c in classes){
      out[[c]] <- fix_threshold(model[[c]], threshold = threshold, value = value)
    }
  }
  return(out)
}

