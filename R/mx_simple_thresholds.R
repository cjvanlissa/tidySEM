# For ordinal data: The real beauty of the 0/1 fixed threshold approach is that the rest of the model is entirely equivalent to the continuous case, containing variances, covariances and means. Thus, e.g., a latent growth curve model is straightforward to modify for use with ordinal data.

# For binary data, this is not possible. Make sure to fix the mean and variance for binary variables to 0 and 1
mx_thresholds <- function (df, variables = names(df), output = "mx")
{
  df = df[, variables, drop = FALSE]
  isordered <- sapply(df, inherits, what = "ordered")
  numcats <- sapply(df, function(x){length(levels(x))})
  maxthres <- max(numcats)-1
  if(any(!isordered)) stop("All variables provided to mx_thresholds() must be ordered categorical.")
  # labels <- sapply(1:length(variables), function(i){
  #   paste0(prefix, variables[i], midfix, 1:maxthres, suffix)
  # })
  free <- matrix(FALSE, nrow = maxthres, ncol = length(numcats))
  for(v in seq_along(numcats)){
    free[1:(numcats[v]-1L), v] <- TRUE
  }
  #labels[!free] <- NA
  values_thresh <- matrix(1e-3, ncol = length(numcats), nrow = maxthres)
  for(i in 1:length(variables)){
    zscore = qnorm(p = cumsum(prop.table(table(df[[i]]))), lower.tail = TRUE)
    if(any(is.infinite(zscore))){
      zscore[is.infinite(zscore)] <- NA
      zscore <- update_thresholds(zscore)
    }
    values_thresh[1:(numcats[i]-1), i] <- zscore[1:(numcats[i]-1)]
  }
  mat_thresh <- list(
    name = "Thresholds",
    type = "Full",
    nrow = maxthres,
    ncol = length(variables),
    free = free,
    values = values_thresh,
    lbound = matrix(NA, nrow = nrow(values_thresh), ncol = ncol(values_thresh)),
    ubound = matrix(NA, nrow = nrow(values_thresh), ncol = ncol(values_thresh)),
    dimnames = list(paste0("th_", 1:maxthres), variables))
  # Devs
  values_dev <- values_thresh
  if (nrow(values_thresh) > 1) {
    devs <- values_thresh[-1, , drop = FALSE] - values_thresh[-nrow(values_thresh), , drop = FALSE]
    values_dev[-1, ] <- devs
    values_dev[!free] <- 1e-3
  }


  mat_indicator <- list(
    name = "Indicators",
    type = "Full",
    nrow = maxthres,
    ncol = length(variables),
    free = FALSE,
    values = as.integer(free))

  mat_dev <- mat_thresh

  mat_dev$name <- "mat_dev"
  #mat_dev$labels[,] <- paste0("dev_", mat_dev$labels)
  mat_dev$values <- values_dev
  mat_dev$lbound[-1,] <- 1e-3
  #mat_dev$dimnames[[1]] <- paste0("dev_", mat_dev$dimnames[[1]])
  mat_ones = list(
    name = "mat_ones",
    type = "Lower",
    nrow = maxthres,
    free = FALSE,
    values = 1)
  algebra <- list(
    name = mat_thresh$name,
    str2lang(paste0(mat_ones$name, " %*% ", mat_dev$name)),
    dimnames = mat_thresh$dimnames)
  out <- list(#mat_thresh = mat_thresh,
       mat_dev = mat_dev,
       mat_ones = mat_ones,
       mat_indicator = mat_indicator,
       alg_thres = algebra)
  if(output == "list"){
    return(out)
  }
  if(output == "mx"){
    return(mx_threshold_matrices(out))
  }
  return(NULL)
}

mx_threshold_matrices <- function(x, ...){
  mats <- startsWith(names(x), "mat_")
  algs <- startsWith(names(x), "alg_")
  cnst <- startsWith(names(x), "const_")
  if(any(mats)){
    x[which(mats)] <- lapply(x[which(mats)], function(l){
      do.call(OpenMx::mxMatrix, l)
    })
  }
  if(any(algs)){
    x[which(algs)] <- lapply(x[which(algs)], function(l){
      do.call(OpenMx::mxAlgebra, l)
    })
  }
  if(any(cnst)){
    x[which(cnst)] <- lapply(x[which(cnst)], function(l){
      do.call(OpenMx::mxConstraint, l)
    })
  }
  return(x)
}



mx_threshold <- function(vars, nThresh = NA, free = FALSE, values = NULL, labels = NA, lbound = NA, ubound = NA){
  if(!requireNamespace("OpenMx", quietly = TRUE)){
    return(NULL)
  }
  if(is.null(values)){
    values <- OpenMx::mxNormalQuantiles(nBreaks = nThresh)
  }
               maxthres <- max(nThresh)
               free_mat <- matrix(FALSE, nrow = maxthres, ncol = length(vars))
               ind <- matrix(0, nrow = maxthres, ncol = length(vars))
               for(v in seq_along(vars)){
                 # Set indicator matrix
                 ind[1:nThresh[v], v] <- 1
               }
               if(length(free) == length(free_mat)){
                 free_mat <- free
               } else {
                 free_mat[] <- ind == 1
               }
               mat_ones <- matrix(0, nrow = maxthres, ncol = maxthres)
               mat_ones[lower.tri(mat_ones, diag = TRUE)] <- 1
               thresh_mat <- ind
               if(length(values) == sum(ind)){
                 thresh_mat[which(ind == 1)] <- values
               } else {
                 if(length(values) == length(ind)){
                   thresh_mat[,] <- values
                 } else {
                   stop("Could not initialize thresholds.")
                 }
               }

               #solve(mat_ones) %*% (mat_ones %*% mat_dev)
               values_dev <- solve(mat_ones) %*% thresh_mat
               # Should I set unused thresholds to a small number?
               values_dev[which(ind == 0)] <- 1e-3


               mat_thresh <- list(
                 name = "Thresholds",
                 type = "Full",
                 nrow = maxthres,
                 ncol = length(vars),
                 free = free_mat,
                 values = thresh_mat,
                 lbound = NA,
                 ubound = NA,
                 dimnames = list(paste0("th_", 1:maxthres), vars))
               # Devs
               mat_indicator <- list(
                 name = "Indicators",
                 type = "Full",
                 nrow = maxthres,
                 ncol = length(vars),
                 free = FALSE,
                 values = ind)

               mat_dev <- mat_thresh
               mat_dev$name <- "mat_dev"
               mat_dev$values <- values_dev
               mat_dev$lbound <- matrix(NA, nrow = nrow(values_dev), ncol = ncol(values_dev))
               mat_dev$lbound[-1,] <- 1e-3
               mat_dev$dimnames <- list(paste0("dev_", 1:maxthres), vars)

               mat_ones = list(
                 name = "mat_ones",
                 type = "Lower",
                 nrow = maxthres,
                 free = FALSE,
                 values = 1)
               algebra <- list(
                 name = mat_thresh$name,
                 str2lang(paste0(mat_ones$name, " %*% ", mat_dev$name)),
                 dimnames = mat_thresh$dimnames)
               if(any(!is.na(labels))){
                 lab_mat <- thresh_mat
                 lab_mat[] <- labels
                 lab_mat <- as.data.frame.table(lab_mat)
                 lab_mat <- lab_mat[!is.na(lab_mat$Freq), , drop = FALSE]
                 lab_mat[c(1,2)] <- lapply(lab_mat[c(1,2)], as.integer)
                 lab_mat$Threshold <- paste0("Thresholds[", lab_mat$Var1, ",", lab_mat$Var2, "]")
                 cnsts <- do.call(c, lapply(unique(na.omit(labels)), function(labl){
                   these_thresh <- lab_mat$Threshold[lab_mat$Freq == labl]
                   if(length(these_thresh) < 2){
                     return(NULL)
                   }
                   out <- do.call(c, lapply(1:(length(these_thresh)-1L), function(i){
                     lapply(these_thresh[(i+1L):length(these_thresh)], function(withthis){
                       list(
                         name = paste0("const_", labl, "_", i),
                         str2lang(paste0(these_thresh[i], "==", withthis))
                       )
                     })
                    }))
                   # This could be done in the lapply above if I figure out how
                   # to properly index the constraints
                   for(i in 1:length(out)){
                     out[[i]]$name <- gsub("_\\d{1,}$", paste0("_", i), out[[i]]$name)
                     names(out)[[i]] <- out[[i]]$name
                   }
                  out
                 }))
               } else {
                 cnsts <- list(NULL)
               }
               out <- list(#mat_thresh = mat_thresh,
                 mat_dev = mat_dev,
                 mat_ones = mat_ones,
                 mat_indicator = mat_indicator,
                 alg_thres = algebra)
               if(length(cnsts) > 0){
                 out <- c(out, cnsts)
               }
               return(mx_threshold_matrices(out))
             }

#' @title Data Quantiles
#' @description Get quantiles based on empirical normal distribution of data.
#' @param df A `data.frame` with only columns of class `ordered`.
#' @return A matrix with the appropriate dimensions for the threshold matrix for
#' `df`, see \code{\link[OpenMx:mxThreshold]{OpenMx::mxThreshold()}}.
#' @examples
#' set.seed(1)
#' df <- data.frame(X = ordered(sample(c(1:4), size = 100, replace = TRUE,
#' prob = c(.1, .2, .5, .2))))
#' mx_data_quantiles(df)
#' @export
mx_data_quantiles <- function(df){
  isordered <- sapply(df, inherits, what = "ordered")
  if(any(!isordered)) stop("All variables provided to mx_data_quantiles() must be ordered categorical.")
  numcats <- sapply(df, function(x){length(levels(x))})
  maxthres <- max(numcats)-1L
  vals <- matrix(1e-3, nrow = maxthres, ncol = ncol(df))
  for(v in seq_along(numcats)){
    vals[1:(numcats[v]-1L), v] <- qnorm(p = cumsum(prop.table(table(df[[v]]))[1:(numcats[v]-1L)]), lower.tail = TRUE)
  }
  colnames(vals) <- names(df)
  return(vals)
}

if(FALSE){
  df <- read.table("ex7.6.dat", header= F)
  names(df) <- c(paste0("u_", 1:4), "c")
  df[["c"]] <- NULL
  df$u_2[df$u_2 == 2] <- 1
  df[1:4] <- lapply(df[1:4], ordered)
  test <- OpenMx::mxModel("test",
                  type = "RAM",
                  manifestVars = names(df),
                  OpenMx::mxPath(from = "one", to = names(df), free = FALSE, values = 0),
                  OpenMx::mxPath(from = names(df), to = names(df), free = FALSE, values = 1, arrows = 2),
                  OpenMx::mxData(df[1:4], type = "raw"))
  # res1 <- mxModel(res1, "Thresholds", remove = T)
  # res1$expectation$thresholds <- "mat_dev"
  thresholds <- list_to_mx(mx_thresholds(df))
  test <- do.call(OpenMx::mxModel, c(list(model = test), thresholds))

  test$expectation$thresholds <- "Thresholds"
  OpenMx::mxRun(test) -> tmp2
  summary(tmp2)
  table_results(tmp2)

}

