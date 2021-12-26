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
  free <- sapply(1:length(variables), function(i){
    1:maxthres %in% 1:(numcats[i]-1)
  })
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

  #devLabels <- paste0(labels, "_dev")
  #devLabels[is.na(labels)] <- NA
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
  if(any(mats)){
    x[which(mats)] <- lapply(x[which(mats)], function(l){
      do.call(mxMatrix, l)
    })
  }
  if(any(algs)){
    x[which(algs)] <- lapply(x[which(algs)], function(l){
      do.call(mxAlgebra, l)
    })
  }
  return(x)
}

if(FALSE){
  df <- read.table("ex7.6.dat", header= F)
  names(df) <- c(paste0("u_", 1:4), "c")
  df[["c"]] <- NULL
  df$u_2[df$u_2 == 2] <- 1
  df[1:4] <- lapply(df[1:4], ordered)
  test <- mxModel("test",
                  type = "RAM",
                  manifestVars = names(df),
                  mxPath(from = "one", to = names(df), free = FALSE, values = 0),
                  mxPath(from = names(df), to = names(df), free = FALSE, values = 1, arrows = 2),
                  mxData(df[1:4], type = "raw"))
  # res1 <- mxModel(res1, "Thresholds", remove = T)
  # res1$expectation$thresholds <- "mat_dev"
  thresholds <- list_to_mx(mx_thresholds(df))
  test <- do.call(mxModel, c(list(model = test), thresholds))

  test$expectation$thresholds <- "Thresholds"
  mxRun(test) -> tmp2
  summary(tmp2)
  table_results(tmp2)

}

