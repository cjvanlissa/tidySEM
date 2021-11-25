mx_thresholds <- function (df, variables = names(df), prefix = "", midfix = "", suffix = "")
{
  df = df[, variables, drop = FALSE]
  isordered <- sapply(df, inherits, what = "ordered")
  numcats <- sapply(df, function(x){length(levels(x))})
  maxthres <- max(numcats)-1
  if(any(!isordered)) stop("All variables provided to mx_thresholds() must be ordered categorical.")
  labels <- sapply(1:length(variables), function(i){
    paste0(prefix, variables[i], midfix, 1:maxthres, suffix)
  })
  #   tidySEM:::bind_list(mapply(function(v, c){
  #     expand.grid(v, 1:(c-1))
  #   }, v = variables, c = numcats, SIMPLIFY = FALSE))
  # labels$value <- paste0(prefix, labels$Var1, midfix, labels$Var2, suffix)
  # labels <- reshape(labels, idvar = "Var2", timevar = "Var1", direction = "wide")

  #outer(paste0(prefix, variables), paste0(midfix, numcats, suffix), "paste0")
  free <- sapply(1:length(variables), function(i){
    1:maxthres %in% 1:(numcats[i]-1)
  })
  labels[!free] <- NA

  # Fix the mean and variance for binary variables to 0 and 1
  values_thresh <- matrix(1e-3, ncol = ncol(labels), nrow = nrow(labels))
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
    labels = labels,
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

  devLabels <- paste0(labels, "_dev")
  devLabels[is.na(labels)] <- NA
  mat_dev <- mat_thresh
  mat_dev$name <- "Threshold_deviations"
  mat_dev$labels[,] <- paste0("dev_", mat_dev$labels)
  mat_dev$values <- values_dev
  mat_dev$lbound[-1,] <- 1e-3
  mat_dev$dimnames[[1]] <- paste0("dev_", mat_dev$dimnames[[1]])
  list(
    name = ,
    type = "Full",
    nrow = maxthres, ncol = nFactors, free = threshMat$free,
    labels = devLabels, values = values_dev, lbound = 0.001,
    ubound = NA, dimnames = list(paste0("dev_", 1:maxthres),
                                 factorVarNames))
  # deviations_for_thresh$lbound[1, ] = l_u_bound[1]
  # deviations_for_thresh$ubound[1, ] = l_u_bound[2]
  mat_ones = list(
    name = "Threshold_ones",
    type = "Lower",
    nrow = maxthres,
    free = TRUE,
    values = 1)
  algebra <- list(
    name = mat_thresh$name,
    str2lang(paste0(mat_ones$name, " %*% ", mat_dev$name)),
    dimnames = mat_thresh$dimnames)
  list(mat_thresh = mat_thresh,
       mat_dev = mat_dev,
       mat_ones = mat_ones,
       algebra = algebra)
}
