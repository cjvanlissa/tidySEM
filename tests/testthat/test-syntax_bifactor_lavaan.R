
# data(Bechtoldt, Bechtoldt.1, Bechtoldt.2, Holzinger, Holzinger.9, Reise, Thurstone, Thurstone.33, Thurstone.33G, Thurstone.9, package = "psych")

datasets_psych <- data(package = "psych")$results

datasets_psych_bifactor <- datasets_psych[stringr::str_detect(datasets_psych[,"Title"], "bifactor"),"Item"]


imitate_psych_efa_1 <- function(m, nfactors = 3) {
  loadings <- psych::omega(m = m, nfactors = nfactors, plot = F, fm = 'ml')$schmid$sl[,c(2:(1+nfactors))]
  allocations <- unlist(apply(loadings, 1, function(x) which(x == max(abs(x))), simplify = T))

  # allocations <- sort(allocations)

  unlist(sapply(1:nfactors, function(f) {
    w <- which(allocations == f)
    setNames(paste0(letters[f], "_", 1:length(w)), names(w))
  })) -> new_names

  rownames(m) <- new_names[rownames(m)]
  colnames(m) <- rownames(m)

  x <- as.data.frame(MASS::mvrnorm(n = 100, mu = rep(0, ncol(m)), Sigma = m, empirical = F))
  names(x) <- colnames(m)

  # Standardize latent var initialization by ordering
  x <- x[names(x)[order(allocations)]]

  # TODO: test cross loadings?
  f <- run_lavaan(bifactor(tidy_sem(x)))

  # Do a trick to make psych use a correlation matrix
  update(f, data = NULL, sample.nobs = 100, sample.cov = m)
}

lapply(datasets_psych_bifactor[-c(4, 5, 6, 7, 8, 9, 10)], function(dataset_name) {

  # How seed sensitive is this?
  # set.seed(1337)

  cat("Modelling: ", dataset_name, "\n")

  e <- new.env()
  data(list = dataset_name, package = "psych", envir = e)
  x <- e[[dataset_name]]

  f <- imitate_psych_efa_1(x)

  a <- psych::omegaFromSem(f, plot = F)$omega.group

  b <- omega(f, standardization_method = "psych", total_variance_calculation = "observed")[,-4]

  perc <- sum(a == b) / length(a == b)

})


