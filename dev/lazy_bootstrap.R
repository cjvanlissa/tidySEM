library(foreach)
library(tidySEM)
library(parallel)

reps <- 100
df <- diabetes[, 2:4]
df <- iris[1:4]
#names(df) <- paste0("x", 1:4)
res <- mx_profiles(df, 1:7)

dat_stat = cor(df)
select_these <- lower.tri(dat_stat)
dat_stat = cor(df)[select_these]

cl <- parallel::makeCluster(parallel::detectCores())
doParallel::registerDoParallel(cl)
rep_stat <- foreach(i = rep(2:length(res), each = reps), .combine = rbind, .packages = "OpenMx") %dopar% {
  c(i,
    (cor(mxGenerateData(res[[i-1]]))[select_these]-dat_stat)^2,
    (cor(mxGenerateData(res[[i]]))[select_these] - dat_stat)^2)
}
parallel::stopCluster(cl)


npars <- sapply(res, function(m){ length(omxGetParameters(m)) })
tests <- lapply(2:length(res), function(i){
  msqs <- colMeans(rep_stat[rep_stat[,1] == i,-1])
  fvals <- msqs[1:length(dat_stat)] / msqs[(length(dat_stat)+1):length(msqs)]
  cbind(fvals,
        sapply(fvals, pf, df1 = npars[i-1], df2 = npars[i], lower.tail = FALSE))
})

sapply(tests, function(i){formatC(i[,2], digits = 3, format = "f")})

tab <- table_fit(res)
tst <- list()
for(i in 2:nrow(tab)){
  tst[[i-1]] <- tidyLPA::calc_lrt(n = tab$n[1],
                    null_ll = tab$LL[i-1],
                    null_param = tab$Parameters[i-1],
                    null_classes = i-1,
                    alt_ll = tab$LL[i],
                    alt_param = tab$Parameters[i],
                    alt_classes = i)
}
