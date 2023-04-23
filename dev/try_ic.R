library(tidySEM)
library(ggplot2)
set.seed(1)
n = 100
C <- sample(c("Man", "Woman"), n, replace = TRUE, prob = c(.05, .95))
means <- c(Man = 10, Woman = 7)
X <- rnorm(n, mean = means[C], sd = 1)
obsparam <- tapply(X, C, mean)
est <- mx_profiles(data = data.frame(X), 1:3)
ft <- table_fit(est)

LBIC3 <- function(x){
  cp <- class_prob(x, type = "individual")
  m <- ordered(cp[["individual"]][, "predicted"], levels = 1:(ncol(cp[["individual"]])-1))
  ns <- table(m)
  n = sum(ns)
  pglobal <- sum(unlist(lapply(x@matrices, `[[`, "free")))
  plocal <- lapply(x@submodels, function(m){
    unlist(lapply(m@matrices, function(thismat){
      thismat$labels[thismat$free]
    }))
  })
  if(length(plocal) == 0){
    plocal = 0
  } else {
    allps <- unique(unlist(plocal))
    isglobal <- sapply(allps, function(p){
      all(sapply(plocal, function(i) p %in% i))
    })
    pglobal <- pglobal + sum(isglobal)
    plocal <- sapply(1:length(plocal), function(i){
      length(plocal[[i]][which(!plocal[[i]] %in% names(isglobal)[isglobal])])
    })
  }

  p = pglobal + sum(plocal)
  if(!p == length(coef(x))) stop("Incorrect parameter calculation.")
  locpen <- plocal * log(ns)
  weights <- 1/(ns/plocal)
  c(
    bic = x@output$Minus2LogLikelihood + p * log(n),
    lbic = x@output$Minus2LogLikelihood + (p * log(n)) + (max(ns) / min(ns)) * sum(weights*locpen),
    lbiclog = x@output$Minus2LogLikelihood + (p * log(n)) + (log(max(ns) / min(ns)) * sum(weights*locpen))
  )
}

sapply(est, LBIC3)
