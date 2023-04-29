library(tidySEM)
library(ggplot2)
ics <- function(x){
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

  # BIC
  bic = x@output$Minus2LogLikelihood + p * log(n)
  # ICL
  z <- cp[["individual"]][, -ncol(cp[["individual"]]), drop = FALSE]
  if(ncol(z) > 1){
    C <- matrix(0, nrow = nrow(z), ncol = ncol(z))
    m <- as.integer(as.character(m))
    for(i in 1:nrow(z)){
      C[i, m[i]] <- 1
    }
  } else {
    C <- matrix(1, nrow = nrow(z), ncol = 1)
  }
  zlog <- log(z)
  zlog[z <= 0] <- 0
  icl <- bic - 2 * sum(C * zlog)

  lbic = x@output$Minus2LogLikelihood + (p * log(n)) + (max(ns) / min(ns)) * sum(weights*locpen)
  lbiclog = (x@output$Minus2LogLikelihood + (p * log(n)) + (log(max(ns) / min(ns)) * sum(weights*locpen)))
  if(is.infinite(lbic)){
    lbic <- -1*lbic
    lbiclog <- -1*lbiclog
  }

  c(
    bic = bic,
    lbic = lbic,
    lbiclog = lbiclog,
    icl = icl
  )
}

set.seed(2)
C <- c(rep(1, 100), rep(2, 2))
means <- c("1" = 0, "2" = 5)
X <- data.frame(X = rnorm(length(C), mean = means[C], sd = 1))
res <- mx_profiles(data = data.frame(X), 1:3)
table_fit(res)
apply(sapply(res, ics), 1, which.min)


set.seed(2)
C <- c(rep(1, 100), rep(2, 2))
means <- c("1" = 0, "2" = 3)
X <- data.frame(X = rnorm(length(C), mean = means[C], sd = 1))
res <- mx_profiles(data = data.frame(X), 1:3)
table_fit(res)
apply(sapply(res, ics), 1, which.min)


set.seed(2)
C <- c(rep(1, 92), rep(2, 30))
means <- c("1" = 0, "2" = 4)
X <- data.frame(X = rnorm(length(C), mean = means[C], sd = 1))
res <- mx_profiles(data = data.frame(X), 1:3)
table_fit(res)
apply(sapply(res, ics), 1, which.min)

set.seed(2)
C <- c(rep(1, 92), rep(2, 5))
means <- c("1" = 0, "2" = 5)
X <- data.frame(X = rnorm(length(C), mean = means[C], sd = 1))
res <- mx_profiles(data = data.frame(X), 1:3)
table_fit(res)
apply(sapply(res, ics), 1, which.min)
