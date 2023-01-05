blrt2 <- function(null, alt, iter = 10, ...){
  LR <- null$output$Minus2LogLikelihood - alt$output$Minus2LogLikelihood
  mnull <- null
  malt <- alt
  out <- replicate(n = iter, {
    df_boot <- mxGenerateData(null)
    m1 <- mxModel(mnull, mxData(observed = df_boot, type = "raw"))
    m1 <- try(mxRun(m1, silent = TRUE))
    if(inherits(m1, "try-error") | m1$output$status$code %in% c(5, 6)){
      m1 <- try(mxTryHard(m1, silent = TRUE, extraTries = 10, verbose = FALSE, exhaustive = FALSE))
    }
    m2 <- mxModel(malt, mxData(observed = df_boot, type = "raw"))
    m2 <- try(mxRun(m2, silent = TRUE))
    if(inherits(m2, "try-error") | m2$output$status$code %in% c(5, 6)){
      m2 <- try(mxTryHard(m2, silent = TRUE, extraTries = 10, verbose = FALSE, exhaustive = FALSE))
    }
    m1$output$Minus2LogLikelihood-m2$output$Minus2LogLikelihood
  })
  pval <- sum(out > LR) / length(out)
  c(LR = LR, p_blrt = pval)
}

blrtpar <- function(null, alt, iter = 10, parallel = TRUE, ...){
    LR <- null$output$Minus2LogLikelihood - alt$output$Minus2LogLikelihood
    mnull <- null
    malt <- alt
    cl <- parallel::makeCluster(parallel::detectCores()) #change the 2 to your number of CPU cores
    doSNOW::registerDoSNOW(cl)
    out <- foreach::foreach(n = 1:iter, .packages = c("OpenMx")) %dopar% {
      df_boot <- mxGenerateData(null)
      m1 <- mxModel(mnull, mxData(observed = df_boot, type = "raw"))
      m1 <- try(mxRun(m1, silent = TRUE))
      if(inherits(m1, "try-error") | m1$output$status$code %in% c(5, 6)){
        m1 <- try(mxTryHard(m1, silent = TRUE, extraTries = 10, verbose = FALSE, exhaustive = FALSE))
      }
      m2 <- mxModel(malt, mxData(observed = df_boot, type = "raw"))
      m2 <- try(mxRun(m2, silent = TRUE))
      if(inherits(m2, "try-error") | m2$output$status$code %in% c(5, 6)){
        m2 <- try(mxTryHard(m2, silent = TRUE, extraTries = 10, verbose = FALSE, exhaustive = FALSE))
      }
      m1$output$Minus2LogLikelihood-m2$output$Minus2LogLikelihood
    }
    stopCluster(cl)
    pval <- sum(out > LR) / length(out)
    c(LR = LR, p_blrt = pval)
}

