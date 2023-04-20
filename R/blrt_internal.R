blrt_internal <- function(mod_simple, mod_complex, replications = 100, parallel = TRUE){
  lrtest <- mod_simple@output$Minus2LogLikelihood - mod_complex@output$Minus2LogLikelihood
  pb <- txtProgressBar(max = replications, style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  if(isTRUE(parallel) | inherits(parallel, "integer")){
    if(inherits(parallel, "logical")){
      parallel <- parallel::detectCores()
    } else {
      parallel <- min(c(parallel::detectCores(), parallel))
    }
    cl <- parallel::makeCluster(parallel)
    opts <- list(progress = progress)
    doSNOW::registerDoSNOW(cl)
    lrdist <- foreach(i = 1:replications, .combine = c,
                      .options.snow = opts, .packages = "OpenMx") %dopar%
      {
        df_sim <- mxGenerateData(mod_simple)
        mod_simple@data$observed <- df_sim
        mod_simple <- mxRun(mod_simple, silent = TRUE)
        mod_complex@data$observed <- df_sim
        mod_complex <- mxRun(mod_complex, silent = TRUE)
        mod_simple@output$Minus2LogLikelihood - mod_complex@output$Minus2LogLikelihood
      }
    stopCluster(cl)
  } else {
    lrdist <- vector("numeric", length = replications)
    for(i in 1:replications){
        df_sim <- mxGenerateData(mod_simple)
        mod_simple@data$observed <- df_sim
        mod_simple <- mxRun(mod_simple, silent = TRUE)
        mod_complex@data$observed <- df_sim
        mod_complex <- mxRun(mod_complex, silent = TRUE)
        setTxtProgressBar(pb,i)
        lrdist[i] <- mod_simple@output$Minus2LogLikelihood - mod_complex@output$Minus2LogLikelihood
      }
  }
  close(pb)
  data.frame(lr = lrtest,
             df = length(omxGetParameters(mod_complex)) - length(omxGetParameters(mod_simple)),
             blrt_p = sum(lrdist > lrtest) / length(lrdist))
}
