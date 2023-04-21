#' @importFrom future.apply future_lapply
#' @importFrom progressr with_progress progressor
blrt_internal <-
  function(mod_simple, mod_complex, replications = 100) {
    lrtest <-
      mod_simple@output$Minus2LogLikelihood - mod_complex@output$Minus2LogLikelihood
    progmsg <- paste0(mod_simple@name, " vs. ", mod_complex@name)
    progressr::with_progress({
      pgs <- progressr::progressor(steps = replications)
    bootres <- future.apply::future_lapply(
      X = 1:replications,
      future.seed = TRUE,
      FUN = function(i) {
        pgs(sprintf(progmsg))
        tryCatch({
          df_sim <- mxGenerateData(mod_simple)
          mod_simple@data$observed <-
            df_sim
          mod_complex@data$observed <-
            df_sim
          mod_simple <-
            mxRun(mod_simple,
                  silent = TRUE,
                  suppressWarnings = TRUE)
          mod_complex <-
            mxRun(mod_complex,
                  silent = TRUE,
                  suppressWarnings = TRUE)
          c(
            mod_simple@output$Minus2LogLikelihood - mod_complex@output$Minus2LogLikelihood,
            mod_simple@output$status$code + mod_complex@output$status$code
          )
        }, error = function(e) {
          c(NA, 1)
        })
      }
    )})
    bootres <- do.call(rbind, bootres)
    isvalid <- bootres[, 2] == 0
    lrdist <- bootres[isvalid, 1]
    data.frame(
      lr = lrtest,
      df = length(omxGetParameters(mod_complex)) - length(omxGetParameters(mod_simple)),
      blrt_p = sum(lrdist > lrtest) / length(lrdist),
      samples = sum(isvalid)
    )
  }
