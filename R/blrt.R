#' @title Conduct Bootstrapped Likelihood Ratio Test
#' @description Conduct Bootstrapped Likelihood Ratio Test to compare two
#' mixture models.
#' @param x An object for which a method exists.
#' @param replications Integer reflecting the number of bootstrapped
#' replications, defaults to `100`.
#' @param ... further arguments to be passed to or from other methods.
#' @return A data.frame.
#' @examples
#' \dontrun{
#' df <- iris[, 1, drop = FALSE]
#' names(df) <- "x"
#' res <- mx_mixture(model = "x ~ m{C}*1
#'                            x ~~ v{C}*x", classes = 1:2, data = df)
#' BLRT(res, replications = 4)
#' }
#' @export
BLRT <- function(x, replications = 100, ...){
  UseMethod("BLRT", x)
}

#' @method BLRT MxModel
#' @export
BLRT.MxModel <- function(x, replications = 100, ...){
  dots <- list(...)
  object1 <- x
  object2 <- dots[[which(sapply(dots, inherits, what = "MxModel"))[1]]]
  blrt_internal(object1, object2, replications = replications)
}

# blrt_simple <- function(mod_simple, mod_complex, replications = 100, parallel = TRUE){
#' @method BLRT mixture_list
#' @export
BLRT.mixture_list <- function(x, replications = 100, ...){
  df_empty <- data.frame(lr = NA, df = NA, blrt_p = NA, samples = NA)
  if(length(x) > 1){
    out <- mapply(function(smaller, bigger){
      tryCatch({
        blrt_internal(smaller, bigger, replications = replications)
      },
      error = function(e){
        df_empty })
    }, bigger = x[-1], smaller = x[-length(x)], SIMPLIFY = FALSE)
    out <- do.call(rbind, append(out, list(df_empty), 0))
  } else {
    out <- df_empty
  }
  out <- data.frame(null = c(NA, sapply(x[-length(x)], function(x){x@name})),
                    alt = c(NA, sapply(x[-1], function(x){x@name})),
                    out)[-1, , drop = FALSE]
  rownames(out) <- NULL
  return(out)
}

#' @method BLRT list
#' @export
BLRT.list <- BLRT.mixture_list

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
