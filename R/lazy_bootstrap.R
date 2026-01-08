# df = iris[1:4]
# names(df) <- letters[1:4]
# res_mx <- mx_profiles(data = df, classes = 1:4)
#
# srmr <- function(x, y){
#   select_these <- lower.tri(x)
#   sqrt(mean((y[select_these] - x[select_these])^2))
# }
#
# vc_sim = cov(sims)
# vc_obs = cov(thisres$data)
# for(j in 1:ncol(vc_obs)){
#   for(k in 1:(j-1)){
#     print(((vc_sim[j,k]/sqrt(vc_sim[j,j]*vc_sim[k,k]))-(vc_obs[j,k]/sqrt(vc_obs[j,j]*vc_obs[k,k])))^2)
#   }
# }
#
# srmr.MxModel <- function(null, alt, statistics = "all"){
#   if(!inherits(y, what = "MxModel")) stop("Both 'x' and 'y' arguments must be of the same type.")
#
# }
#
#
# srmr.data.frame <- function(null, alt, statistics = "all"){
#   cor_null <- cor(null)
#   cor_alt <- cor(alt)
#   select_these <- lower.tri(cor_null)
#   sd_null <- apply(null, 2, sd)
#   sd_alt <- apply(alt, 2, sd)
#   vec <- vector(mode = "numeric")
#   if(any(c("cov", "all") %in% statistics)){
#     vec <- append(vec, (cor_alt[select_these] - cor_null[select_these]))
#   }
#   if(any(c("var", "all") %in% statistics)){
#     vec <- append(vec, (sd_alt^2 - sd_null^2)/sd_alt^2)
#   }
#   if(any(c("means", "all") %in% statistics)){
#     vec <- append(vec, (colMeans(alt)/sd_alt - colMeans(null)/sd_null))
#   }
#   sqrt(mean(vec)^2)
# }
#
# mr <- function(x){
#   sqrt(mean(x[lower.tri(x)]^2))
# }

#' @title Predictive Model Comparison
#' @description Compare (non-nested) models estimated on the same data using
#' model-simulated data.
#' @param x An object for which a method exists.
#' @param reps Number of data sets to simulate, Default: `100`.
#' @param ci Confidence interval coverage, Default: `0.95`.
#' @param ... Additional arguments.
#' @return A `data.frame`.
#' @rdname pmc_srmr
#' @export
pmc_srmr <- function(x, ..., reps = 20, ci = .95){
  UseMethod("pmc_srmr", x)
}

#' @importFrom stats quantile
#' @method pmc_srmr mixture_list
#' @export
pmc_srmr.mixture_list <- function(x, ..., reps = 100, ci = .95){
  nams <- do.call(c, lapply(x, function(i) i@name))
  dat_obs <- x[[1]]$data$observed
  not_num <- !sapply(dat_obs, inherits, what = "numeric")
  if(any(not_num)){
    dat_obs[which(not_num)] <- lapply(dat_obs[which(not_num)], as.numeric)
  }
  cor_obs <- cor(dat_obs)
  select_these <- lower.tri(cor_obs)
  cor_obs_sel <- cor_obs[select_these]

  if(requireNamespace("progressr", quietly = TRUE) & requireNamespace("progress", quietly = TRUE)){
    progressr::with_progress({
      pgs <- progressr::progressor(steps = reps*length(x))
      rep_stat <- do.call(cbind, lapply(seq_along(x), function(i){

        progmsg <- x[[i]]@name
        simres <- do.call(c, future.apply::future_replicate(
          n = reps,
          future.seed = TRUE,
          simplify = FALSE,
          expr = {
            pgs(sprintf(progmsg))
            sims <- OpenMx::mxGenerateData(x[[i]])
            if(any(not_num)){
              sims[which(not_num)] <- lapply(sims[which(not_num)], as.numeric)
            }
            cor_sim <- cor(sims)
            sqrt(mean((cor_sim[select_these] - cor_obs_sel)^2))
          }
        ))

        }))
      })

  } else {
    rep_stat <- simplify2array(lapply(x, function(thisres) {
      replicate(reps, {
        sims <- OpenMx::mxGenerateData(thisres)
        if(any(not_num)){
          sims[which(not_num)] <- lapply(sims[which(not_num)], as.numeric)
        }
        cor_sim <- cor(sims)
        sqrt(mean((cor_sim[select_these] - cor_obs_sel)^2))
      })}))

  }

  srsm_meds <- apply(rep_stat, 2, median)
  names(srsm_meds) <- nams

  cis <- list(
    dif_seq = rep_stat[, 2:dim(rep_stat)[2]] - rep_stat[, 1:(dim(rep_stat)-1L)[2]],
    dif_one = rep_stat[, 2:dim(rep_stat)[2]] - rep_stat[, rep(1, (dim(rep_stat)[2]-1L))]
  )
  cis <- do.call(rbind, lapply(cis, function(i){matrix(as.vector(apply(i, 2, stats::quantile, probs = c(((1-ci)/2), 1-((1-ci)/2)))), ncol = 2, byrow = TRUE)}))
  colnames(cis) <- paste0("srmr_", c("lb", "ub"))
  # colMeans(rep_stat)
  out <- data.frame(
    null = c(nams[1:(length(nams)-1L)], rep(nams[1], (length(nams)-1L))),
               alt = rep(nams[2:length(nams)], 2))
  out <- data.frame(comparison = rep(c("dif_seq", "dif_one"), each = nrow(out)/2),
                    out)
  out$null_srsm <- srsm_meds[out$null]
  out$alt_srsm <- srsm_meds[out$alt]
  out <- data.frame(out, cis, sig = c("", "*")[(apply(sign(cis), 1, sum) == -2)+1L])
  return(out)
}

#' #' @importFrom nonnest2 llcont
#' ic_confint <- function(x, y, conf.level = .95){
#'   ll1 <- nonnest2::llcont(x)
#'   ll2 <- nonnest2::llcont(y)
#'   n <- x$data$numObs
#'   omega.hat.2 <- (n - 1)/n * var(ll1 - ll2, na.rm = TRUE)
#'   bic1 <- AIC(object1, k = log(n))
#'   aic1 <- AIC(object1)
#'   bic2 <- AIC(object2, k = log(n))
#'   aic2 <- AIC(object2)
#'   bicdiff <- bic1 - bic2
#'   aicdiff <- aic1 - aic2
#'   alpha <- 1 - conf.level
#'   BICci <- bicdiff + qnorm(c(alpha/2, (1 - alpha/2))) * sqrt(n *
#'                                                                4 * omega.hat.2)
#'   AICci <- aicdiff + qnorm(c(alpha/2, (1 - alpha/2))) * sqrt(n *
#'                                                                4 * omega.hat.2)
#'   out <- c(bic1, bic2, BICci, aic1, aic2, AICci)
#'   names(out) <- c(paste0("BIC", 1:2), paste0("dBIC_", c("lb", "ub")), paste0("AIC", 1:2), paste0("dAIC_", c("lb", "ub")))
#'   return(out)
#' }
