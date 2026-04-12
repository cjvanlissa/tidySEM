#globalVariables("y")
#' @title Predictive Model Comparison
#' @description Compare (non-nested) models estimated on the same data using
#' model-simulated data.
#' @param x An object for which a method exists.
#' @param reps Number of data sets to simulate, Default: `100`.
#' @param ci Confidence interval coverage, Default: `0.95`.
#' @param ... Additional arguments.
#' @return A `data.frame`.
#' @rdname pmc
#' @param FUN Function used to compare the real data (referred to as `x`) to the
#' model-implied data (referred to as `y`). Defaults to `NULL`, which uses
#' [tidySEM::chi_sq()] for models with all ordinal variables, and
#' [tidySEM::srmr()] otherwise, treating all variables as continuous.
#' @export
pmc <- function(x, ..., reps = 20, ci = .95, FUN = NULL){
  UseMethod("pmc", x)
}

#' @rdname pmc
#' @export
pmc_srmr <- function(x, ..., reps = 20, ci = .95){
  .Deprecated("pmc")
  cl <- match.call()
  cl[[1]] <- quote(pmc)
  eval.parent(cl)
}

#' @title Calculate Standardized Root Mean Residual
#' @description Given two datasets, computes the correlation matrix for both,
#' and then calculates the standardized root mean residual difference between
#' these two correlation matrices.
#' @param x An object for which a method of [stats::cor()] exists
#' (e.g, `data.frame`).
#' @param y An object for which a method of [stats::cor()] exists
#' (e.g, `data.frame`).
#' @return `numeric`
#' @examples
#' \dontrun{
#' if(interactive()){
#'  srmr(iris[1:2], iris[3:4])
#' }
#' }
#' @rdname srmr
#' @export
srmr <- function(x, y){
  cor_null <- cor(x)
  cor_alt <- cor(y)
  select_these <- lower.tri(cor_null)
  dif <- cor_alt[select_these] - cor_null[select_these]
  sqrt(mean(dif)^2)
}

#' @title Calculate Chi Square Statistic
#' @description Given two datasets with ordinal variables,
#' computes the chi squared statistic. To obtain the lambda statistic as used in
#' [tidySEM::pmc()], subtract the degrees of freedom.
#' @param x An object for which a method exists
#' (e.g, `data.frame`).
#' @param y An object for which a method exists
#' (e.g, `data.frame`).
#' @return `numeric`
#' @examples
#' \dontrun{
#' if(interactive()){
#'  chi_sq(iris[1:2], iris[3:4])
#' }
#' }
#' @rdname chi_sq
#' @export
chi_sq <- function(x, y){
  tab_obs  <- table(x)
  tab_sim <- table(y)
  term <- tab_obs*log(tab_obs/tab_sim)
  term[is.infinite(term) | is.nan(term) | is.na(term)] <- 0
  2*sum(term)
}



#' @method pmc mixture_list
#' @importFrom stats quantile
#' @export
pmc.mixture_list <- function(x, ..., reps = 100, ci = .95, FUN = NULL){
  FUN <- try(match.fun(FUN), silent = TRUE)
  which_stat <- "custom function"
  exp_text <- c('assign("y", OpenMx::mxGenerateData(x[[i]]), envir = eval_env)',
                'as.numeric(eval(body(FUN), envir = eval_env))')
  nams <- c("comparison", "null", "alt", "null_stat", "alt_stat", "lb_dif",
            "ub_dif", "sig")
  if(inherits(FUN, what = "try-error")){
    if(all(sapply(x[[1]]$data$observed, inherits, what = "ordered"))){
      tab_obs  <- table(x[[1]]$data$observed)
      num_obs <- nrow(x[[1]]$data$observed)
      which_stat <- "chi squared"
      exp_text <- c("tab_sim <- table(OpenMx::mxGenerateData(x[[i]]))",
                    "term <- tab_obs*log(tab_obs/tab_sim)",
                    "term[is.infinite(term) | is.nan(term) | is.na(term)] <- 0",
                    "(2*sum(term))-(num_obs - length(coef(x[[i]])))")
    } else {
      which_stat <- "SRMR"
      dat_obs <- x[[1]]$data$observed
      not_num <- !sapply(dat_obs, inherits, what = "numeric")
      if(any(not_num)){
        dat_obs[which(not_num)] <- lapply(dat_obs[which(not_num)], as.numeric)
      }
      cor_obs <- cor(dat_obs)
      select_these <- lower.tri(cor_obs)
      cor_obs_sel <- cor_obs[select_these]
      exp_text <- c("sims <- OpenMx::mxGenerateData(x[[i]])",
                    "if(any(not_num)){",
                    "  sims[which(not_num)] <- lapply(sims[which(not_num)], as.numeric)",
                    "}",
                    "cor_sim <- cor(sims)",
                    "sqrt(mean((cor_sim[select_these] - cor_obs_sel)^2))")
    }
  }
  nams <- do.call(c, lapply(x, function(i) i@name))
  eval_env <- new.env()
  assign("x", x[[1]]$data$observed, envir = eval_env)
  THE_EXP <- parse(text = exp_text)
  rep_stat <- do.call(cbind, lapply(seq_along(x), function(i){
      if(requireNamespace("future.apply", quietly = TRUE)){
        do.call(c, future.apply::future_replicate(
          n = reps,
          future.seed = TRUE,
          simplify = FALSE,
          expr = eval(THE_EXP)
        ))
      } else {
        replicate(reps, eval(THE_EXP))
      }
  }))
  # Get summary statistics and prepare output
  meds <- apply(rep_stat, 2, median)
  names(meds) <- nams
  cis <- list(
    dif_seq = rep_stat[, 2:dim(rep_stat)[2]] - rep_stat[, 1:(dim(rep_stat)-1L)[2]],
    dif_one = rep_stat[, 2:dim(rep_stat)[2]] - rep_stat[, rep(1, (dim(rep_stat)[2]-1L))]
  )
  cis <- do.call(rbind, lapply(cis, function(i){matrix(as.vector(apply(i, 2, stats::quantile, probs = c(((1-ci)/2), 1-((1-ci)/2)))), ncol = 2, byrow = TRUE)}))
  colnames(cis) <- c("lb", "ub")
  out <- data.frame(
    null = c(nams[1:(length(nams)-1L)], rep(nams[1], (length(nams)-1L))),
    alt = rep(nams[2:length(nams)], 2))
  out <- data.frame(comparison = rep(c("dif_seq", "dif_one"), each = nrow(out)/2),
                    out)
  out$null_stat <- meds[out$null]
  out$alt_stat <- meds[out$alt]
  out <- data.frame(out, cis, sig = c("", "*")[(apply(sign(cis), 1, sum) == -2)+1L])
  class(out) <- c("pmc_df", class(out))
  attr(out, "stat") <- which_stat
  return(out)
}

#' @method print pmc_df
#' @export
print.pmc_df <- function(x, ...){
  cat("PMC model comparison using ", attr(x, "stat"), ":\n\n", sep = "")
  print.data.frame(x, row.names = FALSE, ...)
}
