#' @title Get fit indices from objects
#' @description Get fit indices from objects for which a method exists.
#' @param x An object for which a method exists.
#' @param ... further arguments to be passed to or from other methods.
#' @return A data.frame.
#' @examples
#' \dontrun{
#' df <- iris[, 1, drop = FALSE]
#' names(df) <- "x"
#' res <- mx_mixture(model = "x ~ m{C}*1
#'                            x ~~ v{C}*x", classes = 1, data = df)
#' get_fit(res)
#' }
#' @export
get_fit <- function(x, ...) {
  UseMethod("get_fit", x)
}

#' @method get_fit mixture_list
#' @export
get_fit.mixture_list <- function(x, ...) {
  sapply(x, get_fit)
}

#' @method get_fit MxModel
#' @export
get_fit.MxModel <- function(x, ...) {
  if(is.null(attr(x, "type"))) attr(x, "type") <- "default"
  switch(attr(x, "type"),
         "list" = sapply(x, function(i){ get_fit(i, ...)}),
         "mixture" = calc_fitindices(x, type = "mixture", ...),
         calc_fitindices(x, ...))
}

calc_fitindices <- function(model, fitindices, ...){
  UseMethod("calc_fitindices", model)
}

calc_fitindices.MxModel <- function (model, type = NULL, ...){
  sums <- summary(model)
  ll <- sums$Minus2LogLikelihood/-2
  parameters <- sums$estimatedParameters
  n <- model$data$numObs
  post_prob <- NULL
  fits <- NULL
  if(isTRUE(type == "mixture")){
    post_prob <- extract_postprob(model)
    class <- apply(post_prob, 1, which.max)
    class_tab <- table(class)
    if (length(class_tab) == ncol(post_prob)) {
      prop_n <- range(prop.table(class_tab))
    }
    else {
      prop_n <- c(0, max(prop.table(class_tab)))
    }
    fits <- c(
      ifelse(ncol(post_prob) == 1, 1, 1 + (1/(nrow(post_prob) *
                                                log(ncol(post_prob)))) * (sum(rowSums(post_prob *
                                                                                        log(post_prob + 1e-12))))),
      range(diag(classification_probs_mostlikely(post_prob, class))),
      prop_n)
    names(fits) <- c("Entropy", "prob_min", "prob_max", "n_min", "n_max")
  }
  make_fitvector(ll = ll, parameters = parameters, n = n, postprob = post_prob, fits = fits)
}

make_fitvector <- function(ll, parameters, n, postprob = NULL, fits = NULL){
  out <- vector("numeric")
  out["LogLik"]                     <- ll
  out["parameters"]                 <- parameters
  out["n"]                          <- n
  out["AIC"]                        <- -2*ll + (2*parameters)
  if(!is.null(fits)) out["AWE"]     <- -2*(ll + unname(fits[1])) + 2*parameters*(3/2 + log(n))
  out["BIC"]                        <- -2*ll + (parameters * log(n))
  out["CAIC"]                       <- -2*ll + (parameters * (log(n)+1))
  if(!is.null(fits)) out["CLC"]     <- -2*ll + (2*unname(fits[1]))
  out["KIC"]                        <- -2*ll + (3*(parameters + 1))
  out["SABIC"]                      <- -2*ll + (parameters * log(((n+2)/24)))
  if(!is.null(postprob)) out["ICL"] <- icl_default(postprob, out["BIC"])
  c(out, fits)
}

extract_postprob <- function(model){
  priors <- model$expectation$output$weights
  mix_names <- names(model@submodels)

  attr(model@submodels$class1, "fitfunction")$result
  liks <- sapply(mix_names, function(x){ attr(model@submodels[[x]], "fitfunction")$result })

  #First calculate posteriors:
  posteriors <- sweep(liks, 2, priors, "*")
  #Then calculate participants' marginal likelihoods (probably not be necessary):
  marglik <- rowSums(posteriors)
  # Divide posteriors by marginal likelihood
  posteriors <- posteriors/marglik
  # then normalize them into probabilities:
  posteriors/rowSums(posteriors)
}

classification_probs_mostlikely <- function (post_prob, class)
{
  if (is.null(dim(post_prob)))
    return(1)
  avg_probs <- avgprobs_mostlikely(post_prob, class)
  avg_probs[is.na(avg_probs)] <- 0
  C <- dim(post_prob)[2]
  N <- sapply(1:C, function(x) sum(class == x))
  tab <- mapply(function(this_row, this_col) {
    (avg_probs[this_row, this_col] * N[this_row])/(sum(avg_probs[,
                                                                 this_col] * N, na.rm = TRUE))
  }, this_row = rep(1:C, C), this_col = rep(1:C, each = C))
  matrix(tab, C, C, byrow = TRUE)
}

avgprobs_mostlikely <- function (post_prob, class)
{
  if (is.null(dim(post_prob)))
    return(1)
  t(sapply(1:ncol(post_prob), function(i) {
    colMeans(post_prob[class == i, , drop = FALSE])
  }))
}

icl.MxModel <- function (object, ...)
{

}




avgprobs_mostlikely <- function(post_prob, class){
  if(is.null(dim(post_prob))) return(1)
  t(sapply(1:ncol(post_prob), function(i){colMeans(post_prob[class == i, , drop = FALSE])}))
}

classification_probs_mostlikely <- function(post_prob, class){
  if(is.null(dim(post_prob))) return(1)
  avg_probs <- avgprobs_mostlikely(post_prob, class)
  avg_probs[is.na(avg_probs)] <- 0
  C <- dim(post_prob)[2]
  N <- sapply(1:C, function(x) sum(class == x))
  tab <- mapply(function(this_row, this_col){
    (avg_probs[this_row, this_col]*N[this_row])/(sum(avg_probs[ , this_col] * N, na.rm = TRUE))
  }, this_row = rep(1:C, C), this_col = rep(1:C, each = C))

  matrix(tab, C, C, byrow = TRUE)
}

# ICL method for mplus.model ----------------------------------------------

icl.mplus.model <- function(object, ...)
{
  n <- object$summaries$Observations
  z <- object$savedata[, grep("^CPROB", names(object$savedata))]
  if(!is.null(dim(z))){
    C <- z == apply(z, 1, max)
    (-1*object$summaries$BIC) + 2*sum(C * log(apply(z, 1, function(x){x[which.max(x)]})+1e-12))
  } else {
    (-1*object$summaries$BIC) + 2*sum(z * log(z))
  }
}

icl_default <- function(post_prob, BIC){
  tryCatch({
    if (!is.null(dim(post_prob))) {
      C <- post_prob == apply(post_prob, 1, max)
      (-1 * BIC) + 2 * sum(C * log(apply(post_prob,
                                         1, function(x) {
                                           x[which.max(x)]
                                         }) + 1e-12))
    }
    else {
      (-1 * BIC) + 2 * sum(post_prob * log(post_prob))
    }
  }, error = function(e){ NA })
}
