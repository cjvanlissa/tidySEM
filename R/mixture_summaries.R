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
#' table_fit(res)
#' }
#' @export
get_fit <- function(x, ...) {
  message("This function will be deprecated in the next version of tidySEM; use table_fit() instead.")
  UseMethod("table_fit", x)
}



calc_fitindices <- function(model, fitindices, ...){
  UseMethod("calc_fitindices", model)
}

calc_fitindices.MxModel <- function (model, type = NULL, ...){
  sums <- .table_fit_mx(model, type = type)
  if(isTRUE(type == "mixture")){
    ll <- sums[["Minus2LogLikelihood"]]/-2
    parameters <- sums[["estimatedParameters"]]
    n <- sums[["numObs"]]
    if(length(names(model@submodels)) < 2){
      fits = c("Classes" = 1, "Entropy" = 1, "prob_min" = 1, "prob_max" = 1, "n_min" = 1, "n_max" = 1)
    } else {
      post_prob <- extract_postprob(model)
      class <- apply(post_prob, 1, which.max)
      class_tab <- table(factor(class, levels = 1:length(names(model@submodels))))
      if (length(class_tab) == ncol(post_prob)) {
        prop_n <- range(prop.table(class_tab))
      }
      else {
        prop_n <- c(0, max(prop.table(class_tab)))
      }
      fits <- c("Classes" = length(names(model@submodels)),
        ifelse(ncol(post_prob) == 1, 1, 1 + (1/(nrow(post_prob) *
                                                  log(ncol(post_prob)))) * (sum(rowSums(post_prob *
                                                                                          log(post_prob + 1e-12))))),
        range(diag(classification_probs_mostlikely(post_prob, class))),
        prop_n)
      names(fits) <- c("Classes", "Entropy", "prob_min", "prob_max", "n_min", "n_max")
    }
    c(sums, fits)
  } else {
    sums
  }

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

#' @title Obtain latent class probabilities
#' @description Obtain latent class probabilities for an object for which a
#' method exists. See Details.
#' @details The following types are available:
#' \itemize{
#'  \item{"sum.posterior"}{A summary table of the posterior class
#'  probabilities; this indicates what proportion of your data contributes to
#'  each class.}
#'  \item{"sum.mostlikely"}{A summary table of the most likely class
#'  membership, based on the highest posterior class probability. Note that
#'  this is subject to measurement error.}
#'  \item{"mostlikely.class"}{If C is the true class of an observation, and N is
#'  the most likely class based on the model, then this table shows the
#'  probability P(N==i|C==j). The diagonal represents the probability that
#'  observations in each class will be correctly classified.}
#'  \item{"avg.mostlikely"}{Average posterior probabilities for each class, for
#'  the subset of observations with most likely class of 1:k, where k is the
#'  number of classes.}
#'  \item{"individual"}{The posterior probability matrix, with dimensions n
#'  (number of cases in the data) x k (number of classes).}
#' }
#' @param x An object for which a method exists.
#' @param type Character vector, indicating which types of probabilities to
#' extract. See Details.
#' @param ... Further arguments to be passed to or from other methods.
#' @return A data.frame.
#' @examples
#' \dontrun{
#' df <- iris[, 1, drop = FALSE]
#' names(df) <- "x"
#' res <- mx_mixture(model = "x ~ m{C}*1
#'                            x ~~ v{C}*x", classes = 1, data = df)
#' class_prob(res)
#' }
#' @export
class_prob <- function(x, type = c("sum.posterior", "sum.mostlikely", "mostlikely.class", "avg.mostlikely", "individual"), ...){
  UseMethod("class_prob", x)
}

#' @method class_prob MxModel
#' @export
class_prob.MxModel <- function(x, type = c("sum.posterior", "sum.mostlikely", "mostlikely.class", "avg.mostlikely", "individual"), ...){
  post_probs <- extract_postprob(x)
  post_probs_pred <- cbind(post_probs, predicted = apply(post_probs, 1, which.max) )

  out <- lapply(type, function(thetype){
    switch(thetype,
           "mostlikely.class" = classification_probs_mostlikely(post_probs),
           "avg.mostlikely" = avgprobs_mostlikely(post_probs),
           "sum.posterior" = sum_postprob(x),
           "sum.mostlikely" = sum_mostlikely(x),
           post_probs_pred)
  })
  names(out) <- type
  out
}

sum_postprob <- function(model){
  if(!inherits(model$expectation, "MxExpectationMixture")){
    return(data.frame(class = "class1", count = model$data$numObs, proportion = 1))
  }
  weights_name <- model$expectation$weights
  priors <- prop.table(model[[weights_name]]$values)
  mix_names <- model$expectation$components
  numObs <- model$data$numObs
  data.frame(class = mix_names, count = as.vector(priors*numObs), proportion = as.vector(priors))
}

sum_mostlikely <- function(model){
  if(!inherits(model$expectation, "MxExpectationMixture")){
    return(return(data.frame(class = "class1", count = model$data$numObs, proportion = 1)))
  }
  post_prob <- extract_postprob(model)
  classif <- table(apply(post_prob, 1, which.max))
  weights_name <- model$expectation$weights
  mix_names <- model$expectation$components
  data.frame(class = mix_names, count = as.vector(classif), proportion = as.vector(prop.table(classif)))
}

extract_postprob <- function(model){
  UseMethod("extract_postprob", model)
}

extract_postprob.MxModel <- function(model){
  if(!inherits(model$expectation, "MxExpectationMixture")){
    return(matrix(1, nrow = model$data$numObs))
  }
  weights_name <- model$expectation$weights
  priors <- model[[weights_name]]$values
  mix_names <- model$expectation$components

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

extract_postprob.mplus.model <- function(model){
  model$savedata[grepl("^CPROB", names(model$savedata))]
}

classification_probs_mostlikely <- function (post_prob, class = NULL)
{
  if (is.null(dim(post_prob)))
    return(1)
  if (is.null(class))
    class <- apply(post_prob, 1, which.max)
  avg_probs <- avgprobs_mostlikely(post_prob, class)
  avg_probs[is.na(avg_probs)] <- 0
  class_counts <- as.integer(table(ordered(class, levels = 1:ncol(post_prob)))) # Use ordered to ensure empty classes are included
  tab <- avg_probs * class_counts
  tab <- tab / matrix(colSums(avg_probs * class_counts), ncol = ncol(tab), nrow = nrow(tab), byrow = TRUE)
  rownames(tab) <- paste0("assigned.", 1:nrow(tab))
  colnames(tab) <- paste0("avgprob.", 1:nrow(tab))
  return(t(tab))
}


avgprobs_mostlikely <- function (post_prob, class = NULL)
{
  if (is.null(dim(post_prob)))
    return(1)
  if(is.null(class)) class <- apply(post_prob, 1, which.max)
  tab <- t(sapply(1:ncol(post_prob), function(i) {
    colMeans(post_prob[class == i, , drop = FALSE])
  }))
  rownames(tab) <- paste0("assigned.", 1:nrow(tab))
  colnames(tab) <- paste0("meanprob.", colnames(tab))
  return(tab)
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

# blrt_simple <- function(mod_simple, mod_complex, replications = 100, parallel = TRUE){
#' @method BLRT mixture_list
#' @export
BLRT.mixture_list <- function(x, replications = 100, ...){
  df_empty <- data.frame(lr = NA, df = NA, blrt_p = NA, samples = NA)
  if(length(x) > 1){
    out <- mapply(function(smaller, bigger){
      tryCatch({
        tidySEM:::blrt_internal(smaller, bigger, replications = replications)
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
                    out)
  rownames(out) <- NULL
  return(out)
}

#' @method BLRT list
#' @export
BLRT.list <- BLRT.mixture_list
