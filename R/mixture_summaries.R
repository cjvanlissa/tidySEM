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

num_classes <- function(x){
  tryCatch({
    ncol(OpenMx::mxEvalByName("weights", x))
  }, error = function(e){1L})
}

calc_fitindices <- function(model, ...){
  UseMethod("calc_fitindices", model)
}

# Try with dispatch_method, from here:
#' @export
calc_fitindices.MxModel <- function(model, ...) {
  dispatch_method(model, "calc_fitindices", ...)
}

calc_fitindices_MxModel <- function(model, ...) {
  out <- tryCatch({
    suppressMessages({
      satmod <- OpenMx::mxRefModels(model, run = TRUE)
      summary(model, refModels = satmod)
    })
  },
  error = function(e){
    summary(model)
  },
  warning = function(w){
    summary(model)
  })
  classes <- sapply(out, function(i){class(i)[1]})
  is_num <- sapply(out, inherits, what = "numeric")
  is_char <- sapply(out, inherits, what = "character")
  atomvect <- sapply(out, function(i){ is.atomic(i) & length(i) == 1})
  fits <- out[atomvect]
  if("informationCriteria" %in% names(out)){
    add_ic <- out$informationCriteria[, "par"]
    names(add_ic) <- gsub(":", "", names(add_ic), fixed = TRUE)
    fits <- c(fits, add_ic)
  }
  if(is.na(fits$AIC)){
    fits$AIC <- tryCatch(fits$Minus2LogLikelihood + 2 * fits[["estimatedParameters"]], error = function(e) NA)
  }
  if(is.na(fits$BIC)){
    fits$BIC <- fits$Minus2LogLikelihood + log(fits$numObs) * fits[["estimatedParameters"]]
  }
  if(!is.null(out[["RMSEACI"]])){
    if(any(!is.na(out[["RMSEACI"]]))){
      names(out[["RMSEACI"]]) <- paste0("RMSEA.", names(out[["RMSEACI"]]))
      fits <- c(fits, out[["RMSEACI"]])
    }
  }
  fits[["saBIC"]] <- fits[["Minus2LogLikelihood"]] + fits[["estimatedParameters"]] * log((fits[["numObs"]]+2)/24)
  if(any(endsWith(names(fits), "Message"))) fits[["warning"]] <- TRUE
  fits[["converged"]] <- mx_convergence(model)$checks_pass
  dropthese <- c("AIC.Mx", "BIC.Mx", "wasRun", "stale", "infoDefinite", "conditionNumber", "maxAbsGradient",
                 "fitUnits", "fit", "CI.Requested", "statusCode", "maxRelativeOrdinalError",
                 "timestamp", "frontendTime", "backendTime", "independentTime",
                 "wallTime", "cpuTime", "optimizerEngine", "verbose", "npsolMessage"
  )
  fits <- fits[!names(fits) %in% dropthese]
  miss_stats <- sapply(fits, is.na)
  if(any(miss_stats)) fits <- fits[!miss_stats]
  out <- as.data.frame(fits)
  rownames(out) <- NULL
  out
}

calc_fitindices_mixture <- function(model, ...) {

  sums <- calc_fitindices_MxModel(model)
  sums$degreesOfFreedom <- (sums$numObs - sums$estimatedParameters)
  classes <- num_classes(model)

  if (classes == 1L) {
    fits <- data.frame(
      Classes = 1,
      Entropy = 1,
      prob_min = 1,
      prob_max = 1,
      n_min = 1,
      n_max = 1
    )
  } else {

    post_prob <- extract_postprob(model)
    C <- max.col(post_prob)

    # Classification proportions, including empty classes
    class_tab <- tabulate(C, nbins = classes)
    prop_n <- range(class_tab / length(C))

    # Average posterior classification probabilities
    class_probs <- diag(
      classification_probs_mostlikely(post_prob, C)
    )

    fits <- data.frame(
      Classes = classes,
      Entropy = entropy(post_prob),
      prob_min = min(class_probs),
      prob_max = max(class_probs),
      n_min = prop_n[1],
      n_max = prop_n[2]
    )
  }
  drop_these <- setdiff(names(sums), c("Minus2LogLikelihood", "numObs", "estimatedParameters", "observedStatistics", "degreesOfFreedom", "RMSEASquared", "RMSEANull", "modelName", "AIC", "BIC", "saBIC", "converged"))
  if(length(drop_these) > 0) sums <- sums[!names(sums) %in% drop_these]
  data.frame(sums, fits)
}
# To here
# calc_fitindices.MxModel <- function (model, type = NULL, ...){
#   sums <- .table_fit_mx(model, type = type)
#   if(isTRUE(type == "mixture")){
#     ll <- sums[["Minus2LogLikelihood"]]/-2
#     parameters <- sums[["estimatedParameters"]]
#     n <- sums[["numObs"]]
#     classes <- num_classes(model)
#     if(classes < 2){
#       fits = c("Classes" = 1, "Entropy" = 1, "prob_min" = 1, "prob_max" = 1, "n_min" = 1, "n_max" = 1)
#     } else {
#       post_prob <- extract_postprob(model)
#       C <- apply(post_prob, 1, which.max)
#       class_tab <- table(factor(C, levels = 1:classes))
#       if (length(class_tab) == ncol(post_prob)) {
#         prop_n <- range(prop.table(class_tab))
#       }
#       else {
#         prop_n <- c(0, max(prop.table(class_tab)))
#       }
#       fits <- c("Classes" = classes,
#                 entropy(post_prob),
#                 range(diag(classification_probs_mostlikely(post_prob, C))),
#                 prop_n)
#       names(fits) <- c("Classes", "Entropy", "prob_min", "prob_max", "n_min", "n_max")
#     }
#     c(sums, fits)
#   } else {
#     sums
#   }
# }

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
#' \describe{
#' \item{"sum.posterior"}{A summary table of the posterior class
#' probabilities; this indicates what proportion of your data contributes to
#' each class.}
#' \item{"sum.mostlikely"}{A summary table of the most likely class
#' membership, based on the highest posterior class probability. Note that
#' this is subject to measurement error.}
#' \item{"mostlikely.class"}{If C is the true class of an observation, and N is
#' the assigned class based on highest posterior class probability, then this
#' table shows the probability P(C==i|N==j). The diagonal represents the
#' probability that observations in each class will be correctly classified.}
#' \item{"avg.mostlikely"}{Average posterior probabilities for each class, for
#' the subset of observations with most likely class of 1:k, where k is the
#' number of classes.}
#' \item{"individual"}{The posterior probability matrix, with dimensions n
#' (number of cases in the data) x k (number of classes).}
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
  predicted <- apply(post_probs, 1, which.max)
  post_probs_pred <- cbind(post_probs, predicted = predicted)

  out <- lapply(type, function(thetype){
    switch(thetype,
           "mostlikely.class" = classification_probs_mostlikely(post_probs),
           "avg.mostlikely" = avgprobs_mostlikely(post_probs),
           "sum.posterior" = sum_postprob(x),
           "sum.mostlikely" = sum_mostlikely(mostlikely = predicted),
           post_probs_pred)
  })
  names(out) <- type
  out
}

sum_postprob <- function(model){
  if(is_remix(model)){
    return(data.frame(class = paste0("class", seq_len(num_classes(model))),
               count = drop(model@algebras$weights$result*model@data$numObs),
               proportion = drop(model@algebras$weights$result)))

  }
  if(!inherits(model$expectation, "MxExpectationMixture")){
    return(data.frame(class = "class1", count = model$data$numObs, proportion = 1))
  }
  weights_name <- model$expectation$weights
  priors <- prop.table(model[[weights_name]]$values)
  mix_names <- model$expectation$components
  numObs <- model$data$numObs
  data.frame(class = mix_names, count = as.vector(priors*numObs), proportion = as.vector(priors))
}

sum_mostlikely <- function(model = NULL, mostlikely = NULL){
  if(!is.null(mostlikely)){
    count <- table(mostlikely)
    return(data.frame(class = paste0("class", unique(mostlikely)),
                      count = count,
                      proportion = prop.table(count)))
  }
  tryCatch({
    post_prob <- extract_postprob(model)
    classif <- table(apply(post_prob, 1, which.max))

    data.frame(class = paste0("class", seq_len(ncol(post_prob))),
               count = as.numeric(classif),
               proportion = as.numeric(prop.table(classif)))
  }, error = function(e){
    data.frame(class = "class1", count = model$data$numObs, proportion = 1)
  })
}

#extract_postprob <- function(model){
#  UseMethod("extract_postprob", model)
#}
extract_postprob <- function(x, ...) {
  if (methods::is(x, "MxModel")) {
    return(dispatch_method(x, "extract_postprob", ...))
  }
  UseMethod("extract_postprob")
}


extract_postprob_MxModel <- extract_postprob_mixture <- function(x){
  if(num_classes(x) < 2){
    return(matrix(1, nrow = x$data$numObs))
  }
  priors <- OpenMx::mxEvalByName("weights", x)[1,]
  mix_names <- x$expectation$components
  liks <- sapply(mix_names, function(nam){ attr(x@submodels[[nam]], "fitfunction")$result })
  #First calculate posteriors:
  posteriors <- sweep(liks, 2, priors, "*")
  #Then calculate participants' marginal likelihoods (probably not be necessary):
  marglik <- rowSums(posteriors)
  # Divide posteriors by marginal likelihood
  posteriors <- posteriors/marglik
  # then normalize them into probabilities:
  posteriors/rowSums(posteriors)
}

#' @export
extract_postprob.mplus.model <- function(x, ...){
  x$savedata[grepl("^CPROB", names(x$savedata))]
}

#Rows: True latent classes \(C=k\).
#Columns: Assigned classes \(W=j\).
#Entries: \(H_{kj}=P(W=j\mid C=k)\).
classification_probs_mostlikely <- function (post_prob, C = NULL)
{
  if (is.null(dim(post_prob)))
    return(1)
  if (is.null(C))
    C <- apply(post_prob, 1, which.max)
  avg_probs <- avgprobs_mostlikely(post_prob, C)
  avg_probs[is.na(avg_probs)] <- 0
  class_counts <- as.integer(table(ordered(C, levels = 1:ncol(post_prob)))) # Use ordered to ensure empty classes are included
  tab <- avg_probs * class_counts
  tab <- tab / matrix(colSums(avg_probs * class_counts), ncol = ncol(tab), nrow = nrow(tab), byrow = TRUE)
  rownames(tab) <- paste0("assigned.", 1:nrow(tab))
  colnames(tab) <- paste0("avgprob.", 1:nrow(tab))
  return(t(tab))
}


avgprobs_mostlikely <- function (post_prob, C = NULL)
{
  if (is.null(dim(post_prob)))
    return(1)
  if(is.null(C)) C <- apply(post_prob, 1, which.max)
  tab <- t(sapply(1:ncol(post_prob), function(i) {
    colMeans(post_prob[C == i, , drop = FALSE])
  }))
  rownames(tab) <- paste0("assigned.", 1:nrow(tab))
  colnames(tab) <- paste0("meanprob.", 1:ncol(tab))
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
