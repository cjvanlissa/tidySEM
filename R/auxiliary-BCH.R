#' @title Estimate an Auxiliary Model using the BCH Method
#' @description Estimate an auxiliary model based on a latent classification
#' by means of mixture modeling (see \code{\link{mx_mixture}}).
#'
#' The auxiliary model is treated as a multi-group model. All cases are used in
#' all groups, but they are weighted by group-specific BCH weights as described
#' in Bolck, Croon, & Hagenaars, 2004. Note that the standard errors of this
#' approach are not valid.
#' @param x An object for which a method exists.
#' @param model An object that can be converted to an \code{OpenMx} model
#' using \code{\link{as_ram}}.
#' @param data A data.frame on which the auxiliary model can be evaluated.
#' @param ... further arguments to be passed to or from other methods.
#' @return An MxModel.
#' @examples
#' if(requireNamespace("OpenMx", quietly = TRUE)){
#' dat <- data.frame(x = iris$Petal.Length)
#' mixmod <- mx_profiles(dat,
#'                       classes = 2)
#' res <- BCH(mixmod, "y ~ 1", data = data.frame(y = iris$Sepal.Length))
#' }
#' @references Bolck, A., Croon, M., & Hagenaars, J. (2004). Estimating latent
#' structure models with categorical variables: One-step versus three-step
#' estimators. Political Analysis, 12(1), 3-27. \doi{10.1093/pan/mph001}
#' @export
BCH <- function(x, model, data, ...){
  UseMethod("BCH", x)
}

#' @method BCH MxModel
#' @export
BCH.MxModel <- function(x, model, data, ...){
  prep_model <- prep_auxiliary_model(model, data)
  model <- prep_model[["model"]]
  data <- prep_model[["data"]]
  post_probs <- extract_postprob(x)
  Hmat <- classification_probs_mostlikely(post_probs)
  Hmatinv <- solve(Hmat)
  mostlikely <- apply(post_probs, 1, which.max)
  if (any(tabulate(mostlikely, nbins = ncol(post_probs)) == 0L)) {
    stop("At least one class has no modal assignments.")
  }
  bchweights <- data.frame(Hmatinv[mostlikely, ])
  names(bchweights) <- names(bchweights) <- paste0("wclass", seq_len(num_classes(x)))
  df <- cbind(data, bchweights)
  grp_names <- paste0("aux", seq_len(num_classes(x)))



  grps <- lapply(1:ncol(bchweights), function(i){
    OpenMx::mxModel(model,
            name = grp_names[i],
            data = OpenMx::mxData(observed = df, type = "raw", weight = names(bchweights)[i]),
            fitfunction = OpenMx::mxFitFunctionML())
  })
  grps <- do.call(OpenMx::mxModel, c(list(model = "aux", OpenMx::mxFitFunctionMultigroup(grp_names), grps)))

  out <- try(run_mx(grps), silent = TRUE)
  if (inherits(out, "try-error")) {
    warning(
      "BCH auxiliary model estimation failed: ",
      as.character(out),
      call. = FALSE
    )
    return(NULL)
  }

  attr(out, "warnings") <- c(attr(out, "warnings"), "BCH standard errors, confidence intervals, and p-values are uncorrected and should not be used for inference.")
  attr(out, "tidySEM") <- "BCH"
  return(out)

}

prep_auxiliary_model <- function(model, data){

  if(inherits(data, what = c("factor", "numeric", "integer"))){
    if(inherits(data, what = "factor")){
      if(!inherits(data, what = c("ordered"))){
        data <- mx_dummies(data)
        model <- paste0(
          names(data), " | t1",
          collapse = "\n"
        )
      } else {
        cats <- length(levels(data))
        model <- paste0("y |t", 1:(cats-1), collapse = "\n")
        data <- data.frame(y = OpenMx::mxFactor(data, levels = levels(data)))
      }
    } else {
      model <- "y ~1"
      data <- data.frame(y = data)
    }
  }
  if(inherits(model, "character")){
    model <- as_ram(model, meanstructure = TRUE)
  }
  if(!inherits(model, what = c("MxModel", "MxRAMModel"))) stop("Argument 'model' must be either an object of class 'MxModel', or a character string that can be coerced using as_ram().")
  return(list(model = model, data = data))
}

bch_continuous <- function(x, y) {
  tidySEM::BCH(
    x = x,
    model = "y ~1",
    data = data.frame(y = y)
  )
}

bch_categorical <- function(x, y) {
  cats <- length(levels(y))
  mod <- paste0("y |t", seq_len(cats - 1), collapse = "\n")
  tidySEM::BCH(
    x = x,
    model = mod,
    data = data.frame(y = OpenMx::mxFactor(y, levels = levels(y)))
  )
}


#' @title Conduct Likelihood Ratio tests
#' @description For a multigroup model of class `MxModel`,
#' conduct overall and pairwise likelihood ratio tests.
#' All submodels must be identical.
#' @param x An object for which a method exists.
#' @param compare Character vector, indicating which matrices to constrain to be
#' equal in pairwise comparisons.
#' @param ... Additional arguments passed to other functions.
#' @return An object of class `lr_test` and `list`.
#' @examples
#' if(requireNamespace("OpenMx", quietly = TRUE)){
#' df <- iris[c(1:10, 140:150), c(1, 5)]
#' names(df) <- c("x", "group")
#' mod <- as_ram("x~1", data = df, group = "group")
#' mod <- run_mx(mod)
#' lr_test(mod)
#' }
#' @rdname lr_test
#' @export
lr_test <- function(x, compare = c("All", "A", "S", "F", "M", "Thresholds"), ...){
  if(is.null(attr(x, "tidySEM"))) attr(x, "tidySEM") <- "other"
  if(isTRUE(attr(x, "tidySEM") == "mixture")) stop("No valid method to compute likelihood ratio tests for mixture models.")
  submods <- names(x@submodels)
  tests <- expand.grid(submods, submods, stringsAsFactors = FALSE)[which(upper.tri(matrix(nrow = length(submods), ncol = length(submods)))), ]
  mats <- names(x[[submods[1]]]@matrices)
  mats <- mats[sapply(mats, function(m){any(x[[submods[1]]][[m]]$free)})]
  if(!compare[1] == "All") {
    mats <- mats[mats %in% compare]
  }
  if(length(mats) < 1) stop("Argument 'compare' does not refer to any freely estimated parameters.")
  mod_test <- mod_base <- x
  for(m in mats){
    for(c in submods){
      mod_base[[c]][[m]]$labels[,] <- paste0(m, letters[1:length(mod_base[[c]][[m]]$labels)])
    }
  }
  mod_base <- OpenMx::omxAssignFirstParameters(mod_base)

  mod_base <- run_mx(mod_base)

  test_res <- do.call(rbind, lapply(1:nrow(tests), function(i){
    tmp <- mod_test
    for(m in mats){
      tmp[[tests[i, 1]]][[m]]$labels[,] <- paste0(m, letters[1:length(tmp[[tests[i, 1]]][[m]]$labels)])
      tmp[[tests[i, 2]]][[m]]$labels[,] <- paste0(m, letters[1:length(tmp[[tests[i, 1]]][[m]]$labels)])
    }
    tmp <- OpenMx::omxAssignFirstParameters(tmp)
    tmp <- run_mx(tmp)

    return(.lltest(OpenMx::mxCompare(x, tmp)))
  }))
  tests <- data.frame(tests,
                      test_res)
  names(tests)[1:2] <- c("Model1", "Model2")

  # Overall test
  test_comp <- .lltest(OpenMx::mxCompare(x, mod_base))
  tests <- list(overall = test_comp,
                pairwise = tests)
  class(tests) <- c("lr_test", class(tests))
  return(tests)
}


.lltest <- function(x, ...){
  data.frame("LL_baseline" = x$minus2LL[1],
             "LL_restricted" = x$minus2LL[2],
             "LL_dif" = x$diffLL[2],
             "df" = x$diffdf[2],
             "p" = x$p[2])
}


#' @method print lr_test
#' @export
print.lr_test <- function(x, ...){
  cat("BCH test for equality across classes\n\nOverall likelihood ratio test:\n")
  print(x$overall, ..., row.names = FALSE)
  cat("\nPairwise comparisons using", ifelse(any(grepl("^LL", names(x$pairwise))), "likelihood ratio", "Wald chi square"), "tests:\n")
  print(x$pairwise, ..., row.names = FALSE)
}
