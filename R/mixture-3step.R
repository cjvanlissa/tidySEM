#' @title Estimate an Auxiliary Model using the BCH Method
#' @description Estimate an auxiliary model based on a latent classification
#' by means of mixture modeling (see \code{\link{mx_mixture}}).
#'
#' The auxiliary model is treated as a multi-group model. All cases are used in
#' all groups, but they are weighted by group-specific BCH weights as described
#' in Bolck, Croon, & Hagenaars, 2004.
#' @param x An object for which a method exists.
#' @param model An object that can be converted to an \code{OpenMx} model
#' using \code{\link{as_ram}}.
#' @param data A data.frame on which the auxiliary model can be evaluated.
#' @param ... further arguments to be passed to or from other methods.
#' @return An MxModel.
#' @examples
#' dat <- data.frame(x = iris$Petal.Length)
#' mixmod <- mx_profiles(dat,
#'                       classes = 2)
#' res <- BCH(mixmod, "y ~ 1", data = data.frame(y = iris$Sepal.Length))
#' @references Bolck, A., Croon, M., & Hagenaars, J. (2004). Estimating latent
#' structure models with categorical variables: One-step versus three-step
#' estimators. Political Analysis, 12(1), 3–27. <doi:10.2307/25791751>
#' @export
BCH <- function(x, model, data, ...){
  UseMethod("BCH", x)
}

#' @method BCH MxModel
#' @export
BCH.MxModel <- function(x, model, data, ...){
  if(inherits(data, what = c("factor", "numeric", "integer"))){
    cl <- match.call()
    cl <- cl[c(1L, 2L, which(names(cl) == "data"))]
    names(cl)[3] <- "y"
    if(inherits(data, what = "factor")){
      cl[[1L]] <- str2lang("bch_categorical")
    } else {
      cl[[1L]] <- str2lang("bch_continuous")
    }
    return(eval(cl))
  }
  cprobs <- class_prob(x)
  Hmat <- cprobs$mostlikely.class
  Hmatinv <- solve(Hmat)
  mostlikely <- cprobs$individual[,"predicted"]
  bchweights <- data.frame(Hmatinv[mostlikely, ])
  names(bchweights) <- paste0("w", cprobs$sum.posterior$class)
  df <- cbind(data, bchweights)
  grp_names <- cprobs$sum.posterior$class

  if(inherits(model, "character")){
    model <- as_ram(model, meanstructure = TRUE)
  }
  if(!inherits(model, "MxModel")) stop("Argument 'model' must be either an object of class 'MxModel', or a character string that can be coerced using as_ram().")

  grps <- lapply(1:ncol(bchweights), function(i){
    mxModel(model,
            name = grp_names[i],
            data = mxData(observed = df, type = "raw", weight = names(bchweights)[i]),
            fitfunction = mxFitFunctionML())
  })
  grps <- do.call(mxModel, c(list(model = "aux", mxFitFunctionMultigroup(grp_names), grps)))
  out <- try(run_mx(grps), silent = TRUE)
  if(!inherits(out, "try-error")){
    return(out)
  }
  out <- try(mxRun(grps), silent = TRUE)
  if(!inherits(out, "try-error")){
    return(out)
  }
  NULL
}


bch_continuous <- function(x, y){
  cl <- match.call()
  cl[[1L]] <- quote(BCH)
  cl[["y"]] <- NULL
  cl[["model"]] <- "y ~1"
  cl[["data"]] <- data.frame(y = y)
  bchmod <- eval.parent(cl)
  for(c in names(bchmod@submodels)){
    bchmod[[c]]$M$labels[1,1] <- paste0("m_", c)
    bchmod[[c]]$S$labels[1,1] <- "s"
  }
  bchmod <- mxRun(omxAssignFirstParameters(bchmod))
  params <- paste0("m_", names(bchmod@submodels))
  tests <- expand.grid(params, params)[which(upper.tri(matrix(nrow = length(params), ncol = length(params)))), ]
  test_res <- do.call(rbind, suppressWarnings(lapply(paste0(tests$Var1, "=", tests$Var2), function(x){ car::linearHypothesis(model = bchmod, x)[2, 1:3]})))
  test_values <- table_results(bchmod, columns = c("est", "openmx_label"))
  tests <- data.frame(tests,
                      Value1 = test_values$est[match(tests$Var1, test_values$openmx_label)],
                      Value2 = test_values$est[match(tests$Var2, test_values$openmx_label)],
                      test_res)
  names(tests) <- c("Par1", "Par2", "Value1", "Value2", "Df", "Chisq", "p")
  tests$Par1 <- substring(tests$Par1, first = 3)
  tests$Par2 <- substring(tests$Par2, first = 3)

  base <- run_mx(as_ram("y ~1"), data = data.frame(y = y))
  test_comp <- mxCompare(bchmod, base)
  test_comp <- data.frame("LL_baseline" = test_comp$minus2LL[1],
                          "LL_bch" = test_comp$minus2LL[2],
                          "LL_dif" = test_comp$diffLL[2],
                          "df" = test_comp$diffdf[2],
                          "p" = test_comp$p[2])
  tests <- list(overall = test_comp,
              pairwise = tests)
  class(tests) <- c("bch_test", class(tests))
  out <- bchmod
  attr(out, "tests") <- tests
  return(out)
}

bch_categorical <- function(x, y){
  cl <- match.call()
  cl[[1L]] <- quote(BCH)
  cl[["y"]] <- NULL
  cats <- length(levels(y))
  mod <- paste0("y |t", 1:(cats-1), collapse = "\n")
  cl[["model"]] <- mod
  cl[["data"]] <- data.frame(y = mxFactor(y, levels = levels(y)))
  bchmod <- eval.parent(cl)
  bchmod <- mxTryHardOrdinal(bchmod)
  cl[[1L]] <- quote(run_mx)
  cl[["x"]] <- as_ram(mod)
  cl[["model"]] <- NULL
  mod_base <- eval.parent(cl)

  models <- names(bchmod@submodels)
  tests <- expand.grid(models, models, stringsAsFactors = FALSE)[which(upper.tri(matrix(nrow = length(models), ncol = length(models)))), ]

  test_res <- do.call(rbind, lapply(1:nrow(tests), function(i){
    tmp <- bchmod
    tmp[[tests[i, 1]]]$Thresholds$labels[,1] <- letters[1:nrow(tmp[[tests[i, 1]]]$Thresholds$labels)]
    tmp[[tests[i, 2]]]$Thresholds$labels[,1] <- letters[1:nrow(tmp[[tests[i, 1]]]$Thresholds$labels)]
    tmp <- omxAssignFirstParameters(tmp)
    tmp <- mxTryHardOrdinal(tmp, verbose = FALSE, silent = TRUE)
    .lltest(mxCompare(bchmod, tmp))
  }))

  tests <- data.frame(tests,
                      test_res)
  names(tests)[1:3] <- c("Class1", "Class2", "LL_bch")

  test_comp <- .lltest(mxCompare(bchmod, mod_base))
  tests <- list(overall = test_comp,
              pairwise = tests)
  class(tests) <- c("bch_test", class(tests))
  out <- bchmod
  attr(out, "tests") <- tests
  return(out)
}

#' @title Extract BCH test from object
#' @description For a model generated by [BCH()], extract equivalence tests.
#' @param x An object with attribute `test`.
#' @param ... Additional arguments passed to other functions.
#' @return An object of class `bch_test` and `list`.
#' @examples
#' a <- "test"
#' attr(a, "test") <- "The test"
#' bch_test(a)
#' @rdname bch_test
#' @export
bch_test <- function(x, ...){
  if(is.null(attr(x, which = "test"))) stop("This object does not have a BCH test attribute.")
  return(attr(x, which = "test"))
}

.lltest <- function(x, ...){
  data.frame("LL_baseline" = x$minus2LL[1],
             "LL_restricted" = x$minus2LL[2],
             "LL_dif" = x$diffLL[2],
             "df" = x$diffdf[2],
             "p" = x$p[2])
}


#' @method print bch_test
#' @export
print.bch_test <- function(x, ...){
  cat("BCH test for equality of means across classes\n\nOverall likelihood ratio test:\n")
  print(x$overall, ..., row.names = FALSE)
  cat("\nPairwise comparisons using", ifelse(any(grepl("^LL", names(x$pairwise))), "likelihood ratio", "Wald chi square"), "tests:\n")
  print(x$pairwise, ..., row.names = FALSE)
}
