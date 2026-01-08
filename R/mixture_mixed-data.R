#' Estimate mixed data latent class analysis using OpenMx
#'
#' This function simplifies the specification of latent class models with mixed
#' data types:
#' models that estimate membership of a categorical latent variable based on
#' binary/ordinal and continuous indicators. See Details for more information.
#' @inheritParams mx_profiles
#' @return A list of class `mixture_list`.
#' @details
#' The procedure is as follows:
#'
#' 1. Construct a latent profile model for the continuous indicators using
#'    \code{\link[tidySEM:mx_profiles]{mx_profiles()}}.
#' 2. Construct a latent class model for the categorical indicators using
#'    \code{\link[tidySEM:mx_lca]{mx_lca()}}.
#' 3. Combine the models from steps 1. and 2. into one joint model.
#'
#' If `run = TRUE`, simulated annealing is used to estimate the mixture model,
#' as explained in Van Lissa, Garnier-Villareal, & Anadria (2023). However, the
#' inclusion of categorical indicators often leads to a large ordinal error,
#' which automatically initiates a final optimization step using
#' \code{\link[OpenMx:mxTryHardOrdinal]{OpenMx::mxTryHardOrdinal()}}.
#' @export
#' @keywords mixture models openmx
#' @examples
#' \dontrun{
#' if(isTRUE(requireNamespace("OpenMx", quietly = TRUE))) {
#' library(tidySEM)
#' library(OpenMx)
#' # Construct dataset with ordinal and categorical indicators
#' set.seed(1)
#' n = 200
#' mns <- c(rep(0, floor(.3*n)), rep(2, ceiling(.7*n)))
#' df <- rnorm(4*n, mean = rep(mns, 4))
#' df <- matrix(df, nrow = n)
#' df <- t(t(df) * c(1, 2, .5, 1))
#' df <- data.frame(df)
#' df$X4 <- cut(df$X4, 3, labels = FALSE)
#' df$X4 <- OpenMx::mxFactor(df$X4, levels = c(1:3))
#' # Estimate the model
#' set.seed(1)
#' res <- mx_mixed_lca(data = df, classes = 2)
#' }
#' }
#' @inherit mx_mixture references
mx_mixed_lca <- function(data = NULL,
                         classes = 1L,
                         variances = "equal",
                         covariances = "zero",
                         run = TRUE,
                         expand_grid = FALSE,
                         ...){
  if(!isTRUE(requireNamespace("OpenMx", quietly = TRUE))) {
    return(NULL)
  }
  # Identify data type of variables
  vars_cont <- sapply(data, inherits, what = "numeric")
  vars_ord <- sapply(data, inherits, what = "ordered")

  # If there are no continuous variables, or no ordinal variables, or some that are an other type
  if(!any(vars_ord) | !any(vars_cont) | any(!(vars_cont | vars_ord))) stop("Function mx_mixed_lca() only accepts data that contain both ordinal (binary or ordered categorical) and continuous (numeric) levels of measurement.")

  names_cont <- names(data)[vars_cont]
  names_ord <- names(data)[vars_ord]

  df_cont <- data[, names_cont, drop = FALSE]
  df_ord <- data[, names_ord, drop = FALSE]

  # Determine which models must be run
  models <- get_model_grid(variances = variances, covariances = covariances, classes = classes, expand_grid = expand_grid)

  if(nrow(models) > 1){
    # Lapply to each row
    out <- mapply(function(vars, covs, clss){
      # Handle continuous part of model
      mx_mixed_lca(data = data, classes = clss, variances = vars, covariances = covs, run = run, expand_grid = FALSE, ...)
    }, vars = models$variances, covs = models$covariances, clss = models$classes, SIMPLIFY = FALSE)
    out <- do.call(c, out)
    if(inherits(out, "list")){
      class(out) <- c("mixture_list", class(out))
    }
    return(out)
  } else {

    # Handle continuous part of model
    mix_profiles <- mx_mixture(model = profile_syntax(variances, covariances, names_cont),
                               classes = classes, data = df_cont, run = FALSE, run_mixture_starts = FALSE)

    # Handle categorical part of model
    mix_ord <- mx_lca(df_ord, classes = classes, run = FALSE, run_mixture_starts = FALSE)

    # Combine models
    if(classes == 1){
      # Prepare matrices

      # Prepare matrices
      mix_combined <- OpenMx::mxModel(mx_merge_matrices(mix_profiles, mix_ord),
                              name = mx_mixture_model_label(variances = variances, covariances = covariances, classes = classes),
                              OpenMx::mxFitFunctionML(),
                              OpenMx::mxData(data, type = "raw"))
      } else {
        # Make class list
        class_list <- vector(mode = "list", length = classes)
        names(class_list) <- paste0("class", 1:classes)
        for(n in names(class_list)){
          class_list[[n]] <- OpenMx::mxModel(mx_merge_matrices(mix_profiles[[n]], mix_ord[[n]]),
                                      name = n,
                                      OpenMx::mxFitFunctionML(vector = TRUE))
        }
        Args <- c(list(
          model = mx_mixture_model_label(variances = variances, covariances = covariances, classes = classes),
          OpenMx::mxData(data, type = "raw"),
          OpenMx::mxMatrix(values = mix_profiles$weights$values, nrow=1, ncol=ncol(mix_profiles$weights$values), lbound = 1e-4, free=c(FALSE, rep(TRUE, (ncol(mix_profiles$weights$values)-1L))), name="weights"),
          OpenMx::mxExpectationMixture(paste0("class", 1:classes), weights = "weights", scale="sum"),
          OpenMx::mxFitFunctionML()),
          class_list)
        mix_combined <- do.call(OpenMx::mxModel, Args)
      }

    # Check for hidden argument run_mixture_starts with TRUE as default:
    run_mixture_starts <- tryCatch({!isFALSE(list(...)[["run_mixture_starts"]])}, error = function(e){TRUE})
    if(run_mixture_starts){
      mix_combined <- mixture_starts(mix_combined)
    }
  }
  attr(mix_combined, "tidySEM") <- "mixture"
  if(run){
    mix_combined <- run_mx(mix_combined)
  }
  return(mix_combined)
}

mx_mixture_model_label <- function(variances, covariances, classes){
  # Label the models
  vlab <- paste0(c(varying = "free", equal = "equal")[variances], " var")
  clab <- paste0(c(zero = "no", varying = "free", equal = "equal")[covariances], " cov")
  clab[clab == "no cov"] <- NA
  lbs <- gsub(", $", "", paste2(vlab, clab, sep = ", "))
  paste(rep(lbs, each = length(classes)), rep(classes, length(lbs)))
}

get_model_grid <- function(variances = "equal", covariances = "zero", classes = 1, expand_grid = FALSE){
  if(expand_grid){
    return(expand.grid(variances = variances, covariances = covariances, classes = classes, stringsAsFactors = FALSE))
  }

  if(length(variances) == length(covariances)){
    return(do.call(rbind, lapply(classes, function(c){
      data.frame(variances = variances, covariances = covariances, classes = c)
    })))
  }
  out <- try({data.frame(recycle_elements(list(variances = variances, covariances = covariances, classes = classes)))}, silent = TRUE)
  if(inherits(out, what = "try-error")){
    stop(
      "The 'variances' and 'covariances' arguments must be vectors of equal length. Together, they describe the models to be run."
    )
  }
  return(out)
}

recycle_elements <- function(lst) {
  max_length <- max(sapply(lst, length))  # Find the maximum length
  recycled_list <- lapply(lst, function(x) rep(x, length.out = max_length))  # Recycle elements
  return(recycled_list)
}


mx_merge_matrices <- function(x, y){
  if(!all(c(length(x@submodels), length(y@submodels)) == 0)) stop("Model contains submodels and cannot be merged.")
  manv <- c(x$manifestVars, y$manifestVars)
  nms <- list(manv, manv)
  K <- length(manv)
  # Determine which elements
  these_matrices <- c("A", "S", "F")
  these_mats <- c("mat_dev", "mat_ones", "Indicators")
  if(any(these_mats %in% names(x@matrices) & these_mats %in% names(y@matrices))){
    stop("Cannot currently combine two models with categorical variables.")
  }
  these_mats <- these_mats[these_mats %in% names(x@matrices) | these_mats %in% names(y@matrices)]
  these_algs <- c("Thresholds")
  if(any(these_algs %in% names(x@algebras) & these_algs %in% names(y@algebras))){
    stop("Cannot currently combine two models with categorical variables.")
  }
  these_algs <- these_algs[these_algs %in% names(x@algebras) | these_algs %in% names(y@algebras)]
  mat_list <- vector(mode = "list", length = sum(c(length(these_matrices), length(these_mats), length(these_algs))))
  mat_list <- lapply(these_matrices, function(M){
    OpenMx::mxMatrix(name=M, type="Full",
                     nrow = K, ncol = K,
                     values = diag_bind(x[[M]]$values, y[[M]]$values, pad = 0),
                     labels = diag_bind(x[[M]]$labels, y[[M]]$labels, pad = NA),
                     free = diag_bind(x[[M]]$free, y[[M]]$free, pad = FALSE),
                     lbound = diag_bind(x[[M]]$lbound, y[[M]]$lbound, pad = NA),
                     ubound = diag_bind(x[[M]]$ubound, y[[M]]$ubound, pad = NA),
                     dimnames = nms)
  })
  names(mat_list) <- these_matrices

  mat_list$M <- OpenMx::mxMatrix(name = "M",
                                 type = "Full",
                                 nrow = 1,
                                 ncol = K,
                                 free = c(x$M$free, y$M$free),
                                 values = c(x$M$values, y$M$values),
                                 labels = c(x$M$labels, y$M$labels),
                                 dimnames = list(NULL, nms[[1]]))


  for(M in these_mats){
    mat_list[[M]] <- c(x[[M]], y[[M]])[[1]]
  }

  for(A in these_algs){
    mat_list[[A]] <- c(x[[A]], y[[A]])[[1]]
  }

  Args <- c(list(
    model = "combined",
    type = "RAM",
    manifestVars = manv,
    OpenMx::mxExpectationRAM("A","S","F","M")
  ), mat_list)
  mix_combined <- do.call(OpenMx::mxModel, Args)
  if("Thresholds" %in% these_algs){
    mix_combined$expectation$thresholds <- "Thresholds"
  }
  return(mix_combined)
}
