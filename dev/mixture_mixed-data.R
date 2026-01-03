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
      mx_mixed_lca(data = data, classes = clss, variances = vars, covariances = covs, expand_grid = FALSE)
    }, vars = models$variances, covs = models$covariances, clss = models$classes, SIMPLIFY = FALSE)
    out <- do.call(c, out)
    if(inherits(out, "list")){
      class(out) <- c("mixture_list", class(out))
    }
    return(out)
  } else {

    # Handle continuous part of model
    cl <- match.call()
    cl[[1L]] <- str2lang("tidySEM:::mx_mixture")
    if("variances" %in% names(cl)) cl[["variances"]] <- NULL
    if("covariances" %in% names(cl)) cl[["covariances"]] <- NULL
    cl[["data"]] <- df_cont
    cl[["run_mixture_starts"]] <- FALSE
    cl[["run"]] <- FALSE
    cl[["model"]] <- profile_syntax(variances, covariances, names_cont)
    mix_profiles <- eval.parent(cl)

    # Handle categorical part of model
    mix_ord <- mx_lca(df_ord, classes = classes, run = FALSE, run_mixture_starts = FALSE)

    # Combine models
    if(classes == 1){
      # Prepare matrices
      manv <- c(names_cont, names_ord)
      nms <- list(manv, manv)
      mat_a <- OpenMx::mxMatrix(name="A", type="Full", nrow = ncol(data), ncol = ncol(data),
                                free = FALSE, values = 0, dimnames = nms)
      mat_s <- OpenMx::mxMatrix(name = "S",
                                type = "Diag",
                                nrow = nrow(mix_profiles$S) + nrow(mix_ord$S),
                                ncol = ncol(mix_profiles$S) + ncol(mix_ord$S),
                                free = c(diag(mix_profiles$S$free), diag(mix_ord$S$free)),
                                values = c(diag(mix_profiles$S$values), diag(mix_ord$S$values)),
                                labels = c(diag(mix_profiles$S$labels), diag(mix_ord$S$labels)),
                                dimnames = nms)

      mat_f <- OpenMx::mxMatrix(name="F", type="Diag", nrow = ncol(data),
                                free = FALSE, values = 1, dimnames = nms)
      mat_m <- OpenMx::mxMatrix(name = "M",
                                type = "Full",
                                nrow = 1,
                                ncol = ncol(mix_profiles$M) + ncol(mix_ord$M),
                                free = c(mix_profiles$M$free, mix_ord$M$free),
                                values = c(mix_profiles$M$values, mix_ord$M$values),
                                labels = c(mix_profiles$M$labels, mix_ord$M$labels),
                                dimnames = list(NULL, nms[[1]]))

      # Make class list
      mix_combined <- OpenMx::mxModel(
        model = "mix1",
        type = "RAM",
        manifestVars = manv,
        mat_a,
        mat_s,
        mat_f,
        mat_m,
        mix_ord$mat_dev,
        mix_ord$mat_ones,
        mix_ord$Indicators,
        mix_ord$Thresholds,
        OpenMx::mxExpectationRAM("A","S","F","M"),
        OpenMx::mxFitFunctionML(),
        OpenMx::mxData(data[, manv, drop = FALSE], type = "raw")
      )
      mix_combined$expectation$thresholds <- "Thresholds"

    } else {
      # Prepare matrices
      mx_merge_models <- function(x, y){
        manv <- c(mix_profiles$class1$manifestVars, mix_ord$class1$manifestVars)
        nms <- list(manv, manv)
        mat_a <- OpenMx::mxMatrix(name="A", type="Full", nrow = ncol(data), ncol = ncol(data),
                                  free = FALSE, values = 0, dimnames = nms)
        mat_s <- OpenMx::mxMatrix(name = "S",
                                  type = "Diag",
                                  nrow = nrow(mix_profiles$class1$S) + nrow(mix_ord$class1$S),
                                  ncol = ncol(mix_profiles$class1$S) + ncol(mix_ord$class1$S),
                                  free = c(diag(mix_profiles$class1$S$free), diag(mix_ord$class1$S$free)),
                                  values = c(diag(mix_profiles$class1$S$values), diag(mix_ord$class1$S$values)),
                                  labels = c(diag(mix_profiles$class1$S$labels), diag(mix_ord$class1$S$labels)),
                                  dimnames = nms)

        mat_f <- OpenMx::mxMatrix(name="F", type="Diag", nrow = ncol(data),
                                  free = FALSE, values = 1, dimnames = nms)
        mat_m <- OpenMx::mxMatrix(name = "M",
                                  type = "Full",
                                  nrow = 1,
                                  ncol = ncol(mix_profiles$class1$M) + ncol(mix_ord$class1$M),
                                  free = c(mix_profiles$class1$M$free, mix_ord$class1$M$free),
                                  values = c(mix_profiles$class1$M$values, mix_ord$class1$M$values),
                                  labels = c(mix_profiles$class1$M$labels, mix_ord$class1$M$labels),
                                  dimnames = list(NULL, nms[[1]]))

        # Make class list
        c1 <- OpenMx::mxModel(model = "class1",
                              type = "RAM",
                              manifestVars = manv,
                              mat_a,
                              mat_s,
                              mat_f,
                              mat_m,
                              mix_ord$class1$mat_dev,
                              mix_ord$class1$mat_ones,
                              mix_ord$class1$Indicators,
                              mix_ord$class1$Thresholds,
                              OpenMx::mxExpectationRAM("A","S","F","M"),
                              OpenMx::mxFitFunctionML(vector=TRUE))
        c1$expectation$thresholds <- "Thresholds"
      }
      manv <- c(mix_profiles$class1$manifestVars, mix_ord$class1$manifestVars)
      nms <- list(manv, manv)
      mat_a <- OpenMx::mxMatrix(name="A", type="Full", nrow = ncol(data), ncol = ncol(data),
                                free = FALSE, values = 0, dimnames = nms)
      mat_s <- OpenMx::mxMatrix(name = "S",
                                type = "Diag",
                                nrow = nrow(mix_profiles$class1$S) + nrow(mix_ord$class1$S),
                                ncol = ncol(mix_profiles$class1$S) + ncol(mix_ord$class1$S),
                                free = c(diag(mix_profiles$class1$S$free), diag(mix_ord$class1$S$free)),
                                values = c(diag(mix_profiles$class1$S$values), diag(mix_ord$class1$S$values)),
                                labels = c(diag(mix_profiles$class1$S$labels), diag(mix_ord$class1$S$labels)),
                                dimnames = nms)

      mat_f <- OpenMx::mxMatrix(name="F", type="Diag", nrow = ncol(data),
                                free = FALSE, values = 1, dimnames = nms)
      mat_m <- OpenMx::mxMatrix(name = "M",
                                type = "Full",
                                nrow = 1,
                                ncol = ncol(mix_profiles$class1$M) + ncol(mix_ord$class1$M),
                                free = c(mix_profiles$class1$M$free, mix_ord$class1$M$free),
                                values = c(mix_profiles$class1$M$values, mix_ord$class1$M$values),
                                labels = c(mix_profiles$class1$M$labels, mix_ord$class1$M$labels),
                                dimnames = list(NULL, nms[[1]]))

      # Make class list
      c1 <- OpenMx::mxModel(model = "class1",
                            type = "RAM",
                            manifestVars = manv,
                            mat_a,
                            mat_s,
                            mat_f,
                            mat_m,
                            mix_ord$class1$mat_dev,
                            mix_ord$class1$mat_ones,
                            mix_ord$class1$Indicators,
                            mix_ord$class1$Thresholds,
                            OpenMx::mxExpectationRAM("A","S","F","M"),
                            OpenMx::mxFitFunctionML(vector=TRUE))
      c1$expectation$thresholds <- "Thresholds"
      class_list <- vector(mode = "list", length = classes)
      class_list[[1]] <- c1
      if(classes > 1){
        for(i in 2:classes){
          theclass <- paste0("class", i)
          class_list[[i]] <- OpenMx::mxModel(c1, name = theclass)
          class_list[[i]]$S$values <- diag_bind(mix_profiles[[theclass]]$S$values, mix_ord[[theclass]]$S$values, pad = 0)
          class_list[[i]]$S$free <- diag_bind(mix_profiles[[theclass]]$S$free, mix_ord[[theclass]]$S$free, pad = FALSE)
          class_list[[i]]$S$labels = diag_bind(mix_profiles[[theclass]]$S$labels, mix_ord[[theclass]]$S$labels, pad = NA)

          class_list[[i]]$M$values <- cbind(mix_profiles[[theclass]]$M$values, mix_ord[[theclass]]$M$values)
          class_list[[i]]$M$free <- cbind(mix_profiles[[theclass]]$M$free, mix_ord[[theclass]]$M$free)
          class_list[[i]]$M$labels <- cbind(mix_profiles[[theclass]]$M$labels, mix_ord[[theclass]]$M$labels)
          class_list[[i]]$mat_dev$values <- mix_ord[[theclass]]$mat_dev$values
        }
      }
      Args <- c(list(
        model = "mix",
        OpenMx::mxData(data[, manv, drop = FALSE], type = "raw"),
        OpenMx::mxMatrix(values = mix_profiles$weights$values, nrow=1, ncol=ncol(mix_profiles$weights$values), lbound = 1e-4, free=c(FALSE, rep(TRUE, (ncol(mix_profiles$weights$values)-1L))), name="weights"),
        OpenMx::mxExpectationMixture(paste0("class", 1:classes), weights = "weights", scale="sum"),
        OpenMx::mxFitFunctionML()
      ),
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

  # Label the models
  vlab <- paste0(c(varying = "free", equal = "equal")[variances], " var")
  clab <- paste0(c(zero = "no", varying = "free", equal = "equal")[covariances], " cov")
  clab[clab == "no cov"] <- NA
  lbs <- gsub(", $", "", paste2(vlab, clab, sep = ", "))
  lbs <- paste(rep(lbs, each = length(classes)), rep(classes, length(lbs)))
  return(OpenMx::mxModel(mix_combined, name = lbs))
  # out <- list(OpenMx::mxModel(mix_combined, name = lbs))
  # names(out) <- lbs
  # class(out) <- c("mixture_list", class(out))
  # return(out)
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
