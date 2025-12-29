#' Estimate mixed data latent class analysis using OpenMx
#'
#' This function simplifies the specification of latent class models with mixed
#' data types:
#' models that estimate membership of a categorical latent variable based on
#' binary/ordinal and continuous indicators. See Details for more information.
#' @param data The data.frame to be used for model fitting.
#' @param classes A vector of integers, indicating which class solutions to
#' generate. Defaults to 1L. E.g., \code{classes = 1:6},
#' @param run Logical, whether or not to run the model. This should usually be
#' set to `TRUE`, because this function runs several models to aid in specifying
#' good starting values; see Details.
#' @param ... Additional arguments, passed to functions.
#' @return Returns an \code{\link[OpenMx:mxModel]{OpenMx::mxModel()}}.
#' @details
#' The procedure is as follows:
#'
#' 1. Estimate a latent profile analysis for the continuous indicators using
#'    \code{\link[tidySEM:mx_profiles]{mx_profiles()}}. Additional arguments, like
#'    `variabces = "free"`, can be passed via `...`. The estimator uses
#'    simulated annealing.
#' 2. To obtain good starting values for the categorical indicators, use the
#'    classes from step 1. to estimate an auxiliary model for the ordinal
#'    indicators with \code{\link[tidySEM:BCH]{BCH()}}.
#' 3. Estimate a latent class analysis for the categorical indicators using
#'    \code{\link[tidySEM:mx_lca]{mx_lca()}}, with the results of step 2. as starting
#'    values. The estimator uses
#'    \code{\link[OpenMx:mxTryHardOrdinal]{OpenMx::mxTryHardOrdinal()}}.
#' 4. Combine the models from steps 1. and 3. into one joint model. Conduct one
#'    final optimization step using
#'    \code{\link[OpenMx:mxTryHardOrdinal]{OpenMx::mxTryHardOrdinal()}}.
#' @export
#' @keywords mixture models openmx
#' @references Van Lissa, C. J., Garnier-Villarreal, M., & Anadria, D. (2023).
#' Recommended Practices in Latent Class Analysis using the Open-Source
#' R-Package tidySEM. Structural Equation Modeling.
#' \doi{10.1080/10705511.2023.2250920}
#' @examples
#' \dontrun{
#' if(isTRUE(requireNamespace("OpenMx", quietly = TRUE))) {
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
#' res <- mx_mixed_lca(data = df, classes = 2)
#' }
#' }
mx_mixed_lca <- function(data = NULL,
                         classes = 1L,
                         run = TRUE,
                         ...){
  if(!isTRUE(requireNamespace("OpenMx", quietly = TRUE))) {
    return(NULL)
  }
  vars_cont <- sapply(data, inherits, what = "numeric")
  vars_ord <- sapply(data, inherits, what = "ordered")
  # If there are no continuous variables, or no ordinal variables, or some that are an other type
  if(!any(vars_ord) | !any(vars_cont) | any(!(vars_cont | vars_ord))) stop("Function mx_mixed_lca() only accepts data that contain both ordinal (binary or ordered categorical) and continuous (numeric) levels of measurement.")

  cl <- match.call()
  # Recursive function
  if(length(classes) > 1){
    out <- lapply(classes, function(i){
      cl[["classes"]] <- i
      cl[[1L]] <- str2lang("tidySEM::mx_mixed_lca")
      eval.parent(cl)
    })
    attr(out, "tidySEM") <- "list"
    class(out) <- c("mixture_list", class(out))
    return(out)
  } else {
    dots <- list(...)

    # One class model
    # First, get LPA for continuous variables
    Args_profiles <- c(list(
      data = data[, vars_cont, drop = FALSE],
      classes = classes,
      run = run),
      dots[which(names(dots) %in% c("variances", "covariances", "expand_grid"))])

    mix_profiles <- do.call(mx_profiles, Args_profiles)

    # Get starting values for ordinal variables
    df_ord <- data[, vars_ord, drop = FALSE]
    mix_ord <- mx_lca(df_ord, classes = classes, run = FALSE)
    if(classes == 1){
      # Prepare matrices
      manv <- c(mix_profiles$manifestVars, mix_ord$manifestVars)
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
      thresh <- mx_thresholds(df_ord)
      mx_mdl_ord <- OpenMx::mxModel(
        model = "ordinal_starts",
        type = "RAM",
        manifestVars = names(df_ord),
        OpenMx::mxPath(from = "one", to = names(df_ord), free = FALSE, values = 0),
        OpenMx::mxPath(from = names(df_ord), to = names(df_ord), free = FALSE, values = 1, arrows = 2),
        thresh)
      mx_mdl_ord$expectation$thresholds <- "Thresholds"
      ord_strts <- suppressWarnings(tidySEM::BCH(x = mix_profiles, model = mx_mdl_ord, data = df_ord))


      classnams <- names(mix_ord@submodels)
      for(i in 1:classes){
        # Restore deviations from thresholds via the inverse of the indicator matrix
        # solve(mix_ord$class1$mat_ones$values) %*% (mix_ord$class1$mat_ones$values %*% mix_ord$class1$mat_dev$values)
        # mix_ord[[classnams[i]]]$mat_dev$values <- solve(mix_ord[[classnams[i]]]$mat_ones$values) %*% ord_strts[[classnams[i]]]$Thresholds$values
        mix_ord[[classnams[i]]]$mat_dev$values <- ord_strts[[classnams[i]]]$mat_dev$values
      }

      # Prepare matrices
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
    if(run){
      out <- OpenMx::mxTryHardOrdinal(mix_combined)
      attr(out, "tidySEM") <- c(attr(out, "tidySEM"), "mixture")
      return(out)
    } else {
      out
    }
  }
}
