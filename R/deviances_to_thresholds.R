#' Convert deviances to thresholds
#'
#' Converts an OpenMx model in which ordinal thresholds are
#' parameterized as deviances between successive thresholds
#' to an equivalent model in which the parameters are
#' thresholds.
#' @param model An \code{\link[OpenMx]{MxModel}} containing
#' ordinal indicators whose thresholds are specified using
#' deviance parameterization, as created by
#' \code{\link{as_ram}} with
#' \code{threshold_method = "mx_deviances"}.
#' @return An \code{\link[OpenMx]{MxModel}}.
#' @seealso
#' \code{\link{as_ram}}
#' @examples
#' # Plain model
#' if(isTRUE(requireNamespace("OpenMx", quietly = TRUE))){
#' library(OpenMx)
#'
#' set.seed(1)
#' dat <- data.frame(
#'   X1 = ordered(sample(1:2, 200, replace = TRUE)),
#'   X2 = ordered(sample(1:2, 200, replace = TRUE)),
#'   X3 = ordered(sample(1:3, 200, replace = TRUE))
#' )
#'
#' mod <- as_ram(
#'   "
#'   X1 | t1
#'   X2 | t1
#'   X3 | t1
#'   X3 | t2
#'   ",
#'   data = dat,
#'   threshold_method = "mx_deviances"
#' )
#' res <- run_mx(mod)
#' coef(res)
#' res_thresholds <- deviances_to_thresholds(res)
#' coef(res_thresholds)
#'
#' # Multigroup model
#' set.seed(2)
#' dat1 <- data.frame(
#'   X1 = ordered(sample(1:2, 100, replace = TRUE)),
#'   X2 = ordered(sample(1:2, 100, replace = TRUE)),
#'   X3 = ordered(sample(1:3, 100, replace = TRUE))
#' )
#'
#' dat2 <- data.frame(
#'   X1 = ordered(sample(1:2, 100, replace = TRUE)),
#'   X2 = ordered(sample(1:2, 100, replace = TRUE)),
#'   X3 = ordered(sample(1:3, 100, replace = TRUE))
#' )
#'
#' group1 <- as_ram(
#'   "
#'   X1 | t1
#'   X2 | t1
#'   X3 | t1
#'   X3 | t2
#'   ",
#'   data = dat1,
#'   threshold_method = "mx_deviances"
#' )
#' group1 <- mxModel(group1, name = "group1")
#'
#' group2 <- as_ram(
#'   "
#'   X1 | t1
#'   X2 | t1
#'   X3 | t1
#'   X3 | t2
#'   ",
#'   data = dat2,
#'   threshold_method = "mx_deviances"
#' )
#' group2 <- mxModel(group2, name = "group2")
#'
#' multigroup <- mxModel(
#'   "multigroup",
#'   group1,
#'   group2,
#'   mxFitFunctionMultigroup(c("group1", "group2"))
#' )
#' res2 <- run_mx(multigroup)
#' coef(res2)
#' res2_thresholds <- deviances_to_thresholds(res2)
#' coef(res2_thresholds)
#' @export
deviances_to_thresholds <- function(model){
  submodels <- names(model@submodels)
  if(is.null(submodels)){
    if(!is.null(model[["mat_dev"]])){
      thresh <- mxMatrix(name = "Thresholds",
                         type = "Full",
                         nrow = nrow(model$Thresholds$result),
                         ncol = ncol(model$Thresholds$result),
                         free = model$mat_dev$free,
                         values = model$Thresholds$result
      )
      model <- mxModel(model, c("mat_dev", "mat_ones", "Thresholds","Indicators"), remove = TRUE)
      model <- mxModel(model, thresh)
    }
  } else {
    for(thismodel in submodels) {
      model[[thismodel]] <- deviances_to_thresholds(model[[thismodel]])
    }
  }
  is_basecase <- try(!as.character(sys.call(-1))[[1]] == "deviances_to_thresholds", silent = TRUE)
  if(isTRUE(is_basecase)){
    model <- OpenMx::mxRun(model, silent = TRUE)
  }
  return(model)
}
