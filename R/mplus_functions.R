#' @importFrom MplusAutomation mplusObject
drop_mplusObject <- function(x){
  UseMethod("drop_mplusObject", x)
}

drop_mplusObject.mplusObject <- function(x){
  x$results
}


#' @importFrom stats dnorm
#' @importFrom ggplot2 stat_function
plot_thresholds <- function(x){
  thresholds <- x$parameters$unstandardized[x$parameters$unstandardized$paramHeader == "Thresholds", ]
  thresholds$Variable <- gsub("\\$.*$", "", thresholds$param)
  thresholds$Scale <- gsub("\\d\\$.*$", "", thresholds$param)
  thresholds$Item <- gsub("^.+?(\\d)\\$.*$", "\\1", thresholds$param)
  thresholds$Threshold <- gsub("^.+?\\$", "", thresholds$param)

  #thresholds <- thresholds[1:3, ]
  ggplot(data = data.frame(x = c(-3, 3)), aes(x = .data[["x"]])) +
    stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = 1)) +
    geom_vline(data = thresholds, mapping = aes(xintercept = .data[["est"]])) +
    facet_grid(Scale ~ Item) +
    theme_bw() +
    labs(x = NULL, y = NULL)
}

# cut_data <- function(x){
#   x <- mplus_data
#   sum(is.na(x))
#   x[x<4] <- 1
#
# }

calc_omega <- function(loadings, residuals){
  sum(loadings)^2/(sum(loadings)^2+sum(residuals))
}

get_omegas <- function(x){
  x$results$parameters$unstandardized
  get_loadings(x)
}

get_loadings <- function(x, std = "stdyx.standardized"){
  UseMethod("get_loadings", x)
}

get_loadings.mplusObject <- function(x, std = "stdyx.standardized"){
  get_loadings(drop_mplusObject(x))
}

get_loadings.mplus.model <- function(x, std = "stdyx.standardized"){
  x$parameters[[std]][grepl("\\.BY", x$parameters[[std]]$paramHeader), ]
}
