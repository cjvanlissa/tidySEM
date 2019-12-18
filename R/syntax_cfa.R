x <- dictionary(c("bfi_1", "bfi_2", "bfi_3", "bfi_4", "bfi_5",
"macqj_1", "macqj_2", "macqj_3", "macqj_4", "macqj_5", "macqj_6",
"macqj_7", "macqj_8", "macqj_9", "macqj_10", "macqj_11",
"macqj_12", "macqj_13", "macqj_14", "macqj_15", "macqj_16",
"macqj_17", "macqj_18", "macqj_19", "macqj_20", "macqj_21",
"macqr_1", "macqr_2", "macqr_3", "macqr_4", "macqr_5", "macqr_6",
"macqr_7", "macqr_8", "macqr_9", "macqr_10", "macqr_11",
"macqr_12", "macqr_13", "macqr_14", "macqr_15", "macqr_16",
"macqr_17", "macqr_18", "macqr_19", "macqr_20", "macqr_21", "sex"))

#tmp <- measurement(dict)

#' @title Generate syntax for a measurement model
#' @description Generate syntax for a measurement model for latent variables.
#' @param x An object for which a method exists, including \code{data_dict}
#' (generated using \code{\link[tidySEM]{dictionary}}, or \code{data.frame} (for
#' which \code{\link[tidySEM]{dictionary}} will be run first).
#' @param center Whether or not to mean-center the latent variables,
#' Default: TRUE
#' @param scale Whether to identify the model by fixing latent variable variance
#' to 1 (\code{scale = TRUE}), or by fixing the first factor loading to 1
#' (\code{scale = FALSE}).
#' @return An object of class \code{sem_syntax}.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname measurement
#' @export
measurement <- function(x, center = TRUE, scale = TRUE){
  UseMethod("measurement")
}

#' @method measurement data.frame
#' @export
measurement.data_dict <- function(x, center = TRUE, scale = TRUE){
  Args <- as.list(match.call()[-1])
  out <- do.call(switch(getOption("sem_software"),
                        "lavaan" = measurement_lavaan,
                        "mplus" = measurement_mplus), Args)
  class(out) <- c("sem_syntax", class(out))
  attr(out, "sem_software") <- getOption("sem_software")
  out
}

#' @method measurement data.frame
#' @export
measurement.data.frame <- function(x, center = TRUE, scale = TRUE){
  Args <- as.list(match.call()[-1])
  Args$x <- do.call(dictionary, Args[[1]])
  out <- do.call(switch(getOption("sem_software"),
                        "lavaan" = measurement_lavaan,
                        "mplus" = measurement_mplus), Args)
  class(out) <- c("sem_syntax", class(out))
  attr(out, "sem_software") <- getOption("sem_software")
  out
}


#measurement(dict)
# syntax_cfa_lavaan <- function(scales.list){
#   variables <- names(scales.list)
#   outlines <- do.call(c, list(sapply(variables, function(x){
#     # x <- variables[1]
#     c(paste0(x, " =~ NA*", scales.list[[x]][1], " + ",
#              paste(scales.list[[x]][-1], collapse = " + ")),
#       paste0(x, " ~~ 1*", x))
#   }, USE.NAMES = FALSE)))
#   outlines
# }

# measurement_lavaan <- function(x, center = TRUE, scale = TRUE){
#   variables <- unique(c(NA, x$scale))[-1]
#
#   outlines <- do.call(c, list(sapply(variables, function(this_var){
#     out <- paste0(this_var, " =~ NA*", x$name[which(x$scale == this_var)][1], " + ",
#              paste(x$name[which(x$scale == this_var)][-1], collapse = " + "))
#     attr(out, "type") <- "measurement"
#     attr(out, "aspect") <- "loadings"
#     if(center){
#       out <- c(out, paste0(this_var, " ~0*1", this_var))
#       attr(out[length(out)], "type") <- "measurement"
#       attr(out[length(out)], "aspect") <- "center"
#     }
#     if(scale){
#       out <- c(out, paste0(this_var, " ~~ 1*", this_var))
#       attr(out[length(out)], "type") <- "measurement"
#       attr(out[length(out)], "aspect") <- "scale"
#     }
#     out
#   }, USE.NAMES = FALSE)))
#   outlines
# }

measurement_mplus <- function(x, center = TRUE, scale = TRUE){
  variables <- unique(c(NA, x$scale))[-1]
  outlines <- unlist(sapply(variables, function(this_var){
    out <- c(paste0(this_var, " BY ", x$name[which(x$scale == this_var)][1], ifelse(scale, "*", "@1")),
                  paste0(this_var, " BY ", x$name[which(x$scale == this_var)][-1]))
    attr(out, "type") <- "measurement"
    attr(out, "aspect") <- "loadings"
    if(center){
      out <- c(out, paste0("[", this_var, "@0]"))
      attr(out[length(out)], "type") <- "measurement"
      attr(out[length(out)], "aspect") <- "center"
    }
    if(scale){
      out <- c(out, paste0(this_var, "@1"))
      attr(out[length(out)], "type") <- "measurement"
      attr(out[length(out)], "aspect") <- "scale"
    }
    paste0(out, ";")
  }, USE.NAMES = FALSE))
  outlines
}

#' @importFrom lavaan mplus2lavaan.modelSyntax
measurement_lavaan <- function(x, center = TRUE, scale = TRUE){
  Args <- as.list(match.call()[-1])
  out <- do.call(measurement_mplus, Args)
  strsplit(mplus2lavaan.modelSyntax(paste0(out, collapse = "\n")), "\n")[[1]]
}

# measurement_mplus(dict, scale = F)
# measurement_lavaan(dict, scale = T)
# tmp <- paste0(tmp, collapse = "\n")
# library(lavaan)




