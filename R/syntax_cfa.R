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
#' dict <- dictionary(c("bfi_1", "bfi_2", "bfi_3", "bfi_4", "bfi_5"))
#' measurement(dict)
#' @rdname measurement
#' @export
measurement <- function(x, center = TRUE, scale = TRUE){
  UseMethod("measurement")
}

#' @method measurement data_dict
#' @export
measurement.data_dict <- function(x, center = TRUE, scale = TRUE){
  Args <- as.list(match.call()[-1])
  x <- force(x)
  variables <- unique(c(NA, x$scale))[-1]
  update_dict <- rbind(x, data.frame(name = variables, scale = NA, item = NA, type = "latent", label = variables))
  update_dict$type[update_dict$scale %in% variables] <- "indicator"
  out <- list(data_dict = update_dict,
              syntax = do.call(switch(getOption("sem_software"),
                                      "lavaan" = measurement_lavaan,
                                      "mplus" = measurement_mplus), list(x = x)),
              sem_software = getOption("sem_software"))
  class(out) <- c("sem_syntax", class(out))
  out
}

#' @method measurement data.frame
#' @export
measurement.data.frame <- function(x, center = TRUE, scale = TRUE){
  Args <- as.list(match.call()[-1])
  Args$x <- do.call(dictionary, Args[1])
  do.call(measurement, Args)
}

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

