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
#' dict <- get_dictionary(c("bfi_1", "bfi_2", "bfi_3", "bfi_4", "bfi_5"))
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
  Args$x <- x
  variables <- unique(c(NA, x$scale))[-1]
  update_dict <- rbind(x, data.frame(name = variables, scale = NA, type = "latent", label = variables))
  update_dict$type[update_dict$scale %in% variables] <- "indicator"
  out <- list(dictionary = update_dict,
              syntax = do.call(measurement_table, Args))
  class(out) <- c("sem_syntax", class(out))
  out
}

measurement_table <- function(x, center = TRUE, scale = TRUE){
  variables <- unique(c(NA, x$scale))[-1]
  outlines <- lapply(variables, function(this_var){
    rbind(
      c(this_var, "=~", x$name[which(x$scale == this_var)][1], ifelse(scale, "FALSE", "TRUE"), 1, "measurement", "loadings"),
      expand.grid(this_var, "=~", x$name[which(x$scale == this_var)][-1], "TRUE", "1", "measurement", "loadings", stringsAsFactors = FALSE),
      c(this_var, "~1", "", ifelse(center, "FALSE", "TRUE"), 0, "measurement", "center"),
      c(this_var, "~~", this_var, ifelse(scale, "TRUE", "FALSE"), 1, "measurement", "scale"),
      expand.grid(x$name[which(x$scale == this_var)], "~1", "", ifelse(center, "TRUE", "FALSE"), 0, "measurement", "item_intercepts", stringsAsFactors = FALSE),
      data.frame(Var1 = x$name[which(x$scale == this_var)], Var2 = "~~", Var3 = x$name[which(x$scale == this_var)], Var4 = "TRUE", Var5 = 1, Var6 = "measurement", Var7 = "residual_variances", stringsAsFactors = FALSE), stringsAsFactors = FALSE)
  })
  if(length(variables) > 1){
    outlines <- c(outlines,
                  list(setNames(cors_table(variables), paste0("Var", 1:7))))
  }
  outlines <- do.call(rbind, outlines)
  names(outlines) <- c("lhs", "op", "rhs", "free", "value", "category", "aspect")
  outlines
}


measurement.data_dict2 <- function(x, center = TRUE, scale = TRUE){
  Args <- as.list(match.call()[-1])
  x <- force(x)
  variables <- unique(c(NA, x$scale))[-1]
  update_dict <- rbind(x, data.frame(name = variables, scale = NA, item = NA, type = "latent", label = variables))
  update_dict$type[update_dict$scale %in% variables] <- "indicator"
  out <- list(dictionary = update_dict,
              syntax = do.call(measurement_lavaan, list(x = x)))
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
