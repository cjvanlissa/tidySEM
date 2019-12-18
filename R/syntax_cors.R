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
cors <- function(x, y = x){
  UseMethod("cors")
}

#' @method cors sem_syntax
#' @export
cors.sem_syntax <- function(x, y = x){
  out <- force(x)
  Args <- list(x = x$data_dict$name[x$data_dict$type %in% c("observed", "latent")])
  syntax <- do.call(switch(x$sem_software,
                           "lavaan" = cors_lavaan,
                           "mplus" = cors_mplus), Args)
  out$syntax <- c(out$syntax, syntax)
  out
}

#' @method cors data_dict
#' @export
cors.data_dict <- function(x, y = x){
  Args <- as.list(match.call()[-1])
  out <- list(data_dict = x,
              syntax = do.call(switch(getOption("sem_software"),
                                      "lavaan" = cors_lavaan,
                                      "mplus" = cors_mplus), list(x = x$name)),
              sem_software = getOption("sem_software"))
  class(out) <- c("sem_syntax", class(out))
  out
}

#' @method cors data.frame
#' @export
cors.data.frame <- function(x, y = x){
  Args <- as.list(match.call()[-1])
  Args$x <- do.call(dictionary, Args[1])
  do.call(cors, Args)
}

# cors_mplus(c("a", "b", "c"))
# cors_mplus(c("a", "b", "c"), c("x", "y"))
cors_mplus <- function(x, y = x){
  m <- matrix(paste0(rep(y, each = length(x)), " WITH ", rep(x, length(x)), ";"), ncol = length(x))
  m[lower.tri(m)]
}

# cors_lavaan(c("a", "b", "c"))
# cors_lavaan(c("a", "b", "c"), c("x", "y"))
#' @importFrom lavaan mplus2lavaan.modelSyntax
cors_lavaan <- function(x, y = x){
  Args <- as.list(match.call()[-1])
  out <- do.call(cors_mplus, Args)
  strsplit(mplus2lavaan.modelSyntax(paste0(out, collapse = "\n")), "\n")[[1]]
}


syntax_cor_lavaan(c("a", "b", "c"))
syntax_cor_mplus(c("a", "b", "c"))

syntax_cor_lavaan <- function(x, y = x, all = TRUE, label = TRUE){
  if(all){
    cors <- expand.grid(x, " ~~ ", y)
    if(label){
      cors <- cbind(cors, " ~~ c", "*")[, c(1, 4, 1,3,5,3)]
    }
    cors <- cors[!cors$Var1==cors$Var3, ]
    unname(apply(cors, 1, paste0, collapse = ""))
  } else {
    if(label){
      paste0(x, " ~~ c", x, y, "*", y)
    } else {
      paste0(x, " ~~ ", y)
    }
  }
}


