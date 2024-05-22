check_lav_tab <- function(x, lav_names = c("lhs", "op", "rhs", "free", "value", "label", tidy_sem2 = "category", tidy_sem3 = "aspect")){
  #lav_names <- c("lhs", "op", "rhs", "mod.idx", "block", "fixed", "start", "lower", "upper", "label", "prior", "efa", "category", "aspect", tidy_sem1 = "category", tidy_sem2 = "aspect")
  missing_names <- lav_names[!lav_names %in% names(x)]
  for(this_name in missing_names){
    x[[this_name]] <- ""
  }
  x[, lav_names]
}

#' @title Generate syntax for a measurement model
#' @description Generate syntax for a measurement model for latent variables.
#' This function relies on \code{\link{add_paths}} to generate syntax.
#' @param x An object for which a method exists, including \code{tidy_sem}
#' (generated using \code{\link[tidySEM]{dictionary}}, or \code{data.frame} (for
#' which \code{\link[tidySEM]{dictionary}} will be run first).
# @param center Whether or not to mean-center the latent variables,
# Default: TRUE
# @param scale Whether to identify the model by fixing latent variable variance
# to 1 (\code{scale = TRUE}), or by fixing the first factor loading to 1
# (\code{scale = FALSE}).
#' @param ... Additional parameters passed to \code{\link{add_paths}}.
#' @return An object of class \code{tidy_sem}.
#' @examples
#' dict <- tidy_sem(c("bfi_1", "bfi_2", "bfi_3", "bfi_4", "bfi_5"))
#' measurement(dict)
#' @rdname measurement
#' @export
measurement <- function(x, ...){
  UseMethod("measurement")
}

#' @method measurement tidy_sem
#' @export
measurement.tidy_sem <- function(x, groups = NULL, ...){
  out <- x
  Args_lav <- lav_from_dots(...)
  variables <- unique(c(NA, x$dictionary$scale))[-1]
  update_dict <- rbind(x$dictionary, data.frame(name = variables, scale = NA, type = "latent", label = variables))
  update_dict$type[update_dict$scale %in% variables] <- "indicator"
  out$dictionary <- update_dict
  # Add groups
  if(!is.null(groups)){
    group_var(out) <- groups
    Args_lav$ngroups <- group_var(out, "ngroups")
  }
  out$syntax <- do.call(add_paths, c(list(model = NULL,
                                          do.call(syntax_measurement, list(x = x))),
                                     Args_lav))
  if(!inherits(out, "tidy_sem")) class(out) <- c("tidy_sem", class(out))
  out
}

syntax_measurement <- function(x, ...){
  UseMethod("syntax_measurement", x)
}

#' @export
syntax_measurement.tidy_sem <- function(x, ...){
  if(!has_dictionary(x)) stop("Dictionary required to create measurement model syntax.", call. = FALSE)
  variables <- unique(c(NA, x$dictionary$scale))[-1]
  indicators <- lapply(variables, function(this_var) x$dictionary$name[which(x$dictionary$scale == this_var)])
  paste0(paste0(variables, " =~ ", sapply(indicators, paste0, collapse = " + ")), collapse = "; ")
}

measurement_table <- function(x, center = TRUE, scale = TRUE){
  #                   "op", "free",     "value", "category",    "element"
  .load_frst_fix <- c("=~", "FALSE",    "1",     "measurement", "loadings")
  .load_frst_fre <- c("=~", "TRUE",     "",      "measurement", "loadings")
  .load_othr_fre <- c("=~", "TRUE",     "",      "measurement", "loadings")
  .mean_zero <-     c("~1", "FALSE",    "0",     "measurement", "mean")
  .mean_free <-     c("~1", "TRUE",     "",      "measurement", "mean")
  .inte_zero <-     c("~1", "FALSE",    "0",     "measurement", "intercept")
  .inte_free <-     c("~1", "TRUE",     "",      "measurement", "intercept")
  .vari_one <-      c("~~", "FALSE",    "1",     "measurement", "variance")
  .vari_free <-     c("~~", "TRUE",     "",      "measurement", "variance")
  .vari_item <-     c("~~", "TRUE",     "",      "measurement", "variance_residual")

  variables <- unique(c(NA, x$scale))[-1]
  outlines <- vector("list", length = length(variables))
  for(i in seq_along(variables)){
    this_var <- variables[i]
    indicators <- x$name[which(x$scale == this_var)]
    outlines[[i]] <- rbind(
      # First loading
      c(this_var, indicators[1], if(scale){.load_frst_fre}else{.load_frst_fix}),
      # Other loadings
      expand.grid(c(list(this_var, indicators[-1]), as.list(.load_othr_fre)),
                  stringsAsFactors = FALSE),
      # Item intercepts
      expand.grid(c(list(indicators, ""), as.list(if(!center){.inte_zero}else{.inte_free})), stringsAsFactors = FALSE),
      # Mean
      c(this_var, "", if(center){.mean_zero}else{.mean_free}),
      # Item variances
      data.frame(Var1 = indicators, Var2 = indicators, Var3 = "~~", Var4 = "TRUE", Var5 = 0, Var6 = "measurement", Var7 = "variance_residual", stringsAsFactors = FALSE),
      # LV variance
      c(this_var, this_var, if(scale){.vari_one}else{.vari_free}),
      # Finish rbind
      stringsAsFactors = FALSE)[, c(1, 3, 2, 4:7)]
  }
  outlines <- do.call(rbind, outlines)
  names(outlines) <- c("lhs", "op", "rhs", "free", "value", "category", "aspect")
  outlines <- check_lav_tab(outlines)
  if(length(variables) > 1){
    outlines <- rbind(outlines,
                      cors_table(variables))
  }
  outlines
}


# measurement.tidy_sem2 <- function(x, center = TRUE, scale = TRUE){
#   Args <- as.list(match.call()[-1])
#   x <- force(x)
#   variables <- unique(c(NA, x$scale))[-1]
#   update_dict <- rbind(x, data.frame(name = variables, scale = NA, item = NA, type = "latent", label = variables))
#   update_dict$type[update_dict$scale %in% variables] <- "indicator"
#   out <- c(dictionary = update_dict,
#            list(syntax = do.call(measurement_lavaan, list(x = x))))
#   class(out) <- c("tidy_sem", class(out))
#   out
# }

#' @method measurement data.frame
#' @export
measurement.data.frame <- function(x, ...){
  Args <- as.list(match.call()[-1])
  out <- list(
    dictionary = do.call(tidy_sem, Args[1]),
    data = x
  )
  Args$x <- out$dictionary
  out$syntax <- do.call(measurement, Args)

}

# measurement_mplus <- function(x, center = TRUE, scale = TRUE){
#   variables <- unique(c(NA, x$scale))[-1]
#   outlines <- unlist(sapply(variables, function(this_var){
#     out <- c(paste0(this_var, " BY ", x$name[which(x$scale == this_var)][1], ifelse(scale, "*", "@1")),
#                   paste0(this_var, " BY ", x$name[which(x$scale == this_var)][-1]))
#     attr(out, "type") <- "measurement"
#     attr(out, "aspect") <- "loadings"
#     if(center){
#       out <- c(out, paste0("[", this_var, "@0]"))
#       attr(out[length(out)], "type") <- "measurement"
#       attr(out[length(out)], "aspect") <- "center"
#     }
#     if(scale){
#       out <- c(out, paste0(this_var, "@1"))
#       attr(out[length(out)], "type") <- "measurement"
#       attr(out[length(out)], "aspect") <- "scale"
#     }
#     paste0(out, ";")
#   }, USE.NAMES = FALSE))
#   outlines
# }
#
# # @importFrom lavaan mplus2lavaan.modelSyntax
# measurement_lavaan <- function(x, center = TRUE, scale = TRUE){
#   Args <- as.list(match.call()[-1])
#   out <- do.call(measurement_mplus, Args)
#   strsplit(mplus2lavaan.modelSyntax(paste0(out, collapse = "\n")), "\n")[[1]]
# }

group_var <- function(x, value = "name", ...){
  UseMethod("group_var", x)
}

#' @export
group_var.tidy_sem <- function(x, value = "name", ...){
  groupvar <- x$dictionary$name[which(x$dictionary$type == "group")]
  if(length(groupvar) == 0) return(NULL)
  if(length(groupvar) > 1) stop("More than one variable in the data dictionary is declared as a grouping variable.")
  if(!groupvar %in% names(x$data)) stop("Grouping variable from the data dictionary does not exist in the data.")
  switch(value,
         "name" = {
           groupvar
         },
         "values" = {
           as.character(unique(x$data[[groupvar]]))
         },
         "ngroups" = {
           length(unique(x$data[[groupvar]]))
         })
}

`group_var<-` <- function(x, value = NULL){
  UseMethod("group_var<-", x)
}

#' @export
`group_var<-.tidy_sem` <- function(x, value = NULL){
  if(is.null(value)){
    x$dictionary[["type"]][which(x$dictionary[["type"]] == "group")] <- "observed"
  } else {
    if(isTRUE(any(x$dictionary[["type"]] == "group"))){
      oldgroup <- x$dictionary[["name"]][which(x$dictionary[["type"]] == "group")]
      x$dictionary[["type"]][which(x$dictionary[["type"]] == "group")] <- "observed"
      x$dictionary[["type"]][which(x$dictionary[["name"]] == value)] <- "group"
      message("Existing grouping variable '", oldgroup, "' replaced by '", value, "'.")
    } else {
      x$dictionary[["type"]][which(x$dictionary[["name"]] == value)] <- "group"
    }
  }
  return(x)
}
