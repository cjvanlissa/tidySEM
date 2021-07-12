#' @method print tidy_sem
#' @export
print.tidy_sem <- function(x, ...){
  has_element <- sapply(c("dictionary", "data", "syntax"), function(i){
    c("\033[0;37mo  \033[0m  ", "\033[0;32mv  \033[0m  ")[(!is.null(x[[i]]))+1]
    })
  cat("A tidy_sem object\n")
  colmsg(has_element["dictionary"], "$dictionary")
  colmsg(has_element["data"], "$data")
  colmsg(success = has_element["syntax"], "$syntax")
  #if(!is.null(x[["data"]])) msg <- paste0(msg, " ")
  #has_code <- !is.null(x[["syntax"]])
  #cat(msg)
  #print(x[["dictionary"]])
}

#' @method print sem_graph
#' @export
print.sem_graph <- function(x, ...){
  cat("A tidy_sem object with", nrow(x$edges), "edges and", nrow(x$nodes), "nodes, and the following elements:\n")
  elms_tab(x, c("edges", "nodes", "rect_width", "rect_height", "ellipses_width",
             "ellipses_height", "variance_diameter", "spacing_x", "spacing_y",
             "text_size", "curvature", "angle", "fix_coord"))
}

colmsg <- function(..., col = 30) {
  txt <- do.call(paste0, list(...))
  cat(paste0("\033[0;",
             col,
             "m",
             txt,
             "\033[0m",
             "\n"
             ))
}

elms_tab <- function(x, elms = NULL, cols = 2){
  elms <- c("edges", "nodes", "rect_width", "rect_height", "ellipses_width",
            "ellipses_height", "variance_diameter", "spacing_x", "spacing_y",
            "text_size", "curvature", "angle", "fix_coord")
  has_element <- sapply(seq_along(elms), function(i){
    paste0(c("\033[0;37mo  \033[0m", "\033[0;32mv  \033[0m")[(!is.null(x[[elms[i]]]))+1], elms[i])
  })
  add_nas <- rep("", length(has_element) %% cols)
  out <- matrix(c(has_element, add_nas), ncol = cols, byrow = TRUE)
  out[, -ncol(out)] <- apply(out[, -ncol(out), drop = FALSE], 2, function(this_col){
    sprintf(paste0("%-", max(nchar(this_col)), "s"), this_col)
  })
  out[, ncol(out)] <- paste0(out[, ncol(out)], "\n")
  invisible(apply(out, 1, cat))
}

# @method print mplusObject
# @export
# print.mplusObject <- function(x, ...){
#   if(!is.null(x[["summary"]][["ChiSqM_DF"]]))
#   print(summary(x))
# }

# @method print mplus.model
# @export
#print.mplus.model <- print.mplusObject

# @method summary mplus.model
# @export
# summary.mplus.model <- function(object, verbose = FALSE, ...)
# {
#   stopifnot(!is.null(object$summaries))
#   Args <- as.list(match.call()[-1])
#   Args$object <- list(results = object)
#   class(Args$object) <- c("mplusObject", class(Args$object))
#   do.call(summary, Args)
# }


#' @method print tidy_scales
#' @export
#' @importFrom utils head
print.tidy_scales <- function(x, ...){
  has_element <- sapply(c("descriptives", "correlations", "scores"), function(i){
    c("\033[0;37mo  \033[0m  ", "\033[0;32mv  \033[0m  ")[(!is.null(x[[i]]))+1]
  })
  cat("A tidy_scales object\n")
  colmsg(has_element["descriptives"], "$descriptives")
  colmsg(has_element["correlations"], "$correlations")
  colmsg(success = has_element["scores"], "$scores")
  cat("\n")
  tmp <- head(x$descriptives, 5)
  print(tmp)
}
