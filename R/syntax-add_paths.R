#' @title Add paths to an object of class 'tidy_sem'
#' @description Add paths to an object of class \code{tidy_sem}, or replace
#' existing paths. The paths must be specified as
#' \code{\link{lavaan}{model.syntax}}, and separated by commas.
#' @param model An object of class \code{tidy_sem}.
#' @param ... Paths to add or substitute, specified in
#' \code{\link{lavaan}{model.syntax}}, and separated by commas.
#' @return An object of class \code{tidy_sem}.
#' @details Currently, only the \code{\link{lavaan}{lavaan}} commands \code{~,
#' ~~, =~,} and \code{~1} are parsed.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[lavaan]{model.syntax}}
#' @rdname add_paths
#' @export
#' @importFrom lavaan lavParseModelString
add_paths <- function(model, ...){
  # Clean dots
  dots <- list(...)
  is_char <- sapply(dots, inherits, "character")
  if(any(!is_char)){
    no_char <- lapply(sys.call()[-c(1:2)][!is_char], deparse)
    has_name <- names(no_char) == ""
    no_char[!has_name] <- paste0(names(no_char[!has_name]), "=", no_char[!has_name])
    dots[!is_char] <- no_char
  }
  is_char <- sapply(dots, inherits, "character")
  if(any(!is_char)){
    warning("Some elements of '...' could not be parsed. Try entering these commands as a character string.")
    dots <- dots[is_char]
  }
  # Parse dots
  tab <- do.call(rbind, lapply(dots, function(i){
    out <- lavParseModelString(i, as.data.frame. = TRUE)#[, 1:5, drop = FALSE]
  }))

  tab$free <- tab$fixed == ""
  tab$value <- tab$fixed
  tab$value[tab$free] <- 0
  tab <- cbind(tab, category = "user", aspect = "user")
  tab <- check_lav_tab(tab)
  mod <- model$syntax

  for(this_row in 1:nrow(tab)){
    dup <- duplicated(rbind(tab[this_row, c(1, 3)], mod[, c(1, 3)]))[-1]
    if(!any(dup)){
      mod <- rbind(mod, tab[this_row, ])
    } else {
      mod[which(dup), ] <- tab[this_row, ]
    }
  }
  if(any(duplicated(mod[, c(1, 3)]))){
    stop("Several commands specify relationships between the same pair of variables. Specifically:\n  ",
         paste0(do.call(paste, mod[duplicated(mod[, c(1, 3)]) | duplicated(mod[, c(1, 3)], fromLast = TRUE), 1:3]), collapse = "\n  ")
    )
  }
  model$syntax <- mod
  return(model)
}
