#' @title Add paths to an object of class 'tidy_sem'
#' @description Add paths to an object of class \code{tidy_sem}, or replace
#' existing paths. The paths must be specified as
#' \code{\link[lavaan]{model.syntax}}, and separated by commas.
#' @param model An object of class \code{tidy_sem}.
#' @param ... Paths to add or substitute, specified in
#' \code{\link{lavaan}{model.syntax}}, and separated by commas.
#' @param strict_check Logical, indicating whether or not to throw an error if
#' the variable names referenced in the new paths do not exist in the
#' \code{data}, or in the \code{dictionary}, or in the existing \code{syntax}
#' element. If \code{strict_check = FALSE}, the check is still performed, but
#' throws a \code{warning} instead of an \code{error}.
#' @return An object of class \code{tidy_sem}.
#' @details Currently, only the \code{\link{lavaan}{lavaan}} commands \code{~,
#' ~~, =~,} and \code{~1} are parsed.
#' @examples
#' library(lavaan)
#' df <- iris[, 1:4]
#' names(df) <- paste0("x_", 1:4)
#' model <- tidy_sem(df)
#' model <- measurement(model)
#' model <- add_paths(model, x =~ a*x_1 + b*x_2 + a*x_3 + b*x_4)
#' res <- estimate_lavaan(model)
#' summary(res)
#' @seealso
#'  \code{\link[lavaan]{model.syntax}}
#' @rdname add_paths
#' @export
#' @importFrom lavaan lavParseModelString
add_paths <- function(model, ..., strict_check = TRUE){
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
  rownames(tab) <- NULL
  mod <- model$syntax
  rownames(mod) <- NULL

# Perform checks ----------------------------------------------------------
  new_vars <- unique(c(tab$lhs, tab$rhs))
  varnames <- vector("character")
  if(has_data(model)){
    varnames <- append(varnames, names(model$data))
  }
  if(has_dictionary(model)){
    varnames <- append(varnames, model$dictionary$name)
  }
  if(has_syntax(model)){
    varnames <- varnames <- append(varnames, unique(c(model$syntax$lhs, model$syntax$rhs)))
  }
  if(any(!new_vars %in% varnames)){
    msg <- list("Specifying syntax for variables not in existing elements of 'tidy_sem' object. The offending variables are: ", paste0(new_vars[!new_vars %in% varnames], collapse = ", "), call. = FALSE)
    if(strict_check){
      do.call(stop, msg)
    } else {
      do.call(warning, msg)
    }
  }
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
