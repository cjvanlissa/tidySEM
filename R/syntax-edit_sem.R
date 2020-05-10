#' @title Add paths to an object of class 'sem_syntax'
#' @description Add paths to an object of class \code{sem_syntax}, or replace
#' existing paths. The paths must be specified as
#' \code{\link{lavaan}{model.syntax}}, and separated by commas.
#' @param model An object of class \code{sem_syntax}.
#' @param ... Paths to add or substitute, specified in
#' \code{\link{lavaan}{model.syntax}}, and separated by commas.
#' @return An object of class \code{sem_syntax}.
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
  # if(any(duplicated(tab[, c(1, 3)]))){
  #   stop("Several commands specify relationships between the same pair of variables. Specifically:\n  ",
  #        paste0(do.call(paste, tab[duplicated(tab[, c(1, 3)]) | duplicated(tab[, c(1, 3)], fromLast = TRUE), 1:3]), collapse = "\n  ")
  #        )
  # }
  tab$free <- tab$fixed == ""
  tab$value <- tab$fixed
  tab$value[tab$free] <- 0
  tab <- cbind(tab[, c("lhs", "op", "rhs", "free", "value")], category = "user", aspect = "user")
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
  # dots <- gsub("(?<=~).(?=(~|1))", "", unlist(dots), perl = TRUE)
  # dots <- gsub("(?<=(~|1))\\b", " ", unlist(dots), perl = TRUE)
  # dots <- gsub("\\b(?=(=|~|1))", " ", unlist(dots), perl = TRUE)
  # names(dots) <- NULL
  # Count operators
  # n_op <- sapply(dots, function(i)sum(gregexpr("[~=+]+",i)[[1]] > 0))
  # mult <- n_op > 1
  # lapply(dots[mult], function(i){
  #   parts <- strsplit(i, split = "[~=]+")[[1]]
  #   lhs <-
  # })
  model$syntax <- mod
  return(model)
}
