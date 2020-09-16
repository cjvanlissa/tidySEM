# @title Longest common substring
# @description Extract the longest common substring of two strings using
# base R.
# @param x First atomic character vector.
# @param y Second atomic character vector.
# @return Character string
# @examples
# lcs("test1", "test2")
# @rdname lcs
# @keywords mplus utilities
# @export
# @importFrom utils adist
# lcs <- function(x,y) {
#   matches <- gregexpr("M+", drop(attr(adist(x, y, counts=TRUE), "trafos")))[[1]];
#   lengths <- attr(matches, 'match.length')
#   which_longest <- which.max(lengths)
#   index_longest <- matches[which_longest]
#   length_longest <- lengths[which_longest]
#   substring(c(x,y)[which.max(nchar(c(x,y)))], index_longest , index_longest + length_longest - 1)
# }
#' @importFrom MplusAutomation mplusAvailable
has_mplus <- function(verbose = FALSE){
  hasmplus <- mplusAvailable(TRUE) == 0
  if(verbose){
    if(hasmplus){
      cat("\033[0;32mv  \033[0m")
      colmsg("Mplus is available.")
    } else {
      cat("\033[0;31mX  \033[0m")
      colmsg("Mplus is not available.")
    }
  }
  invisible(hasmplus)
}

has_syntax <- function(x){
  if(is.null(x[["syntax"]])){
    cat("\033[0;31mX  \033[0m")
    colmsg("Object of class 'tidy_sem' has no syntax element.")
    return(FALSE)
  } else {
    return(TRUE)
  }
}

has_dictionary <- function(x){
  if(is.null(x[["dictionary"]])){
    cat("\033[0;31mX  \033[0m")
    colmsg("Object of class 'tidy_sem' has no dictionary element.")
    return(FALSE)
  } else {
    return(TRUE)
  }
}

has_data <- function(x){
  if(is.null(x[["data"]])){
    cat("\033[0;31mX  \033[0m")
    colmsg("Object of class 'tidy_sem' has no data element.")
    return(FALSE)
  } else {
    return(TRUE)
  }
}

#' @title Expand abbreviated Mplus variable names
#' @description Expand the Mplus syntax for abbreviating lists of variable
#' names.
#' @param x Atomic character string containing the variable names section of an
#' Mplus syntax file.
#' @return Character vector of names.
#' @examples
#' mplus_expand_names("test1-test12")
#' @rdname mplus_expand_names
#' @keywords mplus utilities
#' @export
mplus_expand_names <- function(x){
  vnames <- strsplit(x, split = "(\\s+|\\n)")[[1]]
  vnames <- gsub(";", "", vnames)
  expand_these <- grepl("-", vnames)
  exp_nam <- vnames[expand_these]
  exp_nam <- lapply(exp_nam, function(i){
    components <- strsplit(i, "-")[[1]]
    name_stub <- gsub("\\d+$", "", components)
    if(!name_stub[1] == name_stub[2]){
      stop("Could not identify name stub for line ", i)
    } else {
      name_stub <- name_stub[1]
    }
    nums <- as.numeric(gsub(name_stub, "", components))
    paste0(name_stub, seq.int(nums[1], nums[2]))
  })
  vnames <- as.list(vnames)
  vnames[expand_these] <- exp_nam
  unlist(vnames)
}

is_cor <- function(x){
  !(x$lhs == x$rhs) & x$op == "~~"
}

is_var <- function(x){
  (x$lhs == x$rhs) & x$op == "~~"
}


# all_args <- function(){
#   Args <- as.list(match.call())[-1]
#   myfor <- formals(prepare_graph.default)
#   for ( v in names(myfor)){
#     if (!(v %in% names(Args)))
#       Args <- append(Args,myfor[v])
#   }
# }

all_args <- function(orig_values = FALSE, ...) {
  # Perhaps ... must be removed altogether, see https://github.com/HenrikBengtsson/future/issues/13
  # get formals for parent function
  parent_formals <- formals(sys.function(sys.parent(n = 1)))

  # Get names of implied arguments
  fnames <- names(parent_formals)

  # Remove '...' from list of parameter names if it exists
  fnames <- fnames[-which(fnames == '...')]

  # Get currently set values for named variables in the parent frame
  args <- evalq(as.list(environment()), envir = parent.frame())

  # Get the list of variables defined in '...'
  # CJ: This needs to be fixed to work with nested function calls
  args <- c(args[fnames], evalq(list(...), envir = parent.frame()))


  if(orig_values) {
    # get default values
    defargs <- as.list(parent_formals)
    defargs <- defargs[unlist(lapply(defargs, FUN = function(x) class(x) != "name"))]
    args[names(defargs)] <- defargs
    setargs <- evalq(as.list(match.call())[-1], envir = parent.frame())
    args[names(setargs)] <- setargs
  }
  return(args)
}
