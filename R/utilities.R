#' @title Concatenate Strings while omitting NA
#' @description Concatenate vectors after converting to character and removing
#' \code{NA} values. See \code{\link{paste}}.
#' @param ...	one or more R objects, to be converted to character vectors.
#' @param sep	a character string to separate the terms.
#' Not \code{NA_character_}.
#' @param collapse an optional character string to separate the results.
#' Not \code{NA_character_}.
#' @param na.rm logical, indicating whether \code{NA} values should be stripped
#' before concatenation.
#' Not \code{NA_character_}.
#' @return A character vector of the concatenated values.
#' @examples
#' paste2("word", NA)
#' @rdname paste2
#' @export
paste2 <- function(..., sep = " ", collapse = NULL, na.rm = TRUE){
  if(!na.rm) return(paste(..., sep = sep, collapse = collapse))
  dots <- suppressWarnings(cbind(...))
  res <- apply(dots, 1, function(...) {
    if(all(is.na(c(...)))) return(NA)
    do.call(paste, as.list(c(na.omit(c(...)), sep = sep)))
  })
  if(is.null(collapse)) res else
    paste(na.omit(res), collapse = collapse)
}

diag_bind <- function(a, b, pad = NA){
  top <- cbind(a, matrix(data = pad, nrow = nrow(a), ncol = ncol(b)))
  bottom <- cbind(matrix(data = pad, nrow = nrow(b), ncol = ncol(a)), b)
  colnames(top)[-c(1:ncol(a))] <- colnames(b)
  rbind(top, bottom)
}

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
#' mplus_expand_names("testa-testb")
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
    name_stub <- gsub("\\d+$|\\w$", "", components)
    if(!name_stub[1] == name_stub[2]){
      stop("Could not identify name stub for line ", i)
    } else {
      name_stub <- name_stub[1]
    }
    last_char <- gsub(name_stub, "", components)
    if(!all(last_char %in% letters)){
      last_char_num <- as.numeric(gsub(name_stub, "", components))
      paste0(name_stub, seq.int(last_char_num[1], last_char_num[2]))
    }else{
      last_char_ind <- match(last_char, letters)
      exp_letter <- letters[seq.int(last_char_ind[1], last_char_ind[2])]
      paste0(name_stub, exp_letter)
    }

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


imp_fun <- function(x){
  if(is.data.frame(x)){
    return(data.frame(sapply(x, imp_fun)))
  } else {
    out <- x
    if(inherits(x, "numeric")){
      out[is.na(out)] <- median(x[!is.na(out)])
    } else {
      out[is.na(out)] <- names(sort(table(out), decreasing = TRUE))[1]
    }
    out
  }
}

fun_from_pack <- function(x){
  eval(parse(text=x))
}
