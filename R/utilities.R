#' @title Longest common substring
#' @description Extract the longest common substring of two strings using
#' base R.
#' @param x First atomic character vector.
#' @param y Second atomic character vector.
#' @return Character string
#' @examples
#' lcs("test1", "test2")
#' @rdname lcs
#' @keywords mplus utilities
#' @export
#' @importFrom utils adist
lcs <- function(x,y) {
  matches <- gregexpr("M+", drop(attr(adist(x, y, counts=TRUE), "trafos")))[[1]];
  lengths <- attr(matches, 'match.length')
  which_longest <- which.max(lengths)
  index_longest <- matches[which_longest]
  length_longest <- lengths[which_longest]
  substring(c(x,y)[which.max(nchar(c(x,y)))], index_longest , index_longest + length_longest - 1)
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
