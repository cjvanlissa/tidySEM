#' Make a data dictionary
#'
#' Make a data dictionary for a data.frame, based on variable names.
# Attempts to generate a keys.list, as used by functions from the \code{psych}
# package, and several functions in this \code{motley} package.
#'
#' @param x An object for which a method exists, e.g., a vector of variable
#' names, or a data.frame.
#' @param split Character. Within variable names, which character separates the
#' name of the scale/construct from the number (or name) of the item? E.g.,
#' if the name is \code{"construct_1"}, the split character \code{"_"} separates
#' the \code{"construct"} name from item number \code{"1"}.
#' @return An object of class "tidy_sem"
#' @author Caspar J. van Lissa
#' @export
#' @examples
#' tidy_sem(c("bfi_1", "bfi_2", "bfi_3", "bfi_4", "bfi_5",
#' "macqj_1", "macqj_2", "macqj_3", "macqj_4", "macqj_5", "macqj_6",
#' "macqj_7", "macqj_8", "macqj_9", "macqj_10", "macqj_11",
#' "macqj_12", "macqj_13", "macqj_14", "macqj_15", "macqj_16",
#' "macqj_17", "macqj_18", "macqj_19", "macqj_20", "macqj_21",
#' "macqr_1", "macqr_2", "macqr_3", "macqr_4", "macqr_5", "macqr_6",
#' "macqr_7", "macqr_8", "macqr_9", "macqr_10", "macqr_11",
#' "macqr_12", "macqr_13", "macqr_14", "macqr_15", "macqr_16",
#' "macqr_17", "macqr_18", "macqr_19", "macqr_20", "macqr_21", "sex"))
#' tidy_sem(c("bfi_1", "bfi_2", "bfi_3", "bfi_4", "bfi_5",
#' "mac_q_j_1", "mac_q_j_2", "mac_q_j_3", "mac_q_j_4", "mac_q_j_5", "mac_q_j_6",
#' "mac_q_j_7", "mac_q_j_8", "mac_q_j_9", "mac_q_j_10", "mac_q_j_11",
#' "mac_q_j_12", "mac_q_j_13", "mac_q_j_14", "mac_q_j_15", "mac_q_j_16",
#' "mac_q_j_17", "mac_q_j_18", "mac_q_j_19", "mac_q_j_20", "mac_q_j_21",
#' "mac_q_r_1", "mac_q_r_2", "mac_q_r_3", "mac_q_r_4", "mac_q_r_5", "mac_q_r_6",
#' "mac_q_r_7", "mac_q_r_8", "mac_q_r_9", "mac_q_r_10", "mac_q_r_11",
#' "mac_q_r_12", "mac_q_r_13", "mac_q_r_14", "mac_q_r_15", "mac_q_r_16",
#' "mac_q_r_17", "mac_q_r_18", "mac_q_r_19", "mac_q_r_20", "mac_q_r_21"))
tidy_sem <- function(x,
                       split = "_"){
  UseMethod("tidy_sem")
}

#' @method tidy_sem character
#' @export
tidy_sem.character <- function(x, split = "_"){
  Args <- as.list(match.call()[-1])
  out <- list(dictionary = do.call(.dict_internal, Args),
              data = NULL,
              syntax = NULL)
  class(out) <- c("tidy_sem", class(out))
  out
}

#' @method tidy_sem data.frame
#' @export
tidy_sem.data.frame <- function(x, split = "_"){
  Args <- as.list(match.call()[-1])
  Args$x <- names(x)
  out <- list(dictionary = do.call(.dict_internal, Args),
              data = x,
              syntax = NULL)
  class(out) <- c("tidy_sem", class(out))
  out
}


keys <- function(x, min_items = 3){
  UseMethod("keys")
}


.dict_internal <- function(x, split = "_", ...){
  split_items <- strsplit(x, split)
  num_splits <- sapply(split_items, length)
  if(any(num_splits > 2)){
    message("Some variable names contained more than one splitting character. Ignored all but the last splitting character.", call. = FALSE)
    split_items[which(num_splits > 2)] <- lapply(split_items[which(num_splits > 2)], function(x){
      c(paste0(x[-length(x)], collapse = split), x[length(x)])
    })
  }
  observed <- num_splits < 2
  if(any(observed)){
    split_items[which(observed)] <- lapply(split_items[which(num_splits < 2)], function(x){
      c(NA, NA)
    })
  }
  dictionary <- data.frame(name = x, do.call(rbind, split_items), stringsAsFactors = FALSE)
  names(dictionary)[c(2,3)] <- c("scale", "item")
  dictionary$type <- "observed"
  #dictionary$type[!observed] <- "indicator"
  dictionary$label <- dictionary$name
  dictionary$item <- NULL
  dictionary
}


#' @method keys tidy_sem
#' @export
keys.tidy_sem <- function(x, min_items = 3){
  num_items <- table(x$scale)
  retain_q <- names(num_items)[which(num_items >= min_items)]
  #retain_q <- retain_q[!retain_q %in% skip_questionnaires]
  outlist <- lapply(retain_q, function(i){x$name[which(x$scale == i)]})
  names(outlist) <- retain_q
  class(outlist) <- c("keys_list", class(outlist))
  outlist
}

#' @method keys character
#' @export
keys.character <- function(x, min_items = 3){
  Args <- list(x = x)
  dictionary <- do.call(tidy_sem, Args)
  Args <- list(x = dictionary,
               min_items = min_items)
  do.call(keys, Args)
}

#' @method keys data.frame
#' @export
keys.data.frame <- function(x, min_items){
  Args <- list(x = names(x))
  dictionary <- do.call(tidy_sem, Args)
  Args <- list(x = dictionary,
               min_items = min_items)
  do.call(keys, Args)
}

