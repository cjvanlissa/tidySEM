edit_sem <- function(model, ..., element = "syntax"){
  #dots <- lapply(list(...), function(i){do.call(quote, list(expr = i))})
  #dots <- lapply(list(...), function(i){Reduce(paste, deparse(i))})
  # Clean dots

  dots <- lapply(sys.call()[-c(1:2)], deparse)
  has_name <- names(dots) == ""
  dots[!has_name] <- paste0(names(dots[!has_name]), "=", dots[!has_name])
  tab <- do.call(rbind, lapply(dots, function(i){
    out <- lavaan::lavParseModelString(i, as.data.frame. = TRUE)#[, 1:5, drop = FALSE]
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
edit_sem(model, vis ~ text, vis ~~ spe, vis ~1, vis=~tex, vis~tex+spe, vis~.5*tex+spe)
