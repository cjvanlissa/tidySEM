# Merge two lavaanify tables



# Replace .p1. labels
replace_ps <- function(vec, repl_nums){
  out <- vec
  rep_these <- which(grepl("^\\.p\\d+\\.$", vec))
  out[rep_these] <- paste0(".p", repl_nums[gsub(".", "", substring(vec[rep_these], first = 3), fixed = T)], ".")
  out
}

#' @importFrom lavaan lavOptions
lav_from_dots <- function(...){
  dots <- list(...)
  # Get list of lavaan option names
  lav_opts <- names(lavOptions())
  # Check if any of these are passed in dots
  these_opts <- which(names(dots) %in% lav_opts)
  if(length(these_opts > 0)){
    Args_lav <- dots[these_opts]
  } else {
    Args_lav <- list()
  }
  # Set default options corresponding to sem() and cfa()
  Args_lav_default <- list(
    int.ov.free = TRUE,
    int.lv.free = FALSE,
    auto.fix.first = TRUE,
    auto.fix.single = TRUE,
    auto.var = TRUE,
    auto.cov.lv.x = TRUE,
    auto.efa = TRUE,
    auto.th = TRUE,
    auto.delta = TRUE,
    auto.cov.y = TRUE
  )
  # If user did not specify default, overwrite
  for(this_arg in names(Args_lav_default)){
    if(!this_arg %in% names(Args_lav)){
      Args_lav[[this_arg]] <- Args_lav_default[[this_arg]]
    }
  }
  return(Args_lav)
}
