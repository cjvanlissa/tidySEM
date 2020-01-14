if(FALSE){
  df_edges <- prep$edges

  x <- df_edges
  lo_recursive <- function(selected = NULL, remaining){
    if(is.null(selected)){
      lo_recursive
    }
  }

  selected = list("")
  # Drop correlations
  tmp <- x[!(x$arrow == "none" & !is.na(x$curvature)), ]
  remaining = unique(c(tmp$to, tmp$from))
  maxit <- 0
  while(length(selected[[length(selected)]]) > 0 & maxit < 100){
    tmp <- tmp[!tmp$to %in% unlist(selected), ]
    selected <- c(selected, list(
      unique(tmp$to[!(tmp$to %in% tmp$from)])
    ))
    maxit <- maxit + 1
  }
  if(maxit == 100){
    stop("Maximum number of iterations exceeded in recursive layout algorithm.")
  }
  selected[[1]] <- NULL
  selected[[length(selected)]] <- unique(x$from[!x$from %in% unlist(selected)])

}
