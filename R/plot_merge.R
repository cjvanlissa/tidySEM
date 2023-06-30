merge.sem_graph <- function(..., nrow = NULL, ncol = NULL, distance_x = 1, distance_y = 1){
  dots <- list(...)
  n <- length(dots)
  if(is.null(nrow) & is.null(ncol)){
    ncol <- ceiling(sqrt(n))
    nrow <- ceiling(n/ncol)
  } else {
    if(is.null(nrow)){
      nrow <- ceiling(n/ncol)
    }
    if(is.null(ncol)){
      ncol <- ceiling(n/nrow)
    }
  }
  if(!(nrow*ncol)>=n) stop("Specify nrow and/or ncol.")
  m <- matrix(nrow = nrow, ncol = ncol)
  m[1:n] <- 1:n
  edges <- p1$edges
  nodes <- p1$nodes
  xwidths <- apply(m, 2, function(x){
    max(unlist(lapply(dots[as.vector(x)], function(d){d$nodes$node_xmax})))
  })
  yheights <- apply(m, 1, function(x){
    max(unlist(lapply(dots[as.vector(x)], function(d){d$nodes$node_ymax})))
  })

  for(i in 2:n){
    if(any(dots[[i]]$nodes$name %in% p1$nodes$name)){
      repthese <- dots[[i]]$nodes$name[dots[[i]]$nodes$name %in% p1$nodes$name]
      names(repthese) <- paste0(repthese, ".", i)
      dots[[i]]$nodes$name[match(dots[[i]]$nodes$name, repthese)] <- names(repthese)
      dots[[i]]$edges$from[dots[[i]]$edges$from %in% repthese] <- names(repthese)[match(dots[[i]]$edges$from, repthese)]
      dots[[i]]$edges$to[dots[[i]]$edges$to %in% repthese] <- names(repthese)[match(dots[[i]]$edges$to, repthese)]
    }
    thisrow <- which(apply(m, 1, function(x){any(x==i)}))
    thiscol <- which(apply(m, 2, function(x){any(x==i)}))
    browser()
    addnodes <- dots[[i]]$nodes
    addx <- sum(xwidths[0:(thiscol-1)]) + (thiscol-1)*distance_x
    addnodes[c("x", "node_xmax", "node_xmin")] <- addnodes[c("x", "node_xmax", "node_xmin")] + addx
    addy <- sum(yheights[0:(thisrow-1)]) + (thisrow-1)*distance_y
    addnodes[c("y", "node_ymax", "node_ymin")] <- addnodes[c("y", "node_ymax", "node_ymin")] + addy

    dots[[i]]$nodes <- addnodes
    p1$edges <- rbind(p1$edges, dots[[m[i]]]$edges)
    p1$nodes <- rbind(p1$nodes, dots[[m[i]]]$nodes)
  }
  return(p1)
}
