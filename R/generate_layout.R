if(FALSE){
  tab_res <- table_results(fit, all = TRUE)
  mm <- tab_res[tab_res$op == "=~", ]
  sem <- tab_res[tab_res$op == "~" | (tab_res$op == "~~" & !(tab_res$lhs == tab_res$rhs)), ]

  selected = list("")
  if(!any(sem$op == "~")){
    selected[[1]] <- unique(c(sem$lhs, sem$rhs))
  } else {
    # Drop correlations
    reg_only <- sem[!sem$op == "~~", ]
    remaining <- unique(c(reg_only$lhs, reg_only$rhs))
    maxit <- 0
    while(length(selected[[length(selected)]]) > 0 & maxit < 100){
      reg_only <- reg_only[!reg_only$lhs %in% unlist(selected), ]
      selected <- c(selected, list(
        unique(reg_only$lhs[!(reg_only$lhs %in% reg_only$rhs)])
      ))
      maxit <- maxit + 1
    }
    if(maxit == 100){
      stop("Maximum number of iterations exceeded in recursive layout algorithm.")
    }
    selected[[1]] <- NULL
    selected[[length(selected)]] <- unique(remaining[!remaining %in% unlist(selected)])
    selected <- selected[length(selected):1]
  }

  if(nrow(mm) > 0){ # If there is a measurement model
    items_per_col <- lapply(selected, function(thisvar){unique(mm$rhs[mm$lhs == thisvar])})
    lvs_per_col <- lapply(selected, function(thiscol){
      unique(mm$lhs[mm$lhs %in% thiscol])
    })
    max_cells <- max(max(sapply(selected, length)), max(sapply(items_per_col, length)))
    if(length(selected) == 1){
      mm_col <- unlist(items_per_col)
      lv_col <- space_these(selected[[1]], max_cells)
      out <- rbind(mm_col, lv_col)
    } else { # If there are multiple cols in the layout

      for(this_col in 1:length(selected)){
        if(this_col == 1){

        }
        if(this_col == length(selected)){

        }
      }
      lapply(selected, function(this_col){

      })
    }

  }
  # Remove rownames
  rownames(out) <- NULL
  return(out)
}

space_these <- function(these, n){
  #these <- letters[1:3]
  #n = 9
  out <- rep(NA, n)
  cellsper <- n/length(these)
  is_int <- (cellsper %% 1) == 0
  if(is_int){
    is_odd <- (cellsper %% 2) == 1
    if(is_odd){
      the_seq <- seq(from = ceiling(cellsper/2), to = n-floor(cellsper/2), length.out = length(these))
    } else {
      the_seq <- seq(from = floor(cellsper/2), to = n-floor(cellsper/2), length.out = length(these))
    }
  } else {
    browser() # Fix this
  }
  out[the_seq] <- these
  out
}

#' @title Generate a rudimentary layout from a model object
#' @description This is a wrapper function to the
#' \code{\link[igraph]{layout.reingold.tilford}} function, or other layout
#' functions from the \code{\link[igraph]{igraph-package}}. It returns a layout
#' in matrix format.
#' @param x A model for which a method exists.
#' @param layout_algorithm Which algorithm to use, from the
#' \code{\link[igraph]{igraph-package}}. The default Reingold-Tilford algorithm
#' is most suitable for SEM graphs.
#' @return Matrix
#' @examples
#' \dontrun{
#' library(lavaan)
#' fit <- sem("Sepal.Length ~ Petal.Width", data = iris)
#' generate_layout(fit)
#' }
#' @rdname generate_layout
#' @export
#' @importFrom igraph graph.data.frame vertex.attributes
generate_layout <- function(x, layout_algorithm = "layout.reingold.tilford"){
  browser()
  tab_res <- table_results(x, all = TRUE)
  df <- tab_res[tab_res$op %in% c("~", "=~"), c("lhs", "rhs")]
  g <- graph.data.frame(df, directed = TRUE)
  #layout_algorithm <- paste0("igraph::", layout_algorithm)
  lo <- do.call(layout_algorithm, list(g))
  lo <- round(lo)
  if(any(duplicated(lo))){
    stop("Could not snap to grid, some nodes were in the same location.")
  }
  lo <- sweep(lo, 2, (apply(lo, 2, min)-1), "-")
  out <- matrix(nrow = max(lo[,2]), ncol = max(lo[, 1]))
  vnames <- vertex.attributes(g)$name
  for(this_var in 1:length(vnames)){
    out[lo[this_var, 2], lo[this_var, 1]] <- vnames[this_var]
  }
  if(dim(out)[2] < dim(out)[1]){
    t(out)
  } else {
    out[nrow(out):1, ]
  }

}
