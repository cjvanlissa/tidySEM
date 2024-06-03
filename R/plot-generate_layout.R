if(FALSE){
  tab_res <- table_results(fit, columns = NULL)
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
    stop("Could not space nodes.")
  }
  out[the_seq] <- these
  out
}

# @title Generate a rudimentary layout from a model object
# @description This is a wrapper function to the
# \code{\link[igraph]{layout_as_tree}} function, or other layout
# functions from the \code{\link[igraph]{igraph-package}}. It returns a layout
# in matrix format.
# @param x A model for which a method exists.
# @param layout_algorithm Which algorithm to use, from the
# \code{\link[igraph]{igraph-package}}. The default Reingold-Tilford algorithm
# is most suitable for SEM graphs.
# @return Matrix
# @examples
# \dontrun{
# library(lavaan)
# fit <- sem("Sepal.Length ~ Petal.Width", data = iris)
# generate_layout(fit)
# }
# @rdname generate_layout

#' @method get_layout lavaan
#' @rdname get_layout
#' @param layout_algorithm Optional argument for fit model objects. Character
#' string, indicating which \code{igraph}
#' layout algorithm to apply to position the nodes. Defaults to
#' \code{"layout_as_tree"}; see details for more options.
#' @export
get_layout.lavaan <- function(x, ..., layout_algorithm = "layout_as_tree"){
  Args <- as.list(match.call()[-1])
  Args$x <- table_results(x, columns = NULL)
  do.call(get_layout, Args)
}

#' @method get_layout mplus.model
#' @export
get_layout.mplus.model <- get_layout.lavaan

#' @method get_layout MxModel
#' @export
get_layout.MxModel <- get_layout.lavaan

#' @method get_layout mplusObject
#' @export
get_layout.mplusObject <- get_layout.lavaan

#' @method get_layout tidy_results
#' @export
#' @importFrom igraph graph.data.frame vertex.attributes
#' layout_as_star layout_as_tree layout_in_circle layout_nicely
#' layout_on_grid layout_randomly layout_with_dh layout_with_fr layout_with_gem
#' layout_with_graphopt layout_with_kk layout_with_lgl layout_with_mds
get_layout.tidy_results <- function(x, ..., layout_algorithm = "layout_as_tree"){
  cl <- match.call()
  cl[[1L]] <- str2lang("tidySEM:::get_edges.tidy_results")
  cl <- cl[c(1L, which(names(cl) == "x"))]
  df <- eval.parent(cl)[c("from", "to")]
  get_layout(df)
}

#' @method get_layout tidy_edges
#' @export
#' @importFrom igraph graph_from_data_frame vertex.attributes
#' layout_as_star layout_as_tree layout_in_circle layout_nicely
#' layout_on_grid layout_randomly layout_with_dh layout_with_fr layout_with_gem
#' layout_with_graphopt layout_with_kk layout_with_lgl layout_with_mds
get_layout.tidy_edges <- function(x, ..., layout_algorithm = "layout_as_tree"){
  g <- igraph::graph_from_data_frame(x, directed = TRUE)
  lo <- do.call(layout_algorithm, list(g))
  lo <- round(lo)
  if(any(duplicated(lo))){
    lo <- resolve_dups(lo)
    #stop("Could not snap to grid, some nodes were in the same location.")
  }
  lo <- sweep(lo, 2, (apply(lo, 2, min)-1), "-")
  out <- matrix(nrow = max(lo[,2]), ncol = max(lo[, 1]))
  vnames <- vertex.attributes(g)$name
  for(this_var in 1:length(vnames)){
    out[lo[this_var, 2], lo[this_var, 1]] <- vnames[this_var]
  }
  if(dim(out)[2] < dim(out)[1]){
    out <- t(out)
  } else {
    out <- out[nrow(out):1, ]
  }
  class(out) <- c("layout_matrix", class(out))
  return(out)
}

#' @importFrom utils tail
resolve_dups <- function(lo){
  new_lo <- lo
  first_dup <- which(duplicated(lo))[1]
  dup_row <- lo[first_dup,]
  neighboring_locs <- t(apply(expand.grid(c(-1,0,1), c(-1,0,1)), 1, `+`, dup_row))
  free_locs <- neighboring_locs[tail(!duplicated(rbind(lo, neighboring_locs)), 9), ]
  if(nrow(free_locs) == 0) stop("Could not generate layout automatically. Please specify a layout manually.")
  new_lo[first_dup, ] <- free_locs[sample.int(nrow(free_locs), 1), ]
  if(any(duplicated(new_lo))){
    resolve_dups(new_lo)
  } else {
    return(new_lo)
  }
}


#' @title Generate graph layout
#' @description Generate a tidy_layout for a SEM graph.
#' @param x An object for which a method exists; currently, methods exist for
#' \code{character}, \code{lavaan}, and \code{mplus.model} objects.
#' @param ... Character arguments corresponding to layout elements. Use node
#' names, empty strings (""), or NA values.
#' @details There are three ways to generate a layout:
#' \enumerate{
#'   \item Specify the layout in the call to \code{get_layout()} by providing
#'   node names and the number of
#'   rows to create a layout matrix. Empty strings (\code{""})
#'   or \code{NA} can be used for empty cells. See Example 1.
#'   \item Call \code{get_layout()} on a model object or \code{tidy_results}
#'   object. It will use the function
#'   \code{\link[igraph]{layout_as_tree}}, or any other layout function
#'   from the \code{igraph} package, to generate a rudimentary layout. See
#'   Example 2.
#'   \item Instead of using \code{get_layout()}, just use a \code{matrix} or
#'   \code{data.frame} with your layout. For example, specify the layout in a
#'   spreadsheet program, and load it into R (see Example 3). Or, copy the
#'   layout to the clipboard from your spreadsheet program, and load it from the
#'   clipboard (see Example 4)
#' }
#' The layout algorithms imported from \code{igraph} are:
#' \code{c("layout_as_star",
#' "layout_as_tree", "layout_in_circle", "layout_nicely",
#' "layout_on_grid", "layout_randomly", "layout_with_dh", "layout_with_fr",
#' "layout_with_gem",
#' "layout_with_graphopt", "layout_with_kk", "layout_with_lgl",
#' "layout_with_mds")}. These can be used by specifying the optional argument
#' \code{layout_algorithm = ""}.
#' @return Object of class 'tidy_layout'
#' @examples
#' # Example 1
#' get_layout("c", NA,  "d",
#'            NA,  "e", NA, rows = 2)
#'
#' # Example 2
#' library(lavaan)
#' fit <- cfa(' visual  =~ x1 + x2 + x3 ',
#'            data = HolzingerSwineford1939[1:50, ])
#' get_layout(fit)
#'
#' \dontrun{
#' # Example 3
#' # Here, we first write the layout to .csv, but you could create it in a
#' # spreadsheet program, and save the spreadsheet to .csv:
#' write.csv(matrix(c("c", "",  "d", "",  "e", ""), nrow = 2, byrow = TRUE),
#'           file = file.path(tempdir(), "example3.csv"), row.names = FALSE)
#' # Now, we load the .csv:
#' read.csv(file.path(tempdir(), "example3.csv"))
#'
#' # Example 4
#' # For this example, make your layout in a spreadsheet program, select it, and
#' # copy to clipboard. Reading from the clipboard works differently in Windows
#' # and Mac. For this example, I used Microsoft Excel.
#' # On Windows, run:
#' read.table("clipboard", sep = "\t")
#' # On Mac, run:
#' read.table(pipe("pbpaste"), sep="\t")
#' }
#' @rdname get_layout
#' @keywords tidy_graph
# @seealso long_layout
#' @export
get_layout <- function(x, ...){
  UseMethod("get_layout", x)
}

# @title Generate graph layout
# @description Generate a tidy_layout for a SEM graph by specifying node names,
# and empty strings or \code{NA} values for spaces.
# @param ... Character arguments corresponding to layout elements. Use node
# names, empty strings (""), or NA values.
# @param rows Numeric, indicating the number of rows of the graph.
# @return Object of class 'tidy_layout'
# @examples
# get_layout("c", "",  "d",
#            "",  "e", "", rows = 2)
# @rdname layout
# @keywords tidy_graph
# @seealso long_layout

#' @param rows Numeric, indicating the number of rows of the graph.
#' @rdname get_layout
#' @method get_layout default
#' @export
get_layout.default <- function(x, ..., rows = NULL){
  Args <- as.list(match.call()[-1])
  if("rows" %in% names(Args)){
    Args$rows <- NULL
  } else {
    if(sum(sapply(Args, is.numeric)) == 1){
      Args[which(sapply(Args, is.numeric))] <- NULL
    } else {
      dots <- list(...)
      cl <- match.call()
      cl["columns"] <- list(NULL)
      cl[[1L]] <- str2lang("tidySEM::table_results")
      cl$x <- tryCatch(eval.parent(cl), error = function(e){
        stop("Could not create layout for object.")
      })
      if("columns" %in% names(dots)){
        cl["columns"] <- dots["columns"]
      }
      cl[[1L]] <- str2lang("tidySEM::get_layout")
      return(eval.parent(cl))
    }
  }
  if(isFALSE(length(Args) %% rows == 0)){
    stop("Number of arguments is not a multiple of rows = ", rows, call. = FALSE)
  }
  vec <- do.call(c, Args)
  out <- do.call(matrix, list(
    data = vec,
    nrow = rows,
    byrow = TRUE
  ))
  class(out) <- c("layout_matrix", class(out))
  return(out)
}


# @title Convert object to layout
# @description Convert an object to a tidy_layout for a SEM graph.
# @param x Object to convert to a tidy_layout. The default argument reads a
# selected matrix from the clipboard.
# To use this functionality, specify your layout in a spreadsheet program,
# select the block of cells, and copy it to the clipboard.
# @return Object of class 'tidy_layout'
# @examples
# \dontrun{
# if(interactive()){
#  #EXAMPLE1
#  }
# }
# @rdname long_layout
# @keywords tidy_graph
# @export
long_layout <- function(x){
  UseMethod("long_layout")
}

#' @method long_layout data.frame
#' @export
long_layout.data.frame <- function(x){
  Args <- as.list(match.call()[-1])
  Args$x <- as.matrix(x)
  do.call(long_layout, Args)
}

#' @method long_layout matrix
#' @export
long_layout.matrix <- function(x){
  mat <- x
  mat[is.na(mat)] <- ""
  nodes_long <- setNames(as.data.frame.table(mat), c("y", "x", "name"))
  nodes_long[1:2] <- lapply(nodes_long[1:2], as.numeric)
  nodes_long$y <- (max(nodes_long$y)+1)-nodes_long$y
  nodes_long$name <- as.character(nodes_long$name)
  nodes_long <- nodes_long[!nodes_long$name == "", ]
  row.names(nodes_long) <- NULL
  class(nodes_long) <- c("tidy_layout", class(nodes_long))
  nodes_long
}
