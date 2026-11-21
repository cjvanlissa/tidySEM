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
get_layout.lavaan <- function(x, ..., layout_algorithm = "sem"){
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
# @importFrom igraph graph_from_data_frame vertex.attributes
# @importFrom igraph layout_as_star layout_as_tree layout_in_circle
# @importFrom igraph layout_nicely layout_on_grid layout_randomly
# @importFrom igraph layout_with_dh layout_with_fr layout_with_gem
# @importFrom igraph layout_with_graphopt layout_with_kk layout_with_lgl
# @importFrom igraph layout_with_mds
get_layout.tidy_results <- function(x, ..., layout_algorithm = "sem") {
  nds <- unique(c(as.character(x$lhs), as.character(x$rhs)))
  nds <- nds[!is.na(nds) & nzchar(nds)]
  .get_layout_nodes_edges(
    x = x,
    nds = nds,
    layout_algorithm = layout_algorithm, ...
  )
}

#' @method get_layout tidy_edges
#' @export
get_layout.tidy_edges <- get_layout.tidy_results

.get_layout_nodes_edges <- function(x, nds, ...,
                                    layout_algorithm = "sem") {
  x <- as.data.frame(x)
  panel_cols <- intersect(c("group", "level"), names(x))

  if (!all(c("lhs", "rhs") %in% names(x)))
    stop("`x` must contain `lhs` and `rhs`.")

  # Exclude mean/intercept pseudo-nodes and invalid endpoints.
  x$lhs <- as.character(x$lhs)
  x$rhs <- as.character(x$rhs)
  valid <- !is.na(x$lhs) & !is.na(x$rhs) &
    nzchar(x$lhs) & nzchar(x$rhs)
  if ("op" %in% names(x)) {
    intercept <- !is.na(x$op) & x$op == "~1" & x$rhs == "1"
    valid <- valid & !intercept
  }
  x <- x[valid, , drop = FALSE]

  # Resolve igraph layout function once; igraph is never needed for
  # the two built-in algorithms.
  built_in <- is.character(layout_algorithm) &&
    length(layout_algorithm) == 1L &&
    !is.na(layout_algorithm) &&
    layout_algorithm %in% c("sem", "fr_grid")

  if (!built_in) {
    if (!requireNamespace("igraph", quietly = TRUE)) {
      stop("Package `igraph` is required for this layout algorithm.",
           call. = FALSE)
    }
    if (is.function(layout_algorithm)) {
      layout_fun <- layout_algorithm
    } else if (is.character(layout_algorithm) &&
               length(layout_algorithm) == 1L &&
               !is.na(layout_algorithm) &&
               exists(layout_algorithm, envir = asNamespace("igraph"),
                      mode = "function", inherits = FALSE)) {
      layout_fun <- get(layout_algorithm, envir = asNamespace("igraph"),
                        inherits = FALSE)
    } else {
      stop("Unknown layout algorithm.", call. = FALSE)
    }
  }

  # Execute an algorithm on exactly one panel. All results are
  # normalized to x, y, name, independently of the algorithm.
  layout_one <- function(part) {
    node_names <- unique(c(part$lhs, part$rhs))
    node_names <- node_names[!is.na(node_names) & nzchar(node_names)]
    dots <- list(...)

    if (!length(node_names)) {
      return(data.frame(x = numeric(), y = numeric(),
                        name = character(), stringsAsFactors = FALSE))
    }

    if (identical(layout_algorithm, "fr_grid")) {
      Args <- c(list(nodes = node_names,
                     edges = as.matrix(part[, c("lhs", "rhs"), drop = FALSE])), dots[which(names(dots) %in% names(formals(fr_layout_integer_compact)))])
      lo <- do.call(fr_layout_integer_compact, Args)
    } else if (identical(layout_algorithm, "sem")) {
      if (!"op" %in% names(part))
        stop("SEM layout requires an `op` column.")
      # The single-panel algorithm must not receive panel columns.
      sem_edges <- part[, c("lhs", "rhs", "op"), drop = FALSE]
      Args <- c(list(nodes = node_names, edges = sem_edges),
                dots[which(names(dots) %in% names(formals(sem_layout_integer)))])
      lo <- do.call(sem_layout_integer, Args)
    } else {
      graph <- igraph::graph_from_data_frame(
        part[, c("lhs", "rhs"), drop = FALSE],
        directed = TRUE,
        vertices = data.frame(name = node_names)
      )
      Args <- c(list(graph),
                dots[which(names(dots) %in% names(formals(layout_fun)))])
      lo <- do.call(layout_fun, Args)
      # layout_with_sugiyama() returns a list with $layout.
      if (is.list(lo) && !is.data.frame(lo) && !is.null(lo$layout))
        lo <- lo$layout
      if (!is.matrix(lo) || nrow(lo) != length(igraph::V(graph)))
        stop("igraph layout must return a coordinate matrix with one row per vertex.")
      if (ncol(lo) < 2L)
        stop("igraph layout must have at least two coordinate columns.")
      lo <- lo[, 1:2, drop = FALSE]
      rownames(lo) <- igraph::V(graph)$name
      lo <- long_layout(snap_layout_to_grid(lo))
    }
    if (is.data.frame(lo)) {
      if (!all(c("x", "y", "name") %in% names(lo)))
        stop("Layout data frame must contain `x`, `y`, and `name`.")
      ans <- as.data.frame(lo[, c("x", "y", "name"), drop = FALSE])
    } else {
      lo <- as.matrix(lo)
      if (ncol(lo) < 2L || nrow(lo) != length(node_names))
        stop("Layout matrix has incorrect dimensions.")
      names_from_rows <- rownames(lo)
      if (is.null(names_from_rows)) names_from_rows <- node_names
      ans <- data.frame(x = lo[, 1], y = lo[, 2],
                        name = names_from_rows, stringsAsFactors = FALSE)
    }
    ans$name <- as.character(ans$name)
    if (anyDuplicated(ans$name) ||
        !setequal(ans$name, node_names))
      stop("Layout must contain each panel node exactly once.")
    ans <- ans[match(node_names, ans$name), , drop = FALSE]
    ans$x <- as.numeric(ans$x)
    ans$y <- as.numeric(ans$y)
    if (any(!is.finite(ans$x)) || any(!is.finite(ans$y)))
      stop("Layout contains non-finite coordinates.")
    # Panel-local coordinates; ggplot faceting provides offsets.
    ans$x <- ans$x - min(ans$x)
    ans$y <- ans$y - min(ans$y)
    rownames(ans) <- NULL
    ans
  }

  if (!length(panel_cols)) {
    result <- layout_one(x)
  } else {
    # An absent panel dimension means one dimension only.
    # Actual NA identifiers are rejected, not treated as wildcards.
    for (col in panel_cols) {
      if (anyNA(x[[col]]))
        stop("Missing values in panel column `", col, "` are ambiguous.")
      x[[col]] <- as.character(x[[col]])
    }
    panels <- unique(x[, panel_cols, drop = FALSE])
    pieces <- vector("list", nrow(panels))
    for (i in seq_len(nrow(panels))) {
      take <- rep(TRUE, nrow(x))
      for (col in panel_cols)
        take <- take & (x[[col]] == panels[[col]][i])
      piece <- layout_one(x[take, , drop = FALSE])
      for (col in panel_cols)
        piece[[col]] <- rep(panels[[col]][i], nrow(piece))
      pieces[[i]] <- piece
    }
    result <- if (length(pieces)) do.call(rbind, pieces) else {
      out <- data.frame(x = numeric(), y = numeric(), name = character())
      for (col in panel_cols) out[[col]] <- character()
      out
    }
  }

  rownames(result) <- NULL
  attr(result, "grid_size") <- c(
    width = if (nrow(result)) max(result$x) + 1 else 0,
    height = if (nrow(result)) max(result$y) + 1 else 0
  )
  class(result) <- c("tidy_layout", "data.frame")
  result
}

snap_layout_to_grid <- function(lo){
  lo <- round(lo)
  if(any(duplicated(lo))){
    lo <- resolve_dups(lo)
    #stop("Could not snap to grid, some nodes were in the same location.")
  }
  lo <- sweep(lo, 2, (apply(lo, 2, min)-1), "-")
  out <- matrix(nrow = max(lo[,2]), ncol = max(lo[, 1]))
  vnames <- rownames(lo)
  for(this_var in 1:length(vnames)){
    out[lo[this_var, 2], lo[this_var, 1]] <- vnames[this_var]
  }
  if(dim(out)[2] < dim(out)[1]){
    out <- t(out)
  } else {
    out <- out[nrow(out):1, , drop = FALSE]
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
#' # read.table("clipboard", sep = "\t")
#' # On Mac, run:
#' # read.table(pipe("pbpaste"), sep="\t")
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
get_layout.default <- function(x, ..., rows = NULL) {
  dots <- list(...)

  # Determine whether rows was supplied
  if (is.null(rows)) {
    numeric_args <- which(vapply(dots, is.numeric, logical(1)))

    if (length(numeric_args) == 1L) {
      rows <- dots[[numeric_args]]
      dots <- dots[-numeric_args]
    } else {
      # Convert object to a results table
      columns <- dots$columns
      dots$columns <- NULL

      tab <- tryCatch(
        do.call(
          table_results,
          c(list(x = x, columns = NULL), dots)
        ),
        error = function(e) {
          stop("Could not create layout for object.", call. = FALSE)
        }
      )

      return(
        get_layout(tab, columns = columns)
      )
    }
  }

  # Construct layout matrix
  vec <- do.call(c, c(list(x), dots))

  if (length(vec) %% rows != 0L) {
    stop(
      "Number of arguments is not a multiple of rows = ",
      rows,
      call. = FALSE
    )
  }

  out <- matrix(
    data = vec,
    nrow = rows,
    byrow = TRUE
  )

  class(out) <- c("layout_matrix", class(out))
  out
}



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
