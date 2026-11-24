# @title Filter graph elements
# @description Apply a filter to one of the elements
# of a \code{sem_graph} object, and return the modified \code{sem_graph}. Uses
# the function \code{\link{subset}}.
# @param x An object of class \code{sem_graph}.
# @param subset logical expression indicating elements or rows to keep:
# missing values are taken as false.
# @param select Expression, indicating columns to select from a data frame.
# @param element Character. The element of the \code{sem_graph} to filter,
# defaults to \code{"edges"}.
# @param ... Arguments passed on to \code{\link{within}}.
# @return An object of class \code{sem_graph}.
# @examples
# library(lavaan)
# res <- sem("Sepal.Length ~ Petal.Length + Petal.Width", iris)
# p <- filter_graph(p, !arrow == "none")
# plot(p)

# @rdname filter_graph
# @export
filter_graph <- function(x, subset, select, element = "edges", ...){
  UseMethod("filter_graph", x)
}

# @method filter_graph sem_graph
#' @export
filter_graph.sem_graph <- function(x, subset, select,
                                   element = "edges", ...) {
  cl <- match.call()
  cl[[1L]] <- quote(base::subset)
  cl$x <- x[[element]]
  cl$element <- NULL

  x[[element]] <- eval(cl, envir = parent.frame())
  x
}

#' @title Edit graph elements
#' @description Evaluate an R expression within the environment of the elements
#' of a \code{sem_graph} object, and return the modified \code{sem_graph}.
#' @param x An object of class \code{sem_graph}.
#' @param expr expression to evaluate.
#' @param element Character. The element of the \code{sem_graph} to edit,
#' defaults to \code{c("edges", "nodes")}.
#' @param ... Arguments passed on to \code{\link{within}}.
#' @return An object of class \code{sem_graph}.
#' @examples
#' p <- prepare_graph(layout = get_layout("x", rows = 1))
#' p <- edit_graph(p, {colour = "blue"}, element = "nodes")
#' plot(p)
#' @rdname edit_graph
#' @export
edit_graph <- function(x, expr, element = c("edges", "nodes"), ...){
  UseMethod("edit_graph", x)
}

#' @method edit_graph sem_graph
#' @export
edit_graph.sem_graph <- function(
    x,
    expr,
    element = c("edges", "nodes"),
    ...
) {

  valid_elements <- c("edges", "nodes")

  if (!all(element %in% valid_elements)) {
    stop(
      "Unknown sem_graph element: ",
      paste(setdiff(element, valid_elements), collapse = ", "),
      call. = FALSE
    )
  }

  expression <- .normalize_sem_edit_expr(substitute(expr))

  for (el in element) {

    original <- .normalize_sem_aesthetics(x[[el]])
    old_names <- names(original)

    # Preserve the caller's environment for NSE.
    eval_env <- new.env(parent = parent.frame())
    eval_env$original <- original

    cl <- substitute(
      FUN(original, EXPR),
      list(
        FUN = edit_graph.data.frame,
        EXPR = expression
      )
    )

    edited <- eval(cl, envir = eval_env)

    # Validate newly created columns.
    new_names <- setdiff(names(edited), old_names)

    allowed <- c(
      names(.sem_aesthetic_registry()),
      "show", "label", "geom_text"
    )

    if (el == "edges") {
      allowed <- setdiff(allowed, "fill")
    }

    if (el == "nodes") {
      allowed <- setdiff(allowed, "label_location")
    }

    unknown <- setdiff(new_names, allowed)

    if (length(unknown)) {
      stop(
        "Unknown ", el, " aesthetic or property: ",
        paste(unknown, collapse = ", "),
        call. = FALSE
      )
    }

    x[[el]] <- edited
  }

  x
}

#' @rdname edit_graph
#' @export
edit_nodes <- function(x, expr, ...){
  cl <- match.call()
  cl[[1L]] <- str2lang("tidySEM::edit_graph")
  cl[["element"]] <- "nodes"
  eval.parent(cl)
}

#' @rdname edit_graph
#' @export
edit_edges <- function(x, expr, ...){
  cl <- match.call()
  cl[[1L]] <- str2lang("tidySEM::edit_graph")
  cl[["element"]] <- "edges"
  eval.parent(cl)
}

#' @method edit_graph data.frame
#' @export
edit_graph.data.frame <- function(x, expr, ...){
  cl <- match.call()
  cl[[1L]] <- quote(within)
  names(cl)[names(cl) == "x"] <- "data"
  eval.parent(cl)
}



#' @title Render a graph
#' @description Render a graph based on a layout, and either nodes and edges, or
#' a model object.
#' @param ... Additional arguments passed to and from functions.
#' @return Object of class 'sem_graph'
#' @details The default interface simply Runs the functions
#' \code{\link[tidySEM]{prepare_graph}} and
#' \code{plot}. The alternative interface first runs
#' \code{\link[tidySEM]{get_nodes}} and \code{\link[tidySEM]{get_edges}} on
#' the \code{model} argument.
#' @examples
#' library(lavaan)
#' res <- sem("dist ~ speed", cars)
#' graph_sem(res)
#' @rdname graph_sem
#' @keywords tidy_graph
#' @export
graph_sem <- function(...){
  UseMethod("graph_sem")
}

#' @method graph_sem default
#' @param edges Object of class 'tidy_edges', or a \code{data.frame} with  (at
#' least) the columns \code{c("from", "to")}, and optionally, \code{c("arrow",
#' "label", "connect_from", "connect_to", "curvature")}.
#' @param layout A matrix (or data.frame) that describes the layout; see
#' \code{\link[tidySEM]{get_layout}}.
#' @param nodes Optional, object of class 'tidy_nodes', created with the
#' \code{\link[tidySEM]{get_nodes}} function, or a \code{data.frame} with (at
#' least) the column \code{c("name")}, and optionally, \code{c("shape",
#' "label")}. If set to \code{NULL} (the default), nodes are inferred from the
#' \code{layout} and \code{edges} arguments.
#' @param rect_width Width of rectangles (used to display observed variables),
#' Default: 1.2
#' @param rect_height Height of rectangles (used to display observed variables),
#' Default: 0.8
#' @param ellipses_width Width of ellipses (used to display latent variables),
#' Default: 1
#' @param ellipses_height Height of ellipses (used to display latent variables),
#' Default: 1
#' @param variance_diameter Diameter of variance circles,
#' Default: .8
#' @param spacing_x Spacing between columns of the graph, Default: 2
#' @param spacing_y Spacing between rows of the graph, Default: 2
#' @param text_size Point size of text, Default: 4
#' @param curvature  Deprecated. Was used to determine curvature of curved
#' edges, which is now being optimized.
#' @param angle Deprecated. Was used to determine node connection points.
#' @param fix_coord Whether or not to fix the aspect ratio of the graph.
#' Does not work with multi-group or multilevel models.
#' Default: TRUE.
#' @rdname graph_sem
#' @export
graph_sem.default <- function(edges = NULL,
                              layout = NULL,
                              nodes = NULL,
                              rect_width = 1.2,
                              rect_height = .8,
                              ellipses_width = 1,
                              ellipses_height = 1,
                              variance_diameter = .8,
                              spacing_x = 2,
                              spacing_y = 2,
                              text_size = 4,
                              curvature = NULL,
                              angle = NULL,
                              fix_coord = TRUE,
                              ...) {
  cl <- match.call()
  cl[[1L]] <- str2lang("tidySEM::prepare_graph")
  prep <- eval.parent(cl)
  plot(prep)
}

#' @method graph_sem lavaan
#' @param model Instead of the edges argument, it is also possible to use the
#' model argument and pass an object for which a method exists (e.g.,
#' \code{mplus.model} or \code{lavaan}).
# @param label Character, indicating which column to use for node labels. Nodes
# are labeled with mean values of the observed/latent variables they represent.
# Defaults to 'est_sig', which consists of the estimate value with significance
# asterisks.
#' @rdname graph_sem
#' @export
graph_sem.lavaan <- function(model,
                             edges = NULL,
                             layout = NULL,
                             nodes = NULL,
                             ...) {
  cl <- match.call()
  cl[[1]] <- str2lang("tidySEM::prepare_graph")
  prep <- eval.parent(cl)
  plot(prep)
}

#' @method graph_sem MxModel
#' @rdname graph_sem
#' @export
graph_sem.MxModel <- graph_sem.lavaan

#' @method graph_sem mplus.model
#' @rdname graph_sem
#' @export
graph_sem.mplus.model <- graph_sem.lavaan

#' @method graph_sem igraph
#' @rdname graph_sem
#' @export
graph_sem.igraph <- graph_sem.lavaan


#' @method graph_sem character
#' @rdname graph_sem
#' @export
graph_sem.character <- function(...){
  cl <- match.call()
  cl_list <- as.list(cl)
  if(isTRUE("model" %in% names(cl))){
    if(inherits(cl_list[["model"]], "matrix")) cl[[1L]] <- str2lang("tidySEM:::graph_sem.default")
    if(inherits(cl_list[["model"]], "character")) cl[[1L]] <- str2lang("tidySEM:::graph_sem.lavaan")
  } else {
    if(any(c("layout", "edges", "nodes") %in% names(cl))){
      cl[[1L]] <- str2lang("tidySEM:::graph_sem.default")
    } else {
      if(inherits(cl_list[[2L]], "matrix")) cl[[1L]] <- str2lang("tidySEM:::graph_sem.default")
      if(inherits(cl_list[[2L]], "character")) cl[[1L]] <- str2lang("tidySEM:::graph_sem.lavaan")
    }
  }
  #if(names(cl)[2L] == "model") cl[[1L]] <- str2lang("tidySEM:::graph_sem.lavaan")
  eval.parent(cl)
}


#' @method graph_sem mplusObject
#' @rdname graph_sem
#' @export
graph_sem.mplusObject <- graph_sem.lavaan

graph_model <- function(model, ...) {
  Args <- as.list(match.call()[-1])
  call_args <- list(x = model,
                    label = Args$label)
  if(!"edges" %in% names(Args)){
    edges <- do.call(get_edges, call_args)
    Args$edges <- edges
  }
  if(!"nodes" %in% names(Args)){
    nodes <- do.call(get_nodes, call_args)
    Args$nodes <- nodes
  }
  if(!"layout" %in% names(Args)){
    layout <- do.call(get_layout, call_args)
    Args$layout <- layout
  }
  Args[["model"]] <- NULL
  do.call(graph_sem.default, Args)
}

#' @title Prepare graph data
#' @description Prepare an object of class \code{sem_graph}, containing
#' data objects that can be rendered into a SEM graph. Using this function
#' allows
#' users to manually change the default graph specification before plotting it.
#' Input consists of (at least) a layout, and either nodes and edges, or
#' a model object.
#' @param ... Additional arguments passed to and from functions.
#' @return Object of class 'sem_graph'
#' @examples
#' library(lavaan)
#' res <- sem("dist ~ speed", cars)
#' prepare_graph(res)
#' @rdname prepare_graph
#' @export
prepare_graph <- function(...){
  UseMethod("prepare_graph")
}

#' @method prepare_graph default
#' @param edges Object of class 'tidy_edges', or a \code{data.frame} with  (at
#' least) the columns \code{c("from", "to")}, and optionally, \code{c("arrow",
#' "label", "connect_from", "connect_to", "curvature")}.
#' @param layout A matrix (or data.frame) that describes the layout; see
#' \code{\link[tidySEM]{get_layout}}.
#' @param nodes Optional, object of class 'tidy_nodes', created with the
#' \code{\link[tidySEM]{get_nodes}} function, or a \code{data.frame} with (at
#' least) the column \code{c("name")}, and optionally, \code{c("shape",
#' "label")}. If set to \code{NULL} (the default), nodes are inferred from the
#' \code{layout} and \code{edges} arguments.
#' @param rect_width Width of rectangles (used to display observed variables),
#' Default: 1.2
#' @param rect_height Height of rectangles (used to display observed variables),
#' Default: 0.8
#' @param ellipses_width Width of ellipses (used to display latent variables),
#' Default: 1
#' @param ellipses_height Height of ellipses (used to display latent variables),
#' Default: 1
#' @param variance_diameter Diameter of variance circles,
#' Default: .8
#' @param spacing_x Spacing between columns of the graph, Default: 2
#' @param spacing_y Spacing between rows of the graph, Default: 2
#' @param text_size Point size of text, Default: 4
#' @param curvature  Deprecated. Was used to determine curvature of curved
#' edges, which is now being optimized.
#' @param angle Deprecated. Was used to determine node connection points.
#' @param fix_coord Whether or not to fix the aspect ratio of the graph.
#' Does not work with multi-group or multilevel models.
#' Default: TRUE.
#' @rdname prepare_graph
#' @export
prepare_graph.default <- function(edges = NULL,
                                  layout = NULL,
                                  nodes = NULL,
                                  rect_width = 1.2,
                                  rect_height = .8,
                                  ellipses_width = 1,
                                  ellipses_height = 1,
                                  variance_diameter = .8,
                                  spacing_x = 2,
                                  spacing_y = 2,
                                  text_size = 4,
                                  curvature = NULL,
                                  angle = NULL,
                                  fix_coord = TRUE,
                                  ...) {
  # Preserve historical public object fields, including the `...` symbol.
  settings <- list(rect_width = rect_width, rect_height = rect_height,
                   ellipses_width = ellipses_width,
                   ellipses_height = ellipses_height,
                   variance_diameter = variance_diameter,
                   spacing_x = spacing_x, spacing_y = spacing_y,
                   text_size = text_size, curvature = curvature,
                   angle = angle, fix_coord = fix_coord)
  if(!is.null(angle)){
    warning("Note that argument `angle` is deprecated and will be removed in future versions of `tidySEM`. Node connections are now optimized, ignoring the `angle` argument.", call. = FALSE)
  }
  if(!is.null(curvature)){
    warning("Note that argument `curvature` is deprecated and will be removed in future versions of `tidySEM`. Edge curvature is now optimized, ignoring the `curvature` argument.", call. = FALSE)
  }
  extra <- list(...)
  as_df <- function(x) {
    if (is.null(x)) return(NULL)
    out <- as.data.frame(x, stringsAsFactors = FALSE)
    factors <- vapply(out, is.factor, logical(1))
    out[factors] <- lapply(out[factors], as.character)
    out
  }
  if (is.null(layout) || !(inherits(layout, "tidy_layout") ||
                           is.matrix(layout) || is.data.frame(layout))) {
    stop("Argument 'layout' must be a matrix or data.frame.")
  }
  if(inherits(layout, what = "data.frame")){
    if (!all(c("name", "x", "y") %in% names(layout)))
      stop("`layout` needs name, x, and y columns.")
  }
  if (inherits(layout, "matrix")){
    layout <- long_layout(layout)
  }
  layout <- as_df(layout)
  layout$name <- as.character(layout$name)
  if (anyNA(layout$name) || any(!nzchar(layout$name)))
    stop("Layout node names must be nonmissing and nonempty.")
  if (!is.numeric(layout$x) || !is.numeric(layout$y) ||
      any(!is.finite(layout$x)) || any(!is.finite(layout$y)))
    stop("Layout coordinates must be finite numeric values.")

  ed <- as_df(edges)
  if (is.null(ed)) ed <- data.frame(from = character(), to = character())
  if (!all(c("from", "to") %in% names(ed)))
    stop("`edges` needs `from` and `to` columns.")
  ed$from <- as.character(ed$from)
  ed$to <- as.character(ed$to)
  nd <- as_df(nodes)
  if (is.null(nd)) {
    nd <- data.frame(name = layout$name, stringsAsFactors = FALSE)
    for (key in intersect(c("group", "level"), names(layout)))
      nd[[key]] <- layout[[key]]
  }
  if (!"name" %in% names(nd)) stop("`nodes` needs a name column.")
  nd$name <- as.character(nd$name)
  if (anyNA(nd$name) || any(!nzchar(nd$name)))
    stop("Node names must be nonmissing and nonempty.")

  dimensions <- c("group", "level")
  panel_cols <- dimensions[vapply(dimensions, function(z)
    z %in% names(nd) || z %in% names(ed) || z %in% names(layout),
    logical(1))]
  if (!length(panel_cols)) {
    if (anyDuplicated(layout$name)) stop("Duplicate layout node names.")
    return(do.call(.prepare_graph_single,
                   c(list(edges = ed, layout = layout, nodes = nd),
                     settings, extra)))
  }

  # Missing dimension COLUMN means the rows are shared across that dimension.
  # An NA VALUE in a present dimension is invalid (not a wildcard).
  for (key in panel_cols) {
    for (tab in list(nd, ed, layout)) {
      if (key %in% names(tab) && anyNA(tab[[key]]))
        stop("Missing values in panel column `", key, "` are not supported.")
    }
  }
  values <- lapply(panel_cols, function(key) {
    all_values <- unlist(lapply(list(nd, ed, layout), function(tab)
      if (key %in% names(tab)) as.character(tab[[key]]) else character()),
      use.names = FALSE)
    unique(all_values)
  })
  names(values) <- panel_cols
  if (any(lengths(values) == 0L))
    stop("A panel dimension has no values.")

  # Only observed panel combinations: do not manufacture missing
  # group-level combinations from an incomplete design.
  panel_sources <- lapply(list(nd, ed, layout), function(tab) {
    if (!nrow(tab)) return(NULL)
    known <- intersect(panel_cols, names(tab))
    if (!length(known)) return(NULL)
    unique(tab[, known, drop = FALSE])
  })
  complete_sources <- Filter(function(z) !is.null(z) &&
                               all(panel_cols %in% names(z)), panel_sources)
  if (length(complete_sources)) {
    panels <- unique(do.call(rbind, complete_sources))
  } else {
    panels <- expand.grid(values, KEEP.OUT.ATTRS = FALSE,
                          stringsAsFactors = FALSE)
  }
  # Complete sources may omit panels represented by partial sources.
  # Expand partial source combinations only when no complete source exists.
  panels <- panels[, panel_cols, drop = FALSE]
  if (!nrow(panels)) stop("No nonempty panels found.")

  subset_panel <- function(tab, panel) {
    if (is.null(tab)) return(NULL)
    keep <- rep(TRUE, nrow(tab))
    for (key in intersect(panel_cols, names(tab)))
      keep <- keep & as.character(tab[[key]]) == as.character(panel[[key]])
    tab[keep, , drop = FALSE]
  }
  identity_key <- function(tab) {
    keys <- c(intersect(panel_cols, names(tab)), "name")
    do.call(paste, c(tab[keys], sep = "\\r"))
  }
  # Duplicate identities are allowed across panels, never within a panel.
  prepared <- list()
  for (j in seq_len(nrow(panels))) {
    panel <- panels[j, , drop = FALSE]
    this_nd <- subset_panel(nd, panel)
    this_ed <- subset_panel(ed, panel)
    this_layout <- subset_panel(layout, panel)
    if (!nrow(this_nd) && !nrow(this_ed)) next

    # Layouts without group/level are reused for each matching panel.
    # Prefer panel-specific coordinates where available.
    if (!nrow(this_layout))
      stop("No layout rows available for panel ", j, ".")
    if (anyDuplicated(this_layout$name))
      stop("Duplicate layout names within panel ", j, ".")
    if (anyDuplicated(this_nd$name))
      stop("Duplicate node names within panel ", j, ".")
    if (!all(this_nd$name %in% this_layout$name))
      stop("Nodes missing from layout in panel ", j, ".")

    # The single-panel worker sees no panel columns, so names are unique.
    strip <- function(tab) tab[, setdiff(names(tab), panel_cols), drop = FALSE]
    part <- do.call(.prepare_graph_single,
                    c(list(edges = strip(this_ed), layout = strip(this_layout),
                           nodes = strip(this_nd)),
                      settings, extra))
    for (key in panel_cols) {
      part$nodes[[key]] <- rep(as.character(panel[[key]]), nrow(part$nodes))
      part$edges[[key]] <- rep(as.character(panel[[key]]), nrow(part$edges))
    }
    prepared[[length(prepared) + 1L]] <- part
  }
  if (!length(prepared)) stop("No nodes or edges to prepare.")
  # Each panel uses local coordinates; ggplot facets handle placement.
  out <- prepared[[1L]]
  out$nodes <- do.call(rbind, lapply(prepared, `[[`, "nodes"))
  out$edges <- do.call(rbind, lapply(prepared, `[[`, "edges"))
  rownames(out$nodes) <- NULL
  rownames(out$edges) <- NULL
  if (anyDuplicated(identity_key(out$nodes)))
    stop("Duplicate node identity after panel preparation.")
  out$layout <- NULL
  class(out) <- "sem_graph"
  out
}


.prepare_graph_single <- function(edges = NULL,
                                  layout = NULL,
                                  nodes = NULL,
                                  rect_width = 1.2,
                                  rect_height = .8,
                                  ellipses_width = 1,
                                  ellipses_height = 1,
                                  variance_diameter = .8,
                                  spacing_x = 2,
                                  spacing_y = 2,
                                  text_size = 4,
                                  curvature = NULL,
                                  angle = NULL,
                                  fix_coord = TRUE,
                                  ...) {
  # Preserve the public object structure and original formal arguments.
  Args <- list(
    edges = edges, layout = layout, nodes = nodes,
    rect_width = rect_width, rect_height = rect_height,
    ellipses_width = ellipses_width, ellipses_height = ellipses_height,
    variance_diameter = variance_diameter,
    spacing_x = spacing_x, spacing_y = spacing_y,
    text_size = text_size, curvature = curvature,
    angle = angle, fix_coord = fix_coord
  )
  Args[["..."]] <- as.name("...")

  scalar_positive <- function(value, name) {
    if (!is.numeric(value) || length(value) != 1L ||
        !is.finite(value) || value <= 0) {
      stop("`", name, "` must be a finite positive number.", call. = FALSE)
    }
  }
  for (nm in c("rect_width", "rect_height", "ellipses_width",
               "ellipses_height", "spacing_x", "spacing_y")) {
    scalar_positive(get(nm, inherits = FALSE), nm)
  }

  convert_factors <- function(df) {
    is_factor <- vapply(df, is.factor, logical(1))
    df[is_factor] <- lapply(df[is_factor], as.character)
    df
  }
  fill_missing <- function(df, column, value) {
    if (!column %in% names(df)) df[[column]] <- rep(value, nrow(df))
    df
  }
  validate_names <- function(df, what) {
    if (!"name" %in% names(df)) stop("`", what, "` needs a `name` column.")
    df$name <- as.character(df$name)
    if (anyNA(df$name) || any(!nzchar(df$name)))
      stop("`", what, "` contains missing or empty names.")
    if (anyDuplicated(df$name))
      stop("Duplicate names in `", what, "`: ",
           paste(unique(df$name[duplicated(df$name)]), collapse = ", "))
    df
  }

  # Convert matrix/wide layout once, and validate tidy_layout as well.
  if (is.matrix(layout)) {
    layout <- long_layout(layout)
  } else if (is.data.frame(layout)) {
    if (!all(c("x", "y", "name") %in% names(layout))) stop("Layout must contain columns 'x', 'y', and 'name'.", call. = FALSE)
  } else {
    stop("Argument 'layout' must be a matrix or data.frame.", call. = FALSE)
  }
  layout <- convert_factors(as.data.frame(layout))
  layout <- validate_names(layout, "layout")
  class(layout) <- unique(c("tidy_layout", class(layout)))
  if (!all(c("x", "y") %in% names(layout)))
    stop("`layout` must contain `x` and `y`.")
  for (nm in c("x", "y")) {
    if (!is.numeric(layout[[nm]]) || any(!is.finite(layout[[nm]])))
      stop("`layout$", nm, "` must contain finite numeric coordinates.")
  }

  df_edges <- if (is.null(edges)) {
    data.frame(from = character(), to = character(), stringsAsFactors = FALSE)
  } else convert_factors(as.data.frame(edges))
  if (!all(c("from", "to") %in% names(df_edges)))
    stop("Argument 'edges' must have columns 'from' and 'to'.")
  df_edges$from <- as.character(df_edges$from)
  df_edges$to <- as.character(df_edges$to)
  valid_edge <- !is.na(df_edges$from) & !is.na(df_edges$to) &
    df_edges$from %in% layout$name & df_edges$to %in% layout$name
  if (any(!valid_edge)) {
    message("Some edges involve nodes not in layout. These were dropped.")
    df_edges <- df_edges[valid_edge, , drop = FALSE]
  }

  # Infer from *names*, never from unlist(layout), which includes coordinates.
  if (is.null(nodes)) {
    df_nodes <- data.frame(name = layout$name, shape = "rect",
                           label = layout$name, stringsAsFactors = FALSE)
  } else {
    df_nodes <- convert_factors(as.data.frame(nodes))
    df_nodes <- validate_names(df_nodes, "nodes")
  }
  if (!"name" %in% names(df_nodes)) stop("`nodes` needs a `name` column.")
  # Keep user-specified node order; fill missing x/y individually.
  # Retain only nodes present in the layout
  idx <- match(df_nodes$name, layout$name)

  keep <- !is.na(idx)

  df_nodes <- df_nodes[keep, , drop = FALSE]
  idx <- idx[keep]

  # Fill missing coordinates from layout
  for (nm in c("x", "y")) {
    if (!nm %in% names(df_nodes)) {
      df_nodes[[nm]] <- layout[[nm]][idx]
    } else {
      missing <- is.na(df_nodes[[nm]])
      df_nodes[[nm]][missing] <- layout[[nm]][idx[missing]]
    }
  }

  # Drop edges involving excluded nodes
  df_edges <- df_edges[
    df_edges$from %in% df_nodes$name &
      df_edges$to %in% df_nodes$name,
    ,
    drop = FALSE
  ]
  for (nm in c("x", "y")) {
    if (!nm %in% names(df_nodes)) {
      df_nodes[[nm]] <- layout[[nm]][idx]
    } else {
      if (!is.numeric(df_nodes[[nm]]))
        stop("`nodes$", nm, "` must be numeric.")
      missing <- is.na(df_nodes[[nm]])
      df_nodes[[nm]][missing] <- layout[[nm]][idx[missing]]
    }
    if (any(!is.finite(df_nodes[[nm]])))
      stop("Non-finite node coordinates in `", nm, "`.")
  }
  df_nodes <- fill_missing(df_nodes, "label", "")
  missing_label <- is.na(df_nodes$label) | !nzchar(as.character(df_nodes$label))
  df_nodes$label[missing_label] <- df_nodes$name[missing_label]
  df_nodes <- fill_missing(df_nodes, "shape", "rect")
  df_nodes$shape[is.na(df_nodes$shape)] <- "rect"
  if (!all(df_nodes$shape %in% c("rect", "none", "oval", "ellipse")))
    stop("Unsupported node shape: ",
         paste(unique(df_nodes$shape[!df_nodes$shape %in%
                                       c("rect", "none", "oval", "ellipse")]),
               collapse = ", "))


  # Normalize names
  df_nodes <- .normalize_sem_aesthetics(df_nodes)
  df_edges <- .normalize_sem_aesthetics(df_edges)

  df_nodes <- .fill_sem_aesthetic_defaults(df_nodes, kind = "node")
  df_edges <- .fill_sem_aesthetic_defaults(df_edges, kind = "edge")

  # One-time layout scaling: node widths/heights are already plot units.
  df_nodes$x <- df_nodes$x * spacing_x
  df_nodes$y <- df_nodes$y * spacing_y

  # Per-node width/height override global shape-specific dimensions.
  oval <- df_nodes$shape %in% c("oval", "ellipse")
  default_width <- ifelse(oval, ellipses_width, rect_width)
  default_height <- ifelse(oval, ellipses_height, rect_height)
  if (!"width" %in% names(df_nodes)) df_nodes$width <- default_width
  if (!"height" %in% names(df_nodes)) df_nodes$height <- default_height
  for (nm in c("width", "height")) {
    if (!is.numeric(df_nodes[[nm]]))
      stop("`nodes$", nm, "` must be numeric.")
    missing <- is.na(df_nodes[[nm]])
    df_nodes[[nm]][missing] <- if (nm == "width")
      default_width[missing] else default_height[missing]
    if (any(!is.finite(df_nodes[[nm]]) | df_nodes[[nm]] <= 0))
      stop("All node `", nm, "` values must be finite and positive.")
  }
  df_nodes$node_xmin <- df_nodes$x - df_nodes$width / 2
  df_nodes$node_xmax <- df_nodes$x + df_nodes$width / 2
  df_nodes$node_ymin <- df_nodes$y - df_nodes$height / 2
  df_nodes$node_ymax <- df_nodes$y + df_nodes$height / 2

  # Track explicitly supplied sides before filling missing values.
  supplied_from <- if ("connect_from" %in% names(df_edges))
    !is.na(df_edges$connect_from) else rep(FALSE, nrow(df_edges))
  supplied_to <- if ("connect_to" %in% names(df_edges))
    !is.na(df_edges$connect_to) else rep(FALSE, nrow(df_edges))

  # Only compute absent connection sides. Do not overwrite user choices.
  df_edges <- fill_missing(df_edges, "arrow", "last")
  df_edges <- fill_missing(df_edges, "label", "")
  df_edges <- fill_missing(df_edges, "curvature", 0)
  df_edges <- fill_missing(df_edges, "connect_from", NA_character_)
  df_edges <- fill_missing(df_edges, "connect_to", NA_character_)
  if (nrow(df_edges)) {
    if (!all(df_edges$arrow %in% c("first", "last", "both", "none")))
      stop("Invalid edge arrow value.")
    missing_connection <- is.na(df_edges$connect_from) |
      is.na(df_edges$connect_to)
    if (any(missing_connection)) {
      connect_cols <- .determine_connections(df_nodes, df_edges)
      if (nrow(connect_cols) != nrow(df_edges) || ncol(connect_cols) < 2L)
        stop("`.determine_connections()` returned an incompatible result.")
      df_edges$connect_from[is.na(df_edges$connect_from)] <-
        connect_cols[is.na(df_edges$connect_from), 1L]
      df_edges$connect_to[is.na(df_edges$connect_to)] <-
        connect_cols[is.na(df_edges$connect_to), 2L]
    }
    # Variances use the same side at both endpoints.
    self_variance <- df_edges$from == df_edges$to & df_edges$arrow == "both"
    df_edges$connect_to[self_variance] <- df_edges$connect_from[self_variance]

    # Optimize only edges with unspecified curvature.
    auto <- which(
      is.na(df_edges$curvature) &
        df_edges$from != df_edges$to
    )

    for (i in auto) {
      df_edges$curvature[i] <- .optimize_edge_curvature(
        edge = df_edges[i, , drop = FALSE],
        df_nodes = df_nodes,
        df_edges = df_edges[-i, , drop = FALSE]
      )
    }
    # Automatic variance positioning only for loops without supplied sides.
    # Keep user-supplied variance sides unchanged.
    if (any(self_variance & !supplied_from & !supplied_to) &&
        exists("reposition_variances", mode = "function")) {
      original_from <- df_edges$connect_from
      original_to <- df_edges$connect_to
      placed <- reposition_variances(df_edges = df_edges, df_nodes = df_nodes)
      auto_variance <- self_variance & !supplied_from & !supplied_to
      df_edges$connect_from[auto_variance] <- placed$connect_from[auto_variance]
      df_edges$connect_to[auto_variance] <- placed$connect_to[auto_variance]
    }

  }

  df_nodes <- fill_missing(df_nodes, "show", TRUE)
  df_edges <- fill_missing(df_edges, "show", TRUE)
  order_nodes <- c("name", "shape", "label", "group", "level", "x", "y",
                   "node_xmin", "node_xmax", "node_ymin", "node_ymax")
  order_edges <- c("from", "to", "label", "group", "level", "arrow",
                   "curvature", "connect_from", "connect_to")
  df_nodes <- df_nodes[, c(intersect(order_nodes, names(df_nodes)),
                           setdiff(names(df_nodes), order_nodes)), drop = FALSE]
  df_edges <- df_edges[, c(intersect(order_edges, names(df_edges)),
                           setdiff(names(df_edges), order_edges)), drop = FALSE]

  Args$nodes <- df_nodes
  Args$edges <- df_edges
  Args$layout <- NULL
  class(Args) <- "sem_graph"
  Args
}

#' @method prepare_graph lavaan
#' @param model Instead of the edges argument, it is also possible to use the
#' model argument and pass an object for which a method exists (e.g.,
#' \code{mplus.model} or \code{lavaan}).
# @param label Character, indicating which column to use for node labels. Nodes
# are labeled with mean values of the observed/latent variables they represent.
# Defaults to 'est_sig', which consists of the estimate value with significance
# asterisks.
#' @rdname prepare_graph
#' @export
prepare_graph.lavaan <- function(
    model,
    edges = NULL,
    layout = NULL,
    nodes = NULL,
    ...
) {

  # Capture unevaluated arguments.
  dots <- match.call(expand.dots = FALSE)[["..."]]

  if (is.null(dots)) {
    dots <- pairlist()
  }

  dots <- as.list(dots)

  pass_args <- c("label", "digits", "columns")

  # Environment containing the evaluated model,
  # with access to the original caller's variables.
  eval_env <- new.env(parent = parent.frame())
  eval_env$model <- model

  # Construct getter calls without evaluating label.
  call_getter <- function(fun, args) {

    cl <- as.call(c(
      list(as.name(fun), x = quote(model)),
      args
    ))

    eval(cl, envir = eval_env)
  }

  # Generate edges, nodes, and layout.
  if (is.null(edges)) {
    edges <- call_getter(
      "get_edges",
      dots[names(dots) %in% pass_args]
    )
  }

  if (is.null(nodes)) {
    nodes <- call_getter(
      "get_nodes",
      dots[names(dots) %in% pass_args]
    )
  }

  if (is.null(layout)) {
    layout <- call_getter(
      "get_layout",
      dots[!names(dots) %in% pass_args]
    )
  }

  # Forward ordinary evaluated arguments.
  remaining <- dots[
    !names(dots) %in% pass_args
  ]

  remaining <- lapply(
    remaining,
    eval,
    envir = eval_env
  )

  args <- c(
    list(
      model = model,
      edges = edges,
      nodes = nodes,
      layout = layout
    ),
    remaining
  )

  do.call(
    prepare_graph_model,
    args
  )
}
#' @method prepare_graph MxModel
#' @rdname prepare_graph
#' @export
prepare_graph.MxModel <- function(model, ...) {
  cl <- match.call()
  tab <- table_results(model, columns = NULL)
  if(!"edges" %in% names(cl)) cl[["edges"]] <- get_edges(tab)
  if(!"nodes" %in% names(cl)) cl[["nodes"]] <- get_nodes(tab)
  if(!"layout" %in% names(cl)) cl[["layout"]] <- get_layout(tab)
  cl[["model"]] <- NULL
  cl[[1]] <- str2lang("tidySEM::prepare_graph")
  eval.parent(cl)
}

#' @method prepare_graph character
#' @rdname prepare_graph
#' @export
prepare_graph.character <- function(...){
  cl <- match.call()
  if(names(cl)[2L] == "model") cl[[1L]] <- str2lang("tidySEM:::prepare_graph.lavaan")
  if(names(cl)[2L] %in% c("layout", "edges", "nodes")) cl[[1L]] <- str2lang("tidySEM:::prepare_graph.default")
  if(as.list(cl[[2L]])[[1]] == "matrix") cl[[1L]] <- str2lang("tidySEM:::prepare_graph.default")
  if(as.list(cl[[2L]])[[1]] == "character") cl[[1L]] <- str2lang("tidySEM:::prepare_graph.lavaan")
  eval.parent(cl)
}

#' @method prepare_graph mplus.model
#' @rdname prepare_graph
#' @export
prepare_graph.mplus.model <- prepare_graph.lavaan

#' @method prepare_graph mplusObject
#' @rdname prepare_graph
#' @export
prepare_graph.mplusObject <- prepare_graph.lavaan

prepare_graph_model <- function(model, ...) {
  Args <- as.list(match.call()[-1])
  call_args <- list(x = model)
  if(!"edges" %in% names(Args)){
    edges <- do.call(get_edges, call_args)
    Args$edges <- edges
  }
  if(!"nodes" %in% names(Args)){
    nodes <- do.call(get_nodes, call_args)
    Args$nodes <- nodes
  }
  if(!"layout" %in% names(Args)){
    layout <- do.call(get_layout, call_args)
    Args$layout <- layout
  }
  Args[["model"]] <- NULL
  do.call(prepare_graph.default, Args)
}

#' @export
#' @importFrom stats setNames
#' @importFrom ggplot2 arrow element_blank facet_grid geom_label
#' @importFrom ggplot2 geom_rect geom_segment geom_text geom_vline ggplot labs
#' @importFrom ggplot2 theme theme_bw unit facet_wrap facet_grid coord_fixed
#' @importFrom graphics plot
#' @method plot sem_graph
#' @export
plot.sem_graph <- function(x, y, ...) {

  geometry <- .sem_graph_geometry(x)

  df_nodes <- geometry$nodes$shapes

  text_size <- x$text_size
  fix_coord <- isTRUE(x$fix_coord)

  p <- ggplot2::ggplot()

  # Edges
  p <- .plot_sem_geometry_edges(
    p,
    geometry$edges,
    text_size
  )

  # Node shapes
  p <- .plot_nodes(
    p,
    df = df_nodes,
    text_size = text_size,
    ellipses_width = x$ellipses_width,
    ellipses_height = x$ellipses_height
  )

  # Node labels
  p <- .plot_label_internal(
    p,
    geometry$nodes$labels,
    text_size
  )

  # --------------------------------------------------
  # 3. Faceting
  # --------------------------------------------------

  has_group <- "group" %in% names(df_nodes)
  has_level <- "level" %in% names(df_nodes)

  if (has_group && has_level) {

    p <- p + ggplot2::facet_grid(
      level ~ group,
      scales = "free"
    )

  } else if (has_level) {

    p <- p + ggplot2::facet_wrap(
      ~level,
      scales = "free"
    )

  } else if (has_group) {

    p <- p + ggplot2::facet_wrap(
      ~group,
      scales = "free"
    )
  }

  # --------------------------------------------------
  # 4. Coordinate system
  # --------------------------------------------------

  if (fix_coord) {

    if (!has_group && !has_level) {

      p <- p + ggplot2::coord_fixed(
        ratio = 1,
        expand = TRUE,
        clip = "on"
      )

    } else {

      p <- p + ggplot2::theme(
        aspect.ratio = 1
      )
    }
  }

  # --------------------------------------------------
  # 5. Theme
  # --------------------------------------------------

  p + ggplot2::theme(
    axis.line = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(),
    axis.title.x = ggplot2::element_blank(),
    axis.title.y = ggplot2::element_blank(),
    legend.position = "none",
    panel.background = ggplot2::element_blank(),
    panel.border = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    plot.background = ggplot2::element_blank()
  )
}

#' @importFrom stats na.omit
.connect_points <- function(df_nodes, df_edges) {
  result_names <- c(
    "edge_xmin", "edge_ymin",
    "edge_xmax", "edge_ymax"
  )
  if (nrow(df_edges) == 0L) {
    out <- as.data.frame(matrix(numeric(0), ncol = 4))
    names(out) <- result_names
    return(out)
  }

  panel_cols <- intersect(c("group", "level"), union(names(df_nodes), names(df_edges)))

  # A panel column must exist in both tables.
  missing_nodes <- setdiff(panel_cols, names(df_nodes))
  missing_edges <- setdiff(panel_cols, names(df_edges))

  if (length(missing_nodes) || length(missing_edges)) {
    stop(
      "Panel columns must be present in both nodes ",
      "and edges. Missing node columns: ",
      paste(missing_nodes, collapse = ", "),
      "; missing edge columns: ",
      paste(missing_edges, collapse = ", ")
    )
  }

  node_key <- do.call(paste, c(df_nodes[c("name", panel_cols)], sep = "|"))
  from_key <- do.call(paste, c(df_edges[c("from", panel_cols)], sep = "|"))
  to_key <- do.call(paste, c(df_edges[c("to", panel_cols)], sep = "|"))
  from_idx <- match(from_key, node_key)
  to_idx <- match(to_key, node_key)

  if (anyNA(from_idx) || anyNA(to_idx)) {
    bad <- which(is.na(from_idx) | is.na(to_idx))
    stop(
      "Could not match edge endpoints to nodes ",
      "within their respective panels. Edge rows: ",
      paste(bad, collapse = ", ")
    )
  }

  sides <- c("left", "right", "top", "bottom")

  if (anyNA(df_edges$connect_from) ||
      anyNA(df_edges$connect_to) ||
      !all(df_edges$connect_from %in% sides) ||
      !all(df_edges$connect_to %in% sides)) {

    stop("Connection sides must be one of: ", paste(sides, collapse = ", "))
  }

  get_anchor <- function(indices, sides) {

    x <- df_nodes$x[indices]
    y <- df_nodes$y[indices]

    x[sides == "left"] <-
      df_nodes$node_xmin[indices[sides == "left"]]

    x[sides == "right"] <-
      df_nodes$node_xmax[indices[sides == "right"]]

    y[sides == "top"] <-
      df_nodes$node_ymax[indices[sides == "top"]]

    y[sides == "bottom"] <-
      df_nodes$node_ymin[indices[sides == "bottom"]]

    cbind(x, y)
  }

  from <- get_anchor(
    from_idx,
    as.character(df_edges$connect_from)
  )

  to <- get_anchor(
    to_idx,
    as.character(df_edges$connect_to)
  )

  out <- data.frame(
    edge_xmin = from[, 1],
    edge_ymin = from[, 2],
    edge_xmax = to[, 1],
    edge_ymax = to[, 2]
  )

  rownames(out) <- NULL

  out
}

matrix_to_nodes <- function(nodes, shape){
  nodes_long <- setNames(as.data.frame.table(nodes), c("y", "x", "label"))
  nodes_long[1:2] <- lapply(nodes_long[1:2], as.numeric)
  nodes_long$y <- (max(nodes_long$y)+1)-nodes_long$y
  nodes_long$label <- as.character(nodes_long$label)
  nodes_long$shape <- as.vector(shape)
  nodes_long <- nodes_long[!nodes_long$label == "", ]
  nodes_long
}

globalVariables(c("name", "..count.."))

#' @title Extract nodes from a SEM model object
#' @description Attempts to extract nodes from a SEM model object, where nodes
#' are defined as observed or latent variables.
#' @param x A model object of class \code{mplusObject} or \code{lavaan}.
#' @param label Either a character, indicating which column to use for node
#' labels, or an expression. See Details.
#' Defaults to \code{paste(name, est_sig, sep = "\n"},
#' which gives the node name followed by the estimated value with
#' significance asterisks.
#' @param ... Additional parameters passed to \code{\link{table_results}}. For
#' example, users can pass the \code{digits} argument to control the number of
#' digits in the node label, or pass the \code{columns} argument to retain
#' auxiliary columns in the \code{tidy_nodes data.frame} for further processing
#' (see Examples).
#' @details The function \code{get_nodes} identifies all dependent and
#' independent variables in the model as nodes. If a mean structure / intercepts
#' are included in the model, the output of \code{table_results} for those
#' means / intercepts is used to label the nodes.
#'
#' ## Custom labels
#'
#' One way to create custom node labels is by passing an expression to
#' \code{label}, as in the default value of the argument. When an expression is
#' passed to \code{label}, it is evaluated in the context of a \code{data.frame}
#' containing the results
#' of a call to \code{\link{table_results}} on the \code{x} argument, with an
#' additional column labeled \code{name}, which contains the node names.
#'
#' Another way to create custom labels is by requesting auxiliary variables
#' using the \code{columns} argument (which is passed to
#' \code{\link{table_results}}), and then using these columns to construct a new
#' label. See examples.
#' @return An object of class 'tidy_nodes'
#' @examples
#' # Standard use extracts node names and shape
#' # (rect for observed, oval for latent)
#' library(lavaan)
#' res <- sem("dist ~ speed", cars)
#' get_nodes(res)
#'
#' # To label nodes with mean values, include meanstructure in the model
#' # Note that it is possible to pass the argument 'digits' to table_results
#' # through '...'
#' res <- sem("dist ~ speed", cars, meanstructure = TRUE)
#' get_nodes(res, digits = 3)
#'
#' # Pass an expression to the 'label' argument for custom labels
#' get_nodes(res, label = paste0(name, " ", est_sig, "\n", confint))
#'
#' # Pass the argument 'columns' to table_results through '...' to retain
#' # auxiliary columns for further processing
#' nod <- get_nodes(res, columns = c("est_sig", "confint"))
#' nod
#' nod <- within(nod, {label <- paste0(name, " ", est_sig, "\n", confint)})
#' nod
#' @rdname get_nodes
#' @keywords tidy_graph
#' @export
get_nodes <- function(x, label = paste2(name, est_sig, sep = "\n"), ...){
  UseMethod("get_nodes", x)
}



#' @method get_nodes lavaan
#' @export
#' @importFrom lavaan parameterTable lavInspect
get_nodes.lavaan <- function(x, label = paste2(name, est_sig, sep = "\n"), ...){
  dots <- list(...)
  cl <- match.call()
  cl["columns"] <- list(NULL)
  cl[[1L]] <- str2lang("tidySEM::table_results")
  cl$x <- eval.parent(cl)
  if("columns" %in% names(dots)){
    cl["columns"] <- dots["columns"]
  }
  cl[[1L]] <- str2lang("tidySEM::get_nodes")
  eval.parent(cl)
}

#' @method get_nodes default
#' @export
get_nodes.default <- function(x, label = paste2(name, est_sig, sep = "\n"), ...){
  dots <- list(...)
  cl <- match.call()
  cl["columns"] <- list(NULL)
  cl[[1L]] <- str2lang("tidySEM::table_results")
  cl$x <- tryCatch(eval.parent(cl), error = function(e){
    stop("Could not extract nodes from object.")
  })
  if("columns" %in% names(dots)){
    cl["columns"] <- dots["columns"]
  }
  cl[[1L]] <- str2lang("tidySEM::get_nodes")
  eval.parent(cl)
}

#' @method get_nodes mplusObject
#' @export
get_nodes.mplusObject <- get_nodes.lavaan


#' @method get_nodes MxModel
#' @export
get_nodes.MxModel <- get_nodes.lavaan


#' @method get_nodes mplus.object
#' @export
get_nodes.mplus.object <- get_nodes.lavaan

#' @method get_nodes mplus.model
#' @export
get_nodes.mplus.model <- get_nodes.lavaan

#' @method get_nodes tidy_results
#' @export
get_nodes.tidy_results <- function(x, label = paste2(name, est_sig, sep = "\n"), label_name = TRUE, ...){
  dots <- list(...)
  cl <- match.call()
  cl[[1L]] <- str2lang("tidySEM:::get_nodes.tidy_results")
  if("group" %in% names(x)){
    x_list <- lapply(unique(x$group), function(i){
      cl$x <- x[x$group == i, -which(names(x) == "group")]
      tmp <- eval(cl)
      if(!is.null(tmp)){
        tmp$group <- i
      }
      tmp
    })
    return(bind_list(x_list))
  }
  if("level" %in% names(x)){
    x_list <- lapply(unique(x$level), function(i){
      cl$x <- x[x$level == i, -which(names(x) == "level")]
      tmp <- eval(cl)
      tmp$name <- paste0(tmp$name, ".", i)
      tmp$level <- i
      tmp
    })
    return(bind_list(x_list))
  }
  if(!all(c("lhs", "rhs", "op") %in% names(x))){
    if(!"label" %in% names(x)){
      stop("Could not extract edges; re-run with table_results(columns = NULL).")
    }
    addcols <- .labeltolhsrhsop(x$label)
    x <- cbind(x, addcols[, names(addcols)[!names(addcols) %in% names(x)], drop = FALSE])
  }

  latent <- unique(x$lhs[x$op == "=~"])
  obs <- unique(c(x$lhs[x$op %in% c("~~", "~", "~1")], x$rhs[x$op %in% c("~")]))
  nodes <- unique(c(latent, obs))
  if(length(nodes) < 1) return(NULL)
  nodes <- data.frame(name = unique(nodes), shape = c("rect", "oval")[(unique(nodes) %in% latent)+1], stringsAsFactors = FALSE)
  #nodes$label <- nodes$name

  # Make a data.frame based on the information about node means
  node_df <- x[x$op == "~1", ]
  if("label" %in% names(node_df)){
    names(node_df)[names(node_df) == "label"] <- "label_results"
  }
  # Keep only rows corresponding to the identified nodes
  node_rows <- match(nodes$name, node_df$lhs)
  if(any(!is.na(node_rows))){
    node_df$name <- node_df$lhs
    nodes <- merge(nodes, node_df, by = "name", all.x = TRUE)
  }
  # If the user asked for a label
  # Check if the label is an expression. If it is, substitute it
  if (inherits(suppressWarnings(try(label, silent = TRUE)), "try-error")) {
    label <- substitute(label)
  }

  if(is.character(label)){
    if(!is.null(nodes[[label]])){
      nodes[["label"]] <- nodes[[label]]
    } else {
      nodes[["label"]] <- nodes[["name"]]
    }
  } else {
    if(!is.null(label)){
      nodes[["label"]] <- tryCatch(eval(label, envir = nodes), error = function(e){
        nodes[["name"]]
      })
    } else {
      nodes[["label"]] <- ""
    }
  }

  # Retain all requested columns
  if(is.null(dots[["columns"]])){
    keep_cols <- unique(c(c("name", "shape", "label"), names(node_df)[names(node_df) %in% names(nodes)]))
  } else {
    keep_cols <- c(c("name", "shape", "label"), dots[["columns"]])
  }

  nodes <- nodes[, keep_cols, drop = FALSE]
  class(nodes) <- c("tidy_nodes", class(nodes))
  nodes
}

#' @title Extract edges from a SEM model object
#' @description Attempts to extract edges from a SEM model object, where edges
#' are defined as regression paths and covariances between variables (nodes).
#' @param x A model object of class \code{mplusObject} or \code{lavaan}.
#' @param label Either a character, indicating which column to use for edge
#' labels, or an expression. See Details.
#' Defaults to \code{"est_sig"},
#' which labels edges with the estimated value with significance asterisks,
#' as obtained from \code{\link{table_results}}. See Details and examples for
#' more information.
#' @param ... Additional parameters passed to \code{\link{table_results}}. For
#' example, users can pass the \code{digits} argument to control the number of
#' digits in the edge label, or pass the \code{columns} argument to retain
#' auxiliary columns in the \code{tidy_edges data.frame} for further processing
#' (see Examples).
#' @details The function \code{get_edges} identifies all regression paths,
#' latent variable definitions, and covariances in the model as edges.
#' The output of \code{table_results} for those
#' paths is used to label the edges.
#'
#' ## Custom labels
#'
#' One way to create custom edge labels is by passing an expression to
#' \code{label}. When an expression is
#' passed to \code{label}, it is evaluated in the context of a \code{data.frame}
#' containing the results
#' of a call to \code{\link{table_results}} on the \code{x} argument.
#'
#' Another way to create custom labels is by requesting auxiliary variables
#' using the \code{columns} argument (which is passed to
#' \code{\link{table_results}}), and then using these columns to construct a new
#' label. See examples.
#' @return An object of class 'tidy_edges'
#' @examples
#' # Standard use
#' library(lavaan)
#' res <- sem("dist ~ speed", cars)
#' get_edges(res)
#'
#' # Pass an expression to the 'label' argument for custom labels
#' get_edges(res, label = paste(est_sig, confint))
#'
#' # Pass the argument 'columns' to table_results through '...' to retain
#' # auxiliary columns for further processing
#' edg <- get_edges(res, columns = c("est_sig", "confint"))
#' edg
#' edg <- within(edg, {label <- paste(est_sig, confint)})
#' edg
#' @rdname get_edges
#' @keywords tidy_graph
#' @export
get_edges <- function(x, label = "est_sig", ...){
  UseMethod("get_edges", x)
}

#' @method get_edges default
#' @export
get_edges.default <- function(x, label = paste2(name, est_sig, sep = "\n"), ...){
  dots <- list(...)
  cl <- match.call()
  cl["columns"] <- list(NULL)
  cl[[1L]] <- str2lang("tidySEM::table_results")
  cl$x <- tryCatch(eval.parent(cl), error = function(e){
    stop("Could not extract edges from object.")
  })
  if("columns" %in% names(dots)){
    cl["columns"] <- dots["columns"]
  }
  cl[[1L]] <- str2lang("tidySEM:::get_edges.tidy_results")
  eval.parent(cl)
}



#' @method get_edges lavaan
#' @export
get_edges.lavaan <- function(x, label = "est_sig", ...){
  dots <- list(...)
  cl <- match.call()
  #cl[["label"]] <- force(label)
  cl[[1L]] <- str2lang("tidySEM::table_results")
  cl["columns"] <- list(NULL)
  cl$x <- eval.parent(cl)
  if("columns" %in% names(dots)){
    cl["columns"] <- dots["columns"]
  }
  cl[[1L]] <- str2lang("tidySEM:::get_edges.tidy_results")
  eval.parent(cl)
}

#' @method get_edges mplusObject
#' @export
get_edges.mplusObject <- get_edges.lavaan

#' @method get_edges MxModel
#' @export
get_edges.MxModel <- get_edges.lavaan

#' @method get_edges mplus.object
#' @export
get_edges.mplus.object <- get_edges.lavaan

#' @method get_edges mplus.model
#' @export
get_edges.mplus.model <- get_edges.lavaan

#' @method get_edges tidy_results
#' @export
get_edges.tidy_results <- function(x, label = "est_sig", ...) {
  dots <- list(...)
  cl <- match.call()
  cl[[1L]] <- quote(tidySEM::get_edges)
  if (!is.null(attr(x, "user_specified"))) {
    x$show <- attr(x, "user_specified")
  }
  if ("group" %in% names(x)) {
    x_list <- lapply(na.omit(unique(x$group)), function(i) {
      cl$x <- x[x$group == i, names(x) != "group", drop = FALSE]

      tmp <- eval.parent(cl)

      if (!is.null(tmp)) {
        tmp$group <- i
      }
      tmp
    })
    return(bind_list(x_list))
  }
  if ("level" %in% names(x)) {
    x_list <- lapply(na.omit(unique(x$level)), function(i) {
      cl$x <- x[x$level == i, names(x) != "level", drop = FALSE]

      tmp <- eval.parent(cl)

      if (!is.null(tmp)) {
        tmp$from <- paste0(tmp$from, ".", i)
        tmp$to <- paste0(tmp$to, ".", i)
        tmp$level <- i
      }

      tmp
    })

    return(bind_list(x_list))
  }
  if(!all(c("lhs", "rhs", "op") %in% names(x))){
    if(!"label" %in% names(x)){
      stop("Could not extract edges; re-run with table_results(columns = NULL).")
    }
    addcols <- .labeltolhsrhsop(x$label)
    x <- cbind(x, addcols[, names(addcols)[!names(addcols) %in% names(x)], drop = FALSE])
  }
  if("label" %in% names(x)){
    names(x)[names(x) == "label"] <- "label_results"
  }
  x <- x[x$op %in% c("~", "~~", "=~"), ]
  if(nrow(x) < 1) return(NULL)
  keep_cols <- names(x)
  x$from <- x$lhs
  x$to <- x$rhs
  x$arrow <- NA
  x$arrow[x$op == "~~"] <- "none" # Covariances
  x$arrow[x$op == "~~" & x$lhs == x$rhs] <- "both" # Variances
  x$arrow[x$op == "=~"] <- "last"
  x$arrow[x$op == "~"] <- "last"
  x$from[x$op == "~"] <- x$rhs[x$op == "~"]
  x$to[x$op == "~"] <- x$lhs[x$op == "~"]

  # If the user asked for a label
  # Check if the label is an expression. If it is, substitute it
  if (inherits(suppressWarnings(try(label, silent = TRUE)), "try-error")) {
    label <- substitute(label)
  }

  if(!is.null(dots[["columns"]])){
    keep_cols <- dots[["columns"]]
  }

  if(is.character(label)){
    if(!is.null(x[[label]])){
      x[["label"]] <- x[[label]]
      keep_cols <- c("label", keep_cols)
    }
  } else {
    if(!is.null(label)){
      x[["label"]] <- tryCatch(eval(label, envir = x), error = function(e){
        message("Could not construct label in get_edges().")
      })
      keep_cols <- c("label", keep_cols)
    }
  }



  tmp <- x #data.frame(as.list(x)[ c("from", "to", "arrow", keep_cols) ], check.names=FALSE)
  tmp$connect_to <- tmp$connect_from <- NA
  tmp$curvature <- 0
  tmp$curvature[x$op == "~~" & !x$lhs == x$rhs] <- NA
  tmp$linetype <- 1
  tmp$linetype[x$op == "~~" & !x$lhs == x$rhs] <- 2
  keep_cols <- unique(c("from", "to", "arrow", "label", "connect_from", "connect_to", "curvature", "linetype", keep_cols))
  keep_cols <- keep_cols[keep_cols %in% names(tmp)]
  tmp <- tmp[, keep_cols, drop = FALSE]
  class(tmp) <- c("tidy_edges", class(tmp))
  attr(tmp, "which_label") <- label
  row.names(tmp) <- NULL
  tmp
}

.labeltolhsrhsop <- function(lbl){
  op <- gsub("^.+?\\.(BY|ON|WITH)\\..+$", "\\1", lbl)
  op <- gsub("\\..*$", "", op)
  repl <- c("BY" = "=~", "WITH" = "~~", "Variances" = "~~", "Means" = "~1", "ON" = "~")
  op <- repl[op]
  lhs <- gsub("^.+\\b(.+?)\\.(BY|WITH|ON)\\..*$", "\\1", lbl)
  lhs <- gsub("^(Variances|Means)\\.", "", lhs)
  rhs <- gsub("^.+\\.(BY|WITH|ON)\\.(.+?)\\b.*$", "\\2", lbl)
  rhs <- gsub("^(Variances|Means)\\.", "", rhs)
  return(data.frame(lhs = lhs, op = op, rhs = rhs))
}

.euclidean_distance <- function(p, q){
  sqrt(sum((p - q)^2))
}

.manhattan_distance <- function(p, q){
  sum(abs(p-q))
}

match.call.defaults <- function(...) {
  call <- evalq(match.call(expand.dots = FALSE), parent.frame(1))
  myfor <- evalq(formals(), parent.frame(1))

  for(i in setdiff(names(myfor), names(call)))
    call[i] <- list( myfor[[i]] )


  match.call(sys.function(sys.parent()), call)
}

.plot_variances <- function(p, df, text_size, diameter, ...) {
  npoints <- 20
  radius <- diameter / 2
  xlabel <- xcenter <- df$edge_xmin
  ylabel <- ycenter <- df$edge_ymin
  offset <- rep(1.5, length(xcenter))
  ycenter[df$connect_from == "top"] <-
    ycenter[df$connect_from == "top"] + radius
  ylabel[df$connect_from == "top"] <-
    ylabel[df$connect_from == "top"] + diameter
  ycenter[df$connect_from == "bottom"] <-
    ycenter[df$connect_from == "bottom"] - radius
  ylabel[df$connect_from == "bottom"] <-
    ylabel[df$connect_from == "bottom"] - diameter
  offset[df$connect_from == "bottom"] <- .5
  xcenter[df$connect_from == "left"] <-
    xcenter[df$connect_from == "left"] - radius
  xlabel[df$connect_from == "left"] <-
    xlabel[df$connect_from == "left"] - diameter
  offset[df$connect_from == "left"] <- 0
  xcenter[df$connect_from == "right"] <-
    xcenter[df$connect_from == "right"] + radius
  xlabel[df$connect_from == "right"] <-
    xlabel[df$connect_from == "right"] + diameter
  offset[df$connect_from == "right"] <- 1
  df_label <- data.frame(df,
                         x = xlabel,
                         y = ylabel,
                         label = df$label)

  df_ellipse <-
    data.frame(do.call(rbind, lapply(1:nrow(df), function(this_var) {
      point_seq <-
        seq((offset[[this_var]] * pi), (2 + offset[[this_var]]) * pi, length.out = npoints) %% (2 *
                                                                                                  pi)
      matrix(
        c(
          xcenter[[this_var]] + radius * cos(point_seq),
          ycenter[[this_var]] + radius * sin(point_seq),
          rep(this_var, npoints)
        ),
        nrow = npoints,
        ncol = 3,
        dimnames = list(NULL, c("x", "y", "id"))
      )
    })))

  df$id <- 1:nrow(df)
  df_ellipse <- merge(df_ellipse, df, by = "id")

  p <- .plot_edges_internal(p, df_ellipse)
  .plot_label_internal(p, df_label, text_size)
}

.plot_nodes <- function(
    p,
    df,
    text_size,
    ellipses_width,
    ellipses_height
) {

  if (nrow(df) == 0L) return(p)
  if (!"colour" %in% names(df)) {
    df$colour <- "black"
  }

  if (!"fill" %in% names(df)) {
    df$fill <- "white"
  }

  is_rect <- !is.na(df$shape) &
    df$shape %in% c("rect", "none")

  if (any(is_rect)) {

    df_rect <- df[is_rect, , drop = FALSE]

    aesthetics <- c(
      "linetype", "linewidth",
      "colour", "fill", "alpha"
    )

    args <- c(
      list(
        data = df_rect,
        mapping = ggplot2::aes(
          xmin = .data[["node_xmin"]],
          xmax = .data[["node_xmax"]],
          ymin = .data[["node_ymin"]],
          ymax = .data[["node_ymax"]]
        )
      ),
      as.list(df_rect[ , intersect(aesthetics, names(df_rect)), drop = FALSE])
    )

    p <- p + do.call(ggplot2::geom_rect, args)
  }

  is_oval <- !is.na(df$shape) &
    df$shape %in% c("oval", "ellipse")

  if (any(is_oval)) {

    df_oval <- df[is_oval, , drop = FALSE]
    df_oval$oval_width <-
      df_oval$node_xmax - df_oval$node_xmin

    df_oval$oval_height <-
      df_oval$node_ymax - df_oval$node_ymin

    p <- .oval_node(p, df = df[is_oval, , drop = FALSE], npoints = 90)
  }

  p
}



#' @importFrom stats dist
#' @importFrom ggplot2 scale_linetype_manual
#' @importFrom ggplot2 arrow
.plot_edges <- function(
    p,
    df,
    text_size = 5,
    npoints = 101,
    ...
) {

  if (nrow(df) == 0L) {
    return(p)
  }

  if (npoints < 3L) {
    stop("`npoints` must be at least 3.")
  }

  if (!"label_location" %in% names(df)) {
    df$label_location <- 0.5
  }

  df$label_location[
    is.na(df$label_location)
  ] <- 0.5

  df$label_location <- pmin(
    1,
    pmax(0, df$label_location)
  )

  edge_paths <- vector("list", nrow(df))
  label_rows <- vector("list", nrow(df))

  for (i in seq_len(nrow(df))) {

    edge <- df[i, , drop = FALSE]

    A <- c(
      edge$edge_xmin,
      edge$edge_ymin
    )

    B <- c(
      edge$edge_xmax,
      edge$edge_ymax
    )

    # Points used to draw the edge
    t <- if (is.na(edge$curvature)) {
      c(0, 0.5, 1)
    } else {
      seq(0, 1, length.out = npoints)
    }

    coords <- .circular_arc(
      x0 = A[1],
      y0 = A[2],
      x1 = B[1],
      y1 = B[2],
      angle = edge$curvature,
      t = t
    )

    # Exact label position along the edge
    label_point <- .circular_arc(
      x0 = A[1],
      y0 = A[2],
      x1 = B[1],
      y1 = B[2],
      angle = edge$curvature,
      t = edge$label_location
    )

    coords$id <- i

    edge_paths[[i]] <- coords

    label_rows[[i]] <- data.frame(
      id = i,
      x = label_point$x,
      y = label_point$y
    )
  }

  df$id <- seq_len(nrow(df))

  df_edges <- do.call(
    rbind,
    edge_paths
  )

  df_edges <- cbind(
    df_edges,
    df[
      df_edges$id,
      setdiff(names(df), "id"),
      drop = FALSE
    ]
  )

  rownames(df_edges) <- NULL

  label_coords <- do.call(
    rbind,
    label_rows
  )

  df_label <- cbind(
    label_coords,
    df[
      label_coords$id,
      setdiff(names(df), "id"),
      drop = FALSE
    ]
  )

  df_label <- df_label[
    !is.na(df_label$label) &
      df_label$label != "",
    ,
    drop = FALSE
  ]

  if (!"linetype" %in% names(df_edges)) {
    df_edges$linetype <- 1
  }

  valid_arrows <- c(
    "first", "last", "both", "none"
  )

  if (!all(df_edges$arrow %in% valid_arrows)) {
    stop(
      "Invalid arrow value. Valid values are: ",
      paste(valid_arrows, collapse = ", ")
    )
  }

  p <- .plot_edges_internal(
    p,
    df_edges
  )

  .plot_label_internal(
    p,
    df_label,
    text_size
  )
}

#' @importFrom ggplot2 arrow
.plot_edges_internal <- function(p, df){
  # Prepare aesthetics ------------------------------------------------------
  if(!"linetype" %in% names(df)){
    df$linetype <- 1
    if("curvature" %in% names(df)) df$linetype[!df[["curvature"]] == 0] <- 2
  }
  if(any(df$arrow != "none")){
    df_path <- df[!df$arrow == "none", ]
    aes_args <- c(".sem_id", "arrow", "linetype", "size", "colour", "color", "alpha", "linewidth")
    aes_args <- df_path[!duplicated(df_path$.sem_id), which(names(df_path) %in% aes_args)]
    Args <- list(
      data = df_path[, c("x", "y", ".sem_id")],
      mapping = aes(x = .data[["x"]], y = .data[["y"]], group = .data[[".sem_id"]]),
      arrow = quote(ggplot2::arrow(angle = 25, length = unit(.1, "inches"), ends = "last", type = "closed")))
    for(this_path in unique(df_path$.sem_id)){
      Args$data <- df_path[df_path$.sem_id == this_path, ]
      Args$arrow[[4]] <- force(aes_args$arrow[aes_args$.sem_id == this_path])
      argslist <- c(Args, as.list(aes_args[aes_args$.sem_id == this_path, -c(1, 2), drop = FALSE]))
      p <- p + do.call(geom_path, argslist)
    }
  }
  if(any(df$arrow == "none")){
    df_path <- df[df$arrow == "none", ]
    Args <- c("linetype", "linewidth", "size", "colour", "color", "alpha")
    Args <- as.list(df_path[which(names(df_path) %in% Args)])
    Args <- c(list(
      data = df_path,
      mapping = aes(x = .data[["x"]], y = .data[["y"]], group = .data[[".sem_id"]])),
      Args)
    p <- p + do.call(geom_path, Args)
  }
  p
}

.plot_label_internal <- function(p, df, text_size) {
  names(df) <- gsub("^label_(x|y)$", "\\1", names(df))
  if (nrow(df) == 0L) return(p)

  # Determine label type for each row.
  # Missing values default to FALSE (geom_label).
  if (!"geom_text" %in% names(df)) {
    df$geom_text <- FALSE
  }

  df$geom_text <- as.logical(df$geom_text)
  df$geom_text[is.na(df$geom_text)] <- FALSE

  # Retain relevant columns
  retain_cols <- c(
    "x", "y", "label", "group", "level", "geom_text"
  )

  retain_cols <- intersect(retain_cols, names(df))

  aesthetic_cols <- grep(
    "^label_(fill|size|family|angle|fontface|hjust|vjust|lineheight|colour|color|alpha)$",
    names(df),
    value = TRUE
  )

  df <- df[
    ,
    c(retain_cols, aesthetic_cols),
    drop = FALSE
  ]

  # Remove label_ prefix
  names(df) <- sub("^label_", "", names(df))

  # Apply defaults
  if (!"fill" %in% names(df)) {
    df$fill <- "white"
  }

  if (!"size" %in% names(df)) {
    df$size <- text_size
  }

  # Render each label type separately
  for (use_geom_text in c(FALSE, TRUE)) {

    subset <- df[
      df$geom_text == use_geom_text,
      ,
      drop = FALSE
    ]

    if (nrow(subset) == 0L) next

    # Do not forward the selector as an aesthetic
    subset$geom_text <- NULL

    Args <- c(
      "fill", "size", "angle", "family",
      "fontface", "hjust", "vjust",
      "lineheight", "colour", "color", "alpha"
    )

    Args <- as.list(subset[ , intersect(names(subset), Args), drop = FALSE])

    Args <- c(
      list(
        data = subset,
        mapping = ggplot2::aes(
          x = .data[["x"]],
          y = .data[["y"]],
          label = .data[["label"]]
        )
      ),
      Args
    )

    if (use_geom_text) {

      # geom_text does not use a background fill.
      Args$fill <- NULL

      p <- p + do.call(
        ggplot2::geom_text,
        Args
      )

    } else {

      # Preserve the original default:
      # no label border.
      Args$linewidth <- NA_real_

      p <- p + do.call(
        ggplot2::geom_label,
        Args
      )
    }
  }

  p
}
#
# reposition_variances <- function(df_edges){
#   variances <- df_edges$from == df_edges$to & df_edges$arrow == "both"
#   variance_vars <- df_edges$from[variances]
#   new_loc <- mapply(function(vv, vloc){
#     tb <- table(c("top", "bottom", "left", "right",
#                   df_edges$connect_from[df_edges$from == vv & !variances],
#                   df_edges$connect_to[df_edges$to == vv & !variances]))
#     if(tb[vloc] == 1){
#       return(vloc)
#     } else {
#       tb <- names(tb)[which.min(tb)]
#       return(tb[sample.int(length(tb), 1)])
#     }
#   }, vv = variance_vars, vloc = df_edges$connect_from[variances])
#   df_edges$connect_from[variances] <- df_edges$connect_to[variances] <- new_loc
#   return(df_edges)
# }


bind_list <- function(L, ...){
  L <- L[which(!sapply(L, is.null))]
  all_names <- unique(unlist(lapply(L, names)))
  L <- lapply(L, function(x){
    x[setdiff(all_names, names(x))] <- NA
    x
  })
  Args <- c(L, list(...))
  do.call(rbind, Args)
}

#' @importFrom dbscan pointdensity
.flip_curve <- function(otherpoints, candidatepoints, eps = floor(sqrt(nrow(otherpoints)))){
  (sum(dbscan::pointdensity(rbind(otherpoints, candidatepoints[[1]]), eps = eps)) > sum(dbscan::pointdensity(rbind(otherpoints, candidatepoints[[2]]), eps = eps)))
}

.position_variance <- function(otherpoints, candidatepoints, eps = floor(sqrt(nrow(otherpoints)))){
  which.min(apply(candidatepoints, 1, function(thispoint){
    sum(dbscan::pointdensity(rbind(otherpoints, thispoint), eps = eps))
  }))
}

.reflect_points <- function(points, start, end){
  out <- points
  colnames(out) <- c("x", "y")
  if(end[1] == start[1]){
    out[1] <- end[1] - (out[1]-end[1])
    return(out)
  }
  if(end[2] == start[2]){
    out[2] <- end[2] - (out[2]-end[2])
    return(out)
  }
  b <- (end[1] * start[2] - start[1] * end[2])/(end[1] - start[1])
  m <- (end[2]-start[2])/(end[1]-start[1])
  m3 <- m1 <- diag(3)
  m1[2,3] <- b
  m3[2,3] <- -1 * b
  m2 <- matrix(c((1-m^2)/(1+m^2), 2*m/(1+m^2), 0,
                 2*m/(1+m^2), (m^2-1)/(1+m^2), 0,
                 0, 0, 1), nrow = 3, byrow = TRUE)
  m4 <- rbind(t(points), 1)
  out <- m1 %*% m2 %*% m3 %*% m4
  t(out)[,1:2]
}


.default_sem_aesthetics <- function() {
  list(
    node = list(
      linetype = 1,
      size = 1,
      linewidth = 0.5,
      colour = "black",
      color = "black",
      fill = "white",
      alpha = 1,

      label_fill = NA_character_,
      label_size = 4,
      label_family = "",
      label_fontface = "plain",
      label_hjust = 0.5,
      label_vjust = 0.5,
      label_lineheight = 1.2,
      label_colour = "black",
      label_color = "black",
      label_alpha = 1
    ),
    edge = list(
      linetype = 1,
      linewidth = 0.5,
      colour = "black",
      color = "black",
      alpha = 1,

      label_fill = NA_character_,
      label_size = 4,
      label_family = "",
      label_fontface = "plain",
      label_hjust = 0.5,
      label_vjust = 0.5,
      label_lineheight = 1.2,
      label_colour = "black",
      label_color = "black",
      label_alpha = 1
    )
  )
}

.fill_sem_aesthetic_defaults <- function(df, kind) {

  defaults <- .default_sem_aesthetics()[[kind]]

  for (nm in intersect(names(df), names(defaults))) {

    default <- defaults[[nm]]

    if (length(default) != 1L || is.na(default)) {
      next
    }

    missing <- is.na(df[[nm]])

    if (any(missing)) {
      df[[nm]][missing] <- default
    }
  }

  df
}


.edge_anchor <- function(node, side) {

  switch(
    side,
    left = c(node$node_xmin, node$y),
    right = c(node$node_xmax, node$y),
    top = c(node$x, node$node_ymax),
    bottom = c(node$x, node$node_ymin),
    stop("Unknown connection side: ", side)
  )
}

.normalize_sem_aesthetics <- function(df) {

  aliases <- c(
    color = "colour",
    label_color = "label_colour",
    size = "linewidth"
  )

  for (alias in names(aliases)) {

    canonical <- aliases[[alias]]

    if (!alias %in% names(df)) next

    if (!canonical %in% names(df)) {

      names(df)[names(df) == alias] <- canonical

    } else {

      # Canonical values take precedence.
      missing <- is.na(df[[canonical]])

      df[[canonical]][missing] <-
        df[[alias]][missing]

      df[[alias]] <- NULL
    }
  }

  df
}

.normalize_sem_edit_expr <- function(expr) {

  if (is.symbol(expr)) {
    name <- as.character(expr)
    canonical <- .normalize_sem_aesthetic(name)

    return(as.name(canonical))
  }

  if (is.call(expr)) {

    # Only normalize assignment targets, not values.
    if (identical(expr[[1L]], as.name("{"))) {
      for (i in seq.int(2L, length(expr))) {
        expr[[i]] <- .normalize_sem_edit_expr(expr[[i]])
      }
    }

    if (as.character(expr[[1L]]) %in% c("<-", "=")) {
      lhs <- expr[[2L]]

      if (is.symbol(lhs)) {
        expr[[2L]] <- as.name(
          .normalize_sem_aesthetic(as.character(lhs))
        )
      }
    }
  }

  expr
}
