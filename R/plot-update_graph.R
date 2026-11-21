#' Update an Existing SEM Graph
#'
#' Modify the nodes, edges, or plotting settings of an existing
#' structural equation model (SEM) graph without recalculating
#' its layout or automatically repositioning its edges.
#'
#' @description
#' `update_graph()` modifies an existing `sem_graph` object while
#' preserving its manually specified geometry and aesthetics.
#'
#' Unlike [prepare_graph()], which constructs a graph from a layout,
#' `update_graph()` operates on coordinates that have already been
#' scaled and prepared for plotting.
#'
#' Node bounding boxes are recalculated using the current node
#' coordinates and dimensions. Individual node dimensions take
#' precedence over the graph's global shape dimensions.
#'
#' @param x An object of class `sem_graph`.
#' @param nodes Optional data frame containing replacement node
#'   information. If `NULL`, the existing nodes are retained.
#' @param edges Optional data frame containing replacement edge
#'   information. If `NULL`, the existing edges are retained.
#' @param ... Additional graph-level plotting settings to update,
#'   such as `rect_width`, `rect_height`, `ellipses_width`,
#'   `ellipses_height`, `variance_diameter`, `text_size`,
#'   `spacing_x`, `spacing_y`, `curvature`, `angle`, or
#'   `fix_coord`.
#'
#' @details
#' The function preserves existing node positions and does not
#' apply layout spacing a second time.
#'
#' Node bounding boxes (`node_xmin`, `node_xmax`, `node_ymin`,
#' and `node_ymax`) are recalculated from node centers and
#' dimensions.
#'
#' If a node has individual `width` or `height` values, these
#' override the corresponding global shape dimensions.
#'
#' The following properties are preserved unless explicitly
#' modified:
#'
#' * Node coordinates (`x`, `y`).
#' * Edge connection sides (`connect_from`, `connect_to`).
#' * Edge curvature.
#' * Variance-loop positions.
#' * Node and edge labels.
#' * Node and edge aesthetics.
#'
#' For self-variance edges with `arrow = "both"`, the source
#' and target connection sides are kept synchronized.
#'
#' Changing `spacing_x` or `spacing_y` does not rescale
#' existing node coordinates.
#'
#' @return An updated object of class `sem_graph`.
#'
#' @seealso [prepare_graph()], [update_graph_node()]
#'
#' @examples
#' \dontrun{
#' # Construct a graph
#' g <- prepare_graph(
#'   edges = edges,
#'   nodes = nodes,
#'   layout = layout
#' )
#'
#' # Change node positions
#' nd <- g$nodes
#' nd$x[nd$name == "visual"] <- 5
#'
#' g <- update_graph(g, nodes = nd)
#'
#' # Change edge aesthetics
#' ed <- g$edges
#' ed$colour <- "black"
#' ed$colour[1] <- "steelblue"
#'
#' g <- update_graph(g, edges = ed)
#'
#' # Change global plotting settings
#' g <- update_graph(
#'   g,
#'   text_size = 5,
#'   variance_diameter = 1
#' )
#'
#' plot(g)
#' }
#'
#' @export
update_graph <- function(x, ...) {
  UseMethod("update_graph")
}

#' @export
update_graph.default <- function(x, ...) {
  stop("`x` must be a `sem_graph` object.", call. = FALSE)
}

#' @rdname update_graph
#' @export
update_graph.sem_graph <- function(x, nodes = NULL, edges = NULL, ...) {
  if (!inherits(x, "sem_graph")) {
    stop("`x` must inherit from `sem_graph`.", call. = FALSE)
  }

  settings <- list(...)
  allowed <- c("rect_width", "rect_height", "ellipses_width",
               "ellipses_height", "variance_diameter", "spacing_x",
               "spacing_y", "text_size", "curvature", "angle", "fix_coord")
  if (length(settings) &&
      (is.null(names(settings)) || anyNA(names(settings)) ||
       any(!nzchar(names(settings))) || anyDuplicated(names(settings)))) {
    stop("All settings in `...` must have unique, nonempty names.", call. = FALSE)
  }
  unknown <- setdiff(names(settings), allowed)
  if (length(unknown)) {
    stop("Unknown graph setting(s): ", paste(unknown, collapse = ", "),
         call. = FALSE)
  }

  out <- x
  for (nm in names(settings)) out[[nm]] <- settings[[nm]]

  positive_settings <- c("rect_width", "rect_height", "ellipses_width",
                         "ellipses_height", "variance_diameter",
                         "spacing_x", "spacing_y", "text_size")
  for (nm in positive_settings) {
    v <- out[[nm]]
    if (!is.numeric(v) || length(v) != 1L || !is.finite(v) || v <= 0) {
      stop("`", nm, "` must be a finite positive number.", call. = FALSE)
    }
  }
  if (!is.null(out$angle) &&
      (!is.numeric(out$angle) || length(out$angle) != 1L ||
       !is.finite(out$angle))) {
    stop("`angle` must be NULL or a finite number.", call. = FALSE)
  }
  if (!is.numeric(out$curvature) || length(out$curvature) != 1L ||
      !is.finite(out$curvature)) {
    stop("`curvature` must be a finite numeric scalar.", call. = FALSE)
  }
  if (!is.logical(out$fix_coord) || length(out$fix_coord) != 1L ||
      is.na(out$fix_coord)) {
    stop("`fix_coord` must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.null(nodes)) out$nodes <- as.data.frame(nodes)
  if (!is.null(edges)) out$edges <- as.data.frame(edges)

  nd <- out$nodes
  ed <- out$edges
  if (!is.data.frame(nd) || !all(c("name", "x", "y") %in% names(nd))) {
    stop("`nodes` must be a data frame containing `name`, `x`, and `y`.",
         call. = FALSE)
  }
  if (!is.data.frame(ed) || !all(c("from", "to") %in% names(ed))) {
    stop("`edges` must be a data frame containing `from` and `to`.",
         call. = FALSE)
  }
  factor_to_character <- function(df) {
    factor_columns <- vapply(df, is.factor, logical(1))
    df[factor_columns] <- lapply(df[factor_columns], as.character)
    df
  }
  nd <- factor_to_character(nd)
  ed <- factor_to_character(ed)
  nd$name <- as.character(nd$name)
  ed$from <- as.character(ed$from)
  ed$to <- as.character(ed$to)

  if (anyNA(nd$name) || any(!nzchar(nd$name))) {
    stop("Node names must be nonmissing and nonempty.", call. = FALSE)
  }
  # Names may repeat across groups/levels, but not within one partition.
  partition <- intersect(c("group", "level"), names(nd))
  if (length(partition)) {
    key <- do.call(paste, c(nd[c("name", partition)], sep = "\r"))
  } else {
    key <- nd$name
  }
  if (anyDuplicated(key)) {
    stop("Node identifiers must be unique within each group/level.",
         call. = FALSE)
  }
  for (nm in c("x", "y")) {
    if (!is.numeric(nd[[nm]]) || any(!is.finite(nd[[nm]]))) {
      stop("`nodes$", nm, "` must contain finite numeric coordinates.",
           call. = FALSE)
    }
  }
  if (!"shape" %in% names(nd)) nd$shape <- rep("rect", nrow(nd))
  nd$shape <- as.character(nd$shape)
  if (anyNA(nd$shape) ||
      any(!nd$shape %in% c("rect", "none", "oval", "ellipse"))) {
    stop("Node shapes must be `rect`, `none`, `oval`, or `ellipse`.",
         call. = FALSE)
  }

  # A width/height column takes precedence over the global shape defaults.
  # These values are in final plot units, not layout-grid units.
  oval <- nd$shape %in% c("oval", "ellipse")
  default_width <- ifelse(oval, out$ellipses_width, out$rect_width)
  default_height <- ifelse(oval, out$ellipses_height, out$rect_height)
  if (!"width" %in% names(nd)) nd$width <- default_width
  if (!"height" %in% names(nd)) nd$height <- default_height
  for (nm in c("width", "height")) {
    if (!is.numeric(nd[[nm]])) {
      stop("`nodes$", nm, "` must be numeric.", call. = FALSE)
    }
    missing <- is.na(nd[[nm]])
    fallback <- if (nm == "width") default_width else default_height
    nd[[nm]][missing] <- fallback[missing]
    if (any(!is.finite(nd[[nm]]) | nd[[nm]] <= 0)) {
      stop("Node `", nm, "` must be finite and positive.", call. = FALSE)
    }
  }
  nd$node_xmin <- nd$x - nd$width / 2
  nd$node_xmax <- nd$x + nd$width / 2
  nd$node_ymin <- nd$y - nd$height / 2
  nd$node_ymax <- nd$y + nd$height / 2

  if (anyNA(ed$from) || anyNA(ed$to)) {
    stop("Edge endpoints cannot be missing.", call. = FALSE)
  }
  if (nrow(ed)) {
    if (!all(ed$from %in% nd$name & ed$to %in% nd$name)) {
      stop("Every edge endpoint must identify a node.", call. = FALSE)
    }
    # For a grouped/leveled graph, endpoints must exist in the same partition.
    shared <- intersect(c("group", "level"), intersect(names(nd), names(ed)))
    if (length(shared)) {
      node_key <- do.call(paste, c(nd[c("name", shared)], sep = "\r"))
      from_key <- do.call(paste, c(list(ed$from), ed[shared], sep = "\r"))
      to_key <- do.call(paste, c(list(ed$to), ed[shared], sep = "\r"))
      if (!all(from_key %in% node_key & to_key %in% node_key)) {
        stop("Some edge endpoints are absent from their group/level.",
             call. = FALSE)
      }
    }
    if (all(c("arrow", "connect_from", "connect_to") %in% names(ed))) {
      self <- !is.na(ed$arrow) & ed$arrow == "both" & ed$from == ed$to
      ed$connect_to[self] <- ed$connect_from[self]
    }
  }

  out$nodes <- nd
  out$edges <- ed
  # A prepared graph has no unscaled layout; keep the original convention.
  out$layout <- NULL
  out
}


#' Update an Individual SEM Graph Node
#'
#' Modify the position, dimensions, label, shape, or aesthetics
#' of an individual node in an existing SEM graph.
#'
#' @description
#' `update_graph_node()` provides a convenient way to modify a
#' single node without manually editing the graph's node data
#' frame.
#'
#' The function updates the selected node and recalculates its
#' bounding box while preserving the remaining graph structure.
#'
#' @param x An object of class `sem_graph`.
#' @param name Character string identifying the node to update.
#' @param group Optional group identifier, used to distinguish
#'   nodes with the same name in grouped graphs.
#' @param level Optional level identifier, used to distinguish
#'   nodes with the same name across levels.
#' @param ... Named node properties to modify. These may include
#'   `x`, `y`, `width`, `height`, `shape`, `label`, or supported
#'   plotting aesthetics such as `colour`, `fill`, `alpha`,
#'   `linetype`, and `linewidth`.
#'
#' @details
#' Node positions are specified in the graph's existing coordinate
#' system and are not rescaled.
#'
#' Individual `width` and `height` values override the global
#' dimensions specified when the graph was constructed.
#'
#' When node dimensions or shape are modified, the node's
#' bounding box is updated automatically.
#'
#' If multiple nodes share the same name, `group` or `level`
#' can be used to identify the intended node.
#'
#' Edge endpoints, connection sides, curvature, and variance
#' positions are not automatically recalculated.
#'
#' @return An updated object of class `sem_graph`.
#'
#' @seealso [update_graph()], [prepare_graph()]
#'
#' @examples
#' \dontrun{
#' g <- prepare_graph(
#'   edges = edges,
#'   nodes = nodes,
#'   layout = layout
#' )
#'
#' # Move a node
#' g <- update_graph_node(
#'   g,
#'   name = "visual",
#'   x = 5,
#'   y = 4
#' )
#'
#' # Change its dimensions and appearance
#' g <- update_graph_node(
#'   g,
#'   name = "visual",
#'   width = 1.8,
#'   height = 1.2,
#'   fill = "lightblue",
#'   colour = "black"
#' )
#'
#' plot(g)
#' }
#'
#' @export
update_graph_node <- function(x, name, ..., group = NULL, level = NULL) {
  if (!inherits(x, "sem_graph")) stop("`x` must be a `sem_graph`.")
  nd <- x$nodes
  idx <- which(as.character(nd$name) == name)
  if (!is.null(group)) {
    if (!"group" %in% names(nd)) stop("Nodes have no `group` column.")
    idx <- idx[as.character(nd$group[idx]) %in% as.character(group)]
  }
  if (!is.null(level)) {
    if (!"level" %in% names(nd)) stop("Nodes have no `level` column.")
    idx <- idx[as.character(nd$level[idx]) %in% as.character(level)]
  }
  if (length(idx) != 1L) {
    stop("Node selection must match exactly one row.", call. = FALSE)
  }
  values <- list(...)
  if (!length(values)) return(x)
  if (is.null(names(values)) || anyNA(names(values)) ||
      any(!nzchar(names(values))) || anyDuplicated(names(values))) {
    stop("All updated properties must have unique, nonempty names.")
  }
  protected <- c("name", "group", "level", "node_xmin", "node_xmax",
                 "node_ymin", "node_ymax")
  if (any(names(values) %in% protected)) {
    stop("Identifiers and bounding boxes cannot be changed directly.")
  }
  for (nm in names(values)) {
    v <- values[[nm]]
    if (length(v) != 1L) stop("Each updated property must have length one.")
    if (!nm %in% names(nd)) {
      prototype <- if (is.numeric(v)) NA_real_ else if (is.logical(v)) NA else NA_character_
      nd[[nm]] <- rep(prototype, nrow(nd))
    }
    nd[[nm]][idx] <- v
  }
  update_graph(x, nodes = nd)
}
