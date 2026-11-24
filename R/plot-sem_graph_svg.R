sem_graph_svg <- function(
    graph,
    panel = NULL,
    selected = NULL,
    width = 900,
    height = 620
) {

  if (!is.null(panel)) {
    return(.sem_graph_svg_panel(
      graph,
      panel = panel,
      selected = selected,
      width = width,
      height = height
    ))
  }

  # Identify facet variables
  facet_cols <- intersect(
    c("group", "level"),
    names(graph$nodes)
  )

  if (!length(facet_cols)) {
    return(.sem_graph_svg_panel(
      graph,
      selected = selected,
      width = width,
      height = height
    ))
  }

  # Find all panel combinations actually present
  panels <- unique(
    graph$nodes[, facet_cols, drop = FALSE]
  )

  # Arrange panels like facet_grid(level ~ group)
  if (all(c("group", "level") %in% facet_cols)) {
    panels <- panels[
      order(panels$level, panels$group),
      ,
      drop = FALSE
    ]
  }

  # Render each panel
  panel_svgs <- lapply(seq_len(nrow(panels)), function(i) {

    this_panel <- as.list(
      panels[i, , drop = FALSE]
    )

    .sem_graph_svg_panel(
      graph,
      panel = this_panel,
      selected = selected,
      width = width,
      height = height
    )
  })

  # Arrange panels
  ncol <- if ("group" %in% facet_cols) {
    length(unique(panels$group))
  } else {
    ceiling(sqrt(nrow(panels)))
  }

  htmltools::tags$div(
    style = sprintf(
      paste0(
        "display:grid;",
        "grid-template-columns:repeat(%d,minmax(0,1fr));",
        "gap:8px;"
      ),
      ncol
    ),
    panel_svgs
  )
}

.sem_graph_svg_panel <- function(graph, panel = NULL, selected = NULL,
                                 width = 900, height = 620, padding = 28) {
  if (!inherits(graph, "sem_graph")) stop("`graph` must be a sem_graph.")
  if (!requireNamespace("htmltools", quietly = TRUE))
    stop("Install the suggested package `htmltools` to render SVG.")
  if (length(width) != 1L || !is.finite(width) || width <= 0 ||
      length(height) != 1L || !is.finite(height) || height <= 0 ||
      length(padding) != 1L || !is.finite(padding) || padding < 0 ||
      2 * padding >= min(width, height)) stop("Invalid SVG dimensions or padding.")

  # Geometry is the single source of node bounds, edge paths and label positions.
  geo <- .sem_graph_geometry(graph)
  nodes <- geo$nodes$shapes
  node_labels <- geo$nodes$labels
  paths <- geo$edges$paths
  edge_labels <- geo$edges$labels

  # Panel selection uses the ORIGINAL .sem_id, never a name-only match.
  panel_cols <- intersect(c("group", "level"), names(nodes))
  if (length(panel_cols) && is.null(panel)) {
    for (nm in panel_cols) {
      vals <- unique(as.character(nodes[[nm]]))
      if (length(vals) > 1L)
        stop("Specify `panel = list(", nm, " = ...)` for a faceted graph.")
    }
    panel <- as.list(nodes[1L, panel_cols, drop = FALSE])
  }
  if (is.list(panel) && length(panel) == 0L) {
    panel <- NULL
  }

  if (!is.null(panel)) {

    if (!is.list(panel) ||
        is.null(names(panel)) ||
        anyNA(names(panel)) ||
        any(!nzchar(names(panel)))) {
      stop(
        "`panel` must be a named list, ",
        "e.g. list(group = 'foo', level = 'within')."
      )
    }

    if (!setequal(names(panel), panel_cols) ||
        length(panel) != length(panel_cols)) {
      stop(
        "`panel` must specify exactly: ",
        paste(panel_cols, collapse = ", ")
      )
    }

    filter_panel <- function(df) {
      keep <- rep(TRUE, nrow(df))

      for (nm in names(panel)) {
        if (!nm %in% names(df)) {
          stop("Missing panel column: ", nm)
        }

        keep <- keep &
          !is.na(df[[nm]]) &
          as.character(df[[nm]]) == as.character(panel[[nm]])
      }

      df[keep, , drop = FALSE]
    }
    if (!is.null(panel)) {
      for (nm in names(panel)) {
        nodes <- filter_panel(nodes)
        node_labels <- filter_panel(node_labels)
        edge_labels <- filter_panel(edge_labels)      }
    }


    paths <- paths[
      paths$.sem_id %in% edge_labels$.sem_id,
      ,
      drop = FALSE
    ]
  }
  if (!nrow(nodes)) stop("No nodes in the requested panel.")

  # Include sampled paths and label positions in bounds (variance loops extend
  # beyond nodes); allow enough margin for label backgrounds and arrowheads.
  all_x <- c(nodes$node_xmin, nodes$node_xmax, paths$x,
             node_labels$x, edge_labels$x)
  all_y <- c(nodes$node_ymin, nodes$node_ymax, paths$y,
             node_labels$y, edge_labels$y)
  if (!all(is.finite(all_x)) || !all(is.finite(all_y)))
    stop("Non-finite geometry coordinates.")
  xr <- range(all_x); yr <- range(all_y)
  dx <- max(diff(xr), 1e-6); dy <- max(diff(yr), 1e-6)
  sx <- (width - 2 * padding) / dx
  sy <- (height - 2 * padding) / dy
  # Uniform scale ensures circles remain circular and angles are preserved.
  scale <- min(sx, sy)
  xoff <- (width - dx * scale) / 2
  yoff <- (height - dy * scale) / 2
  X <- function(v) xoff + (v - xr[1]) * scale
  Y <- function(v) height - (yoff + (v - yr[1]) * scale)

  value <- function(df, i, nm, default) {
    if (!nm %in% names(df)) return(default)
    z <- df[[nm]][i]
    if (length(z) != 1L || is.na(z)) default else z
  }
  alias <- function(df, i, primary, secondary, default) {
    value(df, i, primary, value(df, i, secondary, default))
  }
  dash <- function(v) {
    switch(as.character(v),
           "1" = "none", "solid" = "none", "2" = "6,4", "dashed" = "6,4",
           "3" = "2,3", "dotted" = "2,3", "4" = "6,3,2,3", "dotdash" = "6,3,2,3",
           "5" = "10,3", "longdash" = "10,3", "6" = "10,3,2,3",
           "twodash" = "10,3,2,3", "none")
  }
  sel <- function(kind, id) {
    if (!is.null(selected) && identical(selected$type, kind) &&
        identical(as.character(selected$index), as.character(id)))
      "sem-selected" else ""
  }
  # Physical label units: ggplot2 sizes are approximately mm; SVG CSS px are
  # 96/25.4 per mm. This is independent of graph-coordinate scaling.
  text_size <- if (length(graph$text_size) == 1L &&
                   is.finite(graph$text_size)) graph$text_size else 4
  label_tag <- function(df, i, kind) {
    label <- value(df, i, "label", "")
    if (!length(label) || !nzchar(as.character(label))) return(NULL)
    size <- as.numeric(value(df, i, "label_size", text_size)) * 96 / 25.4
    if (!is.finite(size) || size <= 0) return(NULL)
    x <- X(if (kind == "edge") df$label_x[i] else df$x[i])
    y <- Y(if (kind == "edge") df$label_y[i] else df$y[i])
    lines <- strsplit(as.character(label), "\n", fixed = TRUE)[[1L]]
    lh <- as.numeric(value(df, i, "label_lineheight", 1.15))
    if (!is.finite(lh) || lh <= 0) lh <- 1.15
    family <- value(df, i, "label_family", "sans-serif")
    face <- as.character(value(df, i, "label_fontface", "plain"))
    angle <- as.numeric(value(df, i, "label_angle", 0))
    if (!is.finite(angle)) angle <- 0
    colour <- alias(df, i, "label_colour", "label_color", "black")
    alpha <- value(df, i, "label_alpha", 1)
    text <- htmltools::tags$text(
      x = 0, y = 0, `text-anchor` = "middle", `dominant-baseline` = "middle",
      `font-size` = size, `font-family` = family,
      `font-weight` = if (face %in% c("bold", "bold.italic", "2", "4")) "bold" else "normal",
      `font-style` = if (face %in% c("italic", "bold.italic", "3", "4")) "italic" else "normal",
      fill = colour, opacity = alpha, `pointer-events` = "none",
      lapply(seq_along(lines), function(j) {
        htmltools::tags$tspan(
          x = 0,
          dy = if (j == 1L) (j - (length(lines) + 1)/2) * size * lh
          else size * lh,
          htmltools::htmlEscape(lines[j])
        )
      })
    )
    # Browser-side getBBox() can subsequently size a background exactly.
    # This estimate is deliberately isolated here rather than in geometry.
    plain <- isTRUE(value(df, i, "geom_text", FALSE))
    bg <- NULL
    if (!plain) {
      approx_width <- max(nchar(lines, type = "width"), 1) * size * 0.62 + 8
      approx_height <- length(lines) * size * lh + 6
      bg <- htmltools::tags$rect(
        x = -approx_width / 2, y = -approx_height / 2,
        width = approx_width, height = approx_height, rx = 2,
        fill = value(df, i, "label_fill", "white"),
        stroke = "none", opacity = alpha, `pointer-events` = "none")
    }
    htmltools::tags$g(
      class = paste("sem-label", paste0("sem-", kind, "-label")),
      transform = sprintf("translate(%.6f %.6f) rotate(%.6f)", x, y, -angle),
      `pointer-events` = "none", bg, text)
  }

  # SVG markers provide arrowheads for first/last/both, and are colored via
  # context-stroke on modern browsers. Markers are defined per edge so their
  # colors do not depend on browser context-stroke support.
  defs <- list()
  edge_tags <- list()
  variance_tags <- list()
  edge_label_tags <- list()
  if (nrow(paths)) {
    paths <- paths[order(paths$.sem_id, paths$.sem_t), , drop = FALSE]
    groups <- split(seq_len(nrow(paths)), factor(paths$.sem_id,
                                                 levels = unique(paths$.sem_id)))
    for (key in names(groups)) {
      j <- match(key, as.character(edge_labels$.sem_id))
      if (is.na(j)) next
      pts <- paths[groups[[key]], , drop = FALSE]
      if (nrow(pts) < 2L) next
      d <- paste0("M ", sprintf("%.6f %.6f", X(pts$x[1]), Y(pts$y[1])),
                  paste0(" L ", sprintf("%.6f %.6f", X(pts$x[-1]), Y(pts$y[-1])), collapse = ""))
      stroke <- alias(edge_labels, j, "colour", "color", "#64748b")
      lw <- alias(edge_labels, j, "linewidth", "size", 1)
      ar <- as.character(value(edge_labels, j, "arrow", "none"))
      marker <- paste0("sem-arrow-", key)
      if (ar %in% c("first", "last", "both")) {
        defs[[length(defs) + 1L]] <- htmltools::tags$marker(
          id = marker, viewBox = "0 0 10 10", refX = 9, refY = 5,
          markerWidth = 7, markerHeight = 7, orient = "auto-start-reverse",
          markerUnits = "userSpaceOnUse",
          htmltools::tags$path(d = "M 0 0 L 10 5 L 0 10 z", fill = stroke))
      }
      visible <- htmltools::tags$path(
        d = d, fill = "none", stroke = stroke,
        `stroke-width` = lw, `stroke-dasharray` = dash(value(edge_labels, j, "linetype", 1)),
        opacity = value(edge_labels, j, "alpha", 1),
        `vector-effect` = "non-scaling-stroke", `pointer-events` = "none",
        `marker-start` = if (ar %in% c("first", "both")) paste0("url(#", marker, ")") else NULL,
        `marker-end` = if (ar %in% c("last", "both")) paste0("url(#", marker, ")") else NULL,
        class = "sem-edge-visible")
      hit <- htmltools::tags$path(
        d = d, fill = "none", stroke = "transparent", `stroke-width` = 12,
        `vector-effect` = "non-scaling-stroke", `pointer-events` = "stroke",
        class = "sem-edge-hit", `data-edge` = key)
      is_variance <- all(c("from", "to") %in% names(edge_labels)) &&
        !is.na(edge_labels$from[j]) && !is.na(edge_labels$to[j]) &&
        as.character(edge_labels$from[j]) == as.character(edge_labels$to[j])
      edge_group <- htmltools::tags$g(
        class = paste("sem-edge-group", sel("edge", key)),
        visible,
        htmltools::tags$path(d = d, fill = "none", stroke = "#2563eb",
                             `stroke-width` = 3, `vector-effect` = "non-scaling-stroke",
                             `pointer-events` = "none", class = "sem-edge-highlight"),
        hit)
      if (is_variance) {
        variance_tags[[length(variance_tags) + 1L]] <- edge_group
      } else {
        edge_tags[[length(edge_tags) + 1L]] <- edge_group
      }
      # Draw labels last, above nodes and variance loops. Keep the
      # semantic edge ID on the label for selection and hit testing.
      lab <- label_tag(edge_labels, j, "edge")
      if (!is.null(lab)) {
        edge_label_tags[[length(edge_label_tags) + 1L]] <-
          htmltools::tags$g(class = "sem-edge-label-group",
                            `data-edge` = key, lab)
      }
    }
  }

  node_tags <- lapply(seq_len(nrow(nodes)), function(i) {
    cx <- X(nodes$x[i]); cy <- Y(nodes$y[i])
    w <- (nodes$node_xmax[i] - nodes$node_xmin[i]) * scale
    h <- (nodes$node_ymax[i] - nodes$node_ymin[i]) * scale
    style <- list(
      fill = value(nodes, i, "fill", "white"),
      stroke = alias(nodes, i, "colour", "color", "black"),
      `stroke-width` = alias(nodes, i, "linewidth", "size", 1),
      `stroke-dasharray` = dash(value(nodes, i, "linetype", 1)),
      opacity = value(nodes, i, "alpha", 1),
      `vector-effect` = "non-scaling-stroke")
    if (as.character(nodes$shape[i]) %in% c("oval", "ellipse")) {
      shape <- do.call(htmltools::tags$ellipse,
                       c(list(cx = cx, cy = cy, rx = w/2, ry = h/2), style))
    } else {
      shape <- do.call(htmltools::tags$rect,
                       c(list(x = cx-w/2, y = cy-h/2, width = w, height = h), style))
    }
    j <- match(nodes$.sem_id[i], node_labels$.sem_id)
    htmltools::tags$g(
      class = paste("sem-node", sel("node", nodes$.sem_id[i])),
      `data-node` = nodes$.sem_id[i],
      shape, if (!is.na(j)) label_tag(node_labels, j, "node") else NULL,
      if (as.character(nodes$shape[i]) %in% c("oval", "ellipse"))
        htmltools::tags$ellipse(cx = cx, cy = cy, rx = w/2, ry = h/2,
                                fill = "none", stroke = alias(nodes, i, "colour", "color", "black"),
                                `stroke-width` = alias(nodes, i, "linewidth", "size", 1),
                                `vector-effect` = "non-scaling-stroke", `pointer-events` = "none", class = "sem-node-outline")
      else htmltools::tags$rect(x = cx-w/2, y = cy-h/2, width = w, height = h,
                                fill = "none", stroke = alias(nodes, i, "colour", "color", "black"),
                                `stroke-width` = alias(nodes, i, "linewidth", "size", 1),
                                `vector-effect` = "non-scaling-stroke", `pointer-events` = "none", class = "sem-node-outline"))
  })

  htmltools::tags$svg(
    xmlns = "http://www.w3.org/2000/svg",
    viewBox = sprintf("0 0 %g %g", width, height),
    id = "sem-editor-svg", `data-sem-scale` = scale,
    width = "100%", height = "100%", preserveAspectRatio = "xMidYMid meet",
    htmltools::tags$defs(defs),
    htmltools::tags$style(htmltools::HTML(
      ".sem-node { cursor: grab; }\n       .sem-node > rect, .sem-node > ellipse { pointer-events: all; }\n       .sem-edge-hit { cursor: pointer; stroke: transparent !important; stroke-opacity: 0 !important; fill: none !important; }\n       .sem-edge-highlight { display: none; }\n       .sem-edge-group:hover > .sem-edge-highlight, .sem-edge-group.sem-selected > .sem-edge-highlight { display: inline; }\n       .sem-node:hover > rect, .sem-node:hover > ellipse, .sem-node.sem-selected > rect, .sem-node.sem-selected > ellipse { stroke: #2563eb !important; stroke-width: 3px !important; }")),
    htmltools::tags$script(htmltools::HTML(
      "(function(){var s=document.currentScript.closest('svg'); if(!s)return; s.addEventListener('pointerdown',function(e){var hit=e.target.closest('.sem-node, .sem-edge-hit'); if(!hit)return; var group=hit.closest('.sem-node, .sem-edge-group'); if(!group)return; s.querySelectorAll('.sem-selected').forEach(function(el){el.classList.remove('sem-selected');}); group.classList.add('sem-selected'); var node=group.classList.contains('sem-node'); var id=Number(node?group.dataset.node:hit.dataset.edge); if(window.Shiny&&typeof Shiny.setInputValue==='function'){Shiny.setInputValue(node?'selected_node':'selected_edge',id,{priority:'event'});} s.dispatchEvent(new CustomEvent('sem-select',{bubbles:true,detail:{type:node?'node':'edge',index:id}}));});})();")),
    htmltools::tags$g(edge_tags),
    htmltools::tags$g(node_tags),
    htmltools::tags$g(variance_tags),
    htmltools::tags$g(edge_label_tags)
  )
}
