edit_sem_graph <- function(graph) {

  if (!inherits(graph, "sem_graph")) {
    stop("`graph` must inherit from 'sem_graph'.")
  }

  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package `shiny` is required.")
  }

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package `ggplot2` is required.")
  }

  shiny::runApp(
    sem_graph_editor_app(graph),
    launch.browser = TRUE
  )
}

refresh_sem_graph <- function(g) {

  layout <- data.frame(
    x = as.numeric(g$nodes$x),
    y = as.numeric(g$nodes$y),
    name = as.character(g$nodes$name),
    stringsAsFactors = FALSE
  )
  if (anyNA(layout$name) || anyDuplicated(layout$name)) {
    stop("Node names must be unique and non-missing.")
  }
  if (any(!is.finite(layout$x) | !is.finite(layout$y))) {
    stop("Layout coordinates must be finite.")
  }
  class(layout) <- c("tidy_layout", "data.frame")

  prepare_graph.default(
    edges = g$edges,
    layout = layout,
    nodes = g$nodes,
    rect_width = g$rect_width,
    rect_height = g$rect_height,
    ellipses_width = g$ellipses_width,
    ellipses_height = g$ellipses_height,
    variance_diameter = g$variance_diameter,
    spacing_x = g$spacing_x,
    spacing_y = g$spacing_y,
    text_size = g$text_size,
    curvature = g$curvature,
    angle = g$angle,
    fix_coord = g$fix_coord
  )
}

sem_graph_editor_app <- function(graph) {

  library(shiny)

  sides <- c("top", "bottom", "left", "right")
  label_aesthetics <- paste0(
    "label_",
    c(
      "fill", "size", "family", "fontface",
      "hjust", "vjust", "lineheight",
      "colour", "color", "alpha"
    )
  )

  aesthetic_choices <- list(
    node = c(
      "linetype", "size", "colour", "color",
      "fill", "alpha", "linewidth"
    ),
    edge = c(
      "linetype", "size", "colour", "color",
      "alpha", "linewidth"
    )
  )

  # -------------------------------------------------------
  # Geometry helpers
  # -------------------------------------------------------

  move_node <- function(nodes, i, x, y) {

    dx <- x - nodes$x[i]
    dy <- y - nodes$y[i]

    nodes$x[i] <- x
    nodes$y[i] <- y

    for (nm in c("node_xmin", "node_xmax")) {
      nodes[[nm]][i] <- nodes[[nm]][i] + dx
    }

    for (nm in c("node_ymin", "node_ymax")) {
      nodes[[nm]][i] <- nodes[[nm]][i] + dy
    }

    nodes
  }

  anchor <- function(node, side) {

    switch(
      side,
      top = c(node$x, node$node_ymax),
      bottom = c(node$x, node$node_ymin),
      left = c(node$node_xmin, node$y),
      right = c(node$node_xmax, node$y),
      c(node$x, node$y)
    )
  }

  esc <- function(x) {
    htmltools::htmlEscape(as.character(x))
  }

  # -------------------------------------------------------
  # SVG editor
  #
  # The SVG uses graph coordinates with the y axis
  # inverted by a top-level transform.
  # -------------------------------------------------------
  curved_edge_path <- function(p, q, curvature, svg_x, svg_y) {

    # Convert endpoints to SVG coordinates
    x1 <- svg_x(p[1])
    y1 <- svg_y(p[2])
    x2 <- svg_x(q[1])
    y2 <- svg_y(q[2])

    dx <- x2 - x1
    dy <- y2 - y1

    distance <- sqrt(dx^2 + dy^2)

    if (distance < 1e-10) {
      return(sprintf("M %f %f L %f %f", x1, y1, x2, y2))
    }

    # Interpret curvature as an angle in degrees.
    # Positive curvature bends to one side;
    # negative curvature bends to the other.
    angle <- curvature * pi / 180

    # Perpendicular displacement of the control point.
    offset <- distance / 2 * tan(angle / 2)

    mx <- (x1 + x2) / 2
    my <- (y1 + y2) / 2

    cx <- mx - dy / distance * offset
    cy <- my + dx / distance * offset

    sprintf(
      "M %f %f Q %f %f %f %f",
      x1, y1, cx, cy, x2, y2
    )
  }
  make_svg <- function(g) {

    nd <- g$nodes
    ed <- g$edges

    if (!nrow(nd)) {
      return(tags$div("No nodes to display."))
    }

    xmin <- min(nd$node_xmin) - 2
    xmax <- max(nd$node_xmax) + 2
    ymin <- min(nd$node_ymin) - 2
    ymax <- max(nd$node_ymax) + 2

    width <- xmax - xmin
    height <- ymax - ymin

    # Stable coordinate transform for the editor.
    # SVG y increases downward; graph y increases upward.

    svg_x <- function(x) x - xmin
    svg_y <- function(y) ymax - y

    # -----------------------------------------------------
    # Render edges
    # -----------------------------------------------------

    edge_elements <- lapply(seq_len(nrow(ed)), function(i) {

      e <- ed[i, , drop = FALSE]

      fi <- match(e$from, nd$name)
      ti <- match(e$to, nd$name)

      if (is.na(fi) || is.na(ti)) return(NULL)

      from <- nd[fi, , drop = FALSE]
      to <- nd[ti, , drop = FALSE]

      sf <- e$connect_from
      st <- e$connect_to

      if (is.na(sf) || !sf %in% sides) sf <- "bottom"
      if (is.na(st) || !st %in% sides) st <- "top"

      p <- anchor(from, sf)
      q <- anchor(to, st)

      is_loop <- identical(
        as.character(e$from),
        as.character(e$to)
      )

      if (is_loop) {

        r <- g$variance_diameter / 2

        if (sf %in% c("top", "bottom")) {

          sign <- if (sf == "top") 1 else -1

          p <- c(from$x - r / 2, p[2])
          q <- c(from$x + r / 2, q[2])

          c1 <- p + c(-r, sign * r * 2)
          c2 <- q + c(r, sign * r * 2)

        } else {

          sign <- if (sf == "right") 1 else -1

          p <- c(p[1], from$y - r / 2)
          q <- c(q[1], from$y + r / 2)

          c1 <- p + c(sign * r * 2, -r)
          c2 <- q + c(sign * r * 2, r)
        }

        d <- sprintf(
          "M %f %f C %f %f %f %f %f %f",
          svg_x(p[1]), svg_y(p[2]),
          svg_x(c1[1]), svg_y(c1[2]),
          svg_x(c2[1]), svg_y(c2[2]),
          svg_x(q[1]), svg_y(q[2])
        )

      } else if (!is.na(e$curvature) && e$curvature != 0) {

        d <- curved_edge_path(
          p = p,
          q = q,
          curvature = e$curvature,
          svg_x = svg_x,
          svg_y = svg_y
        )

      } else {

        d <- sprintf(
          "M %f %f L %f %f",
          svg_x(p[1]), svg_y(p[2]),
          svg_x(q[1]), svg_y(q[2])
        )
      }

      colour <- if ("colour" %in% names(ed)) {
        ed$colour[i]
      } else if ("color" %in% names(ed)) {
        ed$color[i]
      } else {
        "#64748b"
      }

      if (is.na(colour)) colour <- "#64748b"

      tags$g(
        class = "sem-edge-group",

        htmltools::tags$path(
          d = d,
          fill = "none",
          stroke = colour,
          `stroke-width` = 1.5,
          `vector-effect` = "non-scaling-stroke",
          class = "sem-edge-visible",
          `pointer-events` = "none"
        ),

        htmltools::tags$path(
          d = d,
          fill = "none",
          stroke = "transparent",
          `stroke-opacity` = 0,
          `stroke-width` = 12,
          `vector-effect` = "non-scaling-stroke",
          `pointer-events` = "stroke",
          `data-edge` = i,
          class = "sem-edge-hit"
        )
      )
    })

    # -----------------------------------------------------
    # Render nodes
    # -----------------------------------------------------

    node_elements <- lapply(seq_len(nrow(nd)), function(i) {

      n <- nd[i, , drop = FALSE]

      cx <- svg_x(n$x)
      cy <- svg_y(n$y)

      w <- n$node_xmax - n$node_xmin
      h <- n$node_ymax - n$node_ymin

      colour <- if ("fill" %in% names(nd)) {
        nd$fill[i]
      } else {
        "#e8eff8"
      }

      if (is.na(colour)) colour <- "#e8eff8"

      shape <- if (n$shape %in% c("oval", "ellipse")) {
        tags$ellipse(
          cx = cx,
          cy = cy,
          rx = w / 2,
          ry = h / 2,
          fill = colour,
          stroke = "#45658b",
          `stroke-width` = 1.3
        )
      } else {
        tags$rect(
          x = cx - w / 2,
          y = cy - h / 2,
          width = w,
          height = h,
          rx = 0.04,
          fill = colour,
          stroke = "#45658b",
          `stroke-width` = 1.3
        )
      }

      tags$g(
        class = "sem-node",
        `data-node` = i,
        shape,
        tags$text(
          x = cx,
          y = cy,
          `text-anchor` = "middle",
          `dominant-baseline` = "middle",
          `font-size` = 0.22,
          fill = "#243e5d",
          esc(strsplit(n$label, "\n")[[1]][1])
        )
      )
    })

    tags$svg(
      id = "sem-editor-svg",
      viewBox = sprintf(
        "%f %f %f %f",
        0, 0, width, height
      ),
      style = paste(
        "width:100%; height:100%;",
        "touch-action:none;",
        "user-select:none;"
      ),
      tags$g(edge_elements),
      tags$g(node_elements)
    )
  }

  # -------------------------------------------------------
  # UI
  # -------------------------------------------------------

  ui <- fluidPage(

    tags$head(
      tags$style(HTML("
        .sem-editor {
          height: 620px;
          border: 1px solid #d1d5db;
          border-radius: 8px;
          overflow: hidden;
          background: white;
        }

        .sem-node {
          cursor: grab;
        }

        .sem-node:active {
          cursor: grabbing;
        }

        /* Normal appearance */
        .sem-node > rect,
        .sem-node > ellipse {
          fill: white !important;
          stroke: #333333 !important;
          stroke-width: 1.5px !important;
          vector-effect: non-scaling-stroke;
        }

        /* Hover appearance */
        .sem-node:hover > rect,
        .sem-node:hover > ellipse {
          stroke: #2563eb !important;
          stroke-width: 2.5px !important;
        }

        /* Labels */
        .sem-node > text {
          fill: #222222 !important;
          pointer-events: none;
        }

        /* Visible edge */
        .sem-edge-visible {
          fill: none !important;
          stroke-width: 1.5px;
          pointer-events: none;
          transition: stroke 0.15s, stroke-width 0.15s;
        }

        /* Invisible clickable area */
        .sem-edge-hit,
        .sem-edge-hit:hover {
          cursor: pointer;
          stroke: transparent !important;
          stroke-opacity: 0 !important;
          stroke-width: 12px;
          fill: none !important;
          pointer-events: stroke;
        }

        /* Highlight edge on hover */
        .sem-edge-group:hover .sem-edge-visible {
          stroke: #2563eb !important;
          stroke-width: 2.5px !important;
        }

        .sem-properties {
          padding: 12px;
          border: 1px solid #e5e7eb;
          border-radius: 8px;
        }
      ")),

      tags$script(HTML("
        (function() {

          let drag = null;

          function graphPoint(svg, event) {
            const pt = svg.createSVGPoint();

            pt.x = event.clientX;
            pt.y = event.clientY;

            const ctm = svg.getScreenCTM();
            if (!ctm) return null;

            return pt.matrixTransform(ctm.inverse());
          }

          // Send edits with the identity of their original element.
          // Select changes are immediate; text/numeric fields commit on
          // change (blur or Enter). No delayed timers can overwrite Undo.
          function sendSemEdit(target) {
            if (!target || !target.closest) return;
            const field = target.closest('[data-sem-field]');
            if (!field) return;
            const panel = field.closest('[data-sem-kind]');
            if (!panel) return;
            const control = document.getElementById(field.dataset.semInput);
            if (!control) return;
            let value = control.value;
            if (window.jQuery && window.jQuery(control).data('selectize')) {
              value = window.jQuery(control).data('selectize').getValue();
            }
            if (Array.isArray(value)) value = value[0] || '';
            const payload = {
              kind: panel.dataset.semKind,
              index: Number(panel.dataset.semIndex),
              field: field.dataset.semField,
              value: String(value),
              nonce: Math.random()
            };
            if (payload.field === 'aesthetic') {
              const nameInput = panel.querySelector('#property_name');
              payload.name = nameInput ? nameInput.value.trim() : '';
              if (!payload.name) return;
            }
            if (window.Shiny && typeof Shiny.setInputValue === 'function') {
              Shiny.setInputValue('property_edit', payload, {priority: 'event'});
            }
          }

          document.addEventListener('change', function(e) {
            if (e.target.closest && e.target.closest('[data-sem-field]')) {
              sendSemEdit(e.target);
            }
          }, true);

          // Selectize also sends jQuery change events.
          if (window.jQuery) {
            window.jQuery(document).on('change', '[data-sem-field] select',
              function() { sendSemEdit(this); });
          }

          function flushFocusedSemField() {
            const active = document.activeElement;
            if (active && active.closest &&
                active.closest('[data-sem-field]')) {
              sendSemEdit(active);
            }
          }

          document.addEventListener('pointerdown', function(e) {

            const node = e.target.closest('.sem-node');
            const edge = e.target.closest('.sem-edge-hit');

            if (node) {
              flushFocusedSemField();

              const svg = node.ownerSVGElement;
              const p = graphPoint(svg, e);

              if (!p) return;

              drag = {
                node: Number(node.dataset.node),
                svg: svg,
                element: node,
                start: p,
                moved: false
              };

              Shiny.setInputValue(
                'selected_node',
                drag.node,
                {priority: 'event'}
              );

              e.preventDefault();

            } else if (edge) {
              flushFocusedSemField();

              Shiny.setInputValue(
                'selected_edge',
                Number(edge.dataset.edge),
                {priority: 'event'}
              );
            }
          });

          document.addEventListener('pointermove', function(e) {

            if (!drag) return;

            const p = graphPoint(drag.svg, e);
            if (!p) return;

            const dx = p.x - drag.start.x;
            const dy = p.y - drag.start.y;

            if (Math.abs(dx) + Math.abs(dy) > 0.01) {
              drag.moved = true;
            }

            drag.element.setAttribute(
              'transform',
              'translate(' + dx + ',' + dy + ')'
            );
          });

          document.addEventListener('pointerup', function(e) {

            if (!drag) return;

            const p = graphPoint(drag.svg, e);

            if (p && drag.moved) {

              Shiny.setInputValue(
                'node_drag',
                {
                  index: drag.node,
                  dx: p.x - drag.start.x,
                  dy: -(p.y - drag.start.y),
                  nonce: Math.random()
                },
                {priority: 'event'}
              );
            }

            drag = null;
          });

        })();
      "))
    ),

    titlePanel("SEM graph editor"),

    fluidRow(

      column(
        7,
        h4("Interactive editor"),
        div(
          class = "sem-editor",
          uiOutput("editor")
        ),
        p(
          "Drag nodes to reposition them. ",
          "Click nodes or edges to edit their properties."
        )
      ),

      column(
        5,
        h4("Properties"),
        div(
          class = "sem-properties",
          uiOutput("properties"),
          div(
            style = "display:flex; gap:6px; flex-wrap:wrap; margin-top:12px;",
            actionButton("undo", "Undo"),
            actionButton("redo", "Redo"),
            actionButton("reset", "Reset changes"),
            checkboxInput(
              "snap_to_grid",
              "Snap nodes to grid",
              value = TRUE
            )
          )
        )
      )
    ),

    hr(),

    h4("ggplot preview"),

    plotOutput(
      "preview",
      height = "450px"
    ),

    hr(),

    fluidRow(
      column(
        6,
        span("Changes are saved automatically")
      ),
      column(
        6,
        div(
          style = "text-align:right",
          actionButton(
            "done",
            "Done",
            class = "btn-primary"
          ),
          actionButton("cancel", "Cancel")
        )
      )
    )
  )

  # -------------------------------------------------------
  # Server
  # -------------------------------------------------------

  server <- function(input, output, session) {

    current <- reactiveVal(graph)
    selection <- reactiveVal(NULL)
    undo_stack <- reactiveVal(list())
    redo_stack <- reactiveVal(list())
    panel_revision <- reactiveVal(0L)

    # The property panel is rebuilt only on selection changes,
    # undo/redo, reset, and completed node drags. It is NOT rebuilt
    # after every text edit (which would steal keyboard focus).
    refresh_panel <- function() {
      panel_revision(isolate(panel_revision()) + 1L)
    }

    commit_change <- function(g) {
      old <- isolate(current())
      if (identical(old, g)) return(invisible(FALSE))
      undo_stack(append(isolate(undo_stack()), list(old)))
      redo_stack(list())
      current(g)
      invisible(TRUE)
    }

    update_node_geometry <- function(g, i) {
      oval <- g$nodes$shape[i] %in% c("oval", "ellipse")
      w <- if (oval) g$ellipses_width else g$rect_width
      h <- if (oval) g$ellipses_height else g$rect_height
      g$nodes$node_xmin[i] <- g$nodes$x[i] - w / 2
      g$nodes$node_xmax[i] <- g$nodes$x[i] + w / 2
      g$nodes$node_ymin[i] <- g$nodes$y[i] - h / 2
      g$nodes$node_ymax[i] <- g$nodes$y[i] + h / 2
      g
    }

    parse_property <- function(value, existing = NULL) {
      if (!is.null(existing)) {
        if (is.numeric(existing)) {
          result <- suppressWarnings(as.numeric(value))
          if (is.na(result) && !identical(value, "NA"))
            stop("This property requires a numeric value.")
          return(result)
        }
        if (is.logical(existing)) {
          if (!value %in% c("TRUE", "FALSE", "NA"))
            stop("This property requires TRUE, FALSE, or NA.")
          return(as.logical(value))
        }
        return(as.character(value))
      }
      if (value %in% c("TRUE", "FALSE", "NA")) return(as.logical(value))
      num <- suppressWarnings(as.numeric(value))
      if (!is.na(num) && nzchar(value)) return(num)
      value
    }

    set_aesthetic <- function(df, i, name, value, kind) {
      if (!nzchar(name)) return(df)
      protected <- if (kind == "node") {
        c("name", "x", "y", "node_xmin", "node_xmax",
          "node_ymin", "node_ymax", "shape", "label")
      } else {
        c("from", "to", "connect_from", "connect_to",
          "curvature", "arrow", "label")
      }
      if (name %in% protected)
        stop(sprintf("Use the dedicated control for '%s'.", name))
      allowed <- c(
        aesthetic_choices[[kind]],
        label_aesthetics
      )

      if (!name %in% allowed) {
        stop(sprintf(
          "Unsupported %s aesthetic: '%s'.",
          kind, name
        ))
      }
      existing <- if (name %in% names(df)) df[[name]] else NULL
      parsed <- parse_property(value, existing)
      if (is.null(existing)) {

        defaults <- .default_sem_aesthetics()[[kind]]

        if (!name %in% names(defaults)) {
          stop("No default defined for aesthetic: ", name)
        }

        df[[name]] <- rep(
          defaults[[name]],
          nrow(df)
        )
      }
      df[[name]][i] <- parsed
      df
    }

    # Edits are sent by the browser with their original element ID.
    # This avoids stale input values when renderUI changes selection.
    observeEvent(input$property_name, {

      s <- selection()
      req(s, input$property_name)

      g <- current()

      df <- if (s$type == "node") {
        g$nodes
      } else {
        g$edges
      }

      name <- input$property_name

      value <- if (name %in% names(df)) {
        as.character(df[[name]][s$index])
      } else {
        ""
      }

      updateTextInput(
        session,
        "property_value",
        value = value
      )
    })

    observeEvent(input$property_edit, {
      edit <- input$property_edit
      kind <- as.character(edit$kind)
      i <- as.integer(edit$index)
      field <- as.character(edit$field)
      value <- as.character(edit$value)
      g <- isolate(current())
      # The browser attaches the owning element's identity to each edit.
      # Do not reject edits just because selection changed before this
      # event reached the server (e.g., clicking a different node).
      if (!kind %in% c("node", "edge") || length(i) != 1L ||
          is.na(i) || length(field) != 1L) return()
      if (length(value) != 1L || is.na(value)) return()
      if (kind == "node" && (i < 1L || i > nrow(g$nodes))) return()
      if (kind == "edge" && (i < 1L || i > nrow(g$edges))) return()

      tryCatch({
        if (kind == "node") {
          if (field == "label") {
            g$nodes$label[i] <- value
          } else if (field == "shape") {
            if (!value %in% c("rect", "oval")) return()
            if (!identical(g$nodes$shape[i], value)) {
              g$nodes$shape[i] <- value
              g <- update_node_geometry(g, i)
            }
          } else if (field %in% c("x", "y")) {
            v <- suppressWarnings(as.numeric(value))

            if (!is.finite(v)) {
              stop("Coordinates must be finite numbers.")
            }

            if (isTRUE(input$snap_to_grid)) {
              v <- round(v)
            }

            x <- if (field == "x") v else g$nodes$x[i]
            y <- if (field == "y") v else g$nodes$y[i]

            g$nodes <- move_node(g$nodes, i, x, y)
          } else if (field == "aesthetic") {
            name <- as.character(edit$name)
            if (length(name) != 1L || is.na(name)) return()
            g$nodes <- set_aesthetic(g$nodes, i, name, value, "node")
          } else return()
        } else if (kind == "edge") {
          if (field == "label") {
            g$edges$label[i] <- value
          } else if (field == "arrow") {
            if (!value %in% c("none", "first", "last", "both")) return()
            g$edges$arrow[i] <- value
          } else if (field == "curvature") {
            if (!value %in% c("straight", "curved")) return()
            g$edges$curvature[i] <- if (value == "straight") NA_real_ else 60
          } else if (field %in% c("connect_from", "connect_to", "variance_side")) {
            if (!value %in% sides) return()
            if (field == "variance_side" ||
                (g$edges$from[i] == g$edges$to[i] &&
                 g$edges$arrow[i] == "both")) {
              g$edges$connect_from[i] <- value
              g$edges$connect_to[i] <- value
            } else {
              g$edges[[field]][i] <- value
            }
          } else if (field == "aesthetic") {
            name <- as.character(edit$name)
            if (length(name) != 1L || is.na(name)) return()
            g$edges <- set_aesthetic(g$edges, i, name, value, "edge")
          } else return()

          # A newly created self-variance always uses one side.
          if (g$edges$from[i] == g$edges$to[i] &&
              g$edges$arrow[i] == "both") {
            g$edges$connect_to[i] <- g$edges$connect_from[i]
          }
        } else return()
        changed <- commit_change(g)
        if (isTRUE(changed) && kind == "edge" &&
            field %in% c("from", "to", "arrow")) refresh_panel()
      }, error = function(e) {
        showNotification(conditionMessage(e), type = "error")
      })
    }, priority = 100)

    output$editor <- renderUI({
      make_svg(current())
    })

    observeEvent(input$selected_node, {
      new <- list(type = "node", index = as.integer(input$selected_node))
      if (!identical(isolate(selection()), new)) selection(new)
    })
    observeEvent(input$selected_edge, {
      new <- list(type = "edge", index = as.integer(input$selected_edge))
      if (!identical(isolate(selection()), new)) selection(new)
    })

    observeEvent(input$node_drag, {
      g <- isolate(current())
      i <- as.integer(input$node_drag$index)
      if (length(i) != 1L || is.na(i) || i < 1L || i > nrow(g$nodes)) return()
      dx <- as.numeric(input$node_drag$dx)
      dy <- as.numeric(input$node_drag$dy)
      if (length(dx) != 1L || length(dy) != 1L ||
          !is.finite(dx) || !is.finite(dy)) return()

      new_x <- g$nodes$x[i] + dx
      new_y <- g$nodes$y[i] + dy

      if (isTRUE(input$snap_to_grid)) {
        new_x <- round(new_x)
        new_y <- round(new_y)
      }

      g$nodes <- move_node(
        g$nodes,
        i,
        new_x,
        new_y
      )
      if (commit_change(g)) refresh_panel()
    })

    # Inputs carry data attributes so the browser sends both the
    # property and the element identity in a single atomic event.
    tagged <- function(control, field, input_id = NULL) {
      # A stable wrapper, independent of Shiny's control internals.
      # Shiny input tags contain the actual input id in a descendant.
      if (is.null(input_id)) input_id <- switch(field,
        label = "node_label",
        shape = "node_shape", x = "node_x", y = "node_y",
        variance_side = "variance_side", connect_from = "edge_from",
        connect_to = "edge_to", arrow = "edge_arrow",
        curvature = "edge_curvature", aesthetic = "property_value"
      )
      tags$div(`data-sem-field` = field,
               `data-sem-input` = input_id, control)
    }

    output$properties <- renderUI({
      panel_revision()
      s <- selection()
      g <- isolate(current())
      if (is.null(s)) return(p("Select a node or edge to edit its properties."))
      i <- s$index
      if (s$type == "node") {
        if (i < 1L || i > nrow(g$nodes)) return(NULL)
        n <- g$nodes[i, , drop = FALSE]
        controls <- tagList(
          h4(paste("Node:", n$name)),
          tagged(textInput("node_label", "Label", value = n$label), "label", "node_label"),
          tagged(selectInput("node_shape", "Shape", c("rect", "oval"),
                             selected = n$shape, selectize = FALSE), "shape"),
          tagged(numericInput("node_x", "X", value = n$x), "x"),
          tagged(numericInput("node_y", "Y", value = n$y), "y")
        )
      } else {
        if (i < 1L || i > nrow(g$edges)) return(NULL)
        e <- g$edges[i, , drop = FALSE]
        is_variance <- e$from == e$to && e$arrow == "both"
        controls <- tagList(
          h4(paste("Edge:", e$from, "→", e$to)),
          tagged(textInput("edge_label", "Label", value = e$label), "label", "edge_label"),
          if (is_variance) {
            tagged(selectInput("variance_side", "Variance position", sides,
                               selected = e$connect_from, selectize = FALSE), "variance_side")
          } else tagList(
            tagged(selectInput("edge_from", "Connect from", sides,
                               selected = e$connect_from, selectize = FALSE), "connect_from"),
            tagged(selectInput("edge_to", "Connect to", sides,
                               selected = e$connect_to, selectize = FALSE), "connect_to")
          ),
          tagged(selectInput("edge_arrow", "Arrow",
                             c("none", "first", "last", "both"),
                             selected = e$arrow, selectize = FALSE), "arrow"),
          tagged(selectInput("edge_curvature", "Edge geometry",
                             c("Straight" = "straight", "Curved" = "curved"),
                             selected = if (is.na(e$curvature)) "straight" else "curved",
                              selectize = FALSE),
                 "curvature")
        )
      }
      div(
        `data-sem-kind` = s$type,
        `data-sem-index` = i,
        controls,
        selectInput(
          "property_name",
          "Aesthetic / property",
          choices = c(
            "Select an aesthetic..." = "",
            list(
              "Element aesthetics" = aesthetic_choices[[s$type]],
              "Label aesthetics" = label_aesthetics
            )
          ),
          selected = "",
          selectize = FALSE
        ),
        tagged(textInput("property_value", "Value",
                         placeholder = "e.g. steelblue"), "aesthetic")
      )
    })

    output$preview <- renderPlot({
      plot(current())
    })

    observeEvent(input$undo, {
      history <- isolate(undo_stack())
      if (!length(history)) return()
      redo_stack(append(isolate(redo_stack()), list(isolate(current()))))
      undo_stack(history[-length(history)])
      current(history[[length(history)]])
      refresh_panel()
    })
    observeEvent(input$redo, {
      history <- isolate(redo_stack())
      if (!length(history)) return()
      undo_stack(append(isolate(undo_stack()), list(isolate(current()))))
      redo_stack(history[-length(history)])
      current(history[[length(history)]])
      refresh_panel()
    })
    observeEvent(input$reset, {
      if (commit_change(graph)) refresh_panel()
    })
    observeEvent(input$done, {
      stopApp(refresh_sem_graph(isolate(current())))
    })
    observeEvent(input$cancel, {
      stopApp(NULL)
    })
  }

  shinyApp(ui, server)
}
