#' Interactively Edit a SEM Graph
#'
#' Launch a Shiny editor for an existing `sem_graph` object. Drag nodes,
#' adjust node dimensions and labels, change edge connections and curvature,
#' edit supported plotting aesthetics, and change graph-level plotting settings.
#' For multigroup or multilevel
#' graphs, switch between panels. Optionally synchronize node positions
#' across groups within the same level. Changes are shown in a ggplot
#' preview and can be undone, redone, or reset.
#'
#' @param graph An object inheriting from class `sem_graph`, typically
#'   created by [prepare_graph()].
#'
#' @details
#' This function requires the optional packages `shiny` and `htmltools`.
#' They are checked at runtime and do not need to be installed for other
#' tidySEM functionality. The editor uses [update_graph()] to maintain
#' node geometry without rerunning layout or edge-routing algorithms.
#'
#' Panel switching does not change the underlying graph. When enabled,
#' **Apply layout to all groups** copies node positions (not aesthetics,
#' dimensions, or edge properties) to same-named nodes in other groups
#' at the same level. This setting is available only for multigroup models.
#'
#' Edits are saved automatically within the app. Clicking **Done** returns
#' the edited graph; clicking **Cancel** returns `NULL`.
#'
#' @return An edited `sem_graph` object when **Done** is clicked, or `NULL`
#'   when **Cancel** is clicked. The supplied object is not modified in place.
#'
#' @seealso [prepare_graph()], [update_graph()]
#'
#' @examples
#' \dontrun{
#'   graph <- prepare_graph(
#'   edges = data.frame(from = "x", to = "y"),
#'   layout = matrix(c("x", "y")))
#'   # Uncomment this to run the code:
#'   # edited <- graph_editor(graph)
#'   # if (!is.null(edited)) plot(edited)
#' }
#'
#' @export
graph_editor <- function(graph) {

  if (!inherits(graph, "sem_graph")) {
    stop("`graph` must inherit from 'sem_graph'.")
  }

  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package `shiny` is required.")
  }

  if (!requireNamespace("htmltools", quietly = TRUE)) {
    stop("Package `htmltools` is required for `graph_editor()`.", call. = FALSE)
  }

  shiny::runApp(
    sem_graph_editor_app(graph),
    launch.browser = TRUE
  )
}

sem_graph_editor_app <- function(graph) {


  # Panel identifiers are metadata, not globally unique node names.
  # NA denotes a shared row only when a panel column is absent from a table.
  panel_cols <- intersect(
    c("group", "level"),
    union(names(graph$nodes), names(graph$edges))
  )
  panel_values <- lapply(panel_cols, function(nm) {
    vals <- c(
      if (nm %in% names(graph$nodes)) as.character(graph$nodes[[nm]]),
      if (nm %in% names(graph$edges)) as.character(graph$edges[[nm]])
    )
    vals <- unique(vals[!is.na(vals)])
    if (!length(vals)) "" else vals
  })
  names(panel_values) <- panel_cols
  has_multigroup <- "group" %in% panel_cols &&
    length(panel_values[["group"]]) > 1L

  # Restrict to the active facet. Missing panel columns are shared;
  # explicit NA values are not silently interpreted as a specific group.
  panel_rows <- function(df, panel) {
    keep <- rep(TRUE, nrow(df))
    for (nm in names(panel)) {
      if (nm %in% names(df)) {
        keep <- keep & !is.na(df[[nm]]) &
          as.character(df[[nm]]) == panel[[nm]]
      }
    }
    which(keep)
  }

  same_level <- function(df, i) {
    if (!"level" %in% names(df)) return(rep(TRUE, nrow(df)))
    !is.na(df$level) & !is.na(df$level[i]) &
      as.character(df$level) == as.character(df$level[i])
  }

  # Copy only positions, never labels, styles, dimensions, or edge routing.
  synchronize_group_positions <- function(g, i) {
    if (!has_multigroup || !"group" %in% names(g$nodes)) return(g)
    nd <- g$nodes
    peers <- which(
      !is.na(nd$group) &
        nd$name == nd$name[i] &
        same_level(nd, i)
    )
    if (length(peers)) {
      nd$x[peers] <- nd$x[i]
      nd$y[peers] <- nd$y[i]
    }
    g$nodes <- nd
    g
  }

  sides <- c("top", "bottom", "left", "right")
  label_aesthetics <- paste0(
    "label_",
    c(
      "fill", "size", "family", "fontface", "angle",
      "hjust", "vjust", "lineheight",
      "colour", "alpha"
    )
  )

  aesthetic_choices <- list(
    node = c(
      "linetype", "linewidth", "colour",
      "fill", "alpha"
    ),
    edge = c(
      "linetype", "linewidth", "colour",
      "alpha"
    )
  )

  # -------------------------------------------------------
  # Helpers
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

  # The SVG geometry and labels are rendered by sem_graph_svg().
  # Keep this editor responsible only for interaction and graph updates.

  # -------------------------------------------------------
  # UI
  # -------------------------------------------------------

  ui <- shiny::fluidPage(

    htmltools::tags$head(
      htmltools::tags$style(htmltools::HTML("
        .sem-editor {
          height: 620px;
          min-height: 400px;
          border: 1px solid #d1d5db;
          border-radius: 8px;
          overflow: hidden;
          background: white;
          display: flex;
          align-items: center;
          justify-content: center;
        }

        .sem-editor > .shiny-html-output {
          width: 100%;
          height: 100%;
          min-height: 0;
        }

        #sem-editor-svg {
          display: block;
          width: 100%;
          height: 100%;
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
          vector-effect: non-scaling-stroke;
          transition: stroke 0.15s, stroke-width 0.15s;
        }

        .sem-node:hover > rect,
        .sem-node:hover > ellipse {
          stroke: #2563eb !important;
          stroke-width: 2.5px !important;
        }

        .sem-node > text {
          pointer-events: none;
        }



        /* Visible edge */
        .sem-edge-visible {
          fill: none !important;
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

        /* Persistent selection, independent of hover. */
        .sem-node.sem-selected > rect,
        .sem-node.sem-selected > ellipse {
          stroke: #2563eb !important;
          stroke-width: 3px !important;
        }
        .sem-edge-group.sem-selected .sem-edge-visible {
          stroke: #2563eb !important;
          stroke-width: 3px !important;
        }

        .sem-properties {
          padding: 12px;
          border: 1px solid #e5e7eb;
          border-radius: 8px;
        }
      ")),

      htmltools::tags$script(htmltools::HTML("
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

          // Graph-level controls have no selected node/edge identity.
          function sendGraphEdit(target) {
            if (!target || !target.closest) return;
            const wrapper = target.closest('[data-sem-graph-field]');
            if (!wrapper) return;
            const control = wrapper.querySelector('input, select');
            if (!control) return;
            const value = control.type === 'checkbox'
              ? String(control.checked) : String(control.value);
            if (window.Shiny && typeof Shiny.setInputValue === 'function') {
              Shiny.setInputValue('graph_setting_edit', {
                field: wrapper.dataset.semGraphField,
                value: value,
                nonce: Math.random()
              }, {priority: 'event'});
            }
          }

          document.addEventListener('change', function(e) {
            if (e.target.closest && e.target.closest('[data-sem-graph-field]')) {
              sendGraphEdit(e.target);
            } else if (e.target.closest && e.target.closest('[data-sem-field]')) {
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

          function highlightSemElement(element) {
            const svg = element && element.ownerSVGElement;
            if (!svg) return;
            svg.querySelectorAll('.sem-selected').forEach(function(el) {
              el.classList.remove('sem-selected');
            });
            const group = element.closest('.sem-node, .sem-edge-group');
            if (group) group.classList.add('sem-selected');
          }

          document.addEventListener('pointerdown', function(e) {
            if (!e.target.closest) return;
            const svg = e.target.closest('#sem-editor-svg');
            if (!svg) return;
            const node = e.target.closest('.sem-node');
            const edge = e.target.closest('.sem-edge-hit');
            if (node || edge) e.stopPropagation();

            if (node) {
              highlightSemElement(node);
              flushFocusedSemField();

              const svg = node.ownerSVGElement;
              const p = graphPoint(svg, e);

              if (!p) return;

              drag = {
                node: Number(node.dataset.node),
                svg: svg,
                element: node,
                start: p,
                scale: Number(svg.dataset.semScale) || 1,
                moved: false
              };

              Shiny.setInputValue(
                'selected_node',
                drag.node,
                {priority: 'event'}
              );

              e.preventDefault();

            } else if (edge) {
              highlightSemElement(edge);
              flushFocusedSemField();

              Shiny.setInputValue(
                'selected_edge',
                Number(edge.dataset.edge),
                {priority: 'event'}
              );
            }
          }, true);

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
                  dx: (p.x - drag.start.x) / drag.scale,
                  dy: -(p.y - drag.start.y) / drag.scale,
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

    shiny::titlePanel("SEM graph editor"),

    shiny::fluidRow(

      shiny::column(
        7,
        htmltools::tags$h4("Interactive editor"),
        shiny::uiOutput("panel_controls"),
        htmltools::tags$div(
          class = "sem-editor",
          shiny::uiOutput("editor")
        ),
        htmltools::tags$p(
          "Drag nodes to reposition them. ",
          "Click nodes or edges to edit their properties.",
          "Changes are saved automatically.",
          "Click Done (below) to finalize the plot."
        )
      ),

      shiny::column(
        5,

        htmltools::tags$h4("Properties"),

        htmltools::tags$div(
          class = "sem-properties",

          shiny::tabsetPanel(
            id = "property_tabs",
            type = "tabs",

            # -----------------------------------------------
            # Node / edge properties
            # -----------------------------------------------
            shiny::tabPanel(
              title = "Element",
              value = "element",

              htmltools::tags$div(
                style = "padding-top: 12px;",
                shiny::uiOutput("properties")
              )
            ),

            # -----------------------------------------------
            # Graph-level properties
            # -----------------------------------------------
            shiny::tabPanel(
              title = "Graph",
              value = "graph",

              htmltools::tags$div(
                style = "padding-top: 12px;",
                shiny::uiOutput("graph_settings")
              )
            )
          ),

          # -----------------------------------------------
          # Global editor controls
          # -----------------------------------------------
          htmltools::tags$div(
            style = paste(
              "display:flex;",
              "gap:6px;",
              "flex-wrap:wrap;",
              "margin-top:12px;"
            ),

            shiny::actionButton("undo", "Undo"),
            shiny::actionButton("redo", "Redo"),
            shiny::actionButton("reset", "Reset changes")
          ),

          htmltools::tags$div(
            style = "margin-top:12px;",

            shiny::checkboxInput(
              "snap_to_grid",
              "Snap nodes to grid",
              value = TRUE
            ),

            if (has_multigroup) {
              shiny::checkboxInput(
                "apply_all_groups",
                "Apply layout to all groups",
                value = TRUE
              )
            }
          )
        )
      )
    ),

    htmltools::tags$hr(),

    htmltools::tags$h4("ggplot preview"),

    shiny::plotOutput(
      "preview",
      height = "450px"
    ),

    htmltools::tags$hr(),

    shiny::fluidRow(
      shiny::column(
        6,
        htmltools::tags$span("")
      ),
      shiny::column(
        6,
        htmltools::tags$div(
          style = "text-align:right",
          shiny::actionButton(
            "done",
            "Done",
            class = "btn-primary"
          ),
          shiny::actionButton("cancel", "Cancel")
        )
      )
    )
  )

  # -------------------------------------------------------
  # Server
  # -------------------------------------------------------

  server <- function(input, output, session) {

    current <- shiny::reactiveVal(update_graph(graph))
    selection <- shiny::reactiveVal(NULL)
    undo_stack <- shiny::reactiveVal(list())
    redo_stack <- shiny::reactiveVal(list())
    panel_revision <- shiny::reactiveVal(0L)

    active_panel <- shiny::reactive({
      result <- list()
      for (nm in panel_cols) {
        val <- input[[paste0("sem_panel_", nm)]]
        if (is.null(val) || !nzchar(val)) val <- panel_values[[nm]][1L]
        result[[nm]] <- val
      }
      result
    })

    output$panel_controls <- shiny::renderUI({
      if (!length(panel_cols)) return(NULL)
      htmltools::tagList(lapply(panel_cols, function(nm) {
        shiny::selectInput(
          paste0("sem_panel_", nm),
          paste0(toupper(substring(nm, 1, 1)), substring(nm, 2)),
          choices = panel_values[[nm]],
          selected = panel_values[[nm]][1L],
          selectize = FALSE
        )
      }))
    })

    shiny::observeEvent(active_panel(), {
      selection(NULL)
      refresh_panel()
    }, ignoreInit = TRUE)

    panel_indices <- shiny::reactive({
      g <- current()
      panel <- active_panel()
      ni <- panel_rows(g$nodes, panel)
      ei <- panel_rows(g$edges, panel)
      # Edges must have both endpoints in this panel's nodes.
      if (length(ei)) {
        e <- g$edges[ei, , drop = FALSE]
        ei <- ei[e$from %in% g$nodes$name[ni] &
                   e$to %in% g$nodes$name[ni]]
      }
      list(nodes = ni, edges = ei)
    })

    # The property panel is rebuilt only on selection changes,
    # undo/redo, reset, and completed node drags. It is NOT rebuilt
    # after every text edit (which would steal keyboard focus).
    refresh_panel <- function() {
      panel_revision(shiny::isolate(panel_revision()) + 1L)
    }

    commit_change <- function(g, moved_node = NULL) {
      old <- shiny::isolate(current())
      if (!is.null(moved_node) &&
          isTRUE(shiny::isolate(input$apply_all_groups))) {
        g <- synchronize_group_positions(g, moved_node)
      }
      g <- update_graph(g)
      if (identical(old, g)) return(invisible(FALSE))
      undo_stack(append(shiny::isolate(undo_stack()), list(old)))
      redo_stack(list())
      current(g)
      invisible(TRUE)
    }

    # Checking the option also applies the currently displayed layout once.
    # Subsequent node moves synchronize automatically in commit_change().
    shiny::observeEvent(input$apply_all_groups, {
      if (!has_multigroup || !isTRUE(input$apply_all_groups)) return()
      ids <- shiny::isolate(panel_indices())$nodes
      g <- shiny::isolate(current())
      for (i in ids) g <- synchronize_group_positions(g, i)
      if (commit_change(g)) refresh_panel()
    }, ignoreInit = TRUE)

    # Graph settings are stored on the sem_graph object, not nodes/edges.
    # Edge curvature is edited per edge; angle is no longer a graph setting.
    graph_defaults <- c(
      rect_width = 1.2,
      rect_height = 0.8,
      ellipses_width = 1,
      ellipses_height = 1,
      variance_diameter = 0.8,
      text_size = 4,
      spacing_x = 2,
      spacing_y = 2
    )
    graph_numeric <- names(graph_defaults)

    graph_value <- function(g, nm) {
      value <- g[[nm]]
      if (is.null(value) || length(value) != 1L ||
          !is.numeric(value) || !is.finite(value) || value <= 0) {
        return(unname(graph_defaults[[nm]]))
      }
      as.numeric(value)
    }

    # Rebuild only on undo/redo/reset (not on every edit, to retain focus).
    output$graph_settings <- shiny::renderUI({
      panel_revision()
      g <- shiny::isolate(current())

      controls <- lapply(graph_numeric, function(nm) {
        control <- shiny::numericInput(
          inputId = paste0("sem_graph_", nm),
          label = gsub("_", " ", nm),
          value = graph_value(g, nm),
          min = 0.001,
          step = 0.1
        )
        htmltools::tags$div(`data-sem-graph-field` = nm, control)
      })

      htmltools::tagList(
        htmltools::tags$p(
          style = "font-size:12px; color:#666; margin-top:8px;",
          "Global dimensions override individual node dimensions. ",
          "Spacing rescales existing coordinates within each panel."
        ),
        controls,
        htmltools::tags$div(
          `data-sem-graph-field` = "fix_coord",
          shiny::checkboxInput(
            "sem_graph_fix_coord", "Fix coordinate aspect ratio",
            value = isTRUE(g$fix_coord)
          )
        )
      )
    })

    # Global settings are explicit overrides of existing node geometry.
    apply_graph_setting <- function(g, nm, value) {
      old <- graph_value(g, nm)
      nd <- g$nodes
      if (nm %in% c("rect_width", "rect_height",
                    "ellipses_width", "ellipses_height")) {
        if (!"width" %in% names(nd))
          nd$width <- nd$node_xmax - nd$node_xmin
        if (!"height" %in% names(nd))
          nd$height <- nd$node_ymax - nd$node_ymin
        rect <- !is.na(nd$shape) & nd$shape %in% c("rect", "none")
        oval <- !is.na(nd$shape) & nd$shape %in% c("oval", "ellipse")
        if (nm == "rect_width") nd$width[rect] <- value
        if (nm == "rect_height") nd$height[rect] <- value
        if (nm == "ellipses_width") nd$width[oval] <- value
        if (nm == "ellipses_height") nd$height[oval] <- value
      }
      if (nm %in% c("spacing_x", "spacing_y") && nrow(nd)) {
        coord <- if (nm == "spacing_x") "x" else "y"
        # Scale around each panel's minimum, not the entire faceted graph.
        panels <- intersect(c("group", "level"), names(nd))
        panel_id <- if (length(panels)) {
          do.call(paste, c(lapply(nd[panels], as.character), sep = "\r"))
        } else rep("single", nrow(nd))
        for (ids in split(seq_len(nrow(nd)), panel_id)) {
          origin <- min(nd[[coord]][ids], na.rm = TRUE)
          nd[[coord]][ids] <- origin + (nd[[coord]][ids] - origin) * value / old
        }
      }
      g$nodes <- nd
      if (nm == "text_size") {
        # The global setting overrides per-element label sizes in both renderers.
        for (part in c("nodes", "edges")) {
          if ("label_size" %in% names(g[[part]])) {
            g[[part]]$label_size[] <- value
          }
        }
      }
      g[[nm]] <- value
      g
    }

    shiny::observeEvent(input$graph_setting_edit, {
      edit <- input$graph_setting_edit
      nm <- as.character(edit$field)
      value <- as.character(edit$value)
      if (length(nm) != 1L || is.na(nm) ||
          length(value) != 1L || is.na(value) ||
          !nm %in% c(graph_numeric, "fix_coord")) return()

      tryCatch({
        if (nm == "fix_coord") {
          if (!tolower(value) %in% c("true", "false")) {
            stop("Invalid fix_coord value.")
          }
          parsed <- tolower(value) == "true"
        } else {
          parsed <- suppressWarnings(as.numeric(value))
          if (length(parsed) != 1L || !is.finite(parsed) || parsed <= 0) {
            stop(nm, " must be a finite number greater than zero.")
          }
        }

        g <- shiny::isolate(current())
        g <- apply_graph_setting(g, nm, parsed)
        if (commit_change(g)) refresh_panel()
      }, error = function(e) {
        shiny::showNotification(conditionMessage(e), type = "error")
      })
    }, priority = 100)

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
      if (name %in% c("color", "label_color")) {
        name <- sub("color$", "colour", name)
      }
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
      if (name %in% c("colour", "label_colour")) {
        alias <- sub("colour$", "color", name)
        if (!name %in% names(df) && alias %in% names(df)) {
          names(df)[names(df) == alias] <- name
        }
        if (alias %in% names(df) && name %in% names(df)) {
          df[[alias]] <- NULL
        }
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
    shiny::observeEvent(input$property_name, {

      s <- selection()
      shiny::req(s, input$property_name)

      g <- current()

      df <- if (s$type == "node") {
        g$nodes
      } else {
        g$edges
      }

      name <- input$property_name

      value <- if (name %in% names(df)) {
        as.character(df[[name]][s$index])
      } else if (name %in% c("colour", "label_colour") &&
                 sub("colour$", "color", name) %in% names(df)) {
        as.character(df[[sub("colour$", "color", name)]][s$index])
      } else {
        ""
      }

      shiny::updateTextInput(
        session,
        "property_value",
        value = value
      )
    })

    shiny::observeEvent(input$property_edit, {
      edit <- input$property_edit
      kind <- as.character(edit$kind)
      i <- as.integer(edit$index)
      field <- as.character(edit$field)
      value <- as.character(edit$value)
      g <- shiny::isolate(current())
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
              # update_graph() recalculates bounds when committed.
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
          } else if (field %in% c("width", "height")) {
            v <- suppressWarnings(as.numeric(value))
            if (length(v) != 1L || !is.finite(v) || v <= 0) {
              stop("Node dimensions must be finite positive numbers.")
            }
            g$nodes[[field]][i] <- v
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

            value <- trimws(value)

            if (!nzchar(value) ||
                toupper(value) == "NA") {

              g$edges$curvature[i] <- NA_real_

            } else {

              angle <- suppressWarnings(as.numeric(value))

              if (!is.finite(angle) ||
                  abs(angle) >= 180 ||
                  angle == 0) {
                stop(
                  "Curvature must be between -180 and 180 degrees."
                )
              }

              g$edges$curvature[i] <- angle
            }
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
        changed <- commit_change(
          g,
          moved_node = if (kind == "node" && field %in% c("x", "y")) i else NULL
        )
        if (isTRUE(changed) && kind == "edge" &&
            field %in% c("from", "to", "arrow")) refresh_panel()
      }, error = function(e) {
        shiny::showNotification(conditionMessage(e), type = "error")
      })
    }, priority = 100)

    output$editor <- shiny::renderUI({
      panel <- active_panel()

      if (!length(panel)) {
        panel <- NULL
      }

      .sem_graph_svg_panel(
        current(),
        panel = panel,
        selected = shiny::isolate(selection())
      )
    })

    shiny::observeEvent(input$selected_node, {

      new <- list(
        type = "node",
        index = as.integer(input$selected_node)
      )

      if (!identical(shiny::isolate(selection()), new)) {
        selection(new)
      }

      shiny::updateTabsetPanel(
        session,
        inputId = "property_tabs",
        selected = "element"
      )
    })

    shiny::observeEvent(input$selected_edge, {

      new <- list(
        type = "edge",
        index = as.integer(input$selected_edge)
      )

      if (!identical(shiny::isolate(selection()), new)) {
        selection(new)
      }

      shiny::updateTabsetPanel(
        session,
        inputId = "property_tabs",
        selected = "element"
      )
    })

    shiny::observeEvent(input$node_drag, {
      g <- shiny::isolate(current())
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
      if (commit_change(g, moved_node = i)) refresh_panel()
    })

    # Inputs carry data attributes so the browser sends both the
    # property and the element identity in a single atomic event.
    tagged <- function(control, field, input_id = NULL) {
      # A stable wrapper, independent of Shiny's control internals.
      # Shiny input tags contain the actual input id in a descendant.
      if (is.null(input_id)) input_id <- switch(field,
                                                label = "node_label",
                                                shape = "node_shape", x = "node_x", y = "node_y",
                                                width = "node_width", height = "node_height",
                                                variance_side = "variance_side", connect_from = "edge_from",
                                                connect_to = "edge_to", arrow = "edge_arrow",
                                                curvature = "edge_curvature", aesthetic = "property_value"
      )
      htmltools::tags$div(`data-sem-field` = field,
                          `data-sem-input` = input_id, control)
    }

    output$properties <- shiny::renderUI({
      panel_revision()
      s <- selection()
      g <- shiny::isolate(current())
      if (is.null(s)) return(htmltools::tags$p("Select a node or edge to edit its properties."))
      i <- s$index
      if (s$type == "node") {
        if (i < 1L || i > nrow(g$nodes)) return(NULL)
        n <- g$nodes[i, , drop = FALSE]
        controls <- htmltools::tagList(
          htmltools::tags$h4(paste("Node:", n$name)),
          tagged(shiny::textInput("node_label", "Label", value = n$label), "label", "node_label"),
          tagged(shiny::selectInput("node_shape", "Shape", c("rect", "oval"),
                                    selected = n$shape, selectize = FALSE), "shape"),
          tagged(shiny::numericInput("node_x", "X", value = n$x), "x"),
          tagged(shiny::numericInput("node_y", "Y", value = n$y), "y"),
          tagged(shiny::numericInput("node_width", "Width", value = n$width, min = 0.01), "width"),
          tagged(shiny::numericInput("node_height", "Height", value = n$height, min = 0.01), "height")
        )
      } else {
        if (i < 1L || i > nrow(g$edges)) return(NULL)
        e <- g$edges[i, , drop = FALSE]
        is_variance <- e$from == e$to && e$arrow == "both"
        controls <- htmltools::tagList(
          htmltools::tags$h4(paste("Edge:", e$from, "->", e$to)),
          tagged(shiny::textInput("edge_label", "Label", value = e$label), "label", "edge_label"),
          if (is_variance) {
            tagged(shiny::selectInput("variance_side", "Variance position", sides,
                                      selected = e$connect_from, selectize = FALSE), "variance_side")
          } else htmltools::tagList(
            tagged(shiny::selectInput("edge_from", "Connect from", sides,
                                      selected = e$connect_from, selectize = FALSE), "connect_from"),
            tagged(shiny::selectInput("edge_to", "Connect to", sides,
                                      selected = e$connect_to, selectize = FALSE), "connect_to")
          ),
          tagged(shiny::selectInput("edge_arrow", "Arrow",
                                    c("none", "first", "last", "both"),
                                    selected = e$arrow, selectize = FALSE), "arrow"),
          if (!is_variance) {
            tagged(
              shiny::textInput(
                "edge_curvature",
                "Curvature (degrees)",
                value = if (is.na(e$curvature)) {
                  ""
                } else {
                  as.character(e$curvature)
                },
                placeholder = "0 = straight; blank = automatic"
              ),
              "curvature"
            )
          }
        )
      }
      htmltools::tags$div(
        `data-sem-kind` = s$type,
        `data-sem-index` = i,
        controls,
        shiny::selectInput(
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
        tagged(shiny::textInput("property_value", "Value",
                                placeholder = "e.g. steelblue"), "aesthetic")
      )
    })

    output$preview <- shiny::renderPlot({
      plot(current())
    })

    shiny::observeEvent(input$undo, {
      history <- shiny::isolate(undo_stack())
      if (!length(history)) return()
      redo_stack(append(shiny::isolate(redo_stack()), list(shiny::isolate(current()))))
      undo_stack(history[-length(history)])
      current(history[[length(history)]])
      refresh_panel()
    })
    shiny::observeEvent(input$redo, {
      history <- shiny::isolate(redo_stack())
      if (!length(history)) return()
      undo_stack(append(shiny::isolate(undo_stack()), list(shiny::isolate(current()))))
      redo_stack(history[-length(history)])
      current(history[[length(history)]])
      refresh_panel()
    })
    shiny::observeEvent(input$reset, {
      if (commit_change(update_graph(graph))) refresh_panel()
    })
    shiny::observeEvent(input$done, {
      shiny::stopApp(update_graph(shiny::isolate(current())))
    })
    shiny::observeEvent(input$cancel, {
      shiny::stopApp(NULL)
    })
  }

  shiny::shinyApp(ui, server)
}
