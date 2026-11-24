.sem_aesthetic_registry <- function() {
  c(
    colour = '"black"',
    linewidth = '1',
    linetype = '1',
    alpha = '1',
    fill = '"white"',
    label_colour = '"black"',
    label_fill = '"white"',
    label_size = '4',
    label_alpha = '1',
    label_family = '"sans"',
    label_fontface = '"plain"',
    label_hjust = '"center"',
    label_vjust = '"middle"',
    label_lineheight = '1',
    label_location = '.5'
  )
}

.sem_aesthetic_aliases <- function() {
  c(color = 'colour', label_color = 'label_colour', size = 'linewidth')
}

.normalize_sem_aesthetic <- function(name) {
  aliases <- .sem_aesthetic_aliases()
  result <- as.character(name)
  hits <- !is.na(result) & result %in% names(aliases)
  result[hits] <- unname(aliases[result[hits]])
  result
}

.normalize_sem_aesthetics <- function(df) {
  for (alias in names(.sem_aesthetic_aliases())) {
    canonical <- unname(.sem_aesthetic_aliases()[[alias]])
    if (!alias %in% names(df)) next
    if (!canonical %in% names(df)) {
      names(df)[names(df) == alias] <- canonical
    } else {
      missing <- is.na(df[[canonical]])
      df[[canonical]][missing] <- df[[alias]][missing]
      df[[alias]] <- NULL
    }
  }
  df
}

nodedg <- function(template, txt = 'nodes') {
  lapply(template, function(x) {
    gsub('c("edges", "nodes")', paste0('"', txt, '"'),
         gsub(' <- function', paste0('_', txt, ' <- function'), x,
              fixed = TRUE), fixed = TRUE)
  })
}

create_all_select_rows <- function(filename = 'R/select_rows.R') {
  txt <- readLines(filename, warn = FALSE)
  marker <- grep('^# Automatically generated functions below here$', txt)
  if (length(marker) != 1L) {
    stop('Expected exactly one generated-functions marker in ', filename)
  }
  header <- txt[seq_len(marker)]

  fun_list <- list(
    sig = 'as.numeric(pval) < .05',
    nonsig = 'as.numeric(pval) >= .05',
    fixed = 'is.na(pval)',
    pos = 'as.numeric(est) > 0',
    neg = 'as.numeric(est) < 0',
    var = 'lhs == rhs & op == "~~"',
    cov = 'lhs != rhs & op == "~~"',
    reg = 'op == "~"',
    load = 'op == "=~"',
    obs = 'shape == "rect"',
    latent = 'shape == "oval"'
  )
  edges_cond <- c('var', 'cov', 'reg', 'load')
  nodes_cond <- c('obs', 'latent')
  aes_list <- .sem_aesthetic_registry()
  aliases <- .sem_aesthetic_aliases()

  # Templates start at #Y / #AY @export and end before the next one.
  source_lines <- txt[grepl('^#A?[XY]', txt)]
  starts <- grep('^#A?Y @export$', source_lines)
  if (!length(starts)) stop('No function templates found in ', filename)
  ends <- c(starts[-1L] - 1L, length(source_lines))
  templates <- Map(function(a, b) source_lines[a:b], starts, ends)
  is_aesthetic <- vapply(templates, function(z) {
    any(grepl('^#AY ', z))
  }, logical(1))
  aesthetic_templates <- lapply(templates[is_aesthetic], function(z) {
    sub('^#A', '#', z)
  })
  ordinary_templates <- templates[!is_aesthetic]

  # Expand canonical aesthetics. Replace the longer placeholder FIRST.
  generated_aes <- unlist(lapply(names(aes_list), function(nm) {
    lapply(aesthetic_templates, function(z) {
      z <- gsub('AES_DEFAULT', aes_list[[nm]], z, fixed = TRUE)
      gsub('AES', nm, z, fixed = TRUE)
    })
  }), recursive = FALSE)

  all_templates <- c(ordinary_templates, generated_aes)
  expanded <- unlist(lapply(names(fun_list), function(nm) {
    lapply(all_templates, function(z) {
      z <- gsub('COND', fun_list[[nm]], z, fixed = TRUE)
      gsub('NAME', nm, z, fixed = TRUE)
    })
  }), recursive = FALSE)

  # Generate all three scopes as before, including restrictions.
  candidates <- c(expanded, nodedg(expanded, 'nodes'),
                  nodedg(expanded, 'edges'))
  generated <- list()
  emitted <- character()

  for (block in candidates) {
    signature <- grep('^#X .* <- function\\(', block, value = TRUE)
    if (length(signature) != 1L) next
    fun <- sub('^#X ([[:alnum:]_]+) <- function\\(.*$', '\\1', signature)

    scope <- if (endsWith(fun, '_nodes')) 'nodes' else
      if (endsWith(fun, '_edges')) 'edges' else 'both'
    base_fun <- if (scope == 'both') fun else
      substr(fun, 1L, nchar(fun) - nchar(scope) - 1L)
    condition <- names(fun_list)[endsWith(base_fun, paste0('_', names(fun_list)))]
    if (length(condition) != 1L) next

    # Scope restrictions from the original generator.
    if (condition %in% edges_cond && scope == 'nodes') next
    if (condition %in% nodes_cond && scope == 'edges') next
    if (condition %in% edges_cond && scope == 'both') {
      block <- gsub('c("edges", "nodes")', '"edges"', block, fixed = TRUE)
    }
    if (condition %in% nodes_cond && scope == 'both') {
      block <- gsub('c("edges", "nodes")', '"nodes"', block, fixed = TRUE)
    }

    prefix <- substr(base_fun, 1L, nchar(base_fun) - nchar(condition) - 1L)
    if (prefix == 'fill' && scope == 'edges') next
    if (prefix == 'fill' && scope == 'both') {
      block <- gsub('c("edges", "nodes")', '"nodes"', block, fixed = TRUE)
    }
    if (prefix == 'label_location' && scope == 'nodes') next
    if (prefix == 'label_location' && scope == 'both') {
      block <- gsub('c("edges", "nodes")', '"edges"', block, fixed = TRUE)
    }
    if (prefix == 'label_location' && condition %in% c('var', 'obs', 'latent')) next

    if (fun %in% emitted) stop('Duplicate generated function: ', fun)
    emitted <- c(emitted, fun)
    generated[[fun]] <- block
  }

  # Public aliases retain their ORIGINAL formal argument names.
  # Direct assignment would break calls such as color_sig(color = "red")
  # because the canonical function expects `colour`, not `color`.
  # Call reconstruction preserves the caller's unevaluated value expression.
  alias_blocks <- list()
  for (alias in names(aliases)) {
    canonical <- unname(aliases[[alias]])
    canonical_names <- emitted[startsWith(emitted, paste0(canonical, '_'))]
    for (canonical_fun in canonical_names) {
      alias_fun <- paste0(alias, substring(canonical_fun, nchar(canonical) + 1L))
      if (alias_fun %in% emitted) next
      alias_blocks[[alias_fun]] <- c(
        "#' @export",
        "#' @rdname if_edit",
        paste0(alias_fun, ' <- function(data, ', alias, ' = ',
               aes_list[[canonical]], ', ...){'),
        '  cl <- match.call()',
        paste0('  names(cl)[names(cl) == "', alias, '"] <- "', canonical, '"'),
        paste0('  cl[[1L]] <- as.name("', canonical_fun, '")'),
        '  eval.parent(cl)',
        '}',
        ''
      )
      emitted <- c(emitted, alias_fun)
    }
  }

  # Convert template markers to ordinary R/roxygen lines.
  generated_lines <- unlist(generated, use.names = FALSE)
  generated_lines <- sub('^#X ', '', generated_lines)
  generated_lines <- sub('^#Y ', "#' ", generated_lines)
  alias_lines <- unlist(alias_blocks, use.names = FALSE)
  result <- c(header, generated_lines, alias_lines)

  # Validate syntax before overwriting the source file.
  tmp <- tempfile(fileext = '.R')
  on.exit(unlink(tmp), add = TRUE)
  writeLines(result, tmp)
  parse(file = tmp)
  writeLines(result, filename)
  invisible(emitted)
}

#' @title Conditionally edit a sem_graph object
#' @description This function allows users to conditionally manipulate the edges
#' and nodes of a \code{sem_graph} object. The generic function \code{if_edit}
#' applies the expression \code{expr} to all rows of the \code{nodes} and
#' \code{edges} data.frames for which \code{condition} is \code{TRUE}.
#'
#' The wrapper functions documented in the Usage section have a hard-coded
#' \code{expr} and \code{condition}; for example,
#' \code{color_sig(color = "green")} colors all nodes and edges with
#' \code{pval < .05} green. If no column exists for the assigned aesthetic
#' (e.g., \code{color}), the wrappers assign the default argument
#' (in this case, \code{color = "black"}) to all other nodes and edges.
#' @param data Object to manipulate.
#' @param condition Expression that returns a logical vector when evaluated in
#' the environment of \code{data}.
#' @param expr Expression to perform on elements of \code{data} for which
#' \code{condition == TRUE}.
#' @param ... Additional arguments passed to and from functions.
#' @return Object of the same class as \code{data}.
#' @examples
#' library(lavaan)
#' res <- sem("dist ~ speed", cars, meanstructure = TRUE)
#' p <- prepare_graph(res)
#' out <- if_edit(p, condition = {pval < .05}, expr = {label = "sig"})
#' @rdname if_edit
#' @export
if_edit <- function(data, condition, expr, ...){
  UseMethod("if_edit", data)
}

#' @method if_edit data.frame
#' @export
if_edit.data.frame <- function(data, condition, expr, ...){
  cl <- match.call()
  cl <- cl[c(1, match(c("data", "condition"), names(cl)))]
  names(cl)[which(names(cl) == "condition")] <- "expr"
  cl[[1L]] <- quote(with)
  condition_met <- tryCatch({which(eval.parent(cl))}, error = function(e){NULL})
  if(length(condition_met) > 0){
    cl <- match.call()
    cl <- cl[c(1, match(c("data", "expr"), names(cl)))]
    cl[["data"]] <- data[condition_met, , FALSE]
    cl[[1L]] <- quote(within)
    data[condition_met, ] <- eval.parent(cl)
  }
  return(data)
}

#' @rdname if_edit
#' @export
#' @examples
#' out <- if_edges(p, condition = {pval < .05}, expr = {label = "sig"})
if_edges <- function(data, condition, expr, ...){
  cl <- match.call()
  out <- data
  cl[[1L]] <- str2lang("tidySEM:::if_edit.data.frame")
  if("edges" %in% names(data)){
    cl[["data"]] <- data$edges
    out[["edges"]] <- eval.parent(cl)
  }
  out
}

#' @rdname if_edit
#' @export
#' @examples
#' out <- if_nodes(p, condition = {pval < .05}, expr = {label = "sig"})
if_nodes <- function(data, condition, expr, ...){
  cl <- match.call()
  out <- data
  cl[[1L]] <- str2lang("tidySEM:::if_edit.data.frame")
  if("nodes" %in% names(data)){
    cl[["data"]] <- data$nodes
    out[["nodes"]] <- eval.parent(cl)
  }
  out
}

#' @method if_edit sem_graph
#' @param element Character vector. The elements of the \code{sem_graph} to
#' edit, defaults to \code{c("edges", "nodes")}.
#' @rdname if_edit
#' @export
if_edit.sem_graph <- function(data, condition, expr,
                              element = c("edges", "nodes"), ...){
  cl <- match.call()
  out <- data
  cl[[1L]] <- str2lang("tidySEM:::if_edit.data.frame")
  if("edges" %in% element & "edges" %in% names(data)){
    cl[["data"]] <- data$edges
    out[["edges"]] <- eval.parent(cl)
  }
  if("nodes" %in% element & "nodes" %in% names(data)){
    cl[["data"]] <- data$nodes
    out[["nodes"]] <- eval.parent(cl)
  }
  out
}

# Template function
#Y @export
#Y @rdname if_edit
#Y @examples
#Y out <- all_NAME(p, expr = {label = "sig"})
#X all_NAME <- function(data, expr, ...){
#X   cl <- match.call()
#X   cl[["condition"]] <- substitute(COND)
#X   cl[["element"]] = c("edges", "nodes")
#X   cl[[1L]] <- str2lang("tidySEM:::if_edit")
#X   eval.parent(cl)
#X }

#Y @export
#Y @rdname if_edit
#Y @examples
#Y out <- hide_NAME(p)
#X hide_NAME <- function(data, ...){
#X   cl <- match.call()
#X   cl[["condition"]] <- substitute(COND)
#X   cl[["expr"]] <- quote({ show = FALSE })
#X   cl[["element"]] = c("edges", "nodes")
#X   cl[[1L]] <- str2lang("tidySEM:::if_edit")
#X   eval.parent(cl)
#X }

#Y @export
#Y @rdname if_edit
#Y @examples
#Y out <- show_NAME(p)
#X show_NAME <- function(data, ...){
#X   cl <- match.call()
#X   cl[["condition"]] <- substitute(COND)
#X   cl[["expr"]] <- quote({ show = TRUE })
#X   cl[["element"]] = c("edges", "nodes")
#X   cl[[1L]] <- str2lang("tidySEM:::if_edit")
#X   eval.parent(cl)
#X }

#AY @export
#AY @param AES Atomic character vector,
#AY indicating which AES to assign to
#AY the selected elements.
#AY @rdname if_edit
#AY @examples
#AY out <- AES_NAME(p, { AES = AES_DEFAULT })
#AX AES_NAME <- function(data, AES = AES_DEFAULT, ...){
#AX   if(inherits(data, "sem_graph")){
#AX     these_elements <- c("edges", "nodes")
#AX     for(el in these_elements){
#AX       if(!"AES" %in% names(data[[el]])){
#AX         data[[el]]$AES <- AES_DEFAULT
#AX       }
#AX     }
#AX   }
#AX   if(inherits(data, "data.frame")){
#AX     if(!"AES" %in% names(data)){
#AX         data$AES <- AES_DEFAULT
#AX     }
#AX   }
#AX   cl <- match.call()
#AX   cl[["data"]] <- data
#AX   cl[["condition"]] <- substitute(COND)
#AX   cl[["expr"]] <- str2lang(paste0("AES = ", deparse(AES)))
#AX   cl[["element"]] <- c("edges", "nodes")
#AX   cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
#AX   cl[[1L]] <- str2lang("tidySEM:::if_edit")
#AX   eval.parent(cl)
#AX }

# End template

# Automatically generated functions below here
#' @export
#' @rdname if_edit
#' @examples
#' out <- all_sig(p, expr = {label = "sig"})
all_sig <- function(data, expr, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["element"]] = c("edges", "nodes")
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @rdname if_edit
#' @examples
#' out <- hide_sig(p)
hide_sig <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["expr"]] <- quote({ show = FALSE })
  cl[["element"]] = c("edges", "nodes")
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @rdname if_edit
#' @examples
#' out <- show_sig(p)
show_sig <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["expr"]] <- quote({ show = TRUE })
  cl[["element"]] = c("edges", "nodes")
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param colour Atomic character vector,
#' indicating which colour to assign to
#' the selected elements.
#' @param color Synonym of `colour`, which is the preferred spelling for
#' `ggplot`.
#' @rdname if_edit
#' @examples
#' out <- colour_sig(p, { colour = "black" })
colour_sig <- function(data, colour = "black", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- c("edges", "nodes")
    for(el in these_elements){
      if(!"colour" %in% names(data[[el]])){
        data[[el]]$colour <- "black"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"colour" %in% names(data)){
        data$colour <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["expr"]] <- str2lang(paste0("colour = ", deparse(colour)))
  cl[["element"]] <- c("edges", "nodes")
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param linewidth Atomic character vector,
#' indicating which linewidth to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- linewidth_sig(p, { linewidth = 1 })
linewidth_sig <- function(data, linewidth = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- c("edges", "nodes")
    for(el in these_elements){
      if(!"linewidth" %in% names(data[[el]])){
        data[[el]]$linewidth <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"linewidth" %in% names(data)){
        data$linewidth <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["expr"]] <- str2lang(paste0("linewidth = ", deparse(linewidth)))
  cl[["element"]] <- c("edges", "nodes")
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param linetype Atomic character vector,
#' indicating which linetype to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- linetype_sig(p, { linetype = 1 })
linetype_sig <- function(data, linetype = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- c("edges", "nodes")
    for(el in these_elements){
      if(!"linetype" %in% names(data[[el]])){
        data[[el]]$linetype <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"linetype" %in% names(data)){
        data$linetype <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["expr"]] <- str2lang(paste0("linetype = ", deparse(linetype)))
  cl[["element"]] <- c("edges", "nodes")
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param alpha Atomic character vector,
#' indicating which alpha to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- alpha_sig(p, { alpha = 1 })
alpha_sig <- function(data, alpha = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- c("edges", "nodes")
    for(el in these_elements){
      if(!"alpha" %in% names(data[[el]])){
        data[[el]]$alpha <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"alpha" %in% names(data)){
        data$alpha <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["expr"]] <- str2lang(paste0("alpha = ", deparse(alpha)))
  cl[["element"]] <- c("edges", "nodes")
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param fill Atomic character vector,
#' indicating which fill to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- fill_sig(p, { fill = "white" })
fill_sig <- function(data, fill = "white", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"fill" %in% names(data[[el]])){
        data[[el]]$fill <- "white"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"fill" %in% names(data)){
        data$fill <- "white"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["expr"]] <- str2lang(paste0("fill = ", deparse(fill)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_colour Atomic character vector,
#' indicating which label_colour to assign to
#' the selected elements.
#' @param label_color Synonym of `label_colour`, which is the preferred spelling
#' for `ggplot`.
#' @rdname if_edit
#' @examples
#' out <- label_colour_sig(p, { label_colour = "black" })
label_colour_sig <- function(data, label_colour = "black", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- c("edges", "nodes")
    for(el in these_elements){
      if(!"label_colour" %in% names(data[[el]])){
        data[[el]]$label_colour <- "black"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_colour" %in% names(data)){
        data$label_colour <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["expr"]] <- str2lang(paste0("label_colour = ", deparse(label_colour)))
  cl[["element"]] <- c("edges", "nodes")
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_fill Atomic character vector,
#' indicating which label_fill to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_fill_sig(p, { label_fill = "white" })
label_fill_sig <- function(data, label_fill = "white", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- c("edges", "nodes")
    for(el in these_elements){
      if(!"label_fill" %in% names(data[[el]])){
        data[[el]]$label_fill <- "white"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_fill" %in% names(data)){
        data$label_fill <- "white"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["expr"]] <- str2lang(paste0("label_fill = ", deparse(label_fill)))
  cl[["element"]] <- c("edges", "nodes")
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_size Atomic character vector,
#' indicating which label_size to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_size_sig(p, { label_size = 4 })
label_size_sig <- function(data, label_size = 4, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- c("edges", "nodes")
    for(el in these_elements){
      if(!"label_size" %in% names(data[[el]])){
        data[[el]]$label_size <- 4
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_size" %in% names(data)){
        data$label_size <- 4
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["expr"]] <- str2lang(paste0("label_size = ", deparse(label_size)))
  cl[["element"]] <- c("edges", "nodes")
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_alpha Atomic character vector,
#' indicating which label_alpha to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_alpha_sig(p, { label_alpha = 1 })
label_alpha_sig <- function(data, label_alpha = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- c("edges", "nodes")
    for(el in these_elements){
      if(!"label_alpha" %in% names(data[[el]])){
        data[[el]]$label_alpha <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_alpha" %in% names(data)){
        data$label_alpha <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["expr"]] <- str2lang(paste0("label_alpha = ", deparse(label_alpha)))
  cl[["element"]] <- c("edges", "nodes")
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_family Atomic character vector,
#' indicating which label_family to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_family_sig(p, { label_family = "sans" })
label_family_sig <- function(data, label_family = "sans", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- c("edges", "nodes")
    for(el in these_elements){
      if(!"label_family" %in% names(data[[el]])){
        data[[el]]$label_family <- "sans"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_family" %in% names(data)){
        data$label_family <- "sans"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["expr"]] <- str2lang(paste0("label_family = ", deparse(label_family)))
  cl[["element"]] <- c("edges", "nodes")
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_fontface Atomic character vector,
#' indicating which label_fontface to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_fontface_sig(p, { label_fontface = "plain" })
label_fontface_sig <- function(data, label_fontface = "plain", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- c("edges", "nodes")
    for(el in these_elements){
      if(!"label_fontface" %in% names(data[[el]])){
        data[[el]]$label_fontface <- "plain"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_fontface" %in% names(data)){
        data$label_fontface <- "plain"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["expr"]] <- str2lang(paste0("label_fontface = ", deparse(label_fontface)))
  cl[["element"]] <- c("edges", "nodes")
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_hjust Atomic character vector,
#' indicating which label_hjust to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_hjust_sig(p, { label_hjust = "center" })
label_hjust_sig <- function(data, label_hjust = "center", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- c("edges", "nodes")
    for(el in these_elements){
      if(!"label_hjust" %in% names(data[[el]])){
        data[[el]]$label_hjust <- "center"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_hjust" %in% names(data)){
        data$label_hjust <- "center"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["expr"]] <- str2lang(paste0("label_hjust = ", deparse(label_hjust)))
  cl[["element"]] <- c("edges", "nodes")
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_vjust Atomic character vector,
#' indicating which label_vjust to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_vjust_sig(p, { label_vjust = "middle" })
label_vjust_sig <- function(data, label_vjust = "middle", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- c("edges", "nodes")
    for(el in these_elements){
      if(!"label_vjust" %in% names(data[[el]])){
        data[[el]]$label_vjust <- "middle"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_vjust" %in% names(data)){
        data$label_vjust <- "middle"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["expr"]] <- str2lang(paste0("label_vjust = ", deparse(label_vjust)))
  cl[["element"]] <- c("edges", "nodes")
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_lineheight Atomic character vector,
#' indicating which label_lineheight to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_lineheight_sig(p, { label_lineheight = 1 })
label_lineheight_sig <- function(data, label_lineheight = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- c("edges", "nodes")
    for(el in these_elements){
      if(!"label_lineheight" %in% names(data[[el]])){
        data[[el]]$label_lineheight <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_lineheight" %in% names(data)){
        data$label_lineheight <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["expr"]] <- str2lang(paste0("label_lineheight = ", deparse(label_lineheight)))
  cl[["element"]] <- c("edges", "nodes")
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_location Atomic character vector,
#' indicating which label_location to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_location_sig(p, { label_location = .5 })
label_location_sig <- function(data, label_location = .5, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_location" %in% names(data[[el]])){
        data[[el]]$label_location <- .5
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_location" %in% names(data)){
        data$label_location <- .5
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["expr"]] <- str2lang(paste0("label_location = ", deparse(label_location)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @rdname if_edit
#' @examples
#' out <- all_nonsig(p, expr = {label = "sig"})
all_nonsig <- function(data, expr, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["element"]] = c("edges", "nodes")
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @rdname if_edit
#' @examples
#' out <- hide_nonsig(p)
hide_nonsig <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["expr"]] <- quote({ show = FALSE })
  cl[["element"]] = c("edges", "nodes")
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @rdname if_edit
#' @examples
#' out <- show_nonsig(p)
show_nonsig <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["expr"]] <- quote({ show = TRUE })
  cl[["element"]] = c("edges", "nodes")
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param colour Atomic character vector,
#' indicating which colour to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- colour_nonsig(p, { colour = "black" })
colour_nonsig <- function(data, colour = "black", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- c("edges", "nodes")
    for(el in these_elements){
      if(!"colour" %in% names(data[[el]])){
        data[[el]]$colour <- "black"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"colour" %in% names(data)){
        data$colour <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["expr"]] <- str2lang(paste0("colour = ", deparse(colour)))
  cl[["element"]] <- c("edges", "nodes")
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param linewidth Atomic character vector,
#' indicating which linewidth to assign to
#' the selected elements.
#' @param size Synonym for `linewidth`, which is preferred because it removes
#' ambiguity.
#' @rdname if_edit
#' @examples
#' out <- linewidth_nonsig(p, { linewidth = 1 })
linewidth_nonsig <- function(data, linewidth = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- c("edges", "nodes")
    for(el in these_elements){
      if(!"linewidth" %in% names(data[[el]])){
        data[[el]]$linewidth <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"linewidth" %in% names(data)){
        data$linewidth <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["expr"]] <- str2lang(paste0("linewidth = ", deparse(linewidth)))
  cl[["element"]] <- c("edges", "nodes")
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param linetype Atomic character vector,
#' indicating which linetype to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- linetype_nonsig(p, { linetype = 1 })
linetype_nonsig <- function(data, linetype = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- c("edges", "nodes")
    for(el in these_elements){
      if(!"linetype" %in% names(data[[el]])){
        data[[el]]$linetype <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"linetype" %in% names(data)){
        data$linetype <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["expr"]] <- str2lang(paste0("linetype = ", deparse(linetype)))
  cl[["element"]] <- c("edges", "nodes")
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param alpha Atomic character vector,
#' indicating which alpha to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- alpha_nonsig(p, { alpha = 1 })
alpha_nonsig <- function(data, alpha = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- c("edges", "nodes")
    for(el in these_elements){
      if(!"alpha" %in% names(data[[el]])){
        data[[el]]$alpha <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"alpha" %in% names(data)){
        data$alpha <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["expr"]] <- str2lang(paste0("alpha = ", deparse(alpha)))
  cl[["element"]] <- c("edges", "nodes")
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param fill Atomic character vector,
#' indicating which fill to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- fill_nonsig(p, { fill = "white" })
fill_nonsig <- function(data, fill = "white", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"fill" %in% names(data[[el]])){
        data[[el]]$fill <- "white"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"fill" %in% names(data)){
        data$fill <- "white"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["expr"]] <- str2lang(paste0("fill = ", deparse(fill)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_colour Atomic character vector,
#' indicating which label_colour to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_colour_nonsig(p, { label_colour = "black" })
label_colour_nonsig <- function(data, label_colour = "black", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- c("edges", "nodes")
    for(el in these_elements){
      if(!"label_colour" %in% names(data[[el]])){
        data[[el]]$label_colour <- "black"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_colour" %in% names(data)){
        data$label_colour <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["expr"]] <- str2lang(paste0("label_colour = ", deparse(label_colour)))
  cl[["element"]] <- c("edges", "nodes")
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_fill Atomic character vector,
#' indicating which label_fill to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_fill_nonsig(p, { label_fill = "white" })
label_fill_nonsig <- function(data, label_fill = "white", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- c("edges", "nodes")
    for(el in these_elements){
      if(!"label_fill" %in% names(data[[el]])){
        data[[el]]$label_fill <- "white"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_fill" %in% names(data)){
        data$label_fill <- "white"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["expr"]] <- str2lang(paste0("label_fill = ", deparse(label_fill)))
  cl[["element"]] <- c("edges", "nodes")
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_size Atomic character vector,
#' indicating which label_size to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_size_nonsig(p, { label_size = 4 })
label_size_nonsig <- function(data, label_size = 4, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- c("edges", "nodes")
    for(el in these_elements){
      if(!"label_size" %in% names(data[[el]])){
        data[[el]]$label_size <- 4
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_size" %in% names(data)){
        data$label_size <- 4
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["expr"]] <- str2lang(paste0("label_size = ", deparse(label_size)))
  cl[["element"]] <- c("edges", "nodes")
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_alpha Atomic character vector,
#' indicating which label_alpha to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_alpha_nonsig(p, { label_alpha = 1 })
label_alpha_nonsig <- function(data, label_alpha = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- c("edges", "nodes")
    for(el in these_elements){
      if(!"label_alpha" %in% names(data[[el]])){
        data[[el]]$label_alpha <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_alpha" %in% names(data)){
        data$label_alpha <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["expr"]] <- str2lang(paste0("label_alpha = ", deparse(label_alpha)))
  cl[["element"]] <- c("edges", "nodes")
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_family Atomic character vector,
#' indicating which label_family to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_family_nonsig(p, { label_family = "sans" })
label_family_nonsig <- function(data, label_family = "sans", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- c("edges", "nodes")
    for(el in these_elements){
      if(!"label_family" %in% names(data[[el]])){
        data[[el]]$label_family <- "sans"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_family" %in% names(data)){
        data$label_family <- "sans"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["expr"]] <- str2lang(paste0("label_family = ", deparse(label_family)))
  cl[["element"]] <- c("edges", "nodes")
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_fontface Atomic character vector,
#' indicating which label_fontface to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_fontface_nonsig(p, { label_fontface = "plain" })
label_fontface_nonsig <- function(data, label_fontface = "plain", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- c("edges", "nodes")
    for(el in these_elements){
      if(!"label_fontface" %in% names(data[[el]])){
        data[[el]]$label_fontface <- "plain"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_fontface" %in% names(data)){
        data$label_fontface <- "plain"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["expr"]] <- str2lang(paste0("label_fontface = ", deparse(label_fontface)))
  cl[["element"]] <- c("edges", "nodes")
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_hjust Atomic character vector,
#' indicating which label_hjust to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_hjust_nonsig(p, { label_hjust = "center" })
label_hjust_nonsig <- function(data, label_hjust = "center", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- c("edges", "nodes")
    for(el in these_elements){
      if(!"label_hjust" %in% names(data[[el]])){
        data[[el]]$label_hjust <- "center"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_hjust" %in% names(data)){
        data$label_hjust <- "center"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["expr"]] <- str2lang(paste0("label_hjust = ", deparse(label_hjust)))
  cl[["element"]] <- c("edges", "nodes")
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_vjust Atomic character vector,
#' indicating which label_vjust to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_vjust_nonsig(p, { label_vjust = "middle" })
label_vjust_nonsig <- function(data, label_vjust = "middle", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- c("edges", "nodes")
    for(el in these_elements){
      if(!"label_vjust" %in% names(data[[el]])){
        data[[el]]$label_vjust <- "middle"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_vjust" %in% names(data)){
        data$label_vjust <- "middle"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["expr"]] <- str2lang(paste0("label_vjust = ", deparse(label_vjust)))
  cl[["element"]] <- c("edges", "nodes")
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_lineheight Atomic character vector,
#' indicating which label_lineheight to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_lineheight_nonsig(p, { label_lineheight = 1 })
label_lineheight_nonsig <- function(data, label_lineheight = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- c("edges", "nodes")
    for(el in these_elements){
      if(!"label_lineheight" %in% names(data[[el]])){
        data[[el]]$label_lineheight <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_lineheight" %in% names(data)){
        data$label_lineheight <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["expr"]] <- str2lang(paste0("label_lineheight = ", deparse(label_lineheight)))
  cl[["element"]] <- c("edges", "nodes")
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_location Atomic character vector,
#' indicating which label_location to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_location_nonsig(p, { label_location = .5 })
label_location_nonsig <- function(data, label_location = .5, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_location" %in% names(data[[el]])){
        data[[el]]$label_location <- .5
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_location" %in% names(data)){
        data$label_location <- .5
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["expr"]] <- str2lang(paste0("label_location = ", deparse(label_location)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @rdname if_edit
#' @examples
#' out <- all_fixed(p, expr = {label = "sig"})
all_fixed <- function(data, expr, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["element"]] = c("edges", "nodes")
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @rdname if_edit
#' @examples
#' out <- hide_fixed(p)
hide_fixed <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["expr"]] <- quote({ show = FALSE })
  cl[["element"]] = c("edges", "nodes")
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @rdname if_edit
#' @examples
#' out <- show_fixed(p)
show_fixed <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["expr"]] <- quote({ show = TRUE })
  cl[["element"]] = c("edges", "nodes")
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param colour Atomic character vector,
#' indicating which colour to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- colour_fixed(p, { colour = "black" })
colour_fixed <- function(data, colour = "black", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- c("edges", "nodes")
    for(el in these_elements){
      if(!"colour" %in% names(data[[el]])){
        data[[el]]$colour <- "black"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"colour" %in% names(data)){
        data$colour <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["expr"]] <- str2lang(paste0("colour = ", deparse(colour)))
  cl[["element"]] <- c("edges", "nodes")
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param linewidth Atomic character vector,
#' indicating which linewidth to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- linewidth_fixed(p, { linewidth = 1 })
linewidth_fixed <- function(data, linewidth = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- c("edges", "nodes")
    for(el in these_elements){
      if(!"linewidth" %in% names(data[[el]])){
        data[[el]]$linewidth <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"linewidth" %in% names(data)){
        data$linewidth <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["expr"]] <- str2lang(paste0("linewidth = ", deparse(linewidth)))
  cl[["element"]] <- c("edges", "nodes")
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param linetype Atomic character vector,
#' indicating which linetype to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- linetype_fixed(p, { linetype = 1 })
linetype_fixed <- function(data, linetype = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- c("edges", "nodes")
    for(el in these_elements){
      if(!"linetype" %in% names(data[[el]])){
        data[[el]]$linetype <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"linetype" %in% names(data)){
        data$linetype <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["expr"]] <- str2lang(paste0("linetype = ", deparse(linetype)))
  cl[["element"]] <- c("edges", "nodes")
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param alpha Atomic character vector,
#' indicating which alpha to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- alpha_fixed(p, { alpha = 1 })
alpha_fixed <- function(data, alpha = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- c("edges", "nodes")
    for(el in these_elements){
      if(!"alpha" %in% names(data[[el]])){
        data[[el]]$alpha <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"alpha" %in% names(data)){
        data$alpha <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["expr"]] <- str2lang(paste0("alpha = ", deparse(alpha)))
  cl[["element"]] <- c("edges", "nodes")
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param fill Atomic character vector,
#' indicating which fill to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- fill_fixed(p, { fill = "white" })
fill_fixed <- function(data, fill = "white", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"fill" %in% names(data[[el]])){
        data[[el]]$fill <- "white"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"fill" %in% names(data)){
        data$fill <- "white"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["expr"]] <- str2lang(paste0("fill = ", deparse(fill)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_colour Atomic character vector,
#' indicating which label_colour to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_colour_fixed(p, { label_colour = "black" })
label_colour_fixed <- function(data, label_colour = "black", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- c("edges", "nodes")
    for(el in these_elements){
      if(!"label_colour" %in% names(data[[el]])){
        data[[el]]$label_colour <- "black"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_colour" %in% names(data)){
        data$label_colour <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["expr"]] <- str2lang(paste0("label_colour = ", deparse(label_colour)))
  cl[["element"]] <- c("edges", "nodes")
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_fill Atomic character vector,
#' indicating which label_fill to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_fill_fixed(p, { label_fill = "white" })
label_fill_fixed <- function(data, label_fill = "white", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- c("edges", "nodes")
    for(el in these_elements){
      if(!"label_fill" %in% names(data[[el]])){
        data[[el]]$label_fill <- "white"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_fill" %in% names(data)){
        data$label_fill <- "white"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["expr"]] <- str2lang(paste0("label_fill = ", deparse(label_fill)))
  cl[["element"]] <- c("edges", "nodes")
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_size Atomic character vector,
#' indicating which label_size to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_size_fixed(p, { label_size = 4 })
label_size_fixed <- function(data, label_size = 4, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- c("edges", "nodes")
    for(el in these_elements){
      if(!"label_size" %in% names(data[[el]])){
        data[[el]]$label_size <- 4
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_size" %in% names(data)){
        data$label_size <- 4
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["expr"]] <- str2lang(paste0("label_size = ", deparse(label_size)))
  cl[["element"]] <- c("edges", "nodes")
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_alpha Atomic character vector,
#' indicating which label_alpha to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_alpha_fixed(p, { label_alpha = 1 })
label_alpha_fixed <- function(data, label_alpha = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- c("edges", "nodes")
    for(el in these_elements){
      if(!"label_alpha" %in% names(data[[el]])){
        data[[el]]$label_alpha <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_alpha" %in% names(data)){
        data$label_alpha <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["expr"]] <- str2lang(paste0("label_alpha = ", deparse(label_alpha)))
  cl[["element"]] <- c("edges", "nodes")
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_family Atomic character vector,
#' indicating which label_family to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_family_fixed(p, { label_family = "sans" })
label_family_fixed <- function(data, label_family = "sans", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- c("edges", "nodes")
    for(el in these_elements){
      if(!"label_family" %in% names(data[[el]])){
        data[[el]]$label_family <- "sans"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_family" %in% names(data)){
        data$label_family <- "sans"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["expr"]] <- str2lang(paste0("label_family = ", deparse(label_family)))
  cl[["element"]] <- c("edges", "nodes")
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_fontface Atomic character vector,
#' indicating which label_fontface to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_fontface_fixed(p, { label_fontface = "plain" })
label_fontface_fixed <- function(data, label_fontface = "plain", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- c("edges", "nodes")
    for(el in these_elements){
      if(!"label_fontface" %in% names(data[[el]])){
        data[[el]]$label_fontface <- "plain"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_fontface" %in% names(data)){
        data$label_fontface <- "plain"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["expr"]] <- str2lang(paste0("label_fontface = ", deparse(label_fontface)))
  cl[["element"]] <- c("edges", "nodes")
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_hjust Atomic character vector,
#' indicating which label_hjust to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_hjust_fixed(p, { label_hjust = "center" })
label_hjust_fixed <- function(data, label_hjust = "center", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- c("edges", "nodes")
    for(el in these_elements){
      if(!"label_hjust" %in% names(data[[el]])){
        data[[el]]$label_hjust <- "center"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_hjust" %in% names(data)){
        data$label_hjust <- "center"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["expr"]] <- str2lang(paste0("label_hjust = ", deparse(label_hjust)))
  cl[["element"]] <- c("edges", "nodes")
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_vjust Atomic character vector,
#' indicating which label_vjust to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_vjust_fixed(p, { label_vjust = "middle" })
label_vjust_fixed <- function(data, label_vjust = "middle", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- c("edges", "nodes")
    for(el in these_elements){
      if(!"label_vjust" %in% names(data[[el]])){
        data[[el]]$label_vjust <- "middle"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_vjust" %in% names(data)){
        data$label_vjust <- "middle"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["expr"]] <- str2lang(paste0("label_vjust = ", deparse(label_vjust)))
  cl[["element"]] <- c("edges", "nodes")
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_lineheight Atomic character vector,
#' indicating which label_lineheight to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_lineheight_fixed(p, { label_lineheight = 1 })
label_lineheight_fixed <- function(data, label_lineheight = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- c("edges", "nodes")
    for(el in these_elements){
      if(!"label_lineheight" %in% names(data[[el]])){
        data[[el]]$label_lineheight <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_lineheight" %in% names(data)){
        data$label_lineheight <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["expr"]] <- str2lang(paste0("label_lineheight = ", deparse(label_lineheight)))
  cl[["element"]] <- c("edges", "nodes")
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_location Atomic character vector,
#' indicating which label_location to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_location_fixed(p, { label_location = .5 })
label_location_fixed <- function(data, label_location = .5, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_location" %in% names(data[[el]])){
        data[[el]]$label_location <- .5
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_location" %in% names(data)){
        data$label_location <- .5
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["expr"]] <- str2lang(paste0("label_location = ", deparse(label_location)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @rdname if_edit
#' @examples
#' out <- all_pos(p, expr = {label = "sig"})
all_pos <- function(data, expr, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["element"]] = c("edges", "nodes")
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @rdname if_edit
#' @examples
#' out <- hide_pos(p)
hide_pos <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["expr"]] <- quote({ show = FALSE })
  cl[["element"]] = c("edges", "nodes")
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @rdname if_edit
#' @examples
#' out <- show_pos(p)
show_pos <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["expr"]] <- quote({ show = TRUE })
  cl[["element"]] = c("edges", "nodes")
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param colour Atomic character vector,
#' indicating which colour to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- colour_pos(p, { colour = "black" })
colour_pos <- function(data, colour = "black", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- c("edges", "nodes")
    for(el in these_elements){
      if(!"colour" %in% names(data[[el]])){
        data[[el]]$colour <- "black"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"colour" %in% names(data)){
        data$colour <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["expr"]] <- str2lang(paste0("colour = ", deparse(colour)))
  cl[["element"]] <- c("edges", "nodes")
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param linewidth Atomic character vector,
#' indicating which linewidth to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- linewidth_pos(p, { linewidth = 1 })
linewidth_pos <- function(data, linewidth = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- c("edges", "nodes")
    for(el in these_elements){
      if(!"linewidth" %in% names(data[[el]])){
        data[[el]]$linewidth <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"linewidth" %in% names(data)){
        data$linewidth <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["expr"]] <- str2lang(paste0("linewidth = ", deparse(linewidth)))
  cl[["element"]] <- c("edges", "nodes")
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param linetype Atomic character vector,
#' indicating which linetype to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- linetype_pos(p, { linetype = 1 })
linetype_pos <- function(data, linetype = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- c("edges", "nodes")
    for(el in these_elements){
      if(!"linetype" %in% names(data[[el]])){
        data[[el]]$linetype <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"linetype" %in% names(data)){
        data$linetype <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["expr"]] <- str2lang(paste0("linetype = ", deparse(linetype)))
  cl[["element"]] <- c("edges", "nodes")
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param alpha Atomic character vector,
#' indicating which alpha to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- alpha_pos(p, { alpha = 1 })
alpha_pos <- function(data, alpha = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- c("edges", "nodes")
    for(el in these_elements){
      if(!"alpha" %in% names(data[[el]])){
        data[[el]]$alpha <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"alpha" %in% names(data)){
        data$alpha <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["expr"]] <- str2lang(paste0("alpha = ", deparse(alpha)))
  cl[["element"]] <- c("edges", "nodes")
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param fill Atomic character vector,
#' indicating which fill to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- fill_pos(p, { fill = "white" })
fill_pos <- function(data, fill = "white", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"fill" %in% names(data[[el]])){
        data[[el]]$fill <- "white"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"fill" %in% names(data)){
        data$fill <- "white"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["expr"]] <- str2lang(paste0("fill = ", deparse(fill)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_colour Atomic character vector,
#' indicating which label_colour to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_colour_pos(p, { label_colour = "black" })
label_colour_pos <- function(data, label_colour = "black", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- c("edges", "nodes")
    for(el in these_elements){
      if(!"label_colour" %in% names(data[[el]])){
        data[[el]]$label_colour <- "black"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_colour" %in% names(data)){
        data$label_colour <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["expr"]] <- str2lang(paste0("label_colour = ", deparse(label_colour)))
  cl[["element"]] <- c("edges", "nodes")
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_fill Atomic character vector,
#' indicating which label_fill to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_fill_pos(p, { label_fill = "white" })
label_fill_pos <- function(data, label_fill = "white", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- c("edges", "nodes")
    for(el in these_elements){
      if(!"label_fill" %in% names(data[[el]])){
        data[[el]]$label_fill <- "white"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_fill" %in% names(data)){
        data$label_fill <- "white"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["expr"]] <- str2lang(paste0("label_fill = ", deparse(label_fill)))
  cl[["element"]] <- c("edges", "nodes")
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_size Atomic character vector,
#' indicating which label_size to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_size_pos(p, { label_size = 4 })
label_size_pos <- function(data, label_size = 4, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- c("edges", "nodes")
    for(el in these_elements){
      if(!"label_size" %in% names(data[[el]])){
        data[[el]]$label_size <- 4
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_size" %in% names(data)){
        data$label_size <- 4
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["expr"]] <- str2lang(paste0("label_size = ", deparse(label_size)))
  cl[["element"]] <- c("edges", "nodes")
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_alpha Atomic character vector,
#' indicating which label_alpha to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_alpha_pos(p, { label_alpha = 1 })
label_alpha_pos <- function(data, label_alpha = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- c("edges", "nodes")
    for(el in these_elements){
      if(!"label_alpha" %in% names(data[[el]])){
        data[[el]]$label_alpha <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_alpha" %in% names(data)){
        data$label_alpha <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["expr"]] <- str2lang(paste0("label_alpha = ", deparse(label_alpha)))
  cl[["element"]] <- c("edges", "nodes")
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_family Atomic character vector,
#' indicating which label_family to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_family_pos(p, { label_family = "sans" })
label_family_pos <- function(data, label_family = "sans", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- c("edges", "nodes")
    for(el in these_elements){
      if(!"label_family" %in% names(data[[el]])){
        data[[el]]$label_family <- "sans"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_family" %in% names(data)){
        data$label_family <- "sans"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["expr"]] <- str2lang(paste0("label_family = ", deparse(label_family)))
  cl[["element"]] <- c("edges", "nodes")
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_fontface Atomic character vector,
#' indicating which label_fontface to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_fontface_pos(p, { label_fontface = "plain" })
label_fontface_pos <- function(data, label_fontface = "plain", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- c("edges", "nodes")
    for(el in these_elements){
      if(!"label_fontface" %in% names(data[[el]])){
        data[[el]]$label_fontface <- "plain"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_fontface" %in% names(data)){
        data$label_fontface <- "plain"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["expr"]] <- str2lang(paste0("label_fontface = ", deparse(label_fontface)))
  cl[["element"]] <- c("edges", "nodes")
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_hjust Atomic character vector,
#' indicating which label_hjust to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_hjust_pos(p, { label_hjust = "center" })
label_hjust_pos <- function(data, label_hjust = "center", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- c("edges", "nodes")
    for(el in these_elements){
      if(!"label_hjust" %in% names(data[[el]])){
        data[[el]]$label_hjust <- "center"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_hjust" %in% names(data)){
        data$label_hjust <- "center"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["expr"]] <- str2lang(paste0("label_hjust = ", deparse(label_hjust)))
  cl[["element"]] <- c("edges", "nodes")
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_vjust Atomic character vector,
#' indicating which label_vjust to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_vjust_pos(p, { label_vjust = "middle" })
label_vjust_pos <- function(data, label_vjust = "middle", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- c("edges", "nodes")
    for(el in these_elements){
      if(!"label_vjust" %in% names(data[[el]])){
        data[[el]]$label_vjust <- "middle"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_vjust" %in% names(data)){
        data$label_vjust <- "middle"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["expr"]] <- str2lang(paste0("label_vjust = ", deparse(label_vjust)))
  cl[["element"]] <- c("edges", "nodes")
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_lineheight Atomic character vector,
#' indicating which label_lineheight to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_lineheight_pos(p, { label_lineheight = 1 })
label_lineheight_pos <- function(data, label_lineheight = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- c("edges", "nodes")
    for(el in these_elements){
      if(!"label_lineheight" %in% names(data[[el]])){
        data[[el]]$label_lineheight <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_lineheight" %in% names(data)){
        data$label_lineheight <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["expr"]] <- str2lang(paste0("label_lineheight = ", deparse(label_lineheight)))
  cl[["element"]] <- c("edges", "nodes")
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_location Atomic character vector,
#' indicating which label_location to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_location_pos(p, { label_location = .5 })
label_location_pos <- function(data, label_location = .5, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_location" %in% names(data[[el]])){
        data[[el]]$label_location <- .5
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_location" %in% names(data)){
        data$label_location <- .5
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["expr"]] <- str2lang(paste0("label_location = ", deparse(label_location)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @rdname if_edit
#' @examples
#' out <- all_neg(p, expr = {label = "sig"})
all_neg <- function(data, expr, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["element"]] = c("edges", "nodes")
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @rdname if_edit
#' @examples
#' out <- hide_neg(p)
hide_neg <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["expr"]] <- quote({ show = FALSE })
  cl[["element"]] = c("edges", "nodes")
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @rdname if_edit
#' @examples
#' out <- show_neg(p)
show_neg <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["expr"]] <- quote({ show = TRUE })
  cl[["element"]] = c("edges", "nodes")
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param colour Atomic character vector,
#' indicating which colour to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- colour_neg(p, { colour = "black" })
colour_neg <- function(data, colour = "black", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- c("edges", "nodes")
    for(el in these_elements){
      if(!"colour" %in% names(data[[el]])){
        data[[el]]$colour <- "black"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"colour" %in% names(data)){
        data$colour <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["expr"]] <- str2lang(paste0("colour = ", deparse(colour)))
  cl[["element"]] <- c("edges", "nodes")
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param linewidth Atomic character vector,
#' indicating which linewidth to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- linewidth_neg(p, { linewidth = 1 })
linewidth_neg <- function(data, linewidth = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- c("edges", "nodes")
    for(el in these_elements){
      if(!"linewidth" %in% names(data[[el]])){
        data[[el]]$linewidth <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"linewidth" %in% names(data)){
        data$linewidth <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["expr"]] <- str2lang(paste0("linewidth = ", deparse(linewidth)))
  cl[["element"]] <- c("edges", "nodes")
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param linetype Atomic character vector,
#' indicating which linetype to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- linetype_neg(p, { linetype = 1 })
linetype_neg <- function(data, linetype = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- c("edges", "nodes")
    for(el in these_elements){
      if(!"linetype" %in% names(data[[el]])){
        data[[el]]$linetype <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"linetype" %in% names(data)){
        data$linetype <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["expr"]] <- str2lang(paste0("linetype = ", deparse(linetype)))
  cl[["element"]] <- c("edges", "nodes")
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param alpha Atomic character vector,
#' indicating which alpha to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- alpha_neg(p, { alpha = 1 })
alpha_neg <- function(data, alpha = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- c("edges", "nodes")
    for(el in these_elements){
      if(!"alpha" %in% names(data[[el]])){
        data[[el]]$alpha <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"alpha" %in% names(data)){
        data$alpha <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["expr"]] <- str2lang(paste0("alpha = ", deparse(alpha)))
  cl[["element"]] <- c("edges", "nodes")
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param fill Atomic character vector,
#' indicating which fill to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- fill_neg(p, { fill = "white" })
fill_neg <- function(data, fill = "white", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"fill" %in% names(data[[el]])){
        data[[el]]$fill <- "white"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"fill" %in% names(data)){
        data$fill <- "white"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["expr"]] <- str2lang(paste0("fill = ", deparse(fill)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_colour Atomic character vector,
#' indicating which label_colour to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_colour_neg(p, { label_colour = "black" })
label_colour_neg <- function(data, label_colour = "black", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- c("edges", "nodes")
    for(el in these_elements){
      if(!"label_colour" %in% names(data[[el]])){
        data[[el]]$label_colour <- "black"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_colour" %in% names(data)){
        data$label_colour <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["expr"]] <- str2lang(paste0("label_colour = ", deparse(label_colour)))
  cl[["element"]] <- c("edges", "nodes")
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_fill Atomic character vector,
#' indicating which label_fill to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_fill_neg(p, { label_fill = "white" })
label_fill_neg <- function(data, label_fill = "white", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- c("edges", "nodes")
    for(el in these_elements){
      if(!"label_fill" %in% names(data[[el]])){
        data[[el]]$label_fill <- "white"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_fill" %in% names(data)){
        data$label_fill <- "white"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["expr"]] <- str2lang(paste0("label_fill = ", deparse(label_fill)))
  cl[["element"]] <- c("edges", "nodes")
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_size Atomic character vector,
#' indicating which label_size to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_size_neg(p, { label_size = 4 })
label_size_neg <- function(data, label_size = 4, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- c("edges", "nodes")
    for(el in these_elements){
      if(!"label_size" %in% names(data[[el]])){
        data[[el]]$label_size <- 4
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_size" %in% names(data)){
        data$label_size <- 4
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["expr"]] <- str2lang(paste0("label_size = ", deparse(label_size)))
  cl[["element"]] <- c("edges", "nodes")
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_alpha Atomic character vector,
#' indicating which label_alpha to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_alpha_neg(p, { label_alpha = 1 })
label_alpha_neg <- function(data, label_alpha = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- c("edges", "nodes")
    for(el in these_elements){
      if(!"label_alpha" %in% names(data[[el]])){
        data[[el]]$label_alpha <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_alpha" %in% names(data)){
        data$label_alpha <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["expr"]] <- str2lang(paste0("label_alpha = ", deparse(label_alpha)))
  cl[["element"]] <- c("edges", "nodes")
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_family Atomic character vector,
#' indicating which label_family to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_family_neg(p, { label_family = "sans" })
label_family_neg <- function(data, label_family = "sans", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- c("edges", "nodes")
    for(el in these_elements){
      if(!"label_family" %in% names(data[[el]])){
        data[[el]]$label_family <- "sans"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_family" %in% names(data)){
        data$label_family <- "sans"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["expr"]] <- str2lang(paste0("label_family = ", deparse(label_family)))
  cl[["element"]] <- c("edges", "nodes")
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_fontface Atomic character vector,
#' indicating which label_fontface to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_fontface_neg(p, { label_fontface = "plain" })
label_fontface_neg <- function(data, label_fontface = "plain", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- c("edges", "nodes")
    for(el in these_elements){
      if(!"label_fontface" %in% names(data[[el]])){
        data[[el]]$label_fontface <- "plain"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_fontface" %in% names(data)){
        data$label_fontface <- "plain"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["expr"]] <- str2lang(paste0("label_fontface = ", deparse(label_fontface)))
  cl[["element"]] <- c("edges", "nodes")
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_hjust Atomic character vector,
#' indicating which label_hjust to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_hjust_neg(p, { label_hjust = "center" })
label_hjust_neg <- function(data, label_hjust = "center", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- c("edges", "nodes")
    for(el in these_elements){
      if(!"label_hjust" %in% names(data[[el]])){
        data[[el]]$label_hjust <- "center"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_hjust" %in% names(data)){
        data$label_hjust <- "center"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["expr"]] <- str2lang(paste0("label_hjust = ", deparse(label_hjust)))
  cl[["element"]] <- c("edges", "nodes")
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_vjust Atomic character vector,
#' indicating which label_vjust to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_vjust_neg(p, { label_vjust = "middle" })
label_vjust_neg <- function(data, label_vjust = "middle", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- c("edges", "nodes")
    for(el in these_elements){
      if(!"label_vjust" %in% names(data[[el]])){
        data[[el]]$label_vjust <- "middle"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_vjust" %in% names(data)){
        data$label_vjust <- "middle"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["expr"]] <- str2lang(paste0("label_vjust = ", deparse(label_vjust)))
  cl[["element"]] <- c("edges", "nodes")
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_lineheight Atomic character vector,
#' indicating which label_lineheight to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_lineheight_neg(p, { label_lineheight = 1 })
label_lineheight_neg <- function(data, label_lineheight = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- c("edges", "nodes")
    for(el in these_elements){
      if(!"label_lineheight" %in% names(data[[el]])){
        data[[el]]$label_lineheight <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_lineheight" %in% names(data)){
        data$label_lineheight <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["expr"]] <- str2lang(paste0("label_lineheight = ", deparse(label_lineheight)))
  cl[["element"]] <- c("edges", "nodes")
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_location Atomic character vector,
#' indicating which label_location to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_location_neg(p, { label_location = .5 })
label_location_neg <- function(data, label_location = .5, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_location" %in% names(data[[el]])){
        data[[el]]$label_location <- .5
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_location" %in% names(data)){
        data$label_location <- .5
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["expr"]] <- str2lang(paste0("label_location = ", deparse(label_location)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @rdname if_edit
#' @examples
#' out <- all_var(p, expr = {label = "sig"})
all_var <- function(data, expr, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(lhs == rhs & op == "~~")
  cl[["element"]] = "edges"
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @rdname if_edit
#' @examples
#' out <- hide_var(p)
hide_var <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(lhs == rhs & op == "~~")
  cl[["expr"]] <- quote({ show = FALSE })
  cl[["element"]] = "edges"
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @rdname if_edit
#' @examples
#' out <- show_var(p)
show_var <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(lhs == rhs & op == "~~")
  cl[["expr"]] <- quote({ show = TRUE })
  cl[["element"]] = "edges"
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param colour Atomic character vector,
#' indicating which colour to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- colour_var(p, { colour = "black" })
colour_var <- function(data, colour = "black", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"colour" %in% names(data[[el]])){
        data[[el]]$colour <- "black"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"colour" %in% names(data)){
        data$colour <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(lhs == rhs & op == "~~")
  cl[["expr"]] <- str2lang(paste0("colour = ", deparse(colour)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param linewidth Atomic character vector,
#' indicating which linewidth to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- linewidth_var(p, { linewidth = 1 })
linewidth_var <- function(data, linewidth = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"linewidth" %in% names(data[[el]])){
        data[[el]]$linewidth <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"linewidth" %in% names(data)){
        data$linewidth <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(lhs == rhs & op == "~~")
  cl[["expr"]] <- str2lang(paste0("linewidth = ", deparse(linewidth)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param linetype Atomic character vector,
#' indicating which linetype to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- linetype_var(p, { linetype = 1 })
linetype_var <- function(data, linetype = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"linetype" %in% names(data[[el]])){
        data[[el]]$linetype <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"linetype" %in% names(data)){
        data$linetype <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(lhs == rhs & op == "~~")
  cl[["expr"]] <- str2lang(paste0("linetype = ", deparse(linetype)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param alpha Atomic character vector,
#' indicating which alpha to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- alpha_var(p, { alpha = 1 })
alpha_var <- function(data, alpha = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"alpha" %in% names(data[[el]])){
        data[[el]]$alpha <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"alpha" %in% names(data)){
        data$alpha <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(lhs == rhs & op == "~~")
  cl[["expr"]] <- str2lang(paste0("alpha = ", deparse(alpha)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param fill Atomic character vector,
#' indicating which fill to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- fill_var(p, { fill = "white" })
fill_var <- function(data, fill = "white", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"fill" %in% names(data[[el]])){
        data[[el]]$fill <- "white"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"fill" %in% names(data)){
        data$fill <- "white"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(lhs == rhs & op == "~~")
  cl[["expr"]] <- str2lang(paste0("fill = ", deparse(fill)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_colour Atomic character vector,
#' indicating which label_colour to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_colour_var(p, { label_colour = "black" })
label_colour_var <- function(data, label_colour = "black", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_colour" %in% names(data[[el]])){
        data[[el]]$label_colour <- "black"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_colour" %in% names(data)){
        data$label_colour <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(lhs == rhs & op == "~~")
  cl[["expr"]] <- str2lang(paste0("label_colour = ", deparse(label_colour)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_fill Atomic character vector,
#' indicating which label_fill to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_fill_var(p, { label_fill = "white" })
label_fill_var <- function(data, label_fill = "white", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_fill" %in% names(data[[el]])){
        data[[el]]$label_fill <- "white"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_fill" %in% names(data)){
        data$label_fill <- "white"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(lhs == rhs & op == "~~")
  cl[["expr"]] <- str2lang(paste0("label_fill = ", deparse(label_fill)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_size Atomic character vector,
#' indicating which label_size to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_size_var(p, { label_size = 4 })
label_size_var <- function(data, label_size = 4, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_size" %in% names(data[[el]])){
        data[[el]]$label_size <- 4
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_size" %in% names(data)){
        data$label_size <- 4
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(lhs == rhs & op == "~~")
  cl[["expr"]] <- str2lang(paste0("label_size = ", deparse(label_size)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_alpha Atomic character vector,
#' indicating which label_alpha to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_alpha_var(p, { label_alpha = 1 })
label_alpha_var <- function(data, label_alpha = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_alpha" %in% names(data[[el]])){
        data[[el]]$label_alpha <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_alpha" %in% names(data)){
        data$label_alpha <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(lhs == rhs & op == "~~")
  cl[["expr"]] <- str2lang(paste0("label_alpha = ", deparse(label_alpha)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_family Atomic character vector,
#' indicating which label_family to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_family_var(p, { label_family = "sans" })
label_family_var <- function(data, label_family = "sans", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_family" %in% names(data[[el]])){
        data[[el]]$label_family <- "sans"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_family" %in% names(data)){
        data$label_family <- "sans"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(lhs == rhs & op == "~~")
  cl[["expr"]] <- str2lang(paste0("label_family = ", deparse(label_family)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_fontface Atomic character vector,
#' indicating which label_fontface to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_fontface_var(p, { label_fontface = "plain" })
label_fontface_var <- function(data, label_fontface = "plain", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_fontface" %in% names(data[[el]])){
        data[[el]]$label_fontface <- "plain"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_fontface" %in% names(data)){
        data$label_fontface <- "plain"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(lhs == rhs & op == "~~")
  cl[["expr"]] <- str2lang(paste0("label_fontface = ", deparse(label_fontface)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_hjust Atomic character vector,
#' indicating which label_hjust to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_hjust_var(p, { label_hjust = "center" })
label_hjust_var <- function(data, label_hjust = "center", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_hjust" %in% names(data[[el]])){
        data[[el]]$label_hjust <- "center"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_hjust" %in% names(data)){
        data$label_hjust <- "center"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(lhs == rhs & op == "~~")
  cl[["expr"]] <- str2lang(paste0("label_hjust = ", deparse(label_hjust)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_vjust Atomic character vector,
#' indicating which label_vjust to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_vjust_var(p, { label_vjust = "middle" })
label_vjust_var <- function(data, label_vjust = "middle", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_vjust" %in% names(data[[el]])){
        data[[el]]$label_vjust <- "middle"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_vjust" %in% names(data)){
        data$label_vjust <- "middle"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(lhs == rhs & op == "~~")
  cl[["expr"]] <- str2lang(paste0("label_vjust = ", deparse(label_vjust)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_lineheight Atomic character vector,
#' indicating which label_lineheight to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_lineheight_var(p, { label_lineheight = 1 })
label_lineheight_var <- function(data, label_lineheight = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_lineheight" %in% names(data[[el]])){
        data[[el]]$label_lineheight <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_lineheight" %in% names(data)){
        data$label_lineheight <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(lhs == rhs & op == "~~")
  cl[["expr"]] <- str2lang(paste0("label_lineheight = ", deparse(label_lineheight)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @rdname if_edit
#' @examples
#' out <- all_cov(p, expr = {label = "sig"})
all_cov <- function(data, expr, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(lhs != rhs & op == "~~")
  cl[["element"]] = "edges"
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @rdname if_edit
#' @examples
#' out <- hide_cov(p)
hide_cov <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(lhs != rhs & op == "~~")
  cl[["expr"]] <- quote({ show = FALSE })
  cl[["element"]] = "edges"
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @rdname if_edit
#' @examples
#' out <- show_cov(p)
show_cov <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(lhs != rhs & op == "~~")
  cl[["expr"]] <- quote({ show = TRUE })
  cl[["element"]] = "edges"
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param colour Atomic character vector,
#' indicating which colour to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- colour_cov(p, { colour = "black" })
colour_cov <- function(data, colour = "black", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"colour" %in% names(data[[el]])){
        data[[el]]$colour <- "black"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"colour" %in% names(data)){
        data$colour <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(lhs != rhs & op == "~~")
  cl[["expr"]] <- str2lang(paste0("colour = ", deparse(colour)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param linewidth Atomic character vector,
#' indicating which linewidth to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- linewidth_cov(p, { linewidth = 1 })
linewidth_cov <- function(data, linewidth = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"linewidth" %in% names(data[[el]])){
        data[[el]]$linewidth <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"linewidth" %in% names(data)){
        data$linewidth <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(lhs != rhs & op == "~~")
  cl[["expr"]] <- str2lang(paste0("linewidth = ", deparse(linewidth)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param linetype Atomic character vector,
#' indicating which linetype to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- linetype_cov(p, { linetype = 1 })
linetype_cov <- function(data, linetype = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"linetype" %in% names(data[[el]])){
        data[[el]]$linetype <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"linetype" %in% names(data)){
        data$linetype <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(lhs != rhs & op == "~~")
  cl[["expr"]] <- str2lang(paste0("linetype = ", deparse(linetype)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param alpha Atomic character vector,
#' indicating which alpha to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- alpha_cov(p, { alpha = 1 })
alpha_cov <- function(data, alpha = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"alpha" %in% names(data[[el]])){
        data[[el]]$alpha <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"alpha" %in% names(data)){
        data$alpha <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(lhs != rhs & op == "~~")
  cl[["expr"]] <- str2lang(paste0("alpha = ", deparse(alpha)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param fill Atomic character vector,
#' indicating which fill to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- fill_cov(p, { fill = "white" })
fill_cov <- function(data, fill = "white", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"fill" %in% names(data[[el]])){
        data[[el]]$fill <- "white"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"fill" %in% names(data)){
        data$fill <- "white"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(lhs != rhs & op == "~~")
  cl[["expr"]] <- str2lang(paste0("fill = ", deparse(fill)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_colour Atomic character vector,
#' indicating which label_colour to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_colour_cov(p, { label_colour = "black" })
label_colour_cov <- function(data, label_colour = "black", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_colour" %in% names(data[[el]])){
        data[[el]]$label_colour <- "black"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_colour" %in% names(data)){
        data$label_colour <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(lhs != rhs & op == "~~")
  cl[["expr"]] <- str2lang(paste0("label_colour = ", deparse(label_colour)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_fill Atomic character vector,
#' indicating which label_fill to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_fill_cov(p, { label_fill = "white" })
label_fill_cov <- function(data, label_fill = "white", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_fill" %in% names(data[[el]])){
        data[[el]]$label_fill <- "white"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_fill" %in% names(data)){
        data$label_fill <- "white"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(lhs != rhs & op == "~~")
  cl[["expr"]] <- str2lang(paste0("label_fill = ", deparse(label_fill)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_size Atomic character vector,
#' indicating which label_size to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_size_cov(p, { label_size = 4 })
label_size_cov <- function(data, label_size = 4, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_size" %in% names(data[[el]])){
        data[[el]]$label_size <- 4
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_size" %in% names(data)){
        data$label_size <- 4
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(lhs != rhs & op == "~~")
  cl[["expr"]] <- str2lang(paste0("label_size = ", deparse(label_size)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_alpha Atomic character vector,
#' indicating which label_alpha to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_alpha_cov(p, { label_alpha = 1 })
label_alpha_cov <- function(data, label_alpha = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_alpha" %in% names(data[[el]])){
        data[[el]]$label_alpha <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_alpha" %in% names(data)){
        data$label_alpha <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(lhs != rhs & op == "~~")
  cl[["expr"]] <- str2lang(paste0("label_alpha = ", deparse(label_alpha)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_family Atomic character vector,
#' indicating which label_family to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_family_cov(p, { label_family = "sans" })
label_family_cov <- function(data, label_family = "sans", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_family" %in% names(data[[el]])){
        data[[el]]$label_family <- "sans"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_family" %in% names(data)){
        data$label_family <- "sans"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(lhs != rhs & op == "~~")
  cl[["expr"]] <- str2lang(paste0("label_family = ", deparse(label_family)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_fontface Atomic character vector,
#' indicating which label_fontface to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_fontface_cov(p, { label_fontface = "plain" })
label_fontface_cov <- function(data, label_fontface = "plain", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_fontface" %in% names(data[[el]])){
        data[[el]]$label_fontface <- "plain"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_fontface" %in% names(data)){
        data$label_fontface <- "plain"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(lhs != rhs & op == "~~")
  cl[["expr"]] <- str2lang(paste0("label_fontface = ", deparse(label_fontface)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_hjust Atomic character vector,
#' indicating which label_hjust to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_hjust_cov(p, { label_hjust = "center" })
label_hjust_cov <- function(data, label_hjust = "center", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_hjust" %in% names(data[[el]])){
        data[[el]]$label_hjust <- "center"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_hjust" %in% names(data)){
        data$label_hjust <- "center"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(lhs != rhs & op == "~~")
  cl[["expr"]] <- str2lang(paste0("label_hjust = ", deparse(label_hjust)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_vjust Atomic character vector,
#' indicating which label_vjust to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_vjust_cov(p, { label_vjust = "middle" })
label_vjust_cov <- function(data, label_vjust = "middle", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_vjust" %in% names(data[[el]])){
        data[[el]]$label_vjust <- "middle"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_vjust" %in% names(data)){
        data$label_vjust <- "middle"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(lhs != rhs & op == "~~")
  cl[["expr"]] <- str2lang(paste0("label_vjust = ", deparse(label_vjust)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_lineheight Atomic character vector,
#' indicating which label_lineheight to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_lineheight_cov(p, { label_lineheight = 1 })
label_lineheight_cov <- function(data, label_lineheight = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_lineheight" %in% names(data[[el]])){
        data[[el]]$label_lineheight <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_lineheight" %in% names(data)){
        data$label_lineheight <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(lhs != rhs & op == "~~")
  cl[["expr"]] <- str2lang(paste0("label_lineheight = ", deparse(label_lineheight)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_location Atomic character vector,
#' indicating which label_location to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_location_cov(p, { label_location = .5 })
label_location_cov <- function(data, label_location = .5, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_location" %in% names(data[[el]])){
        data[[el]]$label_location <- .5
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_location" %in% names(data)){
        data$label_location <- .5
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(lhs != rhs & op == "~~")
  cl[["expr"]] <- str2lang(paste0("label_location = ", deparse(label_location)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @rdname if_edit
#' @examples
#' out <- all_reg(p, expr = {label = "sig"})
all_reg <- function(data, expr, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(op == "~")
  cl[["element"]] = "edges"
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @rdname if_edit
#' @examples
#' out <- hide_reg(p)
hide_reg <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(op == "~")
  cl[["expr"]] <- quote({ show = FALSE })
  cl[["element"]] = "edges"
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @rdname if_edit
#' @examples
#' out <- show_reg(p)
show_reg <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(op == "~")
  cl[["expr"]] <- quote({ show = TRUE })
  cl[["element"]] = "edges"
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param colour Atomic character vector,
#' indicating which colour to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- colour_reg(p, { colour = "black" })
colour_reg <- function(data, colour = "black", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"colour" %in% names(data[[el]])){
        data[[el]]$colour <- "black"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"colour" %in% names(data)){
        data$colour <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "~")
  cl[["expr"]] <- str2lang(paste0("colour = ", deparse(colour)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param linewidth Atomic character vector,
#' indicating which linewidth to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- linewidth_reg(p, { linewidth = 1 })
linewidth_reg <- function(data, linewidth = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"linewidth" %in% names(data[[el]])){
        data[[el]]$linewidth <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"linewidth" %in% names(data)){
        data$linewidth <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "~")
  cl[["expr"]] <- str2lang(paste0("linewidth = ", deparse(linewidth)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param linetype Atomic character vector,
#' indicating which linetype to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- linetype_reg(p, { linetype = 1 })
linetype_reg <- function(data, linetype = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"linetype" %in% names(data[[el]])){
        data[[el]]$linetype <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"linetype" %in% names(data)){
        data$linetype <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "~")
  cl[["expr"]] <- str2lang(paste0("linetype = ", deparse(linetype)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param alpha Atomic character vector,
#' indicating which alpha to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- alpha_reg(p, { alpha = 1 })
alpha_reg <- function(data, alpha = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"alpha" %in% names(data[[el]])){
        data[[el]]$alpha <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"alpha" %in% names(data)){
        data$alpha <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "~")
  cl[["expr"]] <- str2lang(paste0("alpha = ", deparse(alpha)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param fill Atomic character vector,
#' indicating which fill to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- fill_reg(p, { fill = "white" })
fill_reg <- function(data, fill = "white", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"fill" %in% names(data[[el]])){
        data[[el]]$fill <- "white"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"fill" %in% names(data)){
        data$fill <- "white"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "~")
  cl[["expr"]] <- str2lang(paste0("fill = ", deparse(fill)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_colour Atomic character vector,
#' indicating which label_colour to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_colour_reg(p, { label_colour = "black" })
label_colour_reg <- function(data, label_colour = "black", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_colour" %in% names(data[[el]])){
        data[[el]]$label_colour <- "black"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_colour" %in% names(data)){
        data$label_colour <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "~")
  cl[["expr"]] <- str2lang(paste0("label_colour = ", deparse(label_colour)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_fill Atomic character vector,
#' indicating which label_fill to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_fill_reg(p, { label_fill = "white" })
label_fill_reg <- function(data, label_fill = "white", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_fill" %in% names(data[[el]])){
        data[[el]]$label_fill <- "white"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_fill" %in% names(data)){
        data$label_fill <- "white"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "~")
  cl[["expr"]] <- str2lang(paste0("label_fill = ", deparse(label_fill)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_size Atomic character vector,
#' indicating which label_size to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_size_reg(p, { label_size = 4 })
label_size_reg <- function(data, label_size = 4, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_size" %in% names(data[[el]])){
        data[[el]]$label_size <- 4
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_size" %in% names(data)){
        data$label_size <- 4
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "~")
  cl[["expr"]] <- str2lang(paste0("label_size = ", deparse(label_size)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_alpha Atomic character vector,
#' indicating which label_alpha to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_alpha_reg(p, { label_alpha = 1 })
label_alpha_reg <- function(data, label_alpha = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_alpha" %in% names(data[[el]])){
        data[[el]]$label_alpha <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_alpha" %in% names(data)){
        data$label_alpha <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "~")
  cl[["expr"]] <- str2lang(paste0("label_alpha = ", deparse(label_alpha)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_family Atomic character vector,
#' indicating which label_family to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_family_reg(p, { label_family = "sans" })
label_family_reg <- function(data, label_family = "sans", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_family" %in% names(data[[el]])){
        data[[el]]$label_family <- "sans"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_family" %in% names(data)){
        data$label_family <- "sans"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "~")
  cl[["expr"]] <- str2lang(paste0("label_family = ", deparse(label_family)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_fontface Atomic character vector,
#' indicating which label_fontface to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_fontface_reg(p, { label_fontface = "plain" })
label_fontface_reg <- function(data, label_fontface = "plain", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_fontface" %in% names(data[[el]])){
        data[[el]]$label_fontface <- "plain"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_fontface" %in% names(data)){
        data$label_fontface <- "plain"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "~")
  cl[["expr"]] <- str2lang(paste0("label_fontface = ", deparse(label_fontface)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_hjust Atomic character vector,
#' indicating which label_hjust to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_hjust_reg(p, { label_hjust = "center" })
label_hjust_reg <- function(data, label_hjust = "center", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_hjust" %in% names(data[[el]])){
        data[[el]]$label_hjust <- "center"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_hjust" %in% names(data)){
        data$label_hjust <- "center"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "~")
  cl[["expr"]] <- str2lang(paste0("label_hjust = ", deparse(label_hjust)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_vjust Atomic character vector,
#' indicating which label_vjust to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_vjust_reg(p, { label_vjust = "middle" })
label_vjust_reg <- function(data, label_vjust = "middle", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_vjust" %in% names(data[[el]])){
        data[[el]]$label_vjust <- "middle"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_vjust" %in% names(data)){
        data$label_vjust <- "middle"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "~")
  cl[["expr"]] <- str2lang(paste0("label_vjust = ", deparse(label_vjust)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_lineheight Atomic character vector,
#' indicating which label_lineheight to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_lineheight_reg(p, { label_lineheight = 1 })
label_lineheight_reg <- function(data, label_lineheight = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_lineheight" %in% names(data[[el]])){
        data[[el]]$label_lineheight <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_lineheight" %in% names(data)){
        data$label_lineheight <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "~")
  cl[["expr"]] <- str2lang(paste0("label_lineheight = ", deparse(label_lineheight)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_location Atomic character vector,
#' indicating which label_location to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_location_reg(p, { label_location = .5 })
label_location_reg <- function(data, label_location = .5, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_location" %in% names(data[[el]])){
        data[[el]]$label_location <- .5
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_location" %in% names(data)){
        data$label_location <- .5
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "~")
  cl[["expr"]] <- str2lang(paste0("label_location = ", deparse(label_location)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @rdname if_edit
#' @examples
#' out <- all_load(p, expr = {label = "sig"})
all_load <- function(data, expr, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(op == "=~")
  cl[["element"]] = "edges"
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @rdname if_edit
#' @examples
#' out <- hide_load(p)
hide_load <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(op == "=~")
  cl[["expr"]] <- quote({ show = FALSE })
  cl[["element"]] = "edges"
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @rdname if_edit
#' @examples
#' out <- show_load(p)
show_load <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(op == "=~")
  cl[["expr"]] <- quote({ show = TRUE })
  cl[["element"]] = "edges"
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param colour Atomic character vector,
#' indicating which colour to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- colour_load(p, { colour = "black" })
colour_load <- function(data, colour = "black", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"colour" %in% names(data[[el]])){
        data[[el]]$colour <- "black"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"colour" %in% names(data)){
        data$colour <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "=~")
  cl[["expr"]] <- str2lang(paste0("colour = ", deparse(colour)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param linewidth Atomic character vector,
#' indicating which linewidth to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- linewidth_load(p, { linewidth = 1 })
linewidth_load <- function(data, linewidth = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"linewidth" %in% names(data[[el]])){
        data[[el]]$linewidth <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"linewidth" %in% names(data)){
        data$linewidth <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "=~")
  cl[["expr"]] <- str2lang(paste0("linewidth = ", deparse(linewidth)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param linetype Atomic character vector,
#' indicating which linetype to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- linetype_load(p, { linetype = 1 })
linetype_load <- function(data, linetype = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"linetype" %in% names(data[[el]])){
        data[[el]]$linetype <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"linetype" %in% names(data)){
        data$linetype <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "=~")
  cl[["expr"]] <- str2lang(paste0("linetype = ", deparse(linetype)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param alpha Atomic character vector,
#' indicating which alpha to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- alpha_load(p, { alpha = 1 })
alpha_load <- function(data, alpha = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"alpha" %in% names(data[[el]])){
        data[[el]]$alpha <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"alpha" %in% names(data)){
        data$alpha <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "=~")
  cl[["expr"]] <- str2lang(paste0("alpha = ", deparse(alpha)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param fill Atomic character vector,
#' indicating which fill to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- fill_load(p, { fill = "white" })
fill_load <- function(data, fill = "white", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"fill" %in% names(data[[el]])){
        data[[el]]$fill <- "white"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"fill" %in% names(data)){
        data$fill <- "white"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "=~")
  cl[["expr"]] <- str2lang(paste0("fill = ", deparse(fill)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_colour Atomic character vector,
#' indicating which label_colour to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_colour_load(p, { label_colour = "black" })
label_colour_load <- function(data, label_colour = "black", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_colour" %in% names(data[[el]])){
        data[[el]]$label_colour <- "black"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_colour" %in% names(data)){
        data$label_colour <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "=~")
  cl[["expr"]] <- str2lang(paste0("label_colour = ", deparse(label_colour)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_fill Atomic character vector,
#' indicating which label_fill to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_fill_load(p, { label_fill = "white" })
label_fill_load <- function(data, label_fill = "white", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_fill" %in% names(data[[el]])){
        data[[el]]$label_fill <- "white"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_fill" %in% names(data)){
        data$label_fill <- "white"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "=~")
  cl[["expr"]] <- str2lang(paste0("label_fill = ", deparse(label_fill)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_size Atomic character vector,
#' indicating which label_size to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_size_load(p, { label_size = 4 })
label_size_load <- function(data, label_size = 4, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_size" %in% names(data[[el]])){
        data[[el]]$label_size <- 4
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_size" %in% names(data)){
        data$label_size <- 4
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "=~")
  cl[["expr"]] <- str2lang(paste0("label_size = ", deparse(label_size)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_alpha Atomic character vector,
#' indicating which label_alpha to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_alpha_load(p, { label_alpha = 1 })
label_alpha_load <- function(data, label_alpha = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_alpha" %in% names(data[[el]])){
        data[[el]]$label_alpha <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_alpha" %in% names(data)){
        data$label_alpha <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "=~")
  cl[["expr"]] <- str2lang(paste0("label_alpha = ", deparse(label_alpha)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_family Atomic character vector,
#' indicating which label_family to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_family_load(p, { label_family = "sans" })
label_family_load <- function(data, label_family = "sans", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_family" %in% names(data[[el]])){
        data[[el]]$label_family <- "sans"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_family" %in% names(data)){
        data$label_family <- "sans"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "=~")
  cl[["expr"]] <- str2lang(paste0("label_family = ", deparse(label_family)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_fontface Atomic character vector,
#' indicating which label_fontface to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_fontface_load(p, { label_fontface = "plain" })
label_fontface_load <- function(data, label_fontface = "plain", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_fontface" %in% names(data[[el]])){
        data[[el]]$label_fontface <- "plain"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_fontface" %in% names(data)){
        data$label_fontface <- "plain"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "=~")
  cl[["expr"]] <- str2lang(paste0("label_fontface = ", deparse(label_fontface)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_hjust Atomic character vector,
#' indicating which label_hjust to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_hjust_load(p, { label_hjust = "center" })
label_hjust_load <- function(data, label_hjust = "center", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_hjust" %in% names(data[[el]])){
        data[[el]]$label_hjust <- "center"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_hjust" %in% names(data)){
        data$label_hjust <- "center"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "=~")
  cl[["expr"]] <- str2lang(paste0("label_hjust = ", deparse(label_hjust)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_vjust Atomic character vector,
#' indicating which label_vjust to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_vjust_load(p, { label_vjust = "middle" })
label_vjust_load <- function(data, label_vjust = "middle", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_vjust" %in% names(data[[el]])){
        data[[el]]$label_vjust <- "middle"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_vjust" %in% names(data)){
        data$label_vjust <- "middle"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "=~")
  cl[["expr"]] <- str2lang(paste0("label_vjust = ", deparse(label_vjust)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_lineheight Atomic character vector,
#' indicating which label_lineheight to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_lineheight_load(p, { label_lineheight = 1 })
label_lineheight_load <- function(data, label_lineheight = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_lineheight" %in% names(data[[el]])){
        data[[el]]$label_lineheight <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_lineheight" %in% names(data)){
        data$label_lineheight <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "=~")
  cl[["expr"]] <- str2lang(paste0("label_lineheight = ", deparse(label_lineheight)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_location Atomic character vector,
#' indicating which label_location to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_location_load(p, { label_location = .5 })
label_location_load <- function(data, label_location = .5, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_location" %in% names(data[[el]])){
        data[[el]]$label_location <- .5
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_location" %in% names(data)){
        data$label_location <- .5
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "=~")
  cl[["expr"]] <- str2lang(paste0("label_location = ", deparse(label_location)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @rdname if_edit
#' @examples
#' out <- all_obs(p, expr = {label = "sig"})
all_obs <- function(data, expr, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(shape == "rect")
  cl[["element"]] = "nodes"
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @rdname if_edit
#' @examples
#' out <- hide_obs(p)
hide_obs <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(shape == "rect")
  cl[["expr"]] <- quote({ show = FALSE })
  cl[["element"]] = "nodes"
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @rdname if_edit
#' @examples
#' out <- show_obs(p)
show_obs <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(shape == "rect")
  cl[["expr"]] <- quote({ show = TRUE })
  cl[["element"]] = "nodes"
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param colour Atomic character vector,
#' indicating which colour to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- colour_obs(p, { colour = "black" })
colour_obs <- function(data, colour = "black", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"colour" %in% names(data[[el]])){
        data[[el]]$colour <- "black"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"colour" %in% names(data)){
        data$colour <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(shape == "rect")
  cl[["expr"]] <- str2lang(paste0("colour = ", deparse(colour)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param linewidth Atomic character vector,
#' indicating which linewidth to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- linewidth_obs(p, { linewidth = 1 })
linewidth_obs <- function(data, linewidth = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"linewidth" %in% names(data[[el]])){
        data[[el]]$linewidth <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"linewidth" %in% names(data)){
        data$linewidth <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(shape == "rect")
  cl[["expr"]] <- str2lang(paste0("linewidth = ", deparse(linewidth)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param linetype Atomic character vector,
#' indicating which linetype to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- linetype_obs(p, { linetype = 1 })
linetype_obs <- function(data, linetype = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"linetype" %in% names(data[[el]])){
        data[[el]]$linetype <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"linetype" %in% names(data)){
        data$linetype <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(shape == "rect")
  cl[["expr"]] <- str2lang(paste0("linetype = ", deparse(linetype)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param alpha Atomic character vector,
#' indicating which alpha to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- alpha_obs(p, { alpha = 1 })
alpha_obs <- function(data, alpha = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"alpha" %in% names(data[[el]])){
        data[[el]]$alpha <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"alpha" %in% names(data)){
        data$alpha <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(shape == "rect")
  cl[["expr"]] <- str2lang(paste0("alpha = ", deparse(alpha)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param fill Atomic character vector,
#' indicating which fill to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- fill_obs(p, { fill = "white" })
fill_obs <- function(data, fill = "white", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"fill" %in% names(data[[el]])){
        data[[el]]$fill <- "white"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"fill" %in% names(data)){
        data$fill <- "white"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(shape == "rect")
  cl[["expr"]] <- str2lang(paste0("fill = ", deparse(fill)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_colour Atomic character vector,
#' indicating which label_colour to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_colour_obs(p, { label_colour = "black" })
label_colour_obs <- function(data, label_colour = "black", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"label_colour" %in% names(data[[el]])){
        data[[el]]$label_colour <- "black"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_colour" %in% names(data)){
        data$label_colour <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(shape == "rect")
  cl[["expr"]] <- str2lang(paste0("label_colour = ", deparse(label_colour)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_fill Atomic character vector,
#' indicating which label_fill to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_fill_obs(p, { label_fill = "white" })
label_fill_obs <- function(data, label_fill = "white", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"label_fill" %in% names(data[[el]])){
        data[[el]]$label_fill <- "white"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_fill" %in% names(data)){
        data$label_fill <- "white"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(shape == "rect")
  cl[["expr"]] <- str2lang(paste0("label_fill = ", deparse(label_fill)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_size Atomic character vector,
#' indicating which label_size to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_size_obs(p, { label_size = 4 })
label_size_obs <- function(data, label_size = 4, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"label_size" %in% names(data[[el]])){
        data[[el]]$label_size <- 4
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_size" %in% names(data)){
        data$label_size <- 4
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(shape == "rect")
  cl[["expr"]] <- str2lang(paste0("label_size = ", deparse(label_size)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_alpha Atomic character vector,
#' indicating which label_alpha to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_alpha_obs(p, { label_alpha = 1 })
label_alpha_obs <- function(data, label_alpha = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"label_alpha" %in% names(data[[el]])){
        data[[el]]$label_alpha <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_alpha" %in% names(data)){
        data$label_alpha <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(shape == "rect")
  cl[["expr"]] <- str2lang(paste0("label_alpha = ", deparse(label_alpha)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_family Atomic character vector,
#' indicating which label_family to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_family_obs(p, { label_family = "sans" })
label_family_obs <- function(data, label_family = "sans", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"label_family" %in% names(data[[el]])){
        data[[el]]$label_family <- "sans"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_family" %in% names(data)){
        data$label_family <- "sans"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(shape == "rect")
  cl[["expr"]] <- str2lang(paste0("label_family = ", deparse(label_family)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_fontface Atomic character vector,
#' indicating which label_fontface to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_fontface_obs(p, { label_fontface = "plain" })
label_fontface_obs <- function(data, label_fontface = "plain", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"label_fontface" %in% names(data[[el]])){
        data[[el]]$label_fontface <- "plain"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_fontface" %in% names(data)){
        data$label_fontface <- "plain"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(shape == "rect")
  cl[["expr"]] <- str2lang(paste0("label_fontface = ", deparse(label_fontface)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_hjust Atomic character vector,
#' indicating which label_hjust to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_hjust_obs(p, { label_hjust = "center" })
label_hjust_obs <- function(data, label_hjust = "center", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"label_hjust" %in% names(data[[el]])){
        data[[el]]$label_hjust <- "center"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_hjust" %in% names(data)){
        data$label_hjust <- "center"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(shape == "rect")
  cl[["expr"]] <- str2lang(paste0("label_hjust = ", deparse(label_hjust)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_vjust Atomic character vector,
#' indicating which label_vjust to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_vjust_obs(p, { label_vjust = "middle" })
label_vjust_obs <- function(data, label_vjust = "middle", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"label_vjust" %in% names(data[[el]])){
        data[[el]]$label_vjust <- "middle"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_vjust" %in% names(data)){
        data$label_vjust <- "middle"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(shape == "rect")
  cl[["expr"]] <- str2lang(paste0("label_vjust = ", deparse(label_vjust)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_lineheight Atomic character vector,
#' indicating which label_lineheight to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_lineheight_obs(p, { label_lineheight = 1 })
label_lineheight_obs <- function(data, label_lineheight = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"label_lineheight" %in% names(data[[el]])){
        data[[el]]$label_lineheight <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_lineheight" %in% names(data)){
        data$label_lineheight <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(shape == "rect")
  cl[["expr"]] <- str2lang(paste0("label_lineheight = ", deparse(label_lineheight)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @rdname if_edit
#' @examples
#' out <- all_latent(p, expr = {label = "sig"})
all_latent <- function(data, expr, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(shape == "oval")
  cl[["element"]] = "nodes"
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @rdname if_edit
#' @examples
#' out <- hide_latent(p)
hide_latent <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(shape == "oval")
  cl[["expr"]] <- quote({ show = FALSE })
  cl[["element"]] = "nodes"
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @rdname if_edit
#' @examples
#' out <- show_latent(p)
show_latent <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(shape == "oval")
  cl[["expr"]] <- quote({ show = TRUE })
  cl[["element"]] = "nodes"
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param colour Atomic character vector,
#' indicating which colour to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- colour_latent(p, { colour = "black" })
colour_latent <- function(data, colour = "black", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"colour" %in% names(data[[el]])){
        data[[el]]$colour <- "black"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"colour" %in% names(data)){
        data$colour <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(shape == "oval")
  cl[["expr"]] <- str2lang(paste0("colour = ", deparse(colour)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param linewidth Atomic character vector,
#' indicating which linewidth to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- linewidth_latent(p, { linewidth = 1 })
linewidth_latent <- function(data, linewidth = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"linewidth" %in% names(data[[el]])){
        data[[el]]$linewidth <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"linewidth" %in% names(data)){
        data$linewidth <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(shape == "oval")
  cl[["expr"]] <- str2lang(paste0("linewidth = ", deparse(linewidth)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param linetype Atomic character vector,
#' indicating which linetype to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- linetype_latent(p, { linetype = 1 })
linetype_latent <- function(data, linetype = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"linetype" %in% names(data[[el]])){
        data[[el]]$linetype <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"linetype" %in% names(data)){
        data$linetype <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(shape == "oval")
  cl[["expr"]] <- str2lang(paste0("linetype = ", deparse(linetype)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param alpha Atomic character vector,
#' indicating which alpha to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- alpha_latent(p, { alpha = 1 })
alpha_latent <- function(data, alpha = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"alpha" %in% names(data[[el]])){
        data[[el]]$alpha <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"alpha" %in% names(data)){
        data$alpha <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(shape == "oval")
  cl[["expr"]] <- str2lang(paste0("alpha = ", deparse(alpha)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param fill Atomic character vector,
#' indicating which fill to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- fill_latent(p, { fill = "white" })
fill_latent <- function(data, fill = "white", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"fill" %in% names(data[[el]])){
        data[[el]]$fill <- "white"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"fill" %in% names(data)){
        data$fill <- "white"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(shape == "oval")
  cl[["expr"]] <- str2lang(paste0("fill = ", deparse(fill)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_colour Atomic character vector,
#' indicating which label_colour to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_colour_latent(p, { label_colour = "black" })
label_colour_latent <- function(data, label_colour = "black", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"label_colour" %in% names(data[[el]])){
        data[[el]]$label_colour <- "black"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_colour" %in% names(data)){
        data$label_colour <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(shape == "oval")
  cl[["expr"]] <- str2lang(paste0("label_colour = ", deparse(label_colour)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_fill Atomic character vector,
#' indicating which label_fill to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_fill_latent(p, { label_fill = "white" })
label_fill_latent <- function(data, label_fill = "white", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"label_fill" %in% names(data[[el]])){
        data[[el]]$label_fill <- "white"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_fill" %in% names(data)){
        data$label_fill <- "white"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(shape == "oval")
  cl[["expr"]] <- str2lang(paste0("label_fill = ", deparse(label_fill)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_size Atomic character vector,
#' indicating which label_size to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_size_latent(p, { label_size = 4 })
label_size_latent <- function(data, label_size = 4, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"label_size" %in% names(data[[el]])){
        data[[el]]$label_size <- 4
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_size" %in% names(data)){
        data$label_size <- 4
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(shape == "oval")
  cl[["expr"]] <- str2lang(paste0("label_size = ", deparse(label_size)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_alpha Atomic character vector,
#' indicating which label_alpha to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_alpha_latent(p, { label_alpha = 1 })
label_alpha_latent <- function(data, label_alpha = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"label_alpha" %in% names(data[[el]])){
        data[[el]]$label_alpha <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_alpha" %in% names(data)){
        data$label_alpha <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(shape == "oval")
  cl[["expr"]] <- str2lang(paste0("label_alpha = ", deparse(label_alpha)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_family Atomic character vector,
#' indicating which label_family to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_family_latent(p, { label_family = "sans" })
label_family_latent <- function(data, label_family = "sans", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"label_family" %in% names(data[[el]])){
        data[[el]]$label_family <- "sans"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_family" %in% names(data)){
        data$label_family <- "sans"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(shape == "oval")
  cl[["expr"]] <- str2lang(paste0("label_family = ", deparse(label_family)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_fontface Atomic character vector,
#' indicating which label_fontface to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_fontface_latent(p, { label_fontface = "plain" })
label_fontface_latent <- function(data, label_fontface = "plain", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"label_fontface" %in% names(data[[el]])){
        data[[el]]$label_fontface <- "plain"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_fontface" %in% names(data)){
        data$label_fontface <- "plain"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(shape == "oval")
  cl[["expr"]] <- str2lang(paste0("label_fontface = ", deparse(label_fontface)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_hjust Atomic character vector,
#' indicating which label_hjust to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_hjust_latent(p, { label_hjust = "center" })
label_hjust_latent <- function(data, label_hjust = "center", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"label_hjust" %in% names(data[[el]])){
        data[[el]]$label_hjust <- "center"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_hjust" %in% names(data)){
        data$label_hjust <- "center"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(shape == "oval")
  cl[["expr"]] <- str2lang(paste0("label_hjust = ", deparse(label_hjust)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_vjust Atomic character vector,
#' indicating which label_vjust to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_vjust_latent(p, { label_vjust = "middle" })
label_vjust_latent <- function(data, label_vjust = "middle", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"label_vjust" %in% names(data[[el]])){
        data[[el]]$label_vjust <- "middle"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_vjust" %in% names(data)){
        data$label_vjust <- "middle"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(shape == "oval")
  cl[["expr"]] <- str2lang(paste0("label_vjust = ", deparse(label_vjust)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_lineheight Atomic character vector,
#' indicating which label_lineheight to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_lineheight_latent(p, { label_lineheight = 1 })
label_lineheight_latent <- function(data, label_lineheight = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"label_lineheight" %in% names(data[[el]])){
        data[[el]]$label_lineheight <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_lineheight" %in% names(data)){
        data$label_lineheight <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(shape == "oval")
  cl[["expr"]] <- str2lang(paste0("label_lineheight = ", deparse(label_lineheight)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @rdname if_edit
#' @examples
#' out <- all_sig(p, expr = {label = "sig"})
all_sig_nodes <- function(data, expr, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["element"]] = "nodes"
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @rdname if_edit
#' @examples
#' out <- hide_sig(p)
hide_sig_nodes <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["expr"]] <- quote({ show = FALSE })
  cl[["element"]] = "nodes"
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @rdname if_edit
#' @examples
#' out <- show_sig(p)
show_sig_nodes <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["expr"]] <- quote({ show = TRUE })
  cl[["element"]] = "nodes"
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param colour Atomic character vector,
#' indicating which colour to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- colour_sig(p, { colour = "black" })
colour_sig_nodes <- function(data, colour = "black", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"colour" %in% names(data[[el]])){
        data[[el]]$colour <- "black"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"colour" %in% names(data)){
        data$colour <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["expr"]] <- str2lang(paste0("colour = ", deparse(colour)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param linewidth Atomic character vector,
#' indicating which linewidth to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- linewidth_sig(p, { linewidth = 1 })
linewidth_sig_nodes <- function(data, linewidth = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"linewidth" %in% names(data[[el]])){
        data[[el]]$linewidth <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"linewidth" %in% names(data)){
        data$linewidth <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["expr"]] <- str2lang(paste0("linewidth = ", deparse(linewidth)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param linetype Atomic character vector,
#' indicating which linetype to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- linetype_sig(p, { linetype = 1 })
linetype_sig_nodes <- function(data, linetype = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"linetype" %in% names(data[[el]])){
        data[[el]]$linetype <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"linetype" %in% names(data)){
        data$linetype <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["expr"]] <- str2lang(paste0("linetype = ", deparse(linetype)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param alpha Atomic character vector,
#' indicating which alpha to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- alpha_sig(p, { alpha = 1 })
alpha_sig_nodes <- function(data, alpha = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"alpha" %in% names(data[[el]])){
        data[[el]]$alpha <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"alpha" %in% names(data)){
        data$alpha <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["expr"]] <- str2lang(paste0("alpha = ", deparse(alpha)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param fill Atomic character vector,
#' indicating which fill to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- fill_sig(p, { fill = "white" })
fill_sig_nodes <- function(data, fill = "white", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"fill" %in% names(data[[el]])){
        data[[el]]$fill <- "white"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"fill" %in% names(data)){
        data$fill <- "white"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["expr"]] <- str2lang(paste0("fill = ", deparse(fill)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_colour Atomic character vector,
#' indicating which label_colour to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_colour_sig(p, { label_colour = "black" })
label_colour_sig_nodes <- function(data, label_colour = "black", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"label_colour" %in% names(data[[el]])){
        data[[el]]$label_colour <- "black"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_colour" %in% names(data)){
        data$label_colour <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["expr"]] <- str2lang(paste0("label_colour = ", deparse(label_colour)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_fill Atomic character vector,
#' indicating which label_fill to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_fill_sig(p, { label_fill = "white" })
label_fill_sig_nodes <- function(data, label_fill = "white", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"label_fill" %in% names(data[[el]])){
        data[[el]]$label_fill <- "white"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_fill" %in% names(data)){
        data$label_fill <- "white"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["expr"]] <- str2lang(paste0("label_fill = ", deparse(label_fill)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_size Atomic character vector,
#' indicating which label_size to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_size_sig(p, { label_size = 4 })
label_size_sig_nodes <- function(data, label_size = 4, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"label_size" %in% names(data[[el]])){
        data[[el]]$label_size <- 4
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_size" %in% names(data)){
        data$label_size <- 4
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["expr"]] <- str2lang(paste0("label_size = ", deparse(label_size)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_alpha Atomic character vector,
#' indicating which label_alpha to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_alpha_sig(p, { label_alpha = 1 })
label_alpha_sig_nodes <- function(data, label_alpha = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"label_alpha" %in% names(data[[el]])){
        data[[el]]$label_alpha <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_alpha" %in% names(data)){
        data$label_alpha <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["expr"]] <- str2lang(paste0("label_alpha = ", deparse(label_alpha)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_family Atomic character vector,
#' indicating which label_family to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_family_sig(p, { label_family = "sans" })
label_family_sig_nodes <- function(data, label_family = "sans", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"label_family" %in% names(data[[el]])){
        data[[el]]$label_family <- "sans"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_family" %in% names(data)){
        data$label_family <- "sans"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["expr"]] <- str2lang(paste0("label_family = ", deparse(label_family)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_fontface Atomic character vector,
#' indicating which label_fontface to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_fontface_sig(p, { label_fontface = "plain" })
label_fontface_sig_nodes <- function(data, label_fontface = "plain", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"label_fontface" %in% names(data[[el]])){
        data[[el]]$label_fontface <- "plain"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_fontface" %in% names(data)){
        data$label_fontface <- "plain"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["expr"]] <- str2lang(paste0("label_fontface = ", deparse(label_fontface)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_hjust Atomic character vector,
#' indicating which label_hjust to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_hjust_sig(p, { label_hjust = "center" })
label_hjust_sig_nodes <- function(data, label_hjust = "center", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"label_hjust" %in% names(data[[el]])){
        data[[el]]$label_hjust <- "center"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_hjust" %in% names(data)){
        data$label_hjust <- "center"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["expr"]] <- str2lang(paste0("label_hjust = ", deparse(label_hjust)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_vjust Atomic character vector,
#' indicating which label_vjust to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_vjust_sig(p, { label_vjust = "middle" })
label_vjust_sig_nodes <- function(data, label_vjust = "middle", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"label_vjust" %in% names(data[[el]])){
        data[[el]]$label_vjust <- "middle"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_vjust" %in% names(data)){
        data$label_vjust <- "middle"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["expr"]] <- str2lang(paste0("label_vjust = ", deparse(label_vjust)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_lineheight Atomic character vector,
#' indicating which label_lineheight to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_lineheight_sig(p, { label_lineheight = 1 })
label_lineheight_sig_nodes <- function(data, label_lineheight = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"label_lineheight" %in% names(data[[el]])){
        data[[el]]$label_lineheight <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_lineheight" %in% names(data)){
        data$label_lineheight <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["expr"]] <- str2lang(paste0("label_lineheight = ", deparse(label_lineheight)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @rdname if_edit
#' @examples
#' out <- all_nonsig(p, expr = {label = "sig"})
all_nonsig_nodes <- function(data, expr, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["element"]] = "nodes"
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @rdname if_edit
#' @examples
#' out <- hide_nonsig(p)
hide_nonsig_nodes <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["expr"]] <- quote({ show = FALSE })
  cl[["element"]] = "nodes"
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @rdname if_edit
#' @examples
#' out <- show_nonsig(p)
show_nonsig_nodes <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["expr"]] <- quote({ show = TRUE })
  cl[["element"]] = "nodes"
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param colour Atomic character vector,
#' indicating which colour to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- colour_nonsig(p, { colour = "black" })
colour_nonsig_nodes <- function(data, colour = "black", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"colour" %in% names(data[[el]])){
        data[[el]]$colour <- "black"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"colour" %in% names(data)){
        data$colour <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["expr"]] <- str2lang(paste0("colour = ", deparse(colour)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param linewidth Atomic character vector,
#' indicating which linewidth to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- linewidth_nonsig(p, { linewidth = 1 })
linewidth_nonsig_nodes <- function(data, linewidth = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"linewidth" %in% names(data[[el]])){
        data[[el]]$linewidth <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"linewidth" %in% names(data)){
        data$linewidth <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["expr"]] <- str2lang(paste0("linewidth = ", deparse(linewidth)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param linetype Atomic character vector,
#' indicating which linetype to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- linetype_nonsig(p, { linetype = 1 })
linetype_nonsig_nodes <- function(data, linetype = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"linetype" %in% names(data[[el]])){
        data[[el]]$linetype <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"linetype" %in% names(data)){
        data$linetype <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["expr"]] <- str2lang(paste0("linetype = ", deparse(linetype)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param alpha Atomic character vector,
#' indicating which alpha to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- alpha_nonsig(p, { alpha = 1 })
alpha_nonsig_nodes <- function(data, alpha = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"alpha" %in% names(data[[el]])){
        data[[el]]$alpha <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"alpha" %in% names(data)){
        data$alpha <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["expr"]] <- str2lang(paste0("alpha = ", deparse(alpha)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param fill Atomic character vector,
#' indicating which fill to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- fill_nonsig(p, { fill = "white" })
fill_nonsig_nodes <- function(data, fill = "white", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"fill" %in% names(data[[el]])){
        data[[el]]$fill <- "white"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"fill" %in% names(data)){
        data$fill <- "white"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["expr"]] <- str2lang(paste0("fill = ", deparse(fill)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_colour Atomic character vector,
#' indicating which label_colour to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_colour_nonsig(p, { label_colour = "black" })
label_colour_nonsig_nodes <- function(data, label_colour = "black", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"label_colour" %in% names(data[[el]])){
        data[[el]]$label_colour <- "black"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_colour" %in% names(data)){
        data$label_colour <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["expr"]] <- str2lang(paste0("label_colour = ", deparse(label_colour)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_fill Atomic character vector,
#' indicating which label_fill to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_fill_nonsig(p, { label_fill = "white" })
label_fill_nonsig_nodes <- function(data, label_fill = "white", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"label_fill" %in% names(data[[el]])){
        data[[el]]$label_fill <- "white"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_fill" %in% names(data)){
        data$label_fill <- "white"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["expr"]] <- str2lang(paste0("label_fill = ", deparse(label_fill)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_size Atomic character vector,
#' indicating which label_size to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_size_nonsig(p, { label_size = 4 })
label_size_nonsig_nodes <- function(data, label_size = 4, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"label_size" %in% names(data[[el]])){
        data[[el]]$label_size <- 4
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_size" %in% names(data)){
        data$label_size <- 4
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["expr"]] <- str2lang(paste0("label_size = ", deparse(label_size)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_alpha Atomic character vector,
#' indicating which label_alpha to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_alpha_nonsig(p, { label_alpha = 1 })
label_alpha_nonsig_nodes <- function(data, label_alpha = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"label_alpha" %in% names(data[[el]])){
        data[[el]]$label_alpha <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_alpha" %in% names(data)){
        data$label_alpha <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["expr"]] <- str2lang(paste0("label_alpha = ", deparse(label_alpha)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_family Atomic character vector,
#' indicating which label_family to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_family_nonsig(p, { label_family = "sans" })
label_family_nonsig_nodes <- function(data, label_family = "sans", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"label_family" %in% names(data[[el]])){
        data[[el]]$label_family <- "sans"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_family" %in% names(data)){
        data$label_family <- "sans"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["expr"]] <- str2lang(paste0("label_family = ", deparse(label_family)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_fontface Atomic character vector,
#' indicating which label_fontface to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_fontface_nonsig(p, { label_fontface = "plain" })
label_fontface_nonsig_nodes <- function(data, label_fontface = "plain", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"label_fontface" %in% names(data[[el]])){
        data[[el]]$label_fontface <- "plain"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_fontface" %in% names(data)){
        data$label_fontface <- "plain"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["expr"]] <- str2lang(paste0("label_fontface = ", deparse(label_fontface)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_hjust Atomic character vector,
#' indicating which label_hjust to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_hjust_nonsig(p, { label_hjust = "center" })
label_hjust_nonsig_nodes <- function(data, label_hjust = "center", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"label_hjust" %in% names(data[[el]])){
        data[[el]]$label_hjust <- "center"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_hjust" %in% names(data)){
        data$label_hjust <- "center"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["expr"]] <- str2lang(paste0("label_hjust = ", deparse(label_hjust)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_vjust Atomic character vector,
#' indicating which label_vjust to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_vjust_nonsig(p, { label_vjust = "middle" })
label_vjust_nonsig_nodes <- function(data, label_vjust = "middle", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"label_vjust" %in% names(data[[el]])){
        data[[el]]$label_vjust <- "middle"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_vjust" %in% names(data)){
        data$label_vjust <- "middle"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["expr"]] <- str2lang(paste0("label_vjust = ", deparse(label_vjust)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_lineheight Atomic character vector,
#' indicating which label_lineheight to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_lineheight_nonsig(p, { label_lineheight = 1 })
label_lineheight_nonsig_nodes <- function(data, label_lineheight = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"label_lineheight" %in% names(data[[el]])){
        data[[el]]$label_lineheight <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_lineheight" %in% names(data)){
        data$label_lineheight <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["expr"]] <- str2lang(paste0("label_lineheight = ", deparse(label_lineheight)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @rdname if_edit
#' @examples
#' out <- all_fixed(p, expr = {label = "sig"})
all_fixed_nodes <- function(data, expr, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["element"]] = "nodes"
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @rdname if_edit
#' @examples
#' out <- hide_fixed(p)
hide_fixed_nodes <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["expr"]] <- quote({ show = FALSE })
  cl[["element"]] = "nodes"
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @rdname if_edit
#' @examples
#' out <- show_fixed(p)
show_fixed_nodes <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["expr"]] <- quote({ show = TRUE })
  cl[["element"]] = "nodes"
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param colour Atomic character vector,
#' indicating which colour to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- colour_fixed(p, { colour = "black" })
colour_fixed_nodes <- function(data, colour = "black", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"colour" %in% names(data[[el]])){
        data[[el]]$colour <- "black"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"colour" %in% names(data)){
        data$colour <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["expr"]] <- str2lang(paste0("colour = ", deparse(colour)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param linewidth Atomic character vector,
#' indicating which linewidth to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- linewidth_fixed(p, { linewidth = 1 })
linewidth_fixed_nodes <- function(data, linewidth = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"linewidth" %in% names(data[[el]])){
        data[[el]]$linewidth <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"linewidth" %in% names(data)){
        data$linewidth <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["expr"]] <- str2lang(paste0("linewidth = ", deparse(linewidth)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param linetype Atomic character vector,
#' indicating which linetype to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- linetype_fixed(p, { linetype = 1 })
linetype_fixed_nodes <- function(data, linetype = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"linetype" %in% names(data[[el]])){
        data[[el]]$linetype <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"linetype" %in% names(data)){
        data$linetype <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["expr"]] <- str2lang(paste0("linetype = ", deparse(linetype)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param alpha Atomic character vector,
#' indicating which alpha to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- alpha_fixed(p, { alpha = 1 })
alpha_fixed_nodes <- function(data, alpha = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"alpha" %in% names(data[[el]])){
        data[[el]]$alpha <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"alpha" %in% names(data)){
        data$alpha <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["expr"]] <- str2lang(paste0("alpha = ", deparse(alpha)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param fill Atomic character vector,
#' indicating which fill to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- fill_fixed(p, { fill = "white" })
fill_fixed_nodes <- function(data, fill = "white", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"fill" %in% names(data[[el]])){
        data[[el]]$fill <- "white"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"fill" %in% names(data)){
        data$fill <- "white"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["expr"]] <- str2lang(paste0("fill = ", deparse(fill)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_colour Atomic character vector,
#' indicating which label_colour to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_colour_fixed(p, { label_colour = "black" })
label_colour_fixed_nodes <- function(data, label_colour = "black", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"label_colour" %in% names(data[[el]])){
        data[[el]]$label_colour <- "black"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_colour" %in% names(data)){
        data$label_colour <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["expr"]] <- str2lang(paste0("label_colour = ", deparse(label_colour)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_fill Atomic character vector,
#' indicating which label_fill to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_fill_fixed(p, { label_fill = "white" })
label_fill_fixed_nodes <- function(data, label_fill = "white", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"label_fill" %in% names(data[[el]])){
        data[[el]]$label_fill <- "white"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_fill" %in% names(data)){
        data$label_fill <- "white"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["expr"]] <- str2lang(paste0("label_fill = ", deparse(label_fill)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_size Atomic character vector,
#' indicating which label_size to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_size_fixed(p, { label_size = 4 })
label_size_fixed_nodes <- function(data, label_size = 4, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"label_size" %in% names(data[[el]])){
        data[[el]]$label_size <- 4
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_size" %in% names(data)){
        data$label_size <- 4
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["expr"]] <- str2lang(paste0("label_size = ", deparse(label_size)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_alpha Atomic character vector,
#' indicating which label_alpha to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_alpha_fixed(p, { label_alpha = 1 })
label_alpha_fixed_nodes <- function(data, label_alpha = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"label_alpha" %in% names(data[[el]])){
        data[[el]]$label_alpha <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_alpha" %in% names(data)){
        data$label_alpha <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["expr"]] <- str2lang(paste0("label_alpha = ", deparse(label_alpha)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_family Atomic character vector,
#' indicating which label_family to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_family_fixed(p, { label_family = "sans" })
label_family_fixed_nodes <- function(data, label_family = "sans", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"label_family" %in% names(data[[el]])){
        data[[el]]$label_family <- "sans"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_family" %in% names(data)){
        data$label_family <- "sans"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["expr"]] <- str2lang(paste0("label_family = ", deparse(label_family)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_fontface Atomic character vector,
#' indicating which label_fontface to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_fontface_fixed(p, { label_fontface = "plain" })
label_fontface_fixed_nodes <- function(data, label_fontface = "plain", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"label_fontface" %in% names(data[[el]])){
        data[[el]]$label_fontface <- "plain"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_fontface" %in% names(data)){
        data$label_fontface <- "plain"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["expr"]] <- str2lang(paste0("label_fontface = ", deparse(label_fontface)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_hjust Atomic character vector,
#' indicating which label_hjust to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_hjust_fixed(p, { label_hjust = "center" })
label_hjust_fixed_nodes <- function(data, label_hjust = "center", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"label_hjust" %in% names(data[[el]])){
        data[[el]]$label_hjust <- "center"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_hjust" %in% names(data)){
        data$label_hjust <- "center"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["expr"]] <- str2lang(paste0("label_hjust = ", deparse(label_hjust)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_vjust Atomic character vector,
#' indicating which label_vjust to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_vjust_fixed(p, { label_vjust = "middle" })
label_vjust_fixed_nodes <- function(data, label_vjust = "middle", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"label_vjust" %in% names(data[[el]])){
        data[[el]]$label_vjust <- "middle"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_vjust" %in% names(data)){
        data$label_vjust <- "middle"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["expr"]] <- str2lang(paste0("label_vjust = ", deparse(label_vjust)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_lineheight Atomic character vector,
#' indicating which label_lineheight to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_lineheight_fixed(p, { label_lineheight = 1 })
label_lineheight_fixed_nodes <- function(data, label_lineheight = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"label_lineheight" %in% names(data[[el]])){
        data[[el]]$label_lineheight <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_lineheight" %in% names(data)){
        data$label_lineheight <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["expr"]] <- str2lang(paste0("label_lineheight = ", deparse(label_lineheight)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @rdname if_edit
#' @examples
#' out <- all_pos(p, expr = {label = "sig"})
all_pos_nodes <- function(data, expr, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["element"]] = "nodes"
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @rdname if_edit
#' @examples
#' out <- hide_pos(p)
hide_pos_nodes <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["expr"]] <- quote({ show = FALSE })
  cl[["element"]] = "nodes"
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @rdname if_edit
#' @examples
#' out <- show_pos(p)
show_pos_nodes <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["expr"]] <- quote({ show = TRUE })
  cl[["element"]] = "nodes"
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param colour Atomic character vector,
#' indicating which colour to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- colour_pos(p, { colour = "black" })
colour_pos_nodes <- function(data, colour = "black", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"colour" %in% names(data[[el]])){
        data[[el]]$colour <- "black"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"colour" %in% names(data)){
        data$colour <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["expr"]] <- str2lang(paste0("colour = ", deparse(colour)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param linewidth Atomic character vector,
#' indicating which linewidth to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- linewidth_pos(p, { linewidth = 1 })
linewidth_pos_nodes <- function(data, linewidth = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"linewidth" %in% names(data[[el]])){
        data[[el]]$linewidth <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"linewidth" %in% names(data)){
        data$linewidth <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["expr"]] <- str2lang(paste0("linewidth = ", deparse(linewidth)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param linetype Atomic character vector,
#' indicating which linetype to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- linetype_pos(p, { linetype = 1 })
linetype_pos_nodes <- function(data, linetype = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"linetype" %in% names(data[[el]])){
        data[[el]]$linetype <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"linetype" %in% names(data)){
        data$linetype <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["expr"]] <- str2lang(paste0("linetype = ", deparse(linetype)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param alpha Atomic character vector,
#' indicating which alpha to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- alpha_pos(p, { alpha = 1 })
alpha_pos_nodes <- function(data, alpha = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"alpha" %in% names(data[[el]])){
        data[[el]]$alpha <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"alpha" %in% names(data)){
        data$alpha <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["expr"]] <- str2lang(paste0("alpha = ", deparse(alpha)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param fill Atomic character vector,
#' indicating which fill to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- fill_pos(p, { fill = "white" })
fill_pos_nodes <- function(data, fill = "white", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"fill" %in% names(data[[el]])){
        data[[el]]$fill <- "white"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"fill" %in% names(data)){
        data$fill <- "white"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["expr"]] <- str2lang(paste0("fill = ", deparse(fill)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_colour Atomic character vector,
#' indicating which label_colour to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_colour_pos(p, { label_colour = "black" })
label_colour_pos_nodes <- function(data, label_colour = "black", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"label_colour" %in% names(data[[el]])){
        data[[el]]$label_colour <- "black"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_colour" %in% names(data)){
        data$label_colour <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["expr"]] <- str2lang(paste0("label_colour = ", deparse(label_colour)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_fill Atomic character vector,
#' indicating which label_fill to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_fill_pos(p, { label_fill = "white" })
label_fill_pos_nodes <- function(data, label_fill = "white", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"label_fill" %in% names(data[[el]])){
        data[[el]]$label_fill <- "white"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_fill" %in% names(data)){
        data$label_fill <- "white"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["expr"]] <- str2lang(paste0("label_fill = ", deparse(label_fill)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_size Atomic character vector,
#' indicating which label_size to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_size_pos(p, { label_size = 4 })
label_size_pos_nodes <- function(data, label_size = 4, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"label_size" %in% names(data[[el]])){
        data[[el]]$label_size <- 4
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_size" %in% names(data)){
        data$label_size <- 4
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["expr"]] <- str2lang(paste0("label_size = ", deparse(label_size)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_alpha Atomic character vector,
#' indicating which label_alpha to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_alpha_pos(p, { label_alpha = 1 })
label_alpha_pos_nodes <- function(data, label_alpha = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"label_alpha" %in% names(data[[el]])){
        data[[el]]$label_alpha <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_alpha" %in% names(data)){
        data$label_alpha <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["expr"]] <- str2lang(paste0("label_alpha = ", deparse(label_alpha)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_family Atomic character vector,
#' indicating which label_family to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_family_pos(p, { label_family = "sans" })
label_family_pos_nodes <- function(data, label_family = "sans", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"label_family" %in% names(data[[el]])){
        data[[el]]$label_family <- "sans"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_family" %in% names(data)){
        data$label_family <- "sans"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["expr"]] <- str2lang(paste0("label_family = ", deparse(label_family)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_fontface Atomic character vector,
#' indicating which label_fontface to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_fontface_pos(p, { label_fontface = "plain" })
label_fontface_pos_nodes <- function(data, label_fontface = "plain", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"label_fontface" %in% names(data[[el]])){
        data[[el]]$label_fontface <- "plain"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_fontface" %in% names(data)){
        data$label_fontface <- "plain"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["expr"]] <- str2lang(paste0("label_fontface = ", deparse(label_fontface)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_hjust Atomic character vector,
#' indicating which label_hjust to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_hjust_pos(p, { label_hjust = "center" })
label_hjust_pos_nodes <- function(data, label_hjust = "center", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"label_hjust" %in% names(data[[el]])){
        data[[el]]$label_hjust <- "center"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_hjust" %in% names(data)){
        data$label_hjust <- "center"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["expr"]] <- str2lang(paste0("label_hjust = ", deparse(label_hjust)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_vjust Atomic character vector,
#' indicating which label_vjust to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_vjust_pos(p, { label_vjust = "middle" })
label_vjust_pos_nodes <- function(data, label_vjust = "middle", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"label_vjust" %in% names(data[[el]])){
        data[[el]]$label_vjust <- "middle"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_vjust" %in% names(data)){
        data$label_vjust <- "middle"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["expr"]] <- str2lang(paste0("label_vjust = ", deparse(label_vjust)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_lineheight Atomic character vector,
#' indicating which label_lineheight to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_lineheight_pos(p, { label_lineheight = 1 })
label_lineheight_pos_nodes <- function(data, label_lineheight = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"label_lineheight" %in% names(data[[el]])){
        data[[el]]$label_lineheight <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_lineheight" %in% names(data)){
        data$label_lineheight <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["expr"]] <- str2lang(paste0("label_lineheight = ", deparse(label_lineheight)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @rdname if_edit
#' @examples
#' out <- all_neg(p, expr = {label = "sig"})
all_neg_nodes <- function(data, expr, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["element"]] = "nodes"
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @rdname if_edit
#' @examples
#' out <- hide_neg(p)
hide_neg_nodes <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["expr"]] <- quote({ show = FALSE })
  cl[["element"]] = "nodes"
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @rdname if_edit
#' @examples
#' out <- show_neg(p)
show_neg_nodes <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["expr"]] <- quote({ show = TRUE })
  cl[["element"]] = "nodes"
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param colour Atomic character vector,
#' indicating which colour to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- colour_neg(p, { colour = "black" })
colour_neg_nodes <- function(data, colour = "black", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"colour" %in% names(data[[el]])){
        data[[el]]$colour <- "black"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"colour" %in% names(data)){
        data$colour <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["expr"]] <- str2lang(paste0("colour = ", deparse(colour)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param linewidth Atomic character vector,
#' indicating which linewidth to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- linewidth_neg(p, { linewidth = 1 })
linewidth_neg_nodes <- function(data, linewidth = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"linewidth" %in% names(data[[el]])){
        data[[el]]$linewidth <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"linewidth" %in% names(data)){
        data$linewidth <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["expr"]] <- str2lang(paste0("linewidth = ", deparse(linewidth)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param linetype Atomic character vector,
#' indicating which linetype to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- linetype_neg(p, { linetype = 1 })
linetype_neg_nodes <- function(data, linetype = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"linetype" %in% names(data[[el]])){
        data[[el]]$linetype <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"linetype" %in% names(data)){
        data$linetype <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["expr"]] <- str2lang(paste0("linetype = ", deparse(linetype)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param alpha Atomic character vector,
#' indicating which alpha to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- alpha_neg(p, { alpha = 1 })
alpha_neg_nodes <- function(data, alpha = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"alpha" %in% names(data[[el]])){
        data[[el]]$alpha <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"alpha" %in% names(data)){
        data$alpha <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["expr"]] <- str2lang(paste0("alpha = ", deparse(alpha)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param fill Atomic character vector,
#' indicating which fill to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- fill_neg(p, { fill = "white" })
fill_neg_nodes <- function(data, fill = "white", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"fill" %in% names(data[[el]])){
        data[[el]]$fill <- "white"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"fill" %in% names(data)){
        data$fill <- "white"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["expr"]] <- str2lang(paste0("fill = ", deparse(fill)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_colour Atomic character vector,
#' indicating which label_colour to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_colour_neg(p, { label_colour = "black" })
label_colour_neg_nodes <- function(data, label_colour = "black", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"label_colour" %in% names(data[[el]])){
        data[[el]]$label_colour <- "black"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_colour" %in% names(data)){
        data$label_colour <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["expr"]] <- str2lang(paste0("label_colour = ", deparse(label_colour)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_fill Atomic character vector,
#' indicating which label_fill to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_fill_neg(p, { label_fill = "white" })
label_fill_neg_nodes <- function(data, label_fill = "white", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"label_fill" %in% names(data[[el]])){
        data[[el]]$label_fill <- "white"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_fill" %in% names(data)){
        data$label_fill <- "white"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["expr"]] <- str2lang(paste0("label_fill = ", deparse(label_fill)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_size Atomic character vector,
#' indicating which label_size to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_size_neg(p, { label_size = 4 })
label_size_neg_nodes <- function(data, label_size = 4, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"label_size" %in% names(data[[el]])){
        data[[el]]$label_size <- 4
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_size" %in% names(data)){
        data$label_size <- 4
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["expr"]] <- str2lang(paste0("label_size = ", deparse(label_size)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_alpha Atomic character vector,
#' indicating which label_alpha to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_alpha_neg(p, { label_alpha = 1 })
label_alpha_neg_nodes <- function(data, label_alpha = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"label_alpha" %in% names(data[[el]])){
        data[[el]]$label_alpha <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_alpha" %in% names(data)){
        data$label_alpha <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["expr"]] <- str2lang(paste0("label_alpha = ", deparse(label_alpha)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_family Atomic character vector,
#' indicating which label_family to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_family_neg(p, { label_family = "sans" })
label_family_neg_nodes <- function(data, label_family = "sans", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"label_family" %in% names(data[[el]])){
        data[[el]]$label_family <- "sans"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_family" %in% names(data)){
        data$label_family <- "sans"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["expr"]] <- str2lang(paste0("label_family = ", deparse(label_family)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_fontface Atomic character vector,
#' indicating which label_fontface to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_fontface_neg(p, { label_fontface = "plain" })
label_fontface_neg_nodes <- function(data, label_fontface = "plain", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"label_fontface" %in% names(data[[el]])){
        data[[el]]$label_fontface <- "plain"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_fontface" %in% names(data)){
        data$label_fontface <- "plain"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["expr"]] <- str2lang(paste0("label_fontface = ", deparse(label_fontface)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_hjust Atomic character vector,
#' indicating which label_hjust to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_hjust_neg(p, { label_hjust = "center" })
label_hjust_neg_nodes <- function(data, label_hjust = "center", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"label_hjust" %in% names(data[[el]])){
        data[[el]]$label_hjust <- "center"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_hjust" %in% names(data)){
        data$label_hjust <- "center"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["expr"]] <- str2lang(paste0("label_hjust = ", deparse(label_hjust)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_vjust Atomic character vector,
#' indicating which label_vjust to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_vjust_neg(p, { label_vjust = "middle" })
label_vjust_neg_nodes <- function(data, label_vjust = "middle", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"label_vjust" %in% names(data[[el]])){
        data[[el]]$label_vjust <- "middle"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_vjust" %in% names(data)){
        data$label_vjust <- "middle"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["expr"]] <- str2lang(paste0("label_vjust = ", deparse(label_vjust)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_lineheight Atomic character vector,
#' indicating which label_lineheight to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_lineheight_neg(p, { label_lineheight = 1 })
label_lineheight_neg_nodes <- function(data, label_lineheight = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"label_lineheight" %in% names(data[[el]])){
        data[[el]]$label_lineheight <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_lineheight" %in% names(data)){
        data$label_lineheight <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["expr"]] <- str2lang(paste0("label_lineheight = ", deparse(label_lineheight)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @rdname if_edit
#' @examples
#' out <- all_obs(p, expr = {label = "sig"})
all_obs_nodes <- function(data, expr, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(shape == "rect")
  cl[["element"]] = "nodes"
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @rdname if_edit
#' @examples
#' out <- hide_obs(p)
hide_obs_nodes <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(shape == "rect")
  cl[["expr"]] <- quote({ show = FALSE })
  cl[["element"]] = "nodes"
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @rdname if_edit
#' @examples
#' out <- show_obs(p)
show_obs_nodes <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(shape == "rect")
  cl[["expr"]] <- quote({ show = TRUE })
  cl[["element"]] = "nodes"
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param colour Atomic character vector,
#' indicating which colour to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- colour_obs(p, { colour = "black" })
colour_obs_nodes <- function(data, colour = "black", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"colour" %in% names(data[[el]])){
        data[[el]]$colour <- "black"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"colour" %in% names(data)){
        data$colour <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(shape == "rect")
  cl[["expr"]] <- str2lang(paste0("colour = ", deparse(colour)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param linewidth Atomic character vector,
#' indicating which linewidth to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- linewidth_obs(p, { linewidth = 1 })
linewidth_obs_nodes <- function(data, linewidth = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"linewidth" %in% names(data[[el]])){
        data[[el]]$linewidth <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"linewidth" %in% names(data)){
        data$linewidth <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(shape == "rect")
  cl[["expr"]] <- str2lang(paste0("linewidth = ", deparse(linewidth)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param linetype Atomic character vector,
#' indicating which linetype to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- linetype_obs(p, { linetype = 1 })
linetype_obs_nodes <- function(data, linetype = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"linetype" %in% names(data[[el]])){
        data[[el]]$linetype <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"linetype" %in% names(data)){
        data$linetype <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(shape == "rect")
  cl[["expr"]] <- str2lang(paste0("linetype = ", deparse(linetype)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param alpha Atomic character vector,
#' indicating which alpha to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- alpha_obs(p, { alpha = 1 })
alpha_obs_nodes <- function(data, alpha = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"alpha" %in% names(data[[el]])){
        data[[el]]$alpha <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"alpha" %in% names(data)){
        data$alpha <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(shape == "rect")
  cl[["expr"]] <- str2lang(paste0("alpha = ", deparse(alpha)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param fill Atomic character vector,
#' indicating which fill to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- fill_obs(p, { fill = "white" })
fill_obs_nodes <- function(data, fill = "white", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"fill" %in% names(data[[el]])){
        data[[el]]$fill <- "white"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"fill" %in% names(data)){
        data$fill <- "white"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(shape == "rect")
  cl[["expr"]] <- str2lang(paste0("fill = ", deparse(fill)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_colour Atomic character vector,
#' indicating which label_colour to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_colour_obs(p, { label_colour = "black" })
label_colour_obs_nodes <- function(data, label_colour = "black", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"label_colour" %in% names(data[[el]])){
        data[[el]]$label_colour <- "black"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_colour" %in% names(data)){
        data$label_colour <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(shape == "rect")
  cl[["expr"]] <- str2lang(paste0("label_colour = ", deparse(label_colour)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_fill Atomic character vector,
#' indicating which label_fill to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_fill_obs(p, { label_fill = "white" })
label_fill_obs_nodes <- function(data, label_fill = "white", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"label_fill" %in% names(data[[el]])){
        data[[el]]$label_fill <- "white"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_fill" %in% names(data)){
        data$label_fill <- "white"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(shape == "rect")
  cl[["expr"]] <- str2lang(paste0("label_fill = ", deparse(label_fill)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_size Atomic character vector,
#' indicating which label_size to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_size_obs(p, { label_size = 4 })
label_size_obs_nodes <- function(data, label_size = 4, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"label_size" %in% names(data[[el]])){
        data[[el]]$label_size <- 4
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_size" %in% names(data)){
        data$label_size <- 4
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(shape == "rect")
  cl[["expr"]] <- str2lang(paste0("label_size = ", deparse(label_size)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_alpha Atomic character vector,
#' indicating which label_alpha to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_alpha_obs(p, { label_alpha = 1 })
label_alpha_obs_nodes <- function(data, label_alpha = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"label_alpha" %in% names(data[[el]])){
        data[[el]]$label_alpha <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_alpha" %in% names(data)){
        data$label_alpha <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(shape == "rect")
  cl[["expr"]] <- str2lang(paste0("label_alpha = ", deparse(label_alpha)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_family Atomic character vector,
#' indicating which label_family to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_family_obs(p, { label_family = "sans" })
label_family_obs_nodes <- function(data, label_family = "sans", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"label_family" %in% names(data[[el]])){
        data[[el]]$label_family <- "sans"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_family" %in% names(data)){
        data$label_family <- "sans"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(shape == "rect")
  cl[["expr"]] <- str2lang(paste0("label_family = ", deparse(label_family)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_fontface Atomic character vector,
#' indicating which label_fontface to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_fontface_obs(p, { label_fontface = "plain" })
label_fontface_obs_nodes <- function(data, label_fontface = "plain", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"label_fontface" %in% names(data[[el]])){
        data[[el]]$label_fontface <- "plain"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_fontface" %in% names(data)){
        data$label_fontface <- "plain"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(shape == "rect")
  cl[["expr"]] <- str2lang(paste0("label_fontface = ", deparse(label_fontface)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_hjust Atomic character vector,
#' indicating which label_hjust to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_hjust_obs(p, { label_hjust = "center" })
label_hjust_obs_nodes <- function(data, label_hjust = "center", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"label_hjust" %in% names(data[[el]])){
        data[[el]]$label_hjust <- "center"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_hjust" %in% names(data)){
        data$label_hjust <- "center"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(shape == "rect")
  cl[["expr"]] <- str2lang(paste0("label_hjust = ", deparse(label_hjust)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_vjust Atomic character vector,
#' indicating which label_vjust to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_vjust_obs(p, { label_vjust = "middle" })
label_vjust_obs_nodes <- function(data, label_vjust = "middle", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"label_vjust" %in% names(data[[el]])){
        data[[el]]$label_vjust <- "middle"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_vjust" %in% names(data)){
        data$label_vjust <- "middle"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(shape == "rect")
  cl[["expr"]] <- str2lang(paste0("label_vjust = ", deparse(label_vjust)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_lineheight Atomic character vector,
#' indicating which label_lineheight to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_lineheight_obs(p, { label_lineheight = 1 })
label_lineheight_obs_nodes <- function(data, label_lineheight = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"label_lineheight" %in% names(data[[el]])){
        data[[el]]$label_lineheight <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_lineheight" %in% names(data)){
        data$label_lineheight <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(shape == "rect")
  cl[["expr"]] <- str2lang(paste0("label_lineheight = ", deparse(label_lineheight)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @rdname if_edit
#' @examples
#' out <- all_latent(p, expr = {label = "sig"})
all_latent_nodes <- function(data, expr, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(shape == "oval")
  cl[["element"]] = "nodes"
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @rdname if_edit
#' @examples
#' out <- hide_latent(p)
hide_latent_nodes <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(shape == "oval")
  cl[["expr"]] <- quote({ show = FALSE })
  cl[["element"]] = "nodes"
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @rdname if_edit
#' @examples
#' out <- show_latent(p)
show_latent_nodes <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(shape == "oval")
  cl[["expr"]] <- quote({ show = TRUE })
  cl[["element"]] = "nodes"
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param colour Atomic character vector,
#' indicating which colour to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- colour_latent(p, { colour = "black" })
colour_latent_nodes <- function(data, colour = "black", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"colour" %in% names(data[[el]])){
        data[[el]]$colour <- "black"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"colour" %in% names(data)){
        data$colour <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(shape == "oval")
  cl[["expr"]] <- str2lang(paste0("colour = ", deparse(colour)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param linewidth Atomic character vector,
#' indicating which linewidth to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- linewidth_latent(p, { linewidth = 1 })
linewidth_latent_nodes <- function(data, linewidth = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"linewidth" %in% names(data[[el]])){
        data[[el]]$linewidth <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"linewidth" %in% names(data)){
        data$linewidth <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(shape == "oval")
  cl[["expr"]] <- str2lang(paste0("linewidth = ", deparse(linewidth)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param linetype Atomic character vector,
#' indicating which linetype to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- linetype_latent(p, { linetype = 1 })
linetype_latent_nodes <- function(data, linetype = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"linetype" %in% names(data[[el]])){
        data[[el]]$linetype <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"linetype" %in% names(data)){
        data$linetype <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(shape == "oval")
  cl[["expr"]] <- str2lang(paste0("linetype = ", deparse(linetype)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param alpha Atomic character vector,
#' indicating which alpha to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- alpha_latent(p, { alpha = 1 })
alpha_latent_nodes <- function(data, alpha = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"alpha" %in% names(data[[el]])){
        data[[el]]$alpha <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"alpha" %in% names(data)){
        data$alpha <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(shape == "oval")
  cl[["expr"]] <- str2lang(paste0("alpha = ", deparse(alpha)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param fill Atomic character vector,
#' indicating which fill to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- fill_latent(p, { fill = "white" })
fill_latent_nodes <- function(data, fill = "white", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"fill" %in% names(data[[el]])){
        data[[el]]$fill <- "white"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"fill" %in% names(data)){
        data$fill <- "white"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(shape == "oval")
  cl[["expr"]] <- str2lang(paste0("fill = ", deparse(fill)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_colour Atomic character vector,
#' indicating which label_colour to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_colour_latent(p, { label_colour = "black" })
label_colour_latent_nodes <- function(data, label_colour = "black", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"label_colour" %in% names(data[[el]])){
        data[[el]]$label_colour <- "black"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_colour" %in% names(data)){
        data$label_colour <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(shape == "oval")
  cl[["expr"]] <- str2lang(paste0("label_colour = ", deparse(label_colour)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_fill Atomic character vector,
#' indicating which label_fill to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_fill_latent(p, { label_fill = "white" })
label_fill_latent_nodes <- function(data, label_fill = "white", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"label_fill" %in% names(data[[el]])){
        data[[el]]$label_fill <- "white"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_fill" %in% names(data)){
        data$label_fill <- "white"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(shape == "oval")
  cl[["expr"]] <- str2lang(paste0("label_fill = ", deparse(label_fill)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_size Atomic character vector,
#' indicating which label_size to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_size_latent(p, { label_size = 4 })
label_size_latent_nodes <- function(data, label_size = 4, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"label_size" %in% names(data[[el]])){
        data[[el]]$label_size <- 4
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_size" %in% names(data)){
        data$label_size <- 4
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(shape == "oval")
  cl[["expr"]] <- str2lang(paste0("label_size = ", deparse(label_size)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_alpha Atomic character vector,
#' indicating which label_alpha to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_alpha_latent(p, { label_alpha = 1 })
label_alpha_latent_nodes <- function(data, label_alpha = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"label_alpha" %in% names(data[[el]])){
        data[[el]]$label_alpha <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_alpha" %in% names(data)){
        data$label_alpha <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(shape == "oval")
  cl[["expr"]] <- str2lang(paste0("label_alpha = ", deparse(label_alpha)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_family Atomic character vector,
#' indicating which label_family to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_family_latent(p, { label_family = "sans" })
label_family_latent_nodes <- function(data, label_family = "sans", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"label_family" %in% names(data[[el]])){
        data[[el]]$label_family <- "sans"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_family" %in% names(data)){
        data$label_family <- "sans"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(shape == "oval")
  cl[["expr"]] <- str2lang(paste0("label_family = ", deparse(label_family)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_fontface Atomic character vector,
#' indicating which label_fontface to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_fontface_latent(p, { label_fontface = "plain" })
label_fontface_latent_nodes <- function(data, label_fontface = "plain", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"label_fontface" %in% names(data[[el]])){
        data[[el]]$label_fontface <- "plain"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_fontface" %in% names(data)){
        data$label_fontface <- "plain"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(shape == "oval")
  cl[["expr"]] <- str2lang(paste0("label_fontface = ", deparse(label_fontface)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_hjust Atomic character vector,
#' indicating which label_hjust to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_hjust_latent(p, { label_hjust = "center" })
label_hjust_latent_nodes <- function(data, label_hjust = "center", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"label_hjust" %in% names(data[[el]])){
        data[[el]]$label_hjust <- "center"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_hjust" %in% names(data)){
        data$label_hjust <- "center"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(shape == "oval")
  cl[["expr"]] <- str2lang(paste0("label_hjust = ", deparse(label_hjust)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_vjust Atomic character vector,
#' indicating which label_vjust to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_vjust_latent(p, { label_vjust = "middle" })
label_vjust_latent_nodes <- function(data, label_vjust = "middle", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"label_vjust" %in% names(data[[el]])){
        data[[el]]$label_vjust <- "middle"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_vjust" %in% names(data)){
        data$label_vjust <- "middle"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(shape == "oval")
  cl[["expr"]] <- str2lang(paste0("label_vjust = ", deparse(label_vjust)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_lineheight Atomic character vector,
#' indicating which label_lineheight to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_lineheight_latent(p, { label_lineheight = 1 })
label_lineheight_latent_nodes <- function(data, label_lineheight = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "nodes"
    for(el in these_elements){
      if(!"label_lineheight" %in% names(data[[el]])){
        data[[el]]$label_lineheight <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_lineheight" %in% names(data)){
        data$label_lineheight <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(shape == "oval")
  cl[["expr"]] <- str2lang(paste0("label_lineheight = ", deparse(label_lineheight)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @rdname if_edit
#' @examples
#' out <- all_sig(p, expr = {label = "sig"})
all_sig_edges <- function(data, expr, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["element"]] = "edges"
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @rdname if_edit
#' @examples
#' out <- hide_sig(p)
hide_sig_edges <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["expr"]] <- quote({ show = FALSE })
  cl[["element"]] = "edges"
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @rdname if_edit
#' @examples
#' out <- show_sig(p)
show_sig_edges <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["expr"]] <- quote({ show = TRUE })
  cl[["element"]] = "edges"
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param colour Atomic character vector,
#' indicating which colour to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- colour_sig(p, { colour = "black" })
colour_sig_edges <- function(data, colour = "black", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"colour" %in% names(data[[el]])){
        data[[el]]$colour <- "black"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"colour" %in% names(data)){
        data$colour <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["expr"]] <- str2lang(paste0("colour = ", deparse(colour)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param linewidth Atomic character vector,
#' indicating which linewidth to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- linewidth_sig(p, { linewidth = 1 })
linewidth_sig_edges <- function(data, linewidth = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"linewidth" %in% names(data[[el]])){
        data[[el]]$linewidth <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"linewidth" %in% names(data)){
        data$linewidth <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["expr"]] <- str2lang(paste0("linewidth = ", deparse(linewidth)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param linetype Atomic character vector,
#' indicating which linetype to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- linetype_sig(p, { linetype = 1 })
linetype_sig_edges <- function(data, linetype = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"linetype" %in% names(data[[el]])){
        data[[el]]$linetype <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"linetype" %in% names(data)){
        data$linetype <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["expr"]] <- str2lang(paste0("linetype = ", deparse(linetype)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param alpha Atomic character vector,
#' indicating which alpha to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- alpha_sig(p, { alpha = 1 })
alpha_sig_edges <- function(data, alpha = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"alpha" %in% names(data[[el]])){
        data[[el]]$alpha <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"alpha" %in% names(data)){
        data$alpha <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["expr"]] <- str2lang(paste0("alpha = ", deparse(alpha)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_colour Atomic character vector,
#' indicating which label_colour to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_colour_sig(p, { label_colour = "black" })
label_colour_sig_edges <- function(data, label_colour = "black", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_colour" %in% names(data[[el]])){
        data[[el]]$label_colour <- "black"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_colour" %in% names(data)){
        data$label_colour <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["expr"]] <- str2lang(paste0("label_colour = ", deparse(label_colour)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_fill Atomic character vector,
#' indicating which label_fill to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_fill_sig(p, { label_fill = "white" })
label_fill_sig_edges <- function(data, label_fill = "white", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_fill" %in% names(data[[el]])){
        data[[el]]$label_fill <- "white"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_fill" %in% names(data)){
        data$label_fill <- "white"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["expr"]] <- str2lang(paste0("label_fill = ", deparse(label_fill)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_size Atomic character vector,
#' indicating which label_size to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_size_sig(p, { label_size = 4 })
label_size_sig_edges <- function(data, label_size = 4, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_size" %in% names(data[[el]])){
        data[[el]]$label_size <- 4
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_size" %in% names(data)){
        data$label_size <- 4
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["expr"]] <- str2lang(paste0("label_size = ", deparse(label_size)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_alpha Atomic character vector,
#' indicating which label_alpha to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_alpha_sig(p, { label_alpha = 1 })
label_alpha_sig_edges <- function(data, label_alpha = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_alpha" %in% names(data[[el]])){
        data[[el]]$label_alpha <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_alpha" %in% names(data)){
        data$label_alpha <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["expr"]] <- str2lang(paste0("label_alpha = ", deparse(label_alpha)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_family Atomic character vector,
#' indicating which label_family to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_family_sig(p, { label_family = "sans" })
label_family_sig_edges <- function(data, label_family = "sans", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_family" %in% names(data[[el]])){
        data[[el]]$label_family <- "sans"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_family" %in% names(data)){
        data$label_family <- "sans"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["expr"]] <- str2lang(paste0("label_family = ", deparse(label_family)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_fontface Atomic character vector,
#' indicating which label_fontface to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_fontface_sig(p, { label_fontface = "plain" })
label_fontface_sig_edges <- function(data, label_fontface = "plain", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_fontface" %in% names(data[[el]])){
        data[[el]]$label_fontface <- "plain"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_fontface" %in% names(data)){
        data$label_fontface <- "plain"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["expr"]] <- str2lang(paste0("label_fontface = ", deparse(label_fontface)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_hjust Atomic character vector,
#' indicating which label_hjust to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_hjust_sig(p, { label_hjust = "center" })
label_hjust_sig_edges <- function(data, label_hjust = "center", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_hjust" %in% names(data[[el]])){
        data[[el]]$label_hjust <- "center"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_hjust" %in% names(data)){
        data$label_hjust <- "center"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["expr"]] <- str2lang(paste0("label_hjust = ", deparse(label_hjust)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_vjust Atomic character vector,
#' indicating which label_vjust to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_vjust_sig(p, { label_vjust = "middle" })
label_vjust_sig_edges <- function(data, label_vjust = "middle", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_vjust" %in% names(data[[el]])){
        data[[el]]$label_vjust <- "middle"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_vjust" %in% names(data)){
        data$label_vjust <- "middle"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["expr"]] <- str2lang(paste0("label_vjust = ", deparse(label_vjust)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_lineheight Atomic character vector,
#' indicating which label_lineheight to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_lineheight_sig(p, { label_lineheight = 1 })
label_lineheight_sig_edges <- function(data, label_lineheight = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_lineheight" %in% names(data[[el]])){
        data[[el]]$label_lineheight <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_lineheight" %in% names(data)){
        data$label_lineheight <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["expr"]] <- str2lang(paste0("label_lineheight = ", deparse(label_lineheight)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_location Atomic character vector,
#' indicating which label_location to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_location_sig(p, { label_location = .5 })
label_location_sig_edges <- function(data, label_location = .5, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_location" %in% names(data[[el]])){
        data[[el]]$label_location <- .5
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_location" %in% names(data)){
        data$label_location <- .5
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["expr"]] <- str2lang(paste0("label_location = ", deparse(label_location)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @rdname if_edit
#' @examples
#' out <- all_nonsig(p, expr = {label = "sig"})
all_nonsig_edges <- function(data, expr, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["element"]] = "edges"
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @rdname if_edit
#' @examples
#' out <- hide_nonsig(p)
hide_nonsig_edges <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["expr"]] <- quote({ show = FALSE })
  cl[["element"]] = "edges"
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @rdname if_edit
#' @examples
#' out <- show_nonsig(p)
show_nonsig_edges <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["expr"]] <- quote({ show = TRUE })
  cl[["element"]] = "edges"
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param colour Atomic character vector,
#' indicating which colour to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- colour_nonsig(p, { colour = "black" })
colour_nonsig_edges <- function(data, colour = "black", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"colour" %in% names(data[[el]])){
        data[[el]]$colour <- "black"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"colour" %in% names(data)){
        data$colour <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["expr"]] <- str2lang(paste0("colour = ", deparse(colour)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param linewidth Atomic character vector,
#' indicating which linewidth to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- linewidth_nonsig(p, { linewidth = 1 })
linewidth_nonsig_edges <- function(data, linewidth = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"linewidth" %in% names(data[[el]])){
        data[[el]]$linewidth <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"linewidth" %in% names(data)){
        data$linewidth <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["expr"]] <- str2lang(paste0("linewidth = ", deparse(linewidth)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param linetype Atomic character vector,
#' indicating which linetype to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- linetype_nonsig(p, { linetype = 1 })
linetype_nonsig_edges <- function(data, linetype = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"linetype" %in% names(data[[el]])){
        data[[el]]$linetype <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"linetype" %in% names(data)){
        data$linetype <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["expr"]] <- str2lang(paste0("linetype = ", deparse(linetype)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param alpha Atomic character vector,
#' indicating which alpha to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- alpha_nonsig(p, { alpha = 1 })
alpha_nonsig_edges <- function(data, alpha = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"alpha" %in% names(data[[el]])){
        data[[el]]$alpha <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"alpha" %in% names(data)){
        data$alpha <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["expr"]] <- str2lang(paste0("alpha = ", deparse(alpha)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_colour Atomic character vector,
#' indicating which label_colour to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_colour_nonsig(p, { label_colour = "black" })
label_colour_nonsig_edges <- function(data, label_colour = "black", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_colour" %in% names(data[[el]])){
        data[[el]]$label_colour <- "black"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_colour" %in% names(data)){
        data$label_colour <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["expr"]] <- str2lang(paste0("label_colour = ", deparse(label_colour)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_fill Atomic character vector,
#' indicating which label_fill to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_fill_nonsig(p, { label_fill = "white" })
label_fill_nonsig_edges <- function(data, label_fill = "white", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_fill" %in% names(data[[el]])){
        data[[el]]$label_fill <- "white"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_fill" %in% names(data)){
        data$label_fill <- "white"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["expr"]] <- str2lang(paste0("label_fill = ", deparse(label_fill)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_size Atomic character vector,
#' indicating which label_size to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_size_nonsig(p, { label_size = 4 })
label_size_nonsig_edges <- function(data, label_size = 4, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_size" %in% names(data[[el]])){
        data[[el]]$label_size <- 4
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_size" %in% names(data)){
        data$label_size <- 4
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["expr"]] <- str2lang(paste0("label_size = ", deparse(label_size)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_alpha Atomic character vector,
#' indicating which label_alpha to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_alpha_nonsig(p, { label_alpha = 1 })
label_alpha_nonsig_edges <- function(data, label_alpha = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_alpha" %in% names(data[[el]])){
        data[[el]]$label_alpha <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_alpha" %in% names(data)){
        data$label_alpha <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["expr"]] <- str2lang(paste0("label_alpha = ", deparse(label_alpha)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_family Atomic character vector,
#' indicating which label_family to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_family_nonsig(p, { label_family = "sans" })
label_family_nonsig_edges <- function(data, label_family = "sans", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_family" %in% names(data[[el]])){
        data[[el]]$label_family <- "sans"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_family" %in% names(data)){
        data$label_family <- "sans"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["expr"]] <- str2lang(paste0("label_family = ", deparse(label_family)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_fontface Atomic character vector,
#' indicating which label_fontface to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_fontface_nonsig(p, { label_fontface = "plain" })
label_fontface_nonsig_edges <- function(data, label_fontface = "plain", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_fontface" %in% names(data[[el]])){
        data[[el]]$label_fontface <- "plain"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_fontface" %in% names(data)){
        data$label_fontface <- "plain"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["expr"]] <- str2lang(paste0("label_fontface = ", deparse(label_fontface)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_hjust Atomic character vector,
#' indicating which label_hjust to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_hjust_nonsig(p, { label_hjust = "center" })
label_hjust_nonsig_edges <- function(data, label_hjust = "center", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_hjust" %in% names(data[[el]])){
        data[[el]]$label_hjust <- "center"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_hjust" %in% names(data)){
        data$label_hjust <- "center"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["expr"]] <- str2lang(paste0("label_hjust = ", deparse(label_hjust)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_vjust Atomic character vector,
#' indicating which label_vjust to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_vjust_nonsig(p, { label_vjust = "middle" })
label_vjust_nonsig_edges <- function(data, label_vjust = "middle", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_vjust" %in% names(data[[el]])){
        data[[el]]$label_vjust <- "middle"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_vjust" %in% names(data)){
        data$label_vjust <- "middle"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["expr"]] <- str2lang(paste0("label_vjust = ", deparse(label_vjust)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_lineheight Atomic character vector,
#' indicating which label_lineheight to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_lineheight_nonsig(p, { label_lineheight = 1 })
label_lineheight_nonsig_edges <- function(data, label_lineheight = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_lineheight" %in% names(data[[el]])){
        data[[el]]$label_lineheight <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_lineheight" %in% names(data)){
        data$label_lineheight <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["expr"]] <- str2lang(paste0("label_lineheight = ", deparse(label_lineheight)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_location Atomic character vector,
#' indicating which label_location to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_location_nonsig(p, { label_location = .5 })
label_location_nonsig_edges <- function(data, label_location = .5, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_location" %in% names(data[[el]])){
        data[[el]]$label_location <- .5
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_location" %in% names(data)){
        data$label_location <- .5
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["expr"]] <- str2lang(paste0("label_location = ", deparse(label_location)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @rdname if_edit
#' @examples
#' out <- all_fixed(p, expr = {label = "sig"})
all_fixed_edges <- function(data, expr, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["element"]] = "edges"
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @rdname if_edit
#' @examples
#' out <- hide_fixed(p)
hide_fixed_edges <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["expr"]] <- quote({ show = FALSE })
  cl[["element"]] = "edges"
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @rdname if_edit
#' @examples
#' out <- show_fixed(p)
show_fixed_edges <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["expr"]] <- quote({ show = TRUE })
  cl[["element"]] = "edges"
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param colour Atomic character vector,
#' indicating which colour to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- colour_fixed(p, { colour = "black" })
colour_fixed_edges <- function(data, colour = "black", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"colour" %in% names(data[[el]])){
        data[[el]]$colour <- "black"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"colour" %in% names(data)){
        data$colour <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["expr"]] <- str2lang(paste0("colour = ", deparse(colour)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param linewidth Atomic character vector,
#' indicating which linewidth to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- linewidth_fixed(p, { linewidth = 1 })
linewidth_fixed_edges <- function(data, linewidth = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"linewidth" %in% names(data[[el]])){
        data[[el]]$linewidth <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"linewidth" %in% names(data)){
        data$linewidth <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["expr"]] <- str2lang(paste0("linewidth = ", deparse(linewidth)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param linetype Atomic character vector,
#' indicating which linetype to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- linetype_fixed(p, { linetype = 1 })
linetype_fixed_edges <- function(data, linetype = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"linetype" %in% names(data[[el]])){
        data[[el]]$linetype <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"linetype" %in% names(data)){
        data$linetype <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["expr"]] <- str2lang(paste0("linetype = ", deparse(linetype)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param alpha Atomic character vector,
#' indicating which alpha to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- alpha_fixed(p, { alpha = 1 })
alpha_fixed_edges <- function(data, alpha = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"alpha" %in% names(data[[el]])){
        data[[el]]$alpha <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"alpha" %in% names(data)){
        data$alpha <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["expr"]] <- str2lang(paste0("alpha = ", deparse(alpha)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_colour Atomic character vector,
#' indicating which label_colour to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_colour_fixed(p, { label_colour = "black" })
label_colour_fixed_edges <- function(data, label_colour = "black", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_colour" %in% names(data[[el]])){
        data[[el]]$label_colour <- "black"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_colour" %in% names(data)){
        data$label_colour <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["expr"]] <- str2lang(paste0("label_colour = ", deparse(label_colour)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_fill Atomic character vector,
#' indicating which label_fill to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_fill_fixed(p, { label_fill = "white" })
label_fill_fixed_edges <- function(data, label_fill = "white", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_fill" %in% names(data[[el]])){
        data[[el]]$label_fill <- "white"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_fill" %in% names(data)){
        data$label_fill <- "white"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["expr"]] <- str2lang(paste0("label_fill = ", deparse(label_fill)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_size Atomic character vector,
#' indicating which label_size to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_size_fixed(p, { label_size = 4 })
label_size_fixed_edges <- function(data, label_size = 4, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_size" %in% names(data[[el]])){
        data[[el]]$label_size <- 4
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_size" %in% names(data)){
        data$label_size <- 4
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["expr"]] <- str2lang(paste0("label_size = ", deparse(label_size)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_alpha Atomic character vector,
#' indicating which label_alpha to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_alpha_fixed(p, { label_alpha = 1 })
label_alpha_fixed_edges <- function(data, label_alpha = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_alpha" %in% names(data[[el]])){
        data[[el]]$label_alpha <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_alpha" %in% names(data)){
        data$label_alpha <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["expr"]] <- str2lang(paste0("label_alpha = ", deparse(label_alpha)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_family Atomic character vector,
#' indicating which label_family to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_family_fixed(p, { label_family = "sans" })
label_family_fixed_edges <- function(data, label_family = "sans", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_family" %in% names(data[[el]])){
        data[[el]]$label_family <- "sans"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_family" %in% names(data)){
        data$label_family <- "sans"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["expr"]] <- str2lang(paste0("label_family = ", deparse(label_family)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_fontface Atomic character vector,
#' indicating which label_fontface to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_fontface_fixed(p, { label_fontface = "plain" })
label_fontface_fixed_edges <- function(data, label_fontface = "plain", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_fontface" %in% names(data[[el]])){
        data[[el]]$label_fontface <- "plain"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_fontface" %in% names(data)){
        data$label_fontface <- "plain"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["expr"]] <- str2lang(paste0("label_fontface = ", deparse(label_fontface)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_hjust Atomic character vector,
#' indicating which label_hjust to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_hjust_fixed(p, { label_hjust = "center" })
label_hjust_fixed_edges <- function(data, label_hjust = "center", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_hjust" %in% names(data[[el]])){
        data[[el]]$label_hjust <- "center"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_hjust" %in% names(data)){
        data$label_hjust <- "center"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["expr"]] <- str2lang(paste0("label_hjust = ", deparse(label_hjust)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_vjust Atomic character vector,
#' indicating which label_vjust to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_vjust_fixed(p, { label_vjust = "middle" })
label_vjust_fixed_edges <- function(data, label_vjust = "middle", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_vjust" %in% names(data[[el]])){
        data[[el]]$label_vjust <- "middle"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_vjust" %in% names(data)){
        data$label_vjust <- "middle"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["expr"]] <- str2lang(paste0("label_vjust = ", deparse(label_vjust)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_lineheight Atomic character vector,
#' indicating which label_lineheight to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_lineheight_fixed(p, { label_lineheight = 1 })
label_lineheight_fixed_edges <- function(data, label_lineheight = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_lineheight" %in% names(data[[el]])){
        data[[el]]$label_lineheight <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_lineheight" %in% names(data)){
        data$label_lineheight <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["expr"]] <- str2lang(paste0("label_lineheight = ", deparse(label_lineheight)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_location Atomic character vector,
#' indicating which label_location to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_location_fixed(p, { label_location = .5 })
label_location_fixed_edges <- function(data, label_location = .5, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_location" %in% names(data[[el]])){
        data[[el]]$label_location <- .5
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_location" %in% names(data)){
        data$label_location <- .5
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["expr"]] <- str2lang(paste0("label_location = ", deparse(label_location)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @rdname if_edit
#' @examples
#' out <- all_pos(p, expr = {label = "sig"})
all_pos_edges <- function(data, expr, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["element"]] = "edges"
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @rdname if_edit
#' @examples
#' out <- hide_pos(p)
hide_pos_edges <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["expr"]] <- quote({ show = FALSE })
  cl[["element"]] = "edges"
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @rdname if_edit
#' @examples
#' out <- show_pos(p)
show_pos_edges <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["expr"]] <- quote({ show = TRUE })
  cl[["element"]] = "edges"
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param colour Atomic character vector,
#' indicating which colour to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- colour_pos(p, { colour = "black" })
colour_pos_edges <- function(data, colour = "black", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"colour" %in% names(data[[el]])){
        data[[el]]$colour <- "black"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"colour" %in% names(data)){
        data$colour <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["expr"]] <- str2lang(paste0("colour = ", deparse(colour)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param linewidth Atomic character vector,
#' indicating which linewidth to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- linewidth_pos(p, { linewidth = 1 })
linewidth_pos_edges <- function(data, linewidth = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"linewidth" %in% names(data[[el]])){
        data[[el]]$linewidth <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"linewidth" %in% names(data)){
        data$linewidth <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["expr"]] <- str2lang(paste0("linewidth = ", deparse(linewidth)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param linetype Atomic character vector,
#' indicating which linetype to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- linetype_pos(p, { linetype = 1 })
linetype_pos_edges <- function(data, linetype = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"linetype" %in% names(data[[el]])){
        data[[el]]$linetype <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"linetype" %in% names(data)){
        data$linetype <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["expr"]] <- str2lang(paste0("linetype = ", deparse(linetype)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param alpha Atomic character vector,
#' indicating which alpha to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- alpha_pos(p, { alpha = 1 })
alpha_pos_edges <- function(data, alpha = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"alpha" %in% names(data[[el]])){
        data[[el]]$alpha <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"alpha" %in% names(data)){
        data$alpha <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["expr"]] <- str2lang(paste0("alpha = ", deparse(alpha)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_colour Atomic character vector,
#' indicating which label_colour to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_colour_pos(p, { label_colour = "black" })
label_colour_pos_edges <- function(data, label_colour = "black", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_colour" %in% names(data[[el]])){
        data[[el]]$label_colour <- "black"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_colour" %in% names(data)){
        data$label_colour <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["expr"]] <- str2lang(paste0("label_colour = ", deparse(label_colour)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_fill Atomic character vector,
#' indicating which label_fill to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_fill_pos(p, { label_fill = "white" })
label_fill_pos_edges <- function(data, label_fill = "white", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_fill" %in% names(data[[el]])){
        data[[el]]$label_fill <- "white"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_fill" %in% names(data)){
        data$label_fill <- "white"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["expr"]] <- str2lang(paste0("label_fill = ", deparse(label_fill)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_size Atomic character vector,
#' indicating which label_size to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_size_pos(p, { label_size = 4 })
label_size_pos_edges <- function(data, label_size = 4, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_size" %in% names(data[[el]])){
        data[[el]]$label_size <- 4
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_size" %in% names(data)){
        data$label_size <- 4
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["expr"]] <- str2lang(paste0("label_size = ", deparse(label_size)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_alpha Atomic character vector,
#' indicating which label_alpha to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_alpha_pos(p, { label_alpha = 1 })
label_alpha_pos_edges <- function(data, label_alpha = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_alpha" %in% names(data[[el]])){
        data[[el]]$label_alpha <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_alpha" %in% names(data)){
        data$label_alpha <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["expr"]] <- str2lang(paste0("label_alpha = ", deparse(label_alpha)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_family Atomic character vector,
#' indicating which label_family to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_family_pos(p, { label_family = "sans" })
label_family_pos_edges <- function(data, label_family = "sans", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_family" %in% names(data[[el]])){
        data[[el]]$label_family <- "sans"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_family" %in% names(data)){
        data$label_family <- "sans"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["expr"]] <- str2lang(paste0("label_family = ", deparse(label_family)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_fontface Atomic character vector,
#' indicating which label_fontface to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_fontface_pos(p, { label_fontface = "plain" })
label_fontface_pos_edges <- function(data, label_fontface = "plain", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_fontface" %in% names(data[[el]])){
        data[[el]]$label_fontface <- "plain"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_fontface" %in% names(data)){
        data$label_fontface <- "plain"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["expr"]] <- str2lang(paste0("label_fontface = ", deparse(label_fontface)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_hjust Atomic character vector,
#' indicating which label_hjust to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_hjust_pos(p, { label_hjust = "center" })
label_hjust_pos_edges <- function(data, label_hjust = "center", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_hjust" %in% names(data[[el]])){
        data[[el]]$label_hjust <- "center"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_hjust" %in% names(data)){
        data$label_hjust <- "center"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["expr"]] <- str2lang(paste0("label_hjust = ", deparse(label_hjust)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_vjust Atomic character vector,
#' indicating which label_vjust to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_vjust_pos(p, { label_vjust = "middle" })
label_vjust_pos_edges <- function(data, label_vjust = "middle", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_vjust" %in% names(data[[el]])){
        data[[el]]$label_vjust <- "middle"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_vjust" %in% names(data)){
        data$label_vjust <- "middle"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["expr"]] <- str2lang(paste0("label_vjust = ", deparse(label_vjust)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_lineheight Atomic character vector,
#' indicating which label_lineheight to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_lineheight_pos(p, { label_lineheight = 1 })
label_lineheight_pos_edges <- function(data, label_lineheight = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_lineheight" %in% names(data[[el]])){
        data[[el]]$label_lineheight <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_lineheight" %in% names(data)){
        data$label_lineheight <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["expr"]] <- str2lang(paste0("label_lineheight = ", deparse(label_lineheight)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_location Atomic character vector,
#' indicating which label_location to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_location_pos(p, { label_location = .5 })
label_location_pos_edges <- function(data, label_location = .5, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_location" %in% names(data[[el]])){
        data[[el]]$label_location <- .5
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_location" %in% names(data)){
        data$label_location <- .5
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["expr"]] <- str2lang(paste0("label_location = ", deparse(label_location)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @rdname if_edit
#' @examples
#' out <- all_neg(p, expr = {label = "sig"})
all_neg_edges <- function(data, expr, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["element"]] = "edges"
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @rdname if_edit
#' @examples
#' out <- hide_neg(p)
hide_neg_edges <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["expr"]] <- quote({ show = FALSE })
  cl[["element"]] = "edges"
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @rdname if_edit
#' @examples
#' out <- show_neg(p)
show_neg_edges <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["expr"]] <- quote({ show = TRUE })
  cl[["element"]] = "edges"
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param colour Atomic character vector,
#' indicating which colour to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- colour_neg(p, { colour = "black" })
colour_neg_edges <- function(data, colour = "black", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"colour" %in% names(data[[el]])){
        data[[el]]$colour <- "black"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"colour" %in% names(data)){
        data$colour <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["expr"]] <- str2lang(paste0("colour = ", deparse(colour)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param linewidth Atomic character vector,
#' indicating which linewidth to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- linewidth_neg(p, { linewidth = 1 })
linewidth_neg_edges <- function(data, linewidth = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"linewidth" %in% names(data[[el]])){
        data[[el]]$linewidth <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"linewidth" %in% names(data)){
        data$linewidth <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["expr"]] <- str2lang(paste0("linewidth = ", deparse(linewidth)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param linetype Atomic character vector,
#' indicating which linetype to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- linetype_neg(p, { linetype = 1 })
linetype_neg_edges <- function(data, linetype = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"linetype" %in% names(data[[el]])){
        data[[el]]$linetype <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"linetype" %in% names(data)){
        data$linetype <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["expr"]] <- str2lang(paste0("linetype = ", deparse(linetype)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param alpha Atomic character vector,
#' indicating which alpha to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- alpha_neg(p, { alpha = 1 })
alpha_neg_edges <- function(data, alpha = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"alpha" %in% names(data[[el]])){
        data[[el]]$alpha <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"alpha" %in% names(data)){
        data$alpha <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["expr"]] <- str2lang(paste0("alpha = ", deparse(alpha)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_colour Atomic character vector,
#' indicating which label_colour to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_colour_neg(p, { label_colour = "black" })
label_colour_neg_edges <- function(data, label_colour = "black", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_colour" %in% names(data[[el]])){
        data[[el]]$label_colour <- "black"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_colour" %in% names(data)){
        data$label_colour <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["expr"]] <- str2lang(paste0("label_colour = ", deparse(label_colour)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_fill Atomic character vector,
#' indicating which label_fill to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_fill_neg(p, { label_fill = "white" })
label_fill_neg_edges <- function(data, label_fill = "white", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_fill" %in% names(data[[el]])){
        data[[el]]$label_fill <- "white"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_fill" %in% names(data)){
        data$label_fill <- "white"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["expr"]] <- str2lang(paste0("label_fill = ", deparse(label_fill)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_size Atomic character vector,
#' indicating which label_size to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_size_neg(p, { label_size = 4 })
label_size_neg_edges <- function(data, label_size = 4, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_size" %in% names(data[[el]])){
        data[[el]]$label_size <- 4
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_size" %in% names(data)){
        data$label_size <- 4
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["expr"]] <- str2lang(paste0("label_size = ", deparse(label_size)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_alpha Atomic character vector,
#' indicating which label_alpha to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_alpha_neg(p, { label_alpha = 1 })
label_alpha_neg_edges <- function(data, label_alpha = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_alpha" %in% names(data[[el]])){
        data[[el]]$label_alpha <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_alpha" %in% names(data)){
        data$label_alpha <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["expr"]] <- str2lang(paste0("label_alpha = ", deparse(label_alpha)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_family Atomic character vector,
#' indicating which label_family to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_family_neg(p, { label_family = "sans" })
label_family_neg_edges <- function(data, label_family = "sans", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_family" %in% names(data[[el]])){
        data[[el]]$label_family <- "sans"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_family" %in% names(data)){
        data$label_family <- "sans"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["expr"]] <- str2lang(paste0("label_family = ", deparse(label_family)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_fontface Atomic character vector,
#' indicating which label_fontface to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_fontface_neg(p, { label_fontface = "plain" })
label_fontface_neg_edges <- function(data, label_fontface = "plain", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_fontface" %in% names(data[[el]])){
        data[[el]]$label_fontface <- "plain"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_fontface" %in% names(data)){
        data$label_fontface <- "plain"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["expr"]] <- str2lang(paste0("label_fontface = ", deparse(label_fontface)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_hjust Atomic character vector,
#' indicating which label_hjust to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_hjust_neg(p, { label_hjust = "center" })
label_hjust_neg_edges <- function(data, label_hjust = "center", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_hjust" %in% names(data[[el]])){
        data[[el]]$label_hjust <- "center"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_hjust" %in% names(data)){
        data$label_hjust <- "center"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["expr"]] <- str2lang(paste0("label_hjust = ", deparse(label_hjust)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_vjust Atomic character vector,
#' indicating which label_vjust to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_vjust_neg(p, { label_vjust = "middle" })
label_vjust_neg_edges <- function(data, label_vjust = "middle", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_vjust" %in% names(data[[el]])){
        data[[el]]$label_vjust <- "middle"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_vjust" %in% names(data)){
        data$label_vjust <- "middle"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["expr"]] <- str2lang(paste0("label_vjust = ", deparse(label_vjust)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_lineheight Atomic character vector,
#' indicating which label_lineheight to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_lineheight_neg(p, { label_lineheight = 1 })
label_lineheight_neg_edges <- function(data, label_lineheight = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_lineheight" %in% names(data[[el]])){
        data[[el]]$label_lineheight <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_lineheight" %in% names(data)){
        data$label_lineheight <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["expr"]] <- str2lang(paste0("label_lineheight = ", deparse(label_lineheight)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_location Atomic character vector,
#' indicating which label_location to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_location_neg(p, { label_location = .5 })
label_location_neg_edges <- function(data, label_location = .5, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_location" %in% names(data[[el]])){
        data[[el]]$label_location <- .5
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_location" %in% names(data)){
        data$label_location <- .5
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["expr"]] <- str2lang(paste0("label_location = ", deparse(label_location)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @rdname if_edit
#' @examples
#' out <- all_var(p, expr = {label = "sig"})
all_var_edges <- function(data, expr, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(lhs == rhs & op == "~~")
  cl[["element"]] = "edges"
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @rdname if_edit
#' @examples
#' out <- hide_var(p)
hide_var_edges <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(lhs == rhs & op == "~~")
  cl[["expr"]] <- quote({ show = FALSE })
  cl[["element"]] = "edges"
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @rdname if_edit
#' @examples
#' out <- show_var(p)
show_var_edges <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(lhs == rhs & op == "~~")
  cl[["expr"]] <- quote({ show = TRUE })
  cl[["element"]] = "edges"
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param colour Atomic character vector,
#' indicating which colour to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- colour_var(p, { colour = "black" })
colour_var_edges <- function(data, colour = "black", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"colour" %in% names(data[[el]])){
        data[[el]]$colour <- "black"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"colour" %in% names(data)){
        data$colour <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(lhs == rhs & op == "~~")
  cl[["expr"]] <- str2lang(paste0("colour = ", deparse(colour)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param linewidth Atomic character vector,
#' indicating which linewidth to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- linewidth_var(p, { linewidth = 1 })
linewidth_var_edges <- function(data, linewidth = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"linewidth" %in% names(data[[el]])){
        data[[el]]$linewidth <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"linewidth" %in% names(data)){
        data$linewidth <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(lhs == rhs & op == "~~")
  cl[["expr"]] <- str2lang(paste0("linewidth = ", deparse(linewidth)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param linetype Atomic character vector,
#' indicating which linetype to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- linetype_var(p, { linetype = 1 })
linetype_var_edges <- function(data, linetype = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"linetype" %in% names(data[[el]])){
        data[[el]]$linetype <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"linetype" %in% names(data)){
        data$linetype <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(lhs == rhs & op == "~~")
  cl[["expr"]] <- str2lang(paste0("linetype = ", deparse(linetype)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param alpha Atomic character vector,
#' indicating which alpha to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- alpha_var(p, { alpha = 1 })
alpha_var_edges <- function(data, alpha = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"alpha" %in% names(data[[el]])){
        data[[el]]$alpha <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"alpha" %in% names(data)){
        data$alpha <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(lhs == rhs & op == "~~")
  cl[["expr"]] <- str2lang(paste0("alpha = ", deparse(alpha)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_colour Atomic character vector,
#' indicating which label_colour to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_colour_var(p, { label_colour = "black" })
label_colour_var_edges <- function(data, label_colour = "black", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_colour" %in% names(data[[el]])){
        data[[el]]$label_colour <- "black"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_colour" %in% names(data)){
        data$label_colour <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(lhs == rhs & op == "~~")
  cl[["expr"]] <- str2lang(paste0("label_colour = ", deparse(label_colour)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_fill Atomic character vector,
#' indicating which label_fill to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_fill_var(p, { label_fill = "white" })
label_fill_var_edges <- function(data, label_fill = "white", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_fill" %in% names(data[[el]])){
        data[[el]]$label_fill <- "white"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_fill" %in% names(data)){
        data$label_fill <- "white"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(lhs == rhs & op == "~~")
  cl[["expr"]] <- str2lang(paste0("label_fill = ", deparse(label_fill)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_size Atomic character vector,
#' indicating which label_size to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_size_var(p, { label_size = 4 })
label_size_var_edges <- function(data, label_size = 4, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_size" %in% names(data[[el]])){
        data[[el]]$label_size <- 4
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_size" %in% names(data)){
        data$label_size <- 4
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(lhs == rhs & op == "~~")
  cl[["expr"]] <- str2lang(paste0("label_size = ", deparse(label_size)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_alpha Atomic character vector,
#' indicating which label_alpha to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_alpha_var(p, { label_alpha = 1 })
label_alpha_var_edges <- function(data, label_alpha = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_alpha" %in% names(data[[el]])){
        data[[el]]$label_alpha <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_alpha" %in% names(data)){
        data$label_alpha <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(lhs == rhs & op == "~~")
  cl[["expr"]] <- str2lang(paste0("label_alpha = ", deparse(label_alpha)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_family Atomic character vector,
#' indicating which label_family to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_family_var(p, { label_family = "sans" })
label_family_var_edges <- function(data, label_family = "sans", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_family" %in% names(data[[el]])){
        data[[el]]$label_family <- "sans"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_family" %in% names(data)){
        data$label_family <- "sans"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(lhs == rhs & op == "~~")
  cl[["expr"]] <- str2lang(paste0("label_family = ", deparse(label_family)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_fontface Atomic character vector,
#' indicating which label_fontface to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_fontface_var(p, { label_fontface = "plain" })
label_fontface_var_edges <- function(data, label_fontface = "plain", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_fontface" %in% names(data[[el]])){
        data[[el]]$label_fontface <- "plain"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_fontface" %in% names(data)){
        data$label_fontface <- "plain"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(lhs == rhs & op == "~~")
  cl[["expr"]] <- str2lang(paste0("label_fontface = ", deparse(label_fontface)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_hjust Atomic character vector,
#' indicating which label_hjust to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_hjust_var(p, { label_hjust = "center" })
label_hjust_var_edges <- function(data, label_hjust = "center", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_hjust" %in% names(data[[el]])){
        data[[el]]$label_hjust <- "center"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_hjust" %in% names(data)){
        data$label_hjust <- "center"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(lhs == rhs & op == "~~")
  cl[["expr"]] <- str2lang(paste0("label_hjust = ", deparse(label_hjust)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_vjust Atomic character vector,
#' indicating which label_vjust to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_vjust_var(p, { label_vjust = "middle" })
label_vjust_var_edges <- function(data, label_vjust = "middle", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_vjust" %in% names(data[[el]])){
        data[[el]]$label_vjust <- "middle"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_vjust" %in% names(data)){
        data$label_vjust <- "middle"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(lhs == rhs & op == "~~")
  cl[["expr"]] <- str2lang(paste0("label_vjust = ", deparse(label_vjust)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_lineheight Atomic character vector,
#' indicating which label_lineheight to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_lineheight_var(p, { label_lineheight = 1 })
label_lineheight_var_edges <- function(data, label_lineheight = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_lineheight" %in% names(data[[el]])){
        data[[el]]$label_lineheight <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_lineheight" %in% names(data)){
        data$label_lineheight <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(lhs == rhs & op == "~~")
  cl[["expr"]] <- str2lang(paste0("label_lineheight = ", deparse(label_lineheight)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @rdname if_edit
#' @examples
#' out <- all_cov(p, expr = {label = "sig"})
all_cov_edges <- function(data, expr, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(lhs != rhs & op == "~~")
  cl[["element"]] = "edges"
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @rdname if_edit
#' @examples
#' out <- hide_cov(p)
hide_cov_edges <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(lhs != rhs & op == "~~")
  cl[["expr"]] <- quote({ show = FALSE })
  cl[["element"]] = "edges"
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @rdname if_edit
#' @examples
#' out <- show_cov(p)
show_cov_edges <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(lhs != rhs & op == "~~")
  cl[["expr"]] <- quote({ show = TRUE })
  cl[["element"]] = "edges"
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param colour Atomic character vector,
#' indicating which colour to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- colour_cov(p, { colour = "black" })
colour_cov_edges <- function(data, colour = "black", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"colour" %in% names(data[[el]])){
        data[[el]]$colour <- "black"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"colour" %in% names(data)){
        data$colour <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(lhs != rhs & op == "~~")
  cl[["expr"]] <- str2lang(paste0("colour = ", deparse(colour)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param linewidth Atomic character vector,
#' indicating which linewidth to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- linewidth_cov(p, { linewidth = 1 })
linewidth_cov_edges <- function(data, linewidth = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"linewidth" %in% names(data[[el]])){
        data[[el]]$linewidth <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"linewidth" %in% names(data)){
        data$linewidth <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(lhs != rhs & op == "~~")
  cl[["expr"]] <- str2lang(paste0("linewidth = ", deparse(linewidth)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param linetype Atomic character vector,
#' indicating which linetype to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- linetype_cov(p, { linetype = 1 })
linetype_cov_edges <- function(data, linetype = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"linetype" %in% names(data[[el]])){
        data[[el]]$linetype <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"linetype" %in% names(data)){
        data$linetype <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(lhs != rhs & op == "~~")
  cl[["expr"]] <- str2lang(paste0("linetype = ", deparse(linetype)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param alpha Atomic character vector,
#' indicating which alpha to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- alpha_cov(p, { alpha = 1 })
alpha_cov_edges <- function(data, alpha = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"alpha" %in% names(data[[el]])){
        data[[el]]$alpha <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"alpha" %in% names(data)){
        data$alpha <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(lhs != rhs & op == "~~")
  cl[["expr"]] <- str2lang(paste0("alpha = ", deparse(alpha)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_colour Atomic character vector,
#' indicating which label_colour to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_colour_cov(p, { label_colour = "black" })
label_colour_cov_edges <- function(data, label_colour = "black", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_colour" %in% names(data[[el]])){
        data[[el]]$label_colour <- "black"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_colour" %in% names(data)){
        data$label_colour <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(lhs != rhs & op == "~~")
  cl[["expr"]] <- str2lang(paste0("label_colour = ", deparse(label_colour)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_fill Atomic character vector,
#' indicating which label_fill to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_fill_cov(p, { label_fill = "white" })
label_fill_cov_edges <- function(data, label_fill = "white", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_fill" %in% names(data[[el]])){
        data[[el]]$label_fill <- "white"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_fill" %in% names(data)){
        data$label_fill <- "white"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(lhs != rhs & op == "~~")
  cl[["expr"]] <- str2lang(paste0("label_fill = ", deparse(label_fill)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_size Atomic character vector,
#' indicating which label_size to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_size_cov(p, { label_size = 4 })
label_size_cov_edges <- function(data, label_size = 4, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_size" %in% names(data[[el]])){
        data[[el]]$label_size <- 4
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_size" %in% names(data)){
        data$label_size <- 4
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(lhs != rhs & op == "~~")
  cl[["expr"]] <- str2lang(paste0("label_size = ", deparse(label_size)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_alpha Atomic character vector,
#' indicating which label_alpha to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_alpha_cov(p, { label_alpha = 1 })
label_alpha_cov_edges <- function(data, label_alpha = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_alpha" %in% names(data[[el]])){
        data[[el]]$label_alpha <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_alpha" %in% names(data)){
        data$label_alpha <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(lhs != rhs & op == "~~")
  cl[["expr"]] <- str2lang(paste0("label_alpha = ", deparse(label_alpha)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_family Atomic character vector,
#' indicating which label_family to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_family_cov(p, { label_family = "sans" })
label_family_cov_edges <- function(data, label_family = "sans", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_family" %in% names(data[[el]])){
        data[[el]]$label_family <- "sans"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_family" %in% names(data)){
        data$label_family <- "sans"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(lhs != rhs & op == "~~")
  cl[["expr"]] <- str2lang(paste0("label_family = ", deparse(label_family)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_fontface Atomic character vector,
#' indicating which label_fontface to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_fontface_cov(p, { label_fontface = "plain" })
label_fontface_cov_edges <- function(data, label_fontface = "plain", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_fontface" %in% names(data[[el]])){
        data[[el]]$label_fontface <- "plain"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_fontface" %in% names(data)){
        data$label_fontface <- "plain"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(lhs != rhs & op == "~~")
  cl[["expr"]] <- str2lang(paste0("label_fontface = ", deparse(label_fontface)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_hjust Atomic character vector,
#' indicating which label_hjust to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_hjust_cov(p, { label_hjust = "center" })
label_hjust_cov_edges <- function(data, label_hjust = "center", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_hjust" %in% names(data[[el]])){
        data[[el]]$label_hjust <- "center"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_hjust" %in% names(data)){
        data$label_hjust <- "center"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(lhs != rhs & op == "~~")
  cl[["expr"]] <- str2lang(paste0("label_hjust = ", deparse(label_hjust)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_vjust Atomic character vector,
#' indicating which label_vjust to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_vjust_cov(p, { label_vjust = "middle" })
label_vjust_cov_edges <- function(data, label_vjust = "middle", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_vjust" %in% names(data[[el]])){
        data[[el]]$label_vjust <- "middle"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_vjust" %in% names(data)){
        data$label_vjust <- "middle"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(lhs != rhs & op == "~~")
  cl[["expr"]] <- str2lang(paste0("label_vjust = ", deparse(label_vjust)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_lineheight Atomic character vector,
#' indicating which label_lineheight to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_lineheight_cov(p, { label_lineheight = 1 })
label_lineheight_cov_edges <- function(data, label_lineheight = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_lineheight" %in% names(data[[el]])){
        data[[el]]$label_lineheight <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_lineheight" %in% names(data)){
        data$label_lineheight <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(lhs != rhs & op == "~~")
  cl[["expr"]] <- str2lang(paste0("label_lineheight = ", deparse(label_lineheight)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_location Atomic character vector,
#' indicating which label_location to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_location_cov(p, { label_location = .5 })
label_location_cov_edges <- function(data, label_location = .5, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_location" %in% names(data[[el]])){
        data[[el]]$label_location <- .5
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_location" %in% names(data)){
        data$label_location <- .5
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(lhs != rhs & op == "~~")
  cl[["expr"]] <- str2lang(paste0("label_location = ", deparse(label_location)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @rdname if_edit
#' @examples
#' out <- all_reg(p, expr = {label = "sig"})
all_reg_edges <- function(data, expr, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(op == "~")
  cl[["element"]] = "edges"
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @rdname if_edit
#' @examples
#' out <- hide_reg(p)
hide_reg_edges <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(op == "~")
  cl[["expr"]] <- quote({ show = FALSE })
  cl[["element"]] = "edges"
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @rdname if_edit
#' @examples
#' out <- show_reg(p)
show_reg_edges <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(op == "~")
  cl[["expr"]] <- quote({ show = TRUE })
  cl[["element"]] = "edges"
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param colour Atomic character vector,
#' indicating which colour to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- colour_reg(p, { colour = "black" })
colour_reg_edges <- function(data, colour = "black", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"colour" %in% names(data[[el]])){
        data[[el]]$colour <- "black"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"colour" %in% names(data)){
        data$colour <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "~")
  cl[["expr"]] <- str2lang(paste0("colour = ", deparse(colour)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param linewidth Atomic character vector,
#' indicating which linewidth to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- linewidth_reg(p, { linewidth = 1 })
linewidth_reg_edges <- function(data, linewidth = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"linewidth" %in% names(data[[el]])){
        data[[el]]$linewidth <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"linewidth" %in% names(data)){
        data$linewidth <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "~")
  cl[["expr"]] <- str2lang(paste0("linewidth = ", deparse(linewidth)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param linetype Atomic character vector,
#' indicating which linetype to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- linetype_reg(p, { linetype = 1 })
linetype_reg_edges <- function(data, linetype = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"linetype" %in% names(data[[el]])){
        data[[el]]$linetype <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"linetype" %in% names(data)){
        data$linetype <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "~")
  cl[["expr"]] <- str2lang(paste0("linetype = ", deparse(linetype)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param alpha Atomic character vector,
#' indicating which alpha to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- alpha_reg(p, { alpha = 1 })
alpha_reg_edges <- function(data, alpha = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"alpha" %in% names(data[[el]])){
        data[[el]]$alpha <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"alpha" %in% names(data)){
        data$alpha <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "~")
  cl[["expr"]] <- str2lang(paste0("alpha = ", deparse(alpha)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_colour Atomic character vector,
#' indicating which label_colour to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_colour_reg(p, { label_colour = "black" })
label_colour_reg_edges <- function(data, label_colour = "black", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_colour" %in% names(data[[el]])){
        data[[el]]$label_colour <- "black"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_colour" %in% names(data)){
        data$label_colour <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "~")
  cl[["expr"]] <- str2lang(paste0("label_colour = ", deparse(label_colour)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_fill Atomic character vector,
#' indicating which label_fill to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_fill_reg(p, { label_fill = "white" })
label_fill_reg_edges <- function(data, label_fill = "white", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_fill" %in% names(data[[el]])){
        data[[el]]$label_fill <- "white"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_fill" %in% names(data)){
        data$label_fill <- "white"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "~")
  cl[["expr"]] <- str2lang(paste0("label_fill = ", deparse(label_fill)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_size Atomic character vector,
#' indicating which label_size to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_size_reg(p, { label_size = 4 })
label_size_reg_edges <- function(data, label_size = 4, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_size" %in% names(data[[el]])){
        data[[el]]$label_size <- 4
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_size" %in% names(data)){
        data$label_size <- 4
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "~")
  cl[["expr"]] <- str2lang(paste0("label_size = ", deparse(label_size)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_alpha Atomic character vector,
#' indicating which label_alpha to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_alpha_reg(p, { label_alpha = 1 })
label_alpha_reg_edges <- function(data, label_alpha = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_alpha" %in% names(data[[el]])){
        data[[el]]$label_alpha <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_alpha" %in% names(data)){
        data$label_alpha <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "~")
  cl[["expr"]] <- str2lang(paste0("label_alpha = ", deparse(label_alpha)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_family Atomic character vector,
#' indicating which label_family to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_family_reg(p, { label_family = "sans" })
label_family_reg_edges <- function(data, label_family = "sans", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_family" %in% names(data[[el]])){
        data[[el]]$label_family <- "sans"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_family" %in% names(data)){
        data$label_family <- "sans"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "~")
  cl[["expr"]] <- str2lang(paste0("label_family = ", deparse(label_family)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_fontface Atomic character vector,
#' indicating which label_fontface to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_fontface_reg(p, { label_fontface = "plain" })
label_fontface_reg_edges <- function(data, label_fontface = "plain", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_fontface" %in% names(data[[el]])){
        data[[el]]$label_fontface <- "plain"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_fontface" %in% names(data)){
        data$label_fontface <- "plain"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "~")
  cl[["expr"]] <- str2lang(paste0("label_fontface = ", deparse(label_fontface)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_hjust Atomic character vector,
#' indicating which label_hjust to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_hjust_reg(p, { label_hjust = "center" })
label_hjust_reg_edges <- function(data, label_hjust = "center", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_hjust" %in% names(data[[el]])){
        data[[el]]$label_hjust <- "center"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_hjust" %in% names(data)){
        data$label_hjust <- "center"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "~")
  cl[["expr"]] <- str2lang(paste0("label_hjust = ", deparse(label_hjust)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_vjust Atomic character vector,
#' indicating which label_vjust to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_vjust_reg(p, { label_vjust = "middle" })
label_vjust_reg_edges <- function(data, label_vjust = "middle", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_vjust" %in% names(data[[el]])){
        data[[el]]$label_vjust <- "middle"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_vjust" %in% names(data)){
        data$label_vjust <- "middle"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "~")
  cl[["expr"]] <- str2lang(paste0("label_vjust = ", deparse(label_vjust)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_lineheight Atomic character vector,
#' indicating which label_lineheight to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_lineheight_reg(p, { label_lineheight = 1 })
label_lineheight_reg_edges <- function(data, label_lineheight = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_lineheight" %in% names(data[[el]])){
        data[[el]]$label_lineheight <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_lineheight" %in% names(data)){
        data$label_lineheight <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "~")
  cl[["expr"]] <- str2lang(paste0("label_lineheight = ", deparse(label_lineheight)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_location Atomic character vector,
#' indicating which label_location to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_location_reg(p, { label_location = .5 })
label_location_reg_edges <- function(data, label_location = .5, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_location" %in% names(data[[el]])){
        data[[el]]$label_location <- .5
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_location" %in% names(data)){
        data$label_location <- .5
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "~")
  cl[["expr"]] <- str2lang(paste0("label_location = ", deparse(label_location)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @rdname if_edit
#' @examples
#' out <- all_load(p, expr = {label = "sig"})
all_load_edges <- function(data, expr, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(op == "=~")
  cl[["element"]] = "edges"
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @rdname if_edit
#' @examples
#' out <- hide_load(p)
hide_load_edges <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(op == "=~")
  cl[["expr"]] <- quote({ show = FALSE })
  cl[["element"]] = "edges"
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @rdname if_edit
#' @examples
#' out <- show_load(p)
show_load_edges <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(op == "=~")
  cl[["expr"]] <- quote({ show = TRUE })
  cl[["element"]] = "edges"
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param colour Atomic character vector,
#' indicating which colour to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- colour_load(p, { colour = "black" })
colour_load_edges <- function(data, colour = "black", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"colour" %in% names(data[[el]])){
        data[[el]]$colour <- "black"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"colour" %in% names(data)){
        data$colour <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "=~")
  cl[["expr"]] <- str2lang(paste0("colour = ", deparse(colour)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param linewidth Atomic character vector,
#' indicating which linewidth to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- linewidth_load(p, { linewidth = 1 })
linewidth_load_edges <- function(data, linewidth = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"linewidth" %in% names(data[[el]])){
        data[[el]]$linewidth <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"linewidth" %in% names(data)){
        data$linewidth <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "=~")
  cl[["expr"]] <- str2lang(paste0("linewidth = ", deparse(linewidth)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param linetype Atomic character vector,
#' indicating which linetype to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- linetype_load(p, { linetype = 1 })
linetype_load_edges <- function(data, linetype = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"linetype" %in% names(data[[el]])){
        data[[el]]$linetype <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"linetype" %in% names(data)){
        data$linetype <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "=~")
  cl[["expr"]] <- str2lang(paste0("linetype = ", deparse(linetype)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param alpha Atomic character vector,
#' indicating which alpha to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- alpha_load(p, { alpha = 1 })
alpha_load_edges <- function(data, alpha = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"alpha" %in% names(data[[el]])){
        data[[el]]$alpha <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"alpha" %in% names(data)){
        data$alpha <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "=~")
  cl[["expr"]] <- str2lang(paste0("alpha = ", deparse(alpha)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_colour Atomic character vector,
#' indicating which label_colour to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_colour_load(p, { label_colour = "black" })
label_colour_load_edges <- function(data, label_colour = "black", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_colour" %in% names(data[[el]])){
        data[[el]]$label_colour <- "black"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_colour" %in% names(data)){
        data$label_colour <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "=~")
  cl[["expr"]] <- str2lang(paste0("label_colour = ", deparse(label_colour)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_fill Atomic character vector,
#' indicating which label_fill to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_fill_load(p, { label_fill = "white" })
label_fill_load_edges <- function(data, label_fill = "white", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_fill" %in% names(data[[el]])){
        data[[el]]$label_fill <- "white"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_fill" %in% names(data)){
        data$label_fill <- "white"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "=~")
  cl[["expr"]] <- str2lang(paste0("label_fill = ", deparse(label_fill)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_size Atomic character vector,
#' indicating which label_size to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_size_load(p, { label_size = 4 })
label_size_load_edges <- function(data, label_size = 4, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_size" %in% names(data[[el]])){
        data[[el]]$label_size <- 4
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_size" %in% names(data)){
        data$label_size <- 4
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "=~")
  cl[["expr"]] <- str2lang(paste0("label_size = ", deparse(label_size)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_alpha Atomic character vector,
#' indicating which label_alpha to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_alpha_load(p, { label_alpha = 1 })
label_alpha_load_edges <- function(data, label_alpha = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_alpha" %in% names(data[[el]])){
        data[[el]]$label_alpha <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_alpha" %in% names(data)){
        data$label_alpha <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "=~")
  cl[["expr"]] <- str2lang(paste0("label_alpha = ", deparse(label_alpha)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_family Atomic character vector,
#' indicating which label_family to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_family_load(p, { label_family = "sans" })
label_family_load_edges <- function(data, label_family = "sans", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_family" %in% names(data[[el]])){
        data[[el]]$label_family <- "sans"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_family" %in% names(data)){
        data$label_family <- "sans"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "=~")
  cl[["expr"]] <- str2lang(paste0("label_family = ", deparse(label_family)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_fontface Atomic character vector,
#' indicating which label_fontface to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_fontface_load(p, { label_fontface = "plain" })
label_fontface_load_edges <- function(data, label_fontface = "plain", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_fontface" %in% names(data[[el]])){
        data[[el]]$label_fontface <- "plain"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_fontface" %in% names(data)){
        data$label_fontface <- "plain"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "=~")
  cl[["expr"]] <- str2lang(paste0("label_fontface = ", deparse(label_fontface)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_hjust Atomic character vector,
#' indicating which label_hjust to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_hjust_load(p, { label_hjust = "center" })
label_hjust_load_edges <- function(data, label_hjust = "center", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_hjust" %in% names(data[[el]])){
        data[[el]]$label_hjust <- "center"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_hjust" %in% names(data)){
        data$label_hjust <- "center"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "=~")
  cl[["expr"]] <- str2lang(paste0("label_hjust = ", deparse(label_hjust)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_vjust Atomic character vector,
#' indicating which label_vjust to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_vjust_load(p, { label_vjust = "middle" })
label_vjust_load_edges <- function(data, label_vjust = "middle", ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_vjust" %in% names(data[[el]])){
        data[[el]]$label_vjust <- "middle"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_vjust" %in% names(data)){
        data$label_vjust <- "middle"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "=~")
  cl[["expr"]] <- str2lang(paste0("label_vjust = ", deparse(label_vjust)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_lineheight Atomic character vector,
#' indicating which label_lineheight to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_lineheight_load(p, { label_lineheight = 1 })
label_lineheight_load_edges <- function(data, label_lineheight = 1, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_lineheight" %in% names(data[[el]])){
        data[[el]]$label_lineheight <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_lineheight" %in% names(data)){
        data$label_lineheight <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "=~")
  cl[["expr"]] <- str2lang(paste0("label_lineheight = ", deparse(label_lineheight)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @param label_location Atomic character vector,
#' indicating which label_location to assign to
#' the selected elements.
#' @rdname if_edit
#' @examples
#' out <- label_location_load(p, { label_location = .5 })
label_location_load_edges <- function(data, label_location = .5, ...){
  if(inherits(data, "sem_graph")){
    these_elements <- "edges"
    for(el in these_elements){
      if(!"label_location" %in% names(data[[el]])){
        data[[el]]$label_location <- .5
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"label_location" %in% names(data)){
        data$label_location <- .5
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "=~")
  cl[["expr"]] <- str2lang(paste0("label_location = ", deparse(label_location)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- str2lang("tidySEM:::if_edit")
  eval.parent(cl)
}
#' @export
#' @rdname if_edit
color_sig <- function(data, color = "black", ...){
  cl <- match.call()
  names(cl)[names(cl) == "color"] <- "colour"
  cl[[1L]] <- as.name("colour_sig")
  eval.parent(cl)
}

#' @export
#' @rdname if_edit
color_nonsig <- function(data, color = "black", ...){
  cl <- match.call()
  names(cl)[names(cl) == "color"] <- "colour"
  cl[[1L]] <- as.name("colour_nonsig")
  eval.parent(cl)
}

#' @export
#' @rdname if_edit
color_fixed <- function(data, color = "black", ...){
  cl <- match.call()
  names(cl)[names(cl) == "color"] <- "colour"
  cl[[1L]] <- as.name("colour_fixed")
  eval.parent(cl)
}

#' @export
#' @rdname if_edit
color_pos <- function(data, color = "black", ...){
  cl <- match.call()
  names(cl)[names(cl) == "color"] <- "colour"
  cl[[1L]] <- as.name("colour_pos")
  eval.parent(cl)
}

#' @export
#' @rdname if_edit
color_neg <- function(data, color = "black", ...){
  cl <- match.call()
  names(cl)[names(cl) == "color"] <- "colour"
  cl[[1L]] <- as.name("colour_neg")
  eval.parent(cl)
}

#' @export
#' @rdname if_edit
color_var <- function(data, color = "black", ...){
  cl <- match.call()
  names(cl)[names(cl) == "color"] <- "colour"
  cl[[1L]] <- as.name("colour_var")
  eval.parent(cl)
}

#' @export
#' @rdname if_edit
color_cov <- function(data, color = "black", ...){
  cl <- match.call()
  names(cl)[names(cl) == "color"] <- "colour"
  cl[[1L]] <- as.name("colour_cov")
  eval.parent(cl)
}

#' @export
#' @rdname if_edit
color_reg <- function(data, color = "black", ...){
  cl <- match.call()
  names(cl)[names(cl) == "color"] <- "colour"
  cl[[1L]] <- as.name("colour_reg")
  eval.parent(cl)
}

#' @export
#' @rdname if_edit
color_load <- function(data, color = "black", ...){
  cl <- match.call()
  names(cl)[names(cl) == "color"] <- "colour"
  cl[[1L]] <- as.name("colour_load")
  eval.parent(cl)
}

#' @export
#' @rdname if_edit
color_obs <- function(data, color = "black", ...){
  cl <- match.call()
  names(cl)[names(cl) == "color"] <- "colour"
  cl[[1L]] <- as.name("colour_obs")
  eval.parent(cl)
}

#' @export
#' @rdname if_edit
color_latent <- function(data, color = "black", ...){
  cl <- match.call()
  names(cl)[names(cl) == "color"] <- "colour"
  cl[[1L]] <- as.name("colour_latent")
  eval.parent(cl)
}

#' @export
#' @rdname if_edit
color_sig_nodes <- function(data, color = "black", ...){
  cl <- match.call()
  names(cl)[names(cl) == "color"] <- "colour"
  cl[[1L]] <- as.name("colour_sig_nodes")
  eval.parent(cl)
}

#' @export
#' @rdname if_edit
color_nonsig_nodes <- function(data, color = "black", ...){
  cl <- match.call()
  names(cl)[names(cl) == "color"] <- "colour"
  cl[[1L]] <- as.name("colour_nonsig_nodes")
  eval.parent(cl)
}

#' @export
#' @rdname if_edit
color_fixed_nodes <- function(data, color = "black", ...){
  cl <- match.call()
  names(cl)[names(cl) == "color"] <- "colour"
  cl[[1L]] <- as.name("colour_fixed_nodes")
  eval.parent(cl)
}

#' @export
#' @rdname if_edit
color_pos_nodes <- function(data, color = "black", ...){
  cl <- match.call()
  names(cl)[names(cl) == "color"] <- "colour"
  cl[[1L]] <- as.name("colour_pos_nodes")
  eval.parent(cl)
}

#' @export
#' @rdname if_edit
color_neg_nodes <- function(data, color = "black", ...){
  cl <- match.call()
  names(cl)[names(cl) == "color"] <- "colour"
  cl[[1L]] <- as.name("colour_neg_nodes")
  eval.parent(cl)
}

#' @export
#' @rdname if_edit
color_obs_nodes <- function(data, color = "black", ...){
  cl <- match.call()
  names(cl)[names(cl) == "color"] <- "colour"
  cl[[1L]] <- as.name("colour_obs_nodes")
  eval.parent(cl)
}

#' @export
#' @rdname if_edit
color_latent_nodes <- function(data, color = "black", ...){
  cl <- match.call()
  names(cl)[names(cl) == "color"] <- "colour"
  cl[[1L]] <- as.name("colour_latent_nodes")
  eval.parent(cl)
}

#' @export
#' @rdname if_edit
color_sig_edges <- function(data, color = "black", ...){
  cl <- match.call()
  names(cl)[names(cl) == "color"] <- "colour"
  cl[[1L]] <- as.name("colour_sig_edges")
  eval.parent(cl)
}

#' @export
#' @rdname if_edit
color_nonsig_edges <- function(data, color = "black", ...){
  cl <- match.call()
  names(cl)[names(cl) == "color"] <- "colour"
  cl[[1L]] <- as.name("colour_nonsig_edges")
  eval.parent(cl)
}

#' @export
#' @rdname if_edit
color_fixed_edges <- function(data, color = "black", ...){
  cl <- match.call()
  names(cl)[names(cl) == "color"] <- "colour"
  cl[[1L]] <- as.name("colour_fixed_edges")
  eval.parent(cl)
}

#' @export
#' @rdname if_edit
color_pos_edges <- function(data, color = "black", ...){
  cl <- match.call()
  names(cl)[names(cl) == "color"] <- "colour"
  cl[[1L]] <- as.name("colour_pos_edges")
  eval.parent(cl)
}

#' @export
#' @rdname if_edit
color_neg_edges <- function(data, color = "black", ...){
  cl <- match.call()
  names(cl)[names(cl) == "color"] <- "colour"
  cl[[1L]] <- as.name("colour_neg_edges")
  eval.parent(cl)
}

#' @export
#' @rdname if_edit
color_var_edges <- function(data, color = "black", ...){
  cl <- match.call()
  names(cl)[names(cl) == "color"] <- "colour"
  cl[[1L]] <- as.name("colour_var_edges")
  eval.parent(cl)
}

#' @export
#' @rdname if_edit
color_cov_edges <- function(data, color = "black", ...){
  cl <- match.call()
  names(cl)[names(cl) == "color"] <- "colour"
  cl[[1L]] <- as.name("colour_cov_edges")
  eval.parent(cl)
}

#' @export
#' @rdname if_edit
color_reg_edges <- function(data, color = "black", ...){
  cl <- match.call()
  names(cl)[names(cl) == "color"] <- "colour"
  cl[[1L]] <- as.name("colour_reg_edges")
  eval.parent(cl)
}

#' @export
#' @rdname if_edit
color_load_edges <- function(data, color = "black", ...){
  cl <- match.call()
  names(cl)[names(cl) == "color"] <- "colour"
  cl[[1L]] <- as.name("colour_load_edges")
  eval.parent(cl)
}

#' @export
#' @rdname if_edit
label_color_sig <- function(data, label_color = "black", ...){
  cl <- match.call()
  names(cl)[names(cl) == "label_color"] <- "label_colour"
  cl[[1L]] <- as.name("label_colour_sig")
  eval.parent(cl)
}

#' @export
#' @rdname if_edit
label_color_nonsig <- function(data, label_color = "black", ...){
  cl <- match.call()
  names(cl)[names(cl) == "label_color"] <- "label_colour"
  cl[[1L]] <- as.name("label_colour_nonsig")
  eval.parent(cl)
}

#' @export
#' @rdname if_edit
label_color_fixed <- function(data, label_color = "black", ...){
  cl <- match.call()
  names(cl)[names(cl) == "label_color"] <- "label_colour"
  cl[[1L]] <- as.name("label_colour_fixed")
  eval.parent(cl)
}

#' @export
#' @rdname if_edit
label_color_pos <- function(data, label_color = "black", ...){
  cl <- match.call()
  names(cl)[names(cl) == "label_color"] <- "label_colour"
  cl[[1L]] <- as.name("label_colour_pos")
  eval.parent(cl)
}

#' @export
#' @rdname if_edit
label_color_neg <- function(data, label_color = "black", ...){
  cl <- match.call()
  names(cl)[names(cl) == "label_color"] <- "label_colour"
  cl[[1L]] <- as.name("label_colour_neg")
  eval.parent(cl)
}

#' @export
#' @rdname if_edit
label_color_var <- function(data, label_color = "black", ...){
  cl <- match.call()
  names(cl)[names(cl) == "label_color"] <- "label_colour"
  cl[[1L]] <- as.name("label_colour_var")
  eval.parent(cl)
}

#' @export
#' @rdname if_edit
label_color_cov <- function(data, label_color = "black", ...){
  cl <- match.call()
  names(cl)[names(cl) == "label_color"] <- "label_colour"
  cl[[1L]] <- as.name("label_colour_cov")
  eval.parent(cl)
}

#' @export
#' @rdname if_edit
label_color_reg <- function(data, label_color = "black", ...){
  cl <- match.call()
  names(cl)[names(cl) == "label_color"] <- "label_colour"
  cl[[1L]] <- as.name("label_colour_reg")
  eval.parent(cl)
}

#' @export
#' @rdname if_edit
label_color_load <- function(data, label_color = "black", ...){
  cl <- match.call()
  names(cl)[names(cl) == "label_color"] <- "label_colour"
  cl[[1L]] <- as.name("label_colour_load")
  eval.parent(cl)
}

#' @export
#' @rdname if_edit
label_color_obs <- function(data, label_color = "black", ...){
  cl <- match.call()
  names(cl)[names(cl) == "label_color"] <- "label_colour"
  cl[[1L]] <- as.name("label_colour_obs")
  eval.parent(cl)
}

#' @export
#' @rdname if_edit
label_color_latent <- function(data, label_color = "black", ...){
  cl <- match.call()
  names(cl)[names(cl) == "label_color"] <- "label_colour"
  cl[[1L]] <- as.name("label_colour_latent")
  eval.parent(cl)
}

#' @export
#' @rdname if_edit
label_color_sig_nodes <- function(data, label_color = "black", ...){
  cl <- match.call()
  names(cl)[names(cl) == "label_color"] <- "label_colour"
  cl[[1L]] <- as.name("label_colour_sig_nodes")
  eval.parent(cl)
}

#' @export
#' @rdname if_edit
label_color_nonsig_nodes <- function(data, label_color = "black", ...){
  cl <- match.call()
  names(cl)[names(cl) == "label_color"] <- "label_colour"
  cl[[1L]] <- as.name("label_colour_nonsig_nodes")
  eval.parent(cl)
}

#' @export
#' @rdname if_edit
label_color_fixed_nodes <- function(data, label_color = "black", ...){
  cl <- match.call()
  names(cl)[names(cl) == "label_color"] <- "label_colour"
  cl[[1L]] <- as.name("label_colour_fixed_nodes")
  eval.parent(cl)
}

#' @export
#' @rdname if_edit
label_color_pos_nodes <- function(data, label_color = "black", ...){
  cl <- match.call()
  names(cl)[names(cl) == "label_color"] <- "label_colour"
  cl[[1L]] <- as.name("label_colour_pos_nodes")
  eval.parent(cl)
}

#' @export
#' @rdname if_edit
label_color_neg_nodes <- function(data, label_color = "black", ...){
  cl <- match.call()
  names(cl)[names(cl) == "label_color"] <- "label_colour"
  cl[[1L]] <- as.name("label_colour_neg_nodes")
  eval.parent(cl)
}

#' @export
#' @rdname if_edit
label_color_obs_nodes <- function(data, label_color = "black", ...){
  cl <- match.call()
  names(cl)[names(cl) == "label_color"] <- "label_colour"
  cl[[1L]] <- as.name("label_colour_obs_nodes")
  eval.parent(cl)
}

#' @export
#' @rdname if_edit
label_color_latent_nodes <- function(data, label_color = "black", ...){
  cl <- match.call()
  names(cl)[names(cl) == "label_color"] <- "label_colour"
  cl[[1L]] <- as.name("label_colour_latent_nodes")
  eval.parent(cl)
}

#' @export
#' @rdname if_edit
label_color_sig_edges <- function(data, label_color = "black", ...){
  cl <- match.call()
  names(cl)[names(cl) == "label_color"] <- "label_colour"
  cl[[1L]] <- as.name("label_colour_sig_edges")
  eval.parent(cl)
}

#' @export
#' @rdname if_edit
label_color_nonsig_edges <- function(data, label_color = "black", ...){
  cl <- match.call()
  names(cl)[names(cl) == "label_color"] <- "label_colour"
  cl[[1L]] <- as.name("label_colour_nonsig_edges")
  eval.parent(cl)
}

#' @export
#' @rdname if_edit
label_color_fixed_edges <- function(data, label_color = "black", ...){
  cl <- match.call()
  names(cl)[names(cl) == "label_color"] <- "label_colour"
  cl[[1L]] <- as.name("label_colour_fixed_edges")
  eval.parent(cl)
}

#' @export
#' @rdname if_edit
label_color_pos_edges <- function(data, label_color = "black", ...){
  cl <- match.call()
  names(cl)[names(cl) == "label_color"] <- "label_colour"
  cl[[1L]] <- as.name("label_colour_pos_edges")
  eval.parent(cl)
}

#' @export
#' @rdname if_edit
label_color_neg_edges <- function(data, label_color = "black", ...){
  cl <- match.call()
  names(cl)[names(cl) == "label_color"] <- "label_colour"
  cl[[1L]] <- as.name("label_colour_neg_edges")
  eval.parent(cl)
}

#' @export
#' @rdname if_edit
label_color_var_edges <- function(data, label_color = "black", ...){
  cl <- match.call()
  names(cl)[names(cl) == "label_color"] <- "label_colour"
  cl[[1L]] <- as.name("label_colour_var_edges")
  eval.parent(cl)
}

#' @export
#' @rdname if_edit
label_color_cov_edges <- function(data, label_color = "black", ...){
  cl <- match.call()
  names(cl)[names(cl) == "label_color"] <- "label_colour"
  cl[[1L]] <- as.name("label_colour_cov_edges")
  eval.parent(cl)
}

#' @export
#' @rdname if_edit
label_color_reg_edges <- function(data, label_color = "black", ...){
  cl <- match.call()
  names(cl)[names(cl) == "label_color"] <- "label_colour"
  cl[[1L]] <- as.name("label_colour_reg_edges")
  eval.parent(cl)
}

#' @export
#' @rdname if_edit
label_color_load_edges <- function(data, label_color = "black", ...){
  cl <- match.call()
  names(cl)[names(cl) == "label_color"] <- "label_colour"
  cl[[1L]] <- as.name("label_colour_load_edges")
  eval.parent(cl)
}

#' @export
#' @rdname if_edit
size_sig <- function(data, size = 1, ...){
  cl <- match.call()
  names(cl)[names(cl) == "size"] <- "linewidth"
  cl[[1L]] <- as.name("linewidth_sig")
  eval.parent(cl)
}

#' @export
#' @rdname if_edit
size_nonsig <- function(data, size = 1, ...){
  cl <- match.call()
  names(cl)[names(cl) == "size"] <- "linewidth"
  cl[[1L]] <- as.name("linewidth_nonsig")
  eval.parent(cl)
}

#' @export
#' @rdname if_edit
size_fixed <- function(data, size = 1, ...){
  cl <- match.call()
  names(cl)[names(cl) == "size"] <- "linewidth"
  cl[[1L]] <- as.name("linewidth_fixed")
  eval.parent(cl)
}

#' @export
#' @rdname if_edit
size_pos <- function(data, size = 1, ...){
  cl <- match.call()
  names(cl)[names(cl) == "size"] <- "linewidth"
  cl[[1L]] <- as.name("linewidth_pos")
  eval.parent(cl)
}

#' @export
#' @rdname if_edit
size_neg <- function(data, size = 1, ...){
  cl <- match.call()
  names(cl)[names(cl) == "size"] <- "linewidth"
  cl[[1L]] <- as.name("linewidth_neg")
  eval.parent(cl)
}

#' @export
#' @rdname if_edit
size_var <- function(data, size = 1, ...){
  cl <- match.call()
  names(cl)[names(cl) == "size"] <- "linewidth"
  cl[[1L]] <- as.name("linewidth_var")
  eval.parent(cl)
}

#' @export
#' @rdname if_edit
size_cov <- function(data, size = 1, ...){
  cl <- match.call()
  names(cl)[names(cl) == "size"] <- "linewidth"
  cl[[1L]] <- as.name("linewidth_cov")
  eval.parent(cl)
}

#' @export
#' @rdname if_edit
size_reg <- function(data, size = 1, ...){
  cl <- match.call()
  names(cl)[names(cl) == "size"] <- "linewidth"
  cl[[1L]] <- as.name("linewidth_reg")
  eval.parent(cl)
}

#' @export
#' @rdname if_edit
size_load <- function(data, size = 1, ...){
  cl <- match.call()
  names(cl)[names(cl) == "size"] <- "linewidth"
  cl[[1L]] <- as.name("linewidth_load")
  eval.parent(cl)
}

#' @export
#' @rdname if_edit
size_obs <- function(data, size = 1, ...){
  cl <- match.call()
  names(cl)[names(cl) == "size"] <- "linewidth"
  cl[[1L]] <- as.name("linewidth_obs")
  eval.parent(cl)
}

#' @export
#' @rdname if_edit
size_latent <- function(data, size = 1, ...){
  cl <- match.call()
  names(cl)[names(cl) == "size"] <- "linewidth"
  cl[[1L]] <- as.name("linewidth_latent")
  eval.parent(cl)
}

#' @export
#' @rdname if_edit
size_sig_nodes <- function(data, size = 1, ...){
  cl <- match.call()
  names(cl)[names(cl) == "size"] <- "linewidth"
  cl[[1L]] <- as.name("linewidth_sig_nodes")
  eval.parent(cl)
}

#' @export
#' @rdname if_edit
size_nonsig_nodes <- function(data, size = 1, ...){
  cl <- match.call()
  names(cl)[names(cl) == "size"] <- "linewidth"
  cl[[1L]] <- as.name("linewidth_nonsig_nodes")
  eval.parent(cl)
}

#' @export
#' @rdname if_edit
size_fixed_nodes <- function(data, size = 1, ...){
  cl <- match.call()
  names(cl)[names(cl) == "size"] <- "linewidth"
  cl[[1L]] <- as.name("linewidth_fixed_nodes")
  eval.parent(cl)
}

#' @export
#' @rdname if_edit
size_pos_nodes <- function(data, size = 1, ...){
  cl <- match.call()
  names(cl)[names(cl) == "size"] <- "linewidth"
  cl[[1L]] <- as.name("linewidth_pos_nodes")
  eval.parent(cl)
}

#' @export
#' @rdname if_edit
size_neg_nodes <- function(data, size = 1, ...){
  cl <- match.call()
  names(cl)[names(cl) == "size"] <- "linewidth"
  cl[[1L]] <- as.name("linewidth_neg_nodes")
  eval.parent(cl)
}

#' @export
#' @rdname if_edit
size_obs_nodes <- function(data, size = 1, ...){
  cl <- match.call()
  names(cl)[names(cl) == "size"] <- "linewidth"
  cl[[1L]] <- as.name("linewidth_obs_nodes")
  eval.parent(cl)
}

#' @export
#' @rdname if_edit
size_latent_nodes <- function(data, size = 1, ...){
  cl <- match.call()
  names(cl)[names(cl) == "size"] <- "linewidth"
  cl[[1L]] <- as.name("linewidth_latent_nodes")
  eval.parent(cl)
}

#' @export
#' @rdname if_edit
size_sig_edges <- function(data, size = 1, ...){
  cl <- match.call()
  names(cl)[names(cl) == "size"] <- "linewidth"
  cl[[1L]] <- as.name("linewidth_sig_edges")
  eval.parent(cl)
}

#' @export
#' @rdname if_edit
size_nonsig_edges <- function(data, size = 1, ...){
  cl <- match.call()
  names(cl)[names(cl) == "size"] <- "linewidth"
  cl[[1L]] <- as.name("linewidth_nonsig_edges")
  eval.parent(cl)
}

#' @export
#' @rdname if_edit
size_fixed_edges <- function(data, size = 1, ...){
  cl <- match.call()
  names(cl)[names(cl) == "size"] <- "linewidth"
  cl[[1L]] <- as.name("linewidth_fixed_edges")
  eval.parent(cl)
}

#' @export
#' @rdname if_edit
size_pos_edges <- function(data, size = 1, ...){
  cl <- match.call()
  names(cl)[names(cl) == "size"] <- "linewidth"
  cl[[1L]] <- as.name("linewidth_pos_edges")
  eval.parent(cl)
}

#' @export
#' @rdname if_edit
size_neg_edges <- function(data, size = 1, ...){
  cl <- match.call()
  names(cl)[names(cl) == "size"] <- "linewidth"
  cl[[1L]] <- as.name("linewidth_neg_edges")
  eval.parent(cl)
}

#' @export
#' @rdname if_edit
size_var_edges <- function(data, size = 1, ...){
  cl <- match.call()
  names(cl)[names(cl) == "size"] <- "linewidth"
  cl[[1L]] <- as.name("linewidth_var_edges")
  eval.parent(cl)
}

#' @export
#' @rdname if_edit
size_cov_edges <- function(data, size = 1, ...){
  cl <- match.call()
  names(cl)[names(cl) == "size"] <- "linewidth"
  cl[[1L]] <- as.name("linewidth_cov_edges")
  eval.parent(cl)
}

#' @export
#' @rdname if_edit
size_reg_edges <- function(data, size = 1, ...){
  cl <- match.call()
  names(cl)[names(cl) == "size"] <- "linewidth"
  cl[[1L]] <- as.name("linewidth_reg_edges")
  eval.parent(cl)
}

#' @export
#' @rdname if_edit
size_load_edges <- function(data, size = 1, ...){
  cl <- match.call()
  names(cl)[names(cl) == "size"] <- "linewidth"
  cl[[1L]] <- as.name("linewidth_load_edges")
  eval.parent(cl)
}

