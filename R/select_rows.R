nodedg <- function(template, txt = "nodes"){
  lapply(template, function(x){
    gsub('c("edges", "nodes")', paste0('"', txt, '"'), gsub(pattern = " <- function", replacement = paste0('_', txt, ' <- function'), x, fixed = TRUE), fixed = TRUE)})
}


create_all_select_rows <- function(filename = "R/select_rows.R"){
  txt <- readLines(filename)
  out <- txt[1:grep("^# Automatically generated functions below here$", txt)]
  # Prepare lists
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
    latent = 'shape == "oval"'#,
    #fixed_load = 'op == "=~" & is.na(pval)'#,
    #mean = 'op == "~1"'
  )
  edges_cond <- c('var', 'cov', 'reg', 'load')
  nodes_cond <- c('obs', 'latent')
  aes_list <- list(
    colour = '"black"',
    color = '"black"',
    linetype = '1',
    size = '1',
    alpha = '1',
    fill = '"white"',
    label_colour = '"black"',
    label_color = '"black"',
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
  nodes_aes <- 'fill'
  edges_aes <- grep('^label_', names(aes_list), value = TRUE)

  # Get templates
  template <- txt[grepl("^#A?[XY]", txt)]
  template <- mapply(function(st, en){
    template[st:en]
  }, st = grep("@export", template), en = c((grep("@export", template)[-1]-1), length(template)))
  which_aes <- which(sapply(template, function(x){any(grepl(pattern = "#AY", x, fixed = TRUE))}))
  # Create aes templates
  aes_temp <- template[which_aes]
  # Remove from template
  template <- template[-which_aes]
  # Clean up
  aes_temp <- lapply(aes_temp, gsub, pattern = "^#A", replacement = "#")
  # All iterations
  add_aes <- vector("list")
  for(i in 1:length(aes_list)){
    add_aes <- c(add_aes,
                 lapply(aes_temp, function(x){ gsub("AES", names(aes_list)[i], gsub("AES_DEFAULT", aes_list[[i]], x))}))
  }
  # Append to template
  template <- c(template, add_aes)

  # Replace COND and NAME with different conditionals
  out <- list()
  for(i in 1:length(fun_list)){
    out <- c(out,
                  lapply(template, function(x){
                    gsub("COND", fun_list[[i]], gsub("NAME", names(fun_list)[i], x))}))
  }
  # Add node and edge specific templates
  template <- c(out,
                nodedg(out, "nodes"),
                nodedg(out, "edges")
                )

  # Filter functions
  edg_only <- c(paste0("_", edges_cond), paste0("#X ", edges_aes))
  nod_only <- c(paste0("_", nodes_cond), paste0("#X ", nodes_aes))
  not_allowed <- c(paste0(paste0("#X ", nodes_aes), edg_only), "label_location_var")
  out <- lapply(template, function(thisfun){
    #if(any(grepl("fill", thisfun))) browser()
    #thisfun = template[[1]]
    if(any(sapply(not_allowed, grepl, x = thisfun))){
      return(NULL)
    }
    if(any(sapply(edg_only, grepl, x = thisfun))){
      #browser()
      if(any(grepl("_(nodes|edges) <-", thisfun))){
        return(NULL)
      } else {
        return(gsub('c("edges", "nodes")', '"edges"', thisfun, fixed = TRUE))
      }
    }
    if(any(sapply(nod_only, grepl, x = thisfun))){
      #browser()
      if(any(grepl("_(nodes|edges) <-", thisfun))){
        return(NULL)
      } else {
        return(gsub('c("edges", "nodes")', '"nodes"', thisfun, fixed = TRUE))
      }
    }
    return(thisfun)
  })
  template <- unlist(out)

  template <- gsub(pattern = "^#X ", replacement = "", template)
  template <- gsub(pattern = "^#Y ", replacement = "#' ", template)
  out <- c(txt[1:grep("^# Automatically generated functions below here$", txt)], template)
  writeLines(out, "R/select_rows.R")
}

#' @title Apply a conditional expression to a sem_graph object
#' @description This function allows users to conditionally manipulate the edges
#' and nodes of a \code{sem_graph} object. The generic function \code{all_fun}
#' applies the expression \code{expr} to all rows of the \code{nodes} and
#' \code{edges} data.frames for which \code{condition} is \code{TRUE}.
#'
#' The wrapper functions documented in the Usage section have a hard-coded
#' \code{expr} and \code{condition}; for example,
#' \code{color_sig(color = "green")} colors all nodes and edges with
#' \code{pval < .05} green.
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
#' all_fun(table_results(res), {label = "bla"}, {pval < .05})
#' @rdname all_fun
#' @export
all_fun <- function(data, expr, condition, ...){
  UseMethod("all_fun", data)
}


# @method all_fun default
# @export
# all_fun.default <- function(data = environment(), expr, condition, ...){
#   browser()
#   cl <- match.call()
#   cl <- cl[c(1, match(c("data", "condition"), names(cl)))]
#   names(cl)[which(names(cl) == "condition")] <- "expr"
#   cl[[1L]] <- quote(with)
#   condition_met <- which(eval.parent(cl))
#   if(length(condition_met) > 0){
#     cl <- match.call()
#     cl <- cl[c(1, match(c("data", "expr"), names(cl)))]
#     cl[["data"]] <- data[condition_met, , FALSE]
#     cl[[1L]] <- quote(within)
#     tryCatch({
#       data[condition_met, ] <- eval.parent(cl)
#     }, warning = function(w){
#       message("Tried to assign to a non-existent variable.")
#     })
#
#   }
#   return(data)
# }

#' @method all_fun data.frame
#' @export
all_fun.data.frame <- function(data, expr, condition, ...){
  cl <- match.call()
  cl <- cl[c(1, match(c("data", "condition"), names(cl)))]
  names(cl)[which(names(cl) == "condition")] <- "expr"
  cl[[1L]] <- quote(with)
  condition_met <- which(eval.parent(cl))
  if(length(condition_met) > 0){
    cl <- match.call()
    cl <- cl[c(1, match(c("data", "expr"), names(cl)))]
    cl[["data"]] <- data[condition_met, , FALSE]
    cl[[1L]] <- quote(within)
    data[condition_met, ] <- eval.parent(cl)
  }
  return(data)
}

# @method all_fun tidy_nodes
# @export
#all_fun.tidy_nodes <- all_fun.tidy_edges

#' @method all_fun sem_graph
#' @param element Character vector. The elements of the \code{sem_graph} to
#' edit, defaults to \code{"edges"}.
#' @rdname all_fun
#' @export
all_fun.sem_graph <- function(data, expr, condition, element = "edges", ...){
  cl <- match.call()
  out <- data
  cl[[1L]] <- str2lang("tidySEM:::all_fun.data.frame")
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
#Y @rdname all_fun
#X all_NAME <- function(data, expr, ...){
#X   cl <- match.call()
#X   cl[["condition"]] <- substitute(COND)
#X   cl[["element"]] = c("edges", "nodes")
#X   cl[[1L]] <- quote(all_fun)
#X   eval.parent(cl)
#X }

#Y @export
#Y @rdname all_fun
#X hide_NAME <- function(data, ...){
#X   cl <- match.call()
#X   cl[["condition"]] <- substitute(COND)
#X   cl[["expr"]] <- quote({ show = FALSE })
#X   cl[["element"]] = c("edges", "nodes")
#X   cl[[1L]] <- quote(all_fun)
#X   eval.parent(cl)
#X }

#Y @export
#Y @rdname all_fun
#X show_NAME <- function(data, ...){
#X   cl <- match.call()
#X   cl[["condition"]] <- substitute(COND)
#X   cl[["expr"]] <- quote({ show = TRUE })
#X   cl[["element"]] = c("edges", "nodes")
#X   cl[[1L]] <- quote(all_fun)
#X   eval.parent(cl)
#X }

#AY @export
#AY @param AES Atomic character vector,
#AY indicating which AES to assign to
#AY the selected elements.
#AY @rdname all_fun
#AX AES_NAME <- function(data, AES = AES_DEFAULT, ...){
#AX   these_elements <- c("edges", "nodes")
#AX   for(el in these_elements){
#AX     if(!"AES" %in% names(data[[el]])){
#AX       data[[el]]$AES <- AES_DEFAULT
#AX     }
#AX   }
#AX   cl <- match.call()
#AX   cl[["data"]] <- data
#AX   cl[["condition"]] <- substitute(COND)
#AX   cl[["expr"]] <- str2lang(paste0("AES = ", deparse(AES)))
#AX   cl[["element"]] <- c("edges", "nodes")
#AX   cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
#AX   cl[[1L]] <- quote(all_fun)
#AX   eval.parent(cl)
#AX }

# End template

# Automatically generated functions below here
#' @export
#' @rdname all_fun
all_sig <- function(data, expr, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["element"]] = c("edges", "nodes")
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
hide_sig <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["expr"]] <- quote({ show = FALSE })
  cl[["element"]] = c("edges", "nodes")
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
show_sig <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["expr"]] <- quote({ show = TRUE })
  cl[["element"]] = c("edges", "nodes")
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param colour Atomic character vector, indicating which colour to assign to
#' the selected elements.
#' @rdname all_fun
colour_sig <- function(data, colour = "black", ...){
  these_elements <- c("edges", "nodes")
  for(el in these_elements){
    if(!"colour" %in% names(data[[el]])){
      data[[el]]$colour <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["expr"]] <- str2lang(paste0("colour = ", deparse(colour)))
  cl[["element"]] <- c("edges", "nodes")
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param color Atomic character vector, indicating which color to assign to
#' the selected elements.
#' @rdname all_fun
color_sig <- function(data, color = "black", ...){
  these_elements <- c("edges", "nodes")
  for(el in these_elements){
    if(!"color" %in% names(data[[el]])){
      data[[el]]$color <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["expr"]] <- str2lang(paste0("color = ", deparse(color)))
  cl[["element"]] <- c("edges", "nodes")
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param linetype Atomic character vector, indicating which linetype to assign to
#' the selected elements.
#' @rdname all_fun
linetype_sig <- function(data, linetype = 1, ...){
  these_elements <- c("edges", "nodes")
  for(el in these_elements){
    if(!"linetype" %in% names(data[[el]])){
      data[[el]]$linetype <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["expr"]] <- str2lang(paste0("linetype = ", deparse(linetype)))
  cl[["element"]] <- c("edges", "nodes")
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param size Atomic character vector, indicating which size to assign to
#' the selected elements.
#' @rdname all_fun
size_sig <- function(data, size = 1, ...){
  these_elements <- c("edges", "nodes")
  for(el in these_elements){
    if(!"size" %in% names(data[[el]])){
      data[[el]]$size <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["expr"]] <- str2lang(paste0("size = ", deparse(size)))
  cl[["element"]] <- c("edges", "nodes")
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param alpha Atomic character vector, indicating which alpha to assign to
#' the selected elements.
#' @rdname all_fun
alpha_sig <- function(data, alpha = 1, ...){
  these_elements <- c("edges", "nodes")
  for(el in these_elements){
    if(!"alpha" %in% names(data[[el]])){
      data[[el]]$alpha <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["expr"]] <- str2lang(paste0("alpha = ", deparse(alpha)))
  cl[["element"]] <- c("edges", "nodes")
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param fill Atomic character vector, indicating which fill to assign to
#' the selected elements.
#' @rdname all_fun
fill_sig <- function(data, fill = "white", ...){
  these_elements <- "nodes"
  for(el in these_elements){
    if(!"fill" %in% names(data[[el]])){
      data[[el]]$fill <- "white"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["expr"]] <- str2lang(paste0("fill = ", deparse(fill)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_colour Atomic character vector, indicating which label_colour to assign to
#' the selected elements.
#' @rdname all_fun
label_colour_sig <- function(data, label_colour = "black", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_colour" %in% names(data[[el]])){
      data[[el]]$label_colour <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["expr"]] <- str2lang(paste0("label_colour = ", deparse(label_colour)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_color Atomic character vector, indicating which label_color to assign to
#' the selected elements.
#' @rdname all_fun
label_color_sig <- function(data, label_color = "black", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_color" %in% names(data[[el]])){
      data[[el]]$label_color <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["expr"]] <- str2lang(paste0("label_color = ", deparse(label_color)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_fill Atomic character vector, indicating which label_fill to assign to
#' the selected elements.
#' @rdname all_fun
label_fill_sig <- function(data, label_fill = "white", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_fill" %in% names(data[[el]])){
      data[[el]]$label_fill <- "white"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["expr"]] <- str2lang(paste0("label_fill = ", deparse(label_fill)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_size Atomic character vector, indicating which label_size to assign to
#' the selected elements.
#' @rdname all_fun
label_size_sig <- function(data, label_size = 4, ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_size" %in% names(data[[el]])){
      data[[el]]$label_size <- 4
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["expr"]] <- str2lang(paste0("label_size = ", deparse(label_size)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_alpha Atomic character vector, indicating which label_alpha to assign to
#' the selected elements.
#' @rdname all_fun
label_alpha_sig <- function(data, label_alpha = 1, ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_alpha" %in% names(data[[el]])){
      data[[el]]$label_alpha <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["expr"]] <- str2lang(paste0("label_alpha = ", deparse(label_alpha)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_family Atomic character vector, indicating which label_family to assign to
#' the selected elements.
#' @rdname all_fun
label_family_sig <- function(data, label_family = "sans", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_family" %in% names(data[[el]])){
      data[[el]]$label_family <- "sans"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["expr"]] <- str2lang(paste0("label_family = ", deparse(label_family)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_fontface Atomic character vector, indicating which label_fontface to assign to
#' the selected elements.
#' @rdname all_fun
label_fontface_sig <- function(data, label_fontface = "plain", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_fontface" %in% names(data[[el]])){
      data[[el]]$label_fontface <- "plain"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["expr"]] <- str2lang(paste0("label_fontface = ", deparse(label_fontface)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_hjust Atomic character vector, indicating which label_hjust to assign to
#' the selected elements.
#' @rdname all_fun
label_hjust_sig <- function(data, label_hjust = "center", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_hjust" %in% names(data[[el]])){
      data[[el]]$label_hjust <- "center"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["expr"]] <- str2lang(paste0("label_hjust = ", deparse(label_hjust)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_vjust Atomic character vector, indicating which label_vjust to assign to
#' the selected elements.
#' @rdname all_fun
label_vjust_sig <- function(data, label_vjust = "middle", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_vjust" %in% names(data[[el]])){
      data[[el]]$label_vjust <- "middle"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["expr"]] <- str2lang(paste0("label_vjust = ", deparse(label_vjust)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_lineheight Atomic character vector, indicating which label_lineheight to assign to
#' the selected elements.
#' @rdname all_fun
label_lineheight_sig <- function(data, label_lineheight = 1, ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_lineheight" %in% names(data[[el]])){
      data[[el]]$label_lineheight <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["expr"]] <- str2lang(paste0("label_lineheight = ", deparse(label_lineheight)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_location Atomic character vector, indicating which label_location to assign to
#' the selected elements.
#' @rdname all_fun
label_location_sig <- function(data, label_location = .5, ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_location" %in% names(data[[el]])){
      data[[el]]$label_location <- .5
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["expr"]] <- str2lang(paste0("label_location = ", deparse(label_location)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
all_nonsig <- function(data, expr, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["element"]] = c("edges", "nodes")
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
hide_nonsig <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["expr"]] <- quote({ show = FALSE })
  cl[["element"]] = c("edges", "nodes")
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
show_nonsig <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["expr"]] <- quote({ show = TRUE })
  cl[["element"]] = c("edges", "nodes")
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param colour Atomic character vector, indicating which colour to assign to
#' the selected elements.
#' @rdname all_fun
colour_nonsig <- function(data, colour = "black", ...){
  these_elements <- c("edges", "nodes")
  for(el in these_elements){
    if(!"colour" %in% names(data[[el]])){
      data[[el]]$colour <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["expr"]] <- str2lang(paste0("colour = ", deparse(colour)))
  cl[["element"]] <- c("edges", "nodes")
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param color Atomic character vector, indicating which color to assign to
#' the selected elements.
#' @rdname all_fun
color_nonsig <- function(data, color = "black", ...){
  these_elements <- c("edges", "nodes")
  for(el in these_elements){
    if(!"color" %in% names(data[[el]])){
      data[[el]]$color <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["expr"]] <- str2lang(paste0("color = ", deparse(color)))
  cl[["element"]] <- c("edges", "nodes")
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param linetype Atomic character vector, indicating which linetype to assign to
#' the selected elements.
#' @rdname all_fun
linetype_nonsig <- function(data, linetype = 1, ...){
  these_elements <- c("edges", "nodes")
  for(el in these_elements){
    if(!"linetype" %in% names(data[[el]])){
      data[[el]]$linetype <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["expr"]] <- str2lang(paste0("linetype = ", deparse(linetype)))
  cl[["element"]] <- c("edges", "nodes")
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param size Atomic character vector, indicating which size to assign to
#' the selected elements.
#' @rdname all_fun
size_nonsig <- function(data, size = 1, ...){
  these_elements <- c("edges", "nodes")
  for(el in these_elements){
    if(!"size" %in% names(data[[el]])){
      data[[el]]$size <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["expr"]] <- str2lang(paste0("size = ", deparse(size)))
  cl[["element"]] <- c("edges", "nodes")
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param alpha Atomic character vector, indicating which alpha to assign to
#' the selected elements.
#' @rdname all_fun
alpha_nonsig <- function(data, alpha = 1, ...){
  these_elements <- c("edges", "nodes")
  for(el in these_elements){
    if(!"alpha" %in% names(data[[el]])){
      data[[el]]$alpha <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["expr"]] <- str2lang(paste0("alpha = ", deparse(alpha)))
  cl[["element"]] <- c("edges", "nodes")
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param fill Atomic character vector, indicating which fill to assign to
#' the selected elements.
#' @rdname all_fun
fill_nonsig <- function(data, fill = "white", ...){
  these_elements <- "nodes"
  for(el in these_elements){
    if(!"fill" %in% names(data[[el]])){
      data[[el]]$fill <- "white"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["expr"]] <- str2lang(paste0("fill = ", deparse(fill)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_colour Atomic character vector, indicating which label_colour to assign to
#' the selected elements.
#' @rdname all_fun
label_colour_nonsig <- function(data, label_colour = "black", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_colour" %in% names(data[[el]])){
      data[[el]]$label_colour <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["expr"]] <- str2lang(paste0("label_colour = ", deparse(label_colour)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_color Atomic character vector, indicating which label_color to assign to
#' the selected elements.
#' @rdname all_fun
label_color_nonsig <- function(data, label_color = "black", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_color" %in% names(data[[el]])){
      data[[el]]$label_color <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["expr"]] <- str2lang(paste0("label_color = ", deparse(label_color)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_fill Atomic character vector, indicating which label_fill to assign to
#' the selected elements.
#' @rdname all_fun
label_fill_nonsig <- function(data, label_fill = "white", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_fill" %in% names(data[[el]])){
      data[[el]]$label_fill <- "white"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["expr"]] <- str2lang(paste0("label_fill = ", deparse(label_fill)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_size Atomic character vector, indicating which label_size to assign to
#' the selected elements.
#' @rdname all_fun
label_size_nonsig <- function(data, label_size = 4, ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_size" %in% names(data[[el]])){
      data[[el]]$label_size <- 4
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["expr"]] <- str2lang(paste0("label_size = ", deparse(label_size)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_alpha Atomic character vector, indicating which label_alpha to assign to
#' the selected elements.
#' @rdname all_fun
label_alpha_nonsig <- function(data, label_alpha = 1, ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_alpha" %in% names(data[[el]])){
      data[[el]]$label_alpha <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["expr"]] <- str2lang(paste0("label_alpha = ", deparse(label_alpha)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_family Atomic character vector, indicating which label_family to assign to
#' the selected elements.
#' @rdname all_fun
label_family_nonsig <- function(data, label_family = "sans", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_family" %in% names(data[[el]])){
      data[[el]]$label_family <- "sans"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["expr"]] <- str2lang(paste0("label_family = ", deparse(label_family)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_fontface Atomic character vector, indicating which label_fontface to assign to
#' the selected elements.
#' @rdname all_fun
label_fontface_nonsig <- function(data, label_fontface = "plain", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_fontface" %in% names(data[[el]])){
      data[[el]]$label_fontface <- "plain"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["expr"]] <- str2lang(paste0("label_fontface = ", deparse(label_fontface)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_hjust Atomic character vector, indicating which label_hjust to assign to
#' the selected elements.
#' @rdname all_fun
label_hjust_nonsig <- function(data, label_hjust = "center", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_hjust" %in% names(data[[el]])){
      data[[el]]$label_hjust <- "center"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["expr"]] <- str2lang(paste0("label_hjust = ", deparse(label_hjust)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_vjust Atomic character vector, indicating which label_vjust to assign to
#' the selected elements.
#' @rdname all_fun
label_vjust_nonsig <- function(data, label_vjust = "middle", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_vjust" %in% names(data[[el]])){
      data[[el]]$label_vjust <- "middle"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["expr"]] <- str2lang(paste0("label_vjust = ", deparse(label_vjust)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_lineheight Atomic character vector, indicating which label_lineheight to assign to
#' the selected elements.
#' @rdname all_fun
label_lineheight_nonsig <- function(data, label_lineheight = 1, ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_lineheight" %in% names(data[[el]])){
      data[[el]]$label_lineheight <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["expr"]] <- str2lang(paste0("label_lineheight = ", deparse(label_lineheight)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_location Atomic character vector, indicating which label_location to assign to
#' the selected elements.
#' @rdname all_fun
label_location_nonsig <- function(data, label_location = .5, ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_location" %in% names(data[[el]])){
      data[[el]]$label_location <- .5
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["expr"]] <- str2lang(paste0("label_location = ", deparse(label_location)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
all_fixed <- function(data, expr, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["element"]] = c("edges", "nodes")
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
hide_fixed <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["expr"]] <- quote({ show = FALSE })
  cl[["element"]] = c("edges", "nodes")
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
show_fixed <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["expr"]] <- quote({ show = TRUE })
  cl[["element"]] = c("edges", "nodes")
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param colour Atomic character vector, indicating which colour to assign to
#' the selected elements.
#' @rdname all_fun
colour_fixed <- function(data, colour = "black", ...){
  these_elements <- c("edges", "nodes")
  for(el in these_elements){
    if(!"colour" %in% names(data[[el]])){
      data[[el]]$colour <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["expr"]] <- str2lang(paste0("colour = ", deparse(colour)))
  cl[["element"]] <- c("edges", "nodes")
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param color Atomic character vector, indicating which color to assign to
#' the selected elements.
#' @rdname all_fun
color_fixed <- function(data, color = "black", ...){
  these_elements <- c("edges", "nodes")
  for(el in these_elements){
    if(!"color" %in% names(data[[el]])){
      data[[el]]$color <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["expr"]] <- str2lang(paste0("color = ", deparse(color)))
  cl[["element"]] <- c("edges", "nodes")
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param linetype Atomic character vector, indicating which linetype to assign to
#' the selected elements.
#' @rdname all_fun
linetype_fixed <- function(data, linetype = 1, ...){
  these_elements <- c("edges", "nodes")
  for(el in these_elements){
    if(!"linetype" %in% names(data[[el]])){
      data[[el]]$linetype <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["expr"]] <- str2lang(paste0("linetype = ", deparse(linetype)))
  cl[["element"]] <- c("edges", "nodes")
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param size Atomic character vector, indicating which size to assign to
#' the selected elements.
#' @rdname all_fun
size_fixed <- function(data, size = 1, ...){
  these_elements <- c("edges", "nodes")
  for(el in these_elements){
    if(!"size" %in% names(data[[el]])){
      data[[el]]$size <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["expr"]] <- str2lang(paste0("size = ", deparse(size)))
  cl[["element"]] <- c("edges", "nodes")
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param alpha Atomic character vector, indicating which alpha to assign to
#' the selected elements.
#' @rdname all_fun
alpha_fixed <- function(data, alpha = 1, ...){
  these_elements <- c("edges", "nodes")
  for(el in these_elements){
    if(!"alpha" %in% names(data[[el]])){
      data[[el]]$alpha <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["expr"]] <- str2lang(paste0("alpha = ", deparse(alpha)))
  cl[["element"]] <- c("edges", "nodes")
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param fill Atomic character vector, indicating which fill to assign to
#' the selected elements.
#' @rdname all_fun
fill_fixed <- function(data, fill = "white", ...){
  these_elements <- "nodes"
  for(el in these_elements){
    if(!"fill" %in% names(data[[el]])){
      data[[el]]$fill <- "white"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["expr"]] <- str2lang(paste0("fill = ", deparse(fill)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_colour Atomic character vector, indicating which label_colour to assign to
#' the selected elements.
#' @rdname all_fun
label_colour_fixed <- function(data, label_colour = "black", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_colour" %in% names(data[[el]])){
      data[[el]]$label_colour <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["expr"]] <- str2lang(paste0("label_colour = ", deparse(label_colour)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_color Atomic character vector, indicating which label_color to assign to
#' the selected elements.
#' @rdname all_fun
label_color_fixed <- function(data, label_color = "black", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_color" %in% names(data[[el]])){
      data[[el]]$label_color <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["expr"]] <- str2lang(paste0("label_color = ", deparse(label_color)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_fill Atomic character vector, indicating which label_fill to assign to
#' the selected elements.
#' @rdname all_fun
label_fill_fixed <- function(data, label_fill = "white", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_fill" %in% names(data[[el]])){
      data[[el]]$label_fill <- "white"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["expr"]] <- str2lang(paste0("label_fill = ", deparse(label_fill)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_size Atomic character vector, indicating which label_size to assign to
#' the selected elements.
#' @rdname all_fun
label_size_fixed <- function(data, label_size = 4, ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_size" %in% names(data[[el]])){
      data[[el]]$label_size <- 4
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["expr"]] <- str2lang(paste0("label_size = ", deparse(label_size)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_alpha Atomic character vector, indicating which label_alpha to assign to
#' the selected elements.
#' @rdname all_fun
label_alpha_fixed <- function(data, label_alpha = 1, ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_alpha" %in% names(data[[el]])){
      data[[el]]$label_alpha <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["expr"]] <- str2lang(paste0("label_alpha = ", deparse(label_alpha)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_family Atomic character vector, indicating which label_family to assign to
#' the selected elements.
#' @rdname all_fun
label_family_fixed <- function(data, label_family = "sans", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_family" %in% names(data[[el]])){
      data[[el]]$label_family <- "sans"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["expr"]] <- str2lang(paste0("label_family = ", deparse(label_family)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_fontface Atomic character vector, indicating which label_fontface to assign to
#' the selected elements.
#' @rdname all_fun
label_fontface_fixed <- function(data, label_fontface = "plain", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_fontface" %in% names(data[[el]])){
      data[[el]]$label_fontface <- "plain"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["expr"]] <- str2lang(paste0("label_fontface = ", deparse(label_fontface)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_hjust Atomic character vector, indicating which label_hjust to assign to
#' the selected elements.
#' @rdname all_fun
label_hjust_fixed <- function(data, label_hjust = "center", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_hjust" %in% names(data[[el]])){
      data[[el]]$label_hjust <- "center"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["expr"]] <- str2lang(paste0("label_hjust = ", deparse(label_hjust)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_vjust Atomic character vector, indicating which label_vjust to assign to
#' the selected elements.
#' @rdname all_fun
label_vjust_fixed <- function(data, label_vjust = "middle", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_vjust" %in% names(data[[el]])){
      data[[el]]$label_vjust <- "middle"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["expr"]] <- str2lang(paste0("label_vjust = ", deparse(label_vjust)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_lineheight Atomic character vector, indicating which label_lineheight to assign to
#' the selected elements.
#' @rdname all_fun
label_lineheight_fixed <- function(data, label_lineheight = 1, ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_lineheight" %in% names(data[[el]])){
      data[[el]]$label_lineheight <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["expr"]] <- str2lang(paste0("label_lineheight = ", deparse(label_lineheight)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_location Atomic character vector, indicating which label_location to assign to
#' the selected elements.
#' @rdname all_fun
label_location_fixed <- function(data, label_location = .5, ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_location" %in% names(data[[el]])){
      data[[el]]$label_location <- .5
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["expr"]] <- str2lang(paste0("label_location = ", deparse(label_location)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
all_pos <- function(data, expr, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["element"]] = c("edges", "nodes")
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
hide_pos <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["expr"]] <- quote({ show = FALSE })
  cl[["element"]] = c("edges", "nodes")
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
show_pos <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["expr"]] <- quote({ show = TRUE })
  cl[["element"]] = c("edges", "nodes")
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param colour Atomic character vector, indicating which colour to assign to
#' the selected elements.
#' @rdname all_fun
colour_pos <- function(data, colour = "black", ...){
  these_elements <- c("edges", "nodes")
  for(el in these_elements){
    if(!"colour" %in% names(data[[el]])){
      data[[el]]$colour <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["expr"]] <- str2lang(paste0("colour = ", deparse(colour)))
  cl[["element"]] <- c("edges", "nodes")
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param color Atomic character vector, indicating which color to assign to
#' the selected elements.
#' @rdname all_fun
color_pos <- function(data, color = "black", ...){
  these_elements <- c("edges", "nodes")
  for(el in these_elements){
    if(!"color" %in% names(data[[el]])){
      data[[el]]$color <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["expr"]] <- str2lang(paste0("color = ", deparse(color)))
  cl[["element"]] <- c("edges", "nodes")
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param linetype Atomic character vector, indicating which linetype to assign to
#' the selected elements.
#' @rdname all_fun
linetype_pos <- function(data, linetype = 1, ...){
  these_elements <- c("edges", "nodes")
  for(el in these_elements){
    if(!"linetype" %in% names(data[[el]])){
      data[[el]]$linetype <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["expr"]] <- str2lang(paste0("linetype = ", deparse(linetype)))
  cl[["element"]] <- c("edges", "nodes")
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param size Atomic character vector, indicating which size to assign to
#' the selected elements.
#' @rdname all_fun
size_pos <- function(data, size = 1, ...){
  these_elements <- c("edges", "nodes")
  for(el in these_elements){
    if(!"size" %in% names(data[[el]])){
      data[[el]]$size <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["expr"]] <- str2lang(paste0("size = ", deparse(size)))
  cl[["element"]] <- c("edges", "nodes")
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param alpha Atomic character vector, indicating which alpha to assign to
#' the selected elements.
#' @rdname all_fun
alpha_pos <- function(data, alpha = 1, ...){
  these_elements <- c("edges", "nodes")
  for(el in these_elements){
    if(!"alpha" %in% names(data[[el]])){
      data[[el]]$alpha <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["expr"]] <- str2lang(paste0("alpha = ", deparse(alpha)))
  cl[["element"]] <- c("edges", "nodes")
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param fill Atomic character vector, indicating which fill to assign to
#' the selected elements.
#' @rdname all_fun
fill_pos <- function(data, fill = "white", ...){
  these_elements <- "nodes"
  for(el in these_elements){
    if(!"fill" %in% names(data[[el]])){
      data[[el]]$fill <- "white"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["expr"]] <- str2lang(paste0("fill = ", deparse(fill)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_colour Atomic character vector, indicating which label_colour to assign to
#' the selected elements.
#' @rdname all_fun
label_colour_pos <- function(data, label_colour = "black", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_colour" %in% names(data[[el]])){
      data[[el]]$label_colour <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["expr"]] <- str2lang(paste0("label_colour = ", deparse(label_colour)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_color Atomic character vector, indicating which label_color to assign to
#' the selected elements.
#' @rdname all_fun
label_color_pos <- function(data, label_color = "black", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_color" %in% names(data[[el]])){
      data[[el]]$label_color <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["expr"]] <- str2lang(paste0("label_color = ", deparse(label_color)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_fill Atomic character vector, indicating which label_fill to assign to
#' the selected elements.
#' @rdname all_fun
label_fill_pos <- function(data, label_fill = "white", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_fill" %in% names(data[[el]])){
      data[[el]]$label_fill <- "white"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["expr"]] <- str2lang(paste0("label_fill = ", deparse(label_fill)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_size Atomic character vector, indicating which label_size to assign to
#' the selected elements.
#' @rdname all_fun
label_size_pos <- function(data, label_size = 4, ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_size" %in% names(data[[el]])){
      data[[el]]$label_size <- 4
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["expr"]] <- str2lang(paste0("label_size = ", deparse(label_size)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_alpha Atomic character vector, indicating which label_alpha to assign to
#' the selected elements.
#' @rdname all_fun
label_alpha_pos <- function(data, label_alpha = 1, ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_alpha" %in% names(data[[el]])){
      data[[el]]$label_alpha <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["expr"]] <- str2lang(paste0("label_alpha = ", deparse(label_alpha)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_family Atomic character vector, indicating which label_family to assign to
#' the selected elements.
#' @rdname all_fun
label_family_pos <- function(data, label_family = "sans", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_family" %in% names(data[[el]])){
      data[[el]]$label_family <- "sans"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["expr"]] <- str2lang(paste0("label_family = ", deparse(label_family)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_fontface Atomic character vector, indicating which label_fontface to assign to
#' the selected elements.
#' @rdname all_fun
label_fontface_pos <- function(data, label_fontface = "plain", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_fontface" %in% names(data[[el]])){
      data[[el]]$label_fontface <- "plain"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["expr"]] <- str2lang(paste0("label_fontface = ", deparse(label_fontface)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_hjust Atomic character vector, indicating which label_hjust to assign to
#' the selected elements.
#' @rdname all_fun
label_hjust_pos <- function(data, label_hjust = "center", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_hjust" %in% names(data[[el]])){
      data[[el]]$label_hjust <- "center"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["expr"]] <- str2lang(paste0("label_hjust = ", deparse(label_hjust)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_vjust Atomic character vector, indicating which label_vjust to assign to
#' the selected elements.
#' @rdname all_fun
label_vjust_pos <- function(data, label_vjust = "middle", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_vjust" %in% names(data[[el]])){
      data[[el]]$label_vjust <- "middle"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["expr"]] <- str2lang(paste0("label_vjust = ", deparse(label_vjust)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_lineheight Atomic character vector, indicating which label_lineheight to assign to
#' the selected elements.
#' @rdname all_fun
label_lineheight_pos <- function(data, label_lineheight = 1, ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_lineheight" %in% names(data[[el]])){
      data[[el]]$label_lineheight <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["expr"]] <- str2lang(paste0("label_lineheight = ", deparse(label_lineheight)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_location Atomic character vector, indicating which label_location to assign to
#' the selected elements.
#' @rdname all_fun
label_location_pos <- function(data, label_location = .5, ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_location" %in% names(data[[el]])){
      data[[el]]$label_location <- .5
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["expr"]] <- str2lang(paste0("label_location = ", deparse(label_location)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
all_neg <- function(data, expr, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["element"]] = c("edges", "nodes")
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
hide_neg <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["expr"]] <- quote({ show = FALSE })
  cl[["element"]] = c("edges", "nodes")
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
show_neg <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["expr"]] <- quote({ show = TRUE })
  cl[["element"]] = c("edges", "nodes")
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param colour Atomic character vector, indicating which colour to assign to
#' the selected elements.
#' @rdname all_fun
colour_neg <- function(data, colour = "black", ...){
  these_elements <- c("edges", "nodes")
  for(el in these_elements){
    if(!"colour" %in% names(data[[el]])){
      data[[el]]$colour <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["expr"]] <- str2lang(paste0("colour = ", deparse(colour)))
  cl[["element"]] <- c("edges", "nodes")
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param color Atomic character vector, indicating which color to assign to
#' the selected elements.
#' @rdname all_fun
color_neg <- function(data, color = "black", ...){
  these_elements <- c("edges", "nodes")
  for(el in these_elements){
    if(!"color" %in% names(data[[el]])){
      data[[el]]$color <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["expr"]] <- str2lang(paste0("color = ", deparse(color)))
  cl[["element"]] <- c("edges", "nodes")
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param linetype Atomic character vector, indicating which linetype to assign to
#' the selected elements.
#' @rdname all_fun
linetype_neg <- function(data, linetype = 1, ...){
  these_elements <- c("edges", "nodes")
  for(el in these_elements){
    if(!"linetype" %in% names(data[[el]])){
      data[[el]]$linetype <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["expr"]] <- str2lang(paste0("linetype = ", deparse(linetype)))
  cl[["element"]] <- c("edges", "nodes")
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param size Atomic character vector, indicating which size to assign to
#' the selected elements.
#' @rdname all_fun
size_neg <- function(data, size = 1, ...){
  these_elements <- c("edges", "nodes")
  for(el in these_elements){
    if(!"size" %in% names(data[[el]])){
      data[[el]]$size <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["expr"]] <- str2lang(paste0("size = ", deparse(size)))
  cl[["element"]] <- c("edges", "nodes")
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param alpha Atomic character vector, indicating which alpha to assign to
#' the selected elements.
#' @rdname all_fun
alpha_neg <- function(data, alpha = 1, ...){
  these_elements <- c("edges", "nodes")
  for(el in these_elements){
    if(!"alpha" %in% names(data[[el]])){
      data[[el]]$alpha <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["expr"]] <- str2lang(paste0("alpha = ", deparse(alpha)))
  cl[["element"]] <- c("edges", "nodes")
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param fill Atomic character vector, indicating which fill to assign to
#' the selected elements.
#' @rdname all_fun
fill_neg <- function(data, fill = "white", ...){
  these_elements <- "nodes"
  for(el in these_elements){
    if(!"fill" %in% names(data[[el]])){
      data[[el]]$fill <- "white"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["expr"]] <- str2lang(paste0("fill = ", deparse(fill)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_colour Atomic character vector, indicating which label_colour to assign to
#' the selected elements.
#' @rdname all_fun
label_colour_neg <- function(data, label_colour = "black", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_colour" %in% names(data[[el]])){
      data[[el]]$label_colour <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["expr"]] <- str2lang(paste0("label_colour = ", deparse(label_colour)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_color Atomic character vector, indicating which label_color to assign to
#' the selected elements.
#' @rdname all_fun
label_color_neg <- function(data, label_color = "black", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_color" %in% names(data[[el]])){
      data[[el]]$label_color <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["expr"]] <- str2lang(paste0("label_color = ", deparse(label_color)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_fill Atomic character vector, indicating which label_fill to assign to
#' the selected elements.
#' @rdname all_fun
label_fill_neg <- function(data, label_fill = "white", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_fill" %in% names(data[[el]])){
      data[[el]]$label_fill <- "white"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["expr"]] <- str2lang(paste0("label_fill = ", deparse(label_fill)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_size Atomic character vector, indicating which label_size to assign to
#' the selected elements.
#' @rdname all_fun
label_size_neg <- function(data, label_size = 4, ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_size" %in% names(data[[el]])){
      data[[el]]$label_size <- 4
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["expr"]] <- str2lang(paste0("label_size = ", deparse(label_size)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_alpha Atomic character vector, indicating which label_alpha to assign to
#' the selected elements.
#' @rdname all_fun
label_alpha_neg <- function(data, label_alpha = 1, ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_alpha" %in% names(data[[el]])){
      data[[el]]$label_alpha <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["expr"]] <- str2lang(paste0("label_alpha = ", deparse(label_alpha)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_family Atomic character vector, indicating which label_family to assign to
#' the selected elements.
#' @rdname all_fun
label_family_neg <- function(data, label_family = "sans", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_family" %in% names(data[[el]])){
      data[[el]]$label_family <- "sans"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["expr"]] <- str2lang(paste0("label_family = ", deparse(label_family)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_fontface Atomic character vector, indicating which label_fontface to assign to
#' the selected elements.
#' @rdname all_fun
label_fontface_neg <- function(data, label_fontface = "plain", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_fontface" %in% names(data[[el]])){
      data[[el]]$label_fontface <- "plain"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["expr"]] <- str2lang(paste0("label_fontface = ", deparse(label_fontface)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_hjust Atomic character vector, indicating which label_hjust to assign to
#' the selected elements.
#' @rdname all_fun
label_hjust_neg <- function(data, label_hjust = "center", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_hjust" %in% names(data[[el]])){
      data[[el]]$label_hjust <- "center"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["expr"]] <- str2lang(paste0("label_hjust = ", deparse(label_hjust)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_vjust Atomic character vector, indicating which label_vjust to assign to
#' the selected elements.
#' @rdname all_fun
label_vjust_neg <- function(data, label_vjust = "middle", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_vjust" %in% names(data[[el]])){
      data[[el]]$label_vjust <- "middle"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["expr"]] <- str2lang(paste0("label_vjust = ", deparse(label_vjust)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_lineheight Atomic character vector, indicating which label_lineheight to assign to
#' the selected elements.
#' @rdname all_fun
label_lineheight_neg <- function(data, label_lineheight = 1, ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_lineheight" %in% names(data[[el]])){
      data[[el]]$label_lineheight <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["expr"]] <- str2lang(paste0("label_lineheight = ", deparse(label_lineheight)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_location Atomic character vector, indicating which label_location to assign to
#' the selected elements.
#' @rdname all_fun
label_location_neg <- function(data, label_location = .5, ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_location" %in% names(data[[el]])){
      data[[el]]$label_location <- .5
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["expr"]] <- str2lang(paste0("label_location = ", deparse(label_location)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
all_var <- function(data, expr, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(lhs == rhs & op == "~~")
  cl[["element"]] = "edges"
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
hide_var <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(lhs == rhs & op == "~~")
  cl[["expr"]] <- quote({ show = FALSE })
  cl[["element"]] = "edges"
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
show_var <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(lhs == rhs & op == "~~")
  cl[["expr"]] <- quote({ show = TRUE })
  cl[["element"]] = "edges"
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param colour Atomic character vector, indicating which colour to assign to
#' the selected elements.
#' @rdname all_fun
colour_var <- function(data, colour = "black", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"colour" %in% names(data[[el]])){
      data[[el]]$colour <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(lhs == rhs & op == "~~")
  cl[["expr"]] <- str2lang(paste0("colour = ", deparse(colour)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param color Atomic character vector, indicating which color to assign to
#' the selected elements.
#' @rdname all_fun
color_var <- function(data, color = "black", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"color" %in% names(data[[el]])){
      data[[el]]$color <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(lhs == rhs & op == "~~")
  cl[["expr"]] <- str2lang(paste0("color = ", deparse(color)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param linetype Atomic character vector, indicating which linetype to assign to
#' the selected elements.
#' @rdname all_fun
linetype_var <- function(data, linetype = 1, ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"linetype" %in% names(data[[el]])){
      data[[el]]$linetype <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(lhs == rhs & op == "~~")
  cl[["expr"]] <- str2lang(paste0("linetype = ", deparse(linetype)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param size Atomic character vector, indicating which size to assign to
#' the selected elements.
#' @rdname all_fun
size_var <- function(data, size = 1, ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"size" %in% names(data[[el]])){
      data[[el]]$size <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(lhs == rhs & op == "~~")
  cl[["expr"]] <- str2lang(paste0("size = ", deparse(size)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param alpha Atomic character vector, indicating which alpha to assign to
#' the selected elements.
#' @rdname all_fun
alpha_var <- function(data, alpha = 1, ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"alpha" %in% names(data[[el]])){
      data[[el]]$alpha <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(lhs == rhs & op == "~~")
  cl[["expr"]] <- str2lang(paste0("alpha = ", deparse(alpha)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_colour Atomic character vector, indicating which label_colour to assign to
#' the selected elements.
#' @rdname all_fun
label_colour_var <- function(data, label_colour = "black", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_colour" %in% names(data[[el]])){
      data[[el]]$label_colour <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(lhs == rhs & op == "~~")
  cl[["expr"]] <- str2lang(paste0("label_colour = ", deparse(label_colour)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_color Atomic character vector, indicating which label_color to assign to
#' the selected elements.
#' @rdname all_fun
label_color_var <- function(data, label_color = "black", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_color" %in% names(data[[el]])){
      data[[el]]$label_color <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(lhs == rhs & op == "~~")
  cl[["expr"]] <- str2lang(paste0("label_color = ", deparse(label_color)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_fill Atomic character vector, indicating which label_fill to assign to
#' the selected elements.
#' @rdname all_fun
label_fill_var <- function(data, label_fill = "white", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_fill" %in% names(data[[el]])){
      data[[el]]$label_fill <- "white"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(lhs == rhs & op == "~~")
  cl[["expr"]] <- str2lang(paste0("label_fill = ", deparse(label_fill)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_size Atomic character vector, indicating which label_size to assign to
#' the selected elements.
#' @rdname all_fun
label_size_var <- function(data, label_size = 4, ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_size" %in% names(data[[el]])){
      data[[el]]$label_size <- 4
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(lhs == rhs & op == "~~")
  cl[["expr"]] <- str2lang(paste0("label_size = ", deparse(label_size)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_alpha Atomic character vector, indicating which label_alpha to assign to
#' the selected elements.
#' @rdname all_fun
label_alpha_var <- function(data, label_alpha = 1, ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_alpha" %in% names(data[[el]])){
      data[[el]]$label_alpha <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(lhs == rhs & op == "~~")
  cl[["expr"]] <- str2lang(paste0("label_alpha = ", deparse(label_alpha)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_family Atomic character vector, indicating which label_family to assign to
#' the selected elements.
#' @rdname all_fun
label_family_var <- function(data, label_family = "sans", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_family" %in% names(data[[el]])){
      data[[el]]$label_family <- "sans"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(lhs == rhs & op == "~~")
  cl[["expr"]] <- str2lang(paste0("label_family = ", deparse(label_family)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_fontface Atomic character vector, indicating which label_fontface to assign to
#' the selected elements.
#' @rdname all_fun
label_fontface_var <- function(data, label_fontface = "plain", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_fontface" %in% names(data[[el]])){
      data[[el]]$label_fontface <- "plain"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(lhs == rhs & op == "~~")
  cl[["expr"]] <- str2lang(paste0("label_fontface = ", deparse(label_fontface)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_hjust Atomic character vector, indicating which label_hjust to assign to
#' the selected elements.
#' @rdname all_fun
label_hjust_var <- function(data, label_hjust = "center", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_hjust" %in% names(data[[el]])){
      data[[el]]$label_hjust <- "center"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(lhs == rhs & op == "~~")
  cl[["expr"]] <- str2lang(paste0("label_hjust = ", deparse(label_hjust)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_vjust Atomic character vector, indicating which label_vjust to assign to
#' the selected elements.
#' @rdname all_fun
label_vjust_var <- function(data, label_vjust = "middle", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_vjust" %in% names(data[[el]])){
      data[[el]]$label_vjust <- "middle"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(lhs == rhs & op == "~~")
  cl[["expr"]] <- str2lang(paste0("label_vjust = ", deparse(label_vjust)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_lineheight Atomic character vector, indicating which label_lineheight to assign to
#' the selected elements.
#' @rdname all_fun
label_lineheight_var <- function(data, label_lineheight = 1, ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_lineheight" %in% names(data[[el]])){
      data[[el]]$label_lineheight <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(lhs == rhs & op == "~~")
  cl[["expr"]] <- str2lang(paste0("label_lineheight = ", deparse(label_lineheight)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
all_cov <- function(data, expr, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(lhs != rhs & op == "~~")
  cl[["element"]] = "edges"
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
hide_cov <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(lhs != rhs & op == "~~")
  cl[["expr"]] <- quote({ show = FALSE })
  cl[["element"]] = "edges"
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
show_cov <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(lhs != rhs & op == "~~")
  cl[["expr"]] <- quote({ show = TRUE })
  cl[["element"]] = "edges"
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param colour Atomic character vector, indicating which colour to assign to
#' the selected elements.
#' @rdname all_fun
colour_cov <- function(data, colour = "black", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"colour" %in% names(data[[el]])){
      data[[el]]$colour <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(lhs != rhs & op == "~~")
  cl[["expr"]] <- str2lang(paste0("colour = ", deparse(colour)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param color Atomic character vector, indicating which color to assign to
#' the selected elements.
#' @rdname all_fun
color_cov <- function(data, color = "black", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"color" %in% names(data[[el]])){
      data[[el]]$color <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(lhs != rhs & op == "~~")
  cl[["expr"]] <- str2lang(paste0("color = ", deparse(color)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param linetype Atomic character vector, indicating which linetype to assign to
#' the selected elements.
#' @rdname all_fun
linetype_cov <- function(data, linetype = 1, ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"linetype" %in% names(data[[el]])){
      data[[el]]$linetype <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(lhs != rhs & op == "~~")
  cl[["expr"]] <- str2lang(paste0("linetype = ", deparse(linetype)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param size Atomic character vector, indicating which size to assign to
#' the selected elements.
#' @rdname all_fun
size_cov <- function(data, size = 1, ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"size" %in% names(data[[el]])){
      data[[el]]$size <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(lhs != rhs & op == "~~")
  cl[["expr"]] <- str2lang(paste0("size = ", deparse(size)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param alpha Atomic character vector, indicating which alpha to assign to
#' the selected elements.
#' @rdname all_fun
alpha_cov <- function(data, alpha = 1, ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"alpha" %in% names(data[[el]])){
      data[[el]]$alpha <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(lhs != rhs & op == "~~")
  cl[["expr"]] <- str2lang(paste0("alpha = ", deparse(alpha)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_colour Atomic character vector, indicating which label_colour to assign to
#' the selected elements.
#' @rdname all_fun
label_colour_cov <- function(data, label_colour = "black", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_colour" %in% names(data[[el]])){
      data[[el]]$label_colour <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(lhs != rhs & op == "~~")
  cl[["expr"]] <- str2lang(paste0("label_colour = ", deparse(label_colour)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_color Atomic character vector, indicating which label_color to assign to
#' the selected elements.
#' @rdname all_fun
label_color_cov <- function(data, label_color = "black", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_color" %in% names(data[[el]])){
      data[[el]]$label_color <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(lhs != rhs & op == "~~")
  cl[["expr"]] <- str2lang(paste0("label_color = ", deparse(label_color)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_fill Atomic character vector, indicating which label_fill to assign to
#' the selected elements.
#' @rdname all_fun
label_fill_cov <- function(data, label_fill = "white", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_fill" %in% names(data[[el]])){
      data[[el]]$label_fill <- "white"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(lhs != rhs & op == "~~")
  cl[["expr"]] <- str2lang(paste0("label_fill = ", deparse(label_fill)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_size Atomic character vector, indicating which label_size to assign to
#' the selected elements.
#' @rdname all_fun
label_size_cov <- function(data, label_size = 4, ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_size" %in% names(data[[el]])){
      data[[el]]$label_size <- 4
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(lhs != rhs & op == "~~")
  cl[["expr"]] <- str2lang(paste0("label_size = ", deparse(label_size)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_alpha Atomic character vector, indicating which label_alpha to assign to
#' the selected elements.
#' @rdname all_fun
label_alpha_cov <- function(data, label_alpha = 1, ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_alpha" %in% names(data[[el]])){
      data[[el]]$label_alpha <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(lhs != rhs & op == "~~")
  cl[["expr"]] <- str2lang(paste0("label_alpha = ", deparse(label_alpha)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_family Atomic character vector, indicating which label_family to assign to
#' the selected elements.
#' @rdname all_fun
label_family_cov <- function(data, label_family = "sans", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_family" %in% names(data[[el]])){
      data[[el]]$label_family <- "sans"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(lhs != rhs & op == "~~")
  cl[["expr"]] <- str2lang(paste0("label_family = ", deparse(label_family)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_fontface Atomic character vector, indicating which label_fontface to assign to
#' the selected elements.
#' @rdname all_fun
label_fontface_cov <- function(data, label_fontface = "plain", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_fontface" %in% names(data[[el]])){
      data[[el]]$label_fontface <- "plain"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(lhs != rhs & op == "~~")
  cl[["expr"]] <- str2lang(paste0("label_fontface = ", deparse(label_fontface)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_hjust Atomic character vector, indicating which label_hjust to assign to
#' the selected elements.
#' @rdname all_fun
label_hjust_cov <- function(data, label_hjust = "center", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_hjust" %in% names(data[[el]])){
      data[[el]]$label_hjust <- "center"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(lhs != rhs & op == "~~")
  cl[["expr"]] <- str2lang(paste0("label_hjust = ", deparse(label_hjust)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_vjust Atomic character vector, indicating which label_vjust to assign to
#' the selected elements.
#' @rdname all_fun
label_vjust_cov <- function(data, label_vjust = "middle", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_vjust" %in% names(data[[el]])){
      data[[el]]$label_vjust <- "middle"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(lhs != rhs & op == "~~")
  cl[["expr"]] <- str2lang(paste0("label_vjust = ", deparse(label_vjust)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_lineheight Atomic character vector, indicating which label_lineheight to assign to
#' the selected elements.
#' @rdname all_fun
label_lineheight_cov <- function(data, label_lineheight = 1, ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_lineheight" %in% names(data[[el]])){
      data[[el]]$label_lineheight <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(lhs != rhs & op == "~~")
  cl[["expr"]] <- str2lang(paste0("label_lineheight = ", deparse(label_lineheight)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_location Atomic character vector, indicating which label_location to assign to
#' the selected elements.
#' @rdname all_fun
label_location_cov <- function(data, label_location = .5, ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_location" %in% names(data[[el]])){
      data[[el]]$label_location <- .5
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(lhs != rhs & op == "~~")
  cl[["expr"]] <- str2lang(paste0("label_location = ", deparse(label_location)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
all_reg <- function(data, expr, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(op == "~")
  cl[["element"]] = "edges"
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
hide_reg <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(op == "~")
  cl[["expr"]] <- quote({ show = FALSE })
  cl[["element"]] = "edges"
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
show_reg <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(op == "~")
  cl[["expr"]] <- quote({ show = TRUE })
  cl[["element"]] = "edges"
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param colour Atomic character vector, indicating which colour to assign to
#' the selected elements.
#' @rdname all_fun
colour_reg <- function(data, colour = "black", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"colour" %in% names(data[[el]])){
      data[[el]]$colour <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "~")
  cl[["expr"]] <- str2lang(paste0("colour = ", deparse(colour)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param color Atomic character vector, indicating which color to assign to
#' the selected elements.
#' @rdname all_fun
color_reg <- function(data, color = "black", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"color" %in% names(data[[el]])){
      data[[el]]$color <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "~")
  cl[["expr"]] <- str2lang(paste0("color = ", deparse(color)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param linetype Atomic character vector, indicating which linetype to assign to
#' the selected elements.
#' @rdname all_fun
linetype_reg <- function(data, linetype = 1, ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"linetype" %in% names(data[[el]])){
      data[[el]]$linetype <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "~")
  cl[["expr"]] <- str2lang(paste0("linetype = ", deparse(linetype)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param size Atomic character vector, indicating which size to assign to
#' the selected elements.
#' @rdname all_fun
size_reg <- function(data, size = 1, ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"size" %in% names(data[[el]])){
      data[[el]]$size <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "~")
  cl[["expr"]] <- str2lang(paste0("size = ", deparse(size)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param alpha Atomic character vector, indicating which alpha to assign to
#' the selected elements.
#' @rdname all_fun
alpha_reg <- function(data, alpha = 1, ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"alpha" %in% names(data[[el]])){
      data[[el]]$alpha <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "~")
  cl[["expr"]] <- str2lang(paste0("alpha = ", deparse(alpha)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_colour Atomic character vector, indicating which label_colour to assign to
#' the selected elements.
#' @rdname all_fun
label_colour_reg <- function(data, label_colour = "black", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_colour" %in% names(data[[el]])){
      data[[el]]$label_colour <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "~")
  cl[["expr"]] <- str2lang(paste0("label_colour = ", deparse(label_colour)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_color Atomic character vector, indicating which label_color to assign to
#' the selected elements.
#' @rdname all_fun
label_color_reg <- function(data, label_color = "black", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_color" %in% names(data[[el]])){
      data[[el]]$label_color <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "~")
  cl[["expr"]] <- str2lang(paste0("label_color = ", deparse(label_color)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_fill Atomic character vector, indicating which label_fill to assign to
#' the selected elements.
#' @rdname all_fun
label_fill_reg <- function(data, label_fill = "white", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_fill" %in% names(data[[el]])){
      data[[el]]$label_fill <- "white"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "~")
  cl[["expr"]] <- str2lang(paste0("label_fill = ", deparse(label_fill)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_size Atomic character vector, indicating which label_size to assign to
#' the selected elements.
#' @rdname all_fun
label_size_reg <- function(data, label_size = 4, ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_size" %in% names(data[[el]])){
      data[[el]]$label_size <- 4
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "~")
  cl[["expr"]] <- str2lang(paste0("label_size = ", deparse(label_size)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_alpha Atomic character vector, indicating which label_alpha to assign to
#' the selected elements.
#' @rdname all_fun
label_alpha_reg <- function(data, label_alpha = 1, ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_alpha" %in% names(data[[el]])){
      data[[el]]$label_alpha <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "~")
  cl[["expr"]] <- str2lang(paste0("label_alpha = ", deparse(label_alpha)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_family Atomic character vector, indicating which label_family to assign to
#' the selected elements.
#' @rdname all_fun
label_family_reg <- function(data, label_family = "sans", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_family" %in% names(data[[el]])){
      data[[el]]$label_family <- "sans"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "~")
  cl[["expr"]] <- str2lang(paste0("label_family = ", deparse(label_family)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_fontface Atomic character vector, indicating which label_fontface to assign to
#' the selected elements.
#' @rdname all_fun
label_fontface_reg <- function(data, label_fontface = "plain", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_fontface" %in% names(data[[el]])){
      data[[el]]$label_fontface <- "plain"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "~")
  cl[["expr"]] <- str2lang(paste0("label_fontface = ", deparse(label_fontface)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_hjust Atomic character vector, indicating which label_hjust to assign to
#' the selected elements.
#' @rdname all_fun
label_hjust_reg <- function(data, label_hjust = "center", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_hjust" %in% names(data[[el]])){
      data[[el]]$label_hjust <- "center"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "~")
  cl[["expr"]] <- str2lang(paste0("label_hjust = ", deparse(label_hjust)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_vjust Atomic character vector, indicating which label_vjust to assign to
#' the selected elements.
#' @rdname all_fun
label_vjust_reg <- function(data, label_vjust = "middle", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_vjust" %in% names(data[[el]])){
      data[[el]]$label_vjust <- "middle"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "~")
  cl[["expr"]] <- str2lang(paste0("label_vjust = ", deparse(label_vjust)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_lineheight Atomic character vector, indicating which label_lineheight to assign to
#' the selected elements.
#' @rdname all_fun
label_lineheight_reg <- function(data, label_lineheight = 1, ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_lineheight" %in% names(data[[el]])){
      data[[el]]$label_lineheight <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "~")
  cl[["expr"]] <- str2lang(paste0("label_lineheight = ", deparse(label_lineheight)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_location Atomic character vector, indicating which label_location to assign to
#' the selected elements.
#' @rdname all_fun
label_location_reg <- function(data, label_location = .5, ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_location" %in% names(data[[el]])){
      data[[el]]$label_location <- .5
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "~")
  cl[["expr"]] <- str2lang(paste0("label_location = ", deparse(label_location)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
all_load <- function(data, expr, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(op == "=~")
  cl[["element"]] = "edges"
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
hide_load <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(op == "=~")
  cl[["expr"]] <- quote({ show = FALSE })
  cl[["element"]] = "edges"
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
show_load <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(op == "=~")
  cl[["expr"]] <- quote({ show = TRUE })
  cl[["element"]] = "edges"
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param colour Atomic character vector, indicating which colour to assign to
#' the selected elements.
#' @rdname all_fun
colour_load <- function(data, colour = "black", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"colour" %in% names(data[[el]])){
      data[[el]]$colour <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "=~")
  cl[["expr"]] <- str2lang(paste0("colour = ", deparse(colour)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param color Atomic character vector, indicating which color to assign to
#' the selected elements.
#' @rdname all_fun
color_load <- function(data, color = "black", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"color" %in% names(data[[el]])){
      data[[el]]$color <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "=~")
  cl[["expr"]] <- str2lang(paste0("color = ", deparse(color)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param linetype Atomic character vector, indicating which linetype to assign to
#' the selected elements.
#' @rdname all_fun
linetype_load <- function(data, linetype = 1, ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"linetype" %in% names(data[[el]])){
      data[[el]]$linetype <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "=~")
  cl[["expr"]] <- str2lang(paste0("linetype = ", deparse(linetype)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param size Atomic character vector, indicating which size to assign to
#' the selected elements.
#' @rdname all_fun
size_load <- function(data, size = 1, ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"size" %in% names(data[[el]])){
      data[[el]]$size <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "=~")
  cl[["expr"]] <- str2lang(paste0("size = ", deparse(size)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param alpha Atomic character vector, indicating which alpha to assign to
#' the selected elements.
#' @rdname all_fun
alpha_load <- function(data, alpha = 1, ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"alpha" %in% names(data[[el]])){
      data[[el]]$alpha <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "=~")
  cl[["expr"]] <- str2lang(paste0("alpha = ", deparse(alpha)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_colour Atomic character vector, indicating which label_colour to assign to
#' the selected elements.
#' @rdname all_fun
label_colour_load <- function(data, label_colour = "black", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_colour" %in% names(data[[el]])){
      data[[el]]$label_colour <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "=~")
  cl[["expr"]] <- str2lang(paste0("label_colour = ", deparse(label_colour)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_color Atomic character vector, indicating which label_color to assign to
#' the selected elements.
#' @rdname all_fun
label_color_load <- function(data, label_color = "black", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_color" %in% names(data[[el]])){
      data[[el]]$label_color <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "=~")
  cl[["expr"]] <- str2lang(paste0("label_color = ", deparse(label_color)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_fill Atomic character vector, indicating which label_fill to assign to
#' the selected elements.
#' @rdname all_fun
label_fill_load <- function(data, label_fill = "white", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_fill" %in% names(data[[el]])){
      data[[el]]$label_fill <- "white"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "=~")
  cl[["expr"]] <- str2lang(paste0("label_fill = ", deparse(label_fill)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_size Atomic character vector, indicating which label_size to assign to
#' the selected elements.
#' @rdname all_fun
label_size_load <- function(data, label_size = 4, ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_size" %in% names(data[[el]])){
      data[[el]]$label_size <- 4
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "=~")
  cl[["expr"]] <- str2lang(paste0("label_size = ", deparse(label_size)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_alpha Atomic character vector, indicating which label_alpha to assign to
#' the selected elements.
#' @rdname all_fun
label_alpha_load <- function(data, label_alpha = 1, ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_alpha" %in% names(data[[el]])){
      data[[el]]$label_alpha <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "=~")
  cl[["expr"]] <- str2lang(paste0("label_alpha = ", deparse(label_alpha)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_family Atomic character vector, indicating which label_family to assign to
#' the selected elements.
#' @rdname all_fun
label_family_load <- function(data, label_family = "sans", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_family" %in% names(data[[el]])){
      data[[el]]$label_family <- "sans"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "=~")
  cl[["expr"]] <- str2lang(paste0("label_family = ", deparse(label_family)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_fontface Atomic character vector, indicating which label_fontface to assign to
#' the selected elements.
#' @rdname all_fun
label_fontface_load <- function(data, label_fontface = "plain", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_fontface" %in% names(data[[el]])){
      data[[el]]$label_fontface <- "plain"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "=~")
  cl[["expr"]] <- str2lang(paste0("label_fontface = ", deparse(label_fontface)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_hjust Atomic character vector, indicating which label_hjust to assign to
#' the selected elements.
#' @rdname all_fun
label_hjust_load <- function(data, label_hjust = "center", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_hjust" %in% names(data[[el]])){
      data[[el]]$label_hjust <- "center"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "=~")
  cl[["expr"]] <- str2lang(paste0("label_hjust = ", deparse(label_hjust)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_vjust Atomic character vector, indicating which label_vjust to assign to
#' the selected elements.
#' @rdname all_fun
label_vjust_load <- function(data, label_vjust = "middle", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_vjust" %in% names(data[[el]])){
      data[[el]]$label_vjust <- "middle"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "=~")
  cl[["expr"]] <- str2lang(paste0("label_vjust = ", deparse(label_vjust)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_lineheight Atomic character vector, indicating which label_lineheight to assign to
#' the selected elements.
#' @rdname all_fun
label_lineheight_load <- function(data, label_lineheight = 1, ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_lineheight" %in% names(data[[el]])){
      data[[el]]$label_lineheight <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "=~")
  cl[["expr"]] <- str2lang(paste0("label_lineheight = ", deparse(label_lineheight)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_location Atomic character vector, indicating which label_location to assign to
#' the selected elements.
#' @rdname all_fun
label_location_load <- function(data, label_location = .5, ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_location" %in% names(data[[el]])){
      data[[el]]$label_location <- .5
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "=~")
  cl[["expr"]] <- str2lang(paste0("label_location = ", deparse(label_location)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
all_obs <- function(data, expr, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(shape == "rect")
  cl[["element"]] = "nodes"
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
hide_obs <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(shape == "rect")
  cl[["expr"]] <- quote({ show = FALSE })
  cl[["element"]] = "nodes"
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
show_obs <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(shape == "rect")
  cl[["expr"]] <- quote({ show = TRUE })
  cl[["element"]] = "nodes"
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param colour Atomic character vector, indicating which colour to assign to
#' the selected elements.
#' @rdname all_fun
colour_obs <- function(data, colour = "black", ...){
  these_elements <- "nodes"
  for(el in these_elements){
    if(!"colour" %in% names(data[[el]])){
      data[[el]]$colour <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(shape == "rect")
  cl[["expr"]] <- str2lang(paste0("colour = ", deparse(colour)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param color Atomic character vector, indicating which color to assign to
#' the selected elements.
#' @rdname all_fun
color_obs <- function(data, color = "black", ...){
  these_elements <- "nodes"
  for(el in these_elements){
    if(!"color" %in% names(data[[el]])){
      data[[el]]$color <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(shape == "rect")
  cl[["expr"]] <- str2lang(paste0("color = ", deparse(color)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param linetype Atomic character vector, indicating which linetype to assign to
#' the selected elements.
#' @rdname all_fun
linetype_obs <- function(data, linetype = 1, ...){
  these_elements <- "nodes"
  for(el in these_elements){
    if(!"linetype" %in% names(data[[el]])){
      data[[el]]$linetype <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(shape == "rect")
  cl[["expr"]] <- str2lang(paste0("linetype = ", deparse(linetype)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param size Atomic character vector, indicating which size to assign to
#' the selected elements.
#' @rdname all_fun
size_obs <- function(data, size = 1, ...){
  these_elements <- "nodes"
  for(el in these_elements){
    if(!"size" %in% names(data[[el]])){
      data[[el]]$size <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(shape == "rect")
  cl[["expr"]] <- str2lang(paste0("size = ", deparse(size)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param alpha Atomic character vector, indicating which alpha to assign to
#' the selected elements.
#' @rdname all_fun
alpha_obs <- function(data, alpha = 1, ...){
  these_elements <- "nodes"
  for(el in these_elements){
    if(!"alpha" %in% names(data[[el]])){
      data[[el]]$alpha <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(shape == "rect")
  cl[["expr"]] <- str2lang(paste0("alpha = ", deparse(alpha)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param fill Atomic character vector, indicating which fill to assign to
#' the selected elements.
#' @rdname all_fun
fill_obs <- function(data, fill = "white", ...){
  these_elements <- "nodes"
  for(el in these_elements){
    if(!"fill" %in% names(data[[el]])){
      data[[el]]$fill <- "white"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(shape == "rect")
  cl[["expr"]] <- str2lang(paste0("fill = ", deparse(fill)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_colour Atomic character vector, indicating which label_colour to assign to
#' the selected elements.
#' @rdname all_fun
label_colour_obs <- function(data, label_colour = "black", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_colour" %in% names(data[[el]])){
      data[[el]]$label_colour <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(shape == "rect")
  cl[["expr"]] <- str2lang(paste0("label_colour = ", deparse(label_colour)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_color Atomic character vector, indicating which label_color to assign to
#' the selected elements.
#' @rdname all_fun
label_color_obs <- function(data, label_color = "black", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_color" %in% names(data[[el]])){
      data[[el]]$label_color <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(shape == "rect")
  cl[["expr"]] <- str2lang(paste0("label_color = ", deparse(label_color)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_fill Atomic character vector, indicating which label_fill to assign to
#' the selected elements.
#' @rdname all_fun
label_fill_obs <- function(data, label_fill = "white", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_fill" %in% names(data[[el]])){
      data[[el]]$label_fill <- "white"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(shape == "rect")
  cl[["expr"]] <- str2lang(paste0("label_fill = ", deparse(label_fill)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_size Atomic character vector, indicating which label_size to assign to
#' the selected elements.
#' @rdname all_fun
label_size_obs <- function(data, label_size = 4, ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_size" %in% names(data[[el]])){
      data[[el]]$label_size <- 4
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(shape == "rect")
  cl[["expr"]] <- str2lang(paste0("label_size = ", deparse(label_size)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_alpha Atomic character vector, indicating which label_alpha to assign to
#' the selected elements.
#' @rdname all_fun
label_alpha_obs <- function(data, label_alpha = 1, ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_alpha" %in% names(data[[el]])){
      data[[el]]$label_alpha <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(shape == "rect")
  cl[["expr"]] <- str2lang(paste0("label_alpha = ", deparse(label_alpha)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_family Atomic character vector, indicating which label_family to assign to
#' the selected elements.
#' @rdname all_fun
label_family_obs <- function(data, label_family = "sans", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_family" %in% names(data[[el]])){
      data[[el]]$label_family <- "sans"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(shape == "rect")
  cl[["expr"]] <- str2lang(paste0("label_family = ", deparse(label_family)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_fontface Atomic character vector, indicating which label_fontface to assign to
#' the selected elements.
#' @rdname all_fun
label_fontface_obs <- function(data, label_fontface = "plain", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_fontface" %in% names(data[[el]])){
      data[[el]]$label_fontface <- "plain"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(shape == "rect")
  cl[["expr"]] <- str2lang(paste0("label_fontface = ", deparse(label_fontface)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_hjust Atomic character vector, indicating which label_hjust to assign to
#' the selected elements.
#' @rdname all_fun
label_hjust_obs <- function(data, label_hjust = "center", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_hjust" %in% names(data[[el]])){
      data[[el]]$label_hjust <- "center"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(shape == "rect")
  cl[["expr"]] <- str2lang(paste0("label_hjust = ", deparse(label_hjust)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_vjust Atomic character vector, indicating which label_vjust to assign to
#' the selected elements.
#' @rdname all_fun
label_vjust_obs <- function(data, label_vjust = "middle", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_vjust" %in% names(data[[el]])){
      data[[el]]$label_vjust <- "middle"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(shape == "rect")
  cl[["expr"]] <- str2lang(paste0("label_vjust = ", deparse(label_vjust)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_lineheight Atomic character vector, indicating which label_lineheight to assign to
#' the selected elements.
#' @rdname all_fun
label_lineheight_obs <- function(data, label_lineheight = 1, ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_lineheight" %in% names(data[[el]])){
      data[[el]]$label_lineheight <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(shape == "rect")
  cl[["expr"]] <- str2lang(paste0("label_lineheight = ", deparse(label_lineheight)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_location Atomic character vector, indicating which label_location to assign to
#' the selected elements.
#' @rdname all_fun
label_location_obs <- function(data, label_location = .5, ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_location" %in% names(data[[el]])){
      data[[el]]$label_location <- .5
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(shape == "rect")
  cl[["expr"]] <- str2lang(paste0("label_location = ", deparse(label_location)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
all_latent <- function(data, expr, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(shape == "oval")
  cl[["element"]] = "nodes"
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
hide_latent <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(shape == "oval")
  cl[["expr"]] <- quote({ show = FALSE })
  cl[["element"]] = "nodes"
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
show_latent <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(shape == "oval")
  cl[["expr"]] <- quote({ show = TRUE })
  cl[["element"]] = "nodes"
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param colour Atomic character vector, indicating which colour to assign to
#' the selected elements.
#' @rdname all_fun
colour_latent <- function(data, colour = "black", ...){
  these_elements <- "nodes"
  for(el in these_elements){
    if(!"colour" %in% names(data[[el]])){
      data[[el]]$colour <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(shape == "oval")
  cl[["expr"]] <- str2lang(paste0("colour = ", deparse(colour)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param color Atomic character vector, indicating which color to assign to
#' the selected elements.
#' @rdname all_fun
color_latent <- function(data, color = "black", ...){
  these_elements <- "nodes"
  for(el in these_elements){
    if(!"color" %in% names(data[[el]])){
      data[[el]]$color <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(shape == "oval")
  cl[["expr"]] <- str2lang(paste0("color = ", deparse(color)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param linetype Atomic character vector, indicating which linetype to assign to
#' the selected elements.
#' @rdname all_fun
linetype_latent <- function(data, linetype = 1, ...){
  these_elements <- "nodes"
  for(el in these_elements){
    if(!"linetype" %in% names(data[[el]])){
      data[[el]]$linetype <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(shape == "oval")
  cl[["expr"]] <- str2lang(paste0("linetype = ", deparse(linetype)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param size Atomic character vector, indicating which size to assign to
#' the selected elements.
#' @rdname all_fun
size_latent <- function(data, size = 1, ...){
  these_elements <- "nodes"
  for(el in these_elements){
    if(!"size" %in% names(data[[el]])){
      data[[el]]$size <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(shape == "oval")
  cl[["expr"]] <- str2lang(paste0("size = ", deparse(size)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param alpha Atomic character vector, indicating which alpha to assign to
#' the selected elements.
#' @rdname all_fun
alpha_latent <- function(data, alpha = 1, ...){
  these_elements <- "nodes"
  for(el in these_elements){
    if(!"alpha" %in% names(data[[el]])){
      data[[el]]$alpha <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(shape == "oval")
  cl[["expr"]] <- str2lang(paste0("alpha = ", deparse(alpha)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param fill Atomic character vector, indicating which fill to assign to
#' the selected elements.
#' @rdname all_fun
fill_latent <- function(data, fill = "white", ...){
  these_elements <- "nodes"
  for(el in these_elements){
    if(!"fill" %in% names(data[[el]])){
      data[[el]]$fill <- "white"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(shape == "oval")
  cl[["expr"]] <- str2lang(paste0("fill = ", deparse(fill)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_colour Atomic character vector, indicating which label_colour to assign to
#' the selected elements.
#' @rdname all_fun
label_colour_latent <- function(data, label_colour = "black", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_colour" %in% names(data[[el]])){
      data[[el]]$label_colour <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(shape == "oval")
  cl[["expr"]] <- str2lang(paste0("label_colour = ", deparse(label_colour)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_color Atomic character vector, indicating which label_color to assign to
#' the selected elements.
#' @rdname all_fun
label_color_latent <- function(data, label_color = "black", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_color" %in% names(data[[el]])){
      data[[el]]$label_color <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(shape == "oval")
  cl[["expr"]] <- str2lang(paste0("label_color = ", deparse(label_color)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_fill Atomic character vector, indicating which label_fill to assign to
#' the selected elements.
#' @rdname all_fun
label_fill_latent <- function(data, label_fill = "white", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_fill" %in% names(data[[el]])){
      data[[el]]$label_fill <- "white"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(shape == "oval")
  cl[["expr"]] <- str2lang(paste0("label_fill = ", deparse(label_fill)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_size Atomic character vector, indicating which label_size to assign to
#' the selected elements.
#' @rdname all_fun
label_size_latent <- function(data, label_size = 4, ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_size" %in% names(data[[el]])){
      data[[el]]$label_size <- 4
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(shape == "oval")
  cl[["expr"]] <- str2lang(paste0("label_size = ", deparse(label_size)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_alpha Atomic character vector, indicating which label_alpha to assign to
#' the selected elements.
#' @rdname all_fun
label_alpha_latent <- function(data, label_alpha = 1, ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_alpha" %in% names(data[[el]])){
      data[[el]]$label_alpha <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(shape == "oval")
  cl[["expr"]] <- str2lang(paste0("label_alpha = ", deparse(label_alpha)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_family Atomic character vector, indicating which label_family to assign to
#' the selected elements.
#' @rdname all_fun
label_family_latent <- function(data, label_family = "sans", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_family" %in% names(data[[el]])){
      data[[el]]$label_family <- "sans"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(shape == "oval")
  cl[["expr"]] <- str2lang(paste0("label_family = ", deparse(label_family)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_fontface Atomic character vector, indicating which label_fontface to assign to
#' the selected elements.
#' @rdname all_fun
label_fontface_latent <- function(data, label_fontface = "plain", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_fontface" %in% names(data[[el]])){
      data[[el]]$label_fontface <- "plain"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(shape == "oval")
  cl[["expr"]] <- str2lang(paste0("label_fontface = ", deparse(label_fontface)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_hjust Atomic character vector, indicating which label_hjust to assign to
#' the selected elements.
#' @rdname all_fun
label_hjust_latent <- function(data, label_hjust = "center", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_hjust" %in% names(data[[el]])){
      data[[el]]$label_hjust <- "center"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(shape == "oval")
  cl[["expr"]] <- str2lang(paste0("label_hjust = ", deparse(label_hjust)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_vjust Atomic character vector, indicating which label_vjust to assign to
#' the selected elements.
#' @rdname all_fun
label_vjust_latent <- function(data, label_vjust = "middle", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_vjust" %in% names(data[[el]])){
      data[[el]]$label_vjust <- "middle"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(shape == "oval")
  cl[["expr"]] <- str2lang(paste0("label_vjust = ", deparse(label_vjust)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_lineheight Atomic character vector, indicating which label_lineheight to assign to
#' the selected elements.
#' @rdname all_fun
label_lineheight_latent <- function(data, label_lineheight = 1, ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_lineheight" %in% names(data[[el]])){
      data[[el]]$label_lineheight <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(shape == "oval")
  cl[["expr"]] <- str2lang(paste0("label_lineheight = ", deparse(label_lineheight)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param label_location Atomic character vector, indicating which label_location to assign to
#' the selected elements.
#' @rdname all_fun
label_location_latent <- function(data, label_location = .5, ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"label_location" %in% names(data[[el]])){
      data[[el]]$label_location <- .5
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(shape == "oval")
  cl[["expr"]] <- str2lang(paste0("label_location = ", deparse(label_location)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
all_sig_nodes <- function(data, expr, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["element"]] = "nodes"
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
hide_sig_nodes <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["expr"]] <- quote({ show = FALSE })
  cl[["element"]] = "nodes"
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
show_sig_nodes <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["expr"]] <- quote({ show = TRUE })
  cl[["element"]] = "nodes"
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param colour Atomic character vector, indicating which colour to assign to
#' the selected elements.
#' @rdname all_fun
colour_sig_nodes <- function(data, colour = "black", ...){
  these_elements <- "nodes"
  for(el in these_elements){
    if(!"colour" %in% names(data[[el]])){
      data[[el]]$colour <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["expr"]] <- str2lang(paste0("colour = ", deparse(colour)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param color Atomic character vector, indicating which color to assign to
#' the selected elements.
#' @rdname all_fun
color_sig_nodes <- function(data, color = "black", ...){
  these_elements <- "nodes"
  for(el in these_elements){
    if(!"color" %in% names(data[[el]])){
      data[[el]]$color <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["expr"]] <- str2lang(paste0("color = ", deparse(color)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param linetype Atomic character vector, indicating which linetype to assign to
#' the selected elements.
#' @rdname all_fun
linetype_sig_nodes <- function(data, linetype = 1, ...){
  these_elements <- "nodes"
  for(el in these_elements){
    if(!"linetype" %in% names(data[[el]])){
      data[[el]]$linetype <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["expr"]] <- str2lang(paste0("linetype = ", deparse(linetype)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param size Atomic character vector, indicating which size to assign to
#' the selected elements.
#' @rdname all_fun
size_sig_nodes <- function(data, size = 1, ...){
  these_elements <- "nodes"
  for(el in these_elements){
    if(!"size" %in% names(data[[el]])){
      data[[el]]$size <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["expr"]] <- str2lang(paste0("size = ", deparse(size)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param alpha Atomic character vector, indicating which alpha to assign to
#' the selected elements.
#' @rdname all_fun
alpha_sig_nodes <- function(data, alpha = 1, ...){
  these_elements <- "nodes"
  for(el in these_elements){
    if(!"alpha" %in% names(data[[el]])){
      data[[el]]$alpha <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["expr"]] <- str2lang(paste0("alpha = ", deparse(alpha)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
all_nonsig_nodes <- function(data, expr, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["element"]] = "nodes"
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
hide_nonsig_nodes <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["expr"]] <- quote({ show = FALSE })
  cl[["element"]] = "nodes"
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
show_nonsig_nodes <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["expr"]] <- quote({ show = TRUE })
  cl[["element"]] = "nodes"
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param colour Atomic character vector, indicating which colour to assign to
#' the selected elements.
#' @rdname all_fun
colour_nonsig_nodes <- function(data, colour = "black", ...){
  these_elements <- "nodes"
  for(el in these_elements){
    if(!"colour" %in% names(data[[el]])){
      data[[el]]$colour <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["expr"]] <- str2lang(paste0("colour = ", deparse(colour)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param color Atomic character vector, indicating which color to assign to
#' the selected elements.
#' @rdname all_fun
color_nonsig_nodes <- function(data, color = "black", ...){
  these_elements <- "nodes"
  for(el in these_elements){
    if(!"color" %in% names(data[[el]])){
      data[[el]]$color <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["expr"]] <- str2lang(paste0("color = ", deparse(color)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param linetype Atomic character vector, indicating which linetype to assign to
#' the selected elements.
#' @rdname all_fun
linetype_nonsig_nodes <- function(data, linetype = 1, ...){
  these_elements <- "nodes"
  for(el in these_elements){
    if(!"linetype" %in% names(data[[el]])){
      data[[el]]$linetype <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["expr"]] <- str2lang(paste0("linetype = ", deparse(linetype)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param size Atomic character vector, indicating which size to assign to
#' the selected elements.
#' @rdname all_fun
size_nonsig_nodes <- function(data, size = 1, ...){
  these_elements <- "nodes"
  for(el in these_elements){
    if(!"size" %in% names(data[[el]])){
      data[[el]]$size <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["expr"]] <- str2lang(paste0("size = ", deparse(size)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param alpha Atomic character vector, indicating which alpha to assign to
#' the selected elements.
#' @rdname all_fun
alpha_nonsig_nodes <- function(data, alpha = 1, ...){
  these_elements <- "nodes"
  for(el in these_elements){
    if(!"alpha" %in% names(data[[el]])){
      data[[el]]$alpha <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["expr"]] <- str2lang(paste0("alpha = ", deparse(alpha)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
all_fixed_nodes <- function(data, expr, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["element"]] = "nodes"
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
hide_fixed_nodes <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["expr"]] <- quote({ show = FALSE })
  cl[["element"]] = "nodes"
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
show_fixed_nodes <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["expr"]] <- quote({ show = TRUE })
  cl[["element"]] = "nodes"
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param colour Atomic character vector, indicating which colour to assign to
#' the selected elements.
#' @rdname all_fun
colour_fixed_nodes <- function(data, colour = "black", ...){
  these_elements <- "nodes"
  for(el in these_elements){
    if(!"colour" %in% names(data[[el]])){
      data[[el]]$colour <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["expr"]] <- str2lang(paste0("colour = ", deparse(colour)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param color Atomic character vector, indicating which color to assign to
#' the selected elements.
#' @rdname all_fun
color_fixed_nodes <- function(data, color = "black", ...){
  these_elements <- "nodes"
  for(el in these_elements){
    if(!"color" %in% names(data[[el]])){
      data[[el]]$color <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["expr"]] <- str2lang(paste0("color = ", deparse(color)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param linetype Atomic character vector, indicating which linetype to assign to
#' the selected elements.
#' @rdname all_fun
linetype_fixed_nodes <- function(data, linetype = 1, ...){
  these_elements <- "nodes"
  for(el in these_elements){
    if(!"linetype" %in% names(data[[el]])){
      data[[el]]$linetype <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["expr"]] <- str2lang(paste0("linetype = ", deparse(linetype)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param size Atomic character vector, indicating which size to assign to
#' the selected elements.
#' @rdname all_fun
size_fixed_nodes <- function(data, size = 1, ...){
  these_elements <- "nodes"
  for(el in these_elements){
    if(!"size" %in% names(data[[el]])){
      data[[el]]$size <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["expr"]] <- str2lang(paste0("size = ", deparse(size)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param alpha Atomic character vector, indicating which alpha to assign to
#' the selected elements.
#' @rdname all_fun
alpha_fixed_nodes <- function(data, alpha = 1, ...){
  these_elements <- "nodes"
  for(el in these_elements){
    if(!"alpha" %in% names(data[[el]])){
      data[[el]]$alpha <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["expr"]] <- str2lang(paste0("alpha = ", deparse(alpha)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
all_pos_nodes <- function(data, expr, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["element"]] = "nodes"
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
hide_pos_nodes <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["expr"]] <- quote({ show = FALSE })
  cl[["element"]] = "nodes"
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
show_pos_nodes <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["expr"]] <- quote({ show = TRUE })
  cl[["element"]] = "nodes"
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param colour Atomic character vector, indicating which colour to assign to
#' the selected elements.
#' @rdname all_fun
colour_pos_nodes <- function(data, colour = "black", ...){
  these_elements <- "nodes"
  for(el in these_elements){
    if(!"colour" %in% names(data[[el]])){
      data[[el]]$colour <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["expr"]] <- str2lang(paste0("colour = ", deparse(colour)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param color Atomic character vector, indicating which color to assign to
#' the selected elements.
#' @rdname all_fun
color_pos_nodes <- function(data, color = "black", ...){
  these_elements <- "nodes"
  for(el in these_elements){
    if(!"color" %in% names(data[[el]])){
      data[[el]]$color <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["expr"]] <- str2lang(paste0("color = ", deparse(color)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param linetype Atomic character vector, indicating which linetype to assign to
#' the selected elements.
#' @rdname all_fun
linetype_pos_nodes <- function(data, linetype = 1, ...){
  these_elements <- "nodes"
  for(el in these_elements){
    if(!"linetype" %in% names(data[[el]])){
      data[[el]]$linetype <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["expr"]] <- str2lang(paste0("linetype = ", deparse(linetype)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param size Atomic character vector, indicating which size to assign to
#' the selected elements.
#' @rdname all_fun
size_pos_nodes <- function(data, size = 1, ...){
  these_elements <- "nodes"
  for(el in these_elements){
    if(!"size" %in% names(data[[el]])){
      data[[el]]$size <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["expr"]] <- str2lang(paste0("size = ", deparse(size)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param alpha Atomic character vector, indicating which alpha to assign to
#' the selected elements.
#' @rdname all_fun
alpha_pos_nodes <- function(data, alpha = 1, ...){
  these_elements <- "nodes"
  for(el in these_elements){
    if(!"alpha" %in% names(data[[el]])){
      data[[el]]$alpha <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["expr"]] <- str2lang(paste0("alpha = ", deparse(alpha)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
all_neg_nodes <- function(data, expr, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["element"]] = "nodes"
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
hide_neg_nodes <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["expr"]] <- quote({ show = FALSE })
  cl[["element"]] = "nodes"
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
show_neg_nodes <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["expr"]] <- quote({ show = TRUE })
  cl[["element"]] = "nodes"
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param colour Atomic character vector, indicating which colour to assign to
#' the selected elements.
#' @rdname all_fun
colour_neg_nodes <- function(data, colour = "black", ...){
  these_elements <- "nodes"
  for(el in these_elements){
    if(!"colour" %in% names(data[[el]])){
      data[[el]]$colour <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["expr"]] <- str2lang(paste0("colour = ", deparse(colour)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param color Atomic character vector, indicating which color to assign to
#' the selected elements.
#' @rdname all_fun
color_neg_nodes <- function(data, color = "black", ...){
  these_elements <- "nodes"
  for(el in these_elements){
    if(!"color" %in% names(data[[el]])){
      data[[el]]$color <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["expr"]] <- str2lang(paste0("color = ", deparse(color)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param linetype Atomic character vector, indicating which linetype to assign to
#' the selected elements.
#' @rdname all_fun
linetype_neg_nodes <- function(data, linetype = 1, ...){
  these_elements <- "nodes"
  for(el in these_elements){
    if(!"linetype" %in% names(data[[el]])){
      data[[el]]$linetype <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["expr"]] <- str2lang(paste0("linetype = ", deparse(linetype)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param size Atomic character vector, indicating which size to assign to
#' the selected elements.
#' @rdname all_fun
size_neg_nodes <- function(data, size = 1, ...){
  these_elements <- "nodes"
  for(el in these_elements){
    if(!"size" %in% names(data[[el]])){
      data[[el]]$size <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["expr"]] <- str2lang(paste0("size = ", deparse(size)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param alpha Atomic character vector, indicating which alpha to assign to
#' the selected elements.
#' @rdname all_fun
alpha_neg_nodes <- function(data, alpha = 1, ...){
  these_elements <- "nodes"
  for(el in these_elements){
    if(!"alpha" %in% names(data[[el]])){
      data[[el]]$alpha <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["expr"]] <- str2lang(paste0("alpha = ", deparse(alpha)))
  cl[["element"]] <- "nodes"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
all_sig_edges <- function(data, expr, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["element"]] = "edges"
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
hide_sig_edges <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["expr"]] <- quote({ show = FALSE })
  cl[["element"]] = "edges"
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
show_sig_edges <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["expr"]] <- quote({ show = TRUE })
  cl[["element"]] = "edges"
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param colour Atomic character vector, indicating which colour to assign to
#' the selected elements.
#' @rdname all_fun
colour_sig_edges <- function(data, colour = "black", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"colour" %in% names(data[[el]])){
      data[[el]]$colour <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["expr"]] <- str2lang(paste0("colour = ", deparse(colour)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param color Atomic character vector, indicating which color to assign to
#' the selected elements.
#' @rdname all_fun
color_sig_edges <- function(data, color = "black", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"color" %in% names(data[[el]])){
      data[[el]]$color <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["expr"]] <- str2lang(paste0("color = ", deparse(color)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param linetype Atomic character vector, indicating which linetype to assign to
#' the selected elements.
#' @rdname all_fun
linetype_sig_edges <- function(data, linetype = 1, ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"linetype" %in% names(data[[el]])){
      data[[el]]$linetype <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["expr"]] <- str2lang(paste0("linetype = ", deparse(linetype)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param size Atomic character vector, indicating which size to assign to
#' the selected elements.
#' @rdname all_fun
size_sig_edges <- function(data, size = 1, ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"size" %in% names(data[[el]])){
      data[[el]]$size <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["expr"]] <- str2lang(paste0("size = ", deparse(size)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param alpha Atomic character vector, indicating which alpha to assign to
#' the selected elements.
#' @rdname all_fun
alpha_sig_edges <- function(data, alpha = 1, ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"alpha" %in% names(data[[el]])){
      data[[el]]$alpha <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["expr"]] <- str2lang(paste0("alpha = ", deparse(alpha)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
all_nonsig_edges <- function(data, expr, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["element"]] = "edges"
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
hide_nonsig_edges <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["expr"]] <- quote({ show = FALSE })
  cl[["element"]] = "edges"
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
show_nonsig_edges <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["expr"]] <- quote({ show = TRUE })
  cl[["element"]] = "edges"
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param colour Atomic character vector, indicating which colour to assign to
#' the selected elements.
#' @rdname all_fun
colour_nonsig_edges <- function(data, colour = "black", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"colour" %in% names(data[[el]])){
      data[[el]]$colour <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["expr"]] <- str2lang(paste0("colour = ", deparse(colour)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param color Atomic character vector, indicating which color to assign to
#' the selected elements.
#' @rdname all_fun
color_nonsig_edges <- function(data, color = "black", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"color" %in% names(data[[el]])){
      data[[el]]$color <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["expr"]] <- str2lang(paste0("color = ", deparse(color)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param linetype Atomic character vector, indicating which linetype to assign to
#' the selected elements.
#' @rdname all_fun
linetype_nonsig_edges <- function(data, linetype = 1, ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"linetype" %in% names(data[[el]])){
      data[[el]]$linetype <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["expr"]] <- str2lang(paste0("linetype = ", deparse(linetype)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param size Atomic character vector, indicating which size to assign to
#' the selected elements.
#' @rdname all_fun
size_nonsig_edges <- function(data, size = 1, ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"size" %in% names(data[[el]])){
      data[[el]]$size <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["expr"]] <- str2lang(paste0("size = ", deparse(size)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param alpha Atomic character vector, indicating which alpha to assign to
#' the selected elements.
#' @rdname all_fun
alpha_nonsig_edges <- function(data, alpha = 1, ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"alpha" %in% names(data[[el]])){
      data[[el]]$alpha <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["expr"]] <- str2lang(paste0("alpha = ", deparse(alpha)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
all_fixed_edges <- function(data, expr, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["element"]] = "edges"
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
hide_fixed_edges <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["expr"]] <- quote({ show = FALSE })
  cl[["element"]] = "edges"
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
show_fixed_edges <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["expr"]] <- quote({ show = TRUE })
  cl[["element"]] = "edges"
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param colour Atomic character vector, indicating which colour to assign to
#' the selected elements.
#' @rdname all_fun
colour_fixed_edges <- function(data, colour = "black", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"colour" %in% names(data[[el]])){
      data[[el]]$colour <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["expr"]] <- str2lang(paste0("colour = ", deparse(colour)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param color Atomic character vector, indicating which color to assign to
#' the selected elements.
#' @rdname all_fun
color_fixed_edges <- function(data, color = "black", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"color" %in% names(data[[el]])){
      data[[el]]$color <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["expr"]] <- str2lang(paste0("color = ", deparse(color)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param linetype Atomic character vector, indicating which linetype to assign to
#' the selected elements.
#' @rdname all_fun
linetype_fixed_edges <- function(data, linetype = 1, ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"linetype" %in% names(data[[el]])){
      data[[el]]$linetype <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["expr"]] <- str2lang(paste0("linetype = ", deparse(linetype)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param size Atomic character vector, indicating which size to assign to
#' the selected elements.
#' @rdname all_fun
size_fixed_edges <- function(data, size = 1, ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"size" %in% names(data[[el]])){
      data[[el]]$size <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["expr"]] <- str2lang(paste0("size = ", deparse(size)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param alpha Atomic character vector, indicating which alpha to assign to
#' the selected elements.
#' @rdname all_fun
alpha_fixed_edges <- function(data, alpha = 1, ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"alpha" %in% names(data[[el]])){
      data[[el]]$alpha <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["expr"]] <- str2lang(paste0("alpha = ", deparse(alpha)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
all_pos_edges <- function(data, expr, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["element"]] = "edges"
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
hide_pos_edges <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["expr"]] <- quote({ show = FALSE })
  cl[["element"]] = "edges"
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
show_pos_edges <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["expr"]] <- quote({ show = TRUE })
  cl[["element"]] = "edges"
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param colour Atomic character vector, indicating which colour to assign to
#' the selected elements.
#' @rdname all_fun
colour_pos_edges <- function(data, colour = "black", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"colour" %in% names(data[[el]])){
      data[[el]]$colour <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["expr"]] <- str2lang(paste0("colour = ", deparse(colour)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param color Atomic character vector, indicating which color to assign to
#' the selected elements.
#' @rdname all_fun
color_pos_edges <- function(data, color = "black", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"color" %in% names(data[[el]])){
      data[[el]]$color <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["expr"]] <- str2lang(paste0("color = ", deparse(color)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param linetype Atomic character vector, indicating which linetype to assign to
#' the selected elements.
#' @rdname all_fun
linetype_pos_edges <- function(data, linetype = 1, ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"linetype" %in% names(data[[el]])){
      data[[el]]$linetype <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["expr"]] <- str2lang(paste0("linetype = ", deparse(linetype)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param size Atomic character vector, indicating which size to assign to
#' the selected elements.
#' @rdname all_fun
size_pos_edges <- function(data, size = 1, ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"size" %in% names(data[[el]])){
      data[[el]]$size <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["expr"]] <- str2lang(paste0("size = ", deparse(size)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param alpha Atomic character vector, indicating which alpha to assign to
#' the selected elements.
#' @rdname all_fun
alpha_pos_edges <- function(data, alpha = 1, ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"alpha" %in% names(data[[el]])){
      data[[el]]$alpha <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["expr"]] <- str2lang(paste0("alpha = ", deparse(alpha)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
all_neg_edges <- function(data, expr, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["element"]] = "edges"
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
hide_neg_edges <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["expr"]] <- quote({ show = FALSE })
  cl[["element"]] = "edges"
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
show_neg_edges <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["expr"]] <- quote({ show = TRUE })
  cl[["element"]] = "edges"
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param colour Atomic character vector, indicating which colour to assign to
#' the selected elements.
#' @rdname all_fun
colour_neg_edges <- function(data, colour = "black", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"colour" %in% names(data[[el]])){
      data[[el]]$colour <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["expr"]] <- str2lang(paste0("colour = ", deparse(colour)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param color Atomic character vector, indicating which color to assign to
#' the selected elements.
#' @rdname all_fun
color_neg_edges <- function(data, color = "black", ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"color" %in% names(data[[el]])){
      data[[el]]$color <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["expr"]] <- str2lang(paste0("color = ", deparse(color)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param linetype Atomic character vector, indicating which linetype to assign to
#' the selected elements.
#' @rdname all_fun
linetype_neg_edges <- function(data, linetype = 1, ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"linetype" %in% names(data[[el]])){
      data[[el]]$linetype <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["expr"]] <- str2lang(paste0("linetype = ", deparse(linetype)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param size Atomic character vector, indicating which size to assign to
#' the selected elements.
#' @rdname all_fun
size_neg_edges <- function(data, size = 1, ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"size" %in% names(data[[el]])){
      data[[el]]$size <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["expr"]] <- str2lang(paste0("size = ", deparse(size)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param alpha Atomic character vector, indicating which alpha to assign to
#' the selected elements.
#' @rdname all_fun
alpha_neg_edges <- function(data, alpha = 1, ...){
  these_elements <- "edges"
  for(el in these_elements){
    if(!"alpha" %in% names(data[[el]])){
      data[[el]]$alpha <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["expr"]] <- str2lang(paste0("alpha = ", deparse(alpha)))
  cl[["element"]] <- "edges"
  cl <- cl[c(1, match(c("data", "condition", "expr", "element"), names(cl)))]
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
