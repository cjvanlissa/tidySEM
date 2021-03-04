create_all_select_rows <- function(filename = "R/select_rows.R"){
  txt <- readLines(filename)
  out <- txt[1:grep("^# Automatically generated functions below here$", txt)]
  # Prepare lists
  fun_list <- list(
    sig = 'as.numeric(pval) < .05',
    non_sig = 'as.numeric(pval) >= .05',
    fixed = 'is.na(pval)',
    pos = 'as.numeric(est) > 0',
    neg = 'as.numeric(est) < 0',
    var = 'lhs == rhs & op == "~~"',
    cov = 'lhs != rhs & op == "~~"',
    reg = 'op == "~"',
    loading = 'op == "=~"',
    fixed_loading = 'op == "=~" & is.na(pval)',
    mean = 'op == "~1"'
  )
  aes_list <- list(
    colour = '"black"',
    color = '"black"',
    linetype = '1',
    size = '1',
    alpha = '1'
  )

  # Get templates
  template <- txt[grepl("^#A?[XY]", txt)]
  # Create aes templates
  aes_temp <- template[grepl("^#A[XY]", template)]
  # Remove from template
  template <- template[!grepl("^#A", template)]
  # Clean up
  aes_temp <- gsub("^#A", "#", aes_temp)
  # All iterations
  add_aes <- vector("character")
  for(i in 1:length(aes_list)){
    add_aes <- c(add_aes,
                 gsub("AES", names(aes_list)[i], gsub("AES_DEFAULT", aes_list[[i]], aes_temp)),
                 ""
    )
  }
  # Append to template
  template <- c(template, add_aes)

  template <- gsub("^#X ", "", template)
  template <- gsub("^#Y", "#'", template)

  for(i in 1:length(fun_list)){
    out <- c(out,
             gsub("COND", fun_list[[i]], gsub("NAME", names(fun_list)[i], template)),
             ""
    )
  }
  writeLines(out, "R/select_rows.R")
}

#' @title Apply an expression to an object based on a condition
#' @description This function allows users to conditionally manipulate an
#' object.
#' @param data Object to manipulate.
#' @param condition Expression that evaluates to a logical vector in the
#' environment of \code{data}.
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
  #browser()
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
    tryCatch({
      data[condition_met, ] <- eval.parent(cl)
    }, warning = function(w){
      message("Tried to assign to a non-existent variable.")
    })

  }
  return(data)
}

#' @method all_fun sem_graph
#' @param element Character vector. The elements of the \code{sem_graph} to
#' edit, defaults to \code{"edges"}.
#' @rdname all_fun
#' @export
all_fun.sem_graph <- function(data, expr, condition, element = "edges", ...){
  browser()
  cl <- match.call()
  cl[[1L]] <- str2lang(gsub("\\..*$", "", deparse(substitute(cl)[[1L]])))
  out <- data
  these_elements <- element[element %in% names(data)]
  if(length(these_elements) > 0){
    out[these_elements] <- lapply(these_elements, function(this_el){
      cl[["data"]] <- data[[this_el]]
      eval.parent(cl)
    })
  }
  out
}

# Template function
#Y @export
# @examples
# all_NAME(table_results(res), label = "changed")
#Y @rdname all_fun
#X all_NAME <- function(data, expr, ...){
#X   cl <- match.call()
#X   cl[["condition"]] <- substitute(COND)
#X   cl[[1L]] <- quote(all_fun)
#X   eval.parent(cl)
#X }

#Y @export
# @examples
# hide_NAME(table_results(res))
#Y @rdname all_fun
#X hide_NAME <- function(data, ...){
#X   cl <- match.call()
#X   cl[["condition"]] <- substitute(COND)
#X   cl[["expr"]] <- quote({ show = FALSE })
#X   cl[[1L]] <- quote(all_fun)
#X   eval.parent(cl)
#X }

#Y @export
# @examples
# show_NAME(table_results(res))
#Y @rdname all_fun
#X show_NAME <- function(data, ...){
#X   cl <- match.call()
#X   cl[["condition"]] <- substitute(COND)
#X   cl[["expr"]] <- quote({ show = TRUE })
#X   cl[[1L]] <- quote(all_fun)
#X   eval.parent(cl)
#X }

#Y @export
# @examples
# color_NAME(table_results(res))
#Y @rdname all_fun
#X color_NAME <- function(data, ...){
#X   browser()
#X   cl <- match.call()
#X   cl[["condition"]] <- substitute(COND)
#X   cl[["expr"]] <- quote({ show = TRUE })
#X   cl[[1L]] <- quote(all_fun)
#X   eval.parent(cl)
#X }

#AY @export
#AY @param AES Atomic character vector, indicating which AES to assign to
#AY the selected elements.
#AY @rdname all_fun
#AX AES_NAME <- function(data, AES = AES_DEFAULT, ...){
#AX   dots <- list(...)
#AX   if(inherits(data, "sem_graph")){
#AX     if("element" %in% names(dots)){
#AX       these_elements <- dots[["element"]][dots[["element"]] %in% names(data)]
#AX     } else {
#AX       these_elements <- formals(get("all_fun.sem_graph"), envir = environment())$element
#AX     }
#AX     for(el in these_elements){
#AX       if(!"AES" %in% names(data[[el]])){
#AX         data[[el]]$AES <- AES_DEFAULT
#AX       }
#AX     }
#AX   }
#AX   if(inherits(data, "data.frame")){
#AX     if(!"AES" %in% names(data)){
#AX       data$AES <- AES_DEFAULT
#AX     }
#AX   }
#AX   cl <- match.call()
#AX   cl[["data"]] <- data
#AX   cl[["condition"]] <- substitute(COND)
#AX   cl[["expr"]] <- str2lang(paste0("AES = ", deparse(AES)))
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
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
hide_sig <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["expr"]] <- quote({ show = FALSE })
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
show_sig <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["expr"]] <- quote({ show = TRUE })
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
color_sig <- function(data, ...){
  browser()
  cl <- match.call()
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["expr"]] <- quote({ show = TRUE })
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param colour Atomic character vector, indicating which colour to assign to
#' the selected elements.
#' @rdname all_fun
colour_sig <- function(data, colour = "black", ...){
  dots <- list(...)
  if(inherits(data, "sem_graph")){
    if("element" %in% names(dots)){
      these_elements <- dots[["element"]][dots[["element"]] %in% names(data)]
    } else {
      these_elements <- formals(get("all_fun.sem_graph"), envir = environment())$element
    }
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
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}

#' @export
#' @param color Atomic character vector, indicating which color to assign to
#' the selected elements.
#' @rdname all_fun
color_sig <- function(data, color = "black", ...){
  dots <- list(...)
  if(inherits(data, "sem_graph")){
    if("element" %in% names(dots)){
      these_elements <- dots[["element"]][dots[["element"]] %in% names(data)]
    } else {
      these_elements <- formals(get("all_fun.sem_graph"), envir = environment())$element
    }
    for(el in these_elements){
      if(!"color" %in% names(data[[el]])){
        data[[el]]$color <- "black"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"color" %in% names(data)){
      data$color <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["expr"]] <- str2lang(paste0("color = ", deparse(color)))
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}

#' @export
#' @param linetype Atomic character vector, indicating which linetype to assign to
#' the selected elements.
#' @rdname all_fun
linetype_sig <- function(data, linetype = 1, ...){
  dots <- list(...)
  if(inherits(data, "sem_graph")){
    if("element" %in% names(dots)){
      these_elements <- dots[["element"]][dots[["element"]] %in% names(data)]
    } else {
      these_elements <- formals(get("all_fun.sem_graph"), envir = environment())$element
    }
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
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}

#' @export
#' @param size Atomic character vector, indicating which size to assign to
#' the selected elements.
#' @rdname all_fun
size_sig <- function(data, size = 1, ...){
  dots <- list(...)
  if(inherits(data, "sem_graph")){
    if("element" %in% names(dots)){
      these_elements <- dots[["element"]][dots[["element"]] %in% names(data)]
    } else {
      these_elements <- formals(get("all_fun.sem_graph"), envir = environment())$element
    }
    for(el in these_elements){
      if(!"size" %in% names(data[[el]])){
        data[[el]]$size <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"size" %in% names(data)){
      data$size <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) < .05)
  cl[["expr"]] <- str2lang(paste0("size = ", deparse(size)))
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}

#' @export
#' @param alpha Atomic character vector, indicating which alpha to assign to
#' the selected elements.
#' @rdname all_fun
alpha_sig <- function(data, alpha = 1, ...){
  dots <- list(...)
  if(inherits(data, "sem_graph")){
    if("element" %in% names(dots)){
      these_elements <- dots[["element"]][dots[["element"]] %in% names(data)]
    } else {
      these_elements <- formals(get("all_fun.sem_graph"), envir = environment())$element
    }
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
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}


#' @export
#' @rdname all_fun
all_non_sig <- function(data, expr, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
hide_non_sig <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["expr"]] <- quote({ show = FALSE })
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
show_non_sig <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["expr"]] <- quote({ show = TRUE })
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
color_non_sig <- function(data, ...){
  browser()
  cl <- match.call()
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["expr"]] <- quote({ show = TRUE })
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param colour Atomic character vector, indicating which colour to assign to
#' the selected elements.
#' @rdname all_fun
colour_non_sig <- function(data, colour = "black", ...){
  dots <- list(...)
  if(inherits(data, "sem_graph")){
    if("element" %in% names(dots)){
      these_elements <- dots[["element"]][dots[["element"]] %in% names(data)]
    } else {
      these_elements <- formals(get("all_fun.sem_graph"), envir = environment())$element
    }
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
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}

#' @export
#' @param color Atomic character vector, indicating which color to assign to
#' the selected elements.
#' @rdname all_fun
color_non_sig <- function(data, color = "black", ...){
  dots <- list(...)
  if(inherits(data, "sem_graph")){
    if("element" %in% names(dots)){
      these_elements <- dots[["element"]][dots[["element"]] %in% names(data)]
    } else {
      these_elements <- formals(get("all_fun.sem_graph"), envir = environment())$element
    }
    for(el in these_elements){
      if(!"color" %in% names(data[[el]])){
        data[[el]]$color <- "black"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"color" %in% names(data)){
      data$color <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["expr"]] <- str2lang(paste0("color = ", deparse(color)))
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}

#' @export
#' @param linetype Atomic character vector, indicating which linetype to assign to
#' the selected elements.
#' @rdname all_fun
linetype_non_sig <- function(data, linetype = 1, ...){
  dots <- list(...)
  if(inherits(data, "sem_graph")){
    if("element" %in% names(dots)){
      these_elements <- dots[["element"]][dots[["element"]] %in% names(data)]
    } else {
      these_elements <- formals(get("all_fun.sem_graph"), envir = environment())$element
    }
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
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}

#' @export
#' @param size Atomic character vector, indicating which size to assign to
#' the selected elements.
#' @rdname all_fun
size_non_sig <- function(data, size = 1, ...){
  dots <- list(...)
  if(inherits(data, "sem_graph")){
    if("element" %in% names(dots)){
      these_elements <- dots[["element"]][dots[["element"]] %in% names(data)]
    } else {
      these_elements <- formals(get("all_fun.sem_graph"), envir = environment())$element
    }
    for(el in these_elements){
      if(!"size" %in% names(data[[el]])){
        data[[el]]$size <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"size" %in% names(data)){
      data$size <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(pval) >= .05)
  cl[["expr"]] <- str2lang(paste0("size = ", deparse(size)))
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}

#' @export
#' @param alpha Atomic character vector, indicating which alpha to assign to
#' the selected elements.
#' @rdname all_fun
alpha_non_sig <- function(data, alpha = 1, ...){
  dots <- list(...)
  if(inherits(data, "sem_graph")){
    if("element" %in% names(dots)){
      these_elements <- dots[["element"]][dots[["element"]] %in% names(data)]
    } else {
      these_elements <- formals(get("all_fun.sem_graph"), envir = environment())$element
    }
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
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}


#' @export
#' @rdname all_fun
all_fixed <- function(data, expr, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(is.na(pval))
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
hide_fixed <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["expr"]] <- quote({ show = FALSE })
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
show_fixed <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["expr"]] <- quote({ show = TRUE })
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
color_fixed <- function(data, ...){
  browser()
  cl <- match.call()
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["expr"]] <- quote({ show = TRUE })
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param colour Atomic character vector, indicating which colour to assign to
#' the selected elements.
#' @rdname all_fun
colour_fixed <- function(data, colour = "black", ...){
  dots <- list(...)
  if(inherits(data, "sem_graph")){
    if("element" %in% names(dots)){
      these_elements <- dots[["element"]][dots[["element"]] %in% names(data)]
    } else {
      these_elements <- formals(get("all_fun.sem_graph"), envir = environment())$element
    }
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
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}

#' @export
#' @param color Atomic character vector, indicating which color to assign to
#' the selected elements.
#' @rdname all_fun
color_fixed <- function(data, color = "black", ...){
  dots <- list(...)
  if(inherits(data, "sem_graph")){
    if("element" %in% names(dots)){
      these_elements <- dots[["element"]][dots[["element"]] %in% names(data)]
    } else {
      these_elements <- formals(get("all_fun.sem_graph"), envir = environment())$element
    }
    for(el in these_elements){
      if(!"color" %in% names(data[[el]])){
        data[[el]]$color <- "black"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"color" %in% names(data)){
      data$color <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["expr"]] <- str2lang(paste0("color = ", deparse(color)))
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}

#' @export
#' @param linetype Atomic character vector, indicating which linetype to assign to
#' the selected elements.
#' @rdname all_fun
linetype_fixed <- function(data, linetype = 1, ...){
  dots <- list(...)
  if(inherits(data, "sem_graph")){
    if("element" %in% names(dots)){
      these_elements <- dots[["element"]][dots[["element"]] %in% names(data)]
    } else {
      these_elements <- formals(get("all_fun.sem_graph"), envir = environment())$element
    }
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
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}

#' @export
#' @param size Atomic character vector, indicating which size to assign to
#' the selected elements.
#' @rdname all_fun
size_fixed <- function(data, size = 1, ...){
  dots <- list(...)
  if(inherits(data, "sem_graph")){
    if("element" %in% names(dots)){
      these_elements <- dots[["element"]][dots[["element"]] %in% names(data)]
    } else {
      these_elements <- formals(get("all_fun.sem_graph"), envir = environment())$element
    }
    for(el in these_elements){
      if(!"size" %in% names(data[[el]])){
        data[[el]]$size <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"size" %in% names(data)){
      data$size <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(is.na(pval))
  cl[["expr"]] <- str2lang(paste0("size = ", deparse(size)))
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}

#' @export
#' @param alpha Atomic character vector, indicating which alpha to assign to
#' the selected elements.
#' @rdname all_fun
alpha_fixed <- function(data, alpha = 1, ...){
  dots <- list(...)
  if(inherits(data, "sem_graph")){
    if("element" %in% names(dots)){
      these_elements <- dots[["element"]][dots[["element"]] %in% names(data)]
    } else {
      these_elements <- formals(get("all_fun.sem_graph"), envir = environment())$element
    }
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
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}


#' @export
#' @rdname all_fun
all_pos <- function(data, expr, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
hide_pos <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["expr"]] <- quote({ show = FALSE })
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
show_pos <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["expr"]] <- quote({ show = TRUE })
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
color_pos <- function(data, ...){
  browser()
  cl <- match.call()
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["expr"]] <- quote({ show = TRUE })
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param colour Atomic character vector, indicating which colour to assign to
#' the selected elements.
#' @rdname all_fun
colour_pos <- function(data, colour = "black", ...){
  dots <- list(...)
  if(inherits(data, "sem_graph")){
    if("element" %in% names(dots)){
      these_elements <- dots[["element"]][dots[["element"]] %in% names(data)]
    } else {
      these_elements <- formals(get("all_fun.sem_graph"), envir = environment())$element
    }
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
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}

#' @export
#' @param color Atomic character vector, indicating which color to assign to
#' the selected elements.
#' @rdname all_fun
color_pos <- function(data, color = "black", ...){
  dots <- list(...)
  if(inherits(data, "sem_graph")){
    if("element" %in% names(dots)){
      these_elements <- dots[["element"]][dots[["element"]] %in% names(data)]
    } else {
      these_elements <- formals(get("all_fun.sem_graph"), envir = environment())$element
    }
    for(el in these_elements){
      if(!"color" %in% names(data[[el]])){
        data[[el]]$color <- "black"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"color" %in% names(data)){
      data$color <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["expr"]] <- str2lang(paste0("color = ", deparse(color)))
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}

#' @export
#' @param linetype Atomic character vector, indicating which linetype to assign to
#' the selected elements.
#' @rdname all_fun
linetype_pos <- function(data, linetype = 1, ...){
  dots <- list(...)
  if(inherits(data, "sem_graph")){
    if("element" %in% names(dots)){
      these_elements <- dots[["element"]][dots[["element"]] %in% names(data)]
    } else {
      these_elements <- formals(get("all_fun.sem_graph"), envir = environment())$element
    }
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
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}

#' @export
#' @param size Atomic character vector, indicating which size to assign to
#' the selected elements.
#' @rdname all_fun
size_pos <- function(data, size = 1, ...){
  dots <- list(...)
  if(inherits(data, "sem_graph")){
    if("element" %in% names(dots)){
      these_elements <- dots[["element"]][dots[["element"]] %in% names(data)]
    } else {
      these_elements <- formals(get("all_fun.sem_graph"), envir = environment())$element
    }
    for(el in these_elements){
      if(!"size" %in% names(data[[el]])){
        data[[el]]$size <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"size" %in% names(data)){
      data$size <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) > 0)
  cl[["expr"]] <- str2lang(paste0("size = ", deparse(size)))
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}

#' @export
#' @param alpha Atomic character vector, indicating which alpha to assign to
#' the selected elements.
#' @rdname all_fun
alpha_pos <- function(data, alpha = 1, ...){
  dots <- list(...)
  if(inherits(data, "sem_graph")){
    if("element" %in% names(dots)){
      these_elements <- dots[["element"]][dots[["element"]] %in% names(data)]
    } else {
      these_elements <- formals(get("all_fun.sem_graph"), envir = environment())$element
    }
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
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}


#' @export
#' @rdname all_fun
all_neg <- function(data, expr, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
hide_neg <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["expr"]] <- quote({ show = FALSE })
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
show_neg <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["expr"]] <- quote({ show = TRUE })
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
color_neg <- function(data, ...){
  browser()
  cl <- match.call()
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["expr"]] <- quote({ show = TRUE })
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param colour Atomic character vector, indicating which colour to assign to
#' the selected elements.
#' @rdname all_fun
colour_neg <- function(data, colour = "black", ...){
  dots <- list(...)
  if(inherits(data, "sem_graph")){
    if("element" %in% names(dots)){
      these_elements <- dots[["element"]][dots[["element"]] %in% names(data)]
    } else {
      these_elements <- formals(get("all_fun.sem_graph"), envir = environment())$element
    }
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
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}

#' @export
#' @param color Atomic character vector, indicating which color to assign to
#' the selected elements.
#' @rdname all_fun
color_neg <- function(data, color = "black", ...){
  dots <- list(...)
  if(inherits(data, "sem_graph")){
    if("element" %in% names(dots)){
      these_elements <- dots[["element"]][dots[["element"]] %in% names(data)]
    } else {
      these_elements <- formals(get("all_fun.sem_graph"), envir = environment())$element
    }
    for(el in these_elements){
      if(!"color" %in% names(data[[el]])){
        data[[el]]$color <- "black"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"color" %in% names(data)){
      data$color <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["expr"]] <- str2lang(paste0("color = ", deparse(color)))
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}

#' @export
#' @param linetype Atomic character vector, indicating which linetype to assign to
#' the selected elements.
#' @rdname all_fun
linetype_neg <- function(data, linetype = 1, ...){
  dots <- list(...)
  if(inherits(data, "sem_graph")){
    if("element" %in% names(dots)){
      these_elements <- dots[["element"]][dots[["element"]] %in% names(data)]
    } else {
      these_elements <- formals(get("all_fun.sem_graph"), envir = environment())$element
    }
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
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}

#' @export
#' @param size Atomic character vector, indicating which size to assign to
#' the selected elements.
#' @rdname all_fun
size_neg <- function(data, size = 1, ...){
  dots <- list(...)
  if(inherits(data, "sem_graph")){
    if("element" %in% names(dots)){
      these_elements <- dots[["element"]][dots[["element"]] %in% names(data)]
    } else {
      these_elements <- formals(get("all_fun.sem_graph"), envir = environment())$element
    }
    for(el in these_elements){
      if(!"size" %in% names(data[[el]])){
        data[[el]]$size <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"size" %in% names(data)){
      data$size <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(as.numeric(est) < 0)
  cl[["expr"]] <- str2lang(paste0("size = ", deparse(size)))
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}

#' @export
#' @param alpha Atomic character vector, indicating which alpha to assign to
#' the selected elements.
#' @rdname all_fun
alpha_neg <- function(data, alpha = 1, ...){
  dots <- list(...)
  if(inherits(data, "sem_graph")){
    if("element" %in% names(dots)){
      these_elements <- dots[["element"]][dots[["element"]] %in% names(data)]
    } else {
      these_elements <- formals(get("all_fun.sem_graph"), envir = environment())$element
    }
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
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}


#' @export
#' @rdname all_fun
all_var <- function(data, expr, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(lhs == rhs & op == "~~")
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
hide_var <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(lhs == rhs & op == "~~")
  cl[["expr"]] <- quote({ show = FALSE })
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
show_var <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(lhs == rhs & op == "~~")
  cl[["expr"]] <- quote({ show = TRUE })
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
color_var <- function(data, ...){
  browser()
  cl <- match.call()
  cl[["condition"]] <- substitute(lhs == rhs & op == "~~")
  cl[["expr"]] <- quote({ show = TRUE })
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param colour Atomic character vector, indicating which colour to assign to
#' the selected elements.
#' @rdname all_fun
colour_var <- function(data, colour = "black", ...){
  dots <- list(...)
  if(inherits(data, "sem_graph")){
    if("element" %in% names(dots)){
      these_elements <- dots[["element"]][dots[["element"]] %in% names(data)]
    } else {
      these_elements <- formals(get("all_fun.sem_graph"), envir = environment())$element
    }
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
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}

#' @export
#' @param color Atomic character vector, indicating which color to assign to
#' the selected elements.
#' @rdname all_fun
color_var <- function(data, color = "black", ...){
  dots <- list(...)
  if(inherits(data, "sem_graph")){
    if("element" %in% names(dots)){
      these_elements <- dots[["element"]][dots[["element"]] %in% names(data)]
    } else {
      these_elements <- formals(get("all_fun.sem_graph"), envir = environment())$element
    }
    for(el in these_elements){
      if(!"color" %in% names(data[[el]])){
        data[[el]]$color <- "black"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"color" %in% names(data)){
      data$color <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(lhs == rhs & op == "~~")
  cl[["expr"]] <- str2lang(paste0("color = ", deparse(color)))
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}

#' @export
#' @param linetype Atomic character vector, indicating which linetype to assign to
#' the selected elements.
#' @rdname all_fun
linetype_var <- function(data, linetype = 1, ...){
  dots <- list(...)
  if(inherits(data, "sem_graph")){
    if("element" %in% names(dots)){
      these_elements <- dots[["element"]][dots[["element"]] %in% names(data)]
    } else {
      these_elements <- formals(get("all_fun.sem_graph"), envir = environment())$element
    }
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
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}

#' @export
#' @param size Atomic character vector, indicating which size to assign to
#' the selected elements.
#' @rdname all_fun
size_var <- function(data, size = 1, ...){
  dots <- list(...)
  if(inherits(data, "sem_graph")){
    if("element" %in% names(dots)){
      these_elements <- dots[["element"]][dots[["element"]] %in% names(data)]
    } else {
      these_elements <- formals(get("all_fun.sem_graph"), envir = environment())$element
    }
    for(el in these_elements){
      if(!"size" %in% names(data[[el]])){
        data[[el]]$size <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"size" %in% names(data)){
      data$size <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(lhs == rhs & op == "~~")
  cl[["expr"]] <- str2lang(paste0("size = ", deparse(size)))
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}

#' @export
#' @param alpha Atomic character vector, indicating which alpha to assign to
#' the selected elements.
#' @rdname all_fun
alpha_var <- function(data, alpha = 1, ...){
  dots <- list(...)
  if(inherits(data, "sem_graph")){
    if("element" %in% names(dots)){
      these_elements <- dots[["element"]][dots[["element"]] %in% names(data)]
    } else {
      these_elements <- formals(get("all_fun.sem_graph"), envir = environment())$element
    }
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
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}


#' @export
#' @rdname all_fun
all_cov <- function(data, expr, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(lhs != rhs & op == "~~")
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
hide_cov <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(lhs != rhs & op == "~~")
  cl[["expr"]] <- quote({ show = FALSE })
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
show_cov <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(lhs != rhs & op == "~~")
  cl[["expr"]] <- quote({ show = TRUE })
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
color_cov <- function(data, ...){
  browser()
  cl <- match.call()
  cl[["condition"]] <- substitute(lhs != rhs & op == "~~")
  cl[["expr"]] <- quote({ show = TRUE })
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param colour Atomic character vector, indicating which colour to assign to
#' the selected elements.
#' @rdname all_fun
colour_cov <- function(data, colour = "black", ...){
  dots <- list(...)
  if(inherits(data, "sem_graph")){
    if("element" %in% names(dots)){
      these_elements <- dots[["element"]][dots[["element"]] %in% names(data)]
    } else {
      these_elements <- formals(get("all_fun.sem_graph"), envir = environment())$element
    }
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
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}

#' @export
#' @param color Atomic character vector, indicating which color to assign to
#' the selected elements.
#' @rdname all_fun
color_cov <- function(data, color = "black", ...){
  dots <- list(...)
  if(inherits(data, "sem_graph")){
    if("element" %in% names(dots)){
      these_elements <- dots[["element"]][dots[["element"]] %in% names(data)]
    } else {
      these_elements <- formals(get("all_fun.sem_graph"), envir = environment())$element
    }
    for(el in these_elements){
      if(!"color" %in% names(data[[el]])){
        data[[el]]$color <- "black"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"color" %in% names(data)){
      data$color <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(lhs != rhs & op == "~~")
  cl[["expr"]] <- str2lang(paste0("color = ", deparse(color)))
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}

#' @export
#' @param linetype Atomic character vector, indicating which linetype to assign to
#' the selected elements.
#' @rdname all_fun
linetype_cov <- function(data, linetype = 1, ...){
  dots <- list(...)
  if(inherits(data, "sem_graph")){
    if("element" %in% names(dots)){
      these_elements <- dots[["element"]][dots[["element"]] %in% names(data)]
    } else {
      these_elements <- formals(get("all_fun.sem_graph"), envir = environment())$element
    }
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
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}

#' @export
#' @param size Atomic character vector, indicating which size to assign to
#' the selected elements.
#' @rdname all_fun
size_cov <- function(data, size = 1, ...){
  dots <- list(...)
  if(inherits(data, "sem_graph")){
    if("element" %in% names(dots)){
      these_elements <- dots[["element"]][dots[["element"]] %in% names(data)]
    } else {
      these_elements <- formals(get("all_fun.sem_graph"), envir = environment())$element
    }
    for(el in these_elements){
      if(!"size" %in% names(data[[el]])){
        data[[el]]$size <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"size" %in% names(data)){
      data$size <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(lhs != rhs & op == "~~")
  cl[["expr"]] <- str2lang(paste0("size = ", deparse(size)))
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}

#' @export
#' @param alpha Atomic character vector, indicating which alpha to assign to
#' the selected elements.
#' @rdname all_fun
alpha_cov <- function(data, alpha = 1, ...){
  dots <- list(...)
  if(inherits(data, "sem_graph")){
    if("element" %in% names(dots)){
      these_elements <- dots[["element"]][dots[["element"]] %in% names(data)]
    } else {
      these_elements <- formals(get("all_fun.sem_graph"), envir = environment())$element
    }
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
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}


#' @export
#' @rdname all_fun
all_reg <- function(data, expr, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(op == "~")
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
hide_reg <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(op == "~")
  cl[["expr"]] <- quote({ show = FALSE })
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
show_reg <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(op == "~")
  cl[["expr"]] <- quote({ show = TRUE })
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
color_reg <- function(data, ...){
  browser()
  cl <- match.call()
  cl[["condition"]] <- substitute(op == "~")
  cl[["expr"]] <- quote({ show = TRUE })
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param colour Atomic character vector, indicating which colour to assign to
#' the selected elements.
#' @rdname all_fun
colour_reg <- function(data, colour = "black", ...){
  dots <- list(...)
  if(inherits(data, "sem_graph")){
    if("element" %in% names(dots)){
      these_elements <- dots[["element"]][dots[["element"]] %in% names(data)]
    } else {
      these_elements <- formals(get("all_fun.sem_graph"), envir = environment())$element
    }
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
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}

#' @export
#' @param color Atomic character vector, indicating which color to assign to
#' the selected elements.
#' @rdname all_fun
color_reg <- function(data, color = "black", ...){
  dots <- list(...)
  if(inherits(data, "sem_graph")){
    if("element" %in% names(dots)){
      these_elements <- dots[["element"]][dots[["element"]] %in% names(data)]
    } else {
      these_elements <- formals(get("all_fun.sem_graph"), envir = environment())$element
    }
    for(el in these_elements){
      if(!"color" %in% names(data[[el]])){
        data[[el]]$color <- "black"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"color" %in% names(data)){
      data$color <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "~")
  cl[["expr"]] <- str2lang(paste0("color = ", deparse(color)))
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}

#' @export
#' @param linetype Atomic character vector, indicating which linetype to assign to
#' the selected elements.
#' @rdname all_fun
linetype_reg <- function(data, linetype = 1, ...){
  dots <- list(...)
  if(inherits(data, "sem_graph")){
    if("element" %in% names(dots)){
      these_elements <- dots[["element"]][dots[["element"]] %in% names(data)]
    } else {
      these_elements <- formals(get("all_fun.sem_graph"), envir = environment())$element
    }
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
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}

#' @export
#' @param size Atomic character vector, indicating which size to assign to
#' the selected elements.
#' @rdname all_fun
size_reg <- function(data, size = 1, ...){
  dots <- list(...)
  if(inherits(data, "sem_graph")){
    if("element" %in% names(dots)){
      these_elements <- dots[["element"]][dots[["element"]] %in% names(data)]
    } else {
      these_elements <- formals(get("all_fun.sem_graph"), envir = environment())$element
    }
    for(el in these_elements){
      if(!"size" %in% names(data[[el]])){
        data[[el]]$size <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"size" %in% names(data)){
      data$size <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "~")
  cl[["expr"]] <- str2lang(paste0("size = ", deparse(size)))
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}

#' @export
#' @param alpha Atomic character vector, indicating which alpha to assign to
#' the selected elements.
#' @rdname all_fun
alpha_reg <- function(data, alpha = 1, ...){
  dots <- list(...)
  if(inherits(data, "sem_graph")){
    if("element" %in% names(dots)){
      these_elements <- dots[["element"]][dots[["element"]] %in% names(data)]
    } else {
      these_elements <- formals(get("all_fun.sem_graph"), envir = environment())$element
    }
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
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}


#' @export
#' @rdname all_fun
all_loading <- function(data, expr, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(op == "=~")
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
hide_loading <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(op == "=~")
  cl[["expr"]] <- quote({ show = FALSE })
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
show_loading <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(op == "=~")
  cl[["expr"]] <- quote({ show = TRUE })
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
color_loading <- function(data, ...){
  browser()
  cl <- match.call()
  cl[["condition"]] <- substitute(op == "=~")
  cl[["expr"]] <- quote({ show = TRUE })
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param colour Atomic character vector, indicating which colour to assign to
#' the selected elements.
#' @rdname all_fun
colour_loading <- function(data, colour = "black", ...){
  dots <- list(...)
  if(inherits(data, "sem_graph")){
    if("element" %in% names(dots)){
      these_elements <- dots[["element"]][dots[["element"]] %in% names(data)]
    } else {
      these_elements <- formals(get("all_fun.sem_graph"), envir = environment())$element
    }
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
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}

#' @export
#' @param color Atomic character vector, indicating which color to assign to
#' the selected elements.
#' @rdname all_fun
color_loading <- function(data, color = "black", ...){
  dots <- list(...)
  if(inherits(data, "sem_graph")){
    if("element" %in% names(dots)){
      these_elements <- dots[["element"]][dots[["element"]] %in% names(data)]
    } else {
      these_elements <- formals(get("all_fun.sem_graph"), envir = environment())$element
    }
    for(el in these_elements){
      if(!"color" %in% names(data[[el]])){
        data[[el]]$color <- "black"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"color" %in% names(data)){
      data$color <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "=~")
  cl[["expr"]] <- str2lang(paste0("color = ", deparse(color)))
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}

#' @export
#' @param linetype Atomic character vector, indicating which linetype to assign to
#' the selected elements.
#' @rdname all_fun
linetype_loading <- function(data, linetype = 1, ...){
  dots <- list(...)
  if(inherits(data, "sem_graph")){
    if("element" %in% names(dots)){
      these_elements <- dots[["element"]][dots[["element"]] %in% names(data)]
    } else {
      these_elements <- formals(get("all_fun.sem_graph"), envir = environment())$element
    }
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
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}

#' @export
#' @param size Atomic character vector, indicating which size to assign to
#' the selected elements.
#' @rdname all_fun
size_loading <- function(data, size = 1, ...){
  dots <- list(...)
  if(inherits(data, "sem_graph")){
    if("element" %in% names(dots)){
      these_elements <- dots[["element"]][dots[["element"]] %in% names(data)]
    } else {
      these_elements <- formals(get("all_fun.sem_graph"), envir = environment())$element
    }
    for(el in these_elements){
      if(!"size" %in% names(data[[el]])){
        data[[el]]$size <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"size" %in% names(data)){
      data$size <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "=~")
  cl[["expr"]] <- str2lang(paste0("size = ", deparse(size)))
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}

#' @export
#' @param alpha Atomic character vector, indicating which alpha to assign to
#' the selected elements.
#' @rdname all_fun
alpha_loading <- function(data, alpha = 1, ...){
  dots <- list(...)
  if(inherits(data, "sem_graph")){
    if("element" %in% names(dots)){
      these_elements <- dots[["element"]][dots[["element"]] %in% names(data)]
    } else {
      these_elements <- formals(get("all_fun.sem_graph"), envir = environment())$element
    }
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
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}


#' @export
#' @rdname all_fun
all_fixed_loading <- function(data, expr, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(op == "=~" & is.na(pval))
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
hide_fixed_loading <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(op == "=~" & is.na(pval))
  cl[["expr"]] <- quote({ show = FALSE })
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
show_fixed_loading <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(op == "=~" & is.na(pval))
  cl[["expr"]] <- quote({ show = TRUE })
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
color_fixed_loading <- function(data, ...){
  browser()
  cl <- match.call()
  cl[["condition"]] <- substitute(op == "=~" & is.na(pval))
  cl[["expr"]] <- quote({ show = TRUE })
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param colour Atomic character vector, indicating which colour to assign to
#' the selected elements.
#' @rdname all_fun
colour_fixed_loading <- function(data, colour = "black", ...){
  dots <- list(...)
  if(inherits(data, "sem_graph")){
    if("element" %in% names(dots)){
      these_elements <- dots[["element"]][dots[["element"]] %in% names(data)]
    } else {
      these_elements <- formals(get("all_fun.sem_graph"), envir = environment())$element
    }
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
  cl[["condition"]] <- substitute(op == "=~" & is.na(pval))
  cl[["expr"]] <- str2lang(paste0("colour = ", deparse(colour)))
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}

#' @export
#' @param color Atomic character vector, indicating which color to assign to
#' the selected elements.
#' @rdname all_fun
color_fixed_loading <- function(data, color = "black", ...){
  dots <- list(...)
  if(inherits(data, "sem_graph")){
    if("element" %in% names(dots)){
      these_elements <- dots[["element"]][dots[["element"]] %in% names(data)]
    } else {
      these_elements <- formals(get("all_fun.sem_graph"), envir = environment())$element
    }
    for(el in these_elements){
      if(!"color" %in% names(data[[el]])){
        data[[el]]$color <- "black"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"color" %in% names(data)){
      data$color <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "=~" & is.na(pval))
  cl[["expr"]] <- str2lang(paste0("color = ", deparse(color)))
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}

#' @export
#' @param linetype Atomic character vector, indicating which linetype to assign to
#' the selected elements.
#' @rdname all_fun
linetype_fixed_loading <- function(data, linetype = 1, ...){
  dots <- list(...)
  if(inherits(data, "sem_graph")){
    if("element" %in% names(dots)){
      these_elements <- dots[["element"]][dots[["element"]] %in% names(data)]
    } else {
      these_elements <- formals(get("all_fun.sem_graph"), envir = environment())$element
    }
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
  cl[["condition"]] <- substitute(op == "=~" & is.na(pval))
  cl[["expr"]] <- str2lang(paste0("linetype = ", deparse(linetype)))
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}

#' @export
#' @param size Atomic character vector, indicating which size to assign to
#' the selected elements.
#' @rdname all_fun
size_fixed_loading <- function(data, size = 1, ...){
  dots <- list(...)
  if(inherits(data, "sem_graph")){
    if("element" %in% names(dots)){
      these_elements <- dots[["element"]][dots[["element"]] %in% names(data)]
    } else {
      these_elements <- formals(get("all_fun.sem_graph"), envir = environment())$element
    }
    for(el in these_elements){
      if(!"size" %in% names(data[[el]])){
        data[[el]]$size <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"size" %in% names(data)){
      data$size <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "=~" & is.na(pval))
  cl[["expr"]] <- str2lang(paste0("size = ", deparse(size)))
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}

#' @export
#' @param alpha Atomic character vector, indicating which alpha to assign to
#' the selected elements.
#' @rdname all_fun
alpha_fixed_loading <- function(data, alpha = 1, ...){
  dots <- list(...)
  if(inherits(data, "sem_graph")){
    if("element" %in% names(dots)){
      these_elements <- dots[["element"]][dots[["element"]] %in% names(data)]
    } else {
      these_elements <- formals(get("all_fun.sem_graph"), envir = environment())$element
    }
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
  cl[["condition"]] <- substitute(op == "=~" & is.na(pval))
  cl[["expr"]] <- str2lang(paste0("alpha = ", deparse(alpha)))
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}


#' @export
#' @rdname all_fun
all_mean <- function(data, expr, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(op == "~1")
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
hide_mean <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(op == "~1")
  cl[["expr"]] <- quote({ show = FALSE })
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
show_mean <- function(data, ...){
  cl <- match.call()
  cl[["condition"]] <- substitute(op == "~1")
  cl[["expr"]] <- quote({ show = TRUE })
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @rdname all_fun
color_mean <- function(data, ...){
  browser()
  cl <- match.call()
  cl[["condition"]] <- substitute(op == "~1")
  cl[["expr"]] <- quote({ show = TRUE })
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}
#' @export
#' @param colour Atomic character vector, indicating which colour to assign to
#' the selected elements.
#' @rdname all_fun
colour_mean <- function(data, colour = "black", ...){
  dots <- list(...)
  if(inherits(data, "sem_graph")){
    if("element" %in% names(dots)){
      these_elements <- dots[["element"]][dots[["element"]] %in% names(data)]
    } else {
      these_elements <- formals(get("all_fun.sem_graph"), envir = environment())$element
    }
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
  cl[["condition"]] <- substitute(op == "~1")
  cl[["expr"]] <- str2lang(paste0("colour = ", deparse(colour)))
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}

#' @export
#' @param color Atomic character vector, indicating which color to assign to
#' the selected elements.
#' @rdname all_fun
color_mean <- function(data, color = "black", ...){
  dots <- list(...)
  if(inherits(data, "sem_graph")){
    if("element" %in% names(dots)){
      these_elements <- dots[["element"]][dots[["element"]] %in% names(data)]
    } else {
      these_elements <- formals(get("all_fun.sem_graph"), envir = environment())$element
    }
    for(el in these_elements){
      if(!"color" %in% names(data[[el]])){
        data[[el]]$color <- "black"
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"color" %in% names(data)){
      data$color <- "black"
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "~1")
  cl[["expr"]] <- str2lang(paste0("color = ", deparse(color)))
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}

#' @export
#' @param linetype Atomic character vector, indicating which linetype to assign to
#' the selected elements.
#' @rdname all_fun
linetype_mean <- function(data, linetype = 1, ...){
  dots <- list(...)
  if(inherits(data, "sem_graph")){
    if("element" %in% names(dots)){
      these_elements <- dots[["element"]][dots[["element"]] %in% names(data)]
    } else {
      these_elements <- formals(get("all_fun.sem_graph"), envir = environment())$element
    }
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
  cl[["condition"]] <- substitute(op == "~1")
  cl[["expr"]] <- str2lang(paste0("linetype = ", deparse(linetype)))
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}

#' @export
#' @param size Atomic character vector, indicating which size to assign to
#' the selected elements.
#' @rdname all_fun
size_mean <- function(data, size = 1, ...){
  dots <- list(...)
  if(inherits(data, "sem_graph")){
    if("element" %in% names(dots)){
      these_elements <- dots[["element"]][dots[["element"]] %in% names(data)]
    } else {
      these_elements <- formals(get("all_fun.sem_graph"), envir = environment())$element
    }
    for(el in these_elements){
      if(!"size" %in% names(data[[el]])){
        data[[el]]$size <- 1
      }
    }
  }
  if(inherits(data, "data.frame")){
    if(!"size" %in% names(data)){
      data$size <- 1
    }
  }
  cl <- match.call()
  cl[["data"]] <- data
  cl[["condition"]] <- substitute(op == "~1")
  cl[["expr"]] <- str2lang(paste0("size = ", deparse(size)))
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}

#' @export
#' @param alpha Atomic character vector, indicating which alpha to assign to
#' the selected elements.
#' @rdname all_fun
alpha_mean <- function(data, alpha = 1, ...){
  dots <- list(...)
  if(inherits(data, "sem_graph")){
    if("element" %in% names(dots)){
      these_elements <- dots[["element"]][dots[["element"]] %in% names(data)]
    } else {
      these_elements <- formals(get("all_fun.sem_graph"), envir = environment())$element
    }
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
  cl[["condition"]] <- substitute(op == "~1")
  cl[["expr"]] <- str2lang(paste0("alpha = ", deparse(alpha)))
  cl[[1L]] <- quote(all_fun)
  eval.parent(cl)
}


