#' @method table_results mixture_list
#' @export
table_results.mixture_list <- function (x, columns = c("label", "est_sig", "se", "pval", "confint", "group", "class", "level"), digits = 2, format_numeric = TRUE, ...)
{
  cl <- match.call()
  out <- suppressWarnings(lapply(x, function(thismod){
    cl[[1L]] <- str2lang("tidySEM::table_results")
    cl[["x"]] <- thismod
    tmp <- eval.parent(cl)
    tmp$model <- thismod$name
    if(!"class" %in% names(tmp) & thismod$name == "mix1"){
      tmp$class <- "class1"
    }
    tmp
  }))
  out <- bind_list(out)
  rownames(out) <- NULL
  out
}
#' @method table_results MxModel
#' @export
#' @importFrom stats pnorm
table_results.MxModel <- function (x, columns = c("label", "est_sig", "se", "pval", "confint", "group", "class", "level"), digits = 2, format_numeric = TRUE, ...)
{
  # Multigroup:
  # attr(attr(fit,"runstate")$fitfunctions$mg.fitfunction, "groups")
  if(!x@.wasRun) {
    stop("This model has not been run yet. Use `model = tidySEM::run_mx(model)` or `model = OpenMx::mxRun(model)` to estimate a model.")
  }
  messages <- attr(x, "warnings")
  messages <- mx_convergence(x)$warnings
  Args <- list(x = x)
  digits <- force(digits)
  sum_x <- summary(x)
  results <- sum_x$parameters
  results$free <- TRUE
  # Add fixed pars
  if(!is_remix(x)){
    results_fixed <- .mxfixedpars(x)
    if(!is.null(results_fixed)){
      if(any(!results_fixed$name %in% results$name)){
        results <- bind_list(list(results, results_fixed[!results_fixed$name %in% results$name, , drop = FALSE]))
      }
    }
  }

  results$openmx_label <- results$name
  matrixnames <- grepl("\\[.+?\\]", results$name)
  if(any(matrixnames)){
    results$label[matrixnames] <- NA
    results$name[!matrixnames] <- get_mx_names(x, results)[!matrixnames]
  }

  # Add standardized --------------------------------------------------------
  if((is.null(columns) || any(endsWith(columns, "_std"))) && !is_remix(x)){
    # Conditional, to save time
    results_std <- tryCatch(OpenMx::mxStandardizeRAMPaths(x, SE = TRUE), error = function(e) NULL)
    if (!is.null(results_std)) {

      if (is.list(results_std) && !is.data.frame(results_std)) {
        results_std <- lapply(names(results_std), function(n) {
          tab <- results_std[[n]]
          tab$matrix <- paste(n, tab$matrix, sep = ".")
          tab
        })
        results_std <- bind_list(results_std)
      }

      # Remove redundant correlations
      remove_these <- results_std$name[endsWith(results_std$matrix, ".S") | results_std$matrix == "S"]
      remove_these <- remove_these[!remove_these %in% results$name]
      if(length(remove_these) > 0){
        these_rows <- which(results_std$name %in% remove_these)
        flip_S <- results_std[these_rows, , drop = FALSE]
        names(flip_S)[match(c("row", "col"), names(flip_S))] <- c("col", "row")
        flip_S$name <- gsub("\\[(\\d+),(\\d+)\\]$", "\\[\\2,\\1\\]", flip_S$name)
        results_std[these_rows, ] <- flip_S[, names(results_std)]
      }
      # Rename columns
      renamez <- c(
        "Raw.Value" = "Estimate",
        "Raw.SE" = "Std.Error",
        "Std.Value" = "std_est",
        "Std.SE" = "std_se",
        "label" = "openmx_label"
      )

      idx <- match(names(renamez), names(results_std))
      keep <- !is.na(idx)
      names(results_std)[idx[keep]] <- unname(renamez[keep])

      # Compute p-value and CI
      results_std$pval_std <- 2*pnorm(abs(results_std$std_est)/results_std$std_se, lower.tail = FALSE)
      results_std$confint_std <- conf_int(results_std$std_est, se = results_std$std_se)
      # Clean up name vs label
      tab <- merge(results, results_std, by = "name", all = TRUE)
      results <- two_to_one(tab)
    }
  }

  # Add algebras ------------------------------------------------------------
  algs <- alg_to_res(x)
  if (is.data.frame(algs) && nrow(algs) > 0L) {
    algs$free <- NA
    algs$openmx_label <- algs$name
    results <- bind_list(list(results, algs))
  }

  # Check for robust standard errors
  se_robust <- attr(x, "se_robust")
  if (!is.null(se_robust)) {
    # Match using original OpenMx parameter labels
    idx <- match(results$openmx_label, se_robust$label)
    keep <- which(!is.na(idx))
    if (length(keep)) {
      results$Std.Error[keep] <- se_robust$se[idx[keep]]
    } else {
      se_robust <- NULL
    }
  }
  # Label groups ------------------------------------------------------------
  results <- .label_groups_internal(x, results)


  # Drop empty rows
  misrows <- rowSums(is.na(results)) == ncol(results)
  if(any(misrows)) results <- results[-which(misrows), , drop = FALSE]

  # Normalize the output
  results$est <- results$Estimate
  results$lhs <- results$row
  results$rhs <- results$col
  results$op <- NA
  results$op[results$matrix == "A"] <- "~"
  if(!is.null(results[["formula"]])){
    isalg <- !is.na(results$formula)
    results$op[isalg] <- ":="
  }

  results$op[results$matrix == "Thresholds"] <- "|"
  results$op[results$matrix == "S"] <- "~~"
  results$op[results$matrix == "M"] <- "~1"
  results$op[is.na(results$matrix)] <- "="
  flip_lhsrhs <- which(results$op %in% c("~1", "|"))
  results$lhs[flip_lhsrhs] <- results$col[flip_lhsrhs]
  results$rhs[flip_lhsrhs] <- results$row[flip_lhsrhs]
  lvs <- unique(unlist(from_submodels(x, "latentVars")))
  obsv <- unique(unlist(from_submodels(x, "manifestVars")))
  # These are loadings
  fac_load <- which(!is.na(results$op) & results$op == "~" & results$rhs %in% lvs & results$lhs %in% obsv)
  results$rhs[fac_load] <- results$row[fac_load]
  results$lhs[fac_load] <- results$col[fac_load]
  results$op[fac_load] <- "=~"

  # Calculate Wald confidence intervals
  # Wald intervals only for estimated or derived parameters
  valid_wald <- (is.na(results$free) | results$free) &
    is.finite(results$Estimate) &
    is.finite(results$Std.Error) &
    results$Std.Error > 0

  results$confint <- conf_int(
    results$Estimate,
    se = results$Std.Error
  )

  results$confint[!valid_wald] <- NA_character_
  # Record CI type
  results$ci_type <- ifelse(
    valid_wald,
    "Wald",
    NA_character_
  )

  # Check for presence of profile likelihood confidence intervals
  ci_x <- sum_x[["CI"]]
  if (!is.null(ci_x) && NROW(ci_x) > 0L) {
    ci_x <- as.data.frame(ci_x)
    # Identify parameter names
    ci_x$name <- rownames(ci_x)
    # Check required columns
    if (all(c("lbound", "ubound") %in% names(ci_x))) {
      # Valid interval bounds
      valid <- is.finite(ci_x$lbound) & is.finite(ci_x$ubound) & ci_x$lbound <= ci_x$ubound
      # Check OpenMx interval convergence diagnostics
      if (all(c("lboundMet", "uboundMet") %in% names(ci_x))) {
        valid <- valid & !is.na(ci_x$lboundMet) & !is.na(ci_x$uboundMet) & ci_x$lboundMet & ci_x$uboundMet
      }
      # Exclude intervals with OpenMx warning notes
      if ("note" %in% names(ci_x)) {
        valid <- valid & (is.na(ci_x$note) | trimws(as.character(ci_x$note)) == "")
      }
      # Match profile intervals to results
      idx <- match_mx_names(
        ci_x$name,
        results$name
      )
      keep <- which(valid & !is.na(idx))
      if (length(keep)) {
        results$confint[idx[keep]] <- conf_int(x = NULL, lb = ci_x$lbound[keep], ub = ci_x$ubound[keep])
        results$ci_type[idx[keep]] <- "Profile"
      }
    }
  }

  results$pval <- rep(NA_real_, nrow(results))

  results$pval[valid_wald] <- 2 * pnorm(
    abs(results$Estimate[valid_wald]) /
      results$Std.Error[valid_wald],
    lower.tail = FALSE
  )

  results$est_sig <- est_sig(results$est, sig = results$pval)
  results[c("estimate", "Estimate")] <- NULL
  names(results)[match(c("Std.Error"), names(results))] <- c("se")
  names(results) <- tolower(names(results))
  lav_labs <- mx_to_lavaan_labels(results)
  results <- cbind(results, lav_labs)
  # Drop internal stuff
  internalstuff <- grepl("(mat_dev|mat_ones)", results$name)
  internalstuff[is.na(internalstuff)] <- FALSE
  if(any(internalstuff)) results <- results[!internalstuff, ]
  if (is_remix(x) && all(c("category", "formula") %in% names(results))) {
    idx <- which(results$category == "Weights")
    results$formula[idx] <- NA
  }
  # Drop empty columns
  miscols <- colSums(is.na(results)) == nrow(results)
  miscols[names(miscols) %in% c("se", "confint", "ci_type")] <- FALSE
  if(any(miscols)) results <- results[, !miscols, drop = FALSE]
  # Make uniform column names
  renam <- c(std.value = "est_std", std.se = "se_std")
  idx <- match(names(renam), names(results))
  keep <- !is.na(idx)

  names(results)[idx[keep]] <- unname(renam[keep])
  # Impose similar order to lavaan
  firstcols <- c("lhs", "op", "rhs", "est", "se", "pval", "confint", "est_sig",
                 "est_std", "se_std", "pval_std", "confint_std", "est_sig_std",
                 "label", "free")
  results <- results[, c(firstcols[firstcols %in% names(results)], names(results)[!names(results) %in% firstcols]), drop = FALSE]
  # Add group/class to label
  if("group" %in% names(results)) results$label <- paste0(results$label, ".", results$group)
  if("class" %in% names(results)) results$label <- paste0(results$label, ".", results$class)
  results <- results[order(results$name), , drop = FALSE]

  # Add message about robust parameters, using their definitive names
  if(!is.null(se_robust)){
    messages <- c(messages, paste0(se_robust$type[1], " standard errors were used for: ", paste(results$label[match(se_robust$label, results$openmx_label)], collapse = ", "), "."))
  }
  if(!is.null(columns)) {
    keep <- intersect(columns, names(results))
    unknown <- setdiff(columns, c("group", "level", names(results)))
    if (length(unknown)) {
      warning("Unavailable columns requested: ", paste(unknown, collapse = ", "), call. = FALSE)
    }
    results <- results[, keep, drop = FALSE]
  } else {
    order_cols <- intersect("label", names(results))
    remaining_cols <- setdiff(names(results), order_cols)
    results <- results[ , c(order_cols, remaining_cols), drop = FALSE]
  }
  # Format using digits
  if(format_numeric){
    value_columns <- names(results)[can_be_numeric(results)]
    results[, value_columns] <- lapply(results[, value_columns],
                                       format_with_na, digits = digits, format = "f")
  }

  rownames(results) <- NULL

  class(results) <- c("tidy_results", class(results))
  attr(results, "messages") <- messages
  return(results)
}

#' @export
print.tidy_results <- function(x, ...) {
  messages <- attr(x, "messages")
  print.data.frame(x, ...)
  if (length(messages)) {
    cat("\n")
    cat(paste0("Note: ", messages, collapse = "\n"))
    cat("\n")
  }
  invisible(x)
}

submodels <- function(x, results, cols = c("name", "matrix", "row", "col", "Estimate", "Std.Error", "lbound",
                                           "ubound", "lboundMet", "uboundMet"), ...){
  if(!has_submod(x, depth = 1)){
    submod <- names(attr(x, "submodels"))
    #lapply(submod, function(i){ alg_to_res(x[[i]]) })
    thisgroup <- rep(NA_character_, times = nrow(results))
    from_group <- grepl(paste0("(", paste0(submod, collapse = "|"), ")\\."), results$matrix)
    thisgroup[from_group] <- gsub(paste0("^.{0,}(", paste0(submod, collapse = "|"), ").{0,}$"), "\\1", results$matrix[from_group])
    thename <- "group"
    if(inherits(x$expectation, "MxExpectationMixture")){
      thename <- "class"
    }
    existingnames <- sum(gregexpr(thename, paste0(names(results), collapse = ""))[[1]] > 0)
    if(existingnames > 0) thename <- paste0(thename, ".", existingnames, collapse = "")
    results[[thename]] <- thisgroup
    results[["matrix"]][from_group] <- gsub(paste0("(", paste0(submod, collapse = "|"), ")\\."), "", results$matrix[from_group])
    #results
    #dup_pars <- table(names(unlist(lapply(submod, function(i){ omxGetParameters(x[[i]]) }))))
    dup_pars <- table(names(unlist(lapply(submod, function(i){ OpenMx::omxGetParameters(x[[i]]) }))))
    dup_pars <- names(dup_pars)[dup_pars > 1]
    for(i in submod){
      submodpars <- OpenMx::omxGetParameters(x[[i]])
      if(any(dup_pars %in% names(submodpars))){
        add_these <- results[results$name %in% dup_pars, ]
        add_these <- add_these[!add_these[[thename]] == i, ]
        if(nrow(add_these) > 0){
          add_these[[thename]] <- i
          results <- rbind(results, add_these)
        }
      }
    }
    return(results)
  } else {
    subs <- names(attr(x, "submodels"))
    results <- lapply(subs, function(i){submodels(x[[i]], results, cols = cols)})
    results <- do.call(data.frame, results)
    remthese <- rowSums(sapply(paste0(cols, "."), grepl, x = names(results), fixed = TRUE)) > 0
    results[remthese] <- NULL
  }
  # remove all of this stuff from $matrix
  for(i in 1:nrow(results)){
    results[["matrix"]][i] <- gsub(paste0("(", paste0(results[i, -which(names(results) %in% cols)], collapse = "|"), ")\\."), "", results[["matrix"]][i])
  }
  return(results)
}

from_submodels <- function(x, what = NULL, ...){
  out <- do.call(`@`, list(x, what))
  if(has_submod(x)){
    submod <- names(attr(x, "submodels"))
    out <- c(out, lapply(submod, function(i){ from_submodels(x[[i]], what = what) }))
  }
  return(out)
}

.label_groups_internal <- function(x, results, ...){
  cols <- names(results)
  if(!has_submod(x, depth = 1)){
    submod <- names(attr(x, "submodels"))
    thisgroup <- rep(NA_character_, times = nrow(results))
    from_group <- grepl(paste0("(", paste0(submod, collapse = "|"), ")\\."), results$matrix)
    thisgroup[from_group] <- gsub(paste0("^.{0,}(", paste0(submod, collapse = "|"), ").{0,}$"), "\\1", results$matrix[from_group])
    # Default name: group
    thename <- "group"
    # Alternative names:
    if(inherits(x$expectation, "MxExpectationMixture") | isTRUE(attr(x, "tidySEM") == "mixture")){
      thename <- "class"
      if(!inherits(x$expectation, "MxExpectationMixture")) thisgroup[results$matrix %in% c("S", "M")] <- "class1"
    }

    existingnames <- sum(gregexpr(thename, paste0(names(results), collapse = ""))[[1]] > 0)
    if(existingnames > 0) thename <- paste0(thename, ".", existingnames, collapse = "")
    results[[thename]] <- thisgroup
    results[["matrix"]][from_group] <- gsub(paste0("(", paste0(submod, collapse = "|"), ")\\."), "", results$matrix[from_group])
    dup_pars <- table(names(unlist(lapply(submod, function(i){ OpenMx::omxGetParameters(x[[i]]) }))))
    dup_pars <- names(dup_pars)[dup_pars > 1]
    for(i in submod){
      submodpars <- OpenMx::omxGetParameters(x[[i]])
      if(any(dup_pars %in% names(submodpars))){
        add_these <- results[results$name %in% dup_pars, ]
        add_these <- add_these[!add_these[[thename]] == i, ]
        if(nrow(add_these) > 0){
          add_these[[thename]] <- i
          results <- rbind(results, add_these)
        }
      }
    }
  } else {
    subs <- names(attr(x, "submodels"))
    results <- lapply(subs, function(i){.label_groups_internal(x[[i]], results, cols = cols)})
    results <- do.call(data.frame, results)
    remthese <- rowSums(sapply(paste0(cols, "."), grepl, x = names(results), fixed = TRUE)) > 0
    results[remthese] <- NULL
  }
  # remove all of this stuff from $matrix
  for(i in 1:nrow(results)){
    results[["matrix"]][i] <- gsub(paste0("(", paste0(results[i, -which(names(results) %in% cols)], collapse = "|"), ")\\."), "", results[["matrix"]][i])
  }
  return(results)
}

has_submod <- function(x, depth = 0L) {
  subs <- x@submodels
  if (depth == 0L) {
    return(length(subs) > 0L)
  }
  if (!length(subs)) return(FALSE)
  any(vapply(subs, has_submod, logical(1), depth = depth - 1L))
}

# alg_names <- function(x, ...){
#   if(!is.null(names(x@algebras))){
#     out <- list(names(x@algebras))
#   } else {
#     out <- list()
#   }
#   if(tidySEM:::has_submod(x)){
#     subs <- names(attr(x, "submodels"))
#     fromsubs <- lapply(subs, function(thesub){alg_names(x[[thesub]])})
#     names(fromsubs) <- subs
#     out <- c(out, fromsubs)
#   } else {
#     return(out)
#   }
#   out <- unlist(out, use.names = TRUE)
#   paste(names(out), out, sep = ".")
# }

# @importFrom OpenMx mxSE omxGetParameters
# get_algebras <- function(x, ...){
#   if(!isTRUE(requireNamespace("OpenMx", quietly = TRUE))) {
#     return(NULL)
#   }
#   cl <- match.call()
#   cl[[1L]] <- str2lang("tidySEM:::.get_algebras_internal")
#   algs <- eval.parent(cl)
#   Estimate <- unlist_mx(algs, "result")
#   if(is.null(Estimate)) return(NULL)
#   out <- data.frame(name = names(Estimate),
#                     matrix =  unlist_mx(algs, element = "name"),
#                     row = unlist_mx(algs, element = "formula"),
#                     col = ":=",
#                     Estimate = Estimate, row.names = NULL)
#   out$Std.Error <- unlist(lapply(out$name, function(thispar){ tryCatch(OpenMx::mxSE(thispar, model = x, silent = TRUE), error = function(e){ NA }) }))
#   out
# }

# alg_to_res <- function(x, ...){
#   out <- .alg_to_res_internal(x, ...)
#   out <- bind_list(flat(out))
#   out$Std.Error <- unlist(lapply(out$name, function(thispar){ tryCatch(OpenMx::mxSE(thispar, model = x, silent = TRUE), error = function(e){ NA }) }))
#   out
# }

alg_to_res <- function(x, ...) {

  out <- flat(.alg_to_res_internal(x, ...))

  # No algebra results
  if (!length(out)) {
    return(data.frame(
      name = character(),
      Std.Error = numeric()
    ))
  }

  out <- bind_list(out)

  if (!is.data.frame(out)) {
    stop("Could not combine algebra results.")
  }

  if (!"name" %in% names(out)) {
    stop("Algebra results must contain a 'name' column.")
  }

  out$Std.Error <- vapply(
    out$name,
    function(par) {
      tryCatch(
        as.numeric(OpenMx::mxSE(
          par,
          model = x,
          silent = TRUE
        ))[1L],
        error = function(e) NA_real_
      )
    },
    numeric(1)
  )

  out
}

flat <- function(x) {
  if (is.data.frame(x)) return(list(x))
  if (!is.list(x) || length(x) == 0L) return(list())
  unlist(
    lapply(x, flat),
    recursive = FALSE
  )
}

.alg_to_res_internal <- function(x, ...){
  if(is_remix(x)){
    algs <- "weights"
  } else {
    algs <- names(x@algebras)
  }
  out <- list()
  if(!is.null(algs)){
    addthis <- list()
    for(thisalg in algs){
      val <- as.vector(x[[thisalg]]$result)
      dims <- dim(x[[thisalg]]$result)
      if(!sum(dims) == 2){
        nams <- paste0(x[[thisalg]]$name, "[",
                       paste(rep(1:dims[1], dims[2]),
                             rep(1:dims[2], each = dims[1]), sep = ","), "]")
      } else {
        nams = x[[thisalg]]$name
      }
      dimnam <- dimnames(x[[thisalg]])
      rownam <- rep(dimnam[[1]], length(dimnam[[2]]))
      if(is.null(rownam)) rownam <- seq_len(dims[1])
      colnam <- rep(dimnam[[2]], each = length(dimnam[[1]]))
      if(is.null(colnam)) colnam <- seq_len(dims[2])
      addalg <- data.frame(
        name = nams,
        matrix = x[[thisalg]]$name,
        row = rownam,
        col = colnam,
        Estimate = val,
        formula = deparse(x[[thisalg]]$formula),
        row.names = NULL
      )

      # Drop unused thresholds
      if (thisalg == "Thresholds" &&
          "mat_dev" %in% names(x@matrices)) {

        padding <- x$mat_dev$labels == "XXXpaddingXXX"
        padding[is.na(padding)] <- FALSE

        addalg <- addalg[!as.vector(padding), , drop = FALSE]
      }

      addthis <- c(addthis, list(addalg))
    }
    names(addthis) <- algs
    out <- c(out, addthis)
  }
  if(has_submod(x)){
    subs <- names(attr(x, "submodels"))
    fromsubs <- lapply(subs, function(thesub){.alg_to_res_internal(x[[thesub]])})
    fromsubs <- lapply(1:length(fromsubs), function(i){
      lapply(fromsubs[[i]], function(thedf){
        thedf$name <- paste(subs[i], thedf$name, sep = ".")
        thedf$matrix <- paste(subs[i], thedf$matrix, sep = ".")
        thedf
      })
    })
    names(fromsubs) <- subs
    out <- c(out, fromsubs)
  }
  out
}

.get_algebras_internal <- function(x, ...){
  algs <- names(x@algebras)
  out <- list()
  if(!is.null(algs)){
    addthis <- list()
    for(thisalg in algs){
      addthis <- c(addthis, x[[thisalg]])
    }
    names(addthis) <- algs
    out <- c(out, addthis)
  }
  if(has_submod(x)){
    subs <- names(attr(x, "submodels"))
    fromsubs <- lapply(subs, function(thesub){.get_algebras_internal(x[[thesub]])})
    names(fromsubs) <- subs
    out <- c(out, fromsubs)
  }
  out
}

# tmp <- get_algebras(resSD)
# alg_names <- names(unlist(tmp))

# unlist_mx <- function(x){
#   lapply(x, function(i){
#     if(inherits(i, "list")){
#       lapply(i, unlist_mx)
#     } else {
#       out <- i$result
#       if(sum(dim(i$result)) == 2){
#         names(out) <- i$name
#       } else {
#         out <- as.vector(out)
#         names(out) <- paste0(i$name, "[",
#                paste(rep(1:dim(i$result)[1], dim(i$result)[2]),
#                      rep(1:dim(i$result)[2], each = dim(i$result)[1]), sep = ","), "]")
#       }
#       out
#     }
#   })
#}

unlist_mx <- function(i, element, ...){
  cl <- match.call()
  cl[[1L]] <- str2lang("tidySEM:::unlist_mx2")
  out <- unlist(eval.parent(cl))
  if(!is.null(out)) names(out) <- gsub(".[", "[", names(out), fixed = TRUE)
  out
}
unlist_mx2 <- function(i, element, ...){
  if(inherits(i, "list")){
    out <- lapply(i, unlist_mx2, element = element, ...)
    out
  } else {
    dims <- dim(i$result)
    if(element == "result") out <- as.vector(i$result)
    if(element == "name") out <- rep(i$name, length(i$result))
    if(element == "formula") out <- rep(deparse(i$formula), length(i$result))
    if(!sum(dims) == 2){
      names(out) <- paste0("[",
                           paste(rep(1:dims[1], dims[2]),
                                 rep(1:dims[2], each = dims[1]), sep = ","), "]")
    }
    out
  }
}

attr_mx <- function(i, att, ...){
  if(inherits(i, "list")){
    out <- lapply(i, attr_mx, att = att, ...)
    out
  } else {
    tryCatch({do.call(att, list(i))}, error = function(e){NULL})
  }
}

threshdims_mx <- function(i, ...){
  if(inherits(i, "list")){
    out <- lapply(i, threshdims_mx, ...)
    out
  } else {
    tryCatch({
      out <- dimnames(i)
      if(!all(startsWith(out[[1]], "th_"))){
        stop()
      }
      cbind(rep(out[[2]], each = length(out[[1]])),
            rep(out[[1]], length(out[[2]]))
      )
    }, error = function(e){NULL})
  }
}


# m1 <- mxRename(mxModel(as_ram("y ~ a*m + x
# m ~ b*y
# ind1 := a * b", meanstructure= TRUE), mxData(df[1:75,], type = "raw")), "m1")
# m2 <- mxRename(mxModel(as_ram("y ~ a2*m + x
# m ~ b2*y
# ind2 := a2 * b2", meanstructure= TRUE), mxData(df[76:150,], type = "raw")), "m2")
# m3 <- mxModel("mg", m1, m2, mxFitFunctionMultigroup(c("m1", "m2")))
# resMG <- mxRun(m3)
# summary(resMG)
# table_results(resMG)
# resM <- mxRun(mxModel(as_ram(tmp), mxData(df, type = "raw")))
#
#
# mSD <- mxRename(mxModel(as_ram("y ~ a*m + x
# m ~ b*y", meanstructure= TRUE), mxData(df, type = "raw"), mxAlgebra(sqrt(S), name = "sd") ), "msd")
# resSD <- mxRun(mSD)


# get_algebras <- function(x, ...){
#   if(!)
#   algs <- names(x@algebras)
#   if(!is.null(algs)){
#     out <- lapply(algs, function(i){
#       thisalg <- x[[i]]
#       if(sum(dim(thisalg$result)) == 2){
#         data.frame(name = i, row = deparse(thisalg$formula), col = i, Estimate = as.numeric(thisalg$result), Std.Error = tryCatch(as.numeric(suppressMessages(mxSE(i, x))), error = function(e){NA}))
#       } else {
#         return(NULL)
#       }
#       #tab <- as.data.frame.table(thisalg$result)
#       #tab$name <- paste0()

#     })
#     return(bind_list(out))
#   } else {
#     return(NULL)
#   }
# }

mx_to_lavaan_labels <- function(x){
  out <- x$openmx_label
  cat <- rep(NA, length(out))
  # Means
  these <- which(x$op == "~1")
  out[these] <- paste0("Means.", x$lhs[these])
  cat[these] <- "Means"
  # Vars
  these <- which(x$op == "~~" & (x$rhs == x$lhs))
  out[these] <- paste0("Variances.", x$lhs[these])
  cat[these] <- "Variances"
  # covs
  these <- which(x$op == "~~" & !(x$rhs == x$lhs))
  out[these] <- paste0("Covariances.", x$lhs[these], ".WITH.", x$rhs[these])
  cat[these] <- "Covariances"
  # lv def
  these <- which(x$op == "=~")
  out[these] <- paste0("Loadings.", x$lhs[these], ".BY.", x$rhs[these])
  cat[these] <- "Loadings"
  # reg
  these <- which(x$op == "~")
  out[these] <- paste0("Regressions.", x$lhs[these], ".ON.", x$rhs[these])
  cat[these] <- "Regressions"
  # thresh
  these <- which(x$matrix == "Thresholds")
  #out[these] <- paste0("Thresholds.", x$lhs[these], ".ON.", x$rhs[these])
  cat[these] <- "Thresholds"
  these <- which(x$matrix == "weights")
  #out[these] <- paste0("Thresholds.", x$lhs[these], ".ON.", x$rhs[these])
  cat[these] <- "Weights"
  cbind(label = out, Category = cat)
}


get_mx_names <- function(x, res){
  out <- rep(NA, nrow(res))
  if(length(x@submodels) > 0){
    group_pars <- vector("numeric")
    group_par_vals <- vector("character")
    for(nam in names(x@submodels)){
      group_pars_thisgrp <- which(startsWith(res$matrix, paste0(nam, ".")))
      group_pars <- append(group_pars, group_pars_thisgrp)
      resgrp <- res[group_pars_thisgrp, , drop = FALSE]
      resgrp$matrix <- gsub(paste0("^", nam, "\\."), "", resgrp$matrix)
      group_par_vals <- append(group_par_vals, get_mx_names(x[[nam]], resgrp))
    }
    if(length(group_pars) > 0){
      out[group_pars] <- group_par_vals
    }
    res <- res[-group_pars, , drop = FALSE]
  }
  matcol <- which(names(res) == "matrix")
  rowcol <- which(names(res) == "row")
  colcol <- which(names(res) == "col")
  res_par_vals <- apply(res, 1, function(thisrow){
    tryCatch({
      themat <- x[[thisrow[matcol]]]
      if(!is.null(rownames(themat)) & !is.null(rownames(themat))){
        return(paste0(x$name, ".", thisrow[matcol], "[", paste(match(thisrow[rowcol], rownames(themat$values)), match(thisrow[colcol], colnames(themat$values)), sep = ","), "]"))
      }
      if(is.null(rownames(themat)) & !is.null(colnames(themat)) & nrow(themat) == 1){
        return(paste0(x$name, ".", thisrow[matcol], "[", paste(1, match(thisrow[colcol], colnames(themat$values)), sep = ","), "]"))
      }
      return(NA)
    }, error = function(e){ NA })
  })
  if((length(x@submodels) > 0)){
    if(length(group_pars) > 0){
      out[-group_pars] <- res_par_vals
      return(out)
    }
  } else {
    return(res_par_vals)
  }
}


two_to_one <- function(tab){
  dupcol <- table(gsub("\\.[xy]", "", names(tab)))
  unicol <- names(dupcol)[dupcol != 2]
  dupcol <- names(dupcol)[dupcol == 2]
  mergtab <- data.frame(lapply(dupcol, function(thisc){
    vals <- tab[, startsWith(names(tab), thisc)]
    out <- vals[[1]]
    miss <- rowSums(is.na(vals))
    out[miss == 1 & is.na(out)] <- vals[[2]][miss == 1 & is.na(out)]
    out
  }))
  names(mergtab) <- dupcol
  if(nrow(mergtab) > 0){
    return(cbind(tab[names(tab) %in% unicol], mergtab))
  } else {
    return(tab)
  }

}



match_mx_names <- function(nms, targets) {

  idx <- rep(NA_integer_, length(nms))

  # First pass: exact matches
  for (i in seq_along(nms)) {
    if (is.na(nms[i])) next

    hits <- which(!is.na(targets) & targets == nms[i])
    sources <- which(!is.na(nms) & nms == nms[i])

    if (length(hits) == 1L && length(sources) == 1L) {
      idx[i] <- hits
    }
  }

  # Progressively remove prefixes from unmatched names
  candidates <- nms

  repeat {

    unmatched <- which(is.na(idx) & !is.na(candidates))

    if (!length(unmatched)) break

    # Remove one prefix level
    dots <- regexpr(".", candidates[unmatched], fixed = TRUE)

    can_strip <- dots > 0L

    if (!any(can_strip)) break

    strip_idx <- unmatched[can_strip]

    candidates[strip_idx] <- substring(
      candidates[strip_idx],
      dots[can_strip] + 1L
    )

    # Only match against unused target rows
    available <- setdiff(
      seq_along(targets),
      idx[!is.na(idx)]
    )

    # Check uniqueness on both sides
    for (i in strip_idx) {

      candidate <- candidates[i]

      source_hits <- which(
        is.na(idx) &
          !is.na(candidates) &
          candidates == candidate
      )

      target_hits <- available[
        !is.na(targets[available]) &
          targets[available] == candidate
      ]

      if (length(source_hits) == 1L &&
          length(target_hits) == 1L) {

        idx[i] <- target_hits
        available <- setdiff(available, target_hits)
      }
    }
  }

  idx
}
