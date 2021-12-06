#' @method table_results MxModel
#' @export
#' @importFrom stats pnorm
table_results.MxModel <- function (x, columns = c("label", "est_sig", "se", "pval", "confint", "group", "class", "level"), digits = 2, ...)
{
  # Multigroup:
  # attr(attr(fit,"runstate")$fitfunctions$mg.fitfunction, "groups")
  Args <- list(x = x)
  digits <- force(digits)
  sum_x <- summary(x)
  results <- sum_x$parameters
  results$openmx_label <- results$name
  matrixnames <- grepl("\\[.+?\\]", results$name)
  results$label[matrixnames] <- NA
  results$name[!matrixnames] <- get_mx_names(x, results)[!matrixnames]
# Add standardized --------------------------------------------------------
  if(is.null(columns) | any(grepl("std_", columns))){ # Conditional, to save time
    results_std <- mxStandardizeRAMPaths(x, SE = TRUE)
    if(inherits(results_std, "list")){
      for(n in names(results_std)){
        results_std[[n]]$matrix <- paste(n, results_std[[n]]$matrix, sep = ".")
      }
      results_std <- bind_list(results_std)
      renamez <- c("Raw.Value" = "Estimate", "Raw.SE" = "Std.Error", "Std.Value" = "std_est", "Std.SE" = "std_se", "label" = "openmx_label")
      names(results_std)[match(names(renamez), names(results_std))] <- renamez[names(renamez) %in% names(results_std)]
    }
    # Remove redundant correlations
    remove_these <- results_std$name[endsWith(results_std$matrix, ".S")]
    remove_these <- remove_these[!remove_these %in% results$name]
    if(length(remove_these) > 0){
      these_rows <- which(results_std$name %in% remove_these)
      flip_S <- results_std[these_rows, , drop = FALSE]
      names(flip_S)[match(c("row", "col"), names(flip_S))] <- c("col", "row")
      flip_S$name <- gsub("\\[(\\d+),(\\d+)\\]$", "\\[\\2,\\1\\]", flip_S$name)
      results_std[these_rows, ] <- flip_S[, names(results_std)]
    }
    # Clean up name vs label
    tab <- merge(results, results_std, by = "name", all = TRUE)
    results <- two_to_one(tab)
  }

# Add algebras ------------------------------------------------------------
  algs <- alg_to_res(x)
  algs$openmx_label <- algs$name
  results <- bind_list(list(results, algs))

# Label groups ------------------------------------------------------------
  results <- .label_groups_internal(x, results)
  # if(has_submod(x)){
  #   results <- submodels(x, results, cols = names(results))
  # } else {
  #   algs <- alg_to_res(x)
  #   algs$openmx_label <- algs$name
  #   results <- bind_list(list(results, algs))
  # }
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
  fac_load <- results$op == "~" & results$rhs %in% lvs & results$lhs %in% obsv
  results$rhs[fac_load] <- results$row[fac_load]
  results$lhs[fac_load] <- results$col[fac_load]
  results$op[fac_load] <- "=~"
  results$confint <- conf_int(results$est, se = results$Std.Error)
  if(isTRUE(sum_x[["CI.Requested"]])){
    if(!all(is.na(sum_x[["CI"]]))) {
      ci_x <- data.frame(name = rownames(sum_x$CI),
                         sum_x$CI)
      ci_x$CI <- NA
      ci_x$CI[!is.na(ci_x$lbound) & !is.na(ci_x$ubound)] <- conf_int(x= NULL, lb = ci_x$lbound[!is.na(ci_x$lbound) & !is.na(ci_x$ubound)], ub = ci_x$ubound[!is.na(ci_x$lbound) & !is.na(ci_x$ubound)])
      ci_x <- ci_x[!is.na(ci_x$CI), ]

      results$confint[match(ci_x$name, results$name)] <- ci_x$CI
    }
  }
  results$pvalue <- 2*pnorm(abs(results$Estimate)/results$Std.Error, lower.tail = FALSE)
  results$est_sig <- est_sig(results$est, sig = results$pvalue)
  results[c("estimate", "Estimate")] <- NULL
  miscols <- colSums(is.na(results)) == nrow(results)
  if(any(miscols)) results <- results[, !miscols, drop = FALSE]
  names(results)[match(c("Std.Error"), names(results))] <- c("se")
  names(results) <- tolower(names(results))
  lav_labs <- mx_to_lavaan_labels(results)
  results <- cbind(results, lav_labs)
  # Drop internal stuff
  internalstuff <- grepl("mat_dev", results$name)
  if(any(internalstuff)) results <- results[!internalstuff, ]
  if(!is.null(columns)) {
    results <- results[, na.omit(match(columns, names(results))), drop = FALSE]
  }
  else {
    order_cols <- c("label")
    order_cols <- order_cols[order_cols %in% names(results)]
    remaining_cols <- names(results)[(length(order_cols)):(ncol(results))]
    remaining_cols <- remaining_cols[!remaining_cols %in%
                                       order_cols]
    order_cols <- c(match(order_cols, names(results)), match(remaining_cols,
                                                             names(results)))
    class(results) <- c("tidy_results", class(results))
    results <- results[, order_cols, drop = FALSE]
  }
  rownames(results) <- NULL
  return(results)
}

submodels <- function(x, results, cols = c("name", "matrix", "row", "col", "Estimate", "Std.Error", "lbound",
                                           "ubound", "lboundMet", "uboundMet"), ...){
  if(!has_submod(x, depth = 1)){
    submod <- names(attr(x, "submodels"))
    lapply(submod, function(i){ alg_to_res(x[[i]]) })
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
    dup_pars <- table(names(unlist(lapply(submod, function(i){ omxGetParameters(x[[i]]) }))))
    dup_pars <- names(dup_pars)[dup_pars > 1]
    for(i in submod){
      submodpars <- omxGetParameters(x[[i]])
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
    dup_pars <- table(names(unlist(lapply(submod, function(i){ omxGetParameters(x[[i]]) }))))
    dup_pars <- names(dup_pars)[dup_pars > 1]
    for(i in submod){
      submodpars <- omxGetParameters(x[[i]])
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

has_submod <- function(x, depth = 0){
  subs <- names(attr(x, "submodels"))
  if(depth == 0){
    return(length(names(attr(x, "submodels"))) > 0)
  } else {
    if(!(length(subs) > 0)) return(FALSE)
    return(any(sapply(subs, function(i){has_submod(x[[i]], depth = depth-1)})))
  }
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

#' @importFrom OpenMx mxSE omxGetParameters
get_algebras <- function(x, ...){
  cl <- match.call()
  cl[[1L]] <- str2lang("tidySEM:::.get_algebras_internal")
  algs <- eval.parent(cl)
  Estimate <- unlist_mx(algs, "result")
  if(is.null(Estimate)) return(NULL)
  out <- data.frame(name = names(Estimate),
                    matrix =  unlist_mx(algs, element = "name"),
                    row = unlist_mx(algs, element = "formula"),
                    col = ":=",
                    Estimate = Estimate, row.names = NULL)
  out$Std.Error <- unlist(lapply(out$name, function(thispar){ tryCatch(mxSE(thispar, model = x, silent = TRUE), error = function(e){ NA }) }))
  out
}

alg_to_res <- function(x, ...){
  out <- .alg_to_res_internal(x, ...)
  out <- bind_list(flat(out))
  out$Std.Error <- unlist(lapply(out$name, function(thispar){ tryCatch(mxSE(thispar, model = x, silent = TRUE), error = function(e){ NA }) }))
  out
}

flat <- function(x){
  repeat {
    if(!any(vapply(x, is.list, logical(1)) & !vapply(x, is.data.frame, logical(1)))) return(x)
    x <- Reduce(c, x)
  }
}

.alg_to_res_internal <- function(x, ...){
  algs <- names(x@algebras)
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
      colnam <- rep(dimnam[[2]], each = length(dimnam[[1]]))
      addalg <- data.frame(name = nams,
                 matrix =  x[[thisalg]]$name,
                 row = rownam,
                 col = colnam,
                 Estimate = val,
                 formula = deparse(x[[thisalg]]$formula),
                 row.names = NULL)
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
