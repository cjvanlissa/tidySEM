# @title Print results of different types of analyses
# @description Takes an object, and prints the results as an APA table.
# @param x Object to be printed
# @param columns Character vector, indicating the columns to retain.
# Default: c("label", "est_sig", "confint").
# @param digits Integet. Number of digits to round to when formatting;
# Default: 2.
# @param ... Other arguments passed to and from other functions.
# @return data.frame
# @rdname table_results
# @export
# table_results <- function(x, columns = c("label",
#                                          "est_sig", "confint"), digits = 2, ...){
#   UseMethod("table_results", x)
# }

#' @method table_results rma
#' @export
table_results.rma <-  function(x, standardized = TRUE, columns = c("label", "est_sig", "se", "pval", "ci"), digits = 2, ...){

  results <- do.call(cbind, x[c("b", "se", "zval", "pval", "ci.lb", "ci.ub")])
  results <- data.frame(label = rownames(results), results)
  names(results)[2] <- c("est")
  value_columns <- c("est", "se", "zval", "pval", "ci.lb", "ci.ub")
  value_columns <- value_columns[which(value_columns %in%
                                         colnames(results))]
  add_cis <- TRUE
  results$est_sig <- est_sig(results, digits)

  results$confint <- conf_int(results, digits)

  results[, value_columns] <- lapply(results[, value_columns],
                                     formatC, digits = digits, format = "f")
  if (!is.null(columns))
    results <- results[, columns]
  rownames(results) <- NULL
  results
}

#' Print results table formatted for publication
#'
#' Takes a model object, and formats it as a publication-ready table.
#' @param x A model object for which a method exists.
#' @param standardized Logical. Whether to return standardized parameters or
#' not. Defaults to TRUE.
#' @param columns A character vector of columns to retain from the results
#' section. Defaults to \code{c("label", "est_sig", "se", "pval", "ci")}. These
#' correspond to 1)
#' the parameter label, 2) estimate column with significance asterisks appended
#' (\* <.05, \*\* < .01, \*\*\* < .001); 3) standard error, 4) p-value, 5) a
#' formatted confidence interval.
#' @param digits Number of digits to round to when formatting numeric columns.
#' @param ... Logical expressions used to filter the rows of results returned.
#' @return A data.frame of formatted Mplus results.
#' @author Caspar J. van Lissa
#' @family Mplus functions
#' @seealso \code{\link{readModels}}.
#' @export
#' @examples
#' #Make me!
table_results <- function(x, standardized = TRUE, columns = c("label", "est_sig", "se", "pval", "ci"), digits = 2, ...){
  UseMethod("table_results")
}

table_results.mplusModel <- function(x, standardized = TRUE, columns = c("label", "est_sig", "se", "pval", "ci"), digits = 2, ...){

  args <- list(...)
  parameters <- c("unstandardized", "stdyx.standardized")[(standardized+1)]
  results <- x$parameters[[parameters]]
  value_columns <- c("est", "se", "est_se", "pval", "posterior_sd")
  value_columns <- value_columns[which(value_columns %in% names(results))]
  add_cis <- FALSE
  if(!is.null(x$parameters[[paste0("ci.", parameters)]])){
    if(dim(results)[1]==dim(x$parameters[[paste0("ci.", parameters)]])[1]){
      add_cis <- TRUE
      results <- cbind(results, x$parameters[[paste0("ci.", parameters)]][, c("low2.5", "up2.5")])
    }
  }

  if(!is.null(x$indirect[[parameters]])){
    overall <- x$indirect[[parameters]]$overall
    if(!is.null(overall)){
      paramHeader <- "Sum.of.indirect"
      param <- paste(overall$outcome, overall$pred, sep = ".")
      overall$pred <- paramHeader
      overall$outcome <- param
      names(overall)[c(1, 2)] <- c("paramHeader", "param")
      if(add_cis){
        overall <- cbind(overall, x$indirect[[paste0("ci.", parameters)]]$overall[, c("low2.5", "up2.5")])
      }
      names(overall)[match(tolower(names(results)), tolower(names(overall)))] <- names(results)[na.omit(match(tolower(names(overall)), tolower(names(results))))]
      results <- rbind(results, overall[, match(names(results), names(overall))])
    }

    specific <- x$indirect[[parameters]]$specific
    if(!is.null(specific)){
      paramHeader <- "Specific.indirect"
      param <- paste(specific$pred, specific$intervening, specific$outcome, sep = ".")
      specific$pred <- paramHeader
      specific$intervening <- param
      names(specific)[c(1, 2)] <- c("paramHeader", "param")
      if(add_cis){
        specific <- cbind(specific, x$indirect[[paste0("ci.", parameters)]]$specific[, c("low2.5", "up2.5")])
      }
      names(specific)[match(tolower(names(results)), tolower(names(specific)))] <- names(results)[na.omit(match(tolower(names(specific)), tolower(names(results))))]
      results <- rbind(results, specific[, match(names(results), names(specific))])
    }
  }

  var_classes <- sapply(results[value_columns], class)
  results[value_columns[which(var_classes == "character")]] <- lapply(results[value_columns[which(var_classes == "character")]], as.numeric)
  results[value_columns[which(var_classes == "factor")]] <- lapply(results[value_columns[which(var_classes == "factor")]], as.numeric.factor)

  constrained_rows <- results$pval == 999

  results$label <- param_label(results)
  if(all(c("est", "pval") %in% names(results))){
    results$est_sig <- est_sig(results, digits)
  }
  results$confint <- conf_int(results, digits)
  filter_columns <- names(args)[which(names(args) %in% names(results))]
  if(!is.null(filter_columns)){
    atomic_filters <- which(sapply(args[filter_columns], length) == 1)
    args[filter_columns][atomic_filters] <- toString(shQuote(args[filter_columns][atomic_filters]))
    num_cols <- which(sapply(results[filter_columns], is.numeric))
    num_filter <- NULL
    if(length(num_cols > 0)){
      num_filter <- sapply(filter_columns[num_cols], function(x){
        paste(paste0("results[['", x, "']] >= ", args[[x]][1], " & ", "results[['", x, "']] <= ", args[[x]][2]), collapse = " & ")
      })

      row_filter <- paste(c(num_filter,
                            sapply(filter_columns[-num_cols], function(x){
                              paste0("results[['", x, "']] %in% ", args[x])
                            })),
                          collapse = " & ")
    } else {
      row_filter <- paste(sapply(filter_columns, function(x){
        paste0("results[['", x, "']] %in% ", args[x])
      }), collapse = " & ")

    }
    results <- results[eval(parse(text = row_filter)), ]
  }
  constrained_rows <- results$pval == 999

  results[, value_columns] <- lapply(results[, value_columns], formatC, digits = digits, format = "f")
  results[constrained_rows, which(names(results) %in% c("se", "pval", "est_se", "confint"))] <- ""
  if(!is.null(columns)) results <- results[ , columns]
  results
}

#' Add significance asterisks to Mplus output
#'
#' Takes an mplusModel object returned by \code{readModels}, and adds
#' significance asterisks to the "est" column, based on the "pval" column.
#' @param mplusresults An mplusModel object, as returned by \code{readModels}.
#' @param digits Integer. The number of digits to round the "est" column to.
#' @return A character vector of formatted Mplus estimates.
#' @author Caspar J. van Lissa
#' @family Mplus functions
#' @seealso \code{\link{readModels}}.
#' @export
#' @examples
#' #Make me!
est_sig <- function(mplusresults, digits){
  paste0(formatC(mplusresults$est, digits = digits, format = "f"), ifelse(mplusresults$pval<.05, "*", ""), ifelse(mplusresults$pval<.01, "*", ""), ifelse(mplusresults$pval<.001, "*", ""))
}

.est_sig <- function(val, sig, digits){
  out <- formatC(val, digits = digits, format = "f")
  out[which(sig<.05)] <- paste0(out[which(sig<.05)], "*")
  out[which(sig<.01)] <- paste0(out[which(sig<.01)], "*")
  out[which(sig<.001)] <- paste0(out[which(sig<.001)], "*")
  out
}

#' Add confidence intervals to Mplus output
#'
#' Takes an mplusModel object returned by \code{readModels}, and constructs
#' nicely formatted confidence intervals. The method depends on the output;
#' if the results section already contains confidence/credible intervals, these
#' are reported. If not, the confidence interval is constructed from the "est"
#' and "se" columns.
#' @param mplusresults An mplusModel object, as returned by \code{readModels}.
#' @param digits Integer. The number of digits to round the confidence interval
#' to.
#' @return A character vector of formatted confidence intervals.
#' @author Caspar J. van Lissa
#' @family Mplus functions
#' @seealso \code{\link{readModels}}.
#' @export
#' @examples
#' data <- data.frame(est = c(1.325, 2.432), se = c(.05336, .00325))
#' conf_int(data)
conf_int <- function(mplusresults, digits = 2){
  if("low2.5" %in% names(mplusresults) | "lower_2.5ci" %in% names(mplusresults)){
    if("low2.5" %in% names(mplusresults)){
      message("Used bootstrapped confidence intervals.")
      confint <- paste0("[", formatC(mplusresults$low2.5, digits = digits, format = "f"), ", ", formatC(mplusresults$up2.5, digits = digits, format = "f"), "]")
    } else {
      confint <- paste0("[", formatC(mplusresults$lower_2.5ci, digits = digits, format = "f"), ", ", formatC(mplusresults$upper_2.5ci, digits = digits, format = "f"), "]")
    }
  } else {
    message("Calculated confidence intervals from est and se.")
    confint <- paste0("[", formatC(mplusresults$est-(1.96*mplusresults$se), digits = digits, format = "f"), ", ", formatC(mplusresults$est+(1.96*mplusresults$se), digits = digits, format = "f"), "]")
  }
  gsub("^ \\[", "\\[ ", gsub("([^-]\\d\\.\\d{2})", " \\1", confint))
}

#' Add parameter labels to Mplus output
#'
#' Sometimes a single parameter label is more convenient than the two (or more)
#' columns returned by \code{readModels}. This function constructs parameter
#' labels by concatenating the paramHeader and param columns, or other relevant
#' label columns
#' @param mplusresults An mplusModel object, as returned by \code{readModels}.
#' @return A character vector of parameter labels.
#' @author Caspar J. van Lissa
#' @family Mplus functions
#' @seealso \code{\link{readModels}}.
#' @export
#' @examples
#' data <- data.frame(paramHeader = c("F.BY", "F.BY"), param = c("A", "B"))
#' param_label(data)
param_label <- function(mplusresults){
  if(!is.null(mplusresults[["paramHeader"]])&!is.null(mplusresults[["param"]])){
    return(paste(mplusresults$paramHeader, mplusresults$param, sep = "."))
  }
  if(!is.null(mplusresults[["pred"]])&!is.null(mplusresults[["intervening"]])&!is.null(mplusresults[["outcome"]])){
    return(paste("IND", mplusresults$pred, mplusresults$intervening, mplusresults$outcome, sep = "."))
  }
  if(!is.null(mplusresults[["pred"]])&!is.null(mplusresults[["summary"]])&!is.null(mplusresults[["outcome"]])){
    return(paste(gsub("\\s", "\\.", mplusresults$summary), mplusresults$outcome, mplusresults$pred, sep = "."))
  }
}


#' Row-binds tables for publication
#'
#' Converts tables (data.frames, matrices) to character, and row-binds them,
#' inserting a label into the first column for each sub-table.
#' @param table_list A list of tables.
#' @return A table.
#' @author Caspar J. van Lissa
#' @family Mplus functions
#' @export
#' @examples
#' table_list <- list(
#'   table_f = data.frame(paramHeader = c("F.BY", "F.BY"), param = c("A", "B")),
#'   table_g = data.frame(paramHeader = c("G.BY", "G.BY"), param = c("A", "B")))
#' table_list <- list(
#'   data.frame(paramHeader = c("F.BY", "F.BY"), param = c("A", "B")),
#'   data.frame(paramHeader = c("G.BY", "G.BY"), param = c("A", "B")))
#' rbind_tables(table_list)
rbind_tables <- function(table_list){
  if(length(unique(sapply(table_list, ncol))) > 1) stop("Not all tables have the same number of columns.")
  if(is.null(names(table_list))) names(table_list) <- 1:length(table_list)
  do.call(rbind,
          lapply(names(table_list), function(x){
            rbind(
              c(x, rep("", (ncol(table_list[[x]])-1))), sapply(table_list[[x]], as.character))
          })
  )
}

#' Extract correlation tables from mplusModel
#'
#' Takes an mplusModel object returned by \code{readModels}, and extracts a
#' publication-ready correlation matrix.
#' @param mplusModel An mplusModel object, as returned by \code{readModels}.
#' @param parameters A character string corresponding to the name of an element
#' of the $parameters list in \code{mplusModel}. Usually one of
#' \code{c("unstandardized", "stdyx.standardized", "stdy.standardized")}.
#' @param valueColumn Character. Which column to use to propagate the matrix.
#' Defaults to "est_sig", the estimate with significance asterisks.
#' @param digits Number of digits to round to when formatting values.
#' @return A Matrix or a list of matrices (in case there are between/within
#' correlation matrices).
#' @author Caspar J. van Lissa
#' @family Mplus functions
#' @seealso \code{\link{readModels}}.
#' @export
#' @examples
#' #Make me!
#' @importFrom stats na.omit reshape

corTable <- function(mplusModel, parameters = "stdyx.standardized", valueColumn = "est_sig", digits = 2){
  correlations <- mplusModel$parameters[[parameters]]
  if("BetweenWithin" %in% names(correlations)){
    cornames <- unique(correlations$BetweenWithin)
    correlations <- lapply(cornames, function(x){
      correlations[correlations$BetweenWithin == x, ]
    })
    names(correlations) <- cornames
  } else {
    correlations <- list(correlations)
  }
  correlations <- lapply(correlations, function(cors){
    cors <- cors[grep("WITH$", cors$paramHeader), ]
    cors$paramHeader <- gsub("\\.WITH", "", cors$paramHeader)
    cors$paramHeader <- substr(cors$paramHeader,  1, 8)
    cors$param <- substr(cors$param,  1, 8)
    if(valueColumn == "est_sig"){
      cors$est_sig <- est_sig(cors, digits = digits)
    }
    if(valueColumn == "confint"){
      cors$confint <- conf_int(cors, digits = digits)
    }

    cors <- cors[ , c("paramHeader", "param", valueColumn)]
    cor_order <- unique(c(rbind(cors$paramHeader, cors$param)))
    names(cors)[3] <- "value"
    cors <- rbind(cors, data.frame(paramHeader = cors$param, param = cors$paramHeader, value = cors$value))
    cors <- rbind(cors, data.frame(paramHeader = unique(cors$paramHeader), param = unique(cors$paramHeader), value = rep(ifelse(valueColumn %in% c("est", "est_sig"), 1, NA), length(unique(cors$paramHeader)))))
    cors <- reshape(cors, v.names = "value", timevar = "paramHeader", idvar = "param", direction = "wide")
    names(cors) <- substr(names(cors), 7, 15)

    cors <- cors[match(cor_order, cors[[1]]), na.omit(match(cor_order, names(cors)))]
    row.names(cors) <- cor_order
    cors[is.na(cors)] <- ""
    cors
  })
  if(length(correlations) == 1){
    correlations <- correlations[[1]]
  }
  correlations
}


