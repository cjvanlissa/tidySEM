#' @title Create scale scores from observed variables
#' @description This function calculates mean or sum scores from a
#' \code{data.frame} and a named list describing the items in each scale. It
#' returns the scores, a scale descriptive table, and a scale correlation table.
#' It relies on several functions from the
#' \code{psych} package; most notably \code{\link[psych]{scoreItems}},
#' \code{\link[psych]{describe}}, \code{\link[psych]{fa}}, and
#' \code{\link[psych]{omega}}.
#' @param data A \code{data.frame} containing all variables referenced in the
#' \code{keys.list}.
#' @param keys.list A named list, indicating which variables belong to which
#' scale. See \code{\link[psych]{scoreItems}} and
#' \code{\link[psych]{make.keys}} for more information.
#' @param missing Whether to use rows with partially missing values, Default: TRUE.
#' See: \code{\link[psych]{scoreItems}}.
#' @param impute Method for handling missing values, Default: 'none'. This
#' default method uses all available data to calculate scale scores, which is
#' acceptable for mean scales, but not for sum scales. See:
#' \code{\link[psych]{scoreItems}}.
#' @param omega Which of McDonald's \code{\link[psych]{omega}} coefficients to
#' report. Default: NULL; valid options include: \code{"omega_h"},
#' \code{"omega.lim"}, \code{"alpha"}, \code{"omega.tot"}, \code{"G6"}.
#' @param write_files Whether to write the scale descriptive and correlation
#' tables to .csv files, Default: FALSE
#' @param digits Number of digits for rounding, Default: 2
#' @param ... Additional parameters to pass to and from functions.
#' @return List with elements: \code{$descriptives}, \code{$correlations}, and
#' \code{$scores}.
#' @examples
#' out <- create_scales(iris, keys.list = list(scalename =
#'             c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")))
#' out$descriptives
#' @rdname create_scales
#' @export
#' @importFrom psych scoreItems omega fa make.keys describe
#' @importFrom utils write.csv
#' @importFrom stats complete.cases cor.test var
# sb: Spearman-Brown reliability coefficient for two-item scales, as described in Eisinga, R., Grotenhuis, M. te, & Pelzer, B. (2012). The reliability of a two-item scale: Pearson, Cronbach, or Spearman-Brown? International Journal of Public Health, 58(4), 637–642. doi:10.1007/s00038-012-0416-3
create_scales <- function(data, keys.list, missing = TRUE, impute = "none",
                          omega = NULL, write_files = FALSE,
                          digits = 2, ...)
{
  keys.list <- keys.list[!sapply(keys.list, length) < 2]
  scoredatanames <- as.vector(unlist(keys.list))
  scoredatanames <- unique(scoredatanames)
  data <- data[, names(data) %in% scoredatanames]
  keys <- make.keys(length(scoredatanames), keys.list = keys.list,
                    item.labels = scoredatanames)
  scores <- scoreItems(keys, data, missing = missing, impute = impute)

  table_descriptives <- data.frame(Subscale = colnames(scores$scores),
                                   Items = sapply(keys.list, length), describe(scores$scores)[, c(2, 3, 4, 8, 9)])

  table_descriptives <- cbind(table_descriptives, skew_kurtosis(scores$scores, verbose = FALSE, se = FALSE))

  table_descriptives$Reliability <- as.vector(scores$alpha)
  two_items <- table_descriptives$Items < 3
  if(any(two_items)){
    table_descriptives$Reliability[two_items] <- sapply(keys.list[which(two_items)], function(this_scale){
      spearman_brown(data[this_scale])
    })
  }
  table_descriptives$Interpret = interpret(as.vector(scores$alpha))
  if (!is.null(omega)) {
    omegas <- sapply(names(keys.list), function(scale_name){
      tryCatch(omega(data[keys.list[[scale_name]]])[[omega]],
               error = function(e){warning(e); return(NA)},
               warning = function(w){warning(paste0("When computing Omega for ", scale_name, gsub("^.+?(?=:)", "", w, perl = TRUE)), call. = FALSE); return(NA)})

    })
    table_descriptives <- data.frame(table_descriptives,
                                     Omega = omegas, Interpret.O = interpret(omegas))
  }

  fas <- t(sapply(names(keys.list), function(scale_name){
    tryCatch(range(as.numeric(fa(data[keys.list[[scale_name]]])$loadings)),
             error = function(e){warning(e); return(c(NA, NA))},
             warning = function(w){warning(paste0("When computing factor loadings for ", scale_name, gsub("^.+?(?=:)", "", w, perl = TRUE)), call. = FALSE); return(c(NA, NA))})
  }))
  colnames(fas) <- c("min_load", "max_load")
  table_descriptives <- cbind(table_descriptives, fas)
  table_descriptives[, sapply(table_descriptives, is.numeric)] <- lapply(table_descriptives[,
                                                                                            sapply(table_descriptives, is.numeric)], formatC, digits = digits,
                                                                         format = "f")

  if(any(two_items)){
    table_descriptives$Reliability[two_items] <- paste0(table_descriptives$Reliability[two_items],
                                                        "(sb)")
  }
  if (write_files)
    write.csv(table_descriptives, "scale table.csv", row.names = FALSE)
  cordat <- data.frame(scores$scores)
  if (missing == FALSE) {
    cordat <- cordat[complete.cases(cordat), ]
  }
  combos <- expand.grid(names(cordat), names(cordat))
  cortab <- matrix(mapply(function(x, y) {
    tmp <- cor.test(cordat[[x]], cordat[[y]])
    paste0(formatC(tmp$estimate, digits = digits, format = "f"),
           ifelse(tmp$p.value < 0.05, "*", ""), ifelse(tmp$p.value <
                                                         0.01, "*", ""), ifelse(tmp$p.value < 0.001,
                                                                                "*", ""))
  }, x = combos$Var1, y = combos$Var2), nrow = ncol(cordat))
  colnames(cortab) <- rownames(cortab) <- names(cordat)
  cortab[upper.tri(cortab)] <- ""
  if (write_files) write.csv(cortab, "correlation table.csv")

  return(list(descriptives = table_descriptives, correlations = cortab,
              scores = scores$scores))
}

# if(n > 5000 & verbose) message("Sample size > 5000; skew and kurtosis will likely be significant.")
# sk <- sum((x-mean(x))^3)/(n*sqrt(var(x))^3)
# se_sk <- sqrt(6*n*(n-1)/(n-2)/(n+1)/(n+3))
# sk_2se <- sk/(2*se_sk)
# ku <- sum((x-mean(x))^4)/(n*var(x)^2) - 3
# se_ku <- sqrt(24*n*((n-1)^2)/(n-3)/(n-2)/(n+3)/(n+5))
# ku_2se <- ku/(2*se_ku)
# out <- c(sk = sk, se_sk = se_sk, sk_2se = sk_2se, ku = ku, se_ku = se_ku, ku_2se = ku_2se)

interpret <- function(reliability = NULL) {
  interpretation <- rep(NA, length(reliability))
  interpretation[reliability < 0.5] <- "Unacceptable"
  interpretation[reliability >= 0.5] <- "Poor"
  interpretation[reliability >= 0.6] <- "Questionable"
  interpretation[reliability >= 0.7] <- "Acceptable"
  interpretation[reliability >= 0.8] <- "Good"
  interpretation[reliability >= 0.9] <- "Excellent"
  return(interpretation)
}

spearman_brown <- function(x, y = NULL, ...){
  UseMethod("spearman_brown", x)
}

spearman_brown.data.frame <- function(x, y = NULL, ...){
  if(ncol(x) > 2) stop("Spearman Brown is only appropriate as a reliability estimate for two-item scales.")
  cl <- as.list(match.call()[-1])
  cl$x <- x[, 1]
  cl$y <- x[, 2]
  do.call(spearman_brown, cl)
}

spearman_brown.matrix <- spearman_brown.data.frame

#' @importFrom stats cor
spearman_brown.default <- function(x, y, ...){
  r <- cor(x, y, use = "pairwise.complete.obs")
  1/(1+(1/((r/(1-r))+(r/(1-r)))))
}


#' @title Calculate skew and kurtosis
#' @description Calculate skew and kurtosis, standard errors for both, and the
#' estimates divided by two times the standard error. If this latter quantity
#' exceeds an absolute value of 1, the skew/kurtosis is significant. With very
#' large sample sizes, significant skew/kurtosis is common.
#' @param x An object for which a method exists.
#' @param verbose Whether or not to print messages to the console,
#' Default: FALSE
#' @param se Whether or not to return the standard errors, Default: FALSE
#' @param ... Additional arguments to pass to and from functions.
#' @return matrix
#' @examples
#' skew_kurtosis(datasets::anscombe)
#' @rdname skew_kurtosis
#' @export
skew_kurtosis <- function(x, verbose = FALSE, se = FALSE, ...){
  UseMethod("skew_kurtosis", x)
}

#' @method skew_kurtosis data.frame
#' @export
skew_kurtosis.data.frame <- function(x, verbose = FALSE, se = FALSE, ...){
  t(sapply(x, skew_kurtosis))
}

#' @method skew_kurtosis matrix
#' @export
skew_kurtosis.matrix <- function(x, verbose = FALSE, se = FALSE, ...){
  t(apply(x, 2, skew_kurtosis))
}

#' @method skew_kurtosis numeric
#' @export
skew_kurtosis.numeric <- function(x, verbose = FALSE, se = FALSE, ...){
  x <- x[!is.na(x)]
  n <- length(x)
  out <- rep(NA, 6)
  names(out) <- c("skew", "skew_se", "skew_2se", "kurt", "kurt_se", "kurt_2se")
  if(n > 3){
    if(n > 5000 & verbose) message("Sample size > 5000; skew and kurtosis will likely be significant.")
    skew <- sum((x-mean(x))^3)/(n*sqrt(var(x))^3)
    skew_se <- sqrt(6*n*(n-1)/(n-2)/(n+1)/(n+3))
    skew_2se <- skew/(2*skew_se)
    kurt <- sum((x-mean(x))^4)/(n*var(x)^2) - 3
    kurt_se <- sqrt(24*n*((n-1)^2)/(n-3)/(n-2)/(n+3)/(n+5))
    kurt_2se <- kurt/(2*kurt_se)
    out <- c(skew = skew, skew_se = skew_se, skew_2se = skew_2se, kurt = kurt, kurt_se = kurt_se, kurt_2se = kurt_2se)
  }
  if(se){
    return(out)
  } else {
    return(out[c(1,3,4,6)])
  }
}

#' @method skew_kurtosis default
#' @export
skew_kurtosis.default <- function(x, verbose = FALSE, se = FALSE, ...){
  out <- rep(NA, 6)
  names(out) <- c("skew", "skew_se", "skew_2se", "kurt", "kurt_se", "kurt_2se")
  if(se){
    return(out)
  } else {
    return(out[c(1,3,4,6)])
  }
}

