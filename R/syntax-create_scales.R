#' @title Create scale scores from observed variables
#' @description This function calculates mean or sum scores from a
#' \code{data.frame} and a named list describing the items in each scale. It
#' returns the scores, a scale descriptive table, and a scale correlation table.
#' It relies on several functions from the
#' \code{psych} package.
#' @param x A \code{data.frame} containing all variables referenced in the
#' \code{keys.list}, or an object of class \code{tidy_sem}.
#' @param keys.list A named list, indicating which variables belong to which
#' scale.
#' @param missing Whether to use rows with partially missing values.
#' Default: TRUE.
#' @param impute Method for handling missing values, Default: 'none'. This
#' default method uses all available data to calculate scale scores, which is
#' acceptable for mean scales, but not for sum scales.
#' @param omega Which of McDonald's \code{\link[psych]{omega}} coefficients to
#' report. Default: NULL; valid options include: \code{"omega_h"},
#' \code{"omega.lim"}, \code{"alpha"}, \code{"omega.tot"}, \code{"G6"}.
#' @param digits Number of digits for rounding, Default: 2
#' @param ... Additional parameters to pass to and from functions.
#' @return List with elements: \code{$descriptives}, \code{$correlations}, and
#' \code{$scores}.
#' @examples
#' out <- create_scales(iris, keys.list = list(scalename =
#'             c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")))
#' out$descriptives
#' @details For scales with less than 3 items, Cronbach's alpha might not be
#' suitable as an estimate of reliability. For such scales, the Spearman-Brown
#' reliability coefficient for two-item scales is computed, as described in
#' Eisinga, R., Grotenhuis, M. te, & Pelzer, B. (2012). The reliability of a
#' two-item scale: Pearson, Cronbach, or Spearman-Brown? International Journal
#' of Public Health, 58(4), 637–642. \doi{10.1007/s00038-012-0416-3}.
#' These coefficients are marked with "(sb)".
#' @rdname create_scales
#' @export
#' @importFrom psych scoreItems omega fa make.keys describe
#' @importFrom stats complete.cases cor.test var
create_scales <- function(x, keys.list, missing = TRUE, impute = "none",
                          omega = NULL,
                          digits = 2, ...)
{
  UseMethod("create_scales", x)
}

#' @method create_scales tidy_sem
#' @rdname create_scales
#' @examples
#' dict <- tidy_sem(iris, split = "\\.")
#' create_scales(dict)
#' @export
create_scales.tidy_sem <- function(x, keys.list, missing = TRUE, impute = "none",
                          omega = NULL,
                          digits = 2, ...)
{
  Args <- as.list(match.call()[-1])
  Args$x <- x$data
  scale_names <- unique(c(NA, x$dictionary$scale))[-1]
  if(length(scale_names) > 0){
    sl <- lapply(scale_names, function(i){x$dictionary$name[x$dictionary$scale == i & !is.na(x$dictionary$scale)]})
    names(sl) <- scale_names
    Args$keys.list <- sl
    do.call(create_scales, Args)
  } else {
    cat("\033[0;31mX  \033[0m")
    colmsg("No scales found in dictionary.")
    return(NULL)
  }
}

#' @method create_scales data.frame
#' @export
create_scales.data.frame <- function(x, keys.list, missing = TRUE, impute = "none",
                          omega = NULL,
                          digits = 2, reverse_items = TRUE, ...)
{
  data <- x
  use_keys <- keys.list <- keys.list[!sapply(keys.list, length) < 2]

  scoredatanames <- as.vector(unlist(keys.list))
  scoredatanames <- unique(scoredatanames)
  data <- data[, names(data) %in% scoredatanames]

  fas <- matrix(nrow = length(keys.list), ncol = 2)
  for(i in seq_along(names(keys.list))){
    scale_name <- names(keys.list)[i]
    tryCatch({
      fa_loadings <- fa(data[keys.list[[scale_name]]])$loadings
      rev_items <- sign(fa_loadings) == -1
      if(reverse_items & any(rev_items)){
        message("Some factor loadings were negative for scale '", scale_name, "'. These items were automatically reversed: ", paste0(keys.list[[scale_name]][rev_items], collapse = ", "))
        use_keys[[scale_name]][rev_items] <- paste0("-", use_keys[[scale_name]][rev_items])
        fas[i, ] <- range(abs(as.numeric(fa_loadings)))
      } else {
        fas[i, ] <- range(as.numeric(fa_loadings))
      }
      },
      error = function(e){
        warning(e)
        fas[i, ] <- c(NA, NA)
        },
      warning = function(w){
        warning(paste0("When computing factor loadings for ", scale_name, gsub("^.+?(?=:)", "", w, perl = TRUE)), call. = FALSE)
        fas[i, ] <- c(NA, NA)})
  }

  keys <- psych::make.keys(length(scoredatanames), keys.list = use_keys,
                    item.labels = scoredatanames)
  scores <- psych::scoreItems(keys, data, missing = missing, impute = impute, ...)

  table_descriptives <- data.frame(Subscale = colnames(scores$scores),
                                   Items = sapply(keys.list, length))
  desc <- descriptives(scores$scores)[, c("n", "mean", "sd", "min", "max")]
  table_descriptives <- cbind(table_descriptives,
                              desc,
                              skew_kurtosis(scores$scores, verbose = FALSE, se = FALSE))

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

  colnames(fas) <- c("min_load", "max_load")
  table_descriptives <- cbind(table_descriptives, fas)
  table_descriptives[, sapply(table_descriptives, is.numeric)] <- lapply(table_descriptives[,
                                                                                            sapply(table_descriptives, is.numeric)], format_with_na, digits = digits,
                                                                         format = "f")

  if(any(two_items)){
    table_descriptives$Reliability[two_items] <- paste0(table_descriptives$Reliability[two_items],
                                                        "(sb)")
  }
  cordat <- data.frame(scores$scores)
  if (missing == FALSE) {
    cordat <- cordat[complete.cases(cordat), ]
  }
  combos <- expand.grid(names(cordat), names(cordat))
  cortab <- matrix(mapply(function(x, y) {
    tmp <- cor.test(cordat[[x]], cordat[[y]])
    paste0(format_with_na(tmp$estimate, digits = digits, format = "f"),
           ifelse(tmp$p.value < 0.05, "*", ""), ifelse(tmp$p.value <
                                                         0.01, "*", ""), ifelse(tmp$p.value < 0.001,
                                                                                "*", ""))
  }, x = combos$Var1, y = combos$Var2), nrow = ncol(cordat))
  colnames(cortab) <- rownames(cortab) <- names(cordat)
  cortab[upper.tri(cortab)] <- ""
  rownames(table_descriptives) <- NULL
  out <- list(descriptives = table_descriptives, correlations = cortab,
              scores = data.frame(scores$scores))
  class(out) <- c("tidy_scales", class(out))
  return(out)
}

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

#' @export
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

