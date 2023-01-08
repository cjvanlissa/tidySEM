#' @title Describe a dataset
#' @description Provide descriptive statistics for a dataset.
#' @param x An object for which a method exists.
#' @param ... Additional arguments.
#' @return A \code{data.frame} with descriptive statistics for \code{x}.
#' Its elements are:
#' \tabular{lll}{
#'   \strong{name} \tab \code{Character} \tab Variable name\cr
#'   \strong{type} \tab \code{character} \tab Data type in `R`, as obtained by `class(x)[1]`\cr
#'   \strong{n} \tab \code{Integer} \tab Number of valid observations\cr
#'   \strong{missing} \tab \code{Numeric} \tab Proportion missing\cr
#'   \strong{unique} \tab \code{Integer} \tab Number of unique values\cr
#'   \strong{mean} \tab \code{numeric} \tab Mean value of non-missing entries, only defined for variables that can be coerced to numeric\cr
#'   \strong{median} \tab \code{numeric} \tab Median value of non-missing entries, only defined for numeric variables\cr
#'   \strong{mode} \tab \code{Integer} \tab For numeric variables: The mode value. For factors: The frequency of the mode value\cr
#'   \strong{mode_value} \tab \code{Character} \tab For factors: value of the mode\cr
#'   \strong{sd} \tab \code{numeric} \tab Standard deviation of non-missing entries, only defined for variables that can be coerced to numeric\cr
#'   \strong{v} \tab \code{numeric} \tab Variability coefficient V for factor
#'   variables (Agresti, 1990). V is the probability that two independent
#'   observations fall in different categories\cr
#'   \strong{min} \tab \code{numeric} \tab Minimum value for numeric variables\cr
#'   \strong{max} \tab \code{numeric} \tab Maximum value for numeric variables\cr
#'   \strong{range} \tab \code{numeric} \tab Range (distance between min and max) for numeric variables\cr
#'   \strong{skew} \tab \code{numeric} \tab Skewness. The normalized third central moment of a numeric variable, which reflects its skewness. A symmetric distribution has a skewness of zero\cr
#'   \strong{skew_2se} \tab \code{numeric} \tab Skewness, divided by two times its standard error. Values greater than one can be considered "significant" according to a Z-test with significance level of .05\cr
#'   \strong{kurt} \tab \code{numeric} \tab Kurtosis. The normalized fourth central moment of a numeric variable, which reflects its peakedness. A heavy-tailed distribution has high kurtosis, a light-tailed distribution has low kurtosis (sometimes called platykurtic).\cr
#'   \strong{kurt_2se} \tab \code{numeric} \tab Kurtosis, divided by two times its standard error. Values greater than one can be considered "significant" according to a Z-test with significance level of .05
#' }
#' @examples
#' descriptives(iris)
#' @references Agresti, A. (2012). Categorical data analysis (Vol. 792).
#' John Wiley & Sons.
#' @rdname descriptives
#' @export
#' @importFrom stats median sd
descriptives <- function(x, ...) {
  UseMethod("descriptives", x)
}

#' @method descriptives matrix
#' @export
descriptives.matrix <- function(x, ...) {
  Args <- as.list(match.call()[-1])
  Args$x <- data.frame(x)
  do.call(descriptives, Args)
}

#' @method descriptives data.frame
#' @export
descriptives.data.frame <- function(x, ...) {
  data_types <-
    sapply(x, function(i) {
      paste0(class(i), collapse = ", ")
    })
  out <- lapply(x, descriptives)
  all_names <-
    c(
      "n",
      "missing",
      "unique",
      "mean",
      "median",
      "mode",
      "mode_value",
      "sd",
      "v",
      "min",
      "max",
      "range",
      "skew",
      "skew_2se",
      "kurt",
      "kurt_2se"
    )
  out <-
    do.call(rbind, c(lapply(out, function(x)
      data.frame(c(
        x, sapply(setdiff(all_names, names(x)),
                  function(y)
                    NA)
      ))),
      make.row.names = FALSE))
  out <- out[, all_names]

  out <- cbind(name = names(x),
               type = data_types,
               out)
  rownames(out) <- NULL
  out
}

#' @method descriptives numeric
#' @export
descriptives.numeric <- function(x, ...) {
  missingx <- is.na(x)
  completex <- x[!missingx]
  rng <- range(completex)
  sk <- skew_kurtosis(completex)
  cbind(
    data.frame(
      n = sum(!missingx),
      missing = sum(missingx)/length(x),
      unique = length(unique(completex)),
      mean = mean(completex),
      median = median(completex),
      mode = median(completex),
      sd = sd(completex),
      min = rng[1],
      max = rng[2],
      range = diff(rng)
    ),
    t(sk)
  )
}

#' @method descriptives integer
#' @export
descriptives.integer <- descriptives.numeric

#' @method descriptives default
#' @export
descriptives.default <- function(x, ...) {
  if(is.factor(x)) x <- droplevels(x)
  if(!is.vector(x)) x <- tryCatch(as.vector(x), error = function(e){NA})
  tb <- tryCatch(table(x, useNA = "always"), error = function(e){NA})
  data.frame(
    n = tryCatch({sum(!is.na(x))}, error = function(e){NA}),
    missing = sum(is.na(x))/length(x),
    unique = tryCatch(length(tb), error = function(e){NA}),
    mode = tryCatch({
      unname(tb[which.max(tb)])
    }, error = function(e){NA}),
    mode_value = tryCatch(names(tb)[which.max(tb)], error = function(e){NA}),
    v = tryCatch(var_cat(x), error = function(e){NA})
  )
}

#' @method descriptives factor
#' @export
descriptives.factor <- descriptives.default

# Agresti's V for categorical data variability
# Agresti, Alan (1990). Categorical Data Analysis. John Wiley and Sons, Inc. 24-25
var_cat <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x))
    return(NA)
  p <- prop.table(table(x))
  #-1 * sum(p*log(p)) Shannon entropy
  1 - sum(p ^ 2)
}

#' @title Calculate skew and kurtosis
#' @description Calculate skew and kurtosis, standard errors for both, and the
#' estimates divided by two times the standard error. If this latter quantity
#' exceeds an absolute value of 1, the skew/kurtosis is significant. With very
#' large sample sizes, significant skew/kurtosis is common.
#' @param x An object for which a method exists.
#' @param verbose Logical. Whether or not to print messages to the console,
#' Default: FALSE
#' @param se Whether or not to return the standard errors, Default: FALSE
#' @param ... Additional arguments to pass to and from functions.
#' @return A \code{matrix} of skew and kurtosis statistics for \code{x}.
#' @examples
#' skew_kurtosis(datasets::anscombe)
#' @rdname skew_kurtosis
#' @export
skew_kurtosis <- function(x, verbose = FALSE, se = FALSE, ...) {
  UseMethod("skew_kurtosis", x)
}

#' @method skew_kurtosis matrix
#' @export
skew_kurtosis.matrix <-
  function(x, verbose = FALSE, se = FALSE, ...) {
    Args <- as.list(match.call()[-1])
    Args$x <- data.frame(x)
    do.call(skew_kurtosis, Args)
}

#' @method skew_kurtosis data.frame
#' @export
skew_kurtosis.data.frame <-
  function(x, verbose = FALSE, se = FALSE, ...) {
    t(sapply(x, skew_kurtosis))
  }

#' @method skew_kurtosis matrix
#' @export
skew_kurtosis.matrix <-
  function(x, verbose = FALSE, se = FALSE, ...) {
    t(apply(x, 2, skew_kurtosis))
  }

#' @method skew_kurtosis numeric
#' @export
skew_kurtosis.numeric <-
  function(x, verbose = FALSE, se = FALSE, ...) {
    x <- x[!is.na(x)]
    n <- length(x)
    out <- tryCatch({
      if (n > 3) {
        if (n > 5000 &
            verbose)
          message("Sample size > 5000; skew and kurtosis will likely be significant.")
        skew <- (sum((x - mean(x))^3)/n)/(sum((x - mean(x))^2)/n)^(3/2)
        skew_se <- sqrt((6 * n * (n - 1)) / ((n - 2) * (n + 1) * (n + 3)))
        skew_2se <- skew / (2 * skew_se)
        kurt <- n * (sum((x - mean(x)) ^ 4) / (sum((x - mean(x))^2)^2))
        kurt_se <- sqrt(24 * n * ((n - 1) ^ 2) / (n - 3) / (n - 2) / (n + 3) /
                          (n + 5))

        kurt_2se <- kurt / (2 * kurt_se)
        c(skew,
          skew_se,
          skew_2se,
          kurt,
          kurt_se,
          kurt_2se
        )
      } else {
        stop()
      }
    }, error = function(e){ rep(NA, 6) })

    names(out) <-
      c("skew", "skew_se", "skew_2se", "kurt", "kurt_se", "kurt_2se")
    if (se) {
      return(out)
    } else {
      return(out[c(1, 3, 4, 6)])
    }
  }

#' @method skew_kurtosis default
#' @export
skew_kurtosis.default <-
  function(x, verbose = FALSE, se = FALSE, ...) {
    out <- rep(NA, 6)
    names(out) <-
      c("skew", "skew_se", "skew_2se", "kurt", "kurt_se", "kurt_2se")
    if (se) {
      return(out)
    } else {
      return(out[c(1, 3, 4, 6)])
    }
  }


col_message <- function(..., col = 30, success = TRUE) {
  #94
  #cat(paste0("\033[0;", col, "m",txt,"\033[0m","\n"))
  txt <- do.call(paste0, list(...))
  cat(paste0(
    ifelse(success,
           "\033[0;32mv  \033[0m",
           "\033[0;31mX  \033[0m"),
    "\033[0;",
    col,
    "m",
    txt,
    "\033[0m",
    "\n"
  ))
}
