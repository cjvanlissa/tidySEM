#' Report formatted number
#'
#' Report a number, rounded to a specific number of decimals (defaults to two),
#' using \code{\link{formatC}}. Intended for 'R Markdown' reports.
#' @param x Numeric. Value to be reported
#' @param digits Integer. Number of digits to round to.
#' @param equals Logical. Whether to report an equals (or: smaller than) sign.
#' @return An atomic character vector.
#' @author Caspar J. van Lissa
#' @keywords internal
#' @export
#' @examples
#' report(.0234)
report <- function(x, digits = 2, equals = TRUE){
  equal_sign <- "= "
  if(x%%1==0){
    outstring <- format_with_na(x, digits = 0, format = "f")
  } else {
    if(abs(x) <  10^-digits){
      equal_sign <- "< "
      outstring <- 10^-digits
    } else {
      outstring <- format_with_na(x, digits = digits, format = "f")
    }
  }
  ifelse(equals, paste0(equal_sign, outstring), outstring)
}

#' Satorra-Bentler corrected chi-square test
#'
#' Computes Satorra-Bentler corrected chi-square test.
#' @param chisq1 An object for which a method exists; e.g., a chi-square value,
#' or a \code{data.frame}.
#' @param df1 Degrees of freedom of model 1.
#' @param scf1 Scale correction factor of model 1.
#' @param chisq2 Chi square value of model 2.
#' @param df2 Degrees of freedom of model 2.
#' @param scf2 Scale correction factor of model 2.
#' @return Chi-square difference value with corresponding degrees of freedom and
#' p-value.
#' @details Reference:
#' Satorra, A., & Bentler, P. M. (2001). A scaled difference chi-square test
#' statistic for moment structure analysis. Psychometrika, 66(4), 507-514.
#' <doi:10.1007/BF02296192>
#' @author Caspar J. van Lissa
#' @export
#' @keywords internal
#' @examples
#' df <- data.frame(chi2 = c(23, 44, 65), df = c(78, 74, 70), scf = c(1.02, 1.12, 1.28))
#' chisq_sb(24, 78, 1.02, 65, 70, 1.28)
#' @importFrom stats pchisq
chisq_sb <- function(chisq1, df1, scf1 = 1, chisq2, df2, scf2 = 1) {
  UseMethod("chisq_sb", chisq1)
}

#' @export
#' @method chisq_sb default
chisq_sb.default <- function(chisq1, df1, scf1 = 1, chisq2, df2, scf2 = 1) {
  if (df1 == df2) {
    warning("Models cannot be nested, DF are equal.")
    return(c(
      chisq = NA,
      df = NA,
      p = NA))
  }
  which_full <- which.max(c(df1, df2))
  which_restricted <- which.min(c(df1, df2))
  dff <- c(df1, df2)[which_full]
  dfr <- c(df1, df2)[which_restricted]
  c2f <- c(chisq1, chisq2)[which_full]
  c2r <- c(chisq1, chisq2)[which_restricted]
  scf <- c(scf1, scf2)[which_full]
  scr <- c(scf1, scf2)[which_restricted]

  delta_df <- abs(dff-dfr)

  TRd = abs(c2f * scf - c2r * scr) /
    ((dff * scf - dfr * scr) / (dff - dfr))

  return(c(
    Dchisq = TRd,
    Dchisq_df = delta_df,
    Dchisq_p = pchisq(TRd, delta_df, lower.tail = FALSE)
    ))
}

#' @export
#' @method chisq_sb data.frame
chisq_sb.data.frame <- function(chisq1, df1, scf1 = 1, chisq2, df2, scf2 = 1) {
  df <- chisq1
  if(nrow(df) < 2) stop("Need a data.frame with at least two rows to perform chi-square difference test.")
  chi1col <- which(names(df) %in% c("ChiSqM_Value"))
  dfcol <- which(names(df) %in% c("ChiSqM_DF"))
  scfcol <- which(names(df) %in% c("ChiSqM_ScalingCorrection"))
  if(any(sapply(c(chi1col, dfcol, scfcol), length) > 1)){
    stop("Multiple columns found that could be used for a chi-square difference test.")
  }
  if(any(sapply(c(chi1col, dfcol), length) == 0)){
    stop("Could not find chi-square or df column.")
  }

  chisq1 <- df[[chi1col]][-1]
  chisq2 <- df[[chi1col]][-nrow(df)]
  df1 <- df[[dfcol]][-1]
  df2 <- df[[dfcol]][-nrow(df)]
  if(length(scfcol) > 0){
    scf1 <- df[[scfcol]][-1]
    scf2 <- df[[scfcol]][-nrow(df)]
  } else {
    scf1 <- scf2 <- rep(1, (nrow(df)-1))
  }
  out <- t(mapply(chisq_sb,
         chisq1 = chisq1,
         df1 = df1,
         scf1 = scf1,
         chisq2 = chisq2,
         df2 = df2,
         scf2 = scf2))
  out <- rbind(c(NA, NA, NA), out)
  rownames(out) <- NULL
  cbind(df, out)
}

# Satorra-Bentler corrected chi-square tests for table
#
# Computes Satorra-Bentler corrected chi-square test for a table of model chi-
# square values, degrees of freedom, and scale correction factors.
# @param tableChi_df_scf A table of model chi-square values, degrees of freedom
# , and scale correction factors.
# @return A data.frame of chi-square values, degrees of freedom, and p-values
# for chi-square difference tests.
# @author Caspar J. van Lissa
# @family Mplus functions
# @seealso \code{\link{chisq_sb}} for a single chi-square test.
# @export
# @keywords internal
# @examples
# df <- data.frame(chi2 = c(23, 44, 65), df = c(78, 74, 70), scf = c(1.02, 1.12, 1.28))
# SB_chisq_Pvalues(df)
SB_chisq_Pvalues<-function(tableChi_df_scf){
  chisquares<-sapply(1:nrow(tableChi_df_scf), function(x){
    if(x==1){return(c(Chisq=NA, df=NA, p=NA))}
    if(x>1){
      return(chisq_sb(tableChi_df_scf[x,1], tableChi_df_scf[x,2], tableChi_df_scf[x,3], tableChi_df_scf[x-1,1], tableChi_df_scf[x-1,2], tableChi_df_scf[x-1,3]))
    }
  })
  return(as.data.frame(t(chisquares)))
}



#' Apply POMS-coding to data
#'
#' Takes in a data.frame, and applies POMS (proportion of of maximum)-coding to
#' the numeric columns.
#' @param data A data.frame.
#' @return A data.frame.
#' @author Caspar J. van Lissa
#' @export
#' @keywords internal
#' @examples
#' data <- data.frame(a = c(1, 2, 2, 4, 1, 6),
#'                    b = c(6, 6, 3, 5, 3, 4),
#'                    c = c("a", "b", "b", "t", "f", "g"))
#' poms(data)
poms <- function(data){
  nums <- sapply(data, is.numeric)
  minscores <- sapply(data[, nums], min, na.rm=TRUE)
  data[, nums] <- sweep(data[, nums], 2, minscores, `-`)
  maxscores <- sapply(data[, nums], max, na.rm=TRUE)
  data[, nums] <- sweep(data[, nums], 2, maxscores, `/`)
  data
}


# Format numeric columns
#
# Formats the numeric columns of a data.frame, to round to a specific number
# of digits.
# @param x A data.frame.
# @param digits The desired number of digits.
# @author Caspar J. van Lissa
# @export
# @keywords internal
# @examples
# dat <- mtcars
# format_numeric(dat, 1)
format_numeric <- function(x, digits = 2) {
  numeric_columns <- sapply(x, class) == 'numeric'
  x[numeric_columns] <-  lapply(x[numeric_columns], format_with_na, digits, format = "f")
  x
}



#
#Get avarage factor loadings over time for a longitudinal dataset
#
# averageloadingsforparceling<-function(df, items, prefix, postfix, waves){
#   require(dplyr)
#   require(psych)
#   factorout<-NULL
#   for(i in waves){
#     tempdat<-select(df, one_of(datamultiwave(items=items, prefix=prefix, postfix=postfix, waves=i)))
#     tempdat<-sapply(tempdat, as.numeric)
#     factorout<-cbind(factorout, as.vector(fa(tempdat, use="pairwise.complete.obs")$loadings))
#   }
#   theloadings<-round(rowMeans(factorout), 2)
#   names(theloadings)<-items
#   print(sort(theloadings))
# }

as.numeric.factor <- function(x){
  as.numeric(levels(x))[x]
}

