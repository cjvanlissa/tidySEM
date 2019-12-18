#' Report formatted number
#'
#' Report a number, rounded to a specific number of decimals (defaults to two),
#' using C-style formats. Intended for RMarkdown reports.
#' @param x Numeric. Value to be reported
#' @param digits Integer. Number of digits to round to.
#' @param equals Logical. Whether to report an equals (or: smaller than) sign.
#' @return An atomic character vector.
#' @author Caspar J. van Lissa
#' @export
#' @examples
#' report(.0234)
report <- function(x, digits = 2, equals = TRUE){
  equal_sign <- "= "
  if(x%%1==0){
    outstring <- formatC(x, digits = 0, format = "f")
  } else {
    if(abs(x) <  10^-digits){
      equal_sign <- "< "
      outstring <- 10^-digits
    } else {
      outstring <- formatC(x, digits = digits, format = "f")
    }
  }
  ifelse(equals, paste0(equal_sign, outstring), outstring)
}

#' Satorra-Bentler corrected chi-square test
#'
#' Computes Satorra-Bentler corrected chi-square test.
#' @param Chisq1 Chi square value of model 1.
#' @param df1 Degrees of freedom of model 1.
#' @param scf1 Scale correction factor of model 1.
#' @param Chisq2 Chi square value of model 2.
#' @param df2 Degrees of freedom of model 2.
#' @param scf2 Scale correction factor of model 2.
#' @return Named numeric vector with chi-square value, degrees of freedom, and
#' p-value.
#' @author Caspar J. van Lissa
#' @family Mplus functions
#' @seealso \code{\link{SB_chisq_Pvalues}} to apply SBChisquare to a table of
#' model chi-square values.
#' @export
#' @examples
#' df <- data.frame(chi2 = c(23, 44, 65), df = c(78, 74, 70), scf = c(1.02, 1.12, 1.28))
#' SBChisquare(24, 78, 1.02, 65, 70, 1.28)
#' @importFrom stats pchisq
SBChisquare <- function(Chisq1, df1, scf1, Chisq2, df2, scf2) {
  if (df1 == df2) {
    warning("Models cannot be nested, DF are equal")
    return(c(
      Chisq = NaN,
      df = NaN,
      p = NaN))
  }

  more_complex <- FALSE
  fit_worse <- FALSE

  if (df2 > df1){ # If DF increased, model became more complex
    more_complex <- TRUE
  }
  delta_df <- abs(df2-df1)

  if (Chisq2-Chisq1 > 0){ # Fit became worse
    fit_worse <- TRUE
  }

  TRd = abs(Chisq1 * scf1 - Chisq2 * scf2) /
    ((df1 * scf1 - df2 * scf2) / (df1 - df2))

  return(c(
    Chisq = round(ifelse((more_complex&fit_worse)|(!more_complex&!fit_worse), 1, -1)*TRd, 2),
    df = ifelse((more_complex&fit_worse)|(!more_complex&!fit_worse), 1, -1)*delta_df,
    p = ifelse((more_complex&fit_worse)|(!more_complex&!fit_worse),
               round(1-pchisq(TRd, delta_df, lower.tail = FALSE), 3),
               round(pchisq(TRd, delta_df, lower.tail = FALSE), 3))
  ))
}

#' Satorra-Bentler corrected chi-square tests for table
#'
#' Computes Satorra-Bentler corrected chi-square test for a table of model chi-
#' square values, degrees of freedom, and scale correction factors.
#' @param tableChi_df_scf A table of model chi-square values, degrees of freedom
#' , and scale correction factors.
#' @return A data.frame of chi-square values, degrees of freedom, and p-values
#' for chi-square difference tests.
#' @author Caspar J. van Lissa
#' @family Mplus functions
#' @seealso \code{\link{SBChisquare}} for a single chi-square test.
#' @export
#' @examples
#' df <- data.frame(chi2 = c(23, 44, 65), df = c(78, 74, 70), scf = c(1.02, 1.12, 1.28))
#' SB_chisq_Pvalues(df)
SB_chisq_Pvalues<-function(tableChi_df_scf){
  chisquares<-sapply(1:nrow(tableChi_df_scf), function(x){
    if(x==1){return(c(Chisq=NA, df=NA, p=NA))}
    if(x>1){
      return(SBChisquare(tableChi_df_scf[x,1], tableChi_df_scf[x,2], tableChi_df_scf[x,3], tableChi_df_scf[x-1,1], tableChi_df_scf[x-1,2], tableChi_df_scf[x-1,3]))
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


#' Format numeric columns
#'
#' Formats the numeric columns of a data.frame, to round to a specific number
#' of digits.
#' @param x A data.frame.
#' @param digits The desired number of digits.
#' @author Caspar J. van Lissa
#' @export
#' @examples
#' dat <- mtcars
#' format_numeric(dat, 1)
format_numeric <- function(x, digits) {
  numeric_columns <- sapply(x, class) == 'numeric'
  x[numeric_columns] <-  lapply(x[numeric_columns], formatC, digits, format = "f")
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

