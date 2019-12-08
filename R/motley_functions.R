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


#' Make a keys list
#'
#' Attempts to generate a keys.list, as used by functions from the \code{psych}
#' package, and several functions in this \code{motley} package.
#'
#' @param var_names Character vector of variable names to convert to a KeysList.
#' @param questionnaire_filter Character. A regex filter that identifies
#' variable names that are part of a questionnaire.
#' @param item_filter Character. A regex filter that identifies the incremental
#' part of multiple items belonging to the same questionnaire (e.g., if items
#' are labelled c("a1", "a2", "a3"), then the item_filter would be \code{"\\d$"}
#' ).
#' @param skip_questionnaires Character vector. Names of the questionnaires to
#' skip.
#' @param questionnaire_length Integer. Minimum number of items required to be
#' included in the list of questionnaires.
#' @return A named list with elements corresponding to the questionnaires
#' identified. Each element is a character vector with the variable names
#' belonging to that questionnaire.
#' @author Caspar J. van Lissa
#' @export
#' @examples
#' makeKeysList(var_names = c("bfi_1", "bfi_2", "bfi_3", "bfi_4", "bfi_5",
#' "mac_q_j_1", "mac_q_j_2", "mac_q_j_3", "mac_q_j_4", "mac_q_j_5", "mac_q_j_6",
#' "mac_q_j_7", "mac_q_j_8", "mac_q_j_9", "mac_q_j_10", "mac_q_j_11",
#' "mac_q_j_12", "mac_q_j_13", "mac_q_j_14", "mac_q_j_15", "mac_q_j_16",
#' "mac_q_j_17", "mac_q_j_18", "mac_q_j_19", "mac_q_j_20", "mac_q_j_21",
#' "mac_q_r_1", "mac_q_r_2", "mac_q_r_3", "mac_q_r_4", "mac_q_r_5", "mac_q_r_6",
#' "mac_q_r_7", "mac_q_r_8", "mac_q_r_9", "mac_q_r_10", "mac_q_r_11",
#' "mac_q_r_12", "mac_q_r_13", "mac_q_r_14", "mac_q_r_15", "mac_q_r_16",
#' "mac_q_r_17", "mac_q_r_18", "mac_q_r_19", "mac_q_r_20", "mac_q_r_21"),
#' questionnaire_filter = "^[a-zA-Z_]+")
makeKeysList <- function(var_names,
                         questionnaire_filter = "^\\w+\\d+$",
                         item_filter = "\\d+$",
                         skip_questionnaires = NULL,
                         questionnaire_length = 3L){
  items <- grep(questionnaire_filter, var_names, value = TRUE)
  questionnaires <- factor(gsub(item_filter, "", items))
  num_items <- table(questionnaires)
  retain_q <- names(num_items)[which(num_items >= questionnaire_length)]
  retain_q <- retain_q[!retain_q %in% skip_questionnaires]
  outlist <- lapply(retain_q, function(x){items[questionnaires == x]})
  names(outlist) <- retain_q
  outlist
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

