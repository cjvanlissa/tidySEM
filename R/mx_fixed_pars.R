.mxfixedpars <- function(x){
  submods <- names(x@submodels)
  out <- NULL
  if(!is.null(submods)){
    out <- lapply(submods, function(themod){
      tmp <- .mxfixedpars(x[[themod]])
      if(is.null(tmp)) return(NULL)
      tmp$matrix <- paste0(themod, ".", tmp$matrix)
      tmp
    })
  }
  mats <- x@matrices
  if(length(mats) > 0){
  out <- c(out,
           lapply(names(mats)[!names(mats) %in% c("F", "mat_dev", "mat_ones", "Indicators")], function(thism){
             tmp <- as.data.frame.table(x[[thism]][["values"]])
             if(is.null(rownames(x[[thism]][["values"]]))){
               levels(tmp$Var1) <- 1:length(levels(tmp$Var1))
             }
             names(tmp)[3] <- "Estimate"
             tmp$name <- paste0(x$name, ".", thism, "[", as.integer(tmp$Var1), ",", as.integer(tmp$Var2), "]")
             tmp$matrix <- thism
             tmp$row <- as.character(tmp$Var1)
             tmp$col <- as.character(tmp$Var2)
             isfree <- as.vector(x[[thism]][["free"]])
             tmp <- tmp[!isfree & !tmp$Estimate == 0, c("name", "matrix", "row", "col", "Estimate"), drop = FALSE]
             tmp[!rowSums(is.na(tmp)) == ncol(tmp), , drop = FALSE]
           }))
  }
  out <- bind_list(out)
  if(is.null(out)) return(NULL)
  if(nrow(out) == 0) return(NULL)
  return(out)
}
