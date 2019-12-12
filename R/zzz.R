.onLoad <- function(libname, pkgname){
  opt_list <- list(
    "sem_software" = "lavaan",
    "report_columns" = c("label", "est_sig", "se", "pval", "confint", "group", "level")
  )
  lapply(1:length(opt_list), function(x){
    this_opt <- getOption(names(opt_list)[x])
    if(is.null(this_opt)){
      Args <- opt_list[x]
      do.call(options, Args)
    } else {
      if(!is.character(this_opt)){
        warning("getOption('", names(opt_list)[x], "') did not return a character value.", call. = FALSE)
      }
    }
  })

}
