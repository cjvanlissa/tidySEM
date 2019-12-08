#' @importFrom utils read.table
mplus_get_data <- function(x){
  df <- read.table(x$input$data$file)
  names(df) <- strsplit(x$input$variable$names, split = "\\s+")[[1]]
  df
}

#mplus_write_data <- function(x, df){
#  write.table(df, x$input$data$file, row.names = FALSE,
#                col.names = FALSE, sep = "\t")
#}
