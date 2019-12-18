#' @method print sem_syntax
#' @export
print.sem_syntax <- function(x, ...){
  cat("Syntax for ", x$sem_software, ":\n\n", sep = "")
  cat(x$syntax, sep = "\n")
}
