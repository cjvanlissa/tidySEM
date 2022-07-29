skip_if_not_local <- function() {

  if (Sys.info()[["user"]] %in% c("Lissa102")) {
    return(invisible(TRUE))
  }

  testthat::skip("Not run by package maintainer")
}
