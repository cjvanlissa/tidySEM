skip_if_not_local <- function() {
  testthat::skip_if_not(isTRUE(as.logical(toupper(Sys.getenv("TIDYSEM_TEST_LOCAL", "false")))))
  testthat::skip_on_cran()
  testthat::skip_on_ci()
  testthat::skip_on_bioc()
}
