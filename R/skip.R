skip_if_not_local <- function() {

  testthat::skip_on_cran()
  testthat::skip_on_ci()
  testthat::skip_on_bioc()
}
