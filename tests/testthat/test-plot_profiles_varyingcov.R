test_that("plot_profiles works with varying covariances", {
  skip_on_cran()
  testthat::skip_if_not_installed("OpenMx")
  df <- iris[1:4]
  names(df) <- paste0("x", 1:4 )
  res <- mx_profiles(df, 2, covariances = "varying")
  expect_error(tidySEM::plot_profiles(res), NA)
})

