test_that("plot_prob works", {
  testthat::skip_if_not_installed("OpenMx")
  tidySEM:::skip_if_not_local()
  df <- data_mix_ordinal
  df[1:4] <- lapply(df, ordered)
  mx_lca(data = df,
         classes = 2) -> res
  plot_prob(res)
  expect_equal(1, 1)
})
