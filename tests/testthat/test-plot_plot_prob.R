test_that("plot_prob works", {
  skip_on_cran()
  df <- data_mix_ordinal
  df[1:4] <- lapply(df, ordered)
  mx_lca(data = df,
         classes = 2) -> res
  plot_prob(res)
})
