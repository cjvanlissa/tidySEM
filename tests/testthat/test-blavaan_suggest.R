fit <- readRDS("fit.RData")
test_that("table_results() works for blavaan", {
  table_results(fit)
})
