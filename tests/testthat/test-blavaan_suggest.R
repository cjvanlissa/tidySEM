test_that("table_results() works for blavaan", {
  expect_error({
    if(requireNamespace("blavaan", quietly = TRUE)){
      fit <- readRDS("fit.testdata")
      table_results(fit)
    } else {
      tidySEM:::table_results.blavaan("test")
    }
  }, NA)

})


