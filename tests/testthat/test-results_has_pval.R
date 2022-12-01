library(lavaan)
library(tidySEM)
library(MplusAutomation)

test_that("table_results has pval", {
  the_test <- "has_pval"
  old_wd <- getwd()
  test_dir <- file.path(tempdir(), the_test)
  dir.create(test_dir)
  setwd(test_dir)
  on.exit({unlink(test_dir, recursive = TRUE); setwd(old_wd)}, add = TRUE)

  fit <- sem("mpg ~ cyl\nmpg ~ am", data = mtcars)

  expect_equivalent(table_results(fit, columns = c("pval"))$pval[1:3], c("0.00", "0.04", "0.00"))

  if(getOption("test_mplus")){
    res_mplus <- mplusModeler(mplusObject(MODEL = "mpg ON cyl am;", OUTPUT = "standardized;", rdata = mtcars), modelout = "test.inp", run = 1L)

    expect_equivalent(table_results(res_mplus, columns = c("pval"))$pval[1:3], c("0.00", "0.04", "0.00"))
  }
})
