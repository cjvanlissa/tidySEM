library(lavaan)
library(MplusAutomation)

test_that("digits are respected in table_results", {

  fit <- sem("mpg ~ cyl\nmpg ~ am", data = mtcars)
  expect_error(table_results(fit), NA)

  out <- table_results(fit, columns = c("mplus_label", "est_std", "confint"), digits = 3)
  expect_equivalent(sapply(out, function(x){max(nchar(x))}), c(13, 6, 16))


  if(isTRUE(mplusAvailable() == 0)){
    the_test <- "digits"
    old_wd <- getwd()
    test_dir <- file.path(tempdir(), the_test)
    dir.create(test_dir)
    setwd(test_dir)
    on.exit({unlink(test_dir, recursive = TRUE); setwd(old_wd)}, add = TRUE)

    res_mplus <- mplusModeler(mplusObject(MODEL = "mpg ON cyl am;", OUTPUT = "standardized;", rdata = mtcars), modelout = "test.inp", run = 1L)
    expect_error(table_results(res_mplus), NA)
    out <- table_results(res_mplus, columns = c("mplus_label", "est_std", "confint"), digits = 1)
    expect_equivalent(sapply(out, function(x){max(nchar(x))}), c(22, 4, 12))

  }
})
