library(lavaan)
library(MplusAutomation)

test_that("get_edges handles columns argument", {
  fit <- sem("mpg ~ cyl\nmpg ~ am", data = mtcars, meanstructure = TRUE)

  out <- get_edges(fit, label = NULL, columns = c("est", "pval"))
  out <- within(out, {label <- paste(est, pval)})


  expect_equivalent(out$label, c("-2.50 0.00", "2.57 0.04", "8.48 0.00", "3.09 NA", "-0.45 NA",
                                 "0.24 NA"))

  out <- get_nodes(fit, label = NULL, columns = c("est", "pval"))


  if(isTRUE(getOption("test_mplus"))){
    the_test <- "get_edges_columns"
    old_wd <- getwd()
    test_dir <- file.path(tempdir(), the_test)
    dir.create(test_dir)
    setwd(test_dir)
    on.exit({unlink(test_dir, recursive = TRUE); setwd(old_wd)}, add = TRUE)

    res_mplus <- mplusModeler(mplusObject(MODEL = "mpg ON cyl am;", OUTPUT = "standardized;", rdata = mtcars), modelout = "test.inp", run = 1L)

    out <- get_edges(res_mplus$results, label = NULL, columns = c("est", "pval"))
    out <- within(out, {label <- paste(est, pval)})

    expect_equivalent(out$label, c("2.57 0.04", "-2.50 0.00", "8.48 0.00"))

  }
})
