library(lavaan)
library(MplusAutomation)

res_lav <- sem("mpg ~ cyl\nmpg ~ am", data = mtcars, meanstructure = TRUE)

test_that("get_nodes.lavaan handles columns argument", {
  out <- get_nodes(res_lav, label = NULL, columns = c("est", "pval"))
  expect_true(all(c("est", "pval") %in% names(out)))
})

test_that("get_nodes.lavaan handles character label", {
  out <- get_nodes(res_lav, label = "pval", columns = c("est", "pval"))
  expect_equivalent(out$label, out$pval)
})

test_that("get_nodes.lavaan handles custom label", {
  out <- get_nodes(res_lav, label = paste0(name, " (", confint, ")"))
  expect_true(all(grepl("^[a-z]+\\s\\(\\[\\d*\\.\\d{2}, \\d*\\.\\d{2}\\]\\)$", out$label)))
})


if(isTRUE(unname(Sys.info()["user"]) == "Lissa102")){
  the_test <- "get_nodes_label"
  old_wd <- getwd()
  test_dir <- file.path(tempdir(), the_test)
  dir.create(test_dir)
  setwd(test_dir)
  on.exit({unlink(test_dir, recursive = TRUE); setwd(old_wd)}, add = TRUE)

  res_mplus <- mplusModeler(mplusObject(MODEL = "mpg ON cyl am;", OUTPUT = "standardized;", rdata = mtcars), modelout = "test.inp", run = 1L)

  test_that("get_nodes.mplus handles columns argument", {
    out <- get_nodes(res_mplus, label = NULL, columns = c("est", "pval"))
    expect_true(all(c("est", "pval") %in% names(out)))
  })

  test_that("get_nodes.mplus handles character label", {
    out <- get_nodes(res_mplus, label = "pval", columns = c("est", "pval"))
    expect_equivalent(out$label, out$pval)
  })
  test_that("get_nodes.mplus handles custom label", {
    out <- get_nodes(res_mplus, label = paste0(name, " (", confint, ")"))
    expect_true(all(grepl("^[A-Z]+\\s\\((\\[[0-9\\.,\\s -]+\\]|NA)\\)$", out$label)))
  })

}
