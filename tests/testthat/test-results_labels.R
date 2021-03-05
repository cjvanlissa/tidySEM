library(tidySEM)
library(lavaan)

fit <- sem("mpg ~ a * am", data = mtcars)
tmp <- table_results(fit, columns = NULL)

test_that("table_results() returns all labels", {
  expect_true(all(c("lavaan_label", "mplus_label") %in% names(tmp)))
  expect_true(tmp$lavaan_label[1] == "a")
})



df <- HolzingerSwineford1939

names(df)[grepl("^x", names(df))] <- c("vis_1", "vis_2", "vis_3", "tex_1", "tex_2", "tex_3", "spe_1", "spe_2", "spe_3")
dict <- tidy_sem(df)

expect_true(all(dict$dictionary$scale[-c(1:6)] == rep(c("vis", "tex", "spe"), each = 3)))

measurement(dict, meanstructure = TRUE) -> model

res_lav <- sem(as_lavaan(model), data = df)

tb_lav <- table_results(res_lav, columns = NULL)
expect_true(nrow(tb_lav) == 36)

if(isTRUE(mplusAvailable() == 0)){
  test_that("table_results() returns all labels for mplus", {
    the_test <- "mplus_labels"
    old_wd <- getwd()
    test_dir <- file.path(tempdir(), the_test)
    dir.create(test_dir)
    setwd(test_dir)
    on.exit({unlink(test_dir, recursive = TRUE); setwd(old_wd)}, add = TRUE)

    res_mplus <- mplusModeler(mplusObject(MODEL = "mpg ON am (a);", OUTPUT = "standardized;", rdata = mtcars), modelout = "test.inp", run = 1L)

    tb_mplus <- table_results(res_mplus, columns = NULL)

    expect_true("mplus_label" %in% names(tmp))
  })

}
