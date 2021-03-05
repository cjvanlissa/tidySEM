library(lavaan)
library(MplusAutomation)

test_that("CFA gives same results in lavaan and Mplus", {
  the_test <- "compare_mplus_lavaan"
  old_wd <- getwd()
  test_dir <- file.path(tempdir(), the_test)
  dir.create(test_dir)
  setwd(test_dir)
  on.exit({unlink(test_dir, recursive = TRUE); setwd(old_wd)}, add = TRUE)

  df <- HolzingerSwineford1939

  names(df)[grepl("^x", names(df))] <- c("vis_1", "vis_2", "vis_3", "tex_1", "tex_2", "tex_3", "spe_1", "spe_2", "spe_3")
  dict <- tidy_sem(df)

  expect_true(all(dict$dictionary$scale[-c(1:6)] == rep(c("vis", "tex", "spe"), each = 3)))

  measurement(dict, meanstructure = TRUE) -> model

  res_lav <- sem(as_lavaan(model), data = df)

  tb_lav <- table_results(res_lav, columns = NULL)
  expect_true(nrow(tb_lav) == 36)

  if(isTRUE(mplusAvailable() == 0)){
    res_mplus <- mplusModeler(mplusObject(MODEL = as_mplus(model), OUTPUT = "standardized;", rdata = df), modelout = "test.inp", run = 1L)

    tb_mplus <- table_results(res_mplus, columns = NULL)


    expect_true(all(gsub("^intercepts", "means", gsub("^residual.", "", tolower(tb_mplus$mplus_label))) %in% tolower(tb_lav$mplus_label)))


  }
})
