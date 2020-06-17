library(lavaan)
df <- HolzingerSwineford1939

names(df)[grepl("^x", names(df))] <- c("vis_1", "vis_2", "vis_3", "tex_1", "tex_2", "tex_3", "spe_1", "spe_2", "spe_3")
dict <- tidy_sem(df)
measurement(dict, meanstructure = TRUE) -> model

res_lav <- sem(as_lavaan(model), data = df)

tb_lav <- table_results(res_lav, columns = NULL)

if(getOption("test_mplus")){
  library(MplusAutomation)
  res_mplus <- mplusModeler(mplusObject(MODEL = as_mplus(model), rdata = df), modelout = "test.inp", run = 1L)

  tb_mplus <- table_results(res_mplus, columns = NULL)

  test_that("CFA gives same results in lavaan and Mplus", {
    expect_true(all(gsub("^intercepts", "means", gsub("^residual.", "", tolower(tb_mplus$label))) %in% tolower(tb_lav$label)))
  })

}
