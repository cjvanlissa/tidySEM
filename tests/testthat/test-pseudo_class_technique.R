library(lavaan)
test_that("pseudo_class works", {
  testthat::skip_if_not_installed("OpenMx")
  testthat::skip_if_not_installed("mice")
  dat <- iris[,c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")]
  colnames(dat) <- c("SL", "SW", "PL", "PW")
  fit <- mx_profiles(data = dat, classes = 3)

  pct_lm <- pseudo_class(x = fit,
                         model = lm( SL ~ class, data = data ),
                         data = dat)

  pct_lv <- pseudo_class(x = fit,
                         model = sem( "SL ~ class", data = data),
                         df_complete = nrow(dat) - 1,
                         data = dat)

  pct_mx <- pseudo_class(x = fit,
                         model = "SL ~ class",
                         df_complete = nrow(x) - 1,
                         data = dat)


  expect_equivalent(pct_lv$estimate[pct_lv$term == "SL ~ class"], pct_mx$estimate[pct_mx$term == "Regressions.SL.ON.class"], tolerance = .01)
  expect_equivalent(pct_lv$estimate[pct_lv$term == "SL ~~ SL"], pct_mx$estimate[pct_mx$term == "Variances.SL"], tolerance = .01)
  expect_equivalent(pct_lv$se[pct_lv$term == "SL ~ class"], pct_mx$se[pct_mx$term == "Regressions.SL.ON.class"], tolerance = .01)
  expect_equivalent(pct_lv$se[pct_lv$term == "SL ~~ SL"], pct_mx$se[pct_mx$term == "Variances.SL"], tolerance = .01)


  expect_equivalent(pct_lv$estimate[pct_lv$term == "SL ~ class"], pct_lm$estimate[pct_lm$term == "class"], tolerance = .01)
  expect_equivalent(pct_lv$se[pct_lv$term == "SL ~ class"], pct_lm$std.error[pct_lm$term == "class"], tolerance = .01)

})
