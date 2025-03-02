library(lavaan)
test_that("pseudo_class works", {
  testthat::skip_if_not_installed("OpenMx")
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

  overlapping_terms <- c(pct_lv$term, pct_mx$term)
  overlapping_terms <- overlapping_terms[duplicated(overlapping_terms)]

  stopifnot(length(overlapping_terms) == 2)

  lv_results <- pct_lv[pct_lv$term %in% overlapping_terms, c("estimate", "se")]
  mx_results <- pct_mx[pct_mx$term %in% overlapping_terms, c("estimate", "se")]

  expect_equivalent(lv_results$estimate, mx_results$estimate, tolerance = .01)
  expect_equivalent(lv_results$se, mx_results$se, tolerance = .01)


  lm_results <- unlist(pct_lm[pct_lm$term == "class", c("estimate", "std.error"), drop = TRUE])
  lm_specific_lv_results <- unlist(pct_lv[pct_lv$term == "SL ~ class", c("estimate", "se"), drop  = TRUE])

  expect_equivalent(lm_results, lm_specific_lv_results, tolerance = .01)
})
