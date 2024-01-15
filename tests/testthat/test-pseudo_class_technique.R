test_that("pseudo_class works", {

  x <- iris[,c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")]
  colnames(x) <- c("SL", "SW", "PL", "PW")
  mx_profiles(data = x, classes = 3) -> fit


  library(lavaan)

  pct_lm <- pseudo_class( fit = fit,
                                  analysis = lm( SL ~ class ),
                                  pool_results = TRUE  )

  pct_lv <- pseudo_class( fit = fit,
                                 analysis = sem( "SL ~ class", data = data),
                                 pool_results = TRUE,
                                 df_complete = nrow(x) - 1)

  pct_mx <- pseudo_class( fit = fit,
                                    analysis = "SL ~ class",
                                    pool_results = TRUE,
                                    df_complete = nrow(x) - 1  )

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
