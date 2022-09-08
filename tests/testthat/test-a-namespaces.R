df <- iris[1:4]
names(df) <- paste0("x", 1:4)
tmp <- tidy_sem(df)
tmp <- add_paths(tmp, "x1 ~ x2")
if(MplusAutomation::mplusAvailable() == 0){
  res <- suppressWarnings(estimate_mplus(tmp))

  test_that("table_fit does not give namespace error", {
    expect_error({table_fit(res)}, NA)
  })
}


test_that("mx profiles does not give namespace error", {
  expect_error({mx_profiles(data = df, classes = 2)}, NA)
})
