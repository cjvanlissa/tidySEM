# Extracted from test-a-namespaces.R:15

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "tidySEM", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
df <- iris[1:4]
names(df) <- paste0("x", 1:4)
tmp <- tidy_sem(df)
tmp <- add_paths(tmp, "x1 ~ x2")
if(isTRUE(getOption("test_mplus"))){
  res <- suppressWarnings(estimate_mplus(tmp))

  test_that("table_fit does not give namespace error", {
    expect_error({table_fit(res)}, NA)
  })
}

# test -------------------------------------------------------------------------
expect_error({mx_profiles(data = df, classes = 2)}, NA)
