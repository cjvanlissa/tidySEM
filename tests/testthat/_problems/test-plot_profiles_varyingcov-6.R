# Extracted from test-plot_profiles_varyingcov.R:6

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "tidySEM", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
skip_on_cran()
testthat::skip_if_not_installed("OpenMx")
df <- iris[1:4]
names(df) <- paste0("x", 1:4 )
res <- mx_profiles(df, 2, covariances = "varying")
