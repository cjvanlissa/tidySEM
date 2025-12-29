# Extracted from test-pseudo_class_technique.R:7

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "tidySEM", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
library(lavaan)

# test -------------------------------------------------------------------------
testthat::skip_if_not_installed("OpenMx")
testthat::skip_if_not_installed("mice")
dat <- iris[,c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")]
colnames(dat) <- c("SL", "SW", "PL", "PW")
fit <- mx_profiles(data = dat, classes = 3)
