# Extracted from test-plot_if_edit.R:6

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "tidySEM", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
library(lavaan)

# test -------------------------------------------------------------------------
fit <- sem("mpg ~ cyl\nmpg ~ am", data = mtcars, meanstructure = TRUE)
p <- prepare_graph(model = fit)
