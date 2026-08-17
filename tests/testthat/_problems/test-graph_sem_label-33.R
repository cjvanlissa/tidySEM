# Extracted from test-graph_sem_label.R:33

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "tidySEM", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
library(lavaan)
res <- sem("dist ~ speed", cars, meanstructure =T)

# test -------------------------------------------------------------------------
tmp <- graph_sem(res, label = "est_std")
