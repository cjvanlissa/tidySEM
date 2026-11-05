test_that("dispatch_method selects local methods", {

  env <- new.env(parent = asNamespace("tidySEM"))

  env$test_dispatch_mixture <- function(x, ...) "mixture"
  env$test_dispatch_MxModel <- function(x, ...) "MxModel"

  dispatch <- tidySEM:::dispatch_method
  environment(dispatch) <- env

  x <- list()

  attr(x, "tidySEM") <- "mixture"
  expect_equal(dispatch(x, "test_dispatch"), "mixture")

  attr(x, "tidySEM") <- NULL
  expect_equal(dispatch(x, "test_dispatch"), "MxModel")

  attr(x, "tidySEM") <- "other"
  expect_equal(dispatch(x, "test_dispatch"), "MxModel")
})


test_that("dispatch_method forwards arguments", {

  env <- new.env(parent = asNamespace("tidySEM"))

  env$test_dispatch_mixture <- function(x, value, ...) value

  dispatch <- tidySEM:::dispatch_method
  environment(dispatch) <- env

  x <- structure(list(), tidySEM = "mixture")

  expect_equal(
    dispatch(x, "test_dispatch", value = 42),
    42
  )
})
