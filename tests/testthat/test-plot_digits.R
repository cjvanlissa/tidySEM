library(tidySEM)
library(lavaan)

fit <- sem("mpg ~ am", data = mtcars, meanstructure = TRUE)

p <- prepare_graph(fit, digits = 5)

fit <- sem("mpg ~ am", data = mtcars, meanstructure = TRUE)

prepare_graph(fit, digits = 5) |>
  color_sig("green") |>
  label_color_sig("green") |>
  plot()


test_that("prepare_graph passed on digits", {
  expect_true(all(grepl("\\.\\d{5}", p$edges$est)))
  expect_true(all(grepl("\\.\\d{5}", p$nodes$est)))
})

p <- prepare_graph(fit, label = "confint")

test_that("prepare_graph passed on label", {
  expect_true(all(grepl("^\\[.*\\]$", p$edges$label)))
  expect_true(all(grepl("^\\[.*\\]$", p$nodes$label)))
})

p <- prepare_graph(fit, columns = c("lhs", "op", "pval"))

test_that("prepare_graph passed on columns", {
  expect_true(all(c("lhs", "op", "pval") %in% names(p$edges)))
  expect_true(all(c("lhs", "op", "pval") %in% names(p$nodes)))
  expect_true(!"est" %in% names(p$edges))
  expect_true(!"est" %in% names(p$nodes))
})

