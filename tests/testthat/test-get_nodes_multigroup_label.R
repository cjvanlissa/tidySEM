library(lavaan)
library(tidySEM)

HS.model <- '  F =~ x1 + x2 + x3'
fit <- cfa(HS.model,
           data = HolzingerSwineford1939,
           group = "school")

test_that("get_nodes correctly labels multigroup models", {
  expect_true(get_nodes(fit, label = "name")$label[1] == "F")
})

