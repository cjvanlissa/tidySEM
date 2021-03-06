library(lavaan)
library(tidySEM)

HS.model <- '  visual =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '
fit <- cfa(HS.model,
           data = HolzingerSwineford1939,
           group = "school")


graph_sem(fit, nodes = get_nodes(fit, label = "name"))

test_that("get_nodes correctly labels multigroup models", {
  expect_true(get_nodes(fit, label = "name")$label[1] == "speed")
})

