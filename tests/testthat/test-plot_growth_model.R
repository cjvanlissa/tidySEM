library(lavaan)
library(tidySEM)

model.syntax <- '
  # intercept and slope with fixed coefficients
    i =~ 1*t1 + 1*t2 + 1*t3 + 1*t4
    s =~ 0*t1 + 1*t2 + 2*t3 + 3*t4

  # regressions
    i ~ x1 + x2
    s ~ x1 + x2
'

fit <- growth(model.syntax, data = Demo.growth)

lay <- get_layout(
  "t1", "t2", "t3", "t4",
  NA, "i", "s", NA,
  rows = 2
)
tmp <- fit

# Check whether there are any errors related to the environment:
p=prepare_graph(fit, nodes = get_nodes(tmp))
# Then carry on with the regular tests
p=prepare_graph(fit, layout = lay)

test_that("node labels correct", {
  expect_true(all(grepl("\\n", p$nodes$label)))
})

model.syntax <- '
  # intercept and slope with fixed coefficients
    i =~ 1*t1 + 1*t2 + 1*t3 + 1*t4
    s =~ 0*t1 + 1*t2 + 2*t3 + 3*t4

    # time-varying covariates
    t1 ~ c1
    t2 ~ c2
    t3 ~ c3
    t4 ~ c4

  # regressions
    i ~ x1 + x2
    s ~ x1 + x2
'

fit <- growth(model.syntax, data = Demo.growth)

lay <- get_layout(
  "c1", "c2", "c3", "c4",
  "t1", "t2", "t3", "t4",
  NA, "i", "s", NA,
  "x1", NA, NA, "x2",
  rows = 4
)

p <- prepare_graph(fit, layout = lay) # TOO MANY ARROWS!

test_that("node labels correct", {
  expect_true(sum(p$edges$op == "=~") == 8)
})
