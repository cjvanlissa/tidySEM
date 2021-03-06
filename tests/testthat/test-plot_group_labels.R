library(lavaan)
library(tidySEM)

test_that("Multigroup labels are OK", {
  HS.model <- '  visual =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '
  fit <- cfa(HS.model,
             data = HolzingerSwineford1939,
             group = "school")

  p <- prepare_graph(fit)


  expect_true(!any(grepl("Grant-White", p$edges$label)))
  expect_true(!any(grepl("TRUE", as.character(p$edges$label))))
})
