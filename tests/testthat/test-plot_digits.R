library(tidySEM)
library(lavaan)

fit <- sem("mpg ~ am", data = mtcars, meanstructure = TRUE)

p <- prepare_graph(fit, digits = 5)

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

#graph_sem(fit, digits = 5)
#>   from  to    label arrow curvature connect_from connect_to lhs op rhs   est   se pval        confint  est_sig est_std se_std pval_std
#> 1   am mpg  7.24***  last        NA        right       left mpg  ~  am  7.24 1.71 0.00  [3.90, 10.59]  7.24***    0.60   0.10     0.00
#> 2  mpg mpg 22.53***  both        NA        right      right mpg ~~ mpg 22.53 5.63 0.00 [11.49, 33.57] 22.53***    0.64   0.12     0.00
#> 3   am  am     0.24  both        NA         left       left  am ~~  am  0.24 0.00 <NA>   [0.24, 0.24]     0.24    1.00   0.00     <NA>
#>    confint_std est_sig_std show
#> 1 [0.40, 0.80]     0.60*** TRUE
#> 2 [0.40, 0.88]     0.64*** TRUE
#> 3 [1.00, 1.00]        1.00 TRUE
