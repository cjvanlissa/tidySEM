if(requireNamespace("OpenMx", quietly = TRUE)){
library(OpenMx)
set.seed(1)
df <- data.frame(rbind(
  matrix(rnorm(300, mean = 2), ncol = 3),
  matrix(rnorm(600), ncol = 3)
))
df$X1 <- ordered(cut(df$X1, 2, labels = FALSE))
df$X2 <- ordered(cut(df$X2, 2, labels = FALSE))
df2 <- df
df$X3 <- ordered(cut(df$X3, 3, labels = FALSE))


test_that("lavaan syntax handles thresholds", {
  mod <- as_ram("
X1 | t1
X2 | t1
X3 | t1
X3 | t2", data = df)
  res <- run_mx(mod)

  tab_res <- table_results(res, columns = NULL, format_numeric = FALSE)

  expect_equal(sum(grepl("Thresholds", tab_res$label, fixed = TRUE)), 4L)
  # Values correct
  expect_equivalent(tab_res$est[grepl("Thresholds", tab_res$label, fixed = TRUE)], c(0.0585213786057356, 0.108835037959035, -0.524400512831565,
                                                                                     1.00825234765667), tolerance = .005)

  # Check that both threshold methods are the same
  mod2 <- as_ram("
X1 | t1
X2 | t1
X3 | t1
X3 | t2", data = df, threshold_method = "mx_deviances")
  # Has deviances
  expect_true(isFALSE(is.null(mod2[["mat_dev"]])))

  res2 <- run_mx(mod2)
  tab_res2 <- table_results(res2, columns = NULL, format_numeric = FALSE)
  # Results same
  tab_res$label <- gsub("model.", "", tab_res$label, fixed = TRUE)
  expect_equivalent(tab_res$est, tab_res2$est[match(tab_res$label, tab_res2$label)], tolerance = 1e-3)
})


test_that("lavaan threshold start values respected", {
  mod <- as_ram("
X1 | start(1)*t1
X2 | start(2)*t1
X3 | start(3)*t1
X3 | start(4)*t2", data = df, threshold_method = "mx_deviances")
  expect_equal((mod$mat_ones$values %*% mod$mat_dev$values)[c(1,3, 5, 6)], 1L:4L)
})

test_that("lavaan incomplete start values gives message", {
  expect_message(as_ram("
X1 | start(1)*t1
X2 | start(2)*t1
X3 | t1
X3 | start(4)*t2", data = df))
})

test_that("mc_lca works", {
  res <- mx_lca(df, classes = 2)
  expect_equivalent(sort(res$weights$values / sum(res$weights$values)), c(.33, .66), tolerance = .03)
  tab_res <- table_results(res, columns = NULL)
  expect_equal(sum(grepl("Thresholds", tab_res$label, fixed = TRUE)), 8L)
  # Strictly increasing
  expect_equal(sum(sign(apply(res$class1$Thresholds$result, 2, diff))), 3)
  expect_equal(sum(sign(apply(res$class2$Thresholds$result, 2, diff))), 3)
})

test_that("mx_mixed_lca() works", {
  res <- mx_mixed_lca(df2, classes = 2)
  expect_equivalent(sort(res$weights$values / sum(res$weights$values)), c(.33, .66), tolerance = .03)
  tab_res <- table_results(res, columns = NULL)
  expect_equal(sum(grepl("Thresholds", tab_res$label, fixed = TRUE)), 4L)
  # Both thresholds of same sign (should be the case as all indicators are in the same direction)
  expect_equal(abs(sum(sign(res$class1$Thresholds$result))), 2)
  expect_equal(abs(sum(sign(res$class2$Thresholds$result))), 2)
  # Check direction of means
  expect_true(res$class1$M$values[1] < res$class2$M$values[1])
})



}
