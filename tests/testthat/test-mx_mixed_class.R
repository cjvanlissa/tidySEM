if(requireNamespace("OpenMx", quietly = TRUE)){
library(OpenMx)
set.seed(1)
df <- data.frame(rbind(
  matrix(rnorm(300, mean = 2), ncol = 3),
  matrix(rnorm(600), ncol = 3)
))
df$X3 <- ordered(cut(df$X3, 3, labels = FALSE))


test_that("mx_mixed_lca passes arguments", {
  res <- mx_mixed_lca(df, classes = 1:3, covariances = c("zero", "varying"), run = FALSE, expand_grid = TRUE)
  expect_false(res[[2]]@.wasRun)
  expect_true(all(table(sapply(res, function(i){length(i@submodels)})) == rep(2, 3)))

  parnms <- do.call(c, lapply(res, function(i)names(omxGetParameters(i))))
  expect_true(sum(grepl("c\\d{2}", parnms)) == 6) # Half the models have free covariances
  expect_true(sum(grepl("v\\d", parnms)) == 12) # All equal variance
})


}
