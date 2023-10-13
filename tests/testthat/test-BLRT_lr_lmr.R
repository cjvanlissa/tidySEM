set.seed(5)
df <- iris[c(1:5, 50:55, 100:105), 1:4]
names(df) <- letters[1:4]
res <- mx_profiles(df, 1:2)

test_that("blrt works as expected", {
  res_blrt <- BLRT(res, 10)
  expect_equivalent(res_blrt$lr, 79.6, tolerance = .02)
  expect_equivalent(res_blrt$blrt_p, 0)
})

test_that("lr_lmr works as expected", {
  res_lmr <- lr_lmr(res)
  expect_equivalent(res_lmr$lr, 4.89, tolerance = .01)
  expect_equivalent(res_lmr$p, 4.9e-7, tolerance = .001)
})
