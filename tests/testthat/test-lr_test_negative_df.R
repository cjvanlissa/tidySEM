df = iris[1:3]
names(df) <- letters[1:3]
res <- mx_profiles(df, 2)
df_aux <- cut(iris[[4]], 3)
df_aux <- mx_dummies(df_aux)
names(df_aux) <- letters[1:ncol(df_aux)]
res_bch <- BCH(res, paste0(names(df_aux), " | t1", collapse = "\n"), data = df_aux)
testres <- lr_test(res_bch)

test_that("lr_test does not give negative df", {
  expect_true(testres$overall$df > 0)
  expect_true(all(testres$pairwise$df > 0))
})
