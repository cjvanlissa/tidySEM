if(requireNamespace("OpenMx", quietly = TRUE)){

  df <- iris[c(1:10, 140:150), c(1, 5)]
  names(df) <- c("x", "group")
  mod <- as_ram("x~1", data = df, group = "group")
  mod <- run_mx(mod)

  test_that("lrt works with multigroup", {
    tst <- lr_test(mod)
    expect_equal(tst$overall$LL_dif, 42.12566, tolerance = .1)
    expect_equal(tst$pairwise$LL_dif, 42.1256631767803, tolerance = .1)
  })
}
