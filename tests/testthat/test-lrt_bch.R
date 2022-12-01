df <- iris[, 1:2]
names(df) <- c("x", "y")
set.seed(1)
mix <- mx_profiles(df[, "x", drop = FALSE], classes = 3)
bch1 <- BCH(x = mix, data = df$y)
test_that("bch works with continuous data", {
  tab <- table_results(bch1, columns = NULL)
  expect_equal(tab$est[tab$matrix == "M"], c("2.90", "3.16", "3.19"))
})

test_that("bch lrt works with continuous data", {
  tst <- lr_test(bch1)
  expect_equal(tst$overall$LL_dif, 44, tolerance = .1)
  expect_equal(tst$pairwise$LL_dif, c(11.9532756799639, 42.7749648547106, 1.72975571779375)
, tolerance = .1)
})

df$y <- factor(cut(df$y, 3))
set.seed(1)
bch1 <- suppressWarnings(BCH(x = mix, data = df$y))

test_that("bch works with ordinal data", {
  tab <- table_results(bch1, columns = NULL)
  expect_equal(as.numeric(tab$est[tab$matrix == "Thresholds"]), as.numeric(c("-0.25", "8.20", "-0.76", "0.86", "-0.70", "0.89")), tolerance = .1)
})

test_that("bch lrt works with ordinal data", {
  tst <- suppressWarnings(lr_test(bch1))
  expect_equal(tst$overall$LL_dif, 1067.246, tolerance = .1)
  expect_equal(tst$pairwise$LL_dif, c(49.8339753225328, 61.1695798706335, 0.0196484043474641)
               , tolerance = .1)
})
