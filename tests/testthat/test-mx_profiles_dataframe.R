if(FALSE){library(testthat)
set.seed(1)
n = 100
C <- sample(c("Man", "Woman"), n, replace = TRUE)
means <- c(Man = 10, Woman = 7)
X <- rnorm(n, mean = means[C], sd = 1)
test_that("mx_profiles works with data = data.frame()", {
  expect_error({est <- mx_profiles(data = data.frame(X), classes = 1:3)}, NA)
})
}
