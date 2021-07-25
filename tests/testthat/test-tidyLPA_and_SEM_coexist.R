if(FALSE){
library(tidySEM)
library(tidyLPA)
df <- iris[1:3]
names(df) <- letters[1:3]
res <- tidyLPA::estimate_profiles(df, 2)
test_that("tidyLPA and tidySEM can coexist with similarly named methods", {
  expect_error(plot_profiles(res), NA)
  expect_error(plot_density(res), NA)
  expect_error(suppressWarnings(tidyLPA:::plot_bivariate.tidyProfile(res)), NA)
})

res <- mx_profiles(df, 2)
test_that("tidyLPA and tidySEM can coexist with similarly named methods", {
  expect_error(plot_profiles(res), NA)
  expect_error(plot_density(res), NA)
  expect_error(suppressWarnings(plot_bivariate(res)), NA)
})
}
