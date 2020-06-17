library(testthat)
library(tidySEM)
library(dplyr)
df <- PoliticalDemocracy
names(df)[match(c("x1", "x2", "x3", "y1", "y2", "y3", "y4", "y5", "y6", "y7", "y8"), names(df))] <- c(paste0("ind_", 1:3), paste0("dem60_", 1:4), paste0("dem65_", 1:4))
model <- '
  # measurement model
    ind =~ ind_1 + ind_2 + ind_3
    dem60 =~ dem60_1 + dem60_2 + dem60_3 + dem60_4
    dem65 =~ dem65_1 + dem65_2 + dem65_3 + dem65_4'


fit <- sem(model, data=df)



df %>%
  tidy_sem() %>%
  measurement() -> model_tidy

fit2 <- sem(model_tidy$syntax, df)

test_that("Lavaan and measurement return same result", {
  expect_true((fitmeasures(fit)["chisq"] - fitmeasures(fit2)["chisq"]) < .001)
  })


model_reg <- '
  # measurement model
    ind =~ ind_1 + ind_2 + ind_3
    dem60 =~ dem60_1 + dem60_2 + dem60_3 + dem60_4
    dem65 =~ dem65_1 + dem65_2 + dem65_3 + dem65_4
    dem60 ~ ind
    dem65 ~ ind + dem60'

fit_reg <- sem(model_reg, data=df)

model_tidy %>%
  add_paths("dem60 ~ ind",
            "dem65 ~ ind + dem60") -> mod_tidy_reg

fit_reg_tidy <- sem(mod_tidy_reg$syntax, df)

test_that("Lavaan and measurement return same result", {
  expect_true((fitmeasures(fit_reg)["chisq"] - fitmeasures(fit_reg_tidy)["chisq"]) < .001)
})
