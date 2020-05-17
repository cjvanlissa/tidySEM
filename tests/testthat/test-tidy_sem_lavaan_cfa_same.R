df <- HolzingerSwineford1939
names(df)[grepl("^x", names(df))] <- c("vis_1", "vis_2", "vis_3", "tex_1", "tex_2", "tex_3", "spe_1", "spe_2", "spe_3")

mod <- "vis =~ vis_1 + vis_2 + vis_3
tex =~ tex_1 + tex_2 + tex_3
spe =~ spe_1 + spe_2 + spe_3"

res1 <- lavaan::sem (mod, data = df, std.lv = T, meanstructure = T)
parTable(res1)


mod_ts <- measurement(tidy_sem(df))
res2 <- sem(as.lavaan(mod_ts), data = df)


library(testthat)

pt1 <- parTable(res1)
pt2 <- parTable(res2)

test_that("All parameters the same", {
  expect_true(all(do.call(paste0, pt1[, 2:4]) %in% do.call(paste0, pt2[, 2:4])))
  expect_true(all(do.call(paste0, pt2[, 2:4]) %in% do.call(paste0, pt1[, 2:4])))

  expect_true(all(do.call(paste0, pt1[!pt1$free == 0, 2:4]) %in% do.call(paste0, pt2[!pt2$free == 0, 2:4])))
  expect_true(all(do.call(paste0, pt2[!pt2$free == 0, 2:4]) %in% do.call(paste0, pt1[!pt1$free == 0, 2:4])))
})

