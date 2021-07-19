library(lavaan)
HS.model <- ' visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '

tmp <- as_ram(HS.model, groups = c("a", "b"))
test_that("as_ram creates groups", {
  expect_equal(names(tmp@submodels), letters[1:2])
})

fitL <- cfa(HS.model,
           data = HolzingerSwineford1939,
           group = "school")
parL <- table_results(fitL, columns = NULL)

fitM <- run_mx(tmp, data = HolzingerSwineford1939, groups = "school")
parM <- table_results(fitM, columns = NULL)

test_that("mx approximately same results as lav", {
  expect_equivalent(as.numeric(parL$est[parL$op == "=~" & grepl("^x\\d$", parL$rhs)]),parM$est[grepl("\\.BY\\.", parM$label)], tolerance = .01)
})

tmp <- as_ram(HS.model, groups = c("school"), data = HolzingerSwineford1939)

fitM <- run_mx(tmp)
parM <- table_results(fitM, columns = NULL)
# Note: Order of groups is reversed by lavaan
parL <- parL[order(parL$group), ]
test_that("as_ram accepts groups and data", {
  expect_equivalent(as.numeric(parL$est[parL$op == "=~"]),parM$est[grepl("\\.BY\\.", parM$label)], tolerance = .01)
})

dat <- HolzingerSwineford1939[c("school", paste0("x", 1:9))]
names(dat)[-1] <- paste(rep(c("vis", "tex", "spd"), each = 3), rep(1:3, 3), sep ="_")
tmp <- tidy_sem(dat)
tmp <- measurement(tmp, groups = "school")
run_mx(tmp) -> tst
resM <- summary(tst)$parameters
parL <- parL[order(parL$group, decreasing = T), ]
test_that("measurement accepts groups argument", {
  expect_equivalent(as.numeric(parL$est[parL$op == "=~" & grepl("^x[2-3,5-6,8-9]$", parL$rhs)]),
                    resM$Estimate[grepl("^(vis|tex|spd)_\\d$", resM$row) & resM$col %in% c("vis", "tex", "spd")], tolerance = .01)
})


# Add paths to multigroup -------------------------------------------------

tmp <- add_paths(tmp, "vis ~ tex + spd")
run_mx(tmp) -> res_add
tab <- summary(res_add)$parameters
#tab <- table_results(res_add, columns = NULL)
test_that("multigroup tidysem can be updated", {
  expect_true(sum(endsWith(tab$matrix, ".A") & tab$row == "vis") == 4)
  #expect_true(sum(tab$openmx.label == "a") == 2 & sum(tab$openmx.label == "b") == 2)
})

tmp <- add_paths(tmp, "vis ~ c(a, a)*tex + c(b, b)*spd")
run_mx(tmp) -> res_add
tab <- table_results(res_add, NULL)

test_that("multigroup tidysem can be updated", {
  expect_true(sum(tab$openmx.label == "a") == 2 & sum(tab$openmx.label == "b") == 2)
})
