library(OpenMx)
df <- data_mix_ordinal[1:2]
df[1:2] <- lapply(df, ordered)

sem("u1 | th11*t1
    u1 | th12*t2
    u2 | th21*t1
    u2 | th22*t2", data = df) -> res

tabres <- table_results(res, columns = NULL)
tabres <- tabres[!tabres$lavaan_label == "", c("lavaan_label", "est")]
as_ram("u1 | th11*t1
    u1 | th12*t2
    u2 | th21*t1
    u2 | th22*t2", data = df) -> mod
res_mx <- mxRun(mod)

tabres_mx <- table_results(res_mx, columns = NULL)
tabres_mx <- tabres_mx[!is.na(tabres_mx$openmx_label), c("openmx_label", "est")]
names(tabres_mx)[1] <- "lavaan_label"
tabres <- merge(tabres, tabres_mx, by = "lavaan_label")
test_that("lavaan and openmx work with labeled thresholds",
          {
            expect_equivalent(as.numeric(tabres[[3]]), as.numeric(tabres[[2]]), tolerance = .005)
          })

sem("u1 | th11*t1
    u1 | th2*t2
    u2 | th21*t1
    u2 | th2*t2", data = df) -> res2

tabres2 <- table_results(res2, columns = NULL)

as_ram("u1 | th11*t1
    u1 | th2*t2
    u2 | th21*t1
    u2 | th2*t2", data = df) -> mod2
res_mx2 <- mxRun(mod2)

tabres_mx2 <- table_results(res_mx2, columns = NULL)
tabres2 <- tabres2[!tabres2$lavaan_label == "", c("lavaan_label", "est")]
tabres_mx2 <- tabres_mx2[!is.na(tabres_mx2$openmx_label), c("openmx_label", "est")]
names(tabres_mx2)[1] <- "lavaan_label"
tabres2 <- merge(tabres2, tabres_mx2, by = "lavaan_label")
test_that("lavaan and openmx work with constrained thresholds",
          {
            expect_equivalent(as.numeric(tabres2[[2]]), as.numeric(tabres2[[3]]), tolerance = .008)
          })



# Now, try with difference matrix -----------------------------------------
if(FALSE){
thresh <- tidySEM:::mx_thresholds(df)
thresh$alg_thres$labels <- matrix(paste0(rep(dimnames(thresh$alg_thres)[[2]], each = length(dimnames(thresh$alg_thres)[[1]])), "_", rep(dimnames(thresh$alg_thres)[[1]], length(dimnames(thresh$alg_thres)[[2]]))), nrow = length(dimnames(thresh$alg_thres)[[1]]))

thresh <- c(thresh, list(
  mxConstraint(name = "th2", test.mat_dev[1,1]+test.mat_dev[2,1] == test.mat_dev[1,2]+test.mat_dev[2,2])
))

mod3 <- mxModel(model = "test",
              type = "RAM",
              manifestVars = names(df),
              mxPath(from = "one", to = names(df), free = FALSE, values = 0),
              mxPath(from = names(df), to = names(df), free = FALSE, values = 1, arrows = 2),
              thresh,
              mxFitFunctionML(),
              mxData(df, type = "raw"))
mod3$expectation$thresholds <- "Thresholds"
res_mx3 <- mxRun(mod3)
#summary(res_mx3)
res_mx3$Thresholds$result
}
