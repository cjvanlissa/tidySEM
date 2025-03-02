if(requireNamespace("OpenMx", quietly = TRUE)){
  # Building model with umxThresholdMatrix works:
  test_that("ordinal mixture model works with different methods", {
    tidySEM:::skip_if_not_local()
    if(requireNamespace("OpenMx", quietly = TRUE) & requireNamespace("umx", quietly = TRUE)){
      set.seed(1)
      df <- data_mix_ordinal
      df[1:4] <- lapply(df, ordered)
      c1 <- mxModel(model = "class1",
                    type = "RAM",
                    manifestVars = names(df),
                    mxPath(from = "one", to = names(df), free = FALSE, values = 0),
                    mxPath(from = names(df), to = names(df), free = FALSE, values = 1, arrows = 2),
                    umxThresholdMatrix(df, names(df), method = "allFree"),
                    mxFitFunctionML(vector=TRUE))
      c1$expectation$thresholds <- "threshMat"
      c1$deviations_for_thresh$values <- structure(c(0.493797002905578, 0.292025391567409, 0.0954617695580417,
                                                     0.278707105926806, -0.414864787727856, 0.28685436808645, -0.368187507919911,
                                                     0.322331242046851), .Dim = c(2L, 4L), .Dimnames = list(c("dev_1",
                                                                                                              "dev_2"), c("u1", "u2", "u3", "u4")))

      c2 <- mxModel(c1,
                    name = "class2")
      c1$deviations_for_thresh$labels <- paste0(c1$deviations_for_thresh$labels, "_1")
      c2$deviations_for_thresh$labels <- paste0(c2$deviations_for_thresh$labels, "_2")
      mix <- mxModel(model = "mix",
                     c1,
                     c2,
                     mxData(df, type = "raw"),
                     mxMatrix(values=c(1, 0), nrow=1, ncol=2, lbound = 1e-4, free=c(FALSE, TRUE), name="weights"),
                     mxExpectationMixture(paste0("class", 1:2), weights = "weights", scale="sum"),
                     mxFitFunctionML())
      c2$deviations_for_thresh$values <- structure(c(-0.417588977444548, 0.383792478783957, -0.0693951277323035,
                                                     0.299055722485786, 0.231319786555848, 0.32146641858211, 0.204438137718803,
                                                     0.277326511781225), .Dim = c(2L, 4L), .Dimnames = list(c("dev_1",
                                                                                                              "dev_2"), c("u1", "u2", "u3", "u4")))

      res <- mxTryHardOrdinal(mix, extraTries = 10)
      tmp <- class_prob(res)

      mx_props <- table_results(res, columns=c("lhs", "op", "rhs", "est", "matrix"))
      mx_fit <- table_fit(res)
      mat <- matrix(as.numeric(mx_props$est[mx_props$matrix == "threshMat"]), nrow = 2, byrow = F)
      mat <- mat[, unlist(list(c(1:4), c(5:8))[order(tmp$sum.posterior$proportion)])]

      # df_mp <- df
      # df_mp[1:4] <- lapply(df, as.integer)
      # res_mp <- mplusObject(VARIABLE = "categorical are u1-u4;
      #                         CLASSES = c(2);",
      #                       ANALYSIS = "type = MIXTURE;
      #                       starts = 1000 20",
      #             rdata = df_mp,
      #             modelout = "test.inp",
      #             run = 1L
      #             )
      # dput(res_mp$results$summaries$LL, file = "clipboard")
      # dput(res_mp$results$class_counts$modelEstimated$proportion, file = "clipboard")
      # dput(cbind(
      #   matrix(res_mp$results$parameters$unstandardized$est[9:16], nrow = 2),
      #   matrix(res_mp$results$parameters$unstandardized$est[1:8], nrow = 2)
      # ), file = "clipboard")
      # dput(cbind(
      #   res_mp$results$parameters$unstandardized$est[unlist(list(c(1:8), c(9:16))[order(res_mp$results$class_counts$modelEstimated$proportion)])]
      # ), file = "clipboard")


      expect_equal(-19300.165, mx_fit$LL, tolerance = .02)
      expect_equivalent(sort(tmp$sum.posterior$proportion),
                        sort(c(0.5789, 0.4211)), tolerance = .001)

      expect_equivalent(sort(pnorm(mat)),
                        sort(pnorm(c(0.797, 1.289, 0.152, 0.601, -0.667, -0.204, -0.591,
                                     -0.073, -0.672, -0.054, -0.111, 0.367, 0.37, 0.894, 0.327, 0.777
                        ))), tolerance = .12) # Note high tolerance!


      # Building model with mxThreshold, does NOT work:
      c1 <- mxModel(model = "class1",
                    type = "RAM",
                    manifestVars = names(df),
                    mxPath(from = "one", to = names(df), free = FALSE, values = 0),
                    mxPath(from = names(df), to = names(df), free = FALSE, values = 1, arrows = 2),
                    mxThreshold(names(df), nThresh = rep(2, 4), free = TRUE),
                    mxFitFunctionML(vector=TRUE))


      c2 <- mxModel(c1,
                    name = "class2")

      mix_mxthres <- mxModel(model = "mix",
                             c1,
                             c2,
                             mxData(df, type = "raw"),
                             mxMatrix(values=c(1, 0), nrow=1, ncol=2, lbound = 1e-4, free=c(FALSE, TRUE), name="weights"),
                             mxExpectationMixture(paste0("class", 1:2), weights = "weights", scale="sum"),
                             mxFitFunctionML())

      res_mxthres <- mxTryHardOrdinal(mix_mxthres, extraTries = 10)
      tmp_mxthres <- class_prob(res_mxthres)

      props_mxthres <- table_results(res_mxthres, columns=c("label", "matrix", "est"))
      fit_mxthres <- table_fit(res_mxthres)
      mat_mxthres <- matrix(as.numeric(props_mxthres$est[props_mxthres$matrix == "Thresholds"]), nrow = 2, byrow = F)

      # ordinal mixture model works with mxThreshold
      expect_failure(expect_equal(-19300.165, fit_mxthres$LL, tolerance = 1e-3))
      expect_failure(expect_equivalent(sort(tmp_mxthres$sum.posterior$proportion),
                                       sort(c(0.5789, 0.4211)), tolerance = .001))

      expect_failure(expect_equivalent(sort(pnorm(mat_mxthres)),
                                       sort(pnorm(c(0.797, 1.289, 0.152, 0.601, -0.667, -0.204, -0.591,
                                                    -0.073, -0.672, -0.054, -0.111, 0.367, 0.37, 0.894, 0.327, 0.777
                                       ))), tolerance = .12))



      # test_that("mxThreshold works", {
      #   expect_equivalent(mat_mxthres, res_po_thres, tolerance = .005)
      # })

      # Building model with mx_thresholds:
      thresh <- tidySEM:::mx_thresholds(df)

      c1 <- mxModel(model = "class1",
                    type = "RAM",
                    manifestVars = names(df),
                    mxPath(from = "one", to = names(df), free = FALSE, values = 0),
                    mxPath(from = names(df), to = names(df), free = FALSE, values = 1, arrows = 2),
                    thresh,
                    mxFitFunctionML(vector=TRUE))
      c1$expectation$thresholds <- "Thresholds"

      c2 <- mxModel(c1,
                    name = "class2")

      mix_tidysem <- mxModel(model = "mix",
                             c1,
                             c2,
                             mxData(df, type = "raw"),
                             mxMatrix(values=c(1, 0), nrow=1, ncol=2, lbound = 1e-4, free=c(FALSE, TRUE), name="weights"),
                             mxExpectationMixture(paste0("class", 1:2), weights = "weights", scale="sum"),
                             mxFitFunctionML()
      )
      c1$mat_dev$values <- structure(c(0.493797002905578, 0.292025391567409, 0.0954617695580417,
                                       0.278707105926806, -0.414864787727856, 0.28685436808645, -0.368187507919911,
                                       0.322331242046851), .Dim = c(2L, 4L))
      c2$mat_dev$values <- structure(c(-0.417588977444548, 0.383792478783957, -0.0693951277323035,
                                       0.299055722485786, 0.231319786555848, 0.32146641858211, 0.204438137718803,
                                       0.277326511781225), .Dim = c(2L, 4L))

      res_tidysem <- mxTryHardOrdinal(mix_tidysem, extraTries = 10)
      tmp_tidysem <- class_prob(res_tidysem)

      props_tidysem <- table_results(res_tidysem, columns=c("label", "matrix", "est"))
      props_tidysem <- as.numeric(props_tidysem$est[props_tidysem$matrix == "Thresholds"])
      fit_tidysem <- table_fit(res_tidysem)


      # ordinal mixture model works with mx_threshold
      expect_equal(-19300.165, fit_tidysem$LL, tolerance = .02)
      expect_equivalent(sort(tmp_tidysem$sum.posterior$proportion),
                        sort(c(0.5789, 0.4211)), tolerance = .001)

      expect_equivalent(sort(pnorm(props_tidysem)),
                        sort(pnorm(c( 0.797, 1.289, 0.152, 0.601, -0.667, -0.204, -0.591, -0.073,
                                      -0.672, -0.054, -0.111, 0.367, 0.37, 0.894, 0.327, 0.777
                        ))), tolerance = .12) # Note high tolerance!
      expect_equivalent(sort(pnorm(props_tidysem)),
                        sort(pnorm(mat)), tolerance = 1e-5)
    }

  })

}
