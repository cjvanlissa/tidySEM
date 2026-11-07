test_that("two_step estimates auxiliary models for RAM mixtures", {

  skip_if_not_installed("OpenMx")

  set.seed(1234)

  # Simulate two latent classes
  n <- 200
  class <- sample(1:2, n, replace = TRUE)

  dat <- data.frame(
    x1 = rnorm(n, mean = c(-2, 2)[class]),
    x2 = rnorm(n, mean = c(-2, 2)[class]),
    y  = rnorm(n, mean = c(0, 2)[class])
  )

  # Step 1: Fit latent profile model
  fit <- mx_profiles(
    data = dat[, c("x1", "x2")],
    classes = 2
  )

  # Step 2: Fit auxiliary model
  res <- two_step(
    x = fit,
    model = "y ~ 1",
    data = dat["y"]
  )

  # Model estimated successfully
  expect_s4_class(res, "MxModel")
  expect_true(isTRUE(res@.wasRun))
  expect_true(is.finite(res@output$fit))

  # First-stage parameters are fixed
  first_pars <- names(
    OpenMx::omxGetParameters(
      tidySEM:::label_free_parameters(fit),
      free = TRUE
    )
  )

  second_pars <- names(
    OpenMx::omxGetParameters(res, free = TRUE)
  )

  expect_length(
    intersect(first_pars, second_pars),
    0L
  )

  # Auxiliary parameters remain freely estimated
  expect_gt(length(second_pars), 0L)
  expect_true(all(grepl("^aux", second_pars)))

  # Corrected standard errors
  robust <- attr(res, "se_robust")

  expect_false(is.null(robust))
  expect_s3_class(robust, "data.frame")
  expect_true(all(c("label", "se") %in% names(robust)))
  expect_gt(nrow(robust), 0L)
  expect_true(all(is.finite(robust$se)))
  expect_true(all(robust$se >= 0))

  # Results table uses corrected SEs
  tab <- table_results(
    res,
    columns = NULL,
    format_numeric = FALSE
  )

  idx <- match(robust$label, tab$openmx_label)
  expect_false(anyNA(idx))

  expect_equal(
    tab$se[idx],
    robust$se,
    tolerance = 1e-6
  )
})



test_that("two_step handles continuous, ordinal, and multivariate outcomes", {

  skip_if_not_installed("OpenMx")

  # Prepare data
  dat <- iris[, 1:4]
  names(dat) <- c("SL", "SW", "PL", "PW")

  set.seed(123)

  fit <- mx_profiles(
    data = dat[, c("PL", "PW")],
    classes = 2
  )

  # Continuous distal outcome
  res <- two_step(
    x = fit,
    data = dat$SL
  )

  expect_s4_class(res, "MxModel")
  expect_true(isTRUE(res@.wasRun))
  expect_true(is.finite(res@output$fit))

  tab <- table_results(
    res,
    columns = NULL,
    format_numeric = FALSE
  )

  expect_s3_class(tab, "data.frame")
  expect_gt(nrow(tab), 0L)
  expect_true(all(tab$se[tab$free] < .07))
  # Check robust standard errors
  se_robust <- attr(res, "se_robust")

  expect_false(is.null(se_robust))
  expect_true(all(c("label", "se") %in% names(se_robust)))
  expect_true(any(is.finite(se_robust$se)))

  # Equivalent RAM syntax
  res_ram <- two_step(
    x = fit,
    model = "SL ~ 1",
    data = dat["SL"]
  )

  tab2 <- table_results(
    res_ram,
    columns = NULL,
    format_numeric = FALSE
  )
  expect_s4_class(res_ram, "MxModel")
  expect_true(isTRUE(res_ram@.wasRun))
  expect_true(is.finite(res_ram@output$fit))

  # Both specifications should give equivalent likelihoods
  expect_equal(
    as.numeric(res@output$fit),
    as.numeric(res_ram@output$fit),
    tolerance = 1e-4
  )
  expect_equal(tab2$est[tab2$free], tab$est[tab$free])
  expect_equal(tab2$se[tab2$free], tab$se[tab$free])

  # Ordinal distal outcome
  ordinal <- ordered(
    cut(dat$SL, breaks = 3, labels = FALSE)
  )

  res_ord <- two_step(
    x = fit,
    data = ordinal
  )

  expect_s4_class(res_ord, "MxModel")
  expect_true(isTRUE(res_ord@.wasRun))
  expect_true(is.finite(res_ord@output$fit))

  # 4. Multivariate continuous auxiliary model
  res_custom <- two_step(
    x = fit,
    model = "
      SL ~ 1
      SW ~ 1
      SL ~~ SW
    ",
    data = dat[, c("SL", "SW")]
  )

  expect_s4_class(res_custom, "MxModel")
  expect_true(isTRUE(res_custom@.wasRun))
  expect_true(is.finite(res_custom@output$fit))

  # Verify first-stage parameters remain fixed
  first_stage <- names(
    OpenMx::omxGetParameters(
      tidySEM:::label_free_parameters(fit),
      free = TRUE
    )
  )

  second_stage <- names(
    OpenMx::omxGetParameters(
      res_custom,
      free = TRUE
    )
  )

  expect_length(
    intersect(first_stage, second_stage),
    0L
  )

  # Verify auxiliary parameters are freely estimated
  expect_gt(length(second_stage), 0L)
})

