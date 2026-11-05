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
