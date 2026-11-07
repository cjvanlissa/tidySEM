test_that("one_step jointly estimates measurement and auxiliary models", {

  skip_if_not_installed("OpenMx")

  set.seed(123)

  dat <- iris[, 1:4]
  names(dat) <- c("SL", "SW", "PL", "PW")

  fit <- mx_profiles(
    data = dat[, c("PL", "PW")],
    classes = 2
  )

  res <- one_step(
    x = fit,
    model = "SL ~ 1",
    data = dat["SL"]
  )

  expect_s4_class(res, "MxModel")
  expect_true(isTRUE(res@.wasRun))
  expect_true(is.finite(res@output$fit))

  # Measurement parameters remain free
  first <- names(OpenMx::omxGetParameters(
    tidySEM:::label_free_parameters(fit),
    free = TRUE
  ))

  after <- names(OpenMx::omxGetParameters(
    res,
    free = TRUE
  ))

  expect_true(all(first %in% after))

  # Auxiliary parameters are also freely estimated
  expect_true(any(grepl("^aux", after)))

  # One-step estimation should not attach
  # two-step corrected standard errors
  expect_null(attr(res, "se_robust"))
})
