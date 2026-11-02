test_that("num_classes works for latent profile models", {

  skip_if_not_installed("OpenMx")

  set.seed(456)

  dat <- data.frame(
    x1 = rnorm(100),
    x2 = rnorm(100)
  )

  for (K in 1:2) {

    # Unestimated model
    model <- mx_profiles(
      data = dat,
      classes = K,
      run = FALSE,
      run_mixture_starts = FALSE
    )

    expect_equal(tidySEM:::num_classes(model), K)

    # Estimated model
    fit <- OpenMx::mxRun(
      model,
      intervals = FALSE,
      silent = TRUE
    )

    expect_equal(tidySEM:::num_classes(fit), K)
  }
})


test_that("num_classes works for lists of mixture models", {


    skip_if_not_installed("OpenMx")

    set.seed(456)

    dat <- data.frame(
      x1 = rnorm(100),
      x2 = rnorm(100)
    )

    model <- mx_profiles(
        data = dat,
        classes = 1:2,
        run = FALSE,
        run_mixture_starts = FALSE
      )
      expect_equal(unname(sapply(model, tidySEM:::num_classes, USE.NAMES = F)), c(1L:2L))

      # Estimated model
      fit <- lapply(model, OpenMx::mxRun, intervals = FALSE, silent = TRUE)

      expect_equal(unname(sapply(fit, tidySEM:::num_classes, USE.NAMES = F)), c(1L:2L))


})
