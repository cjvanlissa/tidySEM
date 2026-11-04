test_that("profile CIs are matched by exact parameter names", {

  nms <- c("mu1", "tau2", "weights[1,1]")
  targets <- c("weights[1,1]", "mu1", "tau2")

  expect_equal(tidySEM:::match_mx_names(nms, targets), c(2L, 3L, 1L))
})


test_that("profile CIs are matched after removing model prefixes", {

  nms <- c(
    "mu1",
    "RE_Mixture2.weights[1,1]",
    "RE_Mixture2.weights[1,2]"
  )

  targets <- c(
    "mu1",
    "weights[1,1]",
    "weights[1,2]"
  )

  expect_equal(tidySEM:::match_mx_names(nms, targets), 1:3)
})


test_that("ambiguous parameter names are not matched", {

  nms <- c(
    "class1.M[1,1]",
    "class2.M[1,1]"
  )

  targets <- "M[1,1]"

  expect_true(all(is.na(
    tidySEM:::match_mx_names(nms, targets)
  )))
})


test_that("duplicate exact names are not matched", {

  expect_true(all(is.na(
    tidySEM:::match_mx_names(
      c("mu1", "mu1"),
      c("mu1", "mu2")
    )
  )))

  expect_true(all(is.na(
    tidySEM:::match_mx_names(
      "mu1",
      c("mu1", "mu1")
    )
  )))
})



test_that("valid profile CIs replace Wald CIs", {

  skip_if_not_installed("OpenMx")

  set.seed(123)
  dat <- data.frame(y = rnorm(100, mean = 1, sd = 2))

  model <- as_ram(
    "y ~ 1
     y ~~ y",
    data = dat
  )

  # Request profile likelihood confidence intervals
  model <- OpenMx::mxModel(
    model,
    OpenMx::mxCI(c("M[1,1]", "S[1,1]"))
  )

  fit <- OpenMx::mxRun(
    model,
    intervals = TRUE,
    silent = TRUE
  )

  tab <- table_results(
    fit,
    columns = NULL,
    format_numeric = FALSE
  )

  ci <- summary(fit)$CI

  # Check that profile intervals were calculated
  expect_true(all(is.finite(ci[, "lbound"])))
  expect_true(all(is.finite(ci[, "ubound"])))

  # Check that profile intervals are used
  expect_true(all(tab$ci_type %in% c("Profile", NA)))

  # Compare reported intervals with OpenMx
  for (par in rownames(ci)) {

    idx <- tidySEM:::match_mx_names(par, tab$name)

    expect_false(is.na(idx))

    expect_equal(
      tab$ci_type[idx],
      "Profile"
    )

    expect_equal(
      tab$confint[idx],
      conf_int(
        x = NULL,
        lb = ci[par, "lbound"],
        ub = ci[par, "ubound"]
      )
    )
  }
})


test_that("invalid remix profile CIs fall back to Wald", {

  skip_if_not_installed("OpenMx")
  skip_if_not_installed("metamixture")
  library(OpenMx)
  set.seed(123)

  n <- 20
  C <- sample(1:2, n, replace = TRUE)

  vi <- runif(n, 0.01, 0.05)
  yi <- rnorm(
    n,
    mean = c(-1.5, 1.5)[C],
    sd = sqrt(vi + 0.2)
  )

  fit <- metamixture::remix(
    yi = yi,
    vi = vi,
    classes = 2,
    tau2 = "equal"
  )

  ci <- summary(fit)$CI

  expect_false(is.null(ci))

  # Identify invalid profile intervals
  invalid <- !is.finite(ci[, "lbound"]) |
    !is.finite(ci[, "ubound"])

  if ("note" %in% colnames(ci)) {
    notes <- trimws(as.character(ci[, "note"]))
    invalid <- invalid |
      (!is.na(notes) & nzchar(notes))
  }

  # Ensure the test actually exercises the fallback
  if (!any(invalid)) {
    skip("No invalid profile intervals were produced.")
  }

  tab <- table_results(
    fit,
    columns = NULL,
    format_numeric = FALSE
  )

  expect_true("ci_type" %in% names(tab))

  idx <- tidySEM:::match_mx_names(
    rownames(ci)[invalid],
    tab$name
  )

  # Check matched parameters
  keep <- which(!is.na(idx))

  expect_gt(length(keep), 0L)

  for (i in idx[keep]) {

    if (is.finite(tab$se[i])) {

      expect_equal(tab$ci_type[i], "Wald")

      expect_equal(
        tab$confint[i],
        conf_int(
          tab$est[i],
          se = tab$se[i]
        )
      )

    } else {
      expect_true(is.na(tab$ci_type[i]))
    }
  }
})



test_that("missing profile CIs use Wald intervals", {

  skip_if_not_installed("OpenMx")

  set.seed(123)
  dat <- data.frame(y = rnorm(100, mean = 1, sd = 2))

  # Fit model without requesting profile CIs
  model <- as_ram(
    "y ~ 1
     y ~~ y",
    data = dat
  )

  fit <- OpenMx::mxRun(
    model,
    intervals = FALSE,
    silent = TRUE
  )

  out <- table_results(
    fit,
    columns = NULL,
    format_numeric = FALSE
  )

  # Check that no profile CIs were calculated
  expect_true(is.null(summary(fit)$CI) ||
                NROW(summary(fit)$CI) == 0L)

  # Check that Wald intervals are used
  pars <- out[is.finite(out$se) & is.finite(out$est), ]

  expect_gt(nrow(pars), 0L)
  expect_true(all(pars$ci_type == "Wald"))

  expect_equal(
    pars$confint,
    conf_int(pars$est, se = pars$se)
  )
})



test_that("fixed parameters have no Wald CIs or p-values", {

  skip_if_not_installed("OpenMx")

  set.seed(123)
  dat <- data.frame(y = rnorm(100, mean = 1, sd = 2))

  # Freely estimated mean, fixed variance
  model <- as_ram(
    "y ~ 1
     y ~~ 4*y",
    data = dat
  )

  fit <- OpenMx::mxRun(
    model,
    intervals = FALSE,
    silent = TRUE
  )

  out <- table_results(
    fit,
    columns = NULL,
    format_numeric = FALSE
  )

  # Check that free status is preserved
  expect_true("free" %in% names(out))

  free_pars <- out[
    !is.na(out$free) & out$free,
    ,
    drop = FALSE
  ]

  fixed_pars <- out[
    !is.na(out$free) & !out$free,
    ,
    drop = FALSE
  ]

  expect_gt(nrow(free_pars), 0L)
  expect_gt(nrow(fixed_pars), 0L)

  # Free parameters have Wald intervals
  expect_true(all(free_pars$ci_type == "Wald"))
  expect_true(all(!is.na(free_pars$confint)))

  # Fixed parameters have no inferential statistics
  expect_true(all(is.na(fixed_pars$ci_type)))
  expect_true(all(is.na(fixed_pars$confint)))
  expect_true(all(is.na(fixed_pars$pval)))
})
