# Test that exported mx functions work without library(OpenMx)
if(requireNamespace("callr", quietly = TRUE)){
  test_openmx <- function(code, tolerance = 1e-6) {
    testthat::skip_on_cran()
    code <- substitute(code)

    run <- function(attach_openmx) {
      # Create a dedicated temporary directory
      tmp <- tempfile("tidysem-callr-")
      dir.create(tmp)

      # Clean up even if the test fails
      on.exit(
        unlink(tmp, recursive = TRUE, force = TRUE),
        add = TRUE
      )

      callr::r(
        function(attach_openmx, code) {
          if (attach_openmx) {
            library(OpenMx)
          }

          set.seed(123)

          stopifnot(identical(
            "package:OpenMx" %in% search(),
            attach_openmx
          ))

          # Load tidySEM without attaching OpenMx
          library(tidySEM)

          stopifnot(identical(
            "package:OpenMx" %in% search(),
            attach_openmx
          ))

          res <- eval(code)

          stopifnot(identical(
            "package:OpenMx" %in% search(),
            attach_openmx
          ))

          res
        },
        args = list(
          attach_openmx = attach_openmx,
          code = code
        ),
        package = FALSE,
        wd = tmp,
        env = c(
          TMPDIR = tmp,
          TMP = tmp,
          TEMP = tmp
        )
      )
    }

    without <- run(FALSE)
    with <- run(TRUE)

    testthat::expect_equal(
      without, with,
      tolerance = tolerance
    )

    invisible(list(without = without, with = with))
  }


  test_that("mx_dummies works without OpenMx attached", {
    test_openmx({
      mx_dummies(iris[1:5, ])
    })
  })



  test_that("mx_mixed_lca works without OpenMx attached", {
    test_openmx({
      n <- 200
      class <- rep(1:2, times = c(60, 140))

      df <- data.frame(
        X1 = rnorm(n, mean = ifelse(class == 1, 0, 6), sd = 1),
        X2 = ifelse(
          class == 1,
          sample(1:2, n, replace = TRUE, prob = c(.95, .05)),
          sample(2:3, n, replace = TRUE, prob = c(.05, .95))
        )
      )

      df$X2 <- OpenMx::mxFactor(df$X2, levels = 1:3)

      set.seed(1)
      res <- tidySEM::mx_mixed_lca(data = df, classes = 2)

      res@output$fit
    }, tolerance = 2)
  })


  test_that("mx_switch_labels works without OpenMx attached", {
    test_openmx({
      df <- iris[1:4]
      names(df) <- letters[1:4]

      res <- mx_profiles(data = df, classes = 2)

      mx_switch_labels(res, decreasing = FALSE)@output$fit

    })
  })


  test_that("estimate_mx works without OpenMx attached", {
    test_openmx({
      df <- iris[1:4]
      names(df) <- paste0("x_", 1:4)

      model <- tidy_sem(df)
      model <- measurement(model)

      estimate_mx(model)@output$fit
    })
  })


  test_that("run_mx works without OpenMx attached", {
    test_openmx({
      df <- iris[1:3]
      names(df) <- paste0("X_", 1:3)

      run_mx(
        measurement(
          tidy_sem(df),
          meanstructure = TRUE
        )
      )@output$fit
    })
  })


  test_that("mx_deviances works without OpenMx attached", {
    test_openmx({
      mx_deviances(
        vars = c("x", "y"),
        nThresh = 1L,
        free = FALSE,
        values = 1:2,
        labels = NA,
        lbound = NULL,
        ubound = NULL
      )
    })
  })


  test_that("mx_lca works without OpenMx attached", {
    test_openmx({
      df <- tidySEM::data_mix_ordinal
      df[1:4] <- lapply(df[1:4], ordered)

      mx_lca(
        data = df,
        classes = 2,
        extraTries = 2
      )@output$fit
    })
  })


  test_that("mx_growth_mixture works without OpenMx attached", {
    test_openmx({
      data("empathy", package = "tidySEM")
      df <- empathy[1:6]

      mx_growth_mixture(
        model = "
        i =~ 1*ec1 + 1*ec2 + 1*ec3 +
             1*ec4 + 1*ec5 + 1*ec6
        s =~ 0*ec1 + 1*ec2 + 2*ec3 +
             3*ec4 + 4*ec5 + 5*ec6
        ec1 ~~ vec1*ec1
        ec2 ~~ vec2*ec2
        ec3 ~~ vec3*ec3
        ec4 ~~ vec4*ec4
        ec5 ~~ vec5*ec5
        ec6 ~~ vec6*ec6
        i ~~ 0*i
        s ~~ 0*s
        i ~~ 0*s
      ",
        classes = 2,
        data = df
      )@output$fit
    })
  })


  test_that("mx_diagnostics works without OpenMx attached", {
    test_openmx({
      dat <- iris[1:2]
      names(dat) <- c("x", "y")

      res <- run_mx(
        as_ram("y ~ x", data = dat)
      )

      mx_diagnostics(res)
    })
  })


  test_that("mx_profiles works without OpenMx attached", {
    test_openmx({
      data("empathy", package = "tidySEM")
      df <- empathy[1:2]

      mx_profiles(
        data = df,
        classes = 2
      )@output$fit
    })
  })


  test_that("mx_data_quantiles works without OpenMx attached", {
    test_openmx({
      df <- data.frame(
        X = ordered(
          sample(
            1:4,
            size = 100,
            replace = TRUE,
            prob = c(.1, .2, .5, .2)
          )
        )
      )

      mx_data_quantiles(df)
    })
  })


  test_that("mx_mixture works without OpenMx attached", {
    test_openmx({
      df <- iris[1:2]
      names(df) <- c("x", "y")

      mx_mixture(
        model = list(
          "y ~ a*x",
          "y ~ b*x"
        ),
        meanstructure = TRUE,
        data = df
      )@output$fit
    })
  })

}
