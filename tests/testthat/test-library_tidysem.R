test_tidysem <- function(code, tolerance = 1e-6) {
  tidySEM:::skip_if_not_local()
  testthat::skip_if_not_installed("callr")
  code <- substitute(code)

  run <- function(attach_tidysem) {
    tmp <- tempfile("tidysem-test-")
    dir.create(tmp)

    on.exit(
      unlink(tmp, recursive = TRUE, force = TRUE),
      add = TRUE
    )

    callr::r(
      function(attach_tidysem, code) {
        if (attach_tidysem) {
          library(tidySEM)
        } else {
          loadNamespace("tidySEM")
        }

        stopifnot(identical(
          "package:tidySEM" %in% search(),
          attach_tidysem
        ))

        set.seed(123)
        res <- eval(code)

        stopifnot(identical(
          "package:tidySEM" %in% search(),
          attach_tidysem
        ))

        res
      },
      args = list(
        attach_tidysem = attach_tidysem,
        code = code
      ),
      package = FALSE,
      wd = tmp
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

test_that("mx_growth_mixture works without tidySEM attached", {
  skip_if_not_installed("OpenMx"); tidySEM:::skip_if_not_local()

  test_tidysem({
    data("empathy", package = "tidySEM")

    fit <- tidySEM::mx_growth_mixture(
      model = "
        i =~ 1*ec1 + 1*ec2
        s =~ 0*ec1 + 1*ec2
        ec1 ~~ vec1*ec1
        ec2 ~~ vec2*ec2
        i ~~ 0*i
        s ~~ 0*s
        i ~~ 0*s
      ",
      classes = 2,
      data = empathy[1:2]
    )

    as.numeric(fit@output$fit)
  })
})

test_that("plot_bivariate works without tidySEM attached", {
  skip_if_not_installed("OpenMx"); tidySEM:::skip_if_not_local()
  test_tidysem({
    iris_sample <- iris[c(1:5, 145:150), c("Sepal.Length", "Sepal.Width")]
    names(iris_sample) <- c("x", "y")
    res <- tidySEM::mx_profiles(iris_sample, classes = 2)
    tidySEM::plot_bivariate(res, rawdata = FALSE)
  })
})

test_that("plot_density works without tidySEM attached", {
  skip_if_not_installed("OpenMx"); tidySEM:::skip_if_not_local()
  test_tidysem({
    dat <-
      iris[, c("Petal.Length", "Petal.Width")]
    names(dat) <- paste0("x", 1:2)
    res <- tidySEM::mx_profiles(dat, 1:2)
    tidySEM::plot_density(res)
  })
})

test_that("edit_graph works without tidySEM attached", {
  skip_if_not_installed("OpenMx"); tidySEM:::skip_if_not_local()
  test_tidysem({
    dat <- iris
    names(dat) <- letters[1:5]
    res <- tidySEM::run_mx(tidySEM::as_ram("a ~ b + c", data = dat))
    p <- tidySEM::prepare_graph(res)
    p <- tidySEM::edit_graph(p, {colour = "blue"}, element = "nodes")
    plot(p)


  })
})

test_that("plot_profiles works without tidySEM attached", {
  skip_if_not_installed("OpenMx"); tidySEM:::skip_if_not_local()
  test_tidysem({
    df_plot <- data.frame(Variable = "x1",
                          Class = "class1",
                          Classes = 1,
                          Model = "equal var 1",
                          Value = 3.48571428571429,
                          se = 0.426092805342181,
                          Value.Variances = 3.81265306156537,
                          se.Variances = 1.17660769119959)
    tidySEM::plot_profiles(list(df_plot = df_plot, df_raw = NULL),
                  ci = NULL, sd = FALSE, add_line = FALSE,
                  rawdata = FALSE, bw = FALSE)


  })
})

test_that("plot_prob works without tidySEM attached", {
  skip_if_not_installed("OpenMx"); tidySEM:::skip_if_not_local()
  test_tidysem({
    df_plot <- data.frame(Variable = rep(c("u1", "u2"), each = 3),
                          Category = rep(1:3, 2),
                          Probability = c(0.3381302605812, 0.148395173612088, 0.513474565806711,
                                          0.472337708760608, 0.118484201496432, 0.40917808974296))
    tidySEM::plot_prob(df_plot)


  })
})

test_that("plot_growth works without tidySEM attached", {
  skip_if_not_installed("OpenMx"); tidySEM:::skip_if_not_local()
  test_tidysem({
    df <- tidySEM::empathy[1:2]
    tidySEM::mx_growth_mixture(model = "i =~ 1*ec1 + 1*ec2
                           s =~ 0*ec1 + 1*ec2
                           ec1 ~~ vec1*ec1
                           ec2 ~~ vec2*ec2
                           i ~~ 0*i
                           s ~~ 0*s
                           i ~~ 0*s
                           i ~NA*1
                           s ~NA*1",
                      classes = 2,
                      data = df) -> res
    tidySEM::plot_growth(res)
  })
})

test_that("table_results works without tidySEM attached", {
  skip_if_not_installed("OpenMx"); tidySEM:::skip_if_not_local()
  test_tidysem({
    HS.model <- '  visual =~ x1 + x2 + x3
               textual =~ x4 + x5 + x6
               speed   =~ x7 + x8 + x9 '
    fit <- tidySEM::run_mx(tidySEM::as_ram(HS.model, data = lavaan::HolzingerSwineford1939, group = "school"))
    tidySEM::table_results(fit)
  })
})

test_that("get_nodes works without tidySEM attached", {
  skip_if_not_installed("OpenMx"); tidySEM:::skip_if_not_local()
  test_tidysem({
    HS.model <- '  visual =~ x1 + x2 + x3
               textual =~ x4 + x5 + x6
               speed   =~ x7 + x8 + x9 '
    fit <- tidySEM::run_mx(tidySEM::as_ram(HS.model, data = lavaan::HolzingerSwineford1939, group = "school"))
    tidySEM::get_nodes(fit)
  })
})

test_that("get_nodes works without tidySEM attached", {
  skip_if_not_installed("OpenMx"); tidySEM:::skip_if_not_local()
  test_tidysem({
    HS.model <- '  visual =~ x1 + x2 + x3
               textual =~ x4 + x5 + x6
               speed   =~ x7 + x8 + x9 '
    fit <- tidySEM::run_mx(tidySEM::as_ram(HS.model, data = lavaan::HolzingerSwineford1939, group = "school"))
    tidySEM::get_edges(fit, label = paste(est_sig, confint))
  })
})
