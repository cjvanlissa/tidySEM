# Place in tests/testthat/test-prepare-graph-models.R
# Run with devtools::test(filter = "prepare-graph-models")

# These tests deliberately check invariants rather than exact coordinates:
# a change in a layout algorithm should not break a valid graph.

check_prepared_graph <- function(g, expected_panels = NULL) {
  testthat::expect_s3_class(g, "sem_graph")
  testthat::expect_true(is.data.frame(g$nodes))
  testthat::expect_true(is.data.frame(g$edges))
  testthat::expect_true(all(c("name", "x", "y", "node_xmin",
                              "node_xmax", "node_ymin", "node_ymax") %in%
                            names(g$nodes)))
  testthat::expect_true(all(c("from", "to") %in% names(g$edges)))
  if (nrow(g$edges)) {
    testthat::expect_true(all(c("connect_from", "connect_to", "curvature") %in%
                              names(g$edges)))
  }
  nd <- g$nodes
  ed <- g$edges
  testthat::expect_true(nrow(nd) > 0L)
  testthat::expect_true(all(is.finite(nd$x) & is.finite(nd$y)))
  testthat::expect_true(all(nd$node_xmin < nd$node_xmax))
  testthat::expect_true(all(nd$node_ymin < nd$node_ymax))
  testthat::expect_equal(nd$x, (nd$node_xmin + nd$node_xmax) / 2,
                         tolerance = 1e-8)
  testthat::expect_equal(nd$y, (nd$node_ymin + nd$node_ymax) / 2,
                         tolerance = 1e-8)

  panel_cols <- intersect(c("group", "level"), names(nd))
  if (length(panel_cols)) {
    testthat::expect_false(anyDuplicated(nd[c(panel_cols, "name")]) > 0L)
  } else {
    testthat::expect_false(anyDuplicated(nd$name) > 0L)
  }

  # Edge endpoints must exist in the *same* panel, not merely somewhere
  # in the combined graph. Only check dimensions available in both tables.
  for (i in seq_len(nrow(ed))) {
    candidate <- nd
    for (key in intersect(c("group", "level"), intersect(names(ed), names(nd)))) {
      candidate <- candidate[!is.na(candidate[[key]]) &
                               candidate[[key]] == ed[[key]][i], , drop = FALSE]
    }
    testthat::expect_true(ed$from[i] %in% candidate$name,
                          info = paste("source of edge", i))
    testthat::expect_true(ed$to[i] %in% candidate$name,
                          info = paste("target of edge", i))
  }
  if (nrow(ed)) {
    testthat::expect_true(all(ed$connect_from %in%
                                c("top", "bottom", "left", "right")))
    testthat::expect_true(all(ed$connect_to %in%
                                c("top", "bottom", "left", "right")))
    loops <- which(ed$from == ed$to & ed$arrow == "both")
    if (length(loops)) {
      testthat::expect_equal(ed$connect_from[loops], ed$connect_to[loops])
    }
  }
  if (!is.null(expected_panels)) {
    testthat::expect_equal(nrow(unique(nd[c("group", "level")])),
                           expected_panels)
  }
  invisible(g)
}

# Common SEM syntax with enough indicators for identification.
.cfa_syntax <- 'visual =~ x1 + x2 + x3\ntextual =~ x4 + x5 + x6\nvisual ~~ textual'

# The standard lavaan Holzinger-Swineford dataset has two schools and
# therefore provides a real two-group fit without artificial replication.
make_lavaan_fit <- function(multigroup = FALSE) {
  data("HolzingerSwineford1939", package = "lavaan", envir = environment())
  lavaan::cfa(
    .cfa_syntax,
    data = HolzingerSwineford1939,
    group = if (multigroup) "school" else NULL
  )
}

testthat::test_that("lavaan single-group graph prepares successfully", {
  testthat::skip_if_not_installed("lavaan")
  fit <- make_lavaan_fit(FALSE)
  testthat::expect_true(lavaan::lavInspect(fit, "converged"))
  g <- prepare_graph(fit)
  check_prepared_graph(g)
  testthat::expect_false("group" %in% names(g$nodes) &&
                           length(unique(g$nodes$group)) > 1L)
})

testthat::test_that("lavaan multigroup graph retains panel-specific nodes and edges", {
  testthat::skip_if_not_installed("lavaan")
  fit <- make_lavaan_fit(TRUE)
  testthat::expect_true(lavaan::lavInspect(fit, "converged"))
  g <- prepare_graph(fit)
  check_prepared_graph(g)
  testthat::expect_true("group" %in% names(g$nodes))
  testthat::expect_equal(length(unique(g$nodes$group)), 2L)
  testthat::expect_true(anyDuplicated(g$nodes$name) > 0L)
  testthat::expect_true("group" %in% names(g$edges))
  testthat::expect_equal(length(unique(g$edges$group)), 2L)
})

testthat::test_that("direct panel preparation supports repeated names and local coordinates", {
  layout <- data.frame(
    name = rep(c("focc.within", "galo.within",
                 "focc.between", "galo.between"), 2),
    group = rep(c("foo", "bar"), each = 4),
    level = rep(c("within", "within", "between", "between"), 2),
    x = rep(c(0, 2, 0, 2), 2),
    y = 0
  )
  class(layout) <- c("tidy_layout", "data.frame")
  nodes <- layout[c("name", "group", "level")]
  nodes$shape <- "rect"
  edges <- data.frame(
    from = rep(c("focc.within", "focc.between"), 2),
    to = rep(c("galo.within", "galo.between"), 2),
    group = rep(c("foo", "bar"), each = 2),
    level = rep(c("within", "between"), 2),
    arrow = "last",
    curvature = NA_real_
  )
  g <- tidySEM:::prepare_graph.default(nodes = nodes, edges = edges, layout = layout)
  check_prepared_graph(g, expected_panels = 4L)
  testthat::expect_equal(nrow(g$nodes), 8L)
  testthat::expect_equal(nrow(g$edges), 4L)
  # Panel-local coordinates should coincide across groups.
  a <- g$nodes[g$nodes$group == "foo", c("name", "level", "x", "y")]
  b <- g$nodes[g$nodes$group == "bar", c("name", "level", "x", "y")]
  rownames(a) <- NULL
  rownames(b) <- NULL
  testthat::expect_equal(a, b, ignore_attr = TRUE)
})

testthat::test_that("layout may deliberately omit nodes and their edges", {
  layout <- data.frame(name = c("a", "b"), x = c(0, 2), y = c(0, 0))
  nodes <- data.frame(name = c("a", "b", "hidden"))
  edges <- data.frame(from = c("a", "a"), to = c("b", "hidden"))
  g <- tidySEM:::prepare_graph.default(nodes = nodes, edges = edges, layout = layout)
  check_prepared_graph(g)
  testthat::expect_setequal(g$nodes$name, c("a", "b"))
  testthat::expect_equal(nrow(g$edges), 1L)
})

testthat::test_that("individual node dimensions override global defaults", {
  layout <- data.frame(name = c("a", "b"), x = c(0, 2), y = 0)
  nodes <- data.frame(name = c("a", "b"), shape = c("rect", "oval"),
                      width = c(2.5, NA_real_), height = c(1.3, NA_real_))
  g <- tidySEM:::prepare_graph.default(nodes = nodes, layout = layout,
                             rect_width = 1.2, rect_height = 0.8,
                             ellipses_width = 1.4, ellipses_height = 1.1)
  check_prepared_graph(g)
  testthat::expect_equal(g$nodes$width, c(2.5, 1.4))
  testthat::expect_equal(g$nodes$height, c(1.3, 1.1))
})

# OpenMx integration: build RAM models directly to avoid depending on
# lavaan-to-OpenMx conversion utilities or an undocumented mx_mixture API.
make_openmx_ram <- function(data, name = "ram") {
  OpenMx::mxModel(
    name, type = "RAM", manifestVars = c("x1", "x2", "x3"),
    latentVars = "F",
    OpenMx::mxPath(from = "F", to = c("x1", "x2", "x3"),
                   arrows = 1, free = c(FALSE, TRUE, TRUE),
                   values = c(1, 0.7, 0.7)),
    OpenMx::mxPath(from = "F", arrows = 2, free = TRUE, values = 1),
    OpenMx::mxPath(from = c("x1", "x2", "x3"), arrows = 2,
                   free = TRUE, values = 0.5),
    OpenMx::mxPath(from = "one", to = c("x1", "x2", "x3"),
                   arrows = 1, free = TRUE, values = 0),
    OpenMx::mxData(data, type = "raw")
  )
}

testthat::test_that("OpenMx single-group RAM graph prepares successfully", {
  testthat::skip_if_not_installed("OpenMx")
  set.seed(101)
  dat <- data.frame(x1 = rnorm(100), x2 = rnorm(100), x3 = rnorm(100))
  fit <- OpenMx::mxRun(make_openmx_ram(dat), silent = TRUE)
  g <- prepare_graph(fit)
  check_prepared_graph(g)
})

testthat::test_that("OpenMx multigroup model graph prepares successfully", {
  testthat::skip_if_not_installed("OpenMx")
  set.seed(102)
  dat1 <- data.frame(x1 = rnorm(100), x2 = rnorm(100), x3 = rnorm(100))
  dat2 <- data.frame(x1 = rnorm(100), x2 = rnorm(100), x3 = rnorm(100))
  # A joint multigroup model with separately named RAM submodels.
  # tidySEM's get_nodes/get_edges/get_layout methods must recognize
  # these submodels as groups for this integration test to pass.
  m1 <- make_openmx_ram(dat1, "foo")
  m2 <- make_openmx_ram(dat2, "bar")
  fit <- OpenMx::mxModel(
    "multigroup", m1, m2,
    OpenMx::mxFitFunctionMultigroup(c("foo.fitfunction", "bar.fitfunction"))
  )
  fit <- OpenMx::mxRun(fit, silent = TRUE)
  g <- prepare_graph(fit)
  check_prepared_graph(g)
  testthat::expect_true("group" %in% names(g$nodes))
  testthat::expect_equal(length(unique(g$nodes$group)), 2L)
  testthat::expect_true(anyDuplicated(g$nodes$name) > 0L)
})

test_that("lavaan node labels support NSE", {
  skip_if_not_installed("lavaan")
skip()
  fit <- lavaan::cfa(
    "visual =~ x1 + x2 + x3",
    data = HolzingerSwineford1939
  )

  expect_no_error(
    g <- prepare_graph(
      fit,
      label = paste2(name, est_sig, sep = "\n")
    )
  )

  expect_s3_class(g, "sem_graph")
  expect_true(any(grepl("\n", g$nodes$label, fixed = TRUE)))
})

test_that("lavaan edge labels support NSE", {
  skip_if_not_installed("lavaan")

  fit <- lavaan::cfa(
    "visual =~ x1 + x2 + x3",
    data = HolzingerSwineford1939
  )

  expect_no_error(
    g <- prepare_graph(
      fit,
      label = paste2(lhs, est_sig, sep = "\n")
    )
  )

  expect_s3_class(g, "sem_graph")
  expect_true(any(grepl("\n", g$edges$label, fixed = TRUE)))
})
