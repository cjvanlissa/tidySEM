library(lavaan)

test_that("all_fun and its derivatives work", {
  fit <- sem("mpg ~ cyl\nmpg ~ am", data = mtcars, meanstructure = TRUE)

  p <- prepare_graph(model = fit,
                     nodes = get_nodes(fit, columns = NULL),
                     edges = get_edges(fit, columns = NULL))

  tmp <- hide_sig(p)
  expect_equivalent(tmp$edges$show, c(FALSE, FALSE, FALSE, TRUE, TRUE, TRUE))
  expect_error(plot(tmp), NA)

  tmp <- hide_fixed(p)
  expect_equivalent(tmp$edges$show, c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE))
  expect_error(plot(tmp), NA)

  tmp <- hide_sig(p, element = c("nodes", "edges"))
  expect_equivalent(tmp$edges$show, c(FALSE, FALSE, FALSE, TRUE, TRUE, TRUE))
  expect_equivalent(tmp$nodes$show, c(TRUE, TRUE, FALSE))
  expect_message(plot(tmp))
  expect_error(plot(tmp), NA)

  tmp <- hide_fixed(p, element = c("nodes", "edges"))
  expect_equivalent(tmp$edges$show, c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE))
  expect_equivalent(tmp$nodes$show, c(FALSE, FALSE, TRUE))
  expect_message(plot(tmp))
  expect_error(plot(tmp), NA)

  tmp <- hide_sig(tmp, element = c("nodes", "edges"))
  expect_true(all(!tmp$edges$show))
  expect_true(all(!tmp$nodes$show))
  expect_error(plot(tmp))

  tmp <- show_sig(tmp, element = c("nodes", "edges"))
  expect_equivalent(tmp$edges$show, c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE))
  expect_equivalent(tmp$nodes$show, c(FALSE, FALSE, TRUE))
  expect_error(plot(tmp), NA)

  tmp <- all_fun(p, {show = FALSE},  {grepl("3", confint_std)}, element = c("edges", "nodes"))
  expect_equivalent(tmp$edges$show, c(TRUE, TRUE, FALSE, TRUE, TRUE, TRUE))
  expect_equivalent(tmp$nodes$show, c(FALSE, FALSE, TRUE))
  expect_error(plot(tmp), NA)

  # tmp <- p
  # tmp$edges$linetype <- c(1,1,1,2,2,2)
  # tmp<- linetype_sig(p, 2)
  # plot(tmp)
  #
  # plot(colour_fixed(p, "red"))
  # p$edges$pval[4] <- "0.5"
  # plot(linetype_non_sig(p, 3))
  # p$nodes$pval[3] <- "0.5"
  # tmp <- linetype_non_sig(p, 2, elements = c("nodes", "edges"))
  # tmp$edges
  #
  # plot(tmp)

})
