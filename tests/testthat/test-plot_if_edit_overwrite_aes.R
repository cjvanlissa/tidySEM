fit <- sem("mpg ~ cyl\nmpg ~ am", data = mtcars, meanstructure = TRUE)

g <- prepare_graph(fit)
edges(g) %>%
  color_pos("green") %>%
  color_neg("red") ->
  p

test_that("if_edit functions not overwrite aesthetic column", {
  expect_true(all(p$color == c("red", "green", "green", "green", "red", "green")))
})


g <- prepare_graph(fit)
g$edges$bla <- rep(0, nrow(g$edges))
g$edges$bla[1] <- 1
g$edges$linetype = 1

test_that("if_edit works when column exists in either edges or nodes", {
  expect_error({
    p <- if_edit(g, {bla == 1}, {linetype = 2})
    plot(p)
  }, NA)
})
