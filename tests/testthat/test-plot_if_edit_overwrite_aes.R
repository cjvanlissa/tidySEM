fit <- sem("mpg ~ cyl\nmpg ~ am", data = mtcars, meanstructure = TRUE)

g <- prepare_graph(fit)
edges(g) %>%
  color_pos("green") %>%
  color_neg("red") ->
  p

test_that("if_edit functions not overwrite aesthetic column", {
  expect_true(all(p$color == c("red", "green", "green", "green", "red", "green")))
})
