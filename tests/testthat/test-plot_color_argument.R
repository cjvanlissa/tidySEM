library(ggplot2)
test_that("color argument works for graph_sem", {
  tempf1 <- tempfile(fileext = ".png")
  tempf2 <- tempfile(fileext = ".png")
  p <- prepare_graph(layout = get_layout("x", rows = 1))
  p1 <- edit_graph(p, {color = "blue"}, element = "nodes")
  p2 <- edit_graph(p, {colour = "blue"}, element = "nodes")
  ggsave(tempf1, plot(p1), device = "png")
  ggsave(tempf2, plot(p2), device = "png")
  on.exit({file.remove(tempf1); file.remove(tempf2)})
  expect_equivalent(tools::md5sum(tempf1), tools::md5sum(tempf2))

})
