test_that("color argument works for graph_sem", {
  p <- prepare_graph(layout = get_layout("x", rows = 1))
  p <- edit_graph(p, {color = "blue"}, element = "nodes")
  p <- plot(p)
  expect_known_hash(p, hash = "a83fd8931a")

})
