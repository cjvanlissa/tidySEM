library(ggplot2)
test_that("color argument works for graph_sem", {
  the_test <- "plot_color"
  old_wd <- getwd()
  test_dir <- file.path(tempdir(), the_test)
  dir.create(test_dir)
  setwd(test_dir)
  on.exit({unlink(test_dir, recursive = TRUE); setwd(old_wd)}, add = TRUE)

  p <- prepare_graph(layout = get_layout("x", rows = 1))
  p1 <- edit_graph(p, {color = "blue"}, element = "nodes")
  p2 <- edit_graph(p, {colour = "blue"}, element = "nodes")
  ggsave("p1.png", plot(p1), device = "png")
  ggsave("p2.png", plot(p2), device = "png")

  expect_equivalent(tools::md5sum("p1.png"), tools::md5sum("p2.png"))

})
