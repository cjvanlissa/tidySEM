library(lavaan)
res <- sem("dist ~ speed", cars, meanstructure =T)


# graph_sem(res)
# lay <- get_layout("dist", "speed", rows = 1)
# graph_sem(res,
#           layout = lay,           # layout
#           label = "est_std",  # get standardized results (not rounded)
#           angle = 170            # adjust the arrows
# )
#
# graph_sem(res,
#           layout = lay,           # layout
#           nodes = get_nodes(res, label = "name"),
#           edges = get_edges(res, label = "est_std"),
#           angle = 170            # adjust the arrows
# )

test_that("prepare_graph handles label argument", {
  tmp <- prepare_graph(res, label = "est_std")
  expect_true(all(!is.na(as.numeric(tmp$edges$label))))
  expect_true(all(!is.na(as.numeric(tmp$nodes$label))))
})

test_that("prepare_graph handles expression in label argument", {
  tmp <- prepare_graph(res, label = paste2(lhs, est_sig, sep = "\n"))
  expect_true(any(startsWith(tmp$edges$label, "dist")))
  expect_true(any(startsWith(tmp$nodes$label, "dist")))
})

test_that("graph_sem handles label argument", {
  tmp <- graph_sem(res, label = "est_std")
  expect_true(all(!is.na(as.numeric(tmp$layers[[3]]$data$label))))
  expect_true(all(!is.na(as.numeric(tmp$layers[[5]]$data$label))))
})

test_that("graph_sem handles expression in label argument", {
  tmp <- graph_sem(res, label = paste2(lhs, est_sig, sep = "\n"))
  expect_true(any(startsWith(tmp$layers[[3]]$data$label, "dist")))
  expect_true(any(startsWith(tmp$layers[[5]]$data$label, "dist")))
})
