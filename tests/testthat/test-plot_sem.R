layout <- matrix(c("ne", "", "plea", "", "dist", "", "saf", "", "coh",
                      NA, NA, NA, NA, NA, NA, NA, NA, NA, "phys", "", "", "", "", "",
                      "", "", "stress", NA, NA, NA, NA, NA, NA, NA, NA, NA, "", "",
                      "", "", "dep", "", "", "", ""), nrow = 9)

layout <- get_layout(layout)

test_that("matrix correctly converts to tidy_layout", {
  expect_s3_class(layout, "tidy_layout")
})


df_nodes <-  data.frame(node_id = 1:length(layout$param), param = layout$param, stringsAsFactors = FALSE)
df_nodes$shape <- "oval"
df_nodes$shape[grepl("(phys)", df_nodes$param)] <- "rect"
labels <- list("ne" = "Natural environment",
               "plea" = "Pleasantness",
               "dist" = "Disturbance",
               "saf"  = "Safety",
               "coh" = "Cohesion",
               "phys" = "Physical activity",
               "stress" = "Stress",
               "dep" ="Depression"
)
df_nodes$label <- unlist(labels[match(df_nodes$param, names(labels))])
#df_nodes$param <- df_nodes$node_id
df_edges <- data.frame(matrix(c(
  1, 6, "last", "+",
  2, 6, "last", "+",
  3, 6, "last", "-",
  4, 6, "last", "+",
  5, 6, "last", "+",

  1, 7, "last", "-",
  2, 7, "last", "-",
  3, 7, "last", "+",
  4, 7, "last", "-",
  5, 7, "last", "-",
  6, 8, "last", "-",
  7, 8, "last", "+"), ncol = 4, byrow = TRUE), stringsAsFactors = FALSE)
names(df_edges) <- c("from", "to", "arrow", "label")
df_edges$connector <- c(rep("line", 12))

df_edges$from <- df_nodes$param[as.numeric(df_edges$from)]
df_edges$to <- df_nodes$param[as.numeric(df_edges$to)]
df_edges$curvature <- c(rep(NA, 12))

prep <- prepare_sem_graph(nodes = df_nodes, layout = layout, edges = df_edges)

test_that("prepare_sem_graph correctly generates graph data", {
  expect_s3_class(prep, "sem_graph")
})


prep$edges$connect_from <- "right"
prep$edges$connect_to[c(5,6)] <- c("bottom", "top")
prep$edges$connect_to <- c(rep("left", 10), "top", "bottom")
p <- plot(prep)

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
