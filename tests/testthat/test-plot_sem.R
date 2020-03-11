lo <- get_layout("ne", "phys", "",
                     "plea", "", "",
                     "dist", "", "dep",
                     "saf", "", "",
                     "coh", "stress", "", rows = 5)

test_that("get_layout generates matrix", {
  expect_true(class(lo) == "matrix")
})

long_lo <- tidySEM:::long_layout(lo)
df_nodes <-  data.frame(node_id = 1:length(long_lo$name), name = long_lo$name, stringsAsFactors = FALSE)
df_nodes$shape <- "oval"
df_nodes$shape[grepl("(phys)", df_nodes$name)] <- "rect"
labels <- list("ne" = "Natural environment",
               "plea" = "Pleasantness",
               "dist" = "Disturbance",
               "saf"  = "Safety",
               "coh" = "Cohesion",
               "phys" = "Physical activity",
               "stress" = "Stress",
               "dep" ="Depression"
)
df_nodes$label <- unlist(labels[match(df_nodes$name, names(labels))])
#df_nodes$name <- df_nodes$node_id
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

df_edges$from <- df_nodes$name[as.numeric(df_edges$from)]
df_edges$to <- df_nodes$name[as.numeric(df_edges$to)]
df_edges$curvature <- c(rep(NA, 12))

prep <- prepare_graph(nodes = df_nodes, layout = lo, edges = df_edges)

test_that("prepare_graph correctly generates graph data", {
  expect_s3_class(prep, "sem_graph")
})


prep$edges$connect_from <- "right"
prep$edges$connect_to[c(5,6)] <- c("bottom", "top")
prep$edges$connect_to <- c(rep("left", 10), "top", "bottom")
prep$edges$label_location <- .2
p <- plot(prep)

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

