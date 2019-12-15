library(lavaan)
df <- HolzingerSwineford1939
#df$x2 <- ordered(cut(df$x2, 4))

HS.model <- ' visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '
fit <- cfa(HS.model, data=df, meanstructure = T)

layout <- matrix(c("", "", "visual","","textual","","speed","", "",
                   paste0("x", 1:9)), nrow = 2, byrow = TRUE)

layout <- get_layout(layout)
edg <- get_edges(fit)
nod <- get_nodes(fit)

#nod$label[5] <- "bla"

p2 <- prepare_graph(nodes = nod, layout = layout, edges = edg, angle = 95)
#edges(p2)$curvature[10:12] <- edges(p2)$curvature[10:12] *-1
tmp <- plot(p2)

test_that("Plot works for lavaan cfa", {
  expect_s3_class(tmp, "ggplot")
})
