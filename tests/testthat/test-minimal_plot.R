nod <- data.frame(name = "x",
                  shape = "rect")

test_that("possible to plot only a node", {
  expect_s3_class(graph_sem(nodes = nod, layout = get_layout("x", rows = 1)), "ggplot")
})

library(lavaan)

fit <- sem("Sepal.Length ~~ Sepal.Width", data = iris)
if(FALSE){
# Dit gaat mis in .connect: bij één column of één row.
graph_sem(fit)

test_that("can adjust curvature", {
  expect_s3_class(graph_sem(nodes = nod, layout = get_layout("x", rows = 1)), "ggplot")
})

}

