Galo <- read.csv(system.file("testdata", "galo.csv", package="tidySEM"))

L.in.G <- ' group: foo
  level: within
    galo ~ focc
  level: between
    galo ~ focc

group: bar
  level: within
    galo ~ focc
  level: between
    galo ~ focc
'

fit <- suppressWarnings(sem(L.in.G, data = Galo, cluster = "school", fixed.x = FALSE,
                  missing = "fiml", std.lv = TRUE, h1 = TRUE, group = "g"))


nod <- get_nodes(fit)
edg <- get_edges(fit)

layout <- matrix(c("galo.within", "focc.within",
                   "galo.between", "focc.between"), ncol = 2, byrow = TRUE)

test_that("Nodes correct nr of rows", {
  expect_true(nrow(nod) == 2*length(as.vector(layout)[!as.vector(layout) == ""]))
})

plevgroup <- prepare_graph(edges = edg, nodes = nod, layout = layout)
p <- plot(plevgroup)

test_that("multilevmultigroup works", {
  expect_s3_class(p, "ggplot")
})



