library(igraph)
library(tidySEM)
g <- make_ring(10)
edge_attr(g) <- list(
  name = LETTERS[1:10],
  color = rep("green", gsize(g))
)

test_that("igraph edges correct", {
  edg <- get_edges(g)
  expect_true(all(c("from", "to", "name", "color", "arrow") %in% names(edg)) )
  expect_true(all(edg$arrow == "none"))
  edge_attr(g, "label") <- paste0("lab", E(g)$name)
  edg <- get_edges(g)
  expect_true(all(startsWith(edg$label, "lab")))
})

test_that("igraph nodes correct", {
  nod <- get_nodes(g)
  expect_true(all(c("name", "shape", "label") %in% names(nod)) )
  expect_true(all(nod$shape == "none"))
})

test_that("igraph layout correct", {
  lo <- get_layout(g)
  expect_true(all(c("x", "y", "name") %in% names(lo)) )
})

g <- graph_from_data_frame(data.frame(one=1:2,two=2:3))
g <- set_vertex_attr(g,'id',1,'first_id')

test_that("igraph edges correct", {
  edg <- get_edges(g)
  expect_true(all(c("from", "to", "arrow") %in% names(edg)) )
  expect_true(all(edg$arrow == "last"))
})

test_that("igraph nodes correct", {
  nod <- get_nodes(g)
  expect_true(all(c("name", "id", "shape", "label") %in% names(nod)) )
})

test_that("igraph layout correct", {
  lo <- get_layout(g)
  expect_true(all(c("x", "y", "name") %in% names(lo)) )
})


names <- c('A','B','C,','D')
from <- c(113,115,112,114,113)
to <- c(112,112,115,113,114)
structure <- data.frame("from" = from, "to" = to)
g <- igraph::graph_from_data_frame(structure)
#V(g)$label <- names
V(g)$nonsense <- letters[seq_along(names)]

test_that("igraph edges correct", {
  edg <- get_edges(g)
  expect_true(all(c("from", "to", "arrow") %in% names(edg)) )
  expect_true(all(edg$arrow == "last"))
  g_un <- as_undirected(g)
  edg_un <- get_edges(g_un)
  expect_true(all(edg_un$arrow == "none"))
})

test_that("igraph nodes correct", {
  nod <- get_nodes(g)
  expect_true(all(c("name", "nonsense", "shape", "label") %in% names(nod)) )
})

test_that("igraph layout correct", {
  lo <- get_layout(g)
  expect_true(all(c("x", "y", "name") %in% names(lo)) )
})
