library(dagitty)
# Specify a simple DAG containing one path
g <- dagitty("dag{
a -> b ;
b -> c ;
d -> c
}")
nod1 <- tidySEM::get_nodes(g)
edg1 <- tidySEM::get_edges(g)
# Newlines and semicolons are optional
g <- dagitty("dag{
a -> b b -> c d -> c
}")
nod2 <- tidySEM::get_nodes(g)
edg2 <- tidySEM::get_edges(g)
test_that("Newlines and semicolons are optional", {
  expect_equal(nod1, nod2)
  expect_equal(edg1, edg2)
})
# Paths can be specified in one go; the semicolon below is
# optional
g <- dagitty("dag{
a -> b ->c ; d -> c
}")

nod3 <- tidySEM::get_nodes(g)
edg3 <- tidySEM::get_edges(g)
test_that("Paths can be specified in one go", {
  expect_equal(nod1, nod3)
  expect_equal(edg1, edg3)
})

# Edges can be written in reverse notation
g <- dagitty("dag{
a -> b -> c <- d
}")

nod4 <- tidySEM::get_nodes(g)
edg4 <- tidySEM::get_edges(g)
test_that("Edges can be written in reverse notation", {
  expect_equal(nod1, nod4)
  expect_equal(edg1, edg4)
})
# Spaces are optional as well
g <- dagitty("dag{a->b->c<-d}")
nod5 <- tidySEM::get_nodes(g)
edg5 <- tidySEM::get_edges(g)
test_that("Spaces are optional as well", {
  expect_equal(nod1, nod5)
  expect_equal(edg1, edg5)
})
# Variable attributes can be set in square brackets
# Example: DAG with one exposure, one outcome, and one unobserved variable
g <- dagitty("dag{
x -> y ; x <- z -> y
x [exposure]
y [outcome]
z [unobserved]
}")

nod6 <- tidySEM::get_nodes(g)
edg6 <- tidySEM::get_edges(g)
test_that("Node attributes are detected", {
  expect_true(nod6$exposure[nod6$name =="x"])
  expect_true(nod6$outcome[nod6$name =="y"])
  expect_true(nod6$latent[nod6$name =="z"])
})

# The same graph as above
g <- dagitty("dag{x[e]y[o]z[u]x<-z->y<-x}")
nod7 <- tidySEM::get_nodes(g)
edg7 <- tidySEM::get_edges(g)
test_that("Abbreviated node attributes are detected", {
  expect_true(nod7$exposure[nod6$name =="x"])
  expect_true(nod7$outcome[nod6$name =="y"])
  expect_true(nod7$latent[nod6$name =="z"])
})


g <- dagitty('dag{
x -> y ; x <- z -> y
x [exposure,pos="0,1"]
y [outcome, pos = "1,1"]
z [unobserved , pos = "1 , 0"]
}')

nod8 <- tidySEM::get_nodes(g)
edg8 <- tidySEM::get_edges(g)
test_that("Node attributes are detected", {
  expect_true(nod6$exposure[nod6$name =="x"])
  expect_true(nod6$outcome[nod6$name =="y"])
  expect_true(nod6$latent[nod6$name =="z"])
})
