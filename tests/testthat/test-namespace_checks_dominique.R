detach("package:tidySEM", unload = TRUE)
m <- lavaan::sem('
     ind60 =~ x1 + x2 + x3
     dem60 =~ y1 + a*y2 + b*y3
', data = lavaan::PoliticalDemocracy)

test_that("graph_sem works when tidySEM is not attached", {
  expect_error({tidySEM::graph_sem(m, edges = tidySEM::get_edges(m))}, NA)
})

library(tidySEM)
library(ggraph)
test_that("graph_sem works when ggraph masks get_edges etc", {
  expect_error({tidySEM::graph_sem(m)}, NA)
})
detach("package:ggraph", unload = TRUE)
