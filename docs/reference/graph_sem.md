# Render a graph

Render a graph based on a layout, and either nodes and edges, or a model
object.

## Usage

``` r
# S3 method for class 'dagitty'
graph_sem(model, ...)

graph_sem(...)

# Default S3 method
graph_sem(
  edges = NULL,
  layout = NULL,
  nodes = NULL,
  rect_width = 1.2,
  rect_height = 0.8,
  ellipses_width = 1,
  ellipses_height = 1,
  variance_diameter = 0.8,
  spacing_x = 2,
  spacing_y = 2,
  text_size = 4,
  curvature = 60,
  angle = NULL,
  fix_coord = TRUE,
  ...
)

# S3 method for class 'lavaan'
graph_sem(model, edges = NULL, layout = NULL, nodes = NULL, ...)

# S3 method for class 'MxModel'
graph_sem(model, edges = NULL, layout = NULL, nodes = NULL, ...)

# S3 method for class 'mplus.model'
graph_sem(model, edges = NULL, layout = NULL, nodes = NULL, ...)

# S3 method for class 'igraph'
graph_sem(model, edges = NULL, layout = NULL, nodes = NULL, ...)

# S3 method for class 'character'
graph_sem(...)

# S3 method for class 'mplusObject'
graph_sem(model, edges = NULL, layout = NULL, nodes = NULL, ...)
```

## Arguments

- model:

  Instead of the edges argument, it is also possible to use the model
  argument and pass an object for which a method exists (e.g.,
  `mplus.model` or `lavaan`).

- ...:

  Additional arguments passed to and from functions.

- edges:

  Object of class 'tidy_edges', or a `data.frame` with (at least) the
  columns `c("from", "to")`, and optionally,
  `c("arrow", "label", "connect_from", "connect_to", "curvature")`.

- layout:

  A matrix (or data.frame) that describes the layout; see
  [`get_layout`](https://cjvanlissa.github.io/tidySEM/reference/get_layout.md).

- nodes:

  Optional, object of class 'tidy_nodes', created with the
  [`get_nodes`](https://cjvanlissa.github.io/tidySEM/reference/get_nodes.md)
  function, or a `data.frame` with (at least) the column `c("name")`,
  and optionally, `c("shape", "label")`. If set to `NULL` (the default),
  nodes are inferred from the `layout` and `edges` arguments.

- rect_width:

  Width of rectangles (used to display observed variables), Default: 1.2

- rect_height:

  Height of rectangles (used to display observed variables), Default:
  0.8

- ellipses_width:

  Width of ellipses (used to display latent variables), Default: 1

- ellipses_height:

  Height of ellipses (used to display latent variables), Default: 1

- variance_diameter:

  Diameter of variance circles, Default: .8

- spacing_x:

  Spacing between columns of the graph, Default: 2

- spacing_y:

  Spacing between rows of the graph, Default: 2

- text_size:

  Point size of text, Default: 4

- curvature:

  Deprecated. Was used to determine curvature of curved edges, which is
  now being optimized.

- angle:

  Deprecated. Was used to determine node connection points.

- fix_coord:

  Whether or not to fix the aspect ratio of the graph. Does not work
  with multi-group or multilevel models. Default: TRUE.

## Value

Object of class 'sem_graph'

## Details

The default interface simply Runs the functions
[`prepare_graph`](https://cjvanlissa.github.io/tidySEM/reference/prepare_graph.md)
and `plot`. The alternative interface first runs
[`get_nodes`](https://cjvanlissa.github.io/tidySEM/reference/get_nodes.md)
and
[`get_edges`](https://cjvanlissa.github.io/tidySEM/reference/get_edges.md)
on the `model` argument.

## Examples

``` r
library(lavaan)
res <- sem("dist ~ speed", cars)
graph_sem(res)
```
