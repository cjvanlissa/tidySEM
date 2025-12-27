# Prepare graph data

Prepare an object of class `sem_graph`, containing data objects that can
be rendered into a SEM graph. Using this function allows users to
manually change the default graph specification before plotting it.
Input consists of (at least) a layout, and either nodes and edges, or a
model object.

## Usage

``` r
# S3 method for class 'dagitty'
prepare_graph(model, rect_height = 0.5, rect_width = 0.5, ...)

prepare_graph(...)

# Default S3 method
prepare_graph(
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
  fix_coord = FALSE,
  ...
)

# S3 method for class 'lavaan'
prepare_graph(model, edges = NULL, layout = NULL, nodes = NULL, ...)

# S3 method for class 'MxModel'
prepare_graph(model, ...)

# S3 method for class 'character'
prepare_graph(...)

# S3 method for class 'mplus.model'
prepare_graph(model, edges = NULL, layout = NULL, nodes = NULL, ...)

# S3 method for class 'mplusObject'
prepare_graph(model, edges = NULL, layout = NULL, nodes = NULL, ...)
```

## Arguments

- model:

  Instead of the edges argument, it is also possible to use the model
  argument and pass an object for which a method exists (e.g.,
  `mplus.model` or `lavaan`).

- rect_height:

  Height of rectangles (used to display observed variables), Default:
  0.8

- rect_width:

  Width of rectangles (used to display observed variables), Default: 1.2

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

- ellipses_width:

  Width of ellipses (used to display latent variables), Default: 1

- ellipses_height:

  Height of ellipses (used to display latent variables), Default: 1

- variance_diameter:

  Diameter of variance circles, Default: .8

- spacing_x:

  Spacing between columns of the graph, Default: 1

- spacing_y:

  Spacing between rows of the graph, Default: 1

- text_size:

  Point size of text, Default: 4

- curvature:

  Curvature of curved edges. The curve is a circle segment originating
  in a point that forms a triangle with the two connected points, with
  angles at the two connected points equal to `curvature`. To flip a
  curved edge, use a negative value for curvature. Default: 60

- angle:

  Angle used to connect nodes by the top and bottom. Defaults to NULL,
  which means Euclidean distance is used to determine the shortest
  distance between node sides. A numeric value between 0-180 can be
  provided, where 0 means that only nodes with the same x-coordinates
  are connected top-to-bottom, and 180 means that all nodes are
  connected top-to-bottom.

- fix_coord:

  Whether or not to fix the aspect ratio of the graph. Does not work
  with multi-group or multilevel models. Default: FALSE.

## Value

Object of class 'sem_graph'

## Examples

``` r
library(lavaan)
res <- sem("dist ~ speed", cars)
prepare_graph(res)
#> A tidy_sem object with 3 edges and 2 nodes, and the following elements:
#> v  edges             v  nodes
#> v  rect_width        v  rect_height
#> v  ellipses_width    v  ellipses_height
#> v  variance_diameter v  spacing_x
#> v  spacing_y         v  text_size
#> v  curvature         o  angle
#> v  fix_coord         
```
