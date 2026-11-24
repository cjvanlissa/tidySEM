# Update an Existing SEM Graph

`update_graph()` modifies an existing `sem_graph` object while
preserving its manually specified geometry and aesthetics.

Unlike
[`prepare_graph()`](https://cjvanlissa.github.io/tidySEM/reference/prepare_graph.md),
which constructs a graph from a layout, `update_graph()` operates on
coordinates that have already been scaled and prepared for plotting.

Node bounding boxes are recalculated using the current node coordinates
and dimensions. Individual node dimensions take precedence over the
graph's global shape dimensions.

## Usage

``` r
update_graph(x, ...)

# S3 method for class 'sem_graph'
update_graph(x, nodes = NULL, edges = NULL, ...)
```

## Arguments

- x:

  An object of class `sem_graph`.

- ...:

  Additional graph-level plotting settings to update, such as
  `rect_width`, `rect_height`, `ellipses_width`, `ellipses_height`,
  `variance_diameter`, `text_size`, `spacing_x`, `spacing_y`, or
  `fix_coord`.

- nodes:

  Optional data frame containing replacement node information. If
  `NULL`, the existing nodes are retained.

- edges:

  Optional data frame containing replacement edge information. If
  `NULL`, the existing edges are retained.

## Value

An updated object of class `sem_graph`.

## Details

Modify the nodes, edges, or plotting settings of an existing structural
equation model (SEM) graph without recalculating its layout or
automatically repositioning its edges.

The function preserves existing node positions and does not apply layout
spacing a second time.

Node bounding boxes (`node_xmin`, `node_xmax`, `node_ymin`, and
`node_ymax`) are recalculated from node centers and dimensions.

If a node has individual `width` or `height` values, these override the
corresponding global shape dimensions.

The following properties are preserved unless explicitly modified:

- Node coordinates (`x`, `y`).

- Edge connection sides (`connect_from`, `connect_to`).

- Edge curvature.

- Variance-loop positions.

- Node and edge labels.

- Node and edge aesthetics.

For self-variance edges with `arrow = "both"`, the source and target
connection sides are kept synchronized.

Changing `spacing_x` or `spacing_y` does not rescale existing node
coordinates.

## See also

[`prepare_graph()`](https://cjvanlissa.github.io/tidySEM/reference/prepare_graph.md),
[`update_graph_node()`](https://cjvanlissa.github.io/tidySEM/reference/update_graph_node.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Construct a graph
g <- prepare_graph(
  edges = edges,
  nodes = nodes,
  layout = layout
)

# Change node positions
nd <- g$nodes
nd$x[nd$name == "visual"] <- 5

g <- update_graph(g, nodes = nd)

# Change edge aesthetics
ed <- g$edges
ed$colour <- "black"
ed$colour[1] <- "steelblue"

g <- update_graph(g, edges = ed)

# Change global plotting settings
g <- update_graph(
  g,
  text_size = 5,
  variance_diameter = 1
)

plot(g)
} # }
```
