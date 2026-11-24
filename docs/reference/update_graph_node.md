# Update an Individual SEM Graph Node

`update_graph_node()` provides a convenient way to modify a single node
without manually editing the graph's node data frame.

The function updates the selected node and recalculates its bounding box
while preserving the remaining graph structure.

## Usage

``` r
update_graph_node(x, name, ..., group = NULL, level = NULL)
```

## Arguments

- x:

  An object of class `sem_graph`.

- name:

  Character string identifying the node to update.

- ...:

  Named node properties to modify. These may include `x`, `y`, `width`,
  `height`, `shape`, `label`, or supported plotting aesthetics such as
  `colour`, `fill`, `alpha`, `linetype`, and `linewidth`.

- group:

  Optional group identifier, used to distinguish nodes with the same
  name in grouped graphs.

- level:

  Optional level identifier, used to distinguish nodes with the same
  name across levels.

## Value

An updated object of class `sem_graph`.

## Details

Modify the position, dimensions, label, shape, or aesthetics of an
individual node in an existing SEM graph.

Node positions are specified in the graph's existing coordinate system
and are not rescaled.

Individual `width` and `height` values override the global dimensions
specified when the graph was constructed.

When node dimensions or shape are modified, the node's bounding box is
updated automatically.

If multiple nodes share the same name, `group` or `level` can be used to
identify the intended node.

Edge endpoints, connection sides, curvature, and variance positions are
not automatically recalculated.

## See also

[`update_graph()`](https://cjvanlissa.github.io/tidySEM/reference/update_graph.md),
[`prepare_graph()`](https://cjvanlissa.github.io/tidySEM/reference/prepare_graph.md)

## Examples

``` r
if (FALSE) { # \dontrun{
g <- prepare_graph(
  edges = edges,
  nodes = nodes,
  layout = layout
)

# Move a node
g <- update_graph_node(
  g,
  name = "visual",
  x = 5,
  y = 4
)

# Change its dimensions and appearance
g <- update_graph_node(
  g,
  name = "visual",
  width = 1.8,
  height = 1.2,
  fill = "lightblue",
  colour = "black"
)

plot(g)
} # }
```
