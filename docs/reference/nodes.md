# Extract nodes from sem_graph

Provides access to the `nodes` element of a `sem_graph` object. This can
be used to return or assign to the `nodes` element.

## Usage

``` r
nodes(x)

nodes(x) <- value
```

## Arguments

- x:

  Object of class sem_graph.

- value:

  A valid value for `nodes(x)`.

## Value

data.frame

## Examples

``` r
edg <- data.frame(from = "x", to = "y")
p <- prepare_graph(edges = edg, layout = get_layout("x", "y", rows = 1))
nodes(p)
#>   name shape label x y node_xmin node_xmax node_ymin node_ymax show
#> 1    x  rect     x 2 2       1.4       2.6       1.6       2.4 TRUE
#> 2    y  rect     y 4 2       3.4       4.6       1.6       2.4 TRUE
```
