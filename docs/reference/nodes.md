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
#>   name shape label x y node_xmin node_xmax node_ymin node_ymax width height
#> 1    x  rect     x 1 1       0.4       1.6       0.6       1.4   1.2    0.8
#> 2    y  rect     y 2 1       1.4       2.6       0.6       1.4   1.2    0.8
#>   show
#> 1 TRUE
#> 2 TRUE
```
