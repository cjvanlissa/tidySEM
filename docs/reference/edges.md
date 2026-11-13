# Extract edges from sem_graph

Provides access to the `edges` element of a `sem_graph` object. This can
be used to return or assign to the `edges` element.

## Usage

``` r
edges(x)

edges(x) <- value
```

## Arguments

- x:

  Object of class sem_graph.

- value:

  A valid value for `edges(x)`.

## Value

data.frame

## Examples

``` r
edg <- data.frame(from = "x", to = "y")
p <- prepare_graph(edges = edg, layout = get_layout("x", "y", rows = 1))
edges(p)
#>   from to label arrow curvature connect_from connect_to show
#> 1    x  y        last        NA        right       left TRUE
```
