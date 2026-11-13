# Edit graph elements

Evaluate an R expression within the environment of the elements of a
`sem_graph` object, and return the modified `sem_graph`.

## Usage

``` r
edit_graph(x, expr, element = c("edges", "nodes"), ...)

edit_nodes(x, expr, ...)

edit_edges(x, expr, ...)
```

## Arguments

- x:

  An object of class `sem_graph`.

- expr:

  expression to evaluate.

- element:

  Character. The element of the `sem_graph` to edit, defaults to
  `c("edges", "nodes")`.

- ...:

  Arguments passed on to [`within`](https://rdrr.io/r/base/with.html).

## Value

An object of class `sem_graph`.

## Examples

``` r
p <- prepare_graph(layout = get_layout("x", rows = 1))
p <- edit_graph(p, {colour = "blue"}, element = "nodes")
plot(p)
```
