# Apply POMS-coding to data

Takes in a data.frame, and applies POMS (proportion of of
maximum)-coding to the numeric columns.

## Usage

``` r
poms(data)
```

## Arguments

- data:

  A data.frame.

## Value

A data.frame.

## Author

Caspar J. van Lissa

## Examples

``` r
data <- data.frame(a = c(1, 2, 2, 4, 1, 6),
                   b = c(6, 6, 3, 5, 3, 4),
                   c = c("a", "b", "b", "t", "f", "g"))
poms(data)
#>     a         b c
#> 1 0.0 1.0000000 a
#> 2 0.2 1.0000000 b
#> 3 0.2 0.0000000 b
#> 4 0.6 0.6666667 t
#> 5 0.0 0.0000000 f
#> 6 1.0 0.3333333 g
```
