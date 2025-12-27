# Convert tidy_sem to 'lavaan' syntax

Final stage in the 'tidySEM' workflow for syntax generation: Convert the
`tidy_sem` object to `lavaan` syntax in tabular format (see
[`model.syntax`](https://rdrr.io/pkg/lavaan/man/model.syntax.html)).

## Usage

``` r
as_lavaan(x, ...)
```

## Arguments

- x:

  An object of class `tidy_sem`

- ...:

  Additional parameters to be passed to and from functions.

## Value

Character vector.

## Examples

``` r
mod <- list(syntax = structure(list(lhs = "x", op = "~", rhs = "y",
                                    free = TRUE, value = "", label = "",
                                    category = "", aspect = ""),
            class = "data.frame", row.names = c(NA, -1L)))
class(mod) <- "tidy_sem"
as_lavaan(mod)
#>   lhs op rhs free value label category aspect
#> 1   x  ~   y TRUE                            
```
