# Convert tidy_sem to 'Mplus' syntax

Final stage in the 'tidySEM' workflow for syntax generation: Convert the
`tidy_sem` object to 'Mplus' syntax.

## Usage

``` r
as_mplus(x, ...)
```

## Arguments

- x:

  An object of class `tidy_sem`.

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
as_mplus(mod)
#> [1] "x ON y;"
```
