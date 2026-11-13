# Get fit indices from objects

Get fit indices from objects for which a method exists.

## Usage

``` r
get_fit(x, ...)
```

## Arguments

- x:

  An object for which a method exists.

- ...:

  further arguments to be passed to or from other methods.

## Value

A data.frame.

## Examples

``` r
if (FALSE) { # \dontrun{
df <- iris[, 1, drop = FALSE]
names(df) <- "x"
res <- mx_mixture(model = "x ~ m{C}*1
                           x ~~ v{C}*x", classes = 1, data = df)
table_fit(res)
} # }
```
