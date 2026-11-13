# Add significance asterisks to object

Takes an object, and adds significance asterisks.

## Usage

``` r
est_sig(x, digits = 2, sig = NULL)
```

## Arguments

- x:

  An object for which a method exists. This will be treated as numeric
  by the default method.

- digits:

  Integer. The number of digits to round the estimate column to.

- sig:

  Optional, a vector of p-values for the default method.

## Value

A character vector of formatted estimates.

## See also

table_results

Other Reporting tools:
[`conf_int()`](https://cjvanlissa.github.io/tidySEM/reference/conf_int.md),
[`table_fit()`](https://cjvanlissa.github.io/tidySEM/reference/table_fit.md),
[`table_prob()`](https://cjvanlissa.github.io/tidySEM/reference/table_prob.md),
[`table_results()`](https://cjvanlissa.github.io/tidySEM/reference/table_results.md)

## Author

Caspar J. van Lissa

## Examples

``` r
est_sig(c(.222, .3333), sig = c(.054, .045))
#> [1] "0.22"  "0.33*"
```
