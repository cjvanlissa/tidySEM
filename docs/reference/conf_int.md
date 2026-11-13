# Format confidence intervals

Creates 'APA'-formatted confidence intervals, either from an object for
which a method exists, or from the arguments `lb` and `ub`. When
argument `x` is a numeric vector, it is also possible to construct a
confidence interval using the standard error (`se`) and a percentile
interval (`ci`).

## Usage

``` r
conf_int(x, digits = 2, se = NULL, lb = NULL, ub = NULL, ci = 95)
```

## Arguments

- x:

  Optional. An object for which a method exists.

- digits:

  Integer. The number of digits to round the confidence boundaries to.

- se:

  Optional, numeric. Standard error of the parameters.

- lb:

  Optional, numeric. Lower boundary of confidence intervals.

- ub:

  Optional, numeric. Upper boundary of confidence intervals.

- ci:

  Optional, numeric. What percentage CI to use (only used when computing
  CI from a numeric vector `x`, and the standard error `se`, based on a
  normal distribution).

## Value

A character vector of formatted confidence intervals.

## See also

table_results est_sig

Other Reporting tools:
[`est_sig()`](https://cjvanlissa.github.io/tidySEM/reference/est_sig.md),
[`table_fit()`](https://cjvanlissa.github.io/tidySEM/reference/table_fit.md),
[`table_prob()`](https://cjvanlissa.github.io/tidySEM/reference/table_prob.md),
[`table_results()`](https://cjvanlissa.github.io/tidySEM/reference/table_results.md)

## Author

Caspar J. van Lissa

## Examples

``` r
conf_int(x = c(1.325, 2.432), se = c(.05336, .00325))
#> [1] "[1.22, 1.43]" "[2.43, 2.44]"
```
