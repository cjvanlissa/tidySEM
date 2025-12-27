# Results table in probability scale

Returns thresholds for ordinal dependent variables in probability scale.

## Usage

``` r
table_prob(x, ...)
```

## Arguments

- x:

  An object for which a method exists.

- ...:

  Arguments passed to other functions.

## Value

A data.frame with results in probability scale.

## See also

Other Reporting tools:
[`conf_int()`](https://cjvanlissa.github.io/tidySEM/reference/conf_int.md),
[`est_sig()`](https://cjvanlissa.github.io/tidySEM/reference/est_sig.md),
[`table_fit()`](https://cjvanlissa.github.io/tidySEM/reference/table_fit.md),
[`table_results()`](https://cjvanlissa.github.io/tidySEM/reference/table_results.md)

## Examples

``` r
if (FALSE) { # \dontrun{
df <- data_mix_ordinal
df[1:4] <- lapply(df, ordered)
mx_lca(data = df,
       classes = 2) -> res
} # }
```
