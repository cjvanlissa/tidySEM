# Extract correlation tables

Extracts a publication-ready covariance or correlation matrix from an
object for which a method exists.

## Usage

``` r
table_cors(x, value_column = "est_sig_std", digits = 2, ...)
```

## Arguments

- x:

  An object for which a method exists.

- value_column:

  Character. Name of the column to use to propagate the matrix. Defaults
  to "est_sig_std", the standardized estimate with significance
  asterisks.

- digits:

  Number of digits to round to when formatting values.

- ...:

  Additional arguments passed to and from methods.

## Value

A Matrix or a list of matrices (in case there are between/within
correlation matrices).

## Author

Caspar J. van Lissa

## Examples

``` r
library(lavaan)
HS.model <- '  visual =~ x1 + x2 + x3
               textual =~ x4 + x5 + x6
               speed   =~ x7 + x8 + x9 '
fit <- cfa(HS.model,
           data = HolzingerSwineford1939,
           group = "school")
table_cors(fit)
#> $Pasteur
#>         speed    textual   visual   
#> speed   "1.00"   "0.33**"  "0.30**" 
#> textual "0.33**" "1.00"    "0.48***"
#> visual  "0.30**" "0.48***" "1.00"   
#> 
#> $`Grant-White`
#>         speed     textual   visual   
#> speed   "1.00"    "0.34***" "0.52***"
#> textual "0.34***" "1.00"    "0.54***"
#> visual  "0.52***" "0.54***" "1.00"   
#> 
```
