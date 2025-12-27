# Calculate skew and kurtosis

Calculate skew and kurtosis, standard errors for both, and the estimates
divided by two times the standard error. If this latter quantity exceeds
an absolute value of 1, the skew/kurtosis is significant. With very
large sample sizes, significant skew/kurtosis is common.

## Usage

``` r
skew_kurtosis(x, verbose = FALSE, se = FALSE, ...)
```

## Arguments

- x:

  An object for which a method exists.

- verbose:

  Logical. Whether or not to print messages to the console, Default:
  FALSE

- se:

  Whether or not to return the standard errors, Default: FALSE

- ...:

  Additional arguments to pass to and from functions.

## Value

A `matrix` of skew and kurtosis statistics for `x`.

## Examples

``` r
skew_kurtosis(datasets::anscombe)
#>           skew    skew_2se     kurt  kurt_2se
#> x1  0.00000000  0.00000000 1.780000 0.6956300
#> x2  0.00000000  0.00000000 1.780000 0.6956300
#> x3  0.00000000  0.00000000 1.780000 0.6956300
#> x4  2.84604989  2.15385490 9.100000 3.5563107
#> y1 -0.05580807 -0.04223484 2.179061 0.8515845
#> y2 -1.12910800 -0.85449479 3.007674 1.1754091
#> y3  1.59223074  1.20498027 5.130453 2.0049984
#> y4  1.29302529  0.97854534 4.390789 1.7159351
```
