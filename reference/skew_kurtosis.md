# Calculate skew and kurtosis

Calculate skew and kurtosis, standard errors for both, and the estimates
divided by two times the standard error. If this latter quantity exceeds
an absolute value of 1, the skew/kurtosis is significant. With very
large sample sizes, significant skew/kurtosis is common.

## Usage

``` r
skew_kurtosis(
  x,
  verbose = FALSE,
  se = FALSE,
  pearson = FALSE,
  corrected = TRUE,
  ...
)
```

## Arguments

- x:

  An object for which a method exists.

- verbose:

  Logical. Whether or not to print messages to the console, Default:
  FALSE

- se:

  Logical. Whether or not to return the standard errors, Default: FALSE

- pearson:

  Logical. Whether or not to return the Pearson's kurtosis alongside
  excess kurtosis, Default: FALSE

- corrected:

  Logical. Whether or not to correct for bias in skew and kurtosis
  (Joanes & Gill, 1998). Corrects both the estimates and their standard
  errors. Default: TRUE

- ...:

  Additional arguments to pass to and from functions.

## Value

A `matrix` of skew and kurtosis statistics for `x`. The columns labeled
`_2se` contain the test statistic divided by two times its standard
error.

## References

Joanes, D. N. & Gill, C. A. (1998). Comparing measures of sample
skewness and kurtosis. *Journal Of The Royal Statistical Society: Series
D (The Statistician)*, *47*(1), 183–189.
[doi:10.1111/1467-9884.00122](https://doi.org/10.1111/1467-9884.00122)

## Examples

``` r
skew_kurtosis(datasets::anscombe)
#>           skew    skew_2se       kurt   kurt_2se
#> x1  0.00000000  0.00000000 -1.2000000 -0.4689640
#> x2  0.00000000  0.00000000 -1.2000000 -0.4689640
#> x3  0.00000000  0.00000000 -1.2000000 -0.4689640
#> x4  3.31662479  2.50998008 11.0000000  4.2988371
#> y1 -0.06503555 -0.04921809 -0.5348977 -0.2090398
#> y2 -1.31579829 -0.99577966  0.8461232  0.3306678
#> y3  1.85549520  1.40421552  4.3840886  1.7133166
#> y4  1.50681818  1.14034112  3.1513149  1.2315445
```
