# Create scale scores from observed variables

This function calculates mean or sum scores from a `data.frame` and a
named list describing the items in each scale. It returns the scores, a
scale descriptive table, and a scale correlation table. It relies on
several functions from the `psych` package.

## Usage

``` r
create_scales(
  x,
  keys.list,
  missing = TRUE,
  impute = "none",
  omega = NULL,
  digits = 2,
  ...
)

# S3 method for class 'tidy_sem'
create_scales(
  x,
  keys.list,
  missing = TRUE,
  impute = "none",
  omega = NULL,
  digits = 2,
  ...
)
```

## Arguments

- x:

  A `data.frame` containing all variables referenced in the `keys.list`,
  or an object of class `tidy_sem`.

- keys.list:

  A named list, indicating which variables belong to which scale.

- missing:

  Whether to use rows with partially missing values. Default: TRUE.

- impute:

  Method for handling missing values, Default: 'none'. This default
  method uses all available data to calculate scale scores, which is
  acceptable for mean scales, but not for sum scales.

- omega:

  Which of McDonald's
  [`omega`](https://rdrr.io/pkg/psych/man/omega.html) coefficients to
  report. Default: NULL; valid options include: `"omega_h"`,
  `"omega.lim"`, `"alpha"`, `"omega.tot"`, `"G6"`.

- digits:

  Number of digits for rounding, Default: 2

- ...:

  Additional parameters to pass to and from functions.

## Value

List with elements: `$descriptives`, `$correlations`, and `$scores`.

## Details

For scales with less than 3 items, Cronbach's alpha might not be
suitable as an estimate of reliability. For such scales, the
Spearman-Brown reliability coefficient for two-item scales is computed,
as described in Eisinga, R., Grotenhuis, M. te, & Pelzer, B. (2012). The
reliability of a two-item scale: Pearson, Cronbach, or Spearman-Brown?
International Journal of Public Health, 58(4), 637-642.
[doi:10.1007/s00038-012-0416-3](https://doi.org/10.1007/s00038-012-0416-3)
. These coefficients are marked with "(sb)".

## Examples

``` r
out <- create_scales(iris, keys.list = list(scalename =
            c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")))
#> Warning: When computing factor loadings for scalename: The estimated weights for the factor scores are probably incorrect.  Try a different factor score estimation method.
#> Number of categories should be increased  in order to count frequencies. 
out$descriptives
#>    Subscale Items      n mean   sd  min  max  skew skew_2se kurt kurt_2se
#> 1 scalename  4.00 150.00 3.46 0.78 2.10 5.10 -0.01    -0.02 1.82     2.32
#>   Reliability  Interpret min_load max_load
#> 1        0.71 Acceptable       NA       NA
dict <- tidy_sem(iris, split = "\\.")
create_scales(dict)
#> Some factor loadings were negative for scale 'Sepal'. These items were automatically reversed: Sepal.Length
#> Number of categories should be increased  in order to count frequencies. 
#> A tidy_scales object
#> v    $descriptives
#> v    $correlations
#> v    $scores
#> 
#>   Subscale Items      n mean   sd  min  max  skew skew_2se kurt kurt_2se
#> 1    Sepal  2.00 150.00 2.61 0.49 1.45 3.50  0.09     0.24 1.99     2.53
#> 2    Petal  2.00 150.00 2.48 1.25 0.60 4.60 -0.26    -0.66 1.57     1.99
#>   Reliability    Interpret min_load max_load
#> 1   -0.27(sb) Unacceptable     0.34     0.34
#> 2    0.98(sb)         Good     0.98     0.98
```
