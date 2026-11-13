# Print results table formatted for publication

Takes a model object, and formats it as a publication-ready table.

## Usage

``` r
table_results(
  x,
  columns = c("label", "est_sig", "se", "pval", "confint", "group", "level"),
  digits = 2,
  format_numeric = TRUE,
  ...
)
```

## Arguments

- x:

  A model object for which a method exists.

- columns:

  A character vector of columns to retain from the results section. If
  this is set to `NULL`, all available columns are returned. Defaults to
  `c("label", "est_sig", "se", "pval", "confint", "group", "level")`.
  These correspond to 1) the parameter label, 2) estimate column with
  significance asterisks appended (\* \<.05, \*\* \< .01, \*\*\* \<
  .001); 3) standard error, 4) p-value, 5) a formatted confidence
  interval, 6) grouping variable (if available), 7) level variable for
  multilevel models, if available.

- digits:

  Number of digits to round to when formatting numeric columns.

- format_numeric:

  Logical, indicating whether or not to format numeric columns. Defaults
  to `TRUE`.

- ...:

  Logical expressions used to filter the rows of results returned.

## Value

A data.frame of formatted results.

## See also

Other Reporting tools:
[`conf_int()`](https://cjvanlissa.github.io/tidySEM/reference/conf_int.md),
[`est_sig()`](https://cjvanlissa.github.io/tidySEM/reference/est_sig.md),
[`table_fit()`](https://cjvanlissa.github.io/tidySEM/reference/table_fit.md),
[`table_prob()`](https://cjvanlissa.github.io/tidySEM/reference/table_prob.md)

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
table_results(fit)
#>                              label est_sig   se pval       confint       group
#> 1             visual.BY.x1.Pasteur    1.00 0.00 <NA>  [1.00, 1.00]     Pasteur
#> 2             visual.BY.x2.Pasteur  0.39** 0.12 0.00  [0.15, 0.63]     Pasteur
#> 3             visual.BY.x3.Pasteur 0.57*** 0.14 0.00  [0.30, 0.84]     Pasteur
#> 4            textual.BY.x4.Pasteur    1.00 0.00 <NA>  [1.00, 1.00]     Pasteur
#> 5            textual.BY.x5.Pasteur 1.18*** 0.10 0.00  [0.98, 1.38]     Pasteur
#> 6            textual.BY.x6.Pasteur 0.87*** 0.08 0.00  [0.72, 1.03]     Pasteur
#> 7              speed.BY.x7.Pasteur    1.00 0.00 <NA>  [1.00, 1.00]     Pasteur
#> 8              speed.BY.x8.Pasteur 1.12*** 0.28 0.00  [0.58, 1.67]     Pasteur
#> 9              speed.BY.x9.Pasteur 0.92*** 0.22 0.00  [0.48, 1.36]     Pasteur
#> 10            Variances.x1.Pasteur    0.30 0.23 0.20 [-0.16, 0.75]     Pasteur
#> 11            Variances.x2.Pasteur 1.33*** 0.16 0.00  [1.02, 1.64]     Pasteur
#> 12            Variances.x3.Pasteur 0.99*** 0.14 0.00  [0.72, 1.26]     Pasteur
#> 13            Variances.x4.Pasteur 0.43*** 0.07 0.00  [0.29, 0.56]     Pasteur
#> 14            Variances.x5.Pasteur 0.46*** 0.09 0.00  [0.29, 0.62]     Pasteur
#> 15            Variances.x6.Pasteur 0.29*** 0.05 0.00  [0.19, 0.39]     Pasteur
#> 16            Variances.x7.Pasteur 0.82*** 0.12 0.00  [0.58, 1.06]     Pasteur
#> 17            Variances.x8.Pasteur 0.51*** 0.12 0.00  [0.28, 0.74]     Pasteur
#> 18            Variances.x9.Pasteur 0.68*** 0.10 0.00  [0.48, 0.88]     Pasteur
#> 19        Variances.visual.Pasteur 1.10*** 0.28 0.00  [0.55, 1.64]     Pasteur
#> 20       Variances.textual.Pasteur 0.89*** 0.15 0.00  [0.60, 1.19]     Pasteur
#> 21         Variances.speed.Pasteur  0.35** 0.13 0.01  [0.10, 0.60]     Pasteur
#> 22     visual.WITH.textual.Pasteur 0.48*** 0.11 0.00  [0.27, 0.69]     Pasteur
#> 23       visual.WITH.speed.Pasteur   0.19* 0.08 0.02  [0.03, 0.34]     Pasteur
#> 24      textual.WITH.speed.Pasteur  0.18** 0.07 0.01  [0.05, 0.32]     Pasteur
#> 25                Means.x1.Pasteur 4.94*** 0.09 0.00  [4.76, 5.13]     Pasteur
#> 26                Means.x2.Pasteur 5.98*** 0.10 0.00  [5.79, 6.18]     Pasteur
#> 27                Means.x3.Pasteur 2.49*** 0.09 0.00  [2.31, 2.67]     Pasteur
#> 28                Means.x4.Pasteur 2.82*** 0.09 0.00  [2.64, 3.00]     Pasteur
#> 29                Means.x5.Pasteur 4.00*** 0.10 0.00  [3.79, 4.20]     Pasteur
#> 30                Means.x6.Pasteur 1.92*** 0.08 0.00  [1.77, 2.08]     Pasteur
#> 31                Means.x7.Pasteur 4.43*** 0.09 0.00  [4.26, 4.60]     Pasteur
#> 32                Means.x8.Pasteur 5.56*** 0.08 0.00  [5.41, 5.72]     Pasteur
#> 33                Means.x9.Pasteur 5.42*** 0.08 0.00  [5.26, 5.57]     Pasteur
#> 34            Means.visual.Pasteur    0.00 0.00 <NA>  [0.00, 0.00]     Pasteur
#> 35           Means.textual.Pasteur    0.00 0.00 <NA>  [0.00, 0.00]     Pasteur
#> 36             Means.speed.Pasteur    0.00 0.00 <NA>  [0.00, 0.00]     Pasteur
#> 37        visual.BY.x1.Grant-White    1.00 0.00 <NA>  [1.00, 1.00] Grant-White
#> 38        visual.BY.x2.Grant-White 0.74*** 0.15 0.00  [0.43, 1.04] Grant-White
#> 39        visual.BY.x3.Grant-White 0.92*** 0.17 0.00  [0.60, 1.25] Grant-White
#> 40       textual.BY.x4.Grant-White    1.00 0.00 <NA>  [1.00, 1.00] Grant-White
#> 41       textual.BY.x5.Grant-White 0.99*** 0.09 0.00  [0.82, 1.16] Grant-White
#> 42       textual.BY.x6.Grant-White 0.96*** 0.08 0.00  [0.80, 1.13] Grant-White
#> 43         speed.BY.x7.Grant-White    1.00 0.00 <NA>  [1.00, 1.00] Grant-White
#> 44         speed.BY.x8.Grant-White 1.23*** 0.19 0.00  [0.86, 1.59] Grant-White
#> 45         speed.BY.x9.Grant-White 1.06*** 0.16 0.00  [0.74, 1.38] Grant-White
#> 46        Variances.x1.Grant-White 0.71*** 0.13 0.00  [0.47, 0.96] Grant-White
#> 47        Variances.x2.Grant-White 0.90*** 0.12 0.00  [0.66, 1.14] Grant-White
#> 48        Variances.x3.Grant-White 0.56*** 0.10 0.00  [0.36, 0.76] Grant-White
#> 49        Variances.x4.Grant-White 0.32*** 0.06 0.00  [0.19, 0.44] Grant-White
#> 50        Variances.x5.Grant-White 0.42*** 0.07 0.00  [0.28, 0.56] Grant-White
#> 51        Variances.x6.Grant-White 0.41*** 0.07 0.00  [0.27, 0.54] Grant-White
#> 52        Variances.x7.Grant-White 0.60*** 0.09 0.00  [0.42, 0.78] Grant-White
#> 53        Variances.x8.Grant-White 0.40*** 0.09 0.00  [0.22, 0.59] Grant-White
#> 54        Variances.x9.Grant-White 0.53*** 0.09 0.00  [0.36, 0.71] Grant-White
#> 55    Variances.visual.Grant-White 0.60*** 0.16 0.00  [0.29, 0.92] Grant-White
#> 56   Variances.textual.Grant-White 0.94*** 0.15 0.00  [0.64, 1.24] Grant-White
#> 57     Variances.speed.Grant-White 0.46*** 0.12 0.00  [0.23, 0.69] Grant-White
#> 58 visual.WITH.textual.Grant-White 0.41*** 0.10 0.00  [0.22, 0.60] Grant-White
#> 59   visual.WITH.speed.Grant-White 0.28*** 0.08 0.00  [0.13, 0.42] Grant-White
#> 60  textual.WITH.speed.Grant-White  0.22** 0.07 0.00  [0.08, 0.37] Grant-White
#> 61            Means.x1.Grant-White 4.93*** 0.10 0.00  [4.74, 5.12] Grant-White
#> 62            Means.x2.Grant-White 6.20*** 0.09 0.00  [6.02, 6.38] Grant-White
#> 63            Means.x3.Grant-White 2.00*** 0.09 0.00  [1.83, 2.16] Grant-White
#> 64            Means.x4.Grant-White 3.32*** 0.09 0.00  [3.13, 3.50] Grant-White
#> 65            Means.x5.Grant-White 4.71*** 0.10 0.00  [4.52, 4.90] Grant-White
#> 66            Means.x6.Grant-White 2.47*** 0.09 0.00  [2.28, 2.65] Grant-White
#> 67            Means.x7.Grant-White 3.92*** 0.09 0.00  [3.75, 4.09] Grant-White
#> 68            Means.x8.Grant-White 5.49*** 0.09 0.00  [5.32, 5.66] Grant-White
#> 69            Means.x9.Grant-White 5.33*** 0.09 0.00  [5.16, 5.49] Grant-White
#> 70        Means.visual.Grant-White    0.00 0.00 <NA>  [0.00, 0.00] Grant-White
#> 71       Means.textual.Grant-White    0.00 0.00 <NA>  [0.00, 0.00] Grant-White
#> 72         Means.speed.Grant-White    0.00 0.00 <NA>  [0.00, 0.00] Grant-White
```
