# Satorra-Bentler corrected chi-square test

Computes Satorra-Bentler corrected chi-square test.

## Usage

``` r
chisq_sb(chisq1, df1, scf1 = 1, chisq2, df2, scf2 = 1)
```

## Arguments

- chisq1:

  An object for which a method exists; e.g., a chi-square value, or a
  `data.frame`.

- df1:

  Degrees of freedom of model 1.

- scf1:

  Scale correction factor of model 1.

- chisq2:

  Chi square value of model 2.

- df2:

  Degrees of freedom of model 2.

- scf2:

  Scale correction factor of model 2.

## Value

Chi-square difference value with corresponding degrees of freedom and
p-value.

## Details

Reference: Satorra, A., & Bentler, P. M. (2001). A scaled difference
chi-square test statistic for moment structure analysis. Psychometrika,
66(4), 507-514.
[doi:10.1007/BF02296192](https://doi.org/10.1007/BF02296192)

## Author

Caspar J. van Lissa

## Examples

``` r
df <- data.frame(chi2 = c(23, 44, 65), df = c(78, 74, 70), scf = c(1.02, 1.12, 1.28))
chisq_sb(24, 78, 1.02, 65, 70, 1.28)
#> Warning: Models cannot be nested, DF are equal.
#>    Dchisq Dchisq_df  Dchisq_p 
#> -46.78884   8.00000   1.00000 
```
