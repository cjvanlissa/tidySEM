# Print model fit table formatted for publication

Takes a model object, extracts model fit information, and formats it as
a publication-ready table.

## Usage

``` r
table_fit(x, ...)
```

## Arguments

- x:

  A model object for which a method exists.

- ...:

  Arguments passed to other functions.

## Value

A data.frame of formatted results.

## See also

Other Reporting tools:
[`conf_int()`](https://cjvanlissa.github.io/tidySEM/reference/conf_int.md),
[`est_sig()`](https://cjvanlissa.github.io/tidySEM/reference/est_sig.md),
[`table_prob()`](https://cjvanlissa.github.io/tidySEM/reference/table_prob.md),
[`table_results()`](https://cjvanlissa.github.io/tidySEM/reference/table_results.md)

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
table_fit(fit)
#>   Name Parameters      fmin    chisq df       pvalue baseline.chisq baseline.df
#> 1  fit         60 0.1924441 115.8513 48 1.545283e-07       957.7691          72
#>   baseline.pvalue       cfi       tli      nnfi       rfi       nfi      pnfi
#> 1               0 0.9233984 0.8850976 0.8850976 0.8185606 0.8790404 0.5860269
#>         ifi       rni        LL unrestricted.logl      aic      bic   n
#> 1 0.9254192 0.9233984 -3682.198         -3624.272 7484.395 7706.822 301
#>       bic2      rmsea rmsea.ci.lower rmsea.ci.upper rmsea.ci.level rmsea.pvalue
#> 1 7516.536 0.09691486     0.07450529      0.1195828            0.9 0.0005925388
#>   rmsea.close.h0 rmsea.notclose.pvalue rmsea.notclose.h0       rmr rmr_nomean
#> 1           0.05             0.8965993              0.08 0.0834983 0.09146781
#>         srmr srmr_bentler srmr_bentler_nomean       crmr crmr_nomean srmr_mplus
#> 1 0.06786401   0.06786401           0.0743413 0.07434131  0.08311611 0.06786402
#>   srmr_mplus_nomean    cn_05    cn_01       gfi      agfi      pgfi       mfi
#> 1        0.07434131 170.3239 192.4391 0.9950447 0.9888506 0.4422421 0.8934098
#>        ecvi
#> 1 0.7835593
```
