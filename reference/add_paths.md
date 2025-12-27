# Add paths to an object of class 'tidy_sem'

Add paths to an object of class `tidy_sem`, or replace existing paths.
The paths must be specified as
[lavaan::model.syntax](https://rdrr.io/pkg/lavaan/man/model.syntax.html),
and separated by commas.

## Usage

``` r
add_paths(model, ...)
```

## Arguments

- model:

  An object of class `tidy_sem`.

- ...:

  Paths to add or substitute, specified in
  [lavaan::model.syntax](https://rdrr.io/pkg/lavaan/man/model.syntax.html),
  and separated by commas.

## Value

An object of class `tidy_sem`.

## Details

Currently, only the `lavaan` commands `~, ~~, =~,` and `~1` are parsed.

This function relies on
[lavaan::model.syntax](https://rdrr.io/pkg/lavaan/man/model.syntax.html)
to convert syntax strings to `lavaan` parameter tables. By default, is
uses the arguments
`int.ov.free = TRUE, int.lv.free = FALSE, auto.fix.first = TRUE, auto.fix.single = TRUE, auto.var = TRUE, auto.cov.lv.x = TRUE, auto.efa = TRUE, auto.th = TRUE, auto.delta = TRUE, auto.cov.y = TRUE, meanstructure = TRUE`,
in a similar way to
[`lavaan::sem()`](https://rdrr.io/pkg/lavaan/man/sem.html) and
[`lavaan::cfa()`](https://rdrr.io/pkg/lavaan/man/cfa.html).

## See also

[lavaan::model.syntax](https://rdrr.io/pkg/lavaan/man/model.syntax.html)

## Examples

``` r
library(lavaan)
#> This is lavaan 0.6-21
#> lavaan is FREE software! Please report any bugs.
df <- iris[, 1:4]
names(df) <- paste0("x_", 1:4)
model <- tidy_sem(df)
model <- measurement(model)
model <- add_paths(model, x =~ a*x_1 + b*x_2 + a*x_3 + b*x_4)
res <- estimate_lavaan(model)
#> Warning: lavaan->lav_object_post_check():  
#>    some estimated ov variances are negative
summary(res)
#> lavaan 0.6-21 ended normally after 31 iterations
#> 
#>   Estimator                                         ML
#>   Optimization method                           NLMINB
#>   Number of model parameters                        11
#> 
#>   Number of observations                           150
#> 
#> Model Test User Model:
#>                                                       
#>   Test statistic                               359.420
#>   Degrees of freedom                                 3
#>   P-value (Chi-square)                           0.000
#> 
#> Parameter Estimates:
#> 
#>   Standard errors                             Standard
#>   Information                                 Expected
#>   Information saturated (h1) model          Structured
#> 
#> Latent Variables:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>   x =~                                                
#>     x_1        (a)    1.000                           
#>     x_2        (b)   -0.139    0.029   -4.717    0.000
#>     x_3        (a)    1.000                           
#>     x_4        (b)    0.752    0.038   19.760    0.000
#> 
#> Intercepts:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>    .x_1               5.843    0.105   55.447    0.000
#>    .x_2               3.057    0.035   86.196    0.000
#>    .x_3               3.758    0.110   34.094    0.000
#>    .x_4               1.199    0.062   19.335    0.000
#>     x                 0.000                           
#> 
#> Variances:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>    .x_1               0.535    0.070    7.689    0.000
#>    .x_2               0.167    0.019    8.808    0.000
#>    .x_3               0.691    0.085    8.092    0.000
#>    .x_4              -0.063    0.021   -2.972    0.003
#>     x                 1.131    0.168    6.743    0.000
#> 
```
