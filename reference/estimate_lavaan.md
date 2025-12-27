# Estimate tidy_sem using 'lavaan'

This function is a wrapper for the
[`lavaan`](https://rdrr.io/pkg/lavaan/man/lavaan.html) estimating
functions. By default, the wrapper uses
[`sem`](https://rdrr.io/pkg/lavaan/man/sem.html), but users can also
specify [`lavaan`](https://rdrr.io/pkg/lavaan/man/lavaan.html),
[`cfa`](https://rdrr.io/pkg/lavaan/man/cfa.html), or
[`growth`](https://rdrr.io/pkg/lavaan/man/growth.html).

## Usage

``` r
estimate_lavaan(x, func = "sem", ...)
```

## Arguments

- x:

  An object of class `tidy_sem`.

- func:

  The [`lavaan`](https://rdrr.io/pkg/lavaan/man/lavaan.html) modeling
  function to invoke, Default: 'sem'.

- ...:

  Additional parameters passed to the estimating function.

## Value

An object of class `lavaan`.

## Examples

``` r
library(lavaan)
model <- tidy_sem(iris, "\\.")
model <- measurement(model)
res <- estimate_lavaan(model)
#> Warning: lavaan->lav_object_post_check():  
#>    some estimated ov variances are negative
#> Warning: lavaan->lav_object_post_check():  
#>    covariance matrix of latent variables is not positive definite ; use 
#>    lavInspect(fit, "cov.lv") to investigate.
summary(res)
#> lavaan 0.6-21 ended normally after 47 iterations
#> 
#>   Estimator                                         ML
#>   Optimization method                           NLMINB
#>   Number of model parameters                        13
#> 
#>   Number of observations                           150
#> 
#> Model Test User Model:
#>                                                       
#>   Test statistic                                 2.322
#>   Degrees of freedom                                 1
#>   P-value (Chi-square)                           0.128
#> 
#> Parameter Estimates:
#> 
#>   Standard errors                             Standard
#>   Information                                 Expected
#>   Information saturated (h1) model          Structured
#> 
#> Latent Variables:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>   Sepal =~                                            
#>     Sepal.Length      1.000                           
#>     Sepal.Width      -0.270    0.053   -5.140    0.000
#>   Petal =~                                            
#>     Petal.Length      1.000                           
#>     Petal.Width       0.399    0.010   38.635    0.000
#> 
#> Covariances:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>   Sepal ~~                                            
#>     Petal             1.256    0.157    8.020    0.000
#> 
#> Intercepts:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>    .Sepal.Length      5.843    0.067   86.715    0.000
#>    .Sepal.Width       3.057    0.035   86.196    0.000
#>    .Petal.Length      3.758    0.144   26.160    0.000
#>    .Petal.Width       1.199    0.062   19.335    0.000
#>     Sepal             0.000                           
#>     Petal             0.000                           
#> 
#> Variances:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>    .Sepal.Length      0.525    0.100    5.243    0.000
#>    .Sepal.Width       0.177    0.021    8.331    0.000
#>    .Petal.Length     -0.130    0.032   -4.031    0.000
#>    .Petal.Width       0.064    0.009    7.361    0.000
#>     Sepal             0.156    0.094    1.657    0.097
#>     Petal             3.226    0.358    9.004    0.000
#> 
```
