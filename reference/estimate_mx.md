# Estimate tidy_sem using 'OpenMx'

This function is a wrapper for the
[`as_ram`](https://cjvanlissa.github.io/tidySEM/reference/as_ram.md) and
[`run_mx`](https://cjvanlissa.github.io/tidySEM/reference/run_mx.md)
functions.

## Usage

``` r
estimate_mx(x, ...)
```

## Arguments

- x:

  An object of class `tidy_sem`.

- ...:

  Additional parameters passed to the estimating function.

## Value

An object of class `MxModel`.

## Examples

``` r
df <- iris[1:4]
names(df) <- paste0("x_", 1:4)
model <- tidy_sem(df)
model <- measurement(model)
res <- estimate_mx(model)
summary(res)
#> Summary of model 
#>  
#> free parameters:
#>            name matrix row col   Estimate  Std.Error A
#> 1  model.A[2,5]      A x_2   x -0.3496930 0.04368339  
#> 2  model.A[3,5]      A x_3   x  2.7756478 0.16956099  
#> 3  model.A[4,5]      A x_4   x  1.0270097 0.06929740  
#> 4  model.S[1,1]      S x_1 x_1  0.2352754 0.02950895  
#> 5  model.S[2,2]      S x_2 x_2  0.1341924 0.01400658  
#> 6  model.S[3,3]      S x_3 x_3 -0.3393999 0.08105053  
#> 7  model.S[4,4]      S x_4 x_4  0.1068763 0.01823782  
#> 8  model.S[5,5]      S   x   x  0.4458469 0.07470965  
#> 9  model.M[1,1]      M   1 x_1  5.8433333 0.06738552  
#> 10 model.M[1,2]      M   1 x_2  3.0573333 0.03546949  
#> 11 model.M[1,3]      M   1 x_3  3.7580000 0.14365460  
#> 12 model.M[1,4]      M   1 x_4  1.1993333 0.06202858  
#> 
#> Model Statistics: 
#>                |  Parameters  |  Degrees of Freedom  |  Fit (-2lnL units)
#>        Model:             12                    588              819.4226
#>    Saturated:             14                    586                    NA
#> Independence:              8                    592                    NA
#> Number of observations/statistics: 150/600
#> 
#> Information Criteria: 
#>       |  df Penalty  |  Parameters Penalty  |  Sample-Size Adjusted
#> AIC:      -356.5774               843.4226                 845.7000
#> BIC:     -2126.8309               879.5503                 841.5725
#> To get additional fit indices, see help(mxRefModels)
#> timestamp: 2026-01-03 13:45:12 
#> Wall clock time: 0.01848888 secs 
#> optimizer:  SLSQP 
#> OpenMx version number: 2.22.10 
#> Need help?  See help(mxSummary) 
#> 
```
