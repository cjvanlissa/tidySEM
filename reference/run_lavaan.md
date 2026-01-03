# Run as lavaan model

This convenience function runs objects for which a method exists using
lavaan. It is intended for use with `tidySEM`, and passes the `$syntax`
and `$data` elements of a `tidy_sem` object on to
[`lavaan`](https://rdrr.io/pkg/lavaan/man/lavaan.html).

## Usage

``` r
run_lavaan(x, ...)
```

## Arguments

- x:

  An object for which a method exists.

- ...:

  Parameters passed on to other functions.

## Value

Returns a lavaan object.

## Examples

``` r
df <- iris[1:3]
names(df) <- paste0("X_", 1:3)
run_lavaan(measurement(tidy_sem(df), meanstructure = TRUE))
#> Warning: lavaan->lav_object_post_check():  
#>    some estimated ov variances are negative
#> lavaan 0.6-21 ended normally after 85 iterations
#> 
#>   Estimator                                         ML
#>   Optimization method                           NLMINB
#>   Number of model parameters                         9
#> 
#>   Number of observations                           150
#> 
#> Model Test User Model:
#>                                                       
#>   Test statistic                                 0.000
#>   Degrees of freedom                                 0
```
