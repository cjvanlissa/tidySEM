# Compare Information Criteria

IC weights quantify the evidence in favor of different models in a set.
This function normalizes the IC values to obtain IC weights, which sum
to 1. The model with the highest weight is most supported by the data.
The ratio of different weights gives the relative support in favor of
one model over another.

## Usage

``` r
ic_weights(x, ...)
```

## Arguments

- x:

  An object for which a method exists.

- ...:

  Additional arguments.

## Value

A `list` of class `ic_weights` with elements `$weights`, which contains
the model weights, and `$comparison`, which contains the relative
support in favor of each model over the others.

## References

Wagenmakers, E. J., & Farrell, S. (2004). AIC model selection using
Akaike weights. Psychonomic bulletin & review, 11(1), 192-196.
[doi:10.3758/BF03206482](https://doi.org/10.3758/BF03206482)

## Examples

``` r
ics <- c(100, 200, 102, 300)
ic_weights(ics)
#> $weights
#>      Model 1      Model 2      Model 3      Model 4 
#> 7.310586e-01 1.410029e-22 2.689414e-01 2.719593e-44 
#> 
#> $comparison
#>              Model 1      Model 2      Model 3      Model 4
#> Model 1 1.000000e+00 5.184706e+21 2.718282e+00 2.688117e+43
#> Model 2 1.928750e-22 1.000000e+00 5.242886e-22 5.184706e+21
#> Model 3 3.678794e-01 1.907347e+21 1.000000e+00 9.889030e+42
#> Model 4 3.720076e-44 1.928750e-22 1.011221e-43 1.000000e+00
#> 
#> attr(,"class")
#> [1] "ic_weights" "list"      
```
