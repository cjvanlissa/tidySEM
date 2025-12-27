# Conduct Likelihood Ratio tests

For a multigroup model of class `MxModel`, conduct overall and pairwise
likelihood ratio tests. All submodels must be identical.

## Usage

``` r
lr_test(x, compare = c("All", "A", "S", "F", "M", "Thresholds"), ...)
```

## Arguments

- x:

  An object for which a method exists.

- compare:

  Character vector, indicating which matrices to constrain to be equal
  in pairwise comparisons.

- ...:

  Additional arguments passed to other functions.

## Value

An object of class `lr_test` and `list`.

## Examples

``` r
if(requireNamespace("OpenMx", quietly = TRUE)){
library(OpenMx)
df <- iris[c(1:10, 140:150), c(1, 5)]
names(df) <- c("x", "group")
mod <- as_ram("x~1", data = df, group = "group")
mod <- run_mx(mod)
lr_test(mod)
}
#> Running mg with 4 parameters
#> Running mg with 2 parameters
#> Running mg with 2 parameters
#> BCH test for equality of means across classes
#> 
#> Overall likelihood ratio test:
#>  LL_baseline LL_restricted   LL_dif df            p
#>     12.02743      54.15309 42.12566  2 7.120795e-10
#> 
#> Pairwise comparisons using likelihood ratio tests:
#>  Model1    Model2 LL_baseline LL_restricted   LL_dif df            p
#>  setosa virginica    12.02743      54.15309 42.12566  2 7.120795e-10
```
