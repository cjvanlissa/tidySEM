# Data Quantiles

Get quantiles based on empirical normal distribution of data.

## Usage

``` r
mx_data_quantiles(df)
```

## Arguments

- df:

  A `data.frame` with only columns of class `ordered`.

## Value

A matrix with the appropriate dimensions for the threshold matrix for
`df`,
[`mxThreshold()`](https://rdrr.io/pkg/OpenMx/man/mxThreshold.html).

## Examples

``` r
set.seed(1)
df <- data.frame(X = ordered(sample(c(1:4), size = 100, replace = TRUE,
prob = c(.1, .2, .5, .2))))
mx_data_quantiles(df)
#>               X
#> [1,] -1.5547736
#> [2,] -0.7721932
#> [3,]  0.6433454
```
