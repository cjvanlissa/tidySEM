# Estimate an Auxiliary Model using the BCH Method

Estimate an auxiliary model based on a latent classification by means of
mixture modeling (see
[`mx_mixture`](https://cjvanlissa.github.io/tidySEM/reference/mx_mixture.md)).

The auxiliary model is treated as a multi-group model. All cases are
used in all groups, but they are weighted by group-specific BCH weights
as described in Bolck, Croon, & Hagenaars, 2004.

## Usage

``` r
BCH(x, model, data, ...)
```

## Arguments

- x:

  An object for which a method exists.

- model:

  An object that can be converted to an `OpenMx` model using
  [`as_ram`](https://cjvanlissa.github.io/tidySEM/reference/as_ram.md).

- data:

  A data.frame on which the auxiliary model can be evaluated.

- ...:

  further arguments to be passed to or from other methods.

## Value

An MxModel.

## References

Bolck, A., Croon, M., & Hagenaars, J. (2004). Estimating latent
structure models with categorical variables: One-step versus three-step
estimators. Political Analysis, 12(1), 3-27.
[doi:10.1093/pan/mph001](https://doi.org/10.1093/pan/mph001)

## Examples

``` r
if(requireNamespace("OpenMx", quietly = TRUE)){
library(OpenMx)
dat <- data.frame(x = iris$Petal.Length)
mixmod <- mx_profiles(dat,
                      classes = 2)
res <- BCH(mixmod, "y ~ 1", data = data.frame(y = iris$Sepal.Length))
}
#> To take full advantage of multiple cores, use:
#>   mxOption(key='Number of Threads', value=parallel::detectCores()) #now
#>   Sys.setenv(OMP_NUM_THREADS=parallel::detectCores()) #before library(OpenMx)
#> Running mix2 with 4 parameters
```
