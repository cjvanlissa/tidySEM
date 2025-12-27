# Automatically set starting values for an OpenMx mixture model

Automatically set starting values for an OpenMx mixture model. This
function was designed to work with mixture models created using
`tidySEM` functions like
[`mx_mixture`](https://cjvanlissa.github.io/tidySEM/reference/mx_mixture.md),
and may not work with other `mxModel`s.

## Usage

``` r
mixture_starts(model, splits, ...)
```

## Arguments

- model:

  A mixture model of class `mxModel`.

- splits:

  Optional. A numeric vector of length equal to the number of rows in
  the [`OpenMx::mxData()`](https://rdrr.io/pkg/OpenMx/man/mxData.html)
  used in the `model` object. The data will be split by this vector. See
  Details for the default setting and possible alternatives.

- ...:

  Additional arguments, passed to functions.

## Value

Returns an
[`OpenMx::mxModel()`](https://rdrr.io/pkg/OpenMx/man/mxModel.html) with
starting values.

## Details

Starting values are derived by the following procedure:

1.  The mixture model is converted to a multi-group model.

2.  The data are split along `splits`, and assigned to the corresponding
    groups of the multi-group model.

3.  The multi-group model is run, and the final values of each group are
    assigned to the corresponding mixture component as starting values.

4.  The mixture model is returned with these starting values.

If the argument `splits` is not provided, the function will call
`stats::kmeans(x = data, centers = classes)$cluster`, where `data` is
extracted from the `model` argument.

Sensible ways to split the data include:

- Using Hierarchical clustering:
  `cutree(hclust(dist(data)), k = classes))`

- Using K-means clustering:
  `stats::kmeans(x = data, centers = classes)$cluster`

- Using agglomerative hierarchical clustering:
  `mclust::hclass(data = data), G = classes)[, 1]`

- Using a random split:
  `sample.int(n = classes, size = nrow(data), replace = TRUE)`

## References

Shireman, E., Steinley, D. & Brusco, M.J. Examining the effect of
initialization strategies on the performance of Gaussian mixture
modeling. Behav Res 49, 282-293 (2017).
[doi:10.3758/s13428-015-0697-6](https://doi.org/10.3758/s13428-015-0697-6)

Van Lissa, C. J., Garnier-Villarreal, M., & Anadria, D. (2023).
Recommended Practices in Latent Class Analysis using the Open-Source
R-Package tidySEM. Structural Equation Modeling.
[doi:10.1080/10705511.2023.2250920](https://doi.org/10.1080/10705511.2023.2250920)

## Examples

``` r
if (FALSE) { # \dontrun{
df <- iris[, 1, drop = FALSE]
names(df) <- "x"
mod <- mx_mixture(model = "x ~ m{C}*1
                           x ~~ v{C}*x",
                           classes = 2,
                           data = df,
                           run = FALSE)
mod <- mixture_starts(mod)
} # }
```
