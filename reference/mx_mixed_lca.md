# Estimate mixed data latent class analysis using OpenMx

This function simplifies the specification of latent class models with
mixed data types: models that estimate membership of a categorical
latent variable based on binary/ordinal and continuous indicators. See
Details for more information.

## Usage

``` r
mx_mixed_lca(data = NULL, classes = 1L, run = TRUE, ...)
```

## Arguments

- data:

  The data.frame to be used for model fitting.

- classes:

  A vector of integers, indicating which class solutions to generate.
  Defaults to 1L. E.g., `classes = 1:6`,

- run:

  Logical, whether or not to run the model. This should usually be set
  to `TRUE`, because this function runs several models to aid in
  specifying good starting values; see Details.

- ...:

  Additional arguments, passed to functions.

## Value

Returns an
[`OpenMx::mxModel()`](https://rdrr.io/pkg/OpenMx/man/mxModel.html).

## Details

The procedure is as follows:

1.  Estimate a latent profile analysis for the continuous indicators
    using
    [`mx_profiles()`](https://cjvanlissa.github.io/tidySEM/reference/mx_profiles.md).
    Additional arguments, like `variabces = "free"`, can be passed via
    `...`. The estimator uses simulated annealing.

2.  To obtain good starting values for the categorical indicators, use
    the classes from step 1. to estimate an auxiliary model for the
    ordinal indicators with
    [`BCH()`](https://cjvanlissa.github.io/tidySEM/reference/BCH.md).

3.  Estimate a latent class analysis for the categorical indicators
    using
    [`mx_lca()`](https://cjvanlissa.github.io/tidySEM/reference/mx_lca.md),
    with the results of step 2. as starting values. The estimator uses
    [`OpenMx::mxTryHardOrdinal()`](https://rdrr.io/pkg/OpenMx/man/mxTryHard.html).

4.  Combine the models from steps 1. and 3. into one joint model.
    Conduct one final optimization step using
    [`OpenMx::mxTryHardOrdinal()`](https://rdrr.io/pkg/OpenMx/man/mxTryHard.html).

## References

Van Lissa, C. J., Garnier-Villarreal, M., & Anadria, D. (2023).
Recommended Practices in Latent Class Analysis using the Open-Source
R-Package tidySEM. Structural Equation Modeling.
[doi:10.1080/10705511.2023.2250920](https://doi.org/10.1080/10705511.2023.2250920)

## Examples

``` r
if (FALSE) { # \dontrun{
if(isTRUE(requireNamespace("OpenMx", quietly = TRUE))) {
library(OpenMx)
# Construct dataset with ordinal and categorical indicators
set.seed(1)
n = 200
mns <- c(rep(0, floor(.3*n)), rep(2, ceiling(.7*n)))
df <- rnorm(4*n, mean = rep(mns, 4))
df <- matrix(df, nrow = n)
df <- t(t(df) * c(1, 2, .5, 1))
df <- data.frame(df)
df$X4 <- cut(df$X4, 3, labels = FALSE)
df$X4 <- OpenMx::mxFactor(df$X4, levels = c(1:3))
# Estimate the model
res <- mx_mixed_lca(data = df, classes = 2)
}
} # }
```
