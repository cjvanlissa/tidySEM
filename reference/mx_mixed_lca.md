# Estimate mixed data latent class analysis using OpenMx

This function simplifies the specification of latent class models with
mixed data types: models that estimate membership of a categorical
latent variable based on binary/ordinal and continuous indicators. See
Details for more information.

## Usage

``` r
mx_mixed_lca(
  data = NULL,
  classes = 1L,
  variances = "equal",
  covariances = "zero",
  run = TRUE,
  expand_grid = FALSE,
  ...
)
```

## Arguments

- data:

  The data.frame to be used for model fitting.

- classes:

  A vector of integers, indicating which class solutions to generate.
  Defaults to 1L. E.g., `classes = 1:6`,

- variances:

  Character vector. Specifies which variance components to estimate.
  Defaults to "equal" (constrain variances across classes); the other
  option is "varying" (estimate variances freely across classes). Each
  element of this vector refers to one of the models you wish to run.

- covariances:

  Character vector. Specifies which covariance components to estimate.
  Defaults to "zero" (covariances constrained to zero; this corresponds
  to an assumption of conditional independence of the indicators); other
  options are "equal" (covariances between items constrained to be equal
  across classes), and "varying" (free covariances across classes).

- run:

  Logical, whether or not to run the model. If `run = TRUE`, the
  function calls
  [`mixture_starts`](https://cjvanlissa.github.io/tidySEM/reference/mixture_starts.md)
  and
  [`run_mx`](https://cjvanlissa.github.io/tidySEM/reference/run_mx.md).

- expand_grid:

  Logical, whether or not to estimate all possible combinations of the
  `variances` and `covariances` arguments. Defaults to `FALSE`.

- ...:

  Additional arguments, passed to functions.

## Value

A list of class `mixture_list`.

## Details

The procedure is as follows:

1.  Construct a latent profile model for the continuous indicators using
    [`mx_profiles()`](https://cjvanlissa.github.io/tidySEM/reference/mx_profiles.md).

2.  Construct a latent class model for the categorical indicators using
    [`mx_lca()`](https://cjvanlissa.github.io/tidySEM/reference/mx_lca.md).

3.  Combine the models from steps 1. and 2. into one joint model.

If `run = TRUE`, simulated annealing is used to estimate the mixture
model, as explained in Van Lissa, Garnier-Villareal, & Anadria (2023).
However, the inclusion of categorical indicators often leads to a large
ordinal error, which automatically initiates a final optimization step
using
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
library(tidySEM)
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
set.seed(1)
res <- mx_mixed_lca(data = df, classes = 2)
}
} # }
```
