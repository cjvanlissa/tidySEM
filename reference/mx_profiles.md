# Estimate latent profile analyses using OpenMx

This function is a wrapper around
[`mx_mixture`](https://cjvanlissa.github.io/tidySEM/reference/mx_mixture.md)
to simplify the specification of latent profile models, also known as
finite mixture models. By default, the function estimates free means for
all observed variables across classes.

## Usage

``` r
mx_profiles(
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

Returns an
[`OpenMx::mxModel()`](https://rdrr.io/pkg/OpenMx/man/mxModel.html).

## References

Van Lissa, C. J., Garnier-Villarreal, M., & Anadria, D. (2023).
Recommended Practices in Latent Class Analysis using the Open-Source
R-Package tidySEM. Structural Equation Modeling.
[doi:10.1080/10705511.2023.2250920](https://doi.org/10.1080/10705511.2023.2250920)

## Examples

``` r
if (FALSE) { # \dontrun{
data("empathy")
df <- empathy[1:6]
mx_profiles(data = df,
            classes = 2) -> res
} # }
```
