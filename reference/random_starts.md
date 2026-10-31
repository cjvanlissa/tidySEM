# Generate Random Starting Values for OpenMx Models

Generates plausible random starting values for OpenMx models. Starting
values are scaled, where possible, to the observed data. Multiple sets
of starting values can be evaluated to identify the solution with the
lowest minus twice log-likelihood.

## Usage

``` r
random_starts(
  x,
  nstarts = 20,
  tolerance_minus2ll = 1e-06,
  verbose = TRUE,
  run = TRUE,
  ...
)
```

## Arguments

- x:

  An OpenMx `MxModel` object.

- nstarts:

  Integer. Number of sets of starting values to generate.

- tolerance_minus2ll:

  Numeric. Maximum difference from the smallest minus twice
  log-likelihood for solutions to be considered equivalent.

- verbose:

  Logical. Whether to print information about starting values and model
  fit.

- run:

  Logical. If `TRUE`, estimate the models and return the best-fitting
  solution. If `FALSE`, return a list of models with randomized starting
  values.

- ...:

  Additional arguments passed to the model-specific implementation.

- data:

  Optional data on which the model was run, typically taken from the
  model object itself. Default: `NULL`.

- scale_A:

  Numeric vector of length two giving the lower and upper multipliers
  for free elements of the `A` matrix. Defaults to `c(-0.5, 0.5)`. These
  are scaled by the ratio of observed standard deviations.

- scale_mat_dev:

  Numeric vector of length two specifying the uniform sampling range for
  free elements of `mat_dev`. Defaults to `c(0.001, 1.5)`.

- jitter_thresholds:

  Non-negative numeric value controlling the magnitude of uniform
  perturbations applied to empirical threshold starting values. Defaults
  to `0.25`.

## Value

If `run = TRUE`, the best-fitting OpenMx model with an attribute
`tab_ll` containing information about the starting values and their fit
statistics. If `run = FALSE`, a list of OpenMx models.

## Details

Random starting values are generated using R's random-number generator.
Use [`set.seed()`](https://rdrr.io/r/base/Random.html) for
reproducibility.

**RAM mixture models**

Starting values are derived from the observed data and the model's RAM
matrices. The function supports free parameters in matrices named `A`,
`S`, `M`, `mat_dev`, and `Thresholds`. The `F` matrix may be present but
is not randomized.

For continuous variables, observed ranges and standard deviations are
used to determine plausible starting values. Fallback scales are used
when the observed range or standard deviation is zero or undefined.

Free paths in `A` are sampled uniformly after scaling by the ratio of
the standard deviations of the dependent and predictor variables:
\$\$A\_{ij} \sim U(a,b)\frac{sd_i}{sd_j}\$\$ where `c(a, b) = scale_A`.

Starting values for `S` are generated using a random lower-triangular
Cholesky factor \\L\\, giving \$\$S = LL^\top.\$\$ Diagonal elements of
\\L\\ are sampled between 0.5 and 1 times the corresponding observed
standard deviation. Off-diagonal elements are sampled between -0.25 and
0.25 times that standard deviation.

Free means in `M` are sampled uniformly between the observed minimum and
maximum of the corresponding variable.

Free elements of `mat_dev` are sampled uniformly from `scale_mat_dev`.
For ordinal variables, empirical latent-normal thresholds are obtained
from cumulative category proportions using
[`qnorm()`](https://rdrr.io/r/stats/Normal.html), perturbed by uniform
jitter, and sorted to preserve their ordering.

For models containing submodels, initialization is applied recursively.
Observed data stored in the model take precedence over externally
supplied data.

After random values have been assigned,
[`OpenMx::omxAssignFirstParameters()`](https://rdrr.io/pkg/OpenMx/man/omxAssignFirstParameters.html)
ensures that parameters sharing labels have consistent starting values.

## Examples

``` r
if (FALSE) { # \dontrun{
library(OpenMx)

set.seed(123)
model <- remix(yi, vi, classes = 2, run = FALSE)
fit <- random_starts(model, nstarts = 20)

attr(fit, "tab_ll")
} # }
```
