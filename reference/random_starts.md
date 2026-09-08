# Generate random starting values for OpenMx model

Generates plausible random starting values for free parameters in
selected matrices of an OpenMx RAM model. Starting values are scaled,
where possible, to the data used to construct the model.

## Usage

``` r
random_starts(
  x,
  nstarts = 20,
  scale_A = c(-0.5, 0.5),
  scale_mat_dev = c(0.001, 1.5),
  jitter_thresholds = 0.25,
  tolerance_minus2ll = 1e-06,
  verbose = TRUE,
  run = TRUE
)
```

## Arguments

- x:

  An `MxModel` with RAM specification.

- nstarts:

  Integer, how many sets of starting values to generate?

- scale_A:

  Numeric vector of length two giving the lower and upper multipliers
  used when drawing free elements of the `A` matrix. Defaults to
  `c(-0.5, 0.5)`. For an element `A[i, j]`, these multipliers are
  applied to `xsd[i] / xsd[j]`.

- scale_mat_dev:

  Numeric vector of length two giving the lower and upper limits of the
  uniform distribution used for free elements of `mat_dev`. Defaults to
  `c(0.001, 1.5)`. The lower bound should generally be strictly positive
  when `mat_dev` represents positive threshold deviations.

- jitter_thresholds:

  Non-negative numeric value controlling the magnitude of uniform random
  perturbation applied to empirical starting thresholds. Thresholds are
  perturbed by values drawn from
  `runif(n, -jitter_thresholds, jitter_thresholds)`. Defaults to `0.25`.

- tolerance_minus2ll:

  In order for a -2LL to be considered equal to the smallest -2LL, the
  difference must be smaller than `tolerance_minus2ll`.

- verbose:

  Whether or not to print the table of random seeds and -2LLs.

- run:

  Whether or not to use
  [`mxRun`](https://rdrr.io/pkg/OpenMx/man/mxRun.html) to run all
  models, or alternatively, return them as a list.

## Value

If `run = TRUE`, returns the best-fitting OpenMx model. This model has
`attr(x, "tab_ll")`, a table with random seeds and minus 2 log
likelihood values. You can use these seeds to reproduce the models. If
`run = FALSE`, returns a list of OpenMX models.

## Details

Columns of the observed data must be numeric, integer, or ordered
factors. For numeric variables with zero or undefined observed range or
standard deviation, fallback scaling values are constructed to avoid
division by zero or non-finite random-start ranges.

Only free matrices named `A`, `S`, `F`, `M`, `mat_dev`, and `Thresholds`
are supported. The `F` matrix may be present, although this function
does not randomize its elements.

The function uses R's global random-number generator. Use
[`set.seed()`](https://rdrr.io/r/base/Random.html) before calling this
function when reproducible random starting values are required.

For models containing submodels, the function is applied recursively to
each submodel. If observed data are stored in `x@data$observed`, those
data take precedence over the value supplied to `data`.

For continuous variables, the observed minimum, maximum, and standard
deviation are used to determine plausible starting-value scales. For
ordinal variables, values already present in the model matrices are used
where appropriate.

Free paths in the `A` matrix are sampled uniformly after scaling by the
ratio of the standard deviations of the dependent and predictor
variables. The resulting interval for element \\A\_{ij}\\ is
`scale_A * (sd[i] / sd[j])`.

Starting values for the `S` matrix are generated from a random
lower-triangular Cholesky factor \\L\\, and the covariance matrix is
formed as \\S = L L^\top\\. Diagonal elements of \\L\\ are sampled
uniformly between 0.5 and 1 times the corresponding observed standard
deviation, while off-diagonal elements are sampled between -0.25 and
0.25 times that standard deviation. This construction produces a
positive-definite covariance matrix before any restrictions imposed by
the pattern of free parameters in `S` are applied.

Free means in the `M` matrix are sampled uniformly between the observed
minimum and maximum of the corresponding numeric variable.

Free elements of `mat_dev` are sampled uniformly from `scale_mat_dev`.
These elements are intended for parameterizations in which strictly
positive deviations are accumulated to construct ordered thresholds.

For a `Thresholds` matrix, empirical latent-normal thresholds are based
on the cumulative observed category proportions of each ordered
variable, using [`qnorm()`](https://rdrr.io/r/stats/Normal.html).
Uniform random jitter is added to these values and the resulting
thresholds are sorted to preserve their ordering.

After random values have been assigned,
[`omxAssignFirstParameters`](https://rdrr.io/pkg/OpenMx/man/omxAssignFirstParameters.html)
is called so that parameters sharing labels receive consistent starting
values.

## Examples

``` r
if (FALSE) { # \dontrun{
set.seed(1)
df <- iris[1:4]
names(df) <- letters[1:4]
mod <- mx_profiles(data = df, classes = 2, run = FALSE)
res <- random_starts(mod, nstarts = 2)
# Get the table of seeds and minus 2 loglikelihoods:
tab_ll <- attr(res, "tab_ll")
# Seed for smallest -2LL:
seed <- tab_ll$seed[which.min(tab_ll$Minus2LogLikelihood)]
# Set random seed to that seed:
.Random.seed <- seed
# Rerun random starts just for that best seed:
res <- random_starts(mod, nstarts = 1)
} # }
```
