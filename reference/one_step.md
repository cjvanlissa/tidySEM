# One-Step Method for Auxiliary Analysis

Estimates a joint latent class and auxiliary-variable model using
one-step maximum likelihood. The measurement-model parameters, mixing
proportions, and auxiliary-model parameters are estimated
simultaneously.

## Usage

``` r
one_step(x, model = NULL, data, ...)
```

## Arguments

- x:

  A fitted latent class model of class `MxModel`.

- model:

  An auxiliary model specified as an OpenMx RAM model or a character
  string that can be converted using
  [`as_ram()`](https://cjvanlissa.github.io/tidySEM/reference/as_ram.md).
  May be omitted when `data` is a single numeric, integer, or factor
  variable.

- data:

  A `data.frame` containing the auxiliary variables, or a single
  numeric, integer, or factor variable. Observations must be in the same
  order as in the original latent class model.

- ...:

  Additional arguments passed to
  [`OpenMx::mxRun()`](https://rdrr.io/pkg/OpenMx/man/mxRun.html).

## Value

An `MxModel` containing the jointly estimated measurement and auxiliary
models.

## Details

The function starts from a previously fitted latent class model and adds
an auxiliary model to each latent class.

Unlike
[`two_step()`](https://cjvanlissa.github.io/tidySEM/reference/two_step.md),
the measurement-model parameters and mixing proportions are not fixed.
Consequently, auxiliary variables can influence the estimated latent
class solution.

Classification uncertainty is accounted for by integrating over latent
class membership in the joint likelihood.

By default, unlabeled auxiliary parameters are estimated separately
across classes. Explicit parameter labels are preserved, allowing users
to specify equality constraints within and across classes.

The measurement indicators and auxiliary variables are assumed
conditionally independent given latent class membership, unless
additional relationships are explicitly specified.

## References

Bakk, Z., & Kuha, J. (2018). Two-step estimation of models between
latent classes and external variables. *Psychometrika, 83*, 871–892.
[doi:10.1007/s11336-017-9592-7](https://doi.org/10.1007/s11336-017-9592-7)

## Examples

``` r
if (FALSE) { # \dontrun{
dat <- iris[, 1:4]
names(dat) <- c("SL", "SW", "PL", "PW")

set.seed(123)
fit <- mx_profiles(
  data = dat[, c("PL", "PW")],
  classes = 2
)

# Continuous distal outcome
res <- one_step(
  x = fit,
  data = dat$SL
)
table_results(res)

# Equivalent specification using RAM syntax
res <- one_step(
  x = fit,
  model = "SL ~ 1",
  data = dat["SL"]
)

# Ordinal distal outcome
ordinal <- ordered(cut(dat$SL, breaks = 3))
res_ord <- one_step(
  x = fit,
  data = ordinal
)

# Multivariate continuous auxiliary model
res_custom <- one_step(
  x = fit,
  model = "
    SL ~ 1
    SW ~ 1
    SL ~~ SW
  ",
  data = dat[, c("SL", "SW")]
)
} # }
```
