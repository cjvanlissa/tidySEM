# Estimate an Auxiliary Model Using Two-Step Maximum Likelihood

Estimates an auxiliary model based on a previously fitted latent class
model, using a two-step maximum likelihood procedure.

## Usage

``` r
two_step(x, model = NULL, data, ...)
```

## Arguments

- x:

  A fitted latent class model of class `MxModel`.

- model:

  An auxiliary model specified as an OpenMx RAM model or a character
  string that can be converted to a RAM model using
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

An `MxModel` containing the original latent class model with fixed
measurement parameters and freely estimated auxiliary model parameters.

## Details

In the first step, the latent class model is estimated using the
measurement indicators. In the second step, its parameter estimates
(including the mixing proportions) are fixed, and the auxiliary model is
added to each latent class. Only the parameters of the auxiliary model
are freely estimated.

Unlike the one-step approach, the auxiliary variables cannot influence
the measurement model parameters. However, auxiliary variables can
influence posterior class membership probabilities because they
contribute to the joint likelihood. Classification uncertainty is
accounted for by integrating over latent class membership rather than
assigning observations to their most likely class.

Note that conventional standard errors obtained from this procedure
treat the first-step parameter estimates as known and therefore do not
account for uncertainty in the estimation of the latent class model.
Corrected standard errors can be obtained using an appropriate two-step
sandwich estimator or a bootstrap that repeats both steps.

The auxiliary model is incorporated into each class-specific RAM model.
By default, its parameters are freely estimated across latent classes,
allowing class-specific means, variances, and regression coefficients.

The measurement indicators and auxiliary variables are assumed
conditionally independent given latent class membership, unless
additional relationships are explicitly specified.

## References

Bakk, Z., & Kuha, J. (2018). Two-step estimation of models between
latent classes and external variables. *Psychometrika, 83*, 871–892.
[doi:10.1007/s11336-017-9592-7](https://doi.org/10.1007/s11336-017-9592-7)

## See also

[`BCH()`](https://cjvanlissa.github.io/tidySEM/reference/BCH.md),
[`pseudo_class()`](https://cjvanlissa.github.io/tidySEM/reference/pseudo_class.md)

## Examples

``` r
if (FALSE) { # \dontrun{
library(OpenMx)

# Estimate a latent profile model
dat <- iris[, 1:4]
names(dat) <- c("SL", "SW", "PL", "PW")

set.seed(123)
fit <- mx_profiles(
  data = dat[, c("PL", "PW")],
  classes = 2
)

# Continuous distal outcome
res <- two_step(
  x = fit,
  data = dat$SL
)
table_results(res)

# Equivalent specification using RAM syntax
res <- two_step(
  x = fit,
  model = "SL ~ 1",
  data = dat["SL"]
)

# Ordinal distal outcome
ordinal <- ordered(
  cut(dat$SL, breaks = 3)
)

res_ord <- two_step(
  x = fit,
  data = ordinal
)

# Custom auxiliary model with two continuous variables
res_custom <- two_step(
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
