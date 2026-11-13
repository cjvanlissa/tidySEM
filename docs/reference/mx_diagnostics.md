# Diagnose an `OpenMx` Model

Evaluates convergence and numerical diagnostics for an `OpenMx` model.

## Usage

``` r
mx_diagnostics(
  model,
  check = c("convergence", "fit", "parameters", "standard_errors", "boundaries",
    "hessian"),
  tol = 1e-06
)
```

## Arguments

- model:

  A fitted `mxModel` object.

- check:

  Character vector specifying which diagnostics to perform out of
  `c("convergence", "fit", "parameters", "standard_errors", "boundaries", "hessian")`.
  Default: all diagnostics.

- tol:

  Numeric atomic vector, indicating the tolerance for checking whether
  estimates are near their boundaries.

## Value

A list containing:

- checks_pass:

  Logical indicating whether all requested checks pass.

- convergence_ok:

  Logical indicating acceptable optimizer convergence.

- fit_ok:

  Logical indicating a finite model fit value.

- parameters_ok:

  Logical indicating finite parameter estimates.

- boundaries_ok:

  Logical indicating whether parameter boundary checks pass.

- standard_errors_ok:

  Logical indicating whether standard errors are available and valid.

- hessian_ok:

  Logical indicating whether the Hessian satisfies the implemented
  diagnostic criteria.

- warnings:

  Character vector describing detected problems.

## Details

Convergence diagnostics do not guarantee that the global maximum of the
likelihood has been found. Boundary estimates and non-positive-definite
Hessians may indicate weak identification. Boundary estimates do not
necessarily imply optimization failure.

## Examples

``` r
if (FALSE) { # \dontrun{
set.seed(123)
dat <- iris[1:2]
names(dat) <- c("x", "y")
res <- run_mx(as_ram("y~x", data = dat))
mx_diagnostics(res)
} # }
```
