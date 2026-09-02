# Predictive Model Comparison

Compare (non-nested) models estimated on the same data using
model-simulated data.

## Usage

``` r
pmc(x, ..., reps = 20, ci = 0.95, FUN = NULL)

pmc_srmr(x, ..., reps = 20, ci = 0.95)
```

## Arguments

- x:

  An object for which a method exists.

- ...:

  Additional arguments.

- reps:

  Number of data sets to simulate, Default: `100`.

- ci:

  Confidence interval coverage, Default: `0.95`.

- FUN:

  Function used to compare the real data (referred to as `x`) to the
  model-implied data (referred to as `y`). Defaults to `NULL`, which
  uses
  [`chi_sq()`](https://cjvanlissa.github.io/tidySEM/reference/chi_sq.md)
  for models with all ordinal variables, and
  [`srmr()`](https://cjvanlissa.github.io/tidySEM/reference/srmr.md)
  otherwise, treating all variables as continuous.

## Value

A `data.frame`.
