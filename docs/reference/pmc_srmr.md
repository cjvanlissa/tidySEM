# Predictive Model Comparison

Compare (non-nested) models estimated on the same data using
model-simulated data.

## Usage

``` r
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

## Value

A `data.frame`.
