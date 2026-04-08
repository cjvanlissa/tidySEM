# Create Thresholds Based on Deviances

This function creates a list of mxMatrices to specify thresholds for
ordinal variables as a function of strictly positive deviances, see
Details.

## Usage

``` r
mx_deviances(
  vars,
  nThresh = NA,
  free = NULL,
  values = NULL,
  labels = NA,
  lbound = NULL,
  ubound = NULL
)
```

## Arguments

- vars:

  Character vector. Names of variables for which thresholds should be
  specified.

- nThresh:

  Numeric vector. Number of thresholds for each of variable in `vars`.

- free:

  Logical vector, used to fill the `free` matrix which indicates whether
  the deviance will be freely estimated or fixed.

- values:

  Numeric vector of starting values. Default uses
  [`mxNormalQuantiles()`](https://rdrr.io/pkg/OpenMx/man/mxNormalQuantiles.html).

- labels:

  Character vector. Note that constraints specified using these labels
  are applied using
  [`mxConstraint()`](https://rdrr.io/pkg/OpenMx/man/mxConstraint.html),
  because the thresholds are derived quantities, not parameters of the
  model (the deviances are).

- lbound:

  PARAM_DESCRIPTION, Default: NULL

- ubound:

  PARAM_DESCRIPTION, Default: NULL

## Value

A list of `mxMatrix` objects, to be included in an `mxModel`.

## Details

Specifying thresholds as a function of strictly positive deviances is an
efficient way to get models with ordinal indicators to converge, if they
tend to arrive at impossible solutions where the thresholds are not
monotonously increasing (e.g., in mixture models). The parameters of a
model specified this way are deviances (i.e., the differences between
threshold values), which have a lower bound of a small positive number
to ensure that thresholds are always increasing. The thresholds are
computed by adding subsequent deviances.
