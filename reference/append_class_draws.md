# Append Pseudo-class Draws

Generates `m` datasets with random draws of a variable named `class`,
with probability for these draws based on each case's probability of
belonging to that class according to the model in `x`.

## Usage

``` r
append_class_draws(x, data = NULL, m = 20)
```

## Arguments

- x:

  An object for which a method exists, usually a `mx_mixture` model.

- data:

  A data.frame which the `class` variable is appended to. Note that the
  row order must be identical to that of the data used to fit `x`, as
  these data will be augmented with a pseudo-class draw for that
  specific individual.

- m:

  Integer. Number of datasets to generate. Default is 10.

## Value

A data.frame of class `class_draws`.

## Examples

``` r
if(requireNamespace("OpenMx", quietly = TRUE)){
library(OpenMx)
dat <- iris[c(1:5, 50:55, 100:105),1:3]
colnames(dat) <- letters[1:3]
fit <- mx_profiles(data = dat, classes = 2)

append_class_draws(fit, data = iris[c(1:5, 50:55, 100:105), 4, drop = FALSE])
}
#> Running mix2 with 10 parameters
#> Error in model[["expectation"]][["weights"]]: this S4 class is not subsettable
```
