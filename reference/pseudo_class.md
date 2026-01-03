# Estimate an Auxiliary Model using the Pseudo-Class Method

Estimate an auxiliary model based on multiple datasets, randomly drawing
latent class values based on the estimated probability of belonging to
each class. The pseudo class variable is treated as an observed variable
within each dataset, and results are pooled across datasets to account
for classification uncertainty.

## Usage

``` r
pseudo_class(x, model, df_complete = NULL, ...)

# S3 method for class 'MxModel'
pseudo_class(x, model, df_complete = NULL, data = NULL, m = 20, ...)
```

## Arguments

- x:

  An object for which a method exists, typically either a fitted
  `mx_mixture` model or `class_draws` object.

- model:

  Either an expression to execute on every generated dataset, or a
  function that performs the analysis on every generated dataset, or a
  character that can be interpreted as a structural equation model using
  [`as_ram`](https://cjvanlissa.github.io/tidySEM/reference/as_ram.md).
  This `model` can explicitly refer to `data`.

- df_complete:

  Integer. Degrees of freedom of the complete-data analysis.

- ...:

  Additional arguments passed to other functions.

- data:

  A data.frame on which the auxiliary model can be evaluated. Note that
  the row order must be identical to that of the data used to fit `x`,
  as these data will be augmented with a pseudo-class draw for that
  specific individual.

- m:

  Integer. Number of datasets to generate. Default is 20.

## Value

An object of class
[`data.frame`](https://rdrr.io/r/base/data.frame.html) containing pooled
estimates.

## References

Pseudo-class technique: Wang C-P, Brown CH, Bandeen-Roche K (2005).
Residual Diagnostics for Growth Mixture Models: Examining the Impact of
a Preventive Intervention on Multiple Trajectories of Aggressive
Behavior. Journal of the American Statistical Association
100(3):1054-1076.
[doi:10.1198/016214505000000501](https://doi.org/10.1198/016214505000000501)

Pooling results across samples: Van Buuren, S. 2018. Flexible Imputation
of Missing Data. Second Edition. Boca Raton, FL: Chapman & Hall/CRC.
[doi:10.1201/9780429492259](https://doi.org/10.1201/9780429492259)

## Examples

``` r
if(requireNamespace("OpenMx", quietly = TRUE)){
library(OpenMx)
set.seed(2)
dat <- iris[c(1:5, 50:55, 100:105), 1:4]
colnames(dat) <- c("SL", "SW", "PL", "PW")
fit <- suppressWarnings(mx_profiles(data = dat, classes = 3))

pct_mx <- pseudo_class(x = fit,
                       model = "SL ~ class",
                       data = dat,
                       m = 2)

pct_lm <- pseudo_class(x = fit,
             model = lm( SL ~ class, data = data),
             data = dat,
             m = 2)


pcte <- pseudo_class(x = fit,
                     model = lm(SL ~ class, data = data),
                     data = dat,
                     m = 2)

pct_func <- pseudo_class(x = fit,
                         model = function(data){lm(SL ~ class, data = data)},
                         data = dat,
                         m = 2)


}
#> Running mix3 with 18 parameters
#> The degrees of freedom are assumed to be equal to the total number of observations used in the model ( 17 ) minus the number of parameters estimated ( 5 ). This may not be correct. If necessary, provide a better value via the 'df_complete' argument
```
