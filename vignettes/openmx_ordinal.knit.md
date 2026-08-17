---
title: "Ordinal Indicators in OpenMx Models"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Ordinal Indicators in OpenMx Models}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---



In models with ordinal indicators,
the response proportions per level of the ordinal factors is often modeled as a "threshold": a value of the standard normal distribution where the probability mass in the left tail corresponds to the response proportion of that level of the ordinal factor.

An alternative way to parametrize ordinal responses is to model response proportions per level of the ordinal factors in terms of threshold differences, which are strictly positive.
This benefits model convergence, especially when for example estimating an LCA model.
The thresholds are then computed by summing successive deviances.

The choice of parameterization does not change the substantive meaning of the thresholds, but it does change the model parameters.
Consequently, it also changes how hypotheses must be written for a Wald test.

This vignette walks through two possible specifications using `as_ram()`:

1. the default threshold parameterization, in which thresholds can be addressed directly through `model.Thresholds`; and
2. `threshold_method = "mx_deviances"`, in which thresholds are constructed from deviation parameters stored in `model.mat_dev`.

The examples use the same simulated data and the same threshold model under both parameterizations. The main objective is to show how to translate a substantive hypothesis about an ordinal threshold into the parameter expression expected by `wald_test()`.

## Simulate ordinal data

We begin by simulating a small data set with three ordinal variables.


``` r
library(tidySEM)
library(OpenMx)
set.seed(1)
df <- data.frame(rbind(
  matrix(rnorm(300, mean = 2), ncol = 3),
  matrix(rnorm(600), ncol = 3)
))
df$X1 <- ordered(cut(df$X1, 2, labels = FALSE))
df$X2 <- ordered(cut(df$X2, 2, labels = FALSE))
df$X3 <- ordered(cut(df$X3, 3, labels = FALSE))
```

`set.seed(1)` makes the simulation reproducible. The first block contributes 100 rows for three variables with mean 2, whereas the second contributes 200 rows with mean 0. After `rbind()`, the data set therefore contains 300 observations on three columns, named `X1`, `X2`, and `X3` by `data.frame()`.
The first two variables are converted to ordered factors with two categories; `X3` is converted to an ordered factor with three categories.
A two-category ordinal variable requires one threshold to separate those whose score is in category 1 from those in category 2.
A three-category ordinal variable requires two thresholds. The model therefore contains four thresholds in total: one for `X1`, one for `X2`, and two for `X3`.

## Threshold syntax in `as_ram()`

The threshold model is specified with the `|` operator.


``` r
mod <- as_ram("
X1 | t1
X2 | t1
X3 | t1
X3 | t2", data = df)
```

Each line has the form `variable | threshold_label`, where threshold labels are always `t[number of the threshold]`.
The model asks for the first threshold (`t1`) of `X1` and `X2`, and both the first (`t1`) and second (`t2`) thresholds of `X3`.

The labels `t1` and `t2` identify threshold positions within a variable. They do **not** imply that thresholds carrying the same label are constrained to be equal across variables. For example, the first threshold of `X1` and the first threshold of `X2` are distinct model parameters unless an equality constraint is imposed explicitly, as in:



``` r
mod_constrain <- as_ram("
X1 | c*t1
X2 | c*t1
X3 | t1
X3 | t2", data = df)
```

With the default threshold method, the parameters are thresholds, which you can verify by running `coef(mod)`.

## Fit the default threshold parameterization

The model is estimated with `run_mx()`.


``` r
res <- run_mx(mod)
```

`run_mx()` fits the OpenMx model created by `as_ram()` and returns the fitted model object. A tidy summary can be obtained with:


``` r
table_results(res)
```

For ordinal indicators, the output includes the estimated threshold parameters along with their standard errors and other model results. This table is usually the most convenient place to inspect the estimates substantively.

For Wald tests, however, it is important to know how those estimates are represented internally. Under the default specification, hypotheses can refer directly to entries of `model.Thresholds`.

### Testing the first threshold of X1

The first Wald test is:


``` r
wald_test(res, "model.Thresholds[1,1] = 0")
```

Here, the first matrix index refers to the threshold position and the second to the observed variable. Thus, `model.Thresholds[1,1]` is the first threshold of the first ordinal indicator, `X1`.

`wald_test()` evaluates the stated equality using the fitted estimate and its estimated sampling covariance. A small p-value indicates that the threshold differs significantly from zero according to the Wald approximation.

### Testing the second threshold of X3

The second test is:


``` r
wald_test(res, "model.Thresholds[2,3] = 1")
```

`X3` is the third observed variable, and its second threshold occupies row 2, column 3 of the threshold matrix.
This direct indexing is the main convenience of the default parameterization: a hypothesis about a threshold can be written as a hypothesis about the corresponding entry of `model.Thresholds`.

## Using `threshold_method = "mx_deviances"`

The same substantive model can be constructed with a different threshold parameterization:


``` r
mod2 <- as_ram("
X1 | t1
X2 | t1
X3 | t1
X3 | t2", data = df, threshold_method = "mx_deviances")
```

The observed variables and requested thresholds are unchanged. What changes is the way the thresholds are parameterized inside OpenMx.
Verify that these are now defined as deviances by running `coef(mod2)`.

With `threshold_method = "mx_deviances"`, the model uses a matrix of deviation parameters, `model.mat_dev`. Thresholds are then constructed by cumulatively summing these deviations. This cumulative representation is useful for encoding the ordered structure of thresholds. It also means that the second and later thresholds are generally **functions of multiple model parameters**, rather than single free parameters.

Fit and summarize this model in the same way:


``` r
res2 <- run_mx(mod2)
table_results(res2)
```

The substantive threshold estimates reported by `table_results()` are already reconstructed from the deviances, and can be interpreted in the same way as before. 
The difference becomes important when referring to the free parameters directly.

For a variable with only one threshold, the first deviation is equal to the threshold. For a variable with multiple thresholds, later thresholds must be reconstructed by adding the relevant deviations.

### Testing the first threshold of X1

Because the first threshold is equal to the first deviation, the corresponding test for deviances is:


``` r
wald_test(res2, "model.mat_dev[1,1] = 0")
```

Verify that the test result is identical to:


``` r
wald_test(res, "model.Thresholds[1,1] = 0")
```


### Testing the second threshold of X3

The difference between the parameterizations is clearest for the second threshold of `X3`.
It must be written as a constraint on the **sum** of the first two deviations:


``` r
wald_test(res2, "model.mat_dev[1,3]+model.mat_dev[2,3] = 1")
```

This is the deviation-parameter counterpart of:


``` r
wald_test(res, "model.Thresholds[2,3] = 1")
```

Do not make the mistake of testing only `model.mat_dev[2,3] = 1`. That would test whether the *second deviation* equals 1, not whether the *second threshold* equals 1. Those are different hypotheses because the second threshold is cumulative.

## Why the two Wald tests should agree

The default and deviation specifications are alternative parameterizations of the same thresholds. When both models represent the same statistical model and converge to the same solution, a Wald test formulated for the same substantive threshold should lead to equivalent conclusions, apart from small numerical differences.

The expressions passed to `wald_test()` differ because a Wald test operates on the parameterization of the fitted model. Reparameterizing a threshold as a sum of deviations therefore requires re-expressing the null hypothesis in terms of that sum.

This distinction is especially important for second and later thresholds. In the direct specification, the target threshold is a single entry of `model.Thresholds`; in the deviation specification, the same target is a linear combination of entries in `model.mat_dev`.
