# Auxiliary Analyses for Latent Classes

Latent class analysis (LCA) and latent profile analysis (LPA) identify
unobserved groups using a set of observed indicator variables. Often,
researchers also have research questions about variables that were not
used to define the classes. These are called *auxiliary variables*. A
distal outcome is an auxiliary variable considered as an outcome; other
auxiliary variables may be predictors or covariates.

This vignette introduces three approaches in `tidySEM`: **BCH**,
**two-step maximum likelihood**, and **pseudo-class draws**. We
illustrate class-specific means, category probabilities, regressions,
hypothesis tests, and predictors of class membership. For an
introduction to estimating the measurement model, see [Introduction to
Latent Class
Analysis](https://cjvanlissa.github.io/tidySEM/articles/lca_introduction.html).

## The Naive Approach

An often-heard question is: Why can we not simply assign every
participant to their most likely class, and use class membership as a
variable in auxiliary analyses?

The reason is that latent class membership is uncertain, and based on
posterior class probabilities. A participant with posterior class
probabilities $`(.05,.93,.02)`$ is most likely to belong to class 2,
with high certainty. However, a participant with posterior class
probabilities $`(.45,.35,.20)`$ is most likely to belong to class 1, but
there is a lot of uncertainty with this assignment.

If we treat class assignments as an observed variable, this uncertainty
propagates into auxiliary analyses and biases the results and their
standard errors. The three approaches below address this uncertainty in
different ways.

## Example: Caregiver profiles

We use the `zegwaard_carecompass` data included in `tidySEM`. As
explained in the vignette on [Confirmatory
LCA](https://cjvanlissa.github.io/tidySEM/articles/lca_confirmatory.html),
the measurement model uses four caregiver-experience indicators. For the
present analyses, auxiliary variables concern the caregiver and the
person receiving care. One requirement of all analyses presented here is
that the auxiliary data must have the same number of rows in **exactly
the same order** as the measurement data.

``` r

library(tidySEM)
library(OpenMx)
#> To take full advantage of multiple cores, use:
#>   mxOption(key='Number of Threads', value=parallel::detectCores()) #now
#>   Sys.setenv(OMP_NUM_THREADS=parallel::detectCores()) #before library(OpenMx)
df <- zegwaard_carecompass
indicators <- c("burdened", "trapped", "negaffect", "loneliness")
measurement_data <- df[, indicators]
```

For the examples in this vignette, we will just fit a simple two-class
model:

``` r

set.seed(297)
lpa_model <- mx_profiles(measurement_data, classes = 2)
saveRDS(lpa_model, cache_file("measurement"))
```

``` r

lpa_model <- read_cached("measurement")
table_fit(lpa_model)
```

### Classification uncertainty

``` r

cp <- class_prob(lpa_model)
head(cp$individual)
cp$mostlikely.class
```

This table shows the average probability that individuals belong to each
true class (rows), and their probability of being assigned to each class
(columns). The diagonal of this table thus represents the probability
that observations in each class will be correctly classified. These
probabilities are high, but not perfect. As also indicated in the fit
table by a low Entropy criterion, these classes are not perfectly
separated. Therefore, do not treat assigned class as an observed
variable; these classification errors would influence downstream
auxiliary analyses.

## BCH: class differences in distal outcomes

The Bolck–Croon–Hagenaars (BCH) procedure first estimates the
measurement model, then estimates an auxiliary multi-group model,
accounting for classification uncertainty by weighting case
contributions to each class’ group. In `tidySEM`,
[`BCH()`](https://cjvanlissa.github.io/tidySEM/reference/BCH.md) can
compare classes on a single numeric or ordered outcome, or an entire
auxiliary structural equation model.

### Continuous distal outcome

Here we compare caregiver distance across classes. The default
numeric-outcome specification estimates class-specific means and
variances.

``` r

bch_mean <- BCH(lpa_model, data = df$distance)
table_results(bch_mean, columns = NULL)
```

The estimates describe class-specific distal means. They do **not** by
themselves test whether the means differ. For an omnibus and pairwise
comparison, use
[`lr_test()`](https://cjvanlissa.github.io/tidySEM/reference/lr_test.md);
compare the means (`M`) rather than the entire auxiliary model.

``` r

lr_test(bch_mean, compare = "M")
```

One crucial limitation of the BCH approach is that its standard errors
are not valid, so be careful drawing inferences about parameters in
these distal models.

### Binary or ordinal distal outcome

For a binary distal outcome,
[`BCH()`](https://cjvanlissa.github.io/tidySEM/reference/BCH.md)
estimates class-specific thresholds. The function
[`table_prob()`](https://cjvanlissa.github.io/tidySEM/reference/table_prob.md)
represents these in the more interpretable response probability scale:

``` r

sex <- ordered(df$sexpatient, levels = c("male", "female"))
bch_sex <- BCH(lpa_model, data = sex)
table_prob(bch_sex)
```

### A class-specific regression model

The `model` argument accepts lavaan-style syntax. This example
investigates whether the classes differ in the effect of distance on
frequency of visits. If we request the `"matrix"` column of the output,
you can see that the regression coefficients live in the `"A"` matrix
(asymmetric effects, in other words: regressions). A likelihood ratio
test of the `"A"` matrix thus investigates whether the regression
coefficient differs significantly between the two classes:

``` r

df_aux <- df[, c("freqvisit", "distance"), drop = FALSE]
df_aux$freqvisit <- as.numeric(df_aux$freqvisit)
bch_reg <- BCH(lpa_model,
               model = "freqvisit ~ distance",
               data = df_aux)
table_results(bch_reg, columns = c("label", "est_sig", "se", "pval", "confint", "group", "matrix"))
lr_test(bch_reg, compare = "A")
```

## Two-step maximum likelihood

A different approach to auxiliary analysis is the
[`two_step()`](https://cjvanlissa.github.io/tidySEM/reference/two_step.md)
procedure. It first estimates the latent class model, then fixes its
parameters and adds class-specific auxiliary models (whose parameters
are free). This prevents the auxiliary model from influencing the class
solution. Unlike BCH, the auxiliary outcomes enter the likelihood
directly; posterior class probabilities can therefore change even though
measurement parameters stay fixed.

The interface of this function is similar to
[`BCH()`](https://cjvanlissa.github.io/tidySEM/reference/BCH.md):

``` r

two_mean <- two_step(lpa_model, data = df$distance)
table_results(two_mean, columns = NULL)
```

Ordinary standard errors for the auxiliary model are also not valid, but
`tidySEM` replaces them with corrected two-step standard errors. These
are available in the attribute `attr(model, "se_robust")`, and
[`table_results()`](https://cjvanlissa.github.io/tidySEM/reference/table_results.md)
shows them by default.

``` r

attr(two_mean, "se_robust")
```

For a custom auxiliary RAM model, the syntax can include multiple
outcomes and regressions:

``` r

two_reg <- two_step(
  lpa_model,
  model = "freqvisit ~ distance",
  data = df_aux
)
table_results(two_reg)
```

## Pseudo-class draws and pooling

The pseudo-class approach accounts for classification uncertainty by
repeatedly drawing one class for each observation according to its
posterior probabilities. Thus, a person with posterior probabilities
$`(.05,.93,.02)`$ will be assigned to class 2 in *most* replications,
but occasionally to class 1 or 3. Conversely, a participant with
posterior class probabilities $`(.45,.35,.20)`$ will be assigned to each
of the three classes in many replications.

The auxiliary model is fitted in each completed dataset, and the results
are pooled. This propagates classification uncertainty through the
draws. Increasing the number of draws reduces Monte Carlo error.

### Compare distal means

Here we use [`lm()`](https://rdrr.io/r/stats/lm.html) with a
factor-coded class variable and no intercept, yielding one coefficient
per class. The `model` expression is evaluated in each pseudo-class
dataset.

``` r

set.seed(2027)
pseudo_mean <- pseudo_class(
  x = lpa_model,
  model = lm(distance ~ I(factor(class)) - 1, data = data),
  data = df[, "distance", drop = FALSE],
  m = 20
)
pseudo_mean
```

### Use RAM syntax

The
[`pseudo_class()`](https://cjvanlissa.github.io/tidySEM/reference/pseudo_class.md)
approach also accepts auxiliary models expressed using `lavaan` syntax.
If the model does not explicitly reference `class`, the function treats
class as a grouping variable.

``` r

set.seed(2028)
pseudo_ram <- pseudo_class(
  x = lpa_model,
  model = "distance ~ 1",
  data = df[, "distance", drop = FALSE],
  m = 20
)
pseudo_ram
```

## Predicting class membership

So far, we have treated class membership as an explanatory grouping for
auxiliary outcomes. Sometimes the research question instead asks which
external variables predict class membership. These are not the same
estimands: differences in a distal mean do not supply the coefficients
of a multinomial regression adjusted for other predictors.

For example, a multinomial model can be fitted across pseudo-class draws
using [`nnet::multinom()`](https://rdrr.io/pkg/nnet/man/multinom.html):

``` r

set.seed(2030)
pseudo_pred <- pseudo_class(
  x = lpa_model,
  model = function(data) {
    nnet::multinom(class ~ distance + freqvisit,
                   data = data, trace = FALSE)
  },
  data = df_aux,
  m = 20
)
pseudo_pred
```

The coefficients describe conditional associations with class membership
relative to a reference class.

## Reporting auxiliary analyses

A report should identify the measurement indicators, selected number of
classes, classification quality, auxiliary-variable role, estimation
method, and treatment of uncertainty. Report class-specific estimates
and their units, as well as uncertainty intervals. Mention what kind of
standard errors you used, and whether they are valid. For comparisons
across classes via likelihood ratio test, state which parameters were
constrained.
