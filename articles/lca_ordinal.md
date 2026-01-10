# Latent Class Analysis for Ordinal Indicators

This is an example of exploratory LCA with ordinal indicators using
`tidySEM`, as explained in Van Lissa, C. J., Garnier-Villarreal, M., &
Anadria, D. (2023). *Recommended Practices in Latent Class Analysis
using the Open-Source R-Package tidySEM.* Structural Equation Modeling.
<https://doi.org/10.1080/10705511.2023.2250920>. The present example
uses synthetic data based on a study by Maene and colleagues. In a
convenience sample of Flemish (Belgian) high-school students with a
migration background, this study set out to identify distinct classes
based on ordinal indicators of National, Regional, and Heritage
identity. Sample size was not justified.

The approach to class enumeration was semi-theory driven: The
researchers expected to find profiles that were distinct on all three
types of identity (national, regional, and heritage) - but the exact
number of classes was not pre-specified (hypothesis 1).

Hypothesis 2 stated that adolescents who are nationally integrated would
have lower depressive feelings than students from students with other
combinations of identifications (hypothesis 2). Hypothesis 3 was that,
for assimilated and separated adolescents, there would not be a
significant effect of perceived teacher discrimination on depressive
symptoms.

Use the command
[`?tidySEM::maene_identity`](https://cjvanlissa.github.io/tidySEM/reference/maene_identity.md)
to view the data documentation.

## Loading the Data

To load the data, simply attach the `tidySEM` package. For convenience,
we assign the indicator data to an object called `df`:

``` r
# Load required packages
library(tidySEM)
library(ggplot2)
# Load data
df <- maene_identity[1:5]
```

## Examining the Data

We use
[`tidySEM::descriptives()`](https://cjvanlissa.github.io/tidySEM/reference/descriptives.md)
to describe the data numerically. Because all items are categorical, we
remove columns for continuous data to de-clutter the table:

``` r
desc <- tidySEM::descriptives(df)
desc <- desc[, c("name", "type", "n", "missing", "unique", "mode",
    "mode_value", "v")]
desc
```

Additionally, we can plot the data. The `ggplot2` function
[`geom_bar()`](https://ggplot2.tidyverse.org/reference/geom_bar.html) is
useful for ordinal data:

``` r
df_plot <- df
names(df_plot) <- paste0("Value.", names(df_plot))
df_plot <- reshape(df_plot, varying = names(df_plot), direction = "long")
ggplot(df_plot, aes(x = Value)) + geom_bar() + facet_wrap(~time,
    scales = "free") + theme_bw()
```

As we can see, the
[`descriptives()`](https://cjvanlissa.github.io/tidySEM/reference/descriptives.md)
table provides invaluable information about the measurement level of the
indicators, which is used to specify the model correctly. If these data
had not been coded as ordinal factors, these descriptive statistics
would have revealed that each variable has only 5-10 unique values. The
proportion of missing values is reported in the `"missing"` column. If
any variables had missing values, we would report an MCAR test with
[`mice::mcar()`](https://amices.org/mice/reference/mcar.html) and
explain that missing data are accounted for using FIML. In our example,
we see that there are no missing values, hence we proceed with our
analysis. Note that the ethic identification variables are very
right-skewed and some response categories have near-zero frequencies;
this can cause problems in model specification and convergence. One
potential solution would be to merge small adjacent categories. We will
not do this here, however.

## Conducting Latent Class Analysis

Before we fit a series of LCA models, we set a random seed using
[`set.seed()`](https://rdrr.io/r/base/Random.html). This is important
because there is some inherent randomness in the estimation procedure,
and using a seed ensures that we (and others) can exactly reproduce the
results.

Next, we fit the LCA models. As all variables are ordinal, we can use
the convenience function
[`tidySEM::mx_lca()`](https://cjvanlissa.github.io/tidySEM/reference/mx_lca.md),
which is a wrapper for the generic function
[`mx_mixture()`](https://cjvanlissa.github.io/tidySEM/reference/mx_mixture.md)
optimized for LCA with ordinal data. Any mixture model can be specified
through
[`mx_mixture()`](https://cjvanlissa.github.io/tidySEM/reference/mx_mixture.md).
At the time of writing, there are two other wrapper functions for
special cases:
[`mx_profiles()`](https://cjvanlissa.github.io/tidySEM/reference/mx_profiles.md),
for latent profile analysis, and
[`mx_growth_mixture()`](https://cjvanlissa.github.io/tidySEM/reference/mx_growth_mixture.md),
for latent growth analysis and growth mixture models. All of these
functions have arguments `data` and number of `classes`. All variables
in `data` are included in the analysis, so relevant variables must be
selected first.

We here consider 1-6 class models, but note that this may be overfit, as
some of the indicators have only 5 response categories.

``` r
set.seed(123)
res <- mx_lca(data = df, classes = 1:6)
```

## Class Enumeration

In class enumeration, we want to compare a sequence of LCA models fitted
to the data. First, note that all models converged without issues. If
this had not been the case, it is possible to aid convergence using
[`mxTryHardOrdinal()`](https://rdrr.io/pkg/OpenMx/man/mxTryHard.html),
which expands the search for optimal parameter values for models with
ordinal indicators. It is part of the family of functions based on
[`mxTryHard()`](https://rdrr.io/pkg/OpenMx/man/mxTryHard.html).

Next, we create a model fit table using
[`table_fit()`](https://cjvanlissa.github.io/tidySEM/reference/table_fit.md)
and retain relevant columns. We also determine whether any models can be
disqualified.

``` r
fit <- table_fit(res)
fit[, c("Name", "LL", "n", "Parameters", "BIC", "Entropy", "prob_min",
    "n_min", "np_ratio", "np_local")]
```

Note that both the global and local ratio of cases to parameters is low;
for models of 3 or more classes, there are just a few observations per
parameter in the smallest class (see `np_local`). This is a good reason
to disqualify those classes. We will eliminate classes 4-6 on those
criteria, but in real data applications, the 3-class solution might also
be disqualified.

In terms of classification diagnostics, note that the entropy and the
minimum classification probabilities are high for all models. This
indicates that all classes are distinct.

## Class Enumeration

### Using BIC

Based on the BIC, we would prefer the 2-class model. This decision is
also validated by a scree plot of the BIC, obtained by running
`plot(fit)`.

### Theoretical considerations

Despite the BIC indicating that the 2-class model is best, upon
examining the 2-class and 3-class solution, it was noted that
theoretically crucial distinctions between ethnic, regional (Flemish),
and national (Belgian) identity were not captured by the 2-class
solution but were captured by the 3-class solution. Based on this
theoretical consideration, the analysis proceeded with the 3-class
solution.

``` r
res_final <- res[[3]]
```

## Interpreting the Final Class Solution

The 3-class model yielded classes of reasonable size; the largest class
comprised 33%, and the smallest comprised 16% of cases. The entropy was
high, $S = .93$, indicating good class separability. Furthermore, the
posterior classification probability ranged from
$\lbrack.94,.99\rbrack$, which means that all classes had low
classification error. We can produce a table of results using
`table_results(res_final)`. However, the results are thresholds,
indicating quantiles of a standardized normal distribution. These may be
difficult to interpret. Therefore, we ask for the results in probability
scale:

``` r
tab <- table_prob(res_final)
reshape(tab, direction = "wide", v.names = "Probability", timevar = "group",
    idvar = c("Variable", "Category"))
```

The results can also be interpreted by plotting the response
probabilities:

``` r
plot_prob(res_final, bw = TRUE)
```

Note that the first class (33%) has relatively high identification with
the ethnic indicators and relatively low identification with Belgian and
Flemish identity. The second class (16%) has moderate identification
with Belgian and Flemish identity, and relatively low identification
with ethnic identity. Finally, the third class (50%) has high
identification with all identities.

Based on the probability plot, we can label class 1 as *ethic
identifiers*, class 2 as *low identifiers*, and class 3 as *high
identifiers*.

## Auxiliary Analyses

To address the remaining two hypotheses, we will perform auxiliary
analyses. Hypothesis 2 stated that adolescents who are nationally
integrated would have lower depressive feelings than students from
students with other combinations of identifications (hypothesis 2).

To test this hypothesis, we can call the BCH function and supply the
auxiliary variable depression to the `data` argument, omitting the
`model` argument. Below, we estimate an auxiliary model to compare
depressive symptoms across classes:

``` r
aux_dep <- BCH(res_final, data = maene_identity$depression)
```

To obtain an omnibus likelihood ratio test of the significance of
depression differences across classes, as well as pairwise comparisons
between classes, use `lr_test(aux_dep)`. The results indicate that there
are no significant differences in depression across classes,
$\Delta LL(5) = 4.32,p = .50$.

Hypothesis 3 was that, for assimilated and separated adolescents, there
would not be a significant effect of perceived teacher discrimination on
depressive symptoms. To test this hypothesis, we will compare the
regression coefficient of discrimination on depressive symptoms across
classes.

``` r
df_aux <- maene_identity[, c("vict_teacher", "depression")]
# Dummy-code vict_teacher
df_aux$vict_teacher <- (as.integer(df_aux$vict_teacher) - 1)
aux_model <- BCH(res_final, model = "depression ~ vict_teacher",
    data = df_aux)
```

To view the coefficients of this model, we can use either
`coef(aux_model)` or `table_results(aux_model, columns = NULL)`. As
evident from the results table, the coefficients labeled `class1.A[1,2]`
are the regression coefficients.

There are two ways to test the difference in regression coefficients
across classes: using `lr_test(aux_model, compare = "A")`, to compare
the ‘A matrix’ (regression coefficients) across classes, or
`wald_test(aux_model, "class1.A[1,2]=class2.A[1,2]&class1.A[1,2]=class3.A[1,2]")`.
The results indicate that there are no significant differences in the
regression coefficients across classes, $\chi^{2}(2) = 1.16,p = .56$.
