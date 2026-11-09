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
#>   Minus2LogLikelihood   n Parameters observedStatistics  df RMSEASquared
#> 1                4063 513         13               2030 500            0
#>   RMSEANull   modelName  AIC  BIC saBIC Classes Entropy prob_min prob_max n_min
#> 1      0.05 equal var 2 4089 4144  4103       2    0.74     0.91     0.93  0.42
#>   n_max    LL
#> 1  0.58 -2031
```

### Classification uncertainty

``` r

cp <- class_prob(lpa_model)
head(cp$individual)
cp$mostlikely.class
```

    #>      class1 class2 predicted
    #> [1,] "0.69" "0.31" "1.00"   
    #> [2,] "0.00" "1.00" "2.00"   
    #> [3,] "0.77" "0.23" "1.00"   
    #> [4,] "0.05" "0.95" "2.00"   
    #> [5,] "0.16" "0.84" "2.00"   
    #> [6,] "1.00" "0.00" "1.00"
    #>           assigned.1 assigned.2
    #> avgprob.1 "0.93"     "0.07"    
    #> avgprob.2 "0.09"     "0.91"

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

    #>              label lhs op rhs     est     se pval            confint    est_sig
    #> 1     Means.y.aux1   y ~1   1  153.67   2.88 0.00   [148.02, 159.32]  153.67***
    #> 2 Variances.y.aux1   y ~~   y 2496.42 203.70 0.00 [2097.16, 2895.67] 2496.42***
    #> 3     Means.y.aux2   y ~1   1  160.66   3.82 0.00   [153.17, 168.15]  160.66***
    #> 4 Variances.y.aux2   y ~~   y 3076.16 299.84 0.00 [2488.48, 3663.83] 3076.16***
    #>   pval_std  confint_std free        name lboundmet uboundmet std_est std_se col
    #> 1     0.00 [2.80, 3.35] TRUE aux1.M[1,1]     FALSE     FALSE    3.08   0.14   y
    #> 2     0.00 [1.00, 1.00] TRUE aux1.S[1,1]     FALSE     FALSE    1.00   0.00   y
    #> 3     0.00 [2.59, 3.20] TRUE aux2.M[1,1]     FALSE     FALSE    2.90   0.16   y
    #> 4     0.00 [1.00, 1.00] TRUE aux2.S[1,1]     FALSE     FALSE    1.00   0.00   y
    #>   matrix openmx_label row group ci_type  Category
    #> 1      M  aux1.M[1,1]   1  aux1    Wald     Means
    #> 2      S  aux1.S[1,1]   y  aux1    Wald Variances
    #> 3      M  aux2.M[1,1]   1  aux2    Wald     Means
    #> 4      S  aux2.S[1,1]   y  aux2    Wald Variances

The estimates describe class-specific distal means. They do **not** by
themselves test whether the means differ. For an omnibus and pairwise
comparison, use
[`lr_test()`](https://cjvanlissa.github.io/tidySEM/reference/lr_test.md);
compare the means (`M`) rather than the entire auxiliary model.

``` r

lr_test(bch_mean, compare = "M")
```

    #> BCH test for equality across classes
    #> 
    #> Overall likelihood ratio test:
    #>  LL_baseline LL_restricted LL_dif df    p
    #>         5491          5494    2.1  1 0.14
    #> 
    #> Pairwise comparisons using likelihood ratio tests:
    #>  Model1 Model2 LL_baseline LL_restricted LL_dif df    p
    #>    aux1   aux2        5491          5494    2.1  1 0.14

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

    #>   Variable Category Probability group
    #> 1        y        1        0.51  aux1
    #> 2        y        2        0.49  aux1
    #> 3        y        1        0.46  aux2
    #> 4        y        2        0.54  aux2

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

    #>                                     label       lhs op       rhs     est     se
    #> 1  Regressions.freqvisit.ON.distance.aux1 freqvisit  ~  distance   -0.00   0.00
    #> 2                    Means.freqvisit.aux1 freqvisit ~1         1    3.88   0.20
    #> 3                     Means.distance.aux1  distance ~1         1  153.67   2.88
    #> 4                Variances.freqvisit.aux1 freqvisit ~~ freqvisit    1.13   0.09
    #> 5                 Variances.distance.aux1  distance ~~  distance 2496.41 203.70
    #> 6  Regressions.freqvisit.ON.distance.aux2 freqvisit  ~  distance   -0.00   0.00
    #> 7                    Means.freqvisit.aux2 freqvisit ~1         1    3.85   0.22
    #> 8                     Means.distance.aux2  distance ~1         1  160.68   3.82
    #> 9                Variances.freqvisit.aux2 freqvisit ~~ freqvisit    1.05   0.10
    #> 10                Variances.distance.aux2  distance ~~  distance 3076.19 299.82
    #>    pval            confint    est_sig pval_std   confint_std free        name
    #> 1  0.74      [-0.00, 0.00]      -0.00     0.74 [-0.13, 0.09] TRUE aux1.A[1,2]
    #> 2  0.00       [3.50, 4.27]    3.88***     0.00  [3.19, 4.11] TRUE aux1.M[1,1]
    #> 3  0.00   [148.02, 159.32]  153.67***     0.00  [2.80, 3.35] TRUE aux1.M[1,2]
    #> 4  0.00       [0.95, 1.31]    1.13***     0.00  [1.00, 1.00] TRUE aux1.S[1,1]
    #> 5  0.00 [2097.16, 2895.67] 2496.41***     0.00  [1.00, 1.00] TRUE aux1.S[2,2]
    #> 6  0.49      [-0.00, 0.00]      -0.00     0.49 [-0.18, 0.09] TRUE aux2.A[1,2]
    #> 7  0.00       [3.43, 4.28]    3.85***     0.00  [3.22, 4.28] TRUE aux2.M[1,1]
    #> 8  0.00   [153.18, 168.17]  160.68***     0.00  [2.59, 3.20] TRUE aux2.M[1,2]
    #> 9  0.00       [0.85, 1.25]    1.05***     0.00  [0.98, 1.01] TRUE aux2.S[1,1]
    #> 10 0.00 [2488.55, 3663.83] 3076.19***     0.00  [1.00, 1.00] TRUE aux2.S[2,2]
    #>    lboundmet uboundmet std_est std_se       col matrix openmx_label       row
    #> 1      FALSE     FALSE   -0.02   0.06  distance      A  aux1.A[1,2] freqvisit
    #> 2      FALSE     FALSE    3.65   0.24 freqvisit      M  aux1.M[1,1]         1
    #> 3      FALSE     FALSE    3.08   0.14  distance      M  aux1.M[1,2]         1
    #> 4      FALSE     FALSE    1.00   0.00 freqvisit      S  aux1.S[1,1] freqvisit
    #> 5      FALSE     FALSE    1.00   0.00  distance      S  aux1.S[2,2]  distance
    #> 6      FALSE     FALSE   -0.05   0.07  distance      A  aux2.A[1,2] freqvisit
    #> 7      FALSE     FALSE    3.75   0.27 freqvisit      M  aux2.M[1,1]         1
    #> 8      FALSE     FALSE    2.90   0.16  distance      M  aux2.M[1,2]         1
    #> 9      FALSE     FALSE    1.00   0.01 freqvisit      S  aux2.S[1,1] freqvisit
    #> 10     FALSE     FALSE    1.00   0.00  distance      S  aux2.S[2,2]  distance
    #>    group ci_type    Category
    #> 1   aux1    Wald Regressions
    #> 2   aux1    Wald       Means
    #> 3   aux1    Wald       Means
    #> 4   aux1    Wald   Variances
    #> 5   aux1    Wald   Variances
    #> 6   aux2    Wald Regressions
    #> 7   aux2    Wald       Means
    #> 8   aux2    Wald       Means
    #> 9   aux2    Wald   Variances
    #> 10  aux2    Wald   Variances
    #> BCH test for equality across classes
    #> 
    #> Overall likelihood ratio test:
    #>  LL_baseline LL_restricted LL_dif df    p
    #>         6987          6987  0.071  1 0.79
    #> 
    #> Pairwise comparisons using likelihood ratio tests:
    #>  Model1 Model2 LL_baseline LL_restricted LL_dif df    p
    #>    aux1   aux2        6987          6987  0.071  1 0.79

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

    #>                          label        lhs   op        rhs     est     se pval
    #> 1        Means.burdened.class1   burdened   ~1          1    3.74   0.00 <NA>
    #> 2         Means.trapped.class1    trapped   ~1          1    2.30   0.00 <NA>
    #> 3       Means.negaffect.class1  negaffect   ~1          1    2.83   0.00 <NA>
    #> 4      Means.loneliness.class1 loneliness   ~1          1    2.45   0.00 <NA>
    #> 5               Means.y.class1          y   ~1          1  153.82   3.04 0.00
    #> 6    Variances.burdened.class1   burdened   ~~   burdened    0.37   0.00 <NA>
    #> 7     Variances.trapped.class1    trapped   ~~    trapped    0.34   0.00 <NA>
    #> 8   Variances.negaffect.class1  negaffect   ~~  negaffect    0.32   0.00 <NA>
    #> 9  Variances.loneliness.class1 loneliness   ~~ loneliness    0.32   0.00 <NA>
    #> 10          Variances.y.class1          y   ~~          y 2524.65 217.00 0.00
    #> 11       Means.burdened.class2   burdened   ~1          1    2.88   0.00 <NA>
    #> 12        Means.trapped.class2    trapped   ~1          1    0.92   0.00 <NA>
    #> 13      Means.negaffect.class2  negaffect   ~1          1    2.04   0.00 <NA>
    #> 14     Means.loneliness.class2 loneliness   ~1          1    2.96   0.00 <NA>
    #> 15              Means.y.class2          y   ~1          1  160.44   4.04 0.00
    #> 16   Variances.burdened.class2   burdened   ~~   burdened    0.37   0.00 <NA>
    #> 17    Variances.trapped.class2    trapped   ~~    trapped    0.34   0.00 <NA>
    #> 18  Variances.negaffect.class2  negaffect   ~~  negaffect    0.32   0.00 <NA>
    #> 19 Variances.loneliness.class2 loneliness   ~~ loneliness    0.32   0.00 <NA>
    #> 20          Variances.y.class2          y   ~~          y 3039.23 317.83 0.00
    #> 21 equal var 2.weights[1,1].NA          1 <NA>          1    1.00   <NA> <NA>
    #> 22 equal var 2.weights[1,2].NA          1 <NA>          2    0.71   <NA> <NA>
    #>               confint    est_sig pval_std  confint_std  free
    #> 1                <NA>       3.74     0.00 [6.13, 6.13] FALSE
    #> 2                <NA>       2.30     0.00 [3.97, 3.97] FALSE
    #> 3                <NA>       2.83     0.00 [5.00, 5.00] FALSE
    #> 4                <NA>       2.45     0.00 [4.34, 4.34] FALSE
    #> 5    [147.87, 159.78]  153.82***     0.00 [2.78, 3.35]  TRUE
    #> 6                <NA>       0.37     0.00 [1.00, 1.00] FALSE
    #> 7                <NA>       0.34     0.00 [1.00, 1.00] FALSE
    #> 8                <NA>       0.32     0.00 [1.00, 1.00] FALSE
    #> 9                <NA>       0.32     0.00 [1.00, 1.00] FALSE
    #> 10 [2099.33, 2949.97] 2524.65***     0.00 [1.00, 1.00]  TRUE
    #> 11               <NA>       2.88     0.00 [4.71, 4.71] FALSE
    #> 12               <NA>       0.92     0.00 [1.58, 1.58] FALSE
    #> 13               <NA>       2.04     0.00 [3.60, 3.60] FALSE
    #> 14               <NA>       2.96     0.00 [5.24, 5.24] FALSE
    #> 15   [152.52, 168.35]  160.44***     0.00 [2.58, 3.24]  TRUE
    #> 16               <NA>       0.37     0.00 [1.00, 1.00] FALSE
    #> 17               <NA>       0.34     0.00 [1.00, 1.00] FALSE
    #> 18               <NA>       0.32     0.00 [1.00, 1.00] FALSE
    #> 19               <NA>       0.32     0.00 [1.00, 1.00] FALSE
    #> 20 [2416.29, 3662.16] 3039.23***     0.00 [1.00, 1.00]  TRUE
    #> 21               <NA>       1.00     <NA>         <NA> FALSE
    #> 22               <NA>       0.71     <NA>         <NA> FALSE
    #>                        name lboundmet uboundmet std_est std_se        col
    #> 1             class1.M[1,1]        NA        NA    6.13   0.00   burdened
    #> 2             class1.M[1,2]        NA        NA    3.97   0.00    trapped
    #> 3             class1.M[1,3]        NA        NA    5.00   0.00  negaffect
    #> 4             class1.M[1,4]        NA        NA    4.34   0.00 loneliness
    #> 5             class1.M[1,5]     FALSE     FALSE    3.06   0.15          y
    #> 6             class1.S[1,1]        NA        NA    1.00   0.00   burdened
    #> 7             class1.S[2,2]        NA        NA    1.00   0.00    trapped
    #> 8             class1.S[3,3]        NA        NA    1.00   0.00  negaffect
    #> 9             class1.S[4,4]        NA        NA    1.00   0.00 loneliness
    #> 10            class1.S[5,5]     FALSE     FALSE    1.00   0.00          y
    #> 11            class2.M[1,1]        NA        NA    4.71   0.00   burdened
    #> 12            class2.M[1,2]        NA        NA    1.58   0.00    trapped
    #> 13            class2.M[1,3]        NA        NA    3.60   0.00  negaffect
    #> 14            class2.M[1,4]        NA        NA    5.24   0.00 loneliness
    #> 15            class2.M[1,5]     FALSE     FALSE    2.91   0.17          y
    #> 16            class2.S[1,1]        NA        NA    1.00   0.00   burdened
    #> 17            class2.S[2,2]        NA        NA    1.00   0.00    trapped
    #> 18            class2.S[3,3]        NA        NA    1.00   0.00  negaffect
    #> 19            class2.S[4,4]        NA        NA    1.00   0.00 loneliness
    #> 20            class2.S[5,5]     FALSE     FALSE    1.00   0.00          y
    #> 21 equal var 2.weights[1,1]        NA        NA    <NA>   <NA>          1
    #> 22 equal var 2.weights[1,2]        NA        NA    <NA>   <NA>          2
    #>     matrix             openmx_label        row  class ci_type  Category
    #> 1        M            class1.M[1,1]          1 class1    <NA>     Means
    #> 2        M            class1.M[1,2]          1 class1    <NA>     Means
    #> 3        M            class1.M[1,3]          1 class1    <NA>     Means
    #> 4        M            class1.M[1,4]          1 class1    <NA>     Means
    #> 5        M               aux1_M_1_1          1 class1    Wald     Means
    #> 6        S            class1.S[1,1]   burdened class1    <NA> Variances
    #> 7        S            class1.S[2,2]    trapped class1    <NA> Variances
    #> 8        S            class1.S[3,3]  negaffect class1    <NA> Variances
    #> 9        S            class1.S[4,4] loneliness class1    <NA> Variances
    #> 10       S               aux1_S_1_1          y class1    Wald Variances
    #> 11       M            class2.M[1,1]          1 class2    <NA>     Means
    #> 12       M            class2.M[1,2]          1 class2    <NA>     Means
    #> 13       M            class2.M[1,3]          1 class2    <NA>     Means
    #> 14       M            class2.M[1,4]          1 class2    <NA>     Means
    #> 15       M               aux2_M_1_1          1 class2    Wald     Means
    #> 16       S            class2.S[1,1]   burdened class2    <NA> Variances
    #> 17       S            class2.S[2,2]    trapped class2    <NA> Variances
    #> 18       S            class2.S[3,3]  negaffect class2    <NA> Variances
    #> 19       S            class2.S[4,4] loneliness class2    <NA> Variances
    #> 20       S               aux2_S_1_1          y class2    Wald Variances
    #> 21 weights equal var 2.weights[1,1]          1   <NA>    <NA>   Weights
    #> 22 weights equal var 2.weights[1,2]          1   <NA>    <NA>   Weights
    #> 
    #> Note: adjusted standard errors were used for: Variances.y.class1, Means.y.class1, Variances.y.class2, Means.y.class2.

Ordinary standard errors for the auxiliary model are also not valid, but
`tidySEM` replaces them with corrected two-step standard errors. These
are available in the attribute `attr(model, "se_robust")`, and
[`table_results()`](https://cjvanlissa.github.io/tidySEM/reference/table_results.md)
shows them by default.

``` r

attr(two_mean, "se_robust")
```

    #>        label  se     type
    #> 1 aux1_S_1_1 217 adjusted
    #> 2 aux1_M_1_1   3 adjusted
    #> 3 aux2_S_1_1 318 adjusted
    #> 4 aux2_M_1_1   4 adjusted

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

    #>                term estimate std.error statistic  df  p.value
    #> 1 I(factor(class))1      154       3.2        48 330 5.7e-150
    #> 2 I(factor(class))2      160       4.0        40 251 1.7e-111

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

    #>                   term estimate    se statistic  df df_complete   pvalue
    #> 1     Means.distance.1      154   3.0      51.1 441         509 1.7e-187
    #> 2 Variances.distance.1     2560 216.5      11.8 425         509  4.4e-28
    #> 3     Means.distance.2      160   3.9      41.1 420         509 3.8e-149
    #> 4 Variances.distance.2     2990 303.0       9.9 412         509  9.3e-21

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

    #>            term estimate std.error statistic  df p.value
    #> 1 (Intercept).1   -0.480    0.4785     -1.00 274    0.32
    #> 2    distance.1    0.002    0.0019      1.04 212    0.30
    #> 3   freqvisit.1   -0.050    0.0901     -0.55 389    0.58

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
