---
title: "Auxiliary Analyses for Latent Classes"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Auxiliary Analyses for Latent Classes}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---



Latent class analysis (LCA) and latent profile analysis (LPA) identify unobserved groups using a set of observed indicator variables.
Often, researchers also have research questions about variables that were not used to define the classes.
These are called *auxiliary variables*.
A distal outcome is an auxiliary variable considered as an outcome; other auxiliary variables may be predictors or covariates.

This vignette introduces three approaches in `tidySEM`: **BCH**, **two-step maximum likelihood**, and **pseudo-class draws**. We illustrate class-specific means, category probabilities, regressions, hypothesis tests, and predictors of class membership. For an introduction to estimating the measurement model, see [Introduction to Latent Class Analysis](vignette:lca_introduction).

## The Naive Approach

An often-heard question is: Why can we not simply assign every participant to their most likely class, and use class membership as a variable in auxiliary analyses?

The reason is that latent class membership is uncertain, and based on posterior class probabilities.
A participant with posterior class probabilities $(.05,.93,.02)$ is most likely to belong to class 2, with high certainty.
However, a participant with posterior class probabilities $(.45,.35,.20)$ is most likely to belong to class 1, but there is a lot of uncertainty with this assignment.

If we treat class assignments as an observed variable, this uncertainty propagates into auxiliary analyses and biases the results and their standard errors.
The three approaches below address this uncertainty in different ways.

## Example: Caregiver profiles

We use the `zegwaard_carecompass` data included in `tidySEM`.
As explained in the vignette on [Confirmatory LCA](vignette:lca_confirmatory),
the measurement model uses four caregiver-experience indicators.
For the present analyses, auxiliary variables concern the caregiver and the person receiving care.
One requirement of all analyses presented here is that the auxiliary data must have the same number of rows in **exactly the same order** as the measurement data.


``` r
library(tidySEM)
library(OpenMx)
df <- zegwaard_carecompass
indicators <- c("burdened", "trapped", "negaffect", "loneliness")
measurement_data <- df[, indicators]
```

For the examples in this vignette, we will just fit a simple two-class model:


``` r
set.seed(297)
lpa_model <- mx_profiles(measurement_data, classes = 2)
saveRDS(lpa_model, cache_file("measurement"))
```




``` r
lpa_model <- read_cached("measurement")
table_fit(lpa_model)
#>   Minus2LogLikelihood   n Parameters observedStatistics  df RMSEASquared RMSEANull   modelName
#> 1                4063 513         13               2030 500            0      0.05 equal var 2
#>    AIC  BIC saBIC Classes Entropy prob_min prob_max n_min n_max    LL
#> 1 4089 4144  4103       2    0.74     0.91     0.93  0.42  0.58 -2031
```

### Classification uncertainty


``` r
cp <- class_prob(lpa_model)
head(cp$individual)
cp$mostlikely.class
```


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
```

This table shows the average probability that individuals belong to each true class (rows),
and their probability of being assigned to each class (columns). 
The diagonal of this table thus represents the probability that observations in each class will be correctly classified.
These probabilities are high, but not perfect.
As also indicated in the fit table by a low Entropy criterion, these classes are not perfectly separated.
Therefore, do not treat assigned class as an observed variable; these classification errors would influence downstream auxiliary analyses.

## BCH: class differences in distal outcomes

The Bolck–Croon–Hagenaars (BCH) procedure first estimates the measurement model, then estimates an auxiliary multi-group model, accounting for classification uncertainty by weighting case contributions to each class' group.
In `tidySEM`, `BCH()` can compare classes on a single numeric or ordered outcome, or an entire auxiliary structural equation model.

### Continuous distal outcome

Here we compare caregiver distance across classes. The default numeric-outcome specification estimates class-specific means and variances.


``` r
bch_mean <- BCH(lpa_model, data = df$distance)
table_results(bch_mean, columns = NULL)
```




```
#>                label lhs op rhs     est     se pval            confint    est_sig pval_std
#> 1     Means.y.class1   y ~1   1  153.67   2.88 0.00   [148.02, 159.32]  153.67***     0.00
#> 2 Variances.y.class1   y ~~   y 2496.42 203.70 0.00 [2097.16, 2895.67] 2496.42***     0.00
#> 3     Means.y.class2   y ~1   1  160.66   3.82 0.00   [153.17, 168.15]  160.66***     0.00
#> 4 Variances.y.class2   y ~~   y 3076.16 299.84 0.00 [2488.48, 3663.83] 3076.16***     0.00
#>    confint_std free          name lboundmet uboundmet std_est std_se col matrix  openmx_label
#> 1 [2.80, 3.35] TRUE class1.M[1,1]     FALSE     FALSE    3.08   0.14   y      M class1.M[1,1]
#> 2 [1.00, 1.00] TRUE class1.S[1,1]     FALSE     FALSE    1.00   0.00   y      S class1.S[1,1]
#> 3 [2.59, 3.20] TRUE class2.M[1,1]     FALSE     FALSE    2.90   0.16   y      M class2.M[1,1]
#> 4 [1.00, 1.00] TRUE class2.S[1,1]     FALSE     FALSE    1.00   0.00   y      S class2.S[1,1]
#>   row  group ci_type  Category
#> 1   1 class1    Wald     Means
#> 2   y class1    Wald Variances
#> 3   1 class2    Wald     Means
#> 4   y class2    Wald Variances
```

The estimates describe class-specific distal means. They do **not** by themselves test whether the means differ. For an omnibus and pairwise comparison, use `lr_test()`; compare the means (`M`) rather than the entire auxiliary model.


``` r
lr_test(bch_mean, compare = "M")
```




```
#> BCH test for equality across classes
#> 
#> Overall likelihood ratio test:
#>  LL_baseline LL_restricted LL_dif df    p
#>         5491          5494    2.1  1 0.14
#> 
#> Pairwise comparisons using likelihood ratio tests:
#>  Model1 Model2 LL_baseline LL_restricted LL_dif df    p
#>  class1 class2        5491          5494    2.1  1 0.14
```

One crucial limitation of the BCH approach is that its standard errors are not valid,
so be careful drawing inferences about parameters in these distal models.

### Binary or ordinal distal outcome

For a binary distal outcome, `BCH()` estimates class-specific thresholds.
The function `table_prob()` represents these in the more interpretable response probability scale:


``` r
sex <- ordered(df$sexpatient, levels = c("male", "female"))
bch_sex <- BCH(lpa_model, data = sex)
table_prob(bch_sex)
```




```
#>   Variable Category Probability  group
#> 1        y        1        0.51 class1
#> 2        y        2        0.49 class1
#> 3        y        1        0.46 class2
#> 4        y        2        0.54 class2
```

### A class-specific regression model

The `model` argument accepts lavaan-style syntax.
This example investigates whether the classes differ in the effect of distance on frequency of visits.
If we request the `"matrix"` column of the output, you can see that the regression coefficients live in the `"A"` matrix (asymmetric effects, in other words: regressions).
A likelihood ratio test of the `"A"` matrix thus investigates whether the regression coefficient differs significantly between the two classes:


``` r
df_aux <- df[, c("freqvisit", "distance"), drop = FALSE]
df_aux$freqvisit <- as.numeric(df_aux$freqvisit)
bch_reg <- BCH(lpa_model,
               model = "freqvisit ~ distance",
               data = df_aux)
table_results(bch_reg, columns = c("label", "est_sig", "se", "pval", "confint", "group", "matrix"))
lr_test(bch_reg, compare = "A")
```




```
#>                                       label       lhs op       rhs     est     se pval
#> 1  Regressions.freqvisit.ON.distance.class1 freqvisit  ~  distance   -0.00   0.00 0.74
#> 2                    Means.freqvisit.class1 freqvisit ~1         1    3.88   0.20 0.00
#> 3                     Means.distance.class1  distance ~1         1  153.67   2.88 0.00
#> 4                Variances.freqvisit.class1 freqvisit ~~ freqvisit    1.13   0.09 0.00
#> 5                 Variances.distance.class1  distance ~~  distance 2496.41 203.70 0.00
#> 6  Regressions.freqvisit.ON.distance.class2 freqvisit  ~  distance   -0.00   0.00 0.49
#> 7                    Means.freqvisit.class2 freqvisit ~1         1    3.85   0.22 0.00
#> 8                     Means.distance.class2  distance ~1         1  160.68   3.82 0.00
#> 9                Variances.freqvisit.class2 freqvisit ~~ freqvisit    1.05   0.10 0.00
#> 10                Variances.distance.class2  distance ~~  distance 3076.19 299.82 0.00
#>               confint    est_sig pval_std   confint_std free          name lboundmet uboundmet
#> 1       [-0.00, 0.00]      -0.00     0.74 [-0.13, 0.09] TRUE class1.A[1,2]     FALSE     FALSE
#> 2        [3.50, 4.27]    3.88***     0.00  [3.19, 4.11] TRUE class1.M[1,1]     FALSE     FALSE
#> 3    [148.02, 159.32]  153.67***     0.00  [2.80, 3.35] TRUE class1.M[1,2]     FALSE     FALSE
#> 4        [0.95, 1.31]    1.13***     0.00  [1.00, 1.00] TRUE class1.S[1,1]     FALSE     FALSE
#> 5  [2097.16, 2895.67] 2496.41***     0.00  [1.00, 1.00] TRUE class1.S[2,2]     FALSE     FALSE
#> 6       [-0.00, 0.00]      -0.00     0.49 [-0.18, 0.09] TRUE class2.A[1,2]     FALSE     FALSE
#> 7        [3.43, 4.28]    3.85***     0.00  [3.22, 4.28] TRUE class2.M[1,1]     FALSE     FALSE
#> 8    [153.18, 168.17]  160.68***     0.00  [2.59, 3.20] TRUE class2.M[1,2]     FALSE     FALSE
#> 9        [0.85, 1.25]    1.05***     0.00  [0.98, 1.01] TRUE class2.S[1,1]     FALSE     FALSE
#> 10 [2488.55, 3663.83] 3076.19***     0.00  [1.00, 1.00] TRUE class2.S[2,2]     FALSE     FALSE
#>    std_est std_se       col matrix  openmx_label       row  group ci_type    Category
#> 1    -0.02   0.06  distance      A class1.A[1,2] freqvisit class1    Wald Regressions
#> 2     3.65   0.24 freqvisit      M class1.M[1,1]         1 class1    Wald       Means
#> 3     3.08   0.14  distance      M class1.M[1,2]         1 class1    Wald       Means
#> 4     1.00   0.00 freqvisit      S class1.S[1,1] freqvisit class1    Wald   Variances
#> 5     1.00   0.00  distance      S class1.S[2,2]  distance class1    Wald   Variances
#> 6    -0.05   0.07  distance      A class2.A[1,2] freqvisit class2    Wald Regressions
#> 7     3.75   0.27 freqvisit      M class2.M[1,1]         1 class2    Wald       Means
#> 8     2.90   0.16  distance      M class2.M[1,2]         1 class2    Wald       Means
#> 9     1.00   0.01 freqvisit      S class2.S[1,1] freqvisit class2    Wald   Variances
#> 10    1.00   0.00  distance      S class2.S[2,2]  distance class2    Wald   Variances
#> BCH test for equality across classes
#> 
#> Overall likelihood ratio test:
#>  LL_baseline LL_restricted LL_dif df    p
#>         6987          6987  0.071  1 0.79
#> 
#> Pairwise comparisons using likelihood ratio tests:
#>  Model1 Model2 LL_baseline LL_restricted LL_dif df    p
#>  class1 class2        6987          6987  0.071  1 0.79
```

## Two-step maximum likelihood

A different approach to auxiliary analysis is the `two_step()` procedure.
It first estimates the latent class model, then fixes its parameters and adds class-specific auxiliary models (whose parameters are free).
This prevents the auxiliary model from influencing the class solution.
Unlike BCH, the auxiliary outcomes enter the likelihood directly;
posterior class probabilities can therefore change even though measurement parameters stay fixed.

The interface of this function is similar to `BCH()`:


``` r
two_mean <- two_step(lpa_model, data = df$distance)
table_results(two_mean, columns = NULL)
```




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
#>               confint    est_sig pval_std  confint_std  free                     name
#> 1                <NA>       3.74     0.00 [6.13, 6.13] FALSE            class1.M[1,1]
#> 2                <NA>       2.30     0.00 [3.97, 3.97] FALSE            class1.M[1,2]
#> 3                <NA>       2.83     0.00 [5.00, 5.00] FALSE            class1.M[1,3]
#> 4                <NA>       2.45     0.00 [4.34, 4.34] FALSE            class1.M[1,4]
#> 5    [147.87, 159.78]  153.82***     0.00 [2.78, 3.35]  TRUE            class1.M[1,5]
#> 6                <NA>       0.37     0.00 [1.00, 1.00] FALSE            class1.S[1,1]
#> 7                <NA>       0.34     0.00 [1.00, 1.00] FALSE            class1.S[2,2]
#> 8                <NA>       0.32     0.00 [1.00, 1.00] FALSE            class1.S[3,3]
#> 9                <NA>       0.32     0.00 [1.00, 1.00] FALSE            class1.S[4,4]
#> 10 [2099.33, 2949.97] 2524.65***     0.00 [1.00, 1.00]  TRUE            class1.S[5,5]
#> 11               <NA>       2.88     0.00 [4.71, 4.71] FALSE            class2.M[1,1]
#> 12               <NA>       0.92     0.00 [1.58, 1.58] FALSE            class2.M[1,2]
#> 13               <NA>       2.04     0.00 [3.60, 3.60] FALSE            class2.M[1,3]
#> 14               <NA>       2.96     0.00 [5.24, 5.24] FALSE            class2.M[1,4]
#> 15   [152.52, 168.35]  160.44***     0.00 [2.58, 3.24]  TRUE            class2.M[1,5]
#> 16               <NA>       0.37     0.00 [1.00, 1.00] FALSE            class2.S[1,1]
#> 17               <NA>       0.34     0.00 [1.00, 1.00] FALSE            class2.S[2,2]
#> 18               <NA>       0.32     0.00 [1.00, 1.00] FALSE            class2.S[3,3]
#> 19               <NA>       0.32     0.00 [1.00, 1.00] FALSE            class2.S[4,4]
#> 20 [2416.29, 3662.16] 3039.23***     0.00 [1.00, 1.00]  TRUE            class2.S[5,5]
#> 21               <NA>       1.00     <NA>         <NA> FALSE equal var 2.weights[1,1]
#> 22               <NA>       0.71     <NA>         <NA> FALSE equal var 2.weights[1,2]
#>    lboundmet uboundmet std_est std_se        col  matrix             openmx_label        row
#> 1         NA        NA    6.13   0.00   burdened       M            class1.M[1,1]          1
#> 2         NA        NA    3.97   0.00    trapped       M            class1.M[1,2]          1
#> 3         NA        NA    5.00   0.00  negaffect       M            class1.M[1,3]          1
#> 4         NA        NA    4.34   0.00 loneliness       M            class1.M[1,4]          1
#> 5      FALSE     FALSE    3.06   0.15          y       M               aux1_M_1_1          1
#> 6         NA        NA    1.00   0.00   burdened       S            class1.S[1,1]   burdened
#> 7         NA        NA    1.00   0.00    trapped       S            class1.S[2,2]    trapped
#> 8         NA        NA    1.00   0.00  negaffect       S            class1.S[3,3]  negaffect
#> 9         NA        NA    1.00   0.00 loneliness       S            class1.S[4,4] loneliness
#> 10     FALSE     FALSE    1.00   0.00          y       S               aux1_S_1_1          y
#> 11        NA        NA    4.71   0.00   burdened       M            class2.M[1,1]          1
#> 12        NA        NA    1.58   0.00    trapped       M            class2.M[1,2]          1
#> 13        NA        NA    3.60   0.00  negaffect       M            class2.M[1,3]          1
#> 14        NA        NA    5.24   0.00 loneliness       M            class2.M[1,4]          1
#> 15     FALSE     FALSE    2.91   0.17          y       M               aux2_M_1_1          1
#> 16        NA        NA    1.00   0.00   burdened       S            class2.S[1,1]   burdened
#> 17        NA        NA    1.00   0.00    trapped       S            class2.S[2,2]    trapped
#> 18        NA        NA    1.00   0.00  negaffect       S            class2.S[3,3]  negaffect
#> 19        NA        NA    1.00   0.00 loneliness       S            class2.S[4,4] loneliness
#> 20     FALSE     FALSE    1.00   0.00          y       S               aux2_S_1_1          y
#> 21        NA        NA    <NA>   <NA>          1 weights equal var 2.weights[1,1]          1
#> 22        NA        NA    <NA>   <NA>          2 weights equal var 2.weights[1,2]          1
#>     class ci_type  Category
#> 1  class1    <NA>     Means
#> 2  class1    <NA>     Means
#> 3  class1    <NA>     Means
#> 4  class1    <NA>     Means
#> 5  class1    Wald     Means
#> 6  class1    <NA> Variances
#> 7  class1    <NA> Variances
#> 8  class1    <NA> Variances
#> 9  class1    <NA> Variances
#> 10 class1    Wald Variances
#> 11 class2    <NA>     Means
#> 12 class2    <NA>     Means
#> 13 class2    <NA>     Means
#> 14 class2    <NA>     Means
#> 15 class2    Wald     Means
#> 16 class2    <NA> Variances
#> 17 class2    <NA> Variances
#> 18 class2    <NA> Variances
#> 19 class2    <NA> Variances
#> 20 class2    Wald Variances
#> 21   <NA>    <NA>   Weights
#> 22   <NA>    <NA>   Weights
#> 
#> Note: adjusted standard errors were used for: Variances.y.class1, Means.y.class1, Variances.y.class2, Means.y.class2.
```

Ordinary standard errors for the auxiliary model are also not valid, but `tidySEM` replaces them with corrected two-step standard errors.
These are available in the attribute `attr(model, "se_robust")`, and `table_results()` shows them by default.


``` r
attr(two_mean, "se_robust")
```


```
#>        label  se     type
#> 1 aux1_S_1_1 217 adjusted
#> 2 aux1_M_1_1   3 adjusted
#> 3 aux2_S_1_1 318 adjusted
#> 4 aux2_M_1_1   4 adjusted
```

For a custom auxiliary RAM model, the syntax can include multiple outcomes and regressions:






``` r
two_reg <- two_step(
  lpa_model,
  model = "freqvisit ~ distance",
  data = df_aux
)
table_results(two_reg)
```


## Pseudo-class draws and pooling

The pseudo-class approach accounts for classification uncertainty by repeatedly drawing one class for each observation according to its posterior probabilities.
Thus, a person with posterior probabilities $(.05,.93,.02)$ will be assigned to class 2 in *most* replications, but occasionally to class 1 or 3.
Conversely, a participant with posterior class probabilities $(.45,.35,.20)$ will be assigned to each of the three classes in many replications.

The auxiliary model is fitted in each completed dataset, and the results are pooled. This propagates classification uncertainty through the draws. 
Increasing the number of draws reduces Monte Carlo error.

### Compare distal means

Here we use `lm()` with a factor-coded class variable and no intercept, yielding one coefficient per class. The `model` expression is evaluated in each pseudo-class dataset.


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




```
#>                term estimate std.error statistic  df  p.value
#> 1 I(factor(class))1      154       3.2        48 330 5.7e-150
#> 2 I(factor(class))2      160       4.0        40 251 1.7e-111
```


### Use RAM syntax

The `pseudo_class()` approach also accepts auxiliary models expressed using `lavaan` syntax. If the model does not explicitly reference `class`, the function treats class as a grouping variable.


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




```
#>                   term estimate    se statistic  df df_complete   pvalue
#> 1     Means.distance.1      154   3.0      51.1 441         509 1.7e-187
#> 2 Variances.distance.1     2560 216.5      11.8 425         509  4.4e-28
#> 3     Means.distance.2      160   3.9      41.1 420         509 3.8e-149
#> 4 Variances.distance.2     2990 303.0       9.9 412         509  9.3e-21
```


## Predicting class membership

So far, we have treated class membership as an explanatory grouping for auxiliary outcomes.
Sometimes the research question instead asks which external variables predict class membership.
These are not the same estimands: differences in a distal mean do not supply the coefficients of a multinomial regression adjusted for other predictors.

For example, a multinomial model can be fitted across pseudo-class draws using `nnet::multinom()`:


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




```
#>            term estimate std.error statistic  df p.value
#> 1 (Intercept).1   -0.480    0.4785     -1.00 274    0.32
#> 2    distance.1    0.002    0.0019      1.04 212    0.30
#> 3   freqvisit.1   -0.050    0.0901     -0.55 389    0.58
```

The coefficients describe conditional associations with class membership relative to a reference class.

## Choosing an approach

| Approach | Uncertainty correction | Measurement model | Limitations |
|:--|:--|:--|:--|
| BCH | Classification-error-corrected weights | Fixed from step 1 | Class only as predictor/moderator; no valid SEs |
| Two-step ML | Joint auxiliary likelihood with fixed measurement parameters | Fixed from step 1 | Class only as predictor/moderator |
| Pseudo-class | Repeated posterior class draws and pooling | Fixed from step 1 | N/A |


## Reporting auxiliary analyses

A report should identify the measurement indicators, selected number of classes, classification quality, auxiliary-variable role, estimation method, and treatment of uncertainty. Report class-specific estimates and their units, as well as uncertainty intervals obtained from a justified variance estimator. For comparisons across classes, state which parameters were constrained and how the test was calibrated.

The following code illustrates extraction of a results table for the BCH model; it is only displayed when cached results are enabled.


``` r
tab <- table_results(bch_mean, columns = NULL,
                     format_numeric = FALSE)
tab
```


```
#>                label lhs op rhs  est    se    pval            confint    est_sig pval_std
#> 1     Means.y.class1   y ~1   1  154   2.9 0.0e+00   [148.02, 159.32]  153.67*** 7.4e-110
#> 2 Variances.y.class1   y ~~   y 2496 203.7 1.6e-34 [2097.16, 2895.67] 2496.42***  0.0e+00
#> 3     Means.y.class2   y ~1   1  161   3.8 0.0e+00   [153.17, 168.15]  160.66***  6.5e-76
#> 4 Variances.y.class2   y ~~   y 3076 299.8 1.1e-24 [2488.48, 3663.83] 3076.16***  0.0e+00
#>    confint_std free          name lboundmet uboundmet std_est  std_se col matrix  openmx_label
#> 1 [2.80, 3.35] TRUE class1.M[1,1]     FALSE     FALSE     3.1 1.4e-01   y      M class1.M[1,1]
#> 2 [1.00, 1.00] TRUE class1.S[1,1]     FALSE     FALSE     1.0 1.8e-13   y      S class1.S[1,1]
#> 3 [2.59, 3.20] TRUE class2.M[1,1]     FALSE     FALSE     2.9 1.6e-01   y      M class2.M[1,1]
#> 4 [1.00, 1.00] TRUE class2.S[1,1]     FALSE     FALSE     1.0 1.3e-12   y      S class2.S[1,1]
#>   row  group ci_type  Category
#> 1   1 class1    Wald     Means
#> 2   y class1    Wald Variances
#> 3   1 class2    Wald     Means
#> 4   y class2    Wald Variances
```

For related workflows, see [Introduction to Latent Class Analysis](vignette:lca_introduction) and the help pages `?BCH`, `?two_step`, `?pseudo_class`, and `?append_class_draws`.

## Rebuilding this vignette

By default, `run_everything` and `eval_results` are `FALSE`, so the CRAN vignette builds without fitting mixture or auxiliary models and without embedding large result tables or plots. To regenerate cached model objects, set `TIDYSEM_RUN_EVERYTHING=true` (or put `TRUE` in `run_everything.txt`) and rebuild. To show cached output on pkgdown, set `TIDYSEM_EVAL_RESULTS=true` after the caches have been generated. Do not distribute large `.rds` caches as part of the CRAN source package unless they are genuinely needed by users. The code examples remain visible in all builds.
