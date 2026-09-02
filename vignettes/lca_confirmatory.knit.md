---
title: "Confirmatory LPA for the Caregiver Compass"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Confirmatory LPA for the Caregiver Compass}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---



This is an example of confirmatory LPA using `tidySEM`,
as explained in Van Lissa, C. J., Garnier-Villarreal, M., & Anadria, D. (2023). *Recommended Practices in Latent Class Analysis using the Open-Source R-Package tidySEM.* Structural Equation Modeling. <https://doi.org/10.1080/10705511.2023.2250920>.
The simulated data are based on work by Zegwaard and colleagues,
who sought to establish a typology of caregivers who support a close other receiving outpatient psychological care.
Qualitative research among experts resulted in a theory postulating the existence of four types of caregivers (translated from the original Dutch):

**Balanced**

> The balanced caregiver experiences relative balance between the costs and benefits of caring for a close other.

**Imbalanced**

> The imbalanced caregiver experiences a precarious balance between the costs and benefits of caring for a close other.

**Lonely**

> The lonely caregiver experiences a strong sense of isolation.

**Entrapped**

> The entrapped caregiver strongly feels a sense of being entangled in responsibilities which are difficult to fulfill.

The goal of this confirmatory study was to validate this hypothesized class solution in a sample of caregivers.
A convenience sample was used, with no prior sample size justification.
To view the data documentation,
run the command `?tidySEM::zegwaard_carecompass` in the R console.

## Loading the Data

To load the data, simply attach the `tidySEM` package.
For convenience, we assign the variables used for analysis to an object called `df`.
We first only use the four scales: `c("burdened", "trapped", "negaffect", "loneliness")`.


``` r
# Load required packages
library(tidySEM)
library(ggplot2)
library(OpenMx)
# Load data
df <- zegwaard_carecompass[, c("burdened", "trapped", "negaffect",
    "loneliness")]
```

## Descriptive statistics

We use `tidySEM::descriptives()` to describe the data numerically.
Because all scales are continuous,
we select only columns for continuous data to de-clutter the table:


``` r
desc <- tidySEM::descriptives(df)
desc <- desc[, c("name", "n", "missing", "unique", "mean", "median",
    "sd", "min", "max", "skew_2se", "kurt_2se")]
desc
```


Table: Descriptive statistics

|name       |   n| missing| unique| mean| median|   sd|   min| max| skew_2se| kurt_2se|
|:----------|---:|-------:|------:|----:|------:|----:|-----:|---:|--------:|--------:|
|burdened   | 509|    0.01|    509|  3.4|    3.4| 0.75|  1.20| 5.3|     0.17|    -0.41|
|trapped    | 505|    0.02|    505|  1.7|    1.8| 0.90| -0.86| 3.8|    -1.03|    -1.52|
|negaffect  | 506|    0.01|    506|  2.5|    2.5| 0.69|  0.71| 5.0|     0.08|    -0.38|
|loneliness | 510|    0.01|    510|  2.7|    2.7| 0.62|  0.98| 4.2|    -0.33|    -0.62|



The table indicates two potential causes for concern:
there is a small percentage of missingness,
and all variables have relatively high kurtosis.
Since there are some missing values,
we can conduct an MCAR test using `mice::mcar(df)`.
According to Hawkins' test,
there is no evidence to reject the assumptions of multivariate normality and MCAR, $\tilde{\chi^2}(6) = 3.78, \tilde{p} = 0.71$.
Missing data will be accounted for using FIML.

Additionally, we can plot the data.
The `ggplot2` function `geom_density()` is useful for continuous data.
Visual inspection confirms the conclusions from the `descriptives()` table:
the data are kurtotic (peaked).


``` r
df_plot <- df
names(df_plot) <- paste0("Value.", names(df_plot))
df_plot <- reshape(df_plot, varying = names(df_plot), direction = "long",
    timevar = "Variable")
ggplot(df_plot, aes(x = Value)) + geom_density() + facet_wrap(~Variable) +
    theme_bw()
```

<img src="plot_lpa_desc.png" alt="" width="80%" />

## Conducting Latent Profile Analysis

As all variables are continuous, we can use the convenience function
`tidySEM::mx_profiles()`,
which is a wrapper for the generic function `mx_mixture()` optimized for continuous indicators.
Its default settings are appropriate for LPA, assuming fixed variances across classes and zero covariances.
Its arguments are `data` and number of `classes`.
All variables in `data` are included in the analysis,
which is why we first selected the indicator variables.
As this is a confirmatory LCA,
we do not follow a strictly data-driven class enumeration procedure.
We will set the maximum number of classes $K$ to one more than the theoretically expected number.
We set a seed to ensure replicable results.


```
#> MxComputeSimAnnealing(tsallis1996) evaluations 721 fit 6012.21 change -3014MxComputeSimAnnealing(tsallis1996) evaluations 2344 fit 4940.28 change 0.08195MxComputeSimAnnealing(tsallis1996) evaluations 3715 fit 8832.13 change 3682   MxComputeSimAnnealing(tsallis1996) evaluations 5314 fit 8117.91 change 3617MxComputeSimAnnealing(tsallis1996) evaluations 6904 fit 4066.88 change -2773MxComputeSimAnnealing(tsallis1996) evaluations 8485 fit 4093.61 change 30.14MxComputeSimAnnealing(tsallis1996) evaluations 10065 fit 4067.84 change -2215MxComputeSimAnnealing(tsallis1996) evaluations 11628 fit 4067.79 change 4.841MxComputeSimAnnealing(tsallis1996) evaluations 13198 fit 4062.91 change 0.001283MxComputeSimAnnealing(tsallis1996) evaluations 14765 fit 4097.99 change 35.26   MxComputeSimAnnealing(tsallis1996) evaluations 16330 fit 4062.81 change 0.06946                                                                               
#> MxComputeSimAnnealing(tsallis1996) evaluations 340 fit 5215.4 change -242.4MxComputeSimAnnealing(tsallis1996) evaluations 1417 fit 5453.38 change -8.619MxComputeSimAnnealing(tsallis1996) evaluations 2498 fit 5446.16 change -7.221MxComputeSimAnnealing(tsallis1996) evaluations 3578 fit 5514.64 change -7.985MxComputeSimAnnealing(tsallis1996) evaluations 4650 fit 5895.85 change 1268  MxComputeSimAnnealing(tsallis1996) evaluations 5723 fit 5176.87 change -20.84MxComputeSimAnnealing(tsallis1996) evaluations 6787 fit 3974.63 change -15.39MxComputeSimAnnealing(tsallis1996) evaluations 7850 fit 4719.78 change -413.1MxComputeSimAnnealing(tsallis1996) evaluations 8916 fit 5039.22 change 278.5 MxComputeSimAnnealing(tsallis1996) evaluations 9977 fit 4788.05 change 0.0759MxComputeSimAnnealing(tsallis1996) evaluations 11037 fit 3903.01 change -311.3MxComputeSimAnnealing(tsallis1996) evaluations 12088 fit 3904.44 change -3543 MxComputeSimAnnealing(tsallis1996) evaluations 13145 fit 4813.07 change 712.4MxComputeSimAnnealing(tsallis1996) evaluations 14198 fit 3902.89 change -1766MxComputeSimAnnealing(tsallis1996) evaluations 15256 fit 3902.94 change -1273MxComputeSimAnnealing(tsallis1996) evaluations 16308 fit 4951.28 change 1047 MxComputeSimAnnealing(tsallis1996) evaluations 17357 fit 3903.24 change 0.5296MxComputeSimAnnealing(tsallis1996) evaluations 18400 fit 4821.55 change 918.8 MxComputeSimAnnealing(tsallis1996) evaluations 19416 fit 3920.06 change 16.22MxComputeSimAnnealing(tsallis1996) evaluations 20437 fit 3903.3 change 0.5823MxComputeSimAnnealing(tsallis1996) evaluations 21461 fit 3902.71 change -384.8MxComputeSimAnnealing(tsallis1996) evaluations 22476 fit 3902.71 change -12.52                                                                              
#> MxComputeSimAnnealing(tsallis1996) evaluations 268 fit 5489.01 change -10.91MxComputeSimAnnealing(tsallis1996) evaluations 905 fit 8887.47 change 883   MxComputeSimAnnealing(tsallis1996) evaluations 1543 fit 4640.51 change 0.01372MxComputeSimAnnealing(tsallis1996) evaluations 2174 fit 12626.9 change 8050   MxComputeSimAnnealing(tsallis1996) evaluations 2812 fit 3954.13 change -2.02MxComputeSimAnnealing(tsallis1996) evaluations 3445 fit 5088.76 change -404 MxComputeSimAnnealing(tsallis1996) evaluations 4082 fit 6524.41 change 186.1MxComputeSimAnnealing(tsallis1996) evaluations 4716 fit 4634.83 change 0.8952MxComputeSimAnnealing(tsallis1996) evaluations 5343 fit 7189.18 change 3236  MxComputeSimAnnealing(tsallis1996) evaluations 5976 fit 5167.59 change -22.48MxComputeSimAnnealing(tsallis1996) evaluations 6602 fit 4627.98 change 0.8958MxComputeSimAnnealing(tsallis1996) evaluations 7231 fit 8307.32 change 3220  MxComputeSimAnnealing(tsallis1996) evaluations 7858 fit 5108.66 change -7.449MxComputeSimAnnealing(tsallis1996) evaluations 8483 fit 4730.39 change 861.9 MxComputeSimAnnealing(tsallis1996) evaluations 9108 fit 3917.34 change -772.9MxComputeSimAnnealing(tsallis1996) evaluations 9732 fit 4322.07 change -360.2MxComputeSimAnnealing(tsallis1996) evaluations 10361 fit 3875.6 change 21.26 MxComputeSimAnnealing(tsallis1996) evaluations 10985 fit 4883.14 change 1003MxComputeSimAnnealing(tsallis1996) evaluations 11609 fit 4862.59 change 1027MxComputeSimAnnealing(tsallis1996) evaluations 12234 fit 4627.74 change 540.1MxComputeSimAnnealing(tsallis1996) evaluations 12859 fit 4636 change 0.01773 MxComputeSimAnnealing(tsallis1996) evaluations 13486 fit 3833.63 change -162.2MxComputeSimAnnealing(tsallis1996) evaluations 14111 fit 3861.82 change -307.4MxComputeSimAnnealing(tsallis1996) evaluations 14730 fit 3841.72 change -1957 MxComputeSimAnnealing(tsallis1996) evaluations 15353 fit 3832.62 change -25.63MxComputeSimAnnealing(tsallis1996) evaluations 15978 fit 3860.93 change -656  MxComputeSimAnnealing(tsallis1996) evaluations 16599 fit 3837.88 change -950.4MxComputeSimAnnealing(tsallis1996) evaluations 17221 fit 4788.83 change 780.3 MxComputeSimAnnealing(tsallis1996) evaluations 17841 fit 3850.56 change 16.52MxComputeSimAnnealing(tsallis1996) evaluations 18460 fit 4789.89 change 867.9MxComputeSimAnnealing(tsallis1996) evaluations 19083 fit 3921.85 change 88.77MxComputeSimAnnealing(tsallis1996) evaluations 19704 fit 3834.73 change -948.1MxComputeSimAnnealing(tsallis1996) evaluations 20326 fit 4158.72 change 326.2 MxComputeSimAnnealing(tsallis1996) evaluations 20949 fit 3832.57 change 0.03872MxComputeSimAnnealing(tsallis1996) evaluations 21571 fit 3832.63 change -791.3 MxComputeSimAnnealing(tsallis1996) evaluations 22192 fit 4607.04 change -13.11MxComputeSimAnnealing(tsallis1996) evaluations 22813 fit 3869.56 change 37.03 MxComputeSimAnnealing(tsallis1996) evaluations 23435 fit 3832.65 change -789.6MxComputeSimAnnealing(tsallis1996) evaluations 24054 fit 4619.53 change -17.66MxComputeSimAnnealing(tsallis1996) evaluations 24678 fit 3832.54 change -807.5MxComputeSimAnnealing(tsallis1996) evaluations 25298 fit 3832.53 change -792.8MxComputeSimAnnealing(tsallis1996) evaluations 25919 fit 3832.56 change 0.04107MxComputeSimAnnealing(tsallis1996) evaluations 26539 fit 3832.57 change 0.05171MxComputeSimAnnealing(tsallis1996) evaluations 27157 fit 4786.33 change 922.4  MxComputeSimAnnealing(tsallis1996) evaluations 27779 fit 3832.52 change 0.0005425MxComputeSimAnnealing(tsallis1996) evaluations 28401 fit 3833 change -804.2      MxComputeSimAnnealing(tsallis1996) evaluations 29020 fit 3845.97 change 13.42                                                                             
#> MxComputeSimAnnealing(tsallis1996) evaluations 410 fit 4788.76 change -9.762MxComputeSimAnnealing(tsallis1996) evaluations 1054 fit 4788.76 change -9.762MxComputeSimAnnealing(tsallis1996) evaluations 1699 fit 4803.3 change 14.54  MxComputeSimAnnealing(tsallis1996) evaluations 2344 fit 5202.94 change 399.6MxComputeSimAnnealing(tsallis1996) evaluations 2987 fit 4802.31 change 14.54MxComputeSimAnnealing(tsallis1996) evaluations 3631 fit 4802.31 change 14.54MxComputeSimAnnealing(tsallis1996) evaluations 4275 fit 4793.87 change 8.876MxComputeSimAnnealing(tsallis1996) evaluations 4920 fit 5061.88 change 294.8MxComputeSimAnnealing(tsallis1996) evaluations 5563 fit 4772.67 change 13.79MxComputeSimAnnealing(tsallis1996) evaluations 6206 fit 4748.81 change -9.378MxComputeSimAnnealing(tsallis1996) evaluations 6841 fit 4625.75 change -1164 MxComputeSimAnnealing(tsallis1996) evaluations 7484 fit 8226.25 change 4337 MxComputeSimAnnealing(tsallis1996) evaluations 8126 fit 3890.54 change -0.4846MxComputeSimAnnealing(tsallis1996) evaluations 8765 fit 4566.46 change 9.379  MxComputeSimAnnealing(tsallis1996) evaluations 9405 fit 3888 change -384.3  MxComputeSimAnnealing(tsallis1996) evaluations 10041 fit 4746.45 change -4.205MxComputeSimAnnealing(tsallis1996) evaluations 10681 fit 3969.63 change -155  MxComputeSimAnnealing(tsallis1996) evaluations 11320 fit 5517.85 change 1687MxComputeSimAnnealing(tsallis1996) evaluations 11963 fit 3838.92 change -40.67MxComputeSimAnnealing(tsallis1996) evaluations 12602 fit 3889.23 change -239.5MxComputeSimAnnealing(tsallis1996) evaluations 13237 fit 4650.5 change -18.91 MxComputeSimAnnealing(tsallis1996) evaluations 13876 fit 4744.63 change 712.6MxComputeSimAnnealing(tsallis1996) evaluations 14512 fit 8423.8 change 4549  MxComputeSimAnnealing(tsallis1996) evaluations 15149 fit 4563.01 change 733.8MxComputeSimAnnealing(tsallis1996) evaluations 15785 fit 4643.9 change -18.61MxComputeSimAnnealing(tsallis1996) evaluations 16418 fit 3827.71 change -0.6289MxComputeSimAnnealing(tsallis1996) evaluations 17052 fit 4553.35 change 165.6  MxComputeSimAnnealing(tsallis1996) evaluations 17687 fit 3827.37 change -902 MxComputeSimAnnealing(tsallis1996) evaluations 18322 fit 7908.36 change 3055MxComputeSimAnnealing(tsallis1996) evaluations 18957 fit 4574.44 change 9.45MxComputeSimAnnealing(tsallis1996) evaluations 19588 fit 4715.54 change 888.5MxComputeSimAnnealing(tsallis1996) evaluations 20221 fit 3825.65 change -10.14MxComputeSimAnnealing(tsallis1996) evaluations 20856 fit 3825.53 change -0.0752MxComputeSimAnnealing(tsallis1996) evaluations 21493 fit 4740.24 change -4.629 MxComputeSimAnnealing(tsallis1996) evaluations 22128 fit 4000.66 change 175.5 MxComputeSimAnnealing(tsallis1996) evaluations 22758 fit 4636.39 change 805.6MxComputeSimAnnealing(tsallis1996) evaluations 23391 fit 8996.95 change 5170 MxComputeSimAnnealing(tsallis1996) evaluations 24025 fit 4550.61 change -15.93MxComputeSimAnnealing(tsallis1996) evaluations 24658 fit 3827.62 change -924.7MxComputeSimAnnealing(tsallis1996) evaluations 25288 fit 3837.99 change 13.22 MxComputeSimAnnealing(tsallis1996) evaluations 25919 fit 4757.19 change 931.3MxComputeSimAnnealing(tsallis1996) evaluations 26551 fit 3835.25 change 10.31MxComputeSimAnnealing(tsallis1996) evaluations 27182 fit 3824.77 change -799.2MxComputeSimAnnealing(tsallis1996) evaluations 27815 fit 4036.97 change -753.4MxComputeSimAnnealing(tsallis1996) evaluations 28447 fit 3824.76 change -0.6824MxComputeSimAnnealing(tsallis1996) evaluations 29075 fit 3846.97 change 22.2   MxComputeSimAnnealing(tsallis1996) evaluations 29706 fit 3824.75 change -662MxComputeSimAnnealing(tsallis1996) evaluations 30336 fit 3839.39 change -8499MxComputeSimAnnealing(tsallis1996) evaluations 30966 fit 3824.75 change -663.1MxComputeSimAnnealing(tsallis1996) evaluations 31596 fit 4026.91 change -31.51MxComputeSimAnnealing(tsallis1996) evaluations 32224 fit 3824.75 change -0.02669MxComputeSimAnnealing(tsallis1996) evaluations 32854 fit 3824.77 change 0.01211 MxComputeSimAnnealing(tsallis1996) evaluations 33485 fit 3824.76 change -0.478 MxComputeSimAnnealing(tsallis1996) evaluations 34113 fit 3824.76 change -3380 MxComputeSimAnnealing(tsallis1996) evaluations 34743 fit 3824.76 change -6.637MxComputeSimAnnealing(tsallis1996) evaluations 35371 fit 3824.8 change -9.237                                                                              
```

``` r
set.seed(123)
res <- mx_profiles(data = df, classes = 1:5)
```


This analysis should produce some messages about cluster initialization.
These relate to the selection of starting values,
which relies on the K-means algorithm and is not robust to missing data.
The algorithm automatically switches to hierarchical clustering, no further action is required.

## Class Enumeration

To compare the fit of the theoretical model against other models,
we create a model fit table using
`table_fit()` and retain relevant columns.
We also determine whether any models can be disqualified.

In this example, all models converge without issues.
If, for example, the two-class solution had not converged, we could use the function `res[[2]] <- mxTryHard(res[[2]])` to aid convergence.

Next, we check for local identifiability.
The sample size is consistently reported as 513,
which means that partially missing cases were indeed included via FIML.
The smallest class size occurs in the 5-class model,
where the smallest class is assigned 7% of cases, or 38 cases.
This model has 28 parameters, approximately 6 per class.
We thus have at least five observations per parameter in every class,
and do not disqualify the 5-class model.

There are concerns about theoretical interpretability of all solutions,
as the entropies and minimum classification probabilities are all low.
However, in this confirmatory use case, we address this when interpreting the results.


``` r
fit <- table_fit(res)  # model fit table
fit[, c("Name", "LL", "Parameters", "n", "BIC", "Entropy", "prob_min",
    "prob_max", "n_min", "n_max", "np_ratio", "np_local")]
```


Table: Model fit table

|Name        |    LL|  p|   n|  BIC| Entropy| p_min| p_max| n_min| n_max|
|:-----------|-----:|--:|---:|----:|-------:|-----:|-----:|-----:|-----:|
|equal var 1 | -2242|  8| 513| 4534|    1.00|  1.00|  1.00|  1.00|  1.00|
|equal var 2 | -2031| 13| 513| 4144|    0.74|  0.91|  0.93|  0.42|  0.58|
|equal var 3 | -1951| 18| 513| 4015|    0.78|  0.89|  0.91|  0.19|  0.54|
|equal var 4 | -1916| 23| 513| 3976|    0.75|  0.81|  0.92|  0.16|  0.34|
|equal var 5 | -1912| 28| 513| 3999|    0.79|  0.81|  0.92|  0.00|  0.34|



### Using ICs

the 4-class solution has the lowest BIC,
which means it is preferred over all other solutions including a 1-class solution and a solution with more classes.
Note that a scree plot for the BIC can be plotted by calling `plot(fit)`.
Following the elbow criterion, a three-class solution would also be defensible.
The function `ic_weights(fit)` allows us to compute IC weights;
it indicates that, conditional on the set of models,
the 4-class model has a posterior model probability of nearly 100%.

### Using LMR tests

If we conduct LMR tests, we find that the tests are significant for all pairwise model comparisons, except for the 5-class model:


``` r
lr_lmr(res)
```



Table: LMR test table

|null |alt  |    lr| df|    p|   w2| p_w2|
|:----|:----|-----:|--:|----:|----:|----:|
|mix1 |mix2 | 10.25|  5| 0.00| 0.82|    0|
|mix2 |mix3 |  5.30|  5| 0.00| 0.44|    0|
|mix3 |mix4 |  4.14|  5| 0.00| 0.14|    0|
|mix4 |mix5 |  0.88|  5| 0.19| 0.04|    0|



### Using BLRT tests

We can also use the BLRT test.
As it is very computationally expensive,
we will use a low number of replications here.
In practice, one might use a much higher number (1000+) for published research.
Keep in mind that the p-value of the BLRT is subject to Monte Carlo error;
if it fluctuates when analyses are replicated or its value is very close to the critical threshold, consider increasing the number of replications.

To accelerate computations, we can use the `future` package for parallel computing (see `?plan` to select the appropriate back-end for your system).
To track the function's progress,
we use the `progressr` ecosystem,
which allows users to choose how they want to be informed.
The example below uses a progress bar:


``` r
library(future)
library(progressr)
plan(multisession)  # Parallel processing for Windows
handlers("progress")  # Progress bar
set.seed(1)
res_blrt <- BLRT(res, replications = 100)
```



Table: BLRT test table

|null |alt  |    lr| df| blrt_p| samples|
|:----|:----|-----:|--:|------:|-------:|
|mix1 |mix2 | 421.2|  5|   0.00|     100|
|mix2 |mix3 | 160.0|  5|   0.00|     100|
|mix3 |mix4 |  70.2|  5|   0.00|     100|
|mix4 |mix5 |   7.8|  5|   0.36|     100|



In sum, across all class enumeration criteria, there is strong support for a 4-class solution.

## Optional: Alternative Model Specifications

In the case of confirmatory LCA, the theory would be refuted by strong evidence against the hypothesized model and number of classes.
In the preceding, we only compared the theoretical model against models with different number of classes.
Imagine, however, that a Reviewer argues that variance ought to be freely estimated across classes.
We could compare our theoretical model against their competing model as follows.
Note that we can put two models into a list to compare them.


```
#> MxComputeSimAnnealing(tsallis1996) evaluations 301 fit 5176.97 change 78.05MxComputeSimAnnealing(tsallis1996) evaluations 929 fit 5176.97 change 63.94MxComputeSimAnnealing(tsallis1996) evaluations 1554 fit 5727.54 change 92.5MxComputeSimAnnealing(tsallis1996) evaluations 2169 fit 3925.56 change -611.4MxComputeSimAnnealing(tsallis1996) evaluations 2796 fit 4536.1 change 6.898  MxComputeSimAnnealing(tsallis1996) evaluations 3425 fit 4529.2 change -7.781MxComputeSimAnnealing(tsallis1996) evaluations 4051 fit 4515.05 change -660.1MxComputeSimAnnealing(tsallis1996) evaluations 4678 fit 5148.92 change -23.67MxComputeSimAnnealing(tsallis1996) evaluations 5308 fit 5144.44 change -23.64MxComputeSimAnnealing(tsallis1996) evaluations 5935 fit 4363.13 change -808.2MxComputeSimAnnealing(tsallis1996) evaluations 6563 fit 4848.18 change -817.8MxComputeSimAnnealing(tsallis1996) evaluations 7193 fit 5168.94 change -387.6MxComputeSimAnnealing(tsallis1996) evaluations 7817 fit 3920.25 change -1604 MxComputeSimAnnealing(tsallis1996) evaluations 8446 fit 5433.96 change 0    MxComputeSimAnnealing(tsallis1996) evaluations 9073 fit 7565.74 change 3652MxComputeSimAnnealing(tsallis1996) evaluations 9701 fit 3944.34 change -1.391MxComputeSimAnnealing(tsallis1996) evaluations 10327 fit 3943.24 change 7.889MxComputeSimAnnealing(tsallis1996) evaluations 10949 fit 3955.5 change -550  MxComputeSimAnnealing(tsallis1996) evaluations 11576 fit 3866.48 change -1248MxComputeSimAnnealing(tsallis1996) evaluations 12202 fit 5067.27 change 156.9MxComputeSimAnnealing(tsallis1996) evaluations 12826 fit 5574.79 change -2.617MxComputeSimAnnealing(tsallis1996) evaluations 13452 fit 5617.19 change 0     MxComputeSimAnnealing(tsallis1996) evaluations 14074 fit 3848.6 change -0.06176MxComputeSimAnnealing(tsallis1996) evaluations 14699 fit 3989 change 114.5     MxComputeSimAnnealing(tsallis1996) evaluations 15323 fit 4567.45 change 0 MxComputeSimAnnealing(tsallis1996) evaluations 15947 fit 5045.6 change -2.985MxComputeSimAnnealing(tsallis1996) evaluations 16570 fit 4006.4 change 160.3 MxComputeSimAnnealing(tsallis1996) evaluations 17191 fit 3849.17 change -180.7MxComputeSimAnnealing(tsallis1996) evaluations 17814 fit 4012.68 change -587.1MxComputeSimAnnealing(tsallis1996) evaluations 18437 fit 4051.46 change -559.9MxComputeSimAnnealing(tsallis1996) evaluations 19058 fit 4957.18 change 927.3 MxComputeSimAnnealing(tsallis1996) evaluations 19682 fit 5445.77 change 0    MxComputeSimAnnealing(tsallis1996) evaluations 20301 fit 4102.9 change 0 MxComputeSimAnnealing(tsallis1996) evaluations 20925 fit 3875.48 change 16.04MxComputeSimAnnealing(tsallis1996) evaluations 21546 fit 3833.48 change -1013MxComputeSimAnnealing(tsallis1996) evaluations 22167 fit 3833.17 change -1507MxComputeSimAnnealing(tsallis1996) evaluations 22789 fit 4237.95 change 405.1MxComputeSimAnnealing(tsallis1996) evaluations 23407 fit 3829.63 change -810 MxComputeSimAnnealing(tsallis1996) evaluations 24028 fit 4774.34 change 945.9MxComputeSimAnnealing(tsallis1996) evaluations 24650 fit 3827.32 change -3936MxComputeSimAnnealing(tsallis1996) evaluations 25271 fit 3958.66 change 86.57MxComputeSimAnnealing(tsallis1996) evaluations 25891 fit 4643.42 change 819.1MxComputeSimAnnealing(tsallis1996) evaluations 26509 fit 4363.4 change 539.7 MxComputeSimAnnealing(tsallis1996) evaluations 27128 fit 4465.56 change 44.8MxComputeSimAnnealing(tsallis1996) evaluations 27747 fit 3878.54 change 32.23MxComputeSimAnnealing(tsallis1996) evaluations 28365 fit 3821.42 change -1164MxComputeSimAnnealing(tsallis1996) evaluations 28983 fit 3830.96 change 10.96MxComputeSimAnnealing(tsallis1996) evaluations 29598 fit 3844.08 change 24.54MxComputeSimAnnealing(tsallis1996) evaluations 30218 fit 4210.76 change -370.9MxComputeSimAnnealing(tsallis1996) evaluations 30838 fit 4643.21 change 823.9 MxComputeSimAnnealing(tsallis1996) evaluations 31458 fit 3821.61 change 2.135MxComputeSimAnnealing(tsallis1996) evaluations 32078 fit 3818.72 change 0.005557MxComputeSimAnnealing(tsallis1996) evaluations 32695 fit 3818.72 change -858.5  MxComputeSimAnnealing(tsallis1996) evaluations 33312 fit 3824.25 change 3.433 MxComputeSimAnnealing(tsallis1996) evaluations 33930 fit 4767.5 change 948.9 MxComputeSimAnnealing(tsallis1996) evaluations 34546 fit 4050.37 change 228.9MxComputeSimAnnealing(tsallis1996) evaluations 35165 fit 4049.58 change -546.4MxComputeSimAnnealing(tsallis1996) evaluations 35779 fit 3818.51 change -0.07092MxComputeSimAnnealing(tsallis1996) evaluations 36397 fit 4761.15 change 1.171   MxComputeSimAnnealing(tsallis1996) evaluations 37017 fit 4609.31 change 412.3MxComputeSimAnnealing(tsallis1996) evaluations 37637 fit 3833.19 change 13.57MxComputeSimAnnealing(tsallis1996) evaluations 38256 fit 3818.49 change 0.0002186MxComputeSimAnnealing(tsallis1996) evaluations 38872 fit 3818.48 change -5.74    MxComputeSimAnnealing(tsallis1996) evaluations 39490 fit 3826.2 change 7.577 MxComputeSimAnnealing(tsallis1996) evaluations 40108 fit 3820.63 change -45.73MxComputeSimAnnealing(tsallis1996) evaluations 40725 fit 3826.99 change -330.8MxComputeSimAnnealing(tsallis1996) evaluations 41342 fit 3818.49 change -2.956MxComputeSimAnnealing(tsallis1996) evaluations 41956 fit 3819 change 0.5201   MxComputeSimAnnealing(tsallis1996) evaluations 42574 fit 3818.48 change -0.0357MxComputeSimAnnealing(tsallis1996) evaluations 43193 fit 3818.48 change 0.0002501MxComputeSimAnnealing(tsallis1996) evaluations 43810 fit 3871.3 change -725.2    MxComputeSimAnnealing(tsallis1996) evaluations 44429 fit 3818.58 change 0.09307                                                                               
```

``` r
res_alt <- mx_profiles(df, classes = 4, variances = "varying")
compare <- list(res[[4]], res_alt)
table_fit(compare)
```


Table: Comparing competing theoretical models

| Name|    LL| Parameters|  BIC| Entropy| prob_min| prob_max| n_min| n_max|
|----:|-----:|----------:|----:|-------:|--------:|--------:|-----:|-----:|
|    1| -1916|         23| 3976|    0.75|     0.81|     0.92|  0.16|  0.34|
|    2| -1909|         35| 4037|    0.78|     0.84|     0.92|  0.16|  0.32|



The alternative model incurs 12 additional parameters for the free variances.
Yet, it has a higher BIC, which indicates that this additional complexity does not outweigh the increase in fit.

## Interpreting the Final Class Solution

To interpret the final class solution,
we first reorder the 4-class model by class size.
This helps prevent label switching.


``` r
res_final <- mx_switch_labels(res[[4]])
```

```
#> MxComputeNumericDeriv 86/276MxComputeNumericDeriv 252/276                             
```

The 4-class model yielded classes of reasonable size;
using `class_pro`the largest class comprised 33%,
and the smallest comprised 16% of cases.
However, the entropy was low, $S = .75$, indicating poor class separability.
Furthermore, the posterior classification probability ranged from $[.81, .92]$, which means that at least some classes had a high classification error.
We produce a table of the results below.


``` r
table_results(res_final, columns = c("label", "est", "se", "confint",
    "class"))
```



Table: Four-class model results

|label                |  est|   se|confint      |class  |
|:--------------------|----:|----:|:------------|:------|
|Means.burdened       | 3.27| 0.04|[3.18, 3.36] |class1 |
|Means.trapped        | 1.28| 0.05|[1.18, 1.38] |class1 |
|Means.negaffect      | 2.31| 0.06|[2.20, 2.42] |class1 |
|Means.loneliness     | 2.73| 0.04|[2.64, 2.82] |class1 |
|Variances.burdened   | 0.23| 0.02|[0.19, 0.27] |class1 |
|Variances.trapped    | 0.17| 0.02|[0.14, 0.20] |class1 |
|Variances.negaffect  | 0.31| 0.02|[0.27, 0.36] |class1 |
|Variances.loneliness | 0.24| 0.02|[0.20, 0.28] |class1 |
|Means.burdened       | 3.40| 0.06|[3.28, 3.52] |class2 |
|Means.trapped        | 2.27| 0.06|[2.15, 2.38] |class2 |
|Means.negaffect      | 2.81| 0.06|[2.70, 2.93] |class2 |
|Means.loneliness     | 2.79| 0.06|[2.66, 2.91] |class2 |
|Means.burdened       | 4.25| 0.07|[4.12, 4.38] |class3 |
|Means.trapped        | 2.67| 0.05|[2.58, 2.77] |class3 |
|Means.negaffect      | 2.92| 0.06|[2.80, 3.03] |class3 |
|Means.loneliness     | 2.01| 0.06|[1.89, 2.14] |class3 |
|Means.burdened       | 2.38| 0.06|[2.26, 2.50] |class4 |
|Means.trapped        | 0.38| 0.05|[0.28, 0.49] |class4 |
|Means.negaffect      | 1.78| 0.07|[1.65, 1.91] |class4 |
|Means.loneliness     | 3.18| 0.06|[3.07, 3.30] |class4 |
|mix4.weights[1,1]    | 1.00|   NA|NA           |NA     |
|mix4.weights[1,2]    | 0.86| 0.15|[0.56, 1.15] |NA     |
|mix4.weights[1,3]    | 0.66| 0.11|[0.44, 0.88] |NA     |
|mix4.weights[1,4]    | 0.47| 0.08|[0.32, 0.63] |NA     |



The results are best interpreted by examining a plot of the model and data, however.
Relevant plot functions are `plot_bivariate()`, `plot_density()`, and `plot_profiles()`.
However, we omit the density plots, because `plot_bivariate()` also includes them.


``` r
plot_bivariate(res_final)
```
![](lca_confirmatory_files/figure-html/unnamed-chunk-19-1.png)<!-- -->
<div class="figure">
<img src="lpa_bivariate.png" alt="Bivariate profile plot" width="80%" />
<p class="caption">Bivariate profile plot</p>
</div>

On the diagonal of the bivariate plot are weighted density plots:
normal approximations of the density function of observed data,
weighed by class probability.
On the off-diagonal are plots for each pair of indicators,
with the class means indicated by a point,
class standard deviations indicated by lines,
and covariances indicated by circles.
As this model has zero covariances,
all circles are round (albeit warped by the different scales of the X and Y axes)

The marginal density plots show that trappedness distinguishes classes rather well.
For all other indicators, groups are not always clearly separated in terms of marginal density: class 2 and 3 coalesce on negative affect, 1 and 2 coalesce on loneliness, and 1 and 2 coalesce on burden.
Nevertheless, the off-diagonal scatterplots show reasonable bivariate separation for all classes.

We can obtain a more classic profile plot using `plot_profiles(res_final)`.
This plot conveys less information than the bivariate plot,
but is readily interpretable.
Below is a comparison between the most common type of visualization
for LPA, and the best-practices visualization provided by `tidySEM`.
Note that the best practices plot includes class means and error bars,
standard deviations,
and a ribbon plot of raw data weighted by class probability to indicate how well the classes describe the observed distribution.
The overlap between the classes is clearly visible in this figure;
this is why the entropy and classification probabilities are relatively low.

Based on the bivariate plot, we can label class 1 as the *balanced* type (33%),
class 2 as the *imbalanced* type (29%), class 3 as the *entrapped* type (22%),
and class 4 as the *lonely* type (16%).
Note however that the observed classes do not match the hypothesized pattern of class parameters exactly.


``` r
plot_profiles(res_final)
```
![](lca_confirmatory_files/figure-html/unnamed-chunk-22-1.png)<!-- -->![](lca_confirmatory_files/figure-html/unnamed-chunk-22-2.png)<!-- -->
<div class="figure">
<img src="lpa_profiles.png" alt="Bivariate profile plot" width="80%" />
<p class="caption">Bivariate profile plot</p>
</div>

## Auxiliary Analyses

We may want to compare the different classes on auxiliary variables or models.
The `BCH()` function applies three-step analysis,
which compares the classes using a multi-group model,
controlling for classification error.
We consider two examples: a single variable, and an auxiliary model.

### Comparing Means or Proportions Across Classes

For a single (continuous or ordinal) variable,
we can call the BCH function and simply supply the auxiliary variable to the `data` argument, omitting the `model` argument.
Below, we estimate an auxiliary model to compare the sex of patients between classes:


``` r
aux_sex <- BCH(res_final, data = zegwaard_carecompass$sexpatient)
```


To obtain an omnibus likelihood ratio test of the significance of these sex differences across classes,
as well as pairwise comparisons between classes,
use `lr_test(aux_sex)`.
The results indicate that there are significant sex differences across classes, $\Delta LL(1) = 8.7, p = .003$.
Pairwise comparisons indicate that class 3 differs significantly from classes 1 and 2.
The results can be reported in probability scale using `table_prob(aux_sex)`.
It appears that the entrapped class disproportionately cares for female patients.

### Comparing Auxiliary Models Across Classes

We can also compare a simple model between classes.
Specifically, we will examine whether the distance predicts the frequency of visits differently across classes (treated as continuous).


``` r
df_aux <- zegwaard_carecompass[, c("freqvisit", "distance")]
df_aux$freqvisit <- as.numeric(df_aux$freqvisit)
aux_model <- BCH(res_final, model = "freqvisit ~ distance", data = df_aux)
```


To obtain an omnibus likelihood ratio test of the difference in regression coefficients across classes
and pairwise comparisons between classes,
use `lr_test(aux_model, compare = "A")`.
The results indicate that there are no significant sex differences across classes, $\Delta LL(3) = 0.98, p = .81$.
The results can be reported using `table_results(aux_model)`:


``` r
table_results(aux_model)
```


```
#>                                label    est_sig     se pval
#> 1  Regressions.freqvisit.ON.distance       0.00   0.00 0.80
#> 2                    Means.freqvisit    3.99***   0.18 0.00
#> 3                     Means.distance  155.25***   3.80 0.00
#> 4                Variances.freqvisit    0.53***   0.06 0.00
#> 5                 Variances.distance 2464.07*** 266.70 0.00
#> 6  Regressions.freqvisit.ON.distance       0.00   0.00 0.77
#> 7                    Means.freqvisit    3.66***   0.43 0.00
#> 8                     Means.distance  159.52***   2.79 0.00
#> 9                Variances.freqvisit    1.19***   0.14 0.00
#> 10                Variances.distance 1144.27*** 133.42 0.00
#> 11 Regressions.freqvisit.ON.distance      -0.00   0.00 0.35
#> 12                   Means.freqvisit    3.95***   0.27 0.00
#> 13                    Means.distance  147.24***   6.09 0.00
#> 14               Variances.freqvisit    1.29***   0.17 0.00
#> 15                Variances.distance 4200.66*** 558.63 0.00
#> 16 Regressions.freqvisit.ON.distance      -0.00   0.00 0.91
#> 17                   Means.freqvisit    3.32***   0.38 0.00
#> 18                    Means.distance  167.02***   7.06 0.00
#> 19               Variances.freqvisit    1.48***   0.23 0.00
#> 20                Variances.distance 3989.68*** 630.52 0.00
#>               confint  group
#> 1       [-0.00, 0.00] class1
#> 2        [3.64, 4.35] class1
#> 3    [147.80, 162.70] class1
#> 4        [0.42, 0.64] class1
#> 5  [1941.34, 2986.79] class1
#> 6       [-0.00, 0.01] class2
#> 7        [2.81, 4.51] class2
#> 8    [154.05, 164.98] class2
#> 9        [0.92, 1.46] class2
#> 10  [882.77, 1405.76] class2
#> 11      [-0.00, 0.00] class3
#> 12       [3.43, 4.47] class3
#> 13   [135.30, 159.18] class3
#> 14       [0.95, 1.63] class3
#> 15 [3105.75, 5295.56] class3
#> 16      [-0.00, 0.00] class4
#> 17       [2.57, 4.08] class4
#> 18   [153.18, 180.86] class4
#> 19       [1.02, 1.93] class4
#> 20 [2753.87, 5225.48] class4
```


## Predicting class membership

This LCA model was developed to help classify care providers in a clinical context,
so that mental healthcare professionals can provide tailored support to those who take care of their clients.
In `tidySEM`, it is possible to predict class membership for new data.
Imagine that we administer the care compass questionnaire to a new individual.
We can assign their scale scores to a `data.frame`,
and supply it to the `predict_class()` function (in previous versions, we overloaded the `predict()` function) via the `newdata` argument.
The result includes the individual's most likely class,
as well as posterior probabilities for all classes.


``` r
df_new <- data.frame(burdened = 2, trapped = 0.5, negaffect = 1.5,
    loneliness = 4)
predict_class(res_final, newdata = df_new)
#>       class1  class2  class3 class4 predicted
#> [1,] 0.00081 4.6e-08 1.4e-15      1         4
```
