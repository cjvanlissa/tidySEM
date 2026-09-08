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
#> MxComputeSimAnnealing(tsallis1996) evaluations 978 fit 6955.34 change 3.174MxComputeSimAnnealing(tsallis1996) evaluations 2600 fit 7553.37 change 43.21MxComputeSimAnnealing(tsallis1996) evaluations 4206 fit 4518.89 change 139.6MxComputeSimAnnealing(tsallis1996) evaluations 5813 fit 4088.96 change 25.59MxComputeSimAnnealing(tsallis1996) evaluations 7413 fit 4062.98 change 0.1829MxComputeSimAnnealing(tsallis1996) evaluations 8998 fit 4244.39 change 121.2 MxComputeSimAnnealing(tsallis1996) evaluations 10572 fit 4062.74 change -26.14MxComputeSimAnnealing(tsallis1996) evaluations 12141 fit 4062.74 change -152.4MxComputeSimAnnealing(tsallis1996) evaluations 13704 fit 4063.02 change -2237 MxComputeSimAnnealing(tsallis1996) evaluations 15275 fit 7542.52 change 3478                                                                             
#> MxComputeSimAnnealing(tsallis1996) evaluations 293 fit 4882.66 change 0.1274MxComputeSimAnnealing(tsallis1996) evaluations 1375 fit 7424 change 27.64   MxComputeSimAnnealing(tsallis1996) evaluations 2454 fit 5582.35 change 699.7MxComputeSimAnnealing(tsallis1996) evaluations 3529 fit 5424.59 change 26.11MxComputeSimAnnealing(tsallis1996) evaluations 4599 fit 8909.56 change 2896 MxComputeSimAnnealing(tsallis1996) evaluations 5677 fit 4002.29 change -1657MxComputeSimAnnealing(tsallis1996) evaluations 6748 fit 3989.94 change -1526MxComputeSimAnnealing(tsallis1996) evaluations 7821 fit 3915.27 change -717.8MxComputeSimAnnealing(tsallis1996) evaluations 8891 fit 3905.07 change 0.6553MxComputeSimAnnealing(tsallis1996) evaluations 9953 fit 3906.29 change -1124 MxComputeSimAnnealing(tsallis1996) evaluations 11009 fit 3902.94 change -259.1MxComputeSimAnnealing(tsallis1996) evaluations 12062 fit 4810.24 change 758.3 MxComputeSimAnnealing(tsallis1996) evaluations 13106 fit 3902.73 change 0.008203MxComputeSimAnnealing(tsallis1996) evaluations 14154 fit 3902.98 change -917.4  MxComputeSimAnnealing(tsallis1996) evaluations 15204 fit 5668.67 change 1732  MxComputeSimAnnealing(tsallis1996) evaluations 16258 fit 3902.72 change -0.1153MxComputeSimAnnealing(tsallis1996) evaluations 17313 fit 3902.71 change -1761  MxComputeSimAnnealing(tsallis1996) evaluations 18367 fit 4320.06 change 378.7MxComputeSimAnnealing(tsallis1996) evaluations 19419 fit 5672.78 change 1770 MxComputeSimAnnealing(tsallis1996) evaluations 20464 fit 4970.23 change 1068MxComputeSimAnnealing(tsallis1996) evaluations 21517 fit 3903.72 change -0.5596MxComputeSimAnnealing(tsallis1996) evaluations 22565 fit 3902.71 change -5235                                                                               
#> MxComputeSimAnnealing(tsallis1996) evaluations 721 fit 8154.64 change 2048MxComputeSimAnnealing(tsallis1996) evaluations 1527 fit 14270.8 change 5809MxComputeSimAnnealing(tsallis1996) evaluations 2340 fit 4649.23 change 6.321MxComputeSimAnnealing(tsallis1996) evaluations 3146 fit 4707.42 change 58.19MxComputeSimAnnealing(tsallis1996) evaluations 3954 fit 4705.7 change 670.4 MxComputeSimAnnealing(tsallis1996) evaluations 4754 fit 4643.16 change -8.845MxComputeSimAnnealing(tsallis1996) evaluations 5561 fit 4665.03 change 52.37 MxComputeSimAnnealing(tsallis1996) evaluations 6362 fit 4598.32 change 500.4MxComputeSimAnnealing(tsallis1996) evaluations 7161 fit 9526.56 change 3460 MxComputeSimAnnealing(tsallis1996) evaluations 7963 fit 4572.99 change 1.074MxComputeSimAnnealing(tsallis1996) evaluations 8762 fit 4709.32 change 873.9MxComputeSimAnnealing(tsallis1996) evaluations 9564 fit 3837.98 change -904.8MxComputeSimAnnealing(tsallis1996) evaluations 10356 fit 4560.41 change 678.5MxComputeSimAnnealing(tsallis1996) evaluations 11149 fit 3845.93 change 9.835MxComputeSimAnnealing(tsallis1996) evaluations 11943 fit 4564.46 change 58.19MxComputeSimAnnealing(tsallis1996) evaluations 12737 fit 4757.49 change 270.1MxComputeSimAnnealing(tsallis1996) evaluations 13532 fit 3918.63 change 85.96MxComputeSimAnnealing(tsallis1996) evaluations 14324 fit 4175.25 change 289.7MxComputeSimAnnealing(tsallis1996) evaluations 15115 fit 3880.96 change -687.2MxComputeSimAnnealing(tsallis1996) evaluations 15901 fit 3855.69 change -2372 MxComputeSimAnnealing(tsallis1996) evaluations 16687 fit 3832.88 change -6513MxComputeSimAnnealing(tsallis1996) evaluations 17479 fit 4645.63 change -143.6MxComputeSimAnnealing(tsallis1996) evaluations 18264 fit 3833.86 change 1.259 MxComputeSimAnnealing(tsallis1996) evaluations 19051 fit 3832.64 change -748.5MxComputeSimAnnealing(tsallis1996) evaluations 19837 fit 3833.09 change -3158 MxComputeSimAnnealing(tsallis1996) evaluations 20627 fit 4786.05 change -4.22MxComputeSimAnnealing(tsallis1996) evaluations 21414 fit 3832.61 change -492.5MxComputeSimAnnealing(tsallis1996) evaluations 22202 fit 4505.34 change 672.7 MxComputeSimAnnealing(tsallis1996) evaluations 22986 fit 4673.44 change 840.9MxComputeSimAnnealing(tsallis1996) evaluations 23774 fit 4498.84 change 662  MxComputeSimAnnealing(tsallis1996) evaluations 24564 fit 3833.36 change 0.7969MxComputeSimAnnealing(tsallis1996) evaluations 25350 fit 3832.52 change -0.05661MxComputeSimAnnealing(tsallis1996) evaluations 26140 fit 3877.46 change -30.05  MxComputeSimAnnealing(tsallis1996) evaluations 26928 fit 3835.42 change 2.895 MxComputeSimAnnealing(tsallis1996) evaluations 27719 fit 3832.6 change 0.08327MxComputeSimAnnealing(tsallis1996) evaluations 28506 fit 3832.6 change -0.1412MxComputeSimAnnealing(tsallis1996) evaluations 29294 fit 3832.52 change -0.02464                                                                                
#> MxComputeSimAnnealing(tsallis1996) evaluations 443 fit 4607.56 change 7.887MxComputeSimAnnealing(tsallis1996) evaluations 1090 fit 4741.88 change 1.158MxComputeSimAnnealing(tsallis1996) evaluations 1731 fit 4607.56 change 7.887MxComputeSimAnnealing(tsallis1996) evaluations 2374 fit 4599.67 change -6.374MxComputeSimAnnealing(tsallis1996) evaluations 3016 fit 4348.92 change -338.4MxComputeSimAnnealing(tsallis1996) evaluations 3662 fit 4374.51 change -231.5MxComputeSimAnnealing(tsallis1996) evaluations 4304 fit 4612.52 change -74.79MxComputeSimAnnealing(tsallis1996) evaluations 4950 fit 4600.14 change -6.348MxComputeSimAnnealing(tsallis1996) evaluations 5594 fit 4601.44 change -6.342MxComputeSimAnnealing(tsallis1996) evaluations 6237 fit 4624.93 change -6.777MxComputeSimAnnealing(tsallis1996) evaluations 6880 fit 4628.87 change -41.88MxComputeSimAnnealing(tsallis1996) evaluations 7523 fit 4631.94 change 2.001 MxComputeSimAnnealing(tsallis1996) evaluations 8162 fit 6406.44 change -1232MxComputeSimAnnealing(tsallis1996) evaluations 8801 fit 5599.76 change 1754 MxComputeSimAnnealing(tsallis1996) evaluations 9446 fit 6378.21 change 2537MxComputeSimAnnealing(tsallis1996) evaluations 10087 fit 4047.69 change 9.067MxComputeSimAnnealing(tsallis1996) evaluations 10731 fit 4021.91 change -4.777MxComputeSimAnnealing(tsallis1996) evaluations 11368 fit 3969.69 change -613.5MxComputeSimAnnealing(tsallis1996) evaluations 12006 fit 4670.59 change 769.2 MxComputeSimAnnealing(tsallis1996) evaluations 12647 fit 4480.46 change 7.024MxComputeSimAnnealing(tsallis1996) evaluations 13285 fit 3827.46 change -1319MxComputeSimAnnealing(tsallis1996) evaluations 13924 fit 3830.59 change -180.8MxComputeSimAnnealing(tsallis1996) evaluations 14559 fit 3822.2 change -0.1258MxComputeSimAnnealing(tsallis1996) evaluations 15195 fit 3831.08 change 9.561 MxComputeSimAnnealing(tsallis1996) evaluations 15831 fit 5920.47 change 2099 MxComputeSimAnnealing(tsallis1996) evaluations 16471 fit 3821.28 change 0.225MxComputeSimAnnealing(tsallis1996) evaluations 17110 fit 4023.13 change 2.399MxComputeSimAnnealing(tsallis1996) evaluations 17748 fit 4642.33 change -249.4MxComputeSimAnnealing(tsallis1996) evaluations 18384 fit 3827.29 change 3.068 MxComputeSimAnnealing(tsallis1996) evaluations 19022 fit 3820.69 change -0.092MxComputeSimAnnealing(tsallis1996) evaluations 19657 fit 4008.98 change 188.4 MxComputeSimAnnealing(tsallis1996) evaluations 20295 fit 3826.07 change -1082MxComputeSimAnnealing(tsallis1996) evaluations 20928 fit 3820.47 change -0.02837MxComputeSimAnnealing(tsallis1996) evaluations 21565 fit 3820.46 change -218.4  MxComputeSimAnnealing(tsallis1996) evaluations 22199 fit 4927.55 change 653.6 MxComputeSimAnnealing(tsallis1996) evaluations 22835 fit 3820.9 change -1554 MxComputeSimAnnealing(tsallis1996) evaluations 23471 fit 3820.44 change -246.3MxComputeSimAnnealing(tsallis1996) evaluations 24106 fit 3820.44 change -0.000218MxComputeSimAnnealing(tsallis1996) evaluations 24737 fit 3820.75 change 0.3078   MxComputeSimAnnealing(tsallis1996) evaluations 25373 fit 4077.3 change 4.234  MxComputeSimAnnealing(tsallis1996) evaluations 26006 fit 4909.86 change 1089MxComputeSimAnnealing(tsallis1996) evaluations 26641 fit 3821.41 change -7.659MxComputeSimAnnealing(tsallis1996) evaluations 27279 fit 4079.77 change 13.03 MxComputeSimAnnealing(tsallis1996) evaluations 27910 fit 4906.58 change -11.18MxComputeSimAnnealing(tsallis1996) evaluations 28544 fit 3822.17 change 1.721 MxComputeSimAnnealing(tsallis1996) evaluations 29178 fit 3820.88 change 0.4324MxComputeSimAnnealing(tsallis1996) evaluations 29812 fit 4926.96 change 358.2 MxComputeSimAnnealing(tsallis1996) evaluations 30446 fit 3820.61 change -18.77MxComputeSimAnnealing(tsallis1996) evaluations 31078 fit 3820.5 change -842   MxComputeSimAnnealing(tsallis1996) evaluations 31712 fit 3820.65 change 0.2191MxComputeSimAnnealing(tsallis1996) evaluations 32348 fit 3820.44 change -265.3MxComputeSimAnnealing(tsallis1996) evaluations 32984 fit 3820.82 change 0.3914MxComputeSimAnnealing(tsallis1996) evaluations 33618 fit 3820.43 change -10.72MxComputeSimAnnealing(tsallis1996) evaluations 34247 fit 4001.81 change 181.2 MxComputeSimAnnealing(tsallis1996) evaluations 34881 fit 3821.32 change 0.6633MxComputeSimAnnealing(tsallis1996) evaluations 35517 fit 3820.43 change 2.989e-05                                                                                 
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
|equal var 5 | -1910| 28| 513| 3995|    0.76|  0.64|  0.92|  0.08|  0.34|



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

|null |alt  |   lr| df|    p|   w2| p_w2|
|:----|:----|----:|--:|----:|----:|----:|
|mix1 |mix2 | 10.2|  5| 0.00| 0.82| 0.00|
|mix2 |mix3 |  5.3|  5| 0.00| 0.44| 0.00|
|mix3 |mix4 |  4.1|  5| 0.00| 0.14| 0.00|
|mix4 |mix5 |  1.6|  5| 0.05| 0.03| 0.04|



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
#> MxComputeSimAnnealing(tsallis1996) evaluations 416 fit 4899.78 change -21.74MxComputeSimAnnealing(tsallis1996) evaluations 1211 fit 4625.97 change -39.85MxComputeSimAnnealing(tsallis1996) evaluations 2003 fit 8342.72 change 461.7 MxComputeSimAnnealing(tsallis1996) evaluations 2800 fit 4971.3 change 0     MxComputeSimAnnealing(tsallis1996) evaluations 3597 fit 4925.11 change 569.1MxComputeSimAnnealing(tsallis1996) evaluations 4392 fit 4787.69 change 1.708MxComputeSimAnnealing(tsallis1996) evaluations 5185 fit 4970.65 change 1.532MxComputeSimAnnealing(tsallis1996) evaluations 5975 fit 4658.54 change 12.93MxComputeSimAnnealing(tsallis1996) evaluations 6770 fit 4765.52 change -2.473MxComputeSimAnnealing(tsallis1996) evaluations 7564 fit 4974.41 change 15.08 MxComputeSimAnnealing(tsallis1996) evaluations 8360 fit 4002.63 change -138.5MxComputeSimAnnealing(tsallis1996) evaluations 9155 fit 4004.44 change -617.1MxComputeSimAnnealing(tsallis1996) evaluations 9949 fit 3865.48 change -3156 MxComputeSimAnnealing(tsallis1996) evaluations 10742 fit 4877.94 change -3.227MxComputeSimAnnealing(tsallis1996) evaluations 11534 fit 4618.65 change 784.4 MxComputeSimAnnealing(tsallis1996) evaluations 12326 fit 4721.55 change 875.7MxComputeSimAnnealing(tsallis1996) evaluations 13114 fit 4593.86 change -8.922MxComputeSimAnnealing(tsallis1996) evaluations 13900 fit 4024.7 change -786.9 MxComputeSimAnnealing(tsallis1996) evaluations 14692 fit 4807.24 change 986.8MxComputeSimAnnealing(tsallis1996) evaluations 15469 fit 4766.09 change -26.19MxComputeSimAnnealing(tsallis1996) evaluations 16254 fit 4625.61 change -2.095MxComputeSimAnnealing(tsallis1996) evaluations 17045 fit 3864.19 change -342.3MxComputeSimAnnealing(tsallis1996) evaluations 17833 fit 4677.77 change 40.07 MxComputeSimAnnealing(tsallis1996) evaluations 18624 fit 4770.02 change 13.27MxComputeSimAnnealing(tsallis1996) evaluations 19415 fit 4682.54 change 858  MxComputeSimAnnealing(tsallis1996) evaluations 20206 fit 4670.21 change 851.1MxComputeSimAnnealing(tsallis1996) evaluations 20994 fit 4428.46 change 609.7MxComputeSimAnnealing(tsallis1996) evaluations 21780 fit 4681.58 change -6264MxComputeSimAnnealing(tsallis1996) evaluations 22570 fit 3852.43 change 33.31MxComputeSimAnnealing(tsallis1996) evaluations 23362 fit 3826.38 change -846.6MxComputeSimAnnealing(tsallis1996) evaluations 24152 fit 3900.6 change 80.3   MxComputeSimAnnealing(tsallis1996) evaluations 24942 fit 4691 change 349.7 MxComputeSimAnnealing(tsallis1996) evaluations 25727 fit 4771.84 change 953.2MxComputeSimAnnealing(tsallis1996) evaluations 26515 fit 3820.47 change -37.03MxComputeSimAnnealing(tsallis1996) evaluations 27303 fit 3829.43 change 2.795 MxComputeSimAnnealing(tsallis1996) evaluations 28090 fit 3818.58 change -35.61MxComputeSimAnnealing(tsallis1996) evaluations 28876 fit 3828.95 change -756.4MxComputeSimAnnealing(tsallis1996) evaluations 29661 fit 4633.1 change -67.64 MxComputeSimAnnealing(tsallis1996) evaluations 30448 fit 4636.16 change 817.7MxComputeSimAnnealing(tsallis1996) evaluations 31238 fit 3818.54 change -0.4367MxComputeSimAnnealing(tsallis1996) evaluations 32026 fit 3826.62 change 8.121  MxComputeSimAnnealing(tsallis1996) evaluations 32812 fit 4181.9 change 363.4 MxComputeSimAnnealing(tsallis1996) evaluations 33597 fit 3818.49 change -0.01435MxComputeSimAnnealing(tsallis1996) evaluations 34385 fit 3847.18 change 28.61   MxComputeSimAnnealing(tsallis1996) evaluations 35170 fit 3818.52 change -807.6MxComputeSimAnnealing(tsallis1996) evaluations 35954 fit 9158.65 change 5328  MxComputeSimAnnealing(tsallis1996) evaluations 36738 fit 4710.94 change 882.4MxComputeSimAnnealing(tsallis1996) evaluations 37517 fit 3818.48 change -783.4MxComputeSimAnnealing(tsallis1996) evaluations 38301 fit 3818.49 change -0.0581MxComputeSimAnnealing(tsallis1996) evaluations 39088 fit 3839.01 change 20.54  MxComputeSimAnnealing(tsallis1996) evaluations 39874 fit 3818.49 change -1.57MxComputeSimAnnealing(tsallis1996) evaluations 40657 fit 4505.2 change 686.3 MxComputeSimAnnealing(tsallis1996) evaluations 41438 fit 3818.49 change -96.65MxComputeSimAnnealing(tsallis1996) evaluations 42218 fit 3871.43 change -45.87MxComputeSimAnnealing(tsallis1996) evaluations 43000 fit 3819.81 change -894.6MxComputeSimAnnealing(tsallis1996) evaluations 43780 fit 3829.67 change -225.9MxComputeSimAnnealing(tsallis1996) evaluations 44560 fit 3818.48 change -9.99                                                                              
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
#> MxComputeNumericDeriv 169/276                             
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

|label                       |  est|   se|confint      |class  |
|:---------------------------|----:|----:|:------------|:------|
|Means.burdened.class1       | 3.27| 0.04|[3.18, 3.36] |class1 |
|Means.trapped.class1        | 1.28| 0.05|[1.18, 1.38] |class1 |
|Means.negaffect.class1      | 2.31| 0.06|[2.20, 2.42] |class1 |
|Means.loneliness.class1     | 2.73| 0.04|[2.64, 2.82] |class1 |
|Variances.burdened.class1   | 0.23| 0.02|[0.19, 0.27] |class1 |
|Variances.trapped.class1    | 0.17| 0.02|[0.14, 0.20] |class1 |
|Variances.negaffect.class1  | 0.31| 0.02|[0.27, 0.36] |class1 |
|Variances.loneliness.class1 | 0.24| 0.02|[0.20, 0.28] |class1 |
|Means.burdened.class2       | 3.40| 0.06|[3.28, 3.52] |class2 |
|Means.trapped.class2        | 2.27| 0.06|[2.15, 2.38] |class2 |
|Means.negaffect.class2      | 2.81| 0.06|[2.70, 2.93] |class2 |
|Means.loneliness.class2     | 2.79| 0.06|[2.66, 2.91] |class2 |
|Means.burdened.class3       | 4.25| 0.07|[4.12, 4.38] |class3 |
|Means.trapped.class3        | 2.67| 0.05|[2.58, 2.77] |class3 |
|Means.negaffect.class3      | 2.92| 0.06|[2.80, 3.03] |class3 |
|Means.loneliness.class3     | 2.01| 0.06|[1.89, 2.14] |class3 |
|Means.burdened.class4       | 2.38| 0.06|[2.26, 2.50] |class4 |
|Means.trapped.class4        | 0.38| 0.05|[0.28, 0.49] |class4 |
|Means.negaffect.class4      | 1.78| 0.07|[1.65, 1.91] |class4 |
|Means.loneliness.class4     | 3.18| 0.06|[3.07, 3.30] |class4 |
|mix4.weights[1,1].NA        | 1.00|   NA|NA           |NA     |
|mix4.weights[1,2].NA        | 0.86| 0.15|[0.56, 1.15] |NA     |
|mix4.weights[1,3].NA        | 0.66| 0.11|[0.44, 0.88] |NA     |
|mix4.weights[1,4].NA        | 0.47| 0.08|[0.32, 0.63] |NA     |



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
#>                                       label    est_sig     se pval            confint
#> 1  Regressions.freqvisit.ON.distance.class1       0.00   0.00 0.80      [-0.00, 0.00]
#> 2                    Means.freqvisit.class1    3.99***   0.18 0.00       [3.64, 4.35]
#> 3                     Means.distance.class1  155.25***   3.80 0.00   [147.80, 162.70]
#> 4                Variances.freqvisit.class1    0.53***   0.06 0.00       [0.42, 0.64]
#> 5                 Variances.distance.class1 2464.07*** 266.69 0.00 [1941.37, 2986.77]
#> 6  Regressions.freqvisit.ON.distance.class2       0.00   0.00 0.77      [-0.00, 0.01]
#> 7                    Means.freqvisit.class2    3.66***   0.43 0.00       [2.81, 4.51]
#> 8                     Means.distance.class2  159.52***   2.79 0.00   [154.05, 164.98]
#> 9                Variances.freqvisit.class2    1.19***   0.14 0.00       [0.92, 1.46]
#> 10                Variances.distance.class2 1144.27*** 133.42 0.00  [882.77, 1405.77]
#> 11 Regressions.freqvisit.ON.distance.class3      -0.00   0.00 0.35      [-0.00, 0.00]
#> 12                   Means.freqvisit.class3    3.95***   0.27 0.00       [3.43, 4.47]
#> 13                    Means.distance.class3  147.24***   6.09 0.00   [135.30, 159.18]
#> 14               Variances.freqvisit.class3    1.29***   0.17 0.00       [0.95, 1.63]
#> 15                Variances.distance.class3 4200.66*** 558.51 0.00 [3105.99, 5295.32]
#> 16 Regressions.freqvisit.ON.distance.class4      -0.00   0.00 0.91      [-0.00, 0.00]
#> 17                   Means.freqvisit.class4    3.32***   0.38 0.00       [2.57, 4.08]
#> 18                    Means.distance.class4  167.02***   7.06 0.00   [153.18, 180.86]
#> 19               Variances.freqvisit.class4    1.48***   0.23 0.00       [1.02, 1.93]
#> 20                Variances.distance.class4 3989.68*** 630.61 0.00 [2753.69, 5225.66]
#>     group
#> 1  class1
#> 2  class1
#> 3  class1
#> 4  class1
#> 5  class1
#> 6  class2
#> 7  class2
#> 8  class2
#> 9  class2
#> 10 class2
#> 11 class3
#> 12 class3
#> 13 class3
#> 14 class3
#> 15 class3
#> 16 class4
#> 17 class4
#> 18 class4
#> 19 class4
#> 20 class4
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
