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
|burdened   | 509|    0.01|    509|  3.4|    3.4| 0.75|  1.20| 5.3|     0.17|      6.5|
|trapped    | 505|    0.02|    505|  1.7|    1.8| 0.90| -0.86| 3.8|    -1.03|      5.4|
|negaffect  | 506|    0.01|    506|  2.5|    2.5| 0.69|  0.71| 5.0|     0.08|      6.5|
|loneliness | 510|    0.01|    510|  2.7|    2.7| 0.62|  0.98| 4.2|    -0.33|      6.3|



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
#> MxComputeSimAnnealing(tsallis1996) evaluations 1519 fit 11230.4 change -59.65MxComputeSimAnnealing(tsallis1996) evaluations 3094 fit 9278.72 change 53.85 MxComputeSimAnnealing(tsallis1996) evaluations 4664 fit 8764.84 change 4251 MxComputeSimAnnealing(tsallis1996) evaluations 6215 fit 6044.19 change -1726MxComputeSimAnnealing(tsallis1996) evaluations 7748 fit 7581.64 change 3518 MxComputeSimAnnealing(tsallis1996) evaluations 9273 fit 4064.01 change -892MxComputeSimAnnealing(tsallis1996) evaluations 10782 fit 4067.28 change 4.52MxComputeSimAnnealing(tsallis1996) evaluations 12298 fit 4064.85 change -3427MxComputeSimAnnealing(tsallis1996) evaluations 13812 fit 4084.96 change 22.15MxComputeSimAnnealing(tsallis1996) evaluations 15324 fit 4062.74 change -0.06831                                                                                
#> MxComputeSimAnnealing(tsallis1996) evaluations 698 fit 5446.16 change -7.221MxComputeSimAnnealing(tsallis1996) evaluations 1735 fit 8815.92 change 1407 MxComputeSimAnnealing(tsallis1996) evaluations 2781 fit 9972.39 change 3762MxComputeSimAnnealing(tsallis1996) evaluations 3813 fit 5475.45 change 11.74MxComputeSimAnnealing(tsallis1996) evaluations 4852 fit 6304.15 change 86.31MxComputeSimAnnealing(tsallis1996) evaluations 5883 fit 5483.48 change 1152 MxComputeSimAnnealing(tsallis1996) evaluations 6916 fit 4696.2 change 377.8MxComputeSimAnnealing(tsallis1996) evaluations 7947 fit 5294.59 change -4006MxComputeSimAnnealing(tsallis1996) evaluations 8977 fit 3909.32 change -85.78MxComputeSimAnnealing(tsallis1996) evaluations 10002 fit 3908.18 change -4095MxComputeSimAnnealing(tsallis1996) evaluations 11028 fit 5638.78 change 1656 MxComputeSimAnnealing(tsallis1996) evaluations 12047 fit 3904.73 change -0.5392MxComputeSimAnnealing(tsallis1996) evaluations 13066 fit 4983.72 change 1072   MxComputeSimAnnealing(tsallis1996) evaluations 14085 fit 6758.21 change 2855MxComputeSimAnnealing(tsallis1996) evaluations 15103 fit 4973.57 change 1069MxComputeSimAnnealing(tsallis1996) evaluations 16122 fit 5672.07 change 1752MxComputeSimAnnealing(tsallis1996) evaluations 17139 fit 3902.89 change -427.9MxComputeSimAnnealing(tsallis1996) evaluations 18157 fit 3904.23 change -1770 MxComputeSimAnnealing(tsallis1996) evaluations 19172 fit 4812.4 change 521.8 MxComputeSimAnnealing(tsallis1996) evaluations 20186 fit 5272.9 change 1347 MxComputeSimAnnealing(tsallis1996) evaluations 21197 fit 3902.93 change 0.2221MxComputeSimAnnealing(tsallis1996) evaluations 22213 fit 3902.8 change 0.09471                                                                              
#> MxComputeSimAnnealing(tsallis1996) evaluations 163 fit 4640.51 change 0.01372MxComputeSimAnnealing(tsallis1996) evaluations 945 fit 4640.51 change 0.01372MxComputeSimAnnealing(tsallis1996) evaluations 1728 fit 3955.82 change -684.7MxComputeSimAnnealing(tsallis1996) evaluations 2509 fit 4640.51 change 0.01372MxComputeSimAnnealing(tsallis1996) evaluations 3288 fit 4631.25 change -610   MxComputeSimAnnealing(tsallis1996) evaluations 4074 fit 3955.82 change -679.9MxComputeSimAnnealing(tsallis1996) evaluations 4852 fit 4625.35 change 155.8 MxComputeSimAnnealing(tsallis1996) evaluations 5632 fit 5164.93 change -2.655MxComputeSimAnnealing(tsallis1996) evaluations 6412 fit 5174.56 change -294  MxComputeSimAnnealing(tsallis1996) evaluations 7190 fit 5428.08 change -2566MxComputeSimAnnealing(tsallis1996) evaluations 7967 fit 6070.46 change 2076 MxComputeSimAnnealing(tsallis1996) evaluations 8743 fit 3861.1 change -144 MxComputeSimAnnealing(tsallis1996) evaluations 9518 fit 4660.94 change -18.2MxComputeSimAnnealing(tsallis1996) evaluations 10295 fit 3838.2 change -617.5MxComputeSimAnnealing(tsallis1996) evaluations 11064 fit 3866.91 change -779.9MxComputeSimAnnealing(tsallis1996) evaluations 11838 fit 3865.27 change 26.07 MxComputeSimAnnealing(tsallis1996) evaluations 12615 fit 7554.43 change 1915 MxComputeSimAnnealing(tsallis1996) evaluations 13387 fit 4628.11 change 1.167MxComputeSimAnnealing(tsallis1996) evaluations 14159 fit 3917.77 change -4232MxComputeSimAnnealing(tsallis1996) evaluations 14930 fit 3848.43 change -425.5MxComputeSimAnnealing(tsallis1996) evaluations 15704 fit 3837.41 change 4.54  MxComputeSimAnnealing(tsallis1996) evaluations 16474 fit 4280.7 change -210.1MxComputeSimAnnealing(tsallis1996) evaluations 17243 fit 3901.36 change 68.75MxComputeSimAnnealing(tsallis1996) evaluations 18005 fit 3839.65 change -794.3MxComputeSimAnnealing(tsallis1996) evaluations 18775 fit 3916.42 change 83.77 MxComputeSimAnnealing(tsallis1996) evaluations 19547 fit 4623.97 change 791.4MxComputeSimAnnealing(tsallis1996) evaluations 20316 fit 5965.54 change 1956 MxComputeSimAnnealing(tsallis1996) evaluations 21086 fit 3832.52 change -0.009051MxComputeSimAnnealing(tsallis1996) evaluations 21857 fit 3832.73 change 0.1726   MxComputeSimAnnealing(tsallis1996) evaluations 22623 fit 4789.11 change 956.6 MxComputeSimAnnealing(tsallis1996) evaluations 23389 fit 3833.89 change -788.1MxComputeSimAnnealing(tsallis1996) evaluations 24152 fit 3832.6 change 0.07749MxComputeSimAnnealing(tsallis1996) evaluations 24922 fit 3832.67 change -20.85MxComputeSimAnnealing(tsallis1996) evaluations 25689 fit 3836.77 change -788.3MxComputeSimAnnealing(tsallis1996) evaluations 26458 fit 3832.54 change -5256 MxComputeSimAnnealing(tsallis1996) evaluations 27226 fit 3832.57 change -149.9MxComputeSimAnnealing(tsallis1996) evaluations 27995 fit 4447.39 change 614.2 MxComputeSimAnnealing(tsallis1996) evaluations 28766 fit 3835.73 change 3.2                                                                             
#> MxComputeSimAnnealing(tsallis1996) evaluations 312 fit 3901.42 change -670.1MxComputeSimAnnealing(tsallis1996) evaluations 937 fit 18883 change 8911    MxComputeSimAnnealing(tsallis1996) evaluations 1562 fit 5176.33 change -1.559MxComputeSimAnnealing(tsallis1996) evaluations 2186 fit 4571.53 change 0.8667MxComputeSimAnnealing(tsallis1996) evaluations 2815 fit 5708.52 change -1896 MxComputeSimAnnealing(tsallis1996) evaluations 3440 fit 4288.38 change -915.6MxComputeSimAnnealing(tsallis1996) evaluations 4066 fit 3902 change -0.04672 MxComputeSimAnnealing(tsallis1996) evaluations 4691 fit 6589.23 change 4.859MxComputeSimAnnealing(tsallis1996) evaluations 5313 fit 5027.94 change -22.69MxComputeSimAnnealing(tsallis1996) evaluations 5936 fit 4554.85 change 284.2 MxComputeSimAnnealing(tsallis1996) evaluations 6558 fit 3895.37 change -0.5261MxComputeSimAnnealing(tsallis1996) evaluations 7184 fit 4728.86 change -707.9 MxComputeSimAnnealing(tsallis1996) evaluations 7806 fit 3847 change -1137    MxComputeSimAnnealing(tsallis1996) evaluations 8428 fit 4561.7 change 305.6MxComputeSimAnnealing(tsallis1996) evaluations 9049 fit 3886.4 change 0.1753MxComputeSimAnnealing(tsallis1996) evaluations 9669 fit 6618.44 change 2281 MxComputeSimAnnealing(tsallis1996) evaluations 10289 fit 7875.43 change 1976MxComputeSimAnnealing(tsallis1996) evaluations 10906 fit 7184.49 change 3354MxComputeSimAnnealing(tsallis1996) evaluations 11527 fit 4739.58 change 11.85MxComputeSimAnnealing(tsallis1996) evaluations 12150 fit 4294 change -0.416  MxComputeSimAnnealing(tsallis1996) evaluations 12767 fit 4281.28 change -18.87MxComputeSimAnnealing(tsallis1996) evaluations 13383 fit 3981.04 change -297.5MxComputeSimAnnealing(tsallis1996) evaluations 14003 fit 4575.82 change 0.01356MxComputeSimAnnealing(tsallis1996) evaluations 14622 fit 3871.69 change 40.88  MxComputeSimAnnealing(tsallis1996) evaluations 15242 fit 3827.61 change -2150MxComputeSimAnnealing(tsallis1996) evaluations 15856 fit 3827.07 change -0.02225MxComputeSimAnnealing(tsallis1996) evaluations 16472 fit 3827.19 change -29.22  MxComputeSimAnnealing(tsallis1996) evaluations 17086 fit 3826.51 change -2.036MxComputeSimAnnealing(tsallis1996) evaluations 17702 fit 3848.12 change -7.794MxComputeSimAnnealing(tsallis1996) evaluations 18318 fit 3850.71 change 24.63 MxComputeSimAnnealing(tsallis1996) evaluations 18936 fit 4540.83 change 714.8MxComputeSimAnnealing(tsallis1996) evaluations 19552 fit 7484.22 change 3641 MxComputeSimAnnealing(tsallis1996) evaluations 20169 fit 4592.88 change 720.3MxComputeSimAnnealing(tsallis1996) evaluations 20785 fit 4361 change 534.8   MxComputeSimAnnealing(tsallis1996) evaluations 21396 fit 3843.52 change -250.8MxComputeSimAnnealing(tsallis1996) evaluations 22014 fit 3834.62 change -8.414MxComputeSimAnnealing(tsallis1996) evaluations 22626 fit 3825.08 change -749  MxComputeSimAnnealing(tsallis1996) evaluations 23242 fit 4574.1 change 749  MxComputeSimAnnealing(tsallis1996) evaluations 23854 fit 3824.87 change -660.4MxComputeSimAnnealing(tsallis1996) evaluations 24467 fit 3828.51 change -804  MxComputeSimAnnealing(tsallis1996) evaluations 25083 fit 3824.78 change -0.007549MxComputeSimAnnealing(tsallis1996) evaluations 25700 fit 3824.86 change -4.951   MxComputeSimAnnealing(tsallis1996) evaluations 26312 fit 3824.88 change -849  MxComputeSimAnnealing(tsallis1996) evaluations 26927 fit 3902.45 change 77.6MxComputeSimAnnealing(tsallis1996) evaluations 27544 fit 3824.76 change -568.3MxComputeSimAnnealing(tsallis1996) evaluations 28155 fit 3974.36 change -815.7MxComputeSimAnnealing(tsallis1996) evaluations 28768 fit 3824.77 change -2890 MxComputeSimAnnealing(tsallis1996) evaluations 29382 fit 4265.11 change 1.464MxComputeSimAnnealing(tsallis1996) evaluations 29998 fit 3824.75 change -36.78MxComputeSimAnnealing(tsallis1996) evaluations 30614 fit 3824.75 change -23.65MxComputeSimAnnealing(tsallis1996) evaluations 31230 fit 3824.78 change 0.01277MxComputeSimAnnealing(tsallis1996) evaluations 31843 fit 3826.2 change 1.444   MxComputeSimAnnealing(tsallis1996) evaluations 32462 fit 3824.84 change -974.8MxComputeSimAnnealing(tsallis1996) evaluations 33079 fit 3824.76 change -1076 MxComputeSimAnnealing(tsallis1996) evaluations 33696 fit 3824.76 change -15.15MxComputeSimAnnealing(tsallis1996) evaluations 34312 fit 3846.41 change 21.6  MxComputeSimAnnealing(tsallis1996) evaluations 34926 fit 3824.75 change -0.03006MxComputeSimAnnealing(tsallis1996) evaluations 35541 fit 3828.42 change 3.631                                                                                
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
#> MxComputeSimAnnealing(tsallis1996) evaluations 263 fit 5176.97 change -543.1MxComputeSimAnnealing(tsallis1996) evaluations 1040 fit 5176.19 change 24.02MxComputeSimAnnealing(tsallis1996) evaluations 1814 fit 4449.99 change -86.99MxComputeSimAnnealing(tsallis1996) evaluations 2590 fit 3926 change 0        MxComputeSimAnnealing(tsallis1996) evaluations 3367 fit 6802.22 change 2878MxComputeSimAnnealing(tsallis1996) evaluations 4137 fit 4519.67 change 595.4MxComputeSimAnnealing(tsallis1996) evaluations 4911 fit 5555.51 change -123.5MxComputeSimAnnealing(tsallis1996) evaluations 5686 fit 4977.06 change -686.1MxComputeSimAnnealing(tsallis1996) evaluations 6461 fit 5171.33 change 652.8 MxComputeSimAnnealing(tsallis1996) evaluations 7237 fit 4530.24 change 49.98MxComputeSimAnnealing(tsallis1996) evaluations 8009 fit 4525.91 change 0    MxComputeSimAnnealing(tsallis1996) evaluations 8783 fit 4171.82 change -19.93MxComputeSimAnnealing(tsallis1996) evaluations 9557 fit 3924.59 change -22.84MxComputeSimAnnealing(tsallis1996) evaluations 10331 fit 3939.63 change -1.831MxComputeSimAnnealing(tsallis1996) evaluations 11097 fit 3944.32 change 0     MxComputeSimAnnealing(tsallis1996) evaluations 11869 fit 3943.53 change 21.61MxComputeSimAnnealing(tsallis1996) evaluations 12639 fit 3942.18 change 88.37MxComputeSimAnnealing(tsallis1996) evaluations 13410 fit 3947.43 change -2.105MxComputeSimAnnealing(tsallis1996) evaluations 14185 fit 5624.73 change 1002  MxComputeSimAnnealing(tsallis1996) evaluations 14958 fit 4534.37 change -1025MxComputeSimAnnealing(tsallis1996) evaluations 15727 fit 5495.9 change 1472  MxComputeSimAnnealing(tsallis1996) evaluations 16497 fit 3867.73 change 16.39MxComputeSimAnnealing(tsallis1996) evaluations 17263 fit 3846.98 change -391.3MxComputeSimAnnealing(tsallis1996) evaluations 18031 fit 4043.86 change 1.234 MxComputeSimAnnealing(tsallis1996) evaluations 18801 fit 4075.29 change 1.847MxComputeSimAnnealing(tsallis1996) evaluations 19570 fit 4072.35 change 235.1MxComputeSimAnnealing(tsallis1996) evaluations 20338 fit 4110.74 change 210.3MxComputeSimAnnealing(tsallis1996) evaluations 21107 fit 4155.89 change 240.4MxComputeSimAnnealing(tsallis1996) evaluations 21877 fit 3833.47 change -354.1MxComputeSimAnnealing(tsallis1996) evaluations 22646 fit 3831.62 change -10.74MxComputeSimAnnealing(tsallis1996) evaluations 23415 fit 4253.74 change 217.3 MxComputeSimAnnealing(tsallis1996) evaluations 24179 fit 4640.48 change 811.6MxComputeSimAnnealing(tsallis1996) evaluations 24950 fit 3826.38 change -4.01MxComputeSimAnnealing(tsallis1996) evaluations 25718 fit 4650.85 change 820.4MxComputeSimAnnealing(tsallis1996) evaluations 26488 fit 3824.16 change -823.9MxComputeSimAnnealing(tsallis1996) evaluations 27258 fit 3824.07 change -31.57MxComputeSimAnnealing(tsallis1996) evaluations 28025 fit 3823.26 change 1.503 MxComputeSimAnnealing(tsallis1996) evaluations 28793 fit 4415.56 change 592.5MxComputeSimAnnealing(tsallis1996) evaluations 29558 fit 3822.58 change 1.552MxComputeSimAnnealing(tsallis1996) evaluations 30322 fit 3823.65 change -56.6MxComputeSimAnnealing(tsallis1996) evaluations 31090 fit 3819.47 change 0.342MxComputeSimAnnealing(tsallis1996) evaluations 31859 fit 3819.07 change 0.2906MxComputeSimAnnealing(tsallis1996) evaluations 32628 fit 3818.77 change 0.07422MxComputeSimAnnealing(tsallis1996) evaluations 33395 fit 3818.66 change -1.281 MxComputeSimAnnealing(tsallis1996) evaluations 34163 fit 3818.58 change -351.4MxComputeSimAnnealing(tsallis1996) evaluations 34931 fit 4701.12 change 881.3 MxComputeSimAnnealing(tsallis1996) evaluations 35698 fit 3818.52 change -940.6MxComputeSimAnnealing(tsallis1996) evaluations 36466 fit 3818.51 change 0.006202MxComputeSimAnnealing(tsallis1996) evaluations 37229 fit 3818.75 change -0.3181 MxComputeSimAnnealing(tsallis1996) evaluations 37996 fit 3818.48 change -14.77 MxComputeSimAnnealing(tsallis1996) evaluations 38763 fit 3956.12 change 137.1 MxComputeSimAnnealing(tsallis1996) evaluations 39531 fit 4709.72 change 891.1MxComputeSimAnnealing(tsallis1996) evaluations 40297 fit 3818.48 change -904.3MxComputeSimAnnealing(tsallis1996) evaluations 41064 fit 4465.49 change 647   MxComputeSimAnnealing(tsallis1996) evaluations 41830 fit 3833.71 change 15.09MxComputeSimAnnealing(tsallis1996) evaluations 42597 fit 4729.34 change 910.5MxComputeSimAnnealing(tsallis1996) evaluations 43362 fit 3823.03 change 4.416MxComputeSimAnnealing(tsallis1996) evaluations 44124 fit 4599.96 change 781.3                                                                             
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
#> MxComputeNumericDeriv 60/276                            
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
#>                                label    est_sig     se pval            confint  group
#> 1  Regressions.freqvisit.ON.distance       0.00   0.00 0.80      [-0.00, 0.00] class1
#> 2                    Means.freqvisit    3.99***   0.18 0.00       [3.64, 4.35] class1
#> 3                     Means.distance  155.25***   3.80 0.00   [147.80, 162.70] class1
#> 4                Variances.freqvisit    0.53***   0.06 0.00       [0.42, 0.64] class1
#> 5                 Variances.distance 2464.07*** 266.70 0.00 [1941.34, 2986.79] class1
#> 6  Regressions.freqvisit.ON.distance       0.00   0.00 0.77      [-0.00, 0.01] class2
#> 7                    Means.freqvisit    3.66***   0.43 0.00       [2.81, 4.51] class2
#> 8                     Means.distance  159.52***   2.79 0.00   [154.05, 164.98] class2
#> 9                Variances.freqvisit    1.19***   0.14 0.00       [0.92, 1.46] class2
#> 10                Variances.distance 1144.27*** 133.42 0.00  [882.77, 1405.76] class2
#> 11 Regressions.freqvisit.ON.distance      -0.00   0.00 0.35      [-0.00, 0.00] class3
#> 12                   Means.freqvisit    3.95***   0.27 0.00       [3.43, 4.47] class3
#> 13                    Means.distance  147.24***   6.09 0.00   [135.30, 159.18] class3
#> 14               Variances.freqvisit    1.29***   0.17 0.00       [0.95, 1.63] class3
#> 15                Variances.distance 4200.66*** 558.63 0.00 [3105.75, 5295.56] class3
#> 16 Regressions.freqvisit.ON.distance      -0.00   0.00 0.91      [-0.00, 0.00] class4
#> 17                   Means.freqvisit    3.32***   0.38 0.00       [2.57, 4.08] class4
#> 18                    Means.distance  167.02***   7.06 0.00   [153.18, 180.86] class4
#> 19               Variances.freqvisit    1.48***   0.23 0.00       [1.02, 1.93] class4
#> 20                Variances.distance 3989.68*** 630.52 0.00 [2753.87, 5225.48] class4
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
