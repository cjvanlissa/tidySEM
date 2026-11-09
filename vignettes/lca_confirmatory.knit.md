---
title: "Confirmatory Latent Class Analysis"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Confirmatory Latent Class Analysis}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---




When theory makes specific predictions about the number of latent classes, or even their parameter values - a confirmatory use of LCA is in order.


This vignette explains confirmatory Latent Class Analysis (LCA) using `tidySEM`,
as discussed in [Van Lissa, C. J., Garnier-Villarreal, M., & Anadria, D. (2023). *Recommended Practices in Latent Class Analysis using the Open-Source R-Package tidySEM.* Structural Equation Modeling.](https://doi.org/10.1080/10705511.2023.2250920).


# Example 1: Depression

For example, suppose that clinical psychological theory proposes the existence of two subpopulations:

- people **without depression**
- people **with depression**

If all we know is people's depression score, then we can try to identify two latent classes, corresponding to these subpopulations. Suppose that the theory further specifies that a depression score of about 10 or higher is required to count as depressed. We will use this theory to formulate two increasingly specific hypotheses:

1. a **two-class model** should fit better than a one-class model. Adding a third class should not significantly improve fit.
2. the class means should be close to **0** for the non-depressed class and **10** for the depressed class.

The second hypothesis is considerably stronger because it constrains the class-specific means to exact values.

## Simulate example data

We first create a simple dataset containing a single continuous depression score.


``` r
library(tidySEM)
library(OpenMx)

set.seed(123)
x <- data.frame(depression = c(rnorm(50, mean = 6, sd = 4), rbeta(100,
    2, 3)))
hist(x$depression)
```

![](lca_confirmatory_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

## Test the number of classes

The first confirmatory hypothesis concerns **class enumeration**. Theory predicts two classes, so we estimate one-, two-, and three-class solutions.
Note that some analyses take along time to run.
For computationally intensive analyses like this, it is convenient to save the results to a file using `saveRDS()`,
so you can load the results from that file directly without having to wait for the code to run.


``` r
res <- mx_profiles(x, classes = 1:3)
#> MxComputeSimAnnealing(tsallis1996) evaluations 1633 fit 918.074 change 224.9                                                                            
#> MxComputeSimAnnealing(tsallis1996) evaluations 2961 fit 691.426 change -314.7MxComputeSimAnnealing(tsallis1996) evaluations 6203 fit 630.045 change 0.09078                                                                              
saveRDS(res, "res_depression.rds")
```


``` r
res <- readRDS("res_depression.rds")
table_fit(res)
#>          Name Classes   LL   n Parameters AIC BIC saBIC Entropy prob_min prob_max n_min n_max
#> 1 equal var 1       1 -398 150          2 801 807   800    1.00     1.00     1.00  1.00  1.00
#> 2 equal var 2       2 -347 150          4 701 713   701    0.95     0.94     1.00  0.17  0.83
#> 3 equal var 3       3 -315 150          6 642 660   641    0.97     0.96     0.99  0.12  0.72
#>   np_ratio np_local
#> 1       75       75
#> 2       38       17
#> 3       25       14
```

The fit table lets us inspect information criteria and other model-fit statistics across the competing class solutions.

If we want to test the theory, we can compare adjacent class solutions using the bootstrapped likelihood ratio test (BLRT).


``` r
res_blrt <- BLRT(res)
saveRDS(res_blrt, "blrt_depression.rds")
```


``` r
res_blrt <- readRDS("blrt_depression.rds")
res_blrt
#> Bootstrapped Likelihood Ratio Test:
#> 
#>  null  alt    lr df blrt_p samples
#>  mix1 mix2 103.5  2      0      65
#>  mix2 mix3  63.2  2      0      94
```



## Translate theoretical assumptions into parameter constraints

The models above still allow all parameters to be estimated freely from the data. That tests only the broad hypothesis that two groups exist.
The groups themselves could diverge substantially from what was theorized.

Our theory posits that the non-depressed class should have a mean near 0, whereas the depressed class should have a mean near 10 (technically, the minimum value in that class should be 10, but that's a bit more difficult to enforce).

We can represent this stronger hypothesis by fixing the two class means to these values,
or by e.g. setting a parameter constraint for the two classes.

First, create the two-class profile model without estimating it and examine its parameters:


``` r
res_theory <- mx_profiles(x, classes = 2, run = FALSE)
omxGetParameters(res_theory)
#> equal var 2.weights[1,2]                       v1                      m11 
#>                     0.26                     2.30                     0.73 
#>                      m21 
#>                     8.36
```

The relevant mean parameters are `m11` and `m21`. We can either fix these means to the exact values of 0 and 10:


``` r
res_theory <- omxSetParameters(res_theory, labels = c("m11",
    "m21"), values = c(0, 10), free = FALSE)
```

Or alternatively, we could set a lower and upper bound for these parameters, thus allowing bounded free estimation:


``` r
res_theory_bounds <- omxSetParameters(res_theory, labels = c("m11",
    "m21"), values = c(0, 10), free = TRUE, lbound = c(NA, 10),
    ubound = c(1, NA))
```

Now, run your chosen model:


``` r
res_theory <- run_mx(res_theory)
#> MxComputeSimAnnealing(tsallis1996) evaluations 2021 fit 739.009 change 1.685                                                                            
res_theory_bounds <- run_mx(res_theory_bounds)
#> MxComputeSimAnnealing(tsallis1996) evaluations 3800 fit 701.309 change 0.007141                                                                               
```

Next, compare the unconstrained two-class solution with the theory-constrained model.


``` r
compare_these <- list(baseline = res[[2]], theory = res_theory,
    theory_bounds = res_theory_bounds)

table_fit(compare_these)
#>                      Name Classes   LL   n Parameters AIC BIC saBIC Entropy prob_min prob_max
#> baseline             mix2       2 -347 150          4 701 713   701    0.95     0.94     1.00
#> theory        equal var 2       2 -369 150          2 741 747   741    0.95     0.96     0.99
#> theory_bounds equal var 2       2 -351 150          4 709 721   709    0.96     0.97     0.99
#>               n_min n_max
#> baseline       0.17  0.83
#> theory         0.17  0.83
#> theory_bounds  0.16  0.84
```

We can also compare the models using the BLRT:


``` r
blrt_theory <- BLRT(list(res[[2]], res_theory))
saveRDS(blrt_theory, "blrt_theory.rds")
```


``` r
blrt_theory <- readRDS("blrt_theory.rds")
blrt_theory
#> Bootstrapped Likelihood Ratio Test:
#> 
#>  null         alt    lr df blrt_p samples
#>  mix2 equal var 2 -44.1 -2   0.47     100
```


Note that fixing the class means to exact values is a very strong assumption.
In many real applications, theory may imply that class means should be **near** certain values rather than exactly equal to them.
You can also manually specify **start values**, and then test if the estimated **parameter values** deviate significantly from these:


``` r
res_theory_starts <- omxSetParameters(res_theory, labels = c("m11",
    "m21"), values = c(0, 10), free = TRUE)
res_theory_starts <- run_mx(res_theory_starts)
#> MxComputeSimAnnealing(tsallis1996) evaluations 72 fit 1272.7 change -2845MxComputeSimAnnealing(tsallis1996) evaluations 3002 fit 712.731 change -145.3                                                                             
omxGetParameters(res_theory_starts)
#> equal var 2.weights[1,2]                       v1                      m11 
#>                     0.22                     2.49                     0.87 
#>                      m21 
#>                     8.84
wald_test(res_theory_starts, "m11 = 0; m21 = 10")
#> Wald tests for linear hypotheses:
#>  Hypothesis df chisq       p
#>       m11=0  1  31.2 2.3e-08
#>      m21=10  1   8.8 3.0e-03
```



# Example 2: Caregiver Compass

For this next example, we will use simulated data are based on work by Zegwaard and colleagues,
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
#> MxComputeSimAnnealing(tsallis1996) evaluations 637 fit 7568.36 change 43.17MxComputeSimAnnealing(tsallis1996) evaluations 1456 fit 7568.36 change 43.17MxComputeSimAnnealing(tsallis1996) evaluations 2332 fit 5326.1 change -1636 MxComputeSimAnnealing(tsallis1996) evaluations 3202 fit 6771.02 change 5.433MxComputeSimAnnealing(tsallis1996) evaluations 4063 fit 4830.12 change -77.43MxComputeSimAnnealing(tsallis1996) evaluations 4938 fit 4286.6 change -3198  MxComputeSimAnnealing(tsallis1996) evaluations 5794 fit 8097.45 change 3834MxComputeSimAnnealing(tsallis1996) evaluations 6654 fit 7447.71 change 3384MxComputeSimAnnealing(tsallis1996) evaluations 7527 fit 7539.12 change 39.77MxComputeSimAnnealing(tsallis1996) evaluations 8370 fit 4062.92 change -0.8755MxComputeSimAnnealing(tsallis1996) evaluations 9185 fit 4067.55 change -677.6 MxComputeSimAnnealing(tsallis1996) evaluations 10011 fit 4062.77 change -1418MxComputeSimAnnealing(tsallis1996) evaluations 10867 fit 4062.74 change -0.0001829MxComputeSimAnnealing(tsallis1996) evaluations 11734 fit 4064.99 change -916.4    MxComputeSimAnnealing(tsallis1996) evaluations 12571 fit 4167.39 change 104.6 MxComputeSimAnnealing(tsallis1996) evaluations 13429 fit 4062.94 change 0.03692MxComputeSimAnnealing(tsallis1996) evaluations 14270 fit 4062.73 change -0.0009501MxComputeSimAnnealing(tsallis1996) evaluations 15102 fit 4073.01 change 10.27     MxComputeSimAnnealing(tsallis1996) evaluations 15945 fit 4064.33 change 1.592                                                                             
#> MxComputeSimAnnealing(tsallis1996) evaluations 206 fit 14401.7 change 4632MxComputeSimAnnealing(tsallis1996) evaluations 782 fit 9129.72 change -1731MxComputeSimAnnealing(tsallis1996) evaluations 1362 fit 5383.43 change -4572MxComputeSimAnnealing(tsallis1996) evaluations 1930 fit 4882.53 change 2.272MxComputeSimAnnealing(tsallis1996) evaluations 2498 fit 5369.64 change -9.839MxComputeSimAnnealing(tsallis1996) evaluations 3044 fit 4870.45 change -554.1MxComputeSimAnnealing(tsallis1996) evaluations 3604 fit 4882.53 change 2.272 MxComputeSimAnnealing(tsallis1996) evaluations 4206 fit 4283.85 change -463 MxComputeSimAnnealing(tsallis1996) evaluations 4777 fit 4574.5 change -2066MxComputeSimAnnealing(tsallis1996) evaluations 5377 fit 5286.23 change 1370MxComputeSimAnnealing(tsallis1996) evaluations 5940 fit 5216.77 change -5.021MxComputeSimAnnealing(tsallis1996) evaluations 6507 fit 4188 change -117.9   MxComputeSimAnnealing(tsallis1996) evaluations 7110 fit 3911.29 change -1192MxComputeSimAnnealing(tsallis1996) evaluations 7665 fit 5547.85 change 1611 MxComputeSimAnnealing(tsallis1996) evaluations 8231 fit 4803.83 change 870.1MxComputeSimAnnealing(tsallis1996) evaluations 8776 fit 14461.2 change 1.053e+04MxComputeSimAnnealing(tsallis1996) evaluations 9325 fit 5037.33 change 1119     MxComputeSimAnnealing(tsallis1996) evaluations 9902 fit 4802.91 change 766.8MxComputeSimAnnealing(tsallis1996) evaluations 10457 fit 3903.01 change -0.3698MxComputeSimAnnealing(tsallis1996) evaluations 11018 fit 4423.99 change 518.7  MxComputeSimAnnealing(tsallis1996) evaluations 11572 fit 4944.12 change 1040 MxComputeSimAnnealing(tsallis1996) evaluations 12123 fit 3903.99 change -2.447MxComputeSimAnnealing(tsallis1996) evaluations 12998 fit 4810.7 change -162.6 MxComputeSimAnnealing(tsallis1996) evaluations 13999 fit 3944.17 change -1729MxComputeSimAnnealing(tsallis1996) evaluations 15002 fit 3902.83 change 0.111MxComputeSimAnnealing(tsallis1996) evaluations 15556 fit 3902.71 change -0.03463MxComputeSimAnnealing(tsallis1996) evaluations 16089 fit 4092.91 change -1553   MxComputeSimAnnealing(tsallis1996) evaluations 16639 fit 4186.18 change -765.7MxComputeSimAnnealing(tsallis1996) evaluations 17205 fit 3911.33 change 8.533 MxComputeSimAnnealing(tsallis1996) evaluations 17771 fit 4035.17 change -16.86MxComputeSimAnnealing(tsallis1996) evaluations 18343 fit 3902.71 change 0.004202MxComputeSimAnnealing(tsallis1996) evaluations 18905 fit 3902.71 change -922.5  MxComputeSimAnnealing(tsallis1996) evaluations 19472 fit 3902.71 change -37.75MxComputeSimAnnealing(tsallis1996) evaluations 20031 fit 3902.7 change -1759  MxComputeSimAnnealing(tsallis1996) evaluations 20598 fit 3902.96 change 0.2519MxComputeSimAnnealing(tsallis1996) evaluations 21179 fit 3903.05 change -249.8MxComputeSimAnnealing(tsallis1996) evaluations 21745 fit 4961.8 change 18.98  MxComputeSimAnnealing(tsallis1996) evaluations 22313 fit 3902.7 change -0.002197MxComputeSimAnnealing(tsallis1996) evaluations 22880 fit 4814.38 change -151.9                                                                                
#> MxComputeSimAnnealing(tsallis1996) evaluations 108 fit 4642.91 change -9.201MxComputeSimAnnealing(tsallis1996) evaluations 579 fit 4752.46 change 10.59 MxComputeSimAnnealing(tsallis1996) evaluations 1016 fit 4752.46 change 10.59MxComputeSimAnnealing(tsallis1996) evaluations 1455 fit 4753.97 change 0.01369MxComputeSimAnnealing(tsallis1996) evaluations 1901 fit 4652.11 change -2.347 MxComputeSimAnnealing(tsallis1996) evaluations 2347 fit 4878.86 change -0.1781MxComputeSimAnnealing(tsallis1996) evaluations 2787 fit 4752.46 change 10.59  MxComputeSimAnnealing(tsallis1996) evaluations 3220 fit 4879.04 change -21.3MxComputeSimAnnealing(tsallis1996) evaluations 3661 fit 4752.46 change 10.59MxComputeSimAnnealing(tsallis1996) evaluations 4099 fit 4753.96 change 90.83MxComputeSimAnnealing(tsallis1996) evaluations 4525 fit 4649.23 change 6.072MxComputeSimAnnealing(tsallis1996) evaluations 4959 fit 4002.16 change -170.8MxComputeSimAnnealing(tsallis1996) evaluations 5394 fit 3906.49 change -3235 MxComputeSimAnnealing(tsallis1996) evaluations 5833 fit 4616.45 change -9131MxComputeSimAnnealing(tsallis1996) evaluations 6270 fit 4598.32 change -1508MxComputeSimAnnealing(tsallis1996) evaluations 6704 fit 3972.65 change -1314MxComputeSimAnnealing(tsallis1996) evaluations 7141 fit 6116.91 change 2195 MxComputeSimAnnealing(tsallis1996) evaluations 7579 fit 4555.67 change 32.32MxComputeSimAnnealing(tsallis1996) evaluations 8011 fit 3839.13 change -16.95MxComputeSimAnnealing(tsallis1996) evaluations 8447 fit 4557.99 change 465.4 MxComputeSimAnnealing(tsallis1996) evaluations 8869 fit 3853.81 change -4934MxComputeSimAnnealing(tsallis1996) evaluations 9306 fit 3865.22 change -570 MxComputeSimAnnealing(tsallis1996) evaluations 9739 fit 3848.27 change -3088MxComputeSimAnnealing(tsallis1996) evaluations 10173 fit 3840.25 change -730MxComputeSimAnnealing(tsallis1996) evaluations 10605 fit 4688.81 change 18.64MxComputeSimAnnealing(tsallis1996) evaluations 11042 fit 3859.68 change -796.7MxComputeSimAnnealing(tsallis1996) evaluations 11474 fit 4742.48 change 577.9 MxComputeSimAnnealing(tsallis1996) evaluations 11908 fit 4410.1 change -68.78MxComputeSimAnnealing(tsallis1996) evaluations 12344 fit 4477.93 change 644.9MxComputeSimAnnealing(tsallis1996) evaluations 12771 fit 4012.92 change 179.9MxComputeSimAnnealing(tsallis1996) evaluations 13194 fit 3833.01 change -276.9MxComputeSimAnnealing(tsallis1996) evaluations 13626 fit 3838.97 change -2824 MxComputeSimAnnealing(tsallis1996) evaluations 14054 fit 3832.66 change -91.73MxComputeSimAnnealing(tsallis1996) evaluations 14485 fit 3832.72 change -660.3MxComputeSimAnnealing(tsallis1996) evaluations 14913 fit 13065.3 change 2595  MxComputeSimAnnealing(tsallis1996) evaluations 15338 fit 3832.79 change -942.6MxComputeSimAnnealing(tsallis1996) evaluations 15763 fit 3832.55 change -0.003322MxComputeSimAnnealing(tsallis1996) evaluations 16191 fit 3832.61 change -10.11   MxComputeSimAnnealing(tsallis1996) evaluations 16622 fit 3833.21 change -660.3MxComputeSimAnnealing(tsallis1996) evaluations 17058 fit 3832.62 change -1.318MxComputeSimAnnealing(tsallis1996) evaluations 17483 fit 4422 change -222.3   MxComputeSimAnnealing(tsallis1996) evaluations 17914 fit 4747.38 change 914.7MxComputeSimAnnealing(tsallis1996) evaluations 18343 fit 9099.72 change 3232 MxComputeSimAnnealing(tsallis1996) evaluations 18770 fit 4645.71 change 795.9MxComputeSimAnnealing(tsallis1996) evaluations 19198 fit 4489.19 change 353.7MxComputeSimAnnealing(tsallis1996) evaluations 19624 fit 4061.87 change -143.9MxComputeSimAnnealing(tsallis1996) evaluations 20051 fit 3849.08 change -649.9MxComputeSimAnnealing(tsallis1996) evaluations 20475 fit 3832.67 change -2.582MxComputeSimAnnealing(tsallis1996) evaluations 20902 fit 3832.56 change -660.9MxComputeSimAnnealing(tsallis1996) evaluations 21353 fit 3834.01 change -244.8MxComputeSimAnnealing(tsallis1996) evaluations 21775 fit 3940.02 change 12.02 MxComputeSimAnnealing(tsallis1996) evaluations 22202 fit 4505.34 change 672.7MxComputeSimAnnealing(tsallis1996) evaluations 22628 fit 4783.07 change -4.181MxComputeSimAnnealing(tsallis1996) evaluations 23053 fit 3832.57 change -0.04264MxComputeSimAnnealing(tsallis1996) evaluations 23484 fit 3832.52 change 0.0003223MxComputeSimAnnealing(tsallis1996) evaluations 23908 fit 5242.53 change 1406     MxComputeSimAnnealing(tsallis1996) evaluations 24334 fit 4293.87 change 461.3MxComputeSimAnnealing(tsallis1996) evaluations 24763 fit 3832.55 change -1.446MxComputeSimAnnealing(tsallis1996) evaluations 25187 fit 3832.56 change -6.158MxComputeSimAnnealing(tsallis1996) evaluations 25612 fit 4110.98 change 278.4 MxComputeSimAnnealing(tsallis1996) evaluations 26034 fit 3832.53 change 0.004772MxComputeSimAnnealing(tsallis1996) evaluations 26482 fit 3832.52 change -0.04098MxComputeSimAnnealing(tsallis1996) evaluations 26912 fit 3832.52 change 0.0004721MxComputeSimAnnealing(tsallis1996) evaluations 27338 fit 4507.7 change 675.1     MxComputeSimAnnealing(tsallis1996) evaluations 27762 fit 3832.53 change -0.9278MxComputeSimAnnealing(tsallis1996) evaluations 28183 fit 3832.52 change -0.1063MxComputeSimAnnealing(tsallis1996) evaluations 28606 fit 3832.54 change 0.02501MxComputeSimAnnealing(tsallis1996) evaluations 29008 fit 3832.51 change -746.2 MxComputeSimAnnealing(tsallis1996) evaluations 29437 fit 3832.52 change -0.004675                                                                                 
#> MxComputeSimAnnealing(tsallis1996) evaluations 221 fit 4740.73 change 9.537MxComputeSimAnnealing(tsallis1996) evaluations 588 fit 4263.54 change -478.3MxComputeSimAnnealing(tsallis1996) evaluations 938 fit 10571.7 change 1174  MxComputeSimAnnealing(tsallis1996) evaluations 1284 fit 4731.19 change 123.6MxComputeSimAnnealing(tsallis1996) evaluations 1628 fit 4110.29 change -154.6MxComputeSimAnnealing(tsallis1996) evaluations 1976 fit 4689.82 change -6548 MxComputeSimAnnealing(tsallis1996) evaluations 2319 fit 4607.56 change 7.887MxComputeSimAnnealing(tsallis1996) evaluations 2669 fit 7759.34 change 2031 MxComputeSimAnnealing(tsallis1996) evaluations 3015 fit 4687.31 change 3.348MxComputeSimAnnealing(tsallis1996) evaluations 3366 fit 4100.16 change -8.727MxComputeSimAnnealing(tsallis1996) evaluations 3705 fit 6103.71 change 554.8 MxComputeSimAnnealing(tsallis1996) evaluations 4055 fit 4607.56 change 7.887MxComputeSimAnnealing(tsallis1996) evaluations 4405 fit 3967.77 change -885.3MxComputeSimAnnealing(tsallis1996) evaluations 4754 fit 4600.14 change -6.348MxComputeSimAnnealing(tsallis1996) evaluations 5103 fit 4117.42 change 16.43 MxComputeSimAnnealing(tsallis1996) evaluations 5453 fit 4607.79 change -6.509MxComputeSimAnnealing(tsallis1996) evaluations 5801 fit 4115.44 change 100.5 MxComputeSimAnnealing(tsallis1996) evaluations 6148 fit 4691.61 change -1630MxComputeSimAnnealing(tsallis1996) evaluations 6490 fit 4615.88 change -6.141MxComputeSimAnnealing(tsallis1996) evaluations 6835 fit 4235.64 change 3.037 MxComputeSimAnnealing(tsallis1996) evaluations 7182 fit 7125.27 change 2697 MxComputeSimAnnealing(tsallis1996) evaluations 7529 fit 4672.37 change 663.9MxComputeSimAnnealing(tsallis1996) evaluations 7875 fit 3858.55 change -194.8MxComputeSimAnnealing(tsallis1996) evaluations 8219 fit 6243.95 change 712.3 MxComputeSimAnnealing(tsallis1996) evaluations 8563 fit 4599.66 change 7.573MxComputeSimAnnealing(tsallis1996) evaluations 8906 fit 3943.86 change -184.5MxComputeSimAnnealing(tsallis1996) evaluations 9248 fit 3841.15 change -0.4228MxComputeSimAnnealing(tsallis1996) evaluations 9595 fit 4485.65 change 1.98   MxComputeSimAnnealing(tsallis1996) evaluations 9941 fit 4101.28 change 251.1MxComputeSimAnnealing(tsallis1996) evaluations 10288 fit 13208.2 change 5247MxComputeSimAnnealing(tsallis1996) evaluations 10640 fit 4083.53 change -494.4MxComputeSimAnnealing(tsallis1996) evaluations 11004 fit 4082.94 change 231.5 MxComputeSimAnnealing(tsallis1996) evaluations 11350 fit 4810.71 change 832.3MxComputeSimAnnealing(tsallis1996) evaluations 11691 fit 4008.3 change 25.09 MxComputeSimAnnealing(tsallis1996) evaluations 12038 fit 4589.06 change 1.078MxComputeSimAnnealing(tsallis1996) evaluations 12382 fit 4020.44 change 5.063MxComputeSimAnnealing(tsallis1996) evaluations 12728 fit 4481.4 change -165.1MxComputeSimAnnealing(tsallis1996) evaluations 13082 fit 3825.64 change -177.1MxComputeSimAnnealing(tsallis1996) evaluations 13427 fit 3980.27 change -206.2MxComputeSimAnnealing(tsallis1996) evaluations 13775 fit 3824.99 change -789.6MxComputeSimAnnealing(tsallis1996) evaluations 14118 fit 3823.29 change -187.8MxComputeSimAnnealing(tsallis1996) evaluations 14465 fit 4527.87 change -2.664MxComputeSimAnnealing(tsallis1996) evaluations 14810 fit 3846.43 change 24.77 MxComputeSimAnnealing(tsallis1996) evaluations 15136 fit 4049.81 change -128.2MxComputeSimAnnealing(tsallis1996) evaluations 15472 fit 3896.54 change -383  MxComputeSimAnnealing(tsallis1996) evaluations 15812 fit 3821.84 change -694.2MxComputeSimAnnealing(tsallis1996) evaluations 16154 fit 3834.12 change -817.5MxComputeSimAnnealing(tsallis1996) evaluations 16499 fit 3834.94 change 13.75 MxComputeSimAnnealing(tsallis1996) evaluations 16844 fit 4536.81 change 401.1MxComputeSimAnnealing(tsallis1996) evaluations 17203 fit 6202.05 change 2259 MxComputeSimAnnealing(tsallis1996) evaluations 17547 fit 4536.98 change 716 MxComputeSimAnnealing(tsallis1996) evaluations 17892 fit 4021.71 change 200.5MxComputeSimAnnealing(tsallis1996) evaluations 18237 fit 3820.82 change -5.789MxComputeSimAnnealing(tsallis1996) evaluations 18578 fit 4411.67 change 148.7 MxComputeSimAnnealing(tsallis1996) evaluations 18919 fit 3830.77 change -136.4MxComputeSimAnnealing(tsallis1996) evaluations 19280 fit 4405.11 change 478.9 MxComputeSimAnnealing(tsallis1996) evaluations 19621 fit 3821.76 change -1098MxComputeSimAnnealing(tsallis1996) evaluations 19964 fit 3820.9 change -21.42MxComputeSimAnnealing(tsallis1996) evaluations 20304 fit 3820.49 change -190 MxComputeSimAnnealing(tsallis1996) evaluations 20650 fit 3820.7 change -2595MxComputeSimAnnealing(tsallis1996) evaluations 20993 fit 4922.2 change -3.103MxComputeSimAnnealing(tsallis1996) evaluations 21336 fit 3829.26 change -836.4MxComputeSimAnnealing(tsallis1996) evaluations 21682 fit 3830.25 change 9.791 MxComputeSimAnnealing(tsallis1996) evaluations 22016 fit 3820.46 change -49.01MxComputeSimAnnealing(tsallis1996) evaluations 22356 fit 3825.54 change 5.057 MxComputeSimAnnealing(tsallis1996) evaluations 22701 fit 3863.18 change -1063MxComputeSimAnnealing(tsallis1996) evaluations 23042 fit 3823.32 change -26.23MxComputeSimAnnealing(tsallis1996) evaluations 23384 fit 3820.44 change -0.01109MxComputeSimAnnealing(tsallis1996) evaluations 23725 fit 4543.07 change 722.6   MxComputeSimAnnealing(tsallis1996) evaluations 24066 fit 4710.3 change -2985 MxComputeSimAnnealing(tsallis1996) evaluations 24404 fit 4571.84 change 740.8MxComputeSimAnnealing(tsallis1996) evaluations 24750 fit 3855.75 change -803.8MxComputeSimAnnealing(tsallis1996) evaluations 25094 fit 3821.5 change 0.751  MxComputeSimAnnealing(tsallis1996) evaluations 25431 fit 4000.51 change 156.8MxComputeSimAnnealing(tsallis1996) evaluations 25770 fit 3820.44 change -3.236MxComputeSimAnnealing(tsallis1996) evaluations 26108 fit 3850.08 change 29.54 MxComputeSimAnnealing(tsallis1996) evaluations 26447 fit 3820.93 change -1375MxComputeSimAnnealing(tsallis1996) evaluations 26781 fit 11577.8 change 7757 MxComputeSimAnnealing(tsallis1996) evaluations 27113 fit 6969.87 change 3144MxComputeSimAnnealing(tsallis1996) evaluations 27450 fit 5992.69 change 2172MxComputeSimAnnealing(tsallis1996) evaluations 27791 fit 7107.47 change 3287MxComputeSimAnnealing(tsallis1996) evaluations 28132 fit 3820.44 change -0.01325MxComputeSimAnnealing(tsallis1996) evaluations 28472 fit 3825.16 change -22.42  MxComputeSimAnnealing(tsallis1996) evaluations 28810 fit 3824.89 change -836.6MxComputeSimAnnealing(tsallis1996) evaluations 29153 fit 4084.11 change -3.198MxComputeSimAnnealing(tsallis1996) evaluations 29525 fit 5735.31 change 1915  MxComputeSimAnnealing(tsallis1996) evaluations 29869 fit 4924.1 change 936.2MxComputeSimAnnealing(tsallis1996) evaluations 30211 fit 3820.43 change -839.7MxComputeSimAnnealing(tsallis1996) evaluations 30549 fit 3820.43 change -133.6MxComputeSimAnnealing(tsallis1996) evaluations 30892 fit 3820.43 change -3.276MxComputeSimAnnealing(tsallis1996) evaluations 31224 fit 3820.43 change 0.0002438MxComputeSimAnnealing(tsallis1996) evaluations 31561 fit 3822.8 change -262.2    MxComputeSimAnnealing(tsallis1996) evaluations 31901 fit 5733.35 change 1913 MxComputeSimAnnealing(tsallis1996) evaluations 32237 fit 3820.44 change -0.0004667MxComputeSimAnnealing(tsallis1996) evaluations 32576 fit 3820.45 change -54.22    MxComputeSimAnnealing(tsallis1996) evaluations 32918 fit 4560.68 change 740.2 MxComputeSimAnnealing(tsallis1996) evaluations 33257 fit 3870.97 change -1056MxComputeSimAnnealing(tsallis1996) evaluations 33600 fit 3821.81 change 1.38 MxComputeSimAnnealing(tsallis1996) evaluations 33941 fit 4082.77 change 37.37MxComputeSimAnnealing(tsallis1996) evaluations 34279 fit 4085.02 change 264.5MxComputeSimAnnealing(tsallis1996) evaluations 34621 fit 3820.44 change -1.495MxComputeSimAnnealing(tsallis1996) evaluations 34962 fit 4564.12 change -6.042MxComputeSimAnnealing(tsallis1996) evaluations 35305 fit 3820.62 change -138.6MxComputeSimAnnealing(tsallis1996) evaluations 35655 fit 6593.11 change 2768                                                                              
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
#> MxComputeSimAnnealing(tsallis1996) evaluations 80 fit 4665.83 change -8612MxComputeSimAnnealing(tsallis1996) evaluations 513 fit 4580.05 change -7.191MxComputeSimAnnealing(tsallis1996) evaluations 942 fit 5031.12 change 1.304 MxComputeSimAnnealing(tsallis1996) evaluations 1363 fit 5056.79 change 25.67MxComputeSimAnnealing(tsallis1996) evaluations 1792 fit 7098.79 change 2285 MxComputeSimAnnealing(tsallis1996) evaluations 2224 fit 4579.64 change 4.404MxComputeSimAnnealing(tsallis1996) evaluations 2657 fit 5031.12 change 1.304MxComputeSimAnnealing(tsallis1996) evaluations 3090 fit 4672.49 change -6239MxComputeSimAnnealing(tsallis1996) evaluations 3546 fit 4501.11 change -171.4MxComputeSimAnnealing(tsallis1996) evaluations 3975 fit 4481.7 change -18.87 MxComputeSimAnnealing(tsallis1996) evaluations 4415 fit 4814.15 change 1.369MxComputeSimAnnealing(tsallis1996) evaluations 4847 fit 4666.78 change 86.15MxComputeSimAnnealing(tsallis1996) evaluations 5279 fit 4162.52 change -895 MxComputeSimAnnealing(tsallis1996) evaluations 5700 fit 5054.23 change -3.297MxComputeSimAnnealing(tsallis1996) evaluations 6130 fit 4772.88 change -39.9 MxComputeSimAnnealing(tsallis1996) evaluations 6558 fit 4636.84 change 318.7MxComputeSimAnnealing(tsallis1996) evaluations 6987 fit 4549.03 change 223.8MxComputeSimAnnealing(tsallis1996) evaluations 7415 fit 4594.05 change 600.8MxComputeSimAnnealing(tsallis1996) evaluations 7849 fit 6288.46 change -642.1MxComputeSimAnnealing(tsallis1996) evaluations 8279 fit 4476.65 change 92.46 MxComputeSimAnnealing(tsallis1996) evaluations 8714 fit 4262.62 change 403.1MxComputeSimAnnealing(tsallis1996) evaluations 9137 fit 4668.22 change 746.9MxComputeSimAnnealing(tsallis1996) evaluations 9570 fit 4714.86 change 1.355MxComputeSimAnnealing(tsallis1996) evaluations 9994 fit 4251.59 change 387.8MxComputeSimAnnealing(tsallis1996) evaluations 10425 fit 5017.26 change 1115MxComputeSimAnnealing(tsallis1996) evaluations 10854 fit 4826.76 change 14.21MxComputeSimAnnealing(tsallis1996) evaluations 11284 fit 4599.59 change -4.606MxComputeSimAnnealing(tsallis1996) evaluations 11713 fit 4423.21 change -5.173MxComputeSimAnnealing(tsallis1996) evaluations 12141 fit 4940.03 change 3.19  MxComputeSimAnnealing(tsallis1996) evaluations 12575 fit 4331.18 change 501.6MxComputeSimAnnealing(tsallis1996) evaluations 13005 fit 4393.08 change 517.2MxComputeSimAnnealing(tsallis1996) evaluations 13430 fit 4439.79 change 497.6MxComputeSimAnnealing(tsallis1996) evaluations 13858 fit 4985.6 change 24.19 MxComputeSimAnnealing(tsallis1996) evaluations 14278 fit 4024.59 change -878.6MxComputeSimAnnealing(tsallis1996) evaluations 14712 fit 4378.73 change -168.5MxComputeSimAnnealing(tsallis1996) evaluations 15142 fit 3823.57 change -12.39MxComputeSimAnnealing(tsallis1996) evaluations 15574 fit 4737.36 change -209.1MxComputeSimAnnealing(tsallis1996) evaluations 15996 fit 3824.68 change -0.947MxComputeSimAnnealing(tsallis1996) evaluations 16420 fit 4140.62 change -604.1MxComputeSimAnnealing(tsallis1996) evaluations 16845 fit 4545.55 change -560.7MxComputeSimAnnealing(tsallis1996) evaluations 17272 fit 4553.22 change 14.32 MxComputeSimAnnealing(tsallis1996) evaluations 17702 fit 4534.92 change 534.4MxComputeSimAnnealing(tsallis1996) evaluations 18130 fit 3953.56 change -310.6MxComputeSimAnnealing(tsallis1996) evaluations 18546 fit 4883.11 change -21.67MxComputeSimAnnealing(tsallis1996) evaluations 18975 fit 3954.4 change -817.3 MxComputeSimAnnealing(tsallis1996) evaluations 19404 fit 4578.67 change 671.1MxComputeSimAnnealing(tsallis1996) evaluations 19825 fit 3865.51 change 44.21MxComputeSimAnnealing(tsallis1996) evaluations 20248 fit 3903.25 change -679.5MxComputeSimAnnealing(tsallis1996) evaluations 20672 fit 3901.95 change -175.5MxComputeSimAnnealing(tsallis1996) evaluations 21096 fit 3944.06 change -593.2MxComputeSimAnnealing(tsallis1996) evaluations 21523 fit 3931.65 change -878.1MxComputeSimAnnealing(tsallis1996) evaluations 21948 fit 3820.37 change -258.7MxComputeSimAnnealing(tsallis1996) evaluations 22374 fit 3902.32 change 81.66 MxComputeSimAnnealing(tsallis1996) evaluations 22793 fit 6110.46 change 2290 MxComputeSimAnnealing(tsallis1996) evaluations 23219 fit 4638.28 change 818.2MxComputeSimAnnealing(tsallis1996) evaluations 23644 fit 4567.09 change 24.22MxComputeSimAnnealing(tsallis1996) evaluations 24067 fit 4606.43 change 785.6MxComputeSimAnnealing(tsallis1996) evaluations 24491 fit 4729.46 change 658  MxComputeSimAnnealing(tsallis1996) evaluations 24915 fit 4160.56 change -547.7MxComputeSimAnnealing(tsallis1996) evaluations 25345 fit 4787.32 change 751.1 MxComputeSimAnnealing(tsallis1996) evaluations 25777 fit 3819.65 change -884.7MxComputeSimAnnealing(tsallis1996) evaluations 26202 fit 4624.03 change 760.6 MxComputeSimAnnealing(tsallis1996) evaluations 26624 fit 4618.97 change 741  MxComputeSimAnnealing(tsallis1996) evaluations 27040 fit 3834.75 change -146.6MxComputeSimAnnealing(tsallis1996) evaluations 27467 fit 4667.55 change 545.5 MxComputeSimAnnealing(tsallis1996) evaluations 27896 fit 4779.32 change 638.9MxComputeSimAnnealing(tsallis1996) evaluations 28323 fit 3818.71 change -0.118MxComputeSimAnnealing(tsallis1996) evaluations 28745 fit 3862.53 change -1076 MxComputeSimAnnealing(tsallis1996) evaluations 29169 fit 4713.2 change 873.7 MxComputeSimAnnealing(tsallis1996) evaluations 29588 fit 3980.64 change 162 MxComputeSimAnnealing(tsallis1996) evaluations 30014 fit 3875.83 change 10.42MxComputeSimAnnealing(tsallis1996) evaluations 30442 fit 3818.61 change 0.01986MxComputeSimAnnealing(tsallis1996) evaluations 30866 fit 4441.88 change 623.3  MxComputeSimAnnealing(tsallis1996) evaluations 31276 fit 4143.52 change 325  MxComputeSimAnnealing(tsallis1996) evaluations 31701 fit 4645.35 change 785MxComputeSimAnnealing(tsallis1996) evaluations 32127 fit 3818.61 change 0.0119MxComputeSimAnnealing(tsallis1996) evaluations 32553 fit 4753.29 change 934.8 MxComputeSimAnnealing(tsallis1996) evaluations 32979 fit 3818.74 change -2162MxComputeSimAnnealing(tsallis1996) evaluations 33401 fit 4718.5 change 876.9 MxComputeSimAnnealing(tsallis1996) evaluations 33826 fit 4703.24 change -9.215MxComputeSimAnnealing(tsallis1996) evaluations 34254 fit 3818.65 change -0.2941MxComputeSimAnnealing(tsallis1996) evaluations 34676 fit 4631.81 change -55.95 MxComputeSimAnnealing(tsallis1996) evaluations 35110 fit 4756.32 change 909.9 MxComputeSimAnnealing(tsallis1996) evaluations 35533 fit 5221.63 change 1403 MxComputeSimAnnealing(tsallis1996) evaluations 35961 fit 3818.67 change -896.5MxComputeSimAnnealing(tsallis1996) evaluations 36388 fit 4686.59 change 868.1 MxComputeSimAnnealing(tsallis1996) evaluations 36812 fit 3824.12 change -2.92MxComputeSimAnnealing(tsallis1996) evaluations 37237 fit 4575.64 change 757.1MxComputeSimAnnealing(tsallis1996) evaluations 37663 fit 3818.49 change -484.7MxComputeSimAnnealing(tsallis1996) evaluations 38089 fit 3818.49 change -32.42MxComputeSimAnnealing(tsallis1996) evaluations 38516 fit 3819.22 change 0.3761MxComputeSimAnnealing(tsallis1996) evaluations 38943 fit 3818.48 change -0.00762MxComputeSimAnnealing(tsallis1996) evaluations 39365 fit 4185.07 change 366.6   MxComputeSimAnnealing(tsallis1996) evaluations 39775 fit 4274.59 change 456.1MxComputeSimAnnealing(tsallis1996) evaluations 40211 fit 3818.48 change -797.6MxComputeSimAnnealing(tsallis1996) evaluations 40637 fit 4177.43 change 359   MxComputeSimAnnealing(tsallis1996) evaluations 41057 fit 4770.1 change 951.6MxComputeSimAnnealing(tsallis1996) evaluations 41482 fit 3818.49 change -607.9MxComputeSimAnnealing(tsallis1996) evaluations 41904 fit 8264.07 change 4037  MxComputeSimAnnealing(tsallis1996) evaluations 42325 fit 3819.09 change -184MxComputeSimAnnealing(tsallis1996) evaluations 42749 fit 3818.51 change 0.01634MxComputeSimAnnealing(tsallis1996) evaluations 43169 fit 3818.6 change -12.02  MxComputeSimAnnealing(tsallis1996) evaluations 43589 fit 3821.99 change -3.794MxComputeSimAnnealing(tsallis1996) evaluations 44001 fit 3818.48 change 0.0009084MxComputeSimAnnealing(tsallis1996) evaluations 44418 fit 3818.48 change -14.74   MxComputeSimAnnealing(tsallis1996) evaluations 44839 fit 3818.72 change -354.3                                                                              
```

``` r
res_alt <- mx_profiles(df, classes = 4, variances = "varying")
compare <- list(res[[4]], res_alt)
table_fit(compare)
```


Table: Comparing competing theoretical models

|Name       |    LL| Parameters|  BIC| Entropy| prob_min| prob_max| n_min| n_max|
|:----------|-----:|----------:|----:|-------:|--------:|--------:|-----:|-----:|
|mix4       | -1916|         23| 3976|    0.75|     0.81|     0.92|  0.16|  0.34|
|free var 4 | -1909|         35| 4037|    0.78|     0.84|     0.92|  0.16|  0.32|



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
#> MxComputeNumericDeriv 80/276MxComputeNumericDeriv 214/276                             
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
![](lca_confirmatory_files/figure-html/unnamed-chunk-32-1.png)<!-- -->
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
![](lca_confirmatory_files/figure-html/unnamed-chunk-35-1.png)<!-- -->
<div class="figure">
<img src="lpa_profiles.png" alt="Profile plot" width="80%" />
<p class="caption">Profile plot</p>
</div>


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
#>   [1] 1 4 1 1 1 3 2 1 3 3 2 1 4 4 2 1 2 3 2 3 2 3 3 1 1 1 3 2 3 4 2 2 4 4 1 3 2 3 4 3 1 3 2 2 1
#>  [46] 3 1 3 4 2 2 1 2 1 1 4 4 1 2 1 1 1 4 2 4 2 4 4 1 2 1 1 2 2 1 4 3 1 1 3 4 3 1 2 3 1 4 3 3 3
#>  [91] 2 2 3 4 1 2 2 4 3 2 1 1 1 1 3 4 2 1 2 2 2 1 1 2 3 4 2 3 1 2 2 4 4 2 1 2 1 3 1 3 3 3 4 1 3
#> [136] 2 2 3 4 4 4 2 1 1 3 3 2 2 3 1 1 2 2 3 1 1 1 3 3 3 1 1 2 1 2 1 3 1 2 2 4 1 3 4 2 1 3 3 1 4
#> [181] 2 4 3 3 1 2 2 2 2 1 4 1 3 2 1 1 4 4 1 3 4 2 2 2 2 1 3 1 2 1 2 3 3 1 2 2 2 4 4 2 3 4 4 4 1
#> [226] 3 1 2 2 2 1 3 1 2 4 1 2 3 1 2 4 2 1 2 2 4 4 1 1 1 2 3 2 1 1 2 1 4 2 3 3 1 2 3 1 1 1 3 1 3
#> [271] 2 2 2 1 2 2 4 3 3 1 3 2 3 3 1 2 1 3 4 3 4 1 2 1 3 3 1 1 1 3 3 1 2 1 1 2 1 1 1 1 1 1 3 1 3
#> [316] 1 4 3 2 2 2 1 4 1 4 1 2 4 2 2 1 1 1 3 1 2 1 1 1 2 1 3 1 3 3 2 2 3 4 1 2 2 2 2 4 1 2 3 1 3
#> [361] 3 1 1 2 2 1 1 1 2 3 4 1 1 1 2 1 1 2 4 3 1 2 1 4 2 4 2 4 1 1 3 3 1 4 1 4 1 1 2 2 1 1 4 3 1
#> [406] 2 2 4 1 2 3 2 4 2 1 1 3 2 1 3 1 2 2 4 3 2 2 4 1 4 1 1 2 2 2 4 2 2 1 2 4 2 3 4 2 1 4 2 2 1
#> [451] 4 1 1 1 2 3 3 1 3 2 1 3 4 2 1 2 2 4 3 1 1 1 3 2 2 1 1 4 1 1 1 3 4 4 1 3 3 1 3 2 1 3 3 3 2
#> [496] 3 1 2 1 1 4 4 2 1 3 2 2 2 1 1 4 3 1
```
