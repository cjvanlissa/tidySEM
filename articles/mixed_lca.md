# Latent Class Analysis with Mixed (Categorical and Continuous) Data

Latent class analysis for categorical indicators (LCA) and latent
profile analysis for continuous indicators (LPA) are widely used mixture
modeling techniques for identifying unobserved subgroups in a
population. In practice, researchers often encounter **mixed data
types**: a combination of continuous, binary, and ordinal indicators.

Estimating such models was complicated in prior versions of `tidySEM`,
and often led to convergence issues. The new function
[`mx_mixed_lca()`](https://cjvanlissa.github.io/tidySEM/reference/mx_mixed_lca.md),
introduced in `tidySEM` version `0.2.10`, provides a high-level
interface to estimate mixed-data latent class models. It implements a
multi-step estimation procedure designed to improve starting values and
convergence when working with heterogeneous indicators.

This vignette demonstrates how to:

- Prepare mixed continuous and ordinal data
- Estimate mixed-data latent class models
- Fit multiple class solutions
- Inspect and interpret the resulting OpenMx models

### Requirements

The
[`mx_mixed_lca()`](https://cjvanlissa.github.io/tidySEM/reference/mx_mixed_lca.md)
function relies on `OpenMx`. Make sure both packages are installed.

``` r
library(tidySEM)
library(OpenMx)
#> To take full advantage of multiple cores, use:
#>   mxOption(key='Number of Threads', value=parallel::detectCores()) #now
#>   Sys.setenv(OMP_NUM_THREADS=parallel::detectCores()) #before library(OpenMx)
```

### Example Data

We simulate a dataset with:

- Three continuous indicators
- One ordinal indicator
- Two latent classes

``` r
set.seed(10)
n <- 200

# Set class-specific means
class_means <- c(rep(0, floor(0.3 * n)),
rep(2, ceiling(0.7 * n)))

# Simulate continuous indicators
df <- rnorm(4 * n, mean = rep(class_means, 4))
df <- matrix(df, nrow = n)
df <- t(t(df) * c(1, 2, 0.5, 1))
df <- data.frame(df)
names(df) <- paste0("X", 1:4)

# Convert one indicator to ordinal
df$X4 <- cut(df$X4, breaks = 3, labels = FALSE)
df$X4 <- mxFactor(df$X4, levels = 1:3)
```

### Model Estimation with `mx_mixed_lca()`

The
[`mx_mixed_lca()`](https://cjvanlissa.github.io/tidySEM/reference/mx_mixed_lca.md)
function estimates mixed-data latent class models using the following
procedure:

1.  Estimate an **Latent profile analysis (LPA)** for the continuous
    indicators using
    [`mx_profiles()`](https://cjvanlissa.github.io/tidySEM/reference/mx_profiles.md)
2.  Use the **BCH method** to obtain starting values for ordinal
    indicators: the classes probabilities from step 1 are used to
    estimate thresholds for the remaining ordinal indicators.
3.  **Latent class analysis (LCA)** for ordinal indicators using
    [`mx_lca()`](https://cjvanlissa.github.io/tidySEM/reference/mx_lca.md),
    using the thresholds from step 2. as starting values.
4.  **Joint estimation** of continuous and ordinal indicators in a
    single model, using the results from steps 1. and 3. as starting
    values.

#### Estimating a Single Class Solution

To estimate a 2-class mixed-data latent class model, use the following
code. Note that you will see several notifications concerning model
estimation, as the aforementioned steps are completed.

``` r
res_2 <- mx_mixed_lca(
data = df,
classes = 2
)
#> Running mix2 with 10 parameters
#> Running mix2 with 10 parameters
#> Running aux with 4 parameters
#> Running mix with 14 parameters
#> 
#> Beginning initial fit attempt
#> Running mix with 14 parameters
#> 
#>  Lowest minimum so far:  2237.08334440481
#> 
#> Beginning fit attempt 1 of at maximum 10 extra tries
#> Running mix with 14 parameters
#> 
#>  Lowest minimum so far:  2237.08334438657
#> 
#> Beginning fit attempt 2 of at maximum 10 extra tries
#> Running mix with 14 parameters
#> 
#> Beginning fit attempt 3 of at maximum 10 extra tries
#> Running mix with 14 parameters
#> 
#> Beginning fit attempt 4 of at maximum 10 extra tries
#> Running mix with 14 parameters
#> 
#> Beginning fit attempt 5 of at maximum 10 extra tries
#> Running mix with 14 parameters
#> 
#>  Lowest minimum so far:  2237.08334438554
#> 
#> Beginning fit attempt 6 of at maximum 10 extra tries
#> Running mix with 14 parameters
#> 
#>  Lowest minimum so far:  2237.08334438449
#> 
#> Beginning fit attempt 7 of at maximum 10 extra tries
#> Running mix with 14 parameters
#> 
#> Beginning fit attempt 8 of at maximum 10 extra tries
#> Running mix with 14 parameters
#> 
#>  Lowest minimum so far:  2237.08334438444
#> 
#> Beginning fit attempt 9 of at maximum 10 extra tries
#> Running mix with 14 parameters
#> 
#>  Lowest minimum so far:  2237.08334438432
#> 
#> Beginning fit attempt 10 of at maximum 10 extra tries
#> Running mix with 14 parameters
#> 
#>  Fit attempt worse than current best:  2501.09547530228 vs 2237.08334438432
#> 
#> Retry limit reached
#> 
#> Solution found
#> Final run, for Hessian and/or standard errors and/or confidence intervals
#> Running mix with 14 parameters
#>  Warning messages generated from final run for Hessian/SEs/CIs
```

    #> 
    #>  Solution found!  Final fit=2237.0833 (started at 2238.2759)  (11 attempt(s): 11 valid, 0 errors)
    #>  Start values from best fit:
    #> 0.434564376560343,0.886846818403691,3.93590771178758,0.310218680847869,1.94113923528429,4.39272132467571,1.01180424277323,-1.23587289760936,2.12676561778231,-0.227372818807064,-0.113786407828762,-0.00175480568398363,0.647172207312266,7.14655875041862

The returned object is an
[`OpenMx::mxModel`](https://rdrr.io/pkg/OpenMx/man/mxModel.html), and
can be modified using the functions in that package:

``` r
class(res_2)
#> [1] "MxModel"
#> attr(,"package")
#> [1] "OpenMx"
```

### Estimating Multiple Class Solutions

A common workflow is to estimate several class solutions and compare
model fit. This can be done by passing a vector of class numbers.

``` r
res_1_3 <- mx_mixed_lca(
  data = df,
  classes = 1:3
)
#> Running mix1 with 6 parameters
#> Running mix1 with 8 parameters
#> 
#> Beginning initial fit attempt
#> Running mix1 with 8 parameters
#> 
#>  Lowest minimum so far:  2501.0554772669
#> 
#> Beginning fit attempt 1 of at maximum 10 extra tries
#> Running mix1 with 8 parameters
#> 
#> Beginning fit attempt 2 of at maximum 10 extra tries
#> Running mix1 with 8 parameters
#> 
#> Beginning fit attempt 3 of at maximum 10 extra tries
#> Running mix1 with 8 parameters
#> 
#> Beginning fit attempt 4 of at maximum 10 extra tries
#> Running mix1 with 8 parameters
#> 
#> Beginning fit attempt 5 of at maximum 10 extra tries
#> Running mix1 with 8 parameters
#> 
#> Beginning fit attempt 6 of at maximum 10 extra tries
#> Running mix1 with 8 parameters
#> 
#> Beginning fit attempt 7 of at maximum 10 extra tries
#> Running mix1 with 8 parameters
#> 
#> Beginning fit attempt 8 of at maximum 10 extra tries
#> Running mix1 with 8 parameters
#> 
#> Beginning fit attempt 9 of at maximum 10 extra tries
#> Running mix1 with 8 parameters
#> 
#> Beginning fit attempt 10 of at maximum 10 extra tries
#> Running mix1 with 8 parameters
#> 
#> Retry limit reached
#> 
#> Solution found
#> Final run, for Hessian and/or standard errors and/or confidence intervals
#> Running mix1 with 8 parameters
```

    #> 
    #>  Solution found!  Final fit=2501.0555 (started at 2501.0555)  (11 attempt(s): 11 valid, 0 errors)
    #>  Start values from best fit:
    #> 1.8798199150263,8.22430128557246,0.527144866359322,1.28424434013726,3.02759263921926,0.704772507629557,-0.52440058064318,1.65079193999047
    #> Running mix2 with 10 parameters
    #> Running mix2 with 10 parameters
    #> Running aux with 4 parameters
    #> Running mix with 14 parameters
    #> 
    #> Beginning initial fit attempt
    #> Running mix with 14 parameters
    #> 
    #>  Lowest minimum so far:  2237.08334440364
    #> 
    #> Beginning fit attempt 1 of at maximum 10 extra tries
    #> Running mix with 14 parameters
    #> 
    #> Beginning fit attempt 2 of at maximum 10 extra tries
    #> Running mix with 14 parameters
    #> 
    #>  Lowest minimum so far:  2237.08334438442
    #> 
    #> Beginning fit attempt 3 of at maximum 10 extra tries
    #> Running mix with 14 parameters
    #> 
    #> Beginning fit attempt 4 of at maximum 10 extra tries
    #> Running mix with 14 parameters
    #> 
    #> Beginning fit attempt 5 of at maximum 10 extra tries
    #> Running mix with 14 parameters
    #> 
    #>  Lowest minimum so far:  2237.08334438433
    #> 
    #> Beginning fit attempt 6 of at maximum 10 extra tries
    #> Running mix with 14 parameters
    #> 
    #> Beginning fit attempt 7 of at maximum 10 extra tries
    #> Running mix with 14 parameters
    #> 
    #> Beginning fit attempt 8 of at maximum 10 extra tries
    #> Running mix with 14 parameters
    #> 
    #>  Fit attempt worse than current best:  2501.09547526706 vs 2237.08334438433
    #> 
    #> Beginning fit attempt 9 of at maximum 10 extra tries
    #> Running mix with 14 parameters
    #> 
    #> Beginning fit attempt 10 of at maximum 10 extra tries
    #> Running mix with 14 parameters
    #> 
    #> Retry limit reached
    #> 
    #> Solution found
    #> Final run, for Hessian and/or standard errors and/or confidence intervals
    #> Running mix with 14 parameters

    #> 
    #>  Solution found!  Final fit=2237.0833 (started at 2238.2759)  (11 attempt(s): 11 valid, 0 errors)
    #>  Start values from best fit:
    #> 0.434564376342062,0.886846820973262,3.93590742681462,0.310218688139981,1.94113905985034,4.39272134312443,1.01180434594834,-1.23587250275016,2.12676470213211,-0.227372847134914,-0.113786382872647,-0.00175478157242963,0.647172531897165,5.54724493345608
    #> Running mix3 with 14 parameters
    #> Running mix3 with 14 parameters
    #> Running aux with 6 parameters
    #> Running mix with 20 parameters
    #> 
    #> Beginning initial fit attempt
    #> Running mix with 20 parameters
    #> 
    #>  Lowest minimum so far:  2230.74793597882
    #> 
    #> Beginning fit attempt 1 of at maximum 10 extra tries
    #> Running mix with 20 parameters
    #> 
    #>  Lowest minimum so far:  2230.74793597699
    #> 
    #> Beginning fit attempt 2 of at maximum 10 extra tries
    #> Running mix with 20 parameters
    #> 
    #>  Lowest minimum so far:  2230.74793597577
    #> 
    #> Beginning fit attempt 3 of at maximum 10 extra tries
    #> Running mix with 20 parameters
    #> 
    #> Beginning fit attempt 4 of at maximum 10 extra tries
    #> Running mix with 20 parameters
    #> 
    #>  Lowest minimum so far:  2230.7479359649
    #> 
    #> Beginning fit attempt 5 of at maximum 10 extra tries
    #> Running mix with 20 parameters
    #> 
    #> Beginning fit attempt 6 of at maximum 10 extra tries
    #> Running mix with 20 parameters
    #> 
    #> Beginning fit attempt 7 of at maximum 10 extra tries
    #> Running mix with 20 parameters
    #> 
    #>  Lowest minimum so far:  2229.24270208486
    #> 
    #> Beginning fit attempt 8 of at maximum 10 extra tries
    #> Running mix with 20 parameters
    #> 
    #> Beginning fit attempt 9 of at maximum 10 extra tries
    #> Running mix with 20 parameters
    #> 
    #>  Lowest minimum so far:  2229.24270170005
    #> 
    #> Beginning fit attempt 10 of at maximum 10 extra tries
    #> Running mix with 20 parameters
    #> 
    #>  Lowest minimum so far:  2229.24270167369
    #> 
    #> Retry limit reached
    #> 
    #> Solution found
    #> Final run, for Hessian and/or standard errors and/or confidence intervals
    #> Running mix with 20 parameters
    #>  Warning messages generated from final run for Hessian/SEs/CIs

    #> 
    #>  Solution found!  Final fit=2229.2427 (started at 2233.3982)  (11 attempt(s): 11 valid, 0 errors)
    #>  Start values from best fit:
    #> 0.461871704498314,0.0586137611174605,0.845889422646711,3.75921295062132,0.310479868310153,2.0024598940212,4.28364375999033,1.00967201881447,-1.51475238123747,2.39938624411697,-0.221716373262513,-0.114749143036037,0.000196674341689011,0.645530862396846,6.56198384989231,0.89773163165368,6.35965321760118,1.05493295890808,0.986045172195464,0.00100000000000022

The result is a list of OpenMx models, one for each class solution.

### Inspecting Model Results

The model fit can be inspected by printing the object, or calling
`table_fit(res_1_3)`:

``` r
table_fit(res_1_3)
#>   Name Classes        LL   n Parameters      AIC      BIC    saBIC   Entropy
#> 1    1       1 -1250.528 200          8 2517.055 2543.442 2518.097 1.0000000
#> 2    2       2 -1118.542 200         14 2265.083 2311.260 2266.906 0.9304968
#> 3    3       3 -1114.621 200         20 2269.243 2335.209 2271.847 0.9106430
#>    prob_min  prob_max n_min n_max np_ratio   np_local
#> 1 1.0000000 1.0000000 1.000 1.000 25.00000 25.0000000
#> 2 0.9606636 0.9942737 0.295 0.705 14.28571  9.0769231
#> 3 0.5517685 0.9904940 0.025 0.680 10.00000  0.8333333
```

Note that, as expected, the BIC for the 2-class solution is lowest. The
3-class solution has an extremely low ratio of cases to parameters, so
this model is most likely overfit.

The class proportions for the two-class solution are printed in this
table too (`n_min` and `n_max`); note that they correspond nicely to the
simulated .3/.7 split.

We can examine the parameter values using
[`table_results()`](https://cjvanlissa.github.io/tidySEM/reference/table_results.md)
on the second element of the model list, or the 2-class model:

``` r
table_results(res_1_3[[2]])
#>                     label  est_sig      se pval             confint  class
#> 1                Means.X1  1.94***    0.08 0.00        [1.78, 2.10] class1
#> 2                Means.X2  4.39***    0.17 0.00        [4.05, 4.73] class1
#> 3                Means.X3  1.01***    0.05 0.00        [0.92, 1.11] class1
#> 4            Variances.X1  0.89***    0.09 0.00        [0.71, 1.07] class1
#> 5            Variances.X2  3.94***    0.42 0.00        [3.12, 4.75] class1
#> 6            Variances.X3  0.31***    0.03 0.00        [0.25, 0.37] class1
#> 7            Variances.X4     1.00    <NA> <NA>                <NA> class1
#> 8  class1.Thresholds[1,1] -1.24***    0.15 0.00      [-1.52, -0.95] class1
#> 9  class1.Thresholds[2,1]  0.89***    0.12 0.00        [0.65, 1.13] class1
#> 10               Means.X1    -0.23    0.13 0.07       [-0.48, 0.02] class2
#> 11               Means.X2    -0.11    0.27 0.67       [-0.64, 0.41] class2
#> 12               Means.X3    -0.00    0.07 0.98       [-0.15, 0.14] class2
#> 13           Variances.X4     1.00    <NA> <NA>                <NA> class2
#> 14 class2.Thresholds[1,1]  0.65***    0.18 0.00        [0.29, 1.01] class2
#> 15 class2.Thresholds[2,1]     6.19 1338.52 1.00 [-2617.25, 2629.64] class2
#> 16       mix.weights[1,1]     1.00    <NA> <NA>                <NA>   <NA>
#> 17       mix.weights[1,2]  0.43***    0.07 0.00        [0.30, 0.57]   <NA>
```

Note that we get free means for each class, with the variances
constrained to be equal across classes. For the categorical variable, we
get thresholds: These correspond to quartiles of a normal distribution.
For class 1, the probability of scoring within the first response
category corresponds to `pnorm(-2.38, lower.tail = TRUE)`, or about 1%.
To convert these thresholds to the probability scale, we can run:

``` r
table_prob(res_1_3[[2]])
#>   Variable Category  Probability  group
#> 1       X4        1 1.082530e-01 class1
#> 2       X4        2 7.052535e-01 class1
#> 3       X4        3 1.864935e-01 class1
#> 4       X4        1 7.412399e-01 class2
#> 5       X4        2 2.587601e-01 class2
#> 6       X4        3 2.925049e-10 class2
```

### Advanced Options

Additional arguments can be passed via `...` and are forwarded to the
underlying model-building functions. For example, you can release
thevariance constraints for the continuous indicators as follows:

``` r
res_2_free <- mx_mixed_lca(
  data = df,
  classes = 2,
  variances = "free"
)
#> Running mix2 with 13 parameters
#> Running mix2 with 13 parameters
#> Running aux with 4 parameters
#> Running mix with 17 parameters
#> 
#> Beginning initial fit attempt
#> Running mix with 17 parameters
#> 
#>  Lowest minimum so far:  2235.93484633752
#> 
#> Beginning fit attempt 1 of at maximum 10 extra tries
#> Running mix with 17 parameters
#> 
#> Beginning fit attempt 2 of at maximum 10 extra tries
#> Running mix with 17 parameters
#> 
#> Beginning fit attempt 3 of at maximum 10 extra tries
#> Running mix with 17 parameters
#> 
#>  Lowest minimum so far:  2235.93484633027
#> 
#> Beginning fit attempt 4 of at maximum 10 extra tries
#> Running mix with 17 parameters
#> 
#>  Lowest minimum so far:  2235.93484631656
#> 
#> Beginning fit attempt 5 of at maximum 10 extra tries
#> Running mix with 17 parameters
#> 
#>  Fit attempt worse than current best:  2501.09547533531 vs 2235.93484631656
#> 
#> Beginning fit attempt 6 of at maximum 10 extra tries
#> Running mix with 17 parameters
#> 
#> Beginning fit attempt 7 of at maximum 10 extra tries
#> Running mix with 17 parameters
#> 
#> Beginning fit attempt 8 of at maximum 10 extra tries
#> Running mix with 17 parameters
#> 
#> Beginning fit attempt 9 of at maximum 10 extra tries
#> Running mix with 17 parameters
#> 
#> Beginning fit attempt 10 of at maximum 10 extra tries
#> Running mix with 17 parameters
#> 
#> Retry limit reached
#> 
#> Solution found
#> Final run, for Hessian and/or standard errors and/or confidence intervals
#> Running mix with 17 parameters
```

    #> 
    #>  Solution found!  Final fit=2235.9349 (started at 2237.0608)  (11 attempt(s): 11 valid, 0 errors)
    #>  Start values from best fit:
    #> 0.428080203415083,0.924889124074763,4.1627189913582,0.311601605055015,1.93668278567968,4.38259915295965,1.00916290967712,-1.23068042799634,2.12471971794995,0.793277075362616,3.40396220273292,0.308612160090093,-0.239858195067663,-0.137720404121177,-0.00628606939540307,0.660897621241448,4.60350239254635

We can test whether releasing the variances significantly changes the
model fit using an ANOVA:

``` r
anova(res_1_3[[2]], res_2_free)
#>   base comparison ep minus2LL  df      AIC   diffLL diffdf        p
#> 1  mix       <NA> 17 2235.935 783 2269.935       NA     NA       NA
#> 2  mix        mix 14 2237.083 786 2265.083 1.148491      3 0.765383
```

With a p-value of `0.63`, we conclude that the added complexity does not
significantly improve the model (as expected, because we did not
simulate class-specific variances).

## Plotting the Model

The model can be plot with the usual functions, but note that
categorical indicators will not look good in plots for continuous
indicators, and could lead to errors.

Thus, for example, we can use a profile plot for the continuous
indicators:

``` r
plot_profiles(res_1_3[[2]], variables = c("X1", "X2", "X3"))
```

![](mixed_lca_files/figure-html/unnamed-chunk-12-1.png)

Alternatively, we can use a bivariate plot with densities:

``` r
plot_bivariate(res_1_3[[2]], variables = c("X1", "X2", "X3"))
```

![](mixed_lca_files/figure-html/unnamed-chunk-13-1.png)

We can plot the categorical variables as follows:

``` r
plot_prob(res_1_3[[2]])
```

![](mixed_lca_files/figure-html/unnamed-chunk-14-1.png)

The
[`mx_mixed_lca()`](https://cjvanlissa.github.io/tidySEM/reference/mx_mixed_lca.md)
function is particularly useful when:

- Your indicators include **both continuous and ordinal variables**
- You want **robust starting values** to reduce convergence issues
- You prefer a **single high-level interface** to OpenMx mixture
  modeling

For purely continuous indicators, consider
[`mx_profiles()`](https://cjvanlissa.github.io/tidySEM/reference/mx_profiles.md).
For purely categorical indicators, consider
[`mx_lca()`](https://cjvanlissa.github.io/tidySEM/reference/mx_lca.md).

### References

Van Lissa, C. J., Garnier-Villarreal, M., & Anadria, D. (2023).
*Recommended Practices in Latent Class Analysis using the Open-Source
R-Package tidySEM.* Structural Equation Modeling.
<https://doi.org/10.1080/10705511.2023.2250920>
