# Tutorial PMC

This tutorial requires `tidySEM` version `0.2.11` or higher and
`OpenMx`. Make sure both packages are installed and loaded.

``` r

library(tidySEM)
library(OpenMx)
```

## Example Data

We simulate a dataset with two continuous indicators, and two latent
classes.

``` r

set.seed(10)
n <- 200
# Set class-specific means
class_means <- c(rep(0, floor(0.3 * n)), rep(2, ceiling(0.7 * n)))

# Simulate continuous indicators
df <- rnorm(2 * n, mean = rep(class_means, 2))
df <- data.frame(matrix(df, nrow = n))
names(df) <- paste0("X", 1:2)
```

Next, we estimate 1-3 class solutions:

``` r

res <- mx_profiles(data = df, classes = 1:3)
```

Optionally, one can perform the bootstrapped likelihood ratio test. This
takes a long time to run, so we conduct only 100 replications, rather
than a more sensible number like 1000. To accelerate computations, we
can use the `future` package for parallel computing (see
[`?plan`](https://future.futureverse.org/reference/plan.html) to select
the appropriate back-end for your system).

``` r

library(future)
plan(multisession)
res_blrt <- BLRT(res, replications = 100)
res_blrt
```

    #>   null  alt        lr df blrt_p samples
    #> 1 mix1 mix2 91.949066  3   0.00      99
    #> 2 mix2 mix3  3.406989  3   0.44      96

This test confirms that the 2-class solution is significantly better
than the 1-class solution - but the 3-class solution offers no further
significant improvement.

Next, we use predictive model comparison. If all variables are
continuous, the function
[`pmc()`](https://cjvanlissa.github.io/tidySEM/reference/pmc.md) uses
the [`srmr()`](https://cjvanlissa.github.io/tidySEM/reference/srmr.md)
function to compare the standardized root mean squared (SRMR) difference
between the correlation matrices of the real and model-implied data:

``` r

set.seed(1)
res_pmc <- pmc(res)
```

    #>   comparison null  alt  null_stat   alt_stat          lb          ub sig
    #> 1    dif_seq mix1 mix2 0.51665420 0.03633316 -0.64415512 -0.34942787   *
    #> 2    dif_seq mix2 mix3 0.03633316 0.03439975 -0.07001605  0.08783185    
    #> 3    dif_one mix1 mix2 0.51665420 0.03633316 -0.64415512 -0.34942787   *
    #> 4    dif_one mix1 mix3 0.51665420 0.03439975 -0.63939182 -0.36371510   *

This test, too, confirms that the 2-class solution is significantly
better than the 1-class solution - but the 3-class solution offers no
further significant improvement.

If we conduct an LCA with ordinal indicators, the function uses a chi
squared statistic instead of SRMR. To demonstrate this, first, we
convert the data to ordinal. Note that real research data should not be
polytomized like this. Then, we estimate a model for ordinal indicators
using
[`mx_lca()`](https://cjvanlissa.github.io/tidySEM/reference/mx_lca.md).
We specify a custom function that references `x` and `y`, where both are
of type `data.frame`, and we supply it as an argument to
[`pmc()`](https://cjvanlissa.github.io/tidySEM/reference/pmc.md):

``` r

# Convert the indicators to ordinal
df[] <- lapply(df, cut, breaks = 3, labels = FALSE)
df[] <- lapply(df, mxFactor, levels = 1:3)
res_cat <- mx_lca(df, classes = 1:3)
pmc(res_cat, reps = 20)
```

    #>   comparison null  alt null_stat  alt_stat         lb        ub sig
    #> 1    dif_seq mix1 mix2 -143.2320 -186.6654 -73.989003 -27.04252   *
    #> 2    dif_seq mix2 mix3 -186.6654 -181.3458  -3.330308  17.80954    
    #> 3    dif_one mix1 mix2 -143.2320 -186.6654 -73.989003 -27.04252   *
    #> 4    dif_one mix1 mix3 -143.2320 -181.3458 -61.632078 -15.16548   *

The argument `FUN` allows users to override these defaults, and specify
any custom function to compare the observed data (`x`) and model-implied
data (`y`).

## References

Van Lissa, C. J., Garnier-Villarreal, M., & Anadria, D. (2023).
*Recommended Practices in Latent Class Analysis using the Open-Source
R-Package tidySEM.* Structural Equation Modeling.
<https://doi.org/10.1080/10705511.2023.2250920>
