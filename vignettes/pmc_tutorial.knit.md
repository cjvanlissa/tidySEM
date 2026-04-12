---
title: "Tutorial PMC"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{tutorial_pmc}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---




This tutorial requires `tidySEM` version `0.2.11` or higher and `OpenMx`. Make sure both packages are
installed and loaded.


``` r
library(tidySEM)
library(OpenMx)
```

## Example Data

We simulate a dataset with two continuous indicators, and two latent classes.


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
#> MxComputeSimAnnealing(tsallis1996) evaluations 2206 fit 2889.68 change -2413MxComputeSimAnnealing(tsallis1996) evaluations 7375 fit 1890.01 change 569.6                                                                            
#> MxComputeSimAnnealing(tsallis1996) evaluations 1824 fit 1863.63 change 199.8MxComputeSimAnnealing(tsallis1996) evaluations 5517 fit 1346.4 change -245  MxComputeSimAnnealing(tsallis1996) evaluations 9146 fit 1311.49 change 1.025MxComputeSimAnnealing(tsallis1996) evaluations 12776 fit 1310.19 change -1.569                                                                              
```

Optionally, one can perform the bootstrapped likelihood ratio test.
This takes a long time to run, so we conduct only 100 replications, rather than a more sensible number like 1000.
To accelerate computations, we can use the `future` package for parallel computing (see `?plan` to select the appropriate back-end for your system).


``` r
library(future)
plan(multisession)
res_blrt <- BLRT(res, replications = 100)
res_blrt
```


```
#> Bootstrapped Likelihood Ratio Test:
#> 
#>  null  alt    lr df blrt_p samples
#>  mix1 mix2 91.95  3   0.00      98
#>  mix2 mix3  3.41  3   0.44      96
```

This test confirms that the 2-class solution is significantly better than the 1-class solution - but the 3-class solution offers no further significant improvement.

Next, we use predictive model comparison. If all variables are continuous,
the function `pmc()` uses the `srmr()` function to compare the standardized root mean squared (SRMR) difference between the correlation matrices of the real and model-implied data:


``` r
set.seed(1)
res_pmc <- pmc(res)
```


```
#> PMC model comparison using SRMR:
#> 
#>  comparison null  alt  null_stat   alt_stat          lb          ub sig
#>     dif_seq mix1 mix2 0.51665420 0.03633316 -0.64415512 -0.34942787   *
#>     dif_seq mix2 mix3 0.03633316 0.03439975 -0.07001605  0.08783185    
#>     dif_one mix1 mix2 0.51665420 0.03633316 -0.64415512 -0.34942787   *
#>     dif_one mix1 mix3 0.51665420 0.03439975 -0.63939182 -0.36371510   *
```

This test, too, confirms that the 2-class solution is significantly better than the 1-class solution - but the 3-class solution offers no further significant improvement.

If we conduct an LCA with ordinal indicators, the function uses a chi squared statistic instead of SRMR.
To demonstrate this, first, we convert the data to ordinal.
Note that real research data should not be polytomized like this.
Then, we estimate a model for ordinal indicators using `mx_lca()`.
We specify a custom function that references `x` and `y`, where both are of type `data.frame`,
and we supply it as an argument to `pmc()`:


``` r
# Convert the indicators to ordinal
df[] <- lapply(df, cut, breaks = 3, labels = FALSE)
df[] <- lapply(df, mxFactor, levels = 1:3)
res_cat <- mx_lca(df, classes = 1:3)
pmc(res_cat, reps = 20)
```

```
#> Beginning initial fit attemptFit attempt 0, fit=813.722003986849, new current best! (was 813.72200398685)Beginning fit attempt 1 of at maximum 10 extra tries                        Beginning fit attempt 2 of at maximum 10 extra triesBeginning fit attempt 3 of at maximum 10 extra triesBeginning fit attempt 4 of at maximum 10 extra triesBeginning fit attempt 5 of at maximum 10 extra triesBeginning fit attempt 6 of at maximum 10 extra triesBeginning fit attempt 7 of at maximum 10 extra triesBeginning fit attempt 8 of at maximum 10 extra triesBeginning fit attempt 9 of at maximum 10 extra triesBeginning fit attempt 10 of at maximum 10 extra triesFinal run, for Hessian and/or standard errors and/or confidence intervals                                                                         
#> Beginning initial fit attemptFit attempt 0, fit=813.722003986846, new current best! (was 813.722003986849)Beginning fit attempt 1 of at maximum 10 extra tries                         Fit attempt 1, fit=763.493917717475, new current best! (was 813.722003986846)Beginning fit attempt 2 of at maximum 10 extra tries                         Fit attempt 2, fit=763.493917717435, new current best! (was 763.493917717475)Beginning fit attempt 3 of at maximum 10 extra tries                         Fit attempt 3, fit=763.493917717433, new current best! (was 763.493917717435)Beginning fit attempt 4 of at maximum 10 extra tries                         Beginning fit attempt 5 of at maximum 10 extra triesFit attempt 5, fit=763.493917717433, new current best! (was 763.493917717433)Beginning fit attempt 6 of at maximum 10 extra tries                         Beginning fit attempt 7 of at maximum 10 extra triesBeginning fit attempt 8 of at maximum 10 extra triesBeginning fit attempt 9 of at maximum 10 extra triesBeginning fit attempt 10 of at maximum 10 extra triesFit attempt 10, fit=763.493917717431, new current best! (was 763.493917717433)Final run, for Hessian and/or standard errors and/or confidence intervals                                                                              
#> Beginning initial fit attemptFit attempt 0, fit=813.722003986846, new current best! (was 813.722003986849)Beginning fit attempt 1 of at maximum 10 extra tries                         Fit attempt 1, fit=763.490198334132, new current best! (was 813.722003986846)Beginning fit attempt 2 of at maximum 10 extra tries                         Beginning fit attempt 3 of at maximum 10 extra triesFit attempt 3, fit=763.490198334131, new current best! (was 763.490198334132)Beginning fit attempt 4 of at maximum 10 extra tries                         Beginning fit attempt 5 of at maximum 10 extra triesBeginning fit attempt 6 of at maximum 10 extra triesFit attempt 6, fit=763.49019833413, new current best! (was 763.490198334131)Beginning fit attempt 7 of at maximum 10 extra tries                        Beginning fit attempt 8 of at maximum 10 extra triesFit attempt 8, fit=763.490198334128, new current best! (was 763.49019833413)Beginning fit attempt 9 of at maximum 10 extra tries                        Beginning fit attempt 10 of at maximum 10 extra triesFinal run, for Hessian and/or standard errors and/or confidence intervals                                                                         
```

```
#> PMC model comparison using chi squared:
#> 
#>  comparison null  alt null_stat  alt_stat         lb        ub sig
#>     dif_seq mix1 mix2 -143.2320 -186.6654 -73.989003 -27.04252   *
#>     dif_seq mix2 mix3 -186.6654 -181.3458  -3.330308  17.80954    
#>     dif_one mix1 mix2 -143.2320 -186.6654 -73.989003 -27.04252   *
#>     dif_one mix1 mix3 -143.2320 -181.3458 -61.632078 -15.16548   *
```

The argument `FUN` allows users to override these defaults, and specify any custom function to compare the observed data (`x`) and model-implied data (`y`).

## References

Van Lissa, C. J., Garnier-Villarreal, M., & Anadria, D. (2023).
*Recommended Practices in Latent Class Analysis using the Open-Source R-Package
tidySEM.* Structural Equation Modeling.
[https://doi.org/10.1080/10705511.2023.2250920](https://doi.org/10.1080/10705511.2023.2250920)
