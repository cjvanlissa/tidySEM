---
title: "Latent Class Growth Analysis"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Latent Class Growth Analysis}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---



This vignette illustrated `tidySEM`'s ability to perform latent class growth analysis, or growth mixture modeling,
as explained in Van Lissa, C. J., Garnier-Villarreal, M., & Anadria, D. (2023). *Recommended Practices in Latent Class Analysis using the Open-Source R-Package tidySEM.* Structural Equation Modeling. https://doi.org/10.1080/10705511.2023.2250920.
The simulated data used for this example are inspired by work in progress by Plas and colleagues,
on heterogeneity in depression trajectories among Dutch military personnel who were deployed to Afghanistan.
The original data were collected as part of the *Prospection in Stress-related Military Research (PRISMO)* study,
which examined of psychological problems after deployment in more than 1,000 Dutch military personnel from 2005-2019.

First, we load all required packages:


``` r
library(tidySEM)
library(OpenMx)
library(ggplot2)
library(MASS)
```

## Data preprocessing

We first examined the descriptive statistics for the sum score scales:


``` r
# Get descriptives
df <- plas_depression
desc <- descriptives(df)
desc <- desc[, c("name", "mean", "median", "sd", "min", "max",
    "skew_2se", "kurt_2se")]
knitr::kable(desc, caption = "Item descriptives")
```



Table: Item descriptives

|name  | mean| median|  sd| min| max| skew_2se| kurt_2se|
|:-----|----:|------:|---:|---:|---:|--------:|--------:|
|scl.1 |   20|     20| 2.4|  17|  38|       15|       31|
|scl.2 |   20|     19| 3.5|  16|  64|       26|      103|
|scl.3 |   20|     20| 3.4|  17|  59|       26|       98|
|scl.4 |   21|     20| 3.4|  16|  50|       18|       45|
|scl.5 |   21|     20| 4.1|  16|  64|       25|       84|
|scl.6 |   21|     20| 4.1|  16|  58|       20|       56|



Note that all variables were extremely right-skewed due to censoring at the lower end of the scale.

We can examine these distributions visually as well:


``` r
df_plot <- reshape(df, direction = "long", varying = names(df))
ggplot(df_plot, aes(x = scl)) + geom_density() + facet_wrap(~time) +
    theme_bw()
```

<img src="plot_dist.png" alt="" width="80%" />


As this type of skew can result in convergence problems in LCGA,
we compared several transformations to reduce skew:
The square and cube root, log, inverse, and Box-Cox transformations.


``` r
df_scores <- df_plot
# Store original range of SCL
rng_scl <- range(df_scores$scl)
# Log-transform
df_scores$log <- scales::rescale(log(df_scores$scl), to = c(0,
    1))
# Square root transform
df_scores$sqrt <- scales::rescale(sqrt(df_scores$scl), to = c(0,
    1))
# Cube root transform
df_scores$qrt <- scales::rescale(df_scores$scl^0.33, to = c(0,
    1))
# Reciprocal transform
df_scores$reciprocal <- scales::rescale(1/df_scores$scl, to = c(0,
    1))
# Define function for Box-Cox transformation
bc <- function(x, lambda) {
    (((x^lambda) - 1)/lambda)
}
# Inverse Box-Cox transformation
invbc <- function(x, lambda) {
    ((x * lambda) + 1)^(1/lambda)
}
# Box-Cox transform
b <- MASS::boxcox(lm(df_scores$scl ~ 1), plotit = FALSE)
lambda <- b$x[which.max(b$y)]
df_scores$boxcox <- bc(df_scores$scl, lambda)
# Store range of Box-Cox transformed data
rng_bc <- range(df_scores$boxcox)
df_scores$boxcox <- scales::rescale(df_scores$boxcox, to = c(0,
    1))
# Rescale SCL
df_scores$scl <- scales::rescale(df_scores$scl, to = c(0, 1))
```

We can plot these transformations:


``` r
# Make plot data
df_plot <- do.call(rbind, lapply(c("scl", "log", "sqrt", "qrt",
    "boxcox"), function(n) {
    data.frame(df_scores[c("time", "id")], Value = df_scores[[n]],
        Transformation = n)
}))
# Plot
ggplot(df_plot, aes(x = Value, colour = Transformation)) + geom_density() +
    facet_wrap(~time) + scale_y_sqrt() + xlab("scl (rescaled to 0-1)") +
    theme_bw()
```

<img src="plot_trans.png" alt="" width="80%" />


Evidently, the Box-Cox transformation reduced skew the most.
Consequently, we proceeded with the Box-Cox transformed scores for analysis.


``` r
dat <- df_scores[, c("id", "time", "boxcox")]
dat <- reshape(dat, direction = "wide", v.names = "boxcox", timevar = "time",
    idvar = "id")
names(dat) <- gsub("boxcox.", "scl", names(dat))
```

# Latent Class Growth Analysis

Next, we estimated a latent class growth model
for SCL.
The model included an overall intercept, centered at T1, `i`.
To model the potential effect of deployment on
depresion,
we also included a dummy variable that was zero before
deployment, and 1 after deployment, `step`.
Finally, to model potential change (or recovery) in depression post-deployment,
we included a linear slope from T2-T6, `s`.
All variances of growth parameters were fixed to zero due to the sparse nature of the data.
In this vignette,
we do not consider more than 5 classes,
because the analyses are computationally very intensive and the data were simulated from a 3-class model.

It is important to highlight that in LCGA, the subgroups will be limited by
the specify growth structure, meaning that LCA will identify distinctive
growth patterns within the intercept, step, and slope growth. For
example, if there is a subgroup that follows a quadratic growth pattern
this models will not be able to identify it.

**NOTE: The time scales in this model are not correct; it currently assumes that all measurements are equidistant. Feel free to experiment with adjusting this.**


``` r
set.seed(27796)
dat[["id"]] <- NULL
res_step <- mx_growth_mixture(model = "
  i =~ 1*scl1 + 1*scl2 + 1*scl3 +1*scl4 +1*scl5 +1*scl6
  step =~ 0*scl1 + 1*scl2 + 1*scl3 +1*scl4 +1*scl5 +1*scl6
  s =~ 0*scl1 + 0*scl2 + 1*scl3 +2*scl4 +3*scl5 +4*scl6
  scl1 ~~ vscl1*scl1
  scl2 ~~ vscl2*scl2
  scl3 ~~ vscl3*scl3
  scl4 ~~ vscl4*scl4
  scl5 ~~ vscl5*scl5
  scl6 ~~ vscl6*scl6
  i ~~ 0*i
  step ~~ 0*step
  s ~~ 0*s
  i ~~ 0*s
  i ~~ 0*step
  s ~~ 0*step
  scl1~0*1
  scl2~0*1
  scl3~0*1
  scl4~0*1
  scl5~0*1
  scl6~0*1
  i~NA*1
  s~NA*1
  step~NA*1
  ",
    classes = 1:5, data = dat)
# Additional iterations because of convergence problems for
# model 1:
res_step[[1]] <- mxTryHardWideSearch(res_step[[1]], extraTries = 50)
```


```
#> MxComputeSimAnnealing(tsallis1996) evaluations 186 fit -1176.2 change 0MxComputeSimAnnealing(tsallis1996) evaluations 505 fit 6431.72 change 3114MxComputeSimAnnealing(tsallis1996) evaluations 822 fit -1176.2 change 0   MxComputeSimAnnealing(tsallis1996) evaluations 1152 fit -238.73 change -2342MxComputeSimAnnealing(tsallis1996) evaluations 1492 fit -222.523 change -4970MxComputeSimAnnealing(tsallis1996) evaluations 1814 fit 6024.15 change 6817  MxComputeSimAnnealing(tsallis1996) evaluations 2149 fit -1176.2 change 0   MxComputeSimAnnealing(tsallis1996) evaluations 2480 fit 8854.49 change 1.641e+04MxComputeSimAnnealing(tsallis1996) evaluations 2798 fit -6560.97 change -5385   MxComputeSimAnnealing(tsallis1996) evaluations 3131 fit -3923.07 change -1.216e+04MxComputeSimAnnealing(tsallis1996) evaluations 3453 fit 3436.57 change 3845       MxComputeSimAnnealing(tsallis1996) evaluations 3782 fit 12175.6 change 8333MxComputeSimAnnealing(tsallis1996) evaluations 4104 fit -2690.07 change -749.2MxComputeSimAnnealing(tsallis1996) evaluations 4415 fit -2772.26 change -1726 MxComputeSimAnnealing(tsallis1996) evaluations 4734 fit -5989.52 change -1.823e+04MxComputeSimAnnealing(tsallis1996) evaluations 5050 fit -5647.99 change -6809     MxComputeSimAnnealing(tsallis1996) evaluations 5327 fit -4534.29 change -8851MxComputeSimAnnealing(tsallis1996) evaluations 5601 fit -5207.14 change -7987MxComputeSimAnnealing(tsallis1996) evaluations 5871 fit -6447.25 change 1264 MxComputeSimAnnealing(tsallis1996) evaluations 6150 fit 13064.9 change 1.148e+04MxComputeSimAnnealing(tsallis1996) evaluations 6418 fit 4169.27 change 1.16e+04 MxComputeSimAnnealing(tsallis1996) evaluations 6692 fit -7479.73 change -4154  MxComputeSimAnnealing(tsallis1996) evaluations 6963 fit 2115.48 change 6482  MxComputeSimAnnealing(tsallis1996) evaluations 7211 fit 471.35 change 1776 MxComputeSimAnnealing(tsallis1996) evaluations 7483 fit -6037.33 change 1652MxComputeSimAnnealing(tsallis1996) evaluations 7763 fit -7641.02 change -2.132e+04MxComputeSimAnnealing(tsallis1996) evaluations 8059 fit 13683.2 change 2.142e+04  MxComputeSimAnnealing(tsallis1996) evaluations 8374 fit -1551.8 change -1.53e+04MxComputeSimAnnealing(tsallis1996) evaluations 8688 fit -1541.11 change 0       MxComputeSimAnnealing(tsallis1996) evaluations 8991 fit 3618.83 change 6640MxComputeSimAnnealing(tsallis1996) evaluations 9287 fit -7747.39 change -6161MxComputeSimAnnealing(tsallis1996) evaluations 9558 fit -7663.81 change 84.42MxComputeSimAnnealing(tsallis1996) evaluations 9831 fit -7739.27 change -6075MxComputeSimAnnealing(tsallis1996) evaluations 10074 fit -7613.51 change -2928MxComputeSimAnnealing(tsallis1996) evaluations 10345 fit -7749.18 change -204.4MxComputeSimAnnealing(tsallis1996) evaluations 10609 fit 14373.2 change 278.8  MxComputeSimAnnealing(tsallis1996) evaluations 10862 fit -6737.69 change -6398MxComputeSimAnnealing(tsallis1996) evaluations 11134 fit -7690.94 change -9543MxComputeSimAnnealing(tsallis1996) evaluations 11400 fit 14692.9 change 1.571e+04MxComputeSimAnnealing(tsallis1996) evaluations 11672 fit -3763.7 change 3779     MxComputeSimAnnealing(tsallis1996) evaluations 11940 fit -4207.15 change 3538MxComputeSimAnnealing(tsallis1996) evaluations 12207 fit -7264.07 change -2.176e+04MxComputeSimAnnealing(tsallis1996) evaluations 12476 fit -4223.41 change -445.3    MxComputeSimAnnealing(tsallis1996) evaluations 12724 fit -6357.86 change -1778 MxComputeSimAnnealing(tsallis1996) evaluations 12992 fit -7741.8 change -6176 MxComputeSimAnnealing(tsallis1996) evaluations 13264 fit -7716.93 change -6151MxComputeSimAnnealing(tsallis1996) evaluations 13530 fit -5493.28 change -554.8MxComputeSimAnnealing(tsallis1996) evaluations 13795 fit -7750.6 change -1.562e+04MxComputeSimAnnealing(tsallis1996) evaluations 14061 fit -7707.5 change -1417     MxComputeSimAnnealing(tsallis1996) evaluations 14326 fit -7571.89 change -3935MxComputeSimAnnealing(tsallis1996) evaluations 14596 fit -7642.27 change -1008MxComputeSimAnnealing(tsallis1996) evaluations 14872 fit 14425.4 change 2.216e+04MxComputeSimAnnealing(tsallis1996) evaluations 15170 fit 14405.7 change 1.65e+04 MxComputeSimAnnealing(tsallis1996) evaluations 15494 fit -4054.43 change 419.6  MxComputeSimAnnealing(tsallis1996) evaluations 15801 fit -7722.75 change -1.074e+04MxComputeSimAnnealing(tsallis1996) evaluations 16108 fit -7747.79 change -2.209e+04MxComputeSimAnnealing(tsallis1996) evaluations 16405 fit -7750.84 change 0.04195                                                                                   
#> MxComputeSimAnnealing(tsallis1996) evaluations 175 fit -6024.25 change 0MxComputeSimAnnealing(tsallis1996) evaluations 359 fit -6193.33 change 0MxComputeSimAnnealing(tsallis1996) evaluations 541 fit 586.668 change -4978MxComputeSimAnnealing(tsallis1996) evaluations 716 fit -6193.33 change 0   MxComputeSimAnnealing(tsallis1996) evaluations 897 fit 16651.2 change 8362MxComputeSimAnnealing(tsallis1996) evaluations 1076 fit -6024.25 change 0 MxComputeSimAnnealing(tsallis1996) evaluations 1255 fit 586.668 change 1008MxComputeSimAnnealing(tsallis1996) evaluations 1433 fit -6024.25 change 0  MxComputeSimAnnealing(tsallis1996) evaluations 1613 fit 586.668 change 0 MxComputeSimAnnealing(tsallis1996) evaluations 1795 fit 7830.05 change -4736MxComputeSimAnnealing(tsallis1996) evaluations 1978 fit -4575.24 change 1449MxComputeSimAnnealing(tsallis1996) evaluations 2157 fit 586.668 change 0    MxComputeSimAnnealing(tsallis1996) evaluations 2336 fit 553.299 change 7306MxComputeSimAnnealing(tsallis1996) evaluations 2518 fit -6193.33 change 0  MxComputeSimAnnealing(tsallis1996) evaluations 2688 fit -6193.33 change 0MxComputeSimAnnealing(tsallis1996) evaluations 2869 fit 2755.1 change -2335MxComputeSimAnnealing(tsallis1996) evaluations 3053 fit -3506.1 change -7899MxComputeSimAnnealing(tsallis1996) evaluations 3235 fit -6743.53 change -719.3MxComputeSimAnnealing(tsallis1996) evaluations 3414 fit 586.668 change -3194  MxComputeSimAnnealing(tsallis1996) evaluations 3594 fit -8239.23 change -2047MxComputeSimAnnealing(tsallis1996) evaluations 3775 fit -6193.33 change 0    MxComputeSimAnnealing(tsallis1996) evaluations 3954 fit -948.09 change 1399MxComputeSimAnnealing(tsallis1996) evaluations 4138 fit -8103.27 change -610MxComputeSimAnnealing(tsallis1996) evaluations 4320 fit -6193.33 change 0   MxComputeSimAnnealing(tsallis1996) evaluations 4481 fit -3291.25 change -7107MxComputeSimAnnealing(tsallis1996) evaluations 4660 fit -6193.33 change 599.2MxComputeSimAnnealing(tsallis1996) evaluations 4841 fit 3568.58 change -1.102e+04MxComputeSimAnnealing(tsallis1996) evaluations 5019 fit -6024.25 change 0        MxComputeSimAnnealing(tsallis1996) evaluations 5200 fit 586.668 change 0 MxComputeSimAnnealing(tsallis1996) evaluations 5379 fit -7554.75 change 588.4MxComputeSimAnnealing(tsallis1996) evaluations 5558 fit 1061.87 change -2.419e-07MxComputeSimAnnealing(tsallis1996) evaluations 5743 fit -8246.44 change -2948    MxComputeSimAnnealing(tsallis1996) evaluations 5929 fit -1373.44 change -2287MxComputeSimAnnealing(tsallis1996) evaluations 6117 fit 2048.46 change 1633  MxComputeSimAnnealing(tsallis1996) evaluations 6318 fit -2614.25 change 3302MxComputeSimAnnealing(tsallis1996) evaluations 6530 fit -5766.39 change 1262MxComputeSimAnnealing(tsallis1996) evaluations 6739 fit -8013.33 change -2182MxComputeSimAnnealing(tsallis1996) evaluations 6955 fit -5766.39 change 222.2MxComputeSimAnnealing(tsallis1996) evaluations 7163 fit -8288.04 change -1849MxComputeSimAnnealing(tsallis1996) evaluations 7368 fit -8237.39 change -8313MxComputeSimAnnealing(tsallis1996) evaluations 7575 fit -2199.48 change -3051MxComputeSimAnnealing(tsallis1996) evaluations 7786 fit -5749.53 change -1.14e+04MxComputeSimAnnealing(tsallis1996) evaluations 7996 fit -8256.93 change -1286    MxComputeSimAnnealing(tsallis1996) evaluations 8206 fit -6986.98 change -5564MxComputeSimAnnealing(tsallis1996) evaluations 8407 fit 4857.88 change 1.06e+04MxComputeSimAnnealing(tsallis1996) evaluations 8614 fit -3730.12 change -2138  MxComputeSimAnnealing(tsallis1996) evaluations 8823 fit -5591.85 change -1.124e+04MxComputeSimAnnealing(tsallis1996) evaluations 9033 fit -8323.22 change -1345     MxComputeSimAnnealing(tsallis1996) evaluations 9241 fit -3172.18 change 5046 MxComputeSimAnnealing(tsallis1996) evaluations 9449 fit -5248.21 change 1589MxComputeSimAnnealing(tsallis1996) evaluations 9661 fit -8264.42 change -1309MxComputeSimAnnealing(tsallis1996) evaluations 9870 fit -3713.25 change -93.1MxComputeSimAnnealing(tsallis1996) evaluations 10081 fit -5667.13 change -3859MxComputeSimAnnealing(tsallis1996) evaluations 10289 fit -8283.68 change 43.04MxComputeSimAnnealing(tsallis1996) evaluations 10490 fit -5685.64 change 2645 MxComputeSimAnnealing(tsallis1996) evaluations 10698 fit -6961.09 change 1054MxComputeSimAnnealing(tsallis1996) evaluations 10907 fit -4425.27 change 3695MxComputeSimAnnealing(tsallis1996) evaluations 11116 fit 4951.52 change 1.329e+04MxComputeSimAnnealing(tsallis1996) evaluations 11321 fit 5116.26 change -3.251   MxComputeSimAnnealing(tsallis1996) evaluations 11525 fit -8144.66 change -1.333e+04MxComputeSimAnnealing(tsallis1996) evaluations 11734 fit -8302.31 change -1259     MxComputeSimAnnealing(tsallis1996) evaluations 11941 fit -8247.67 change -712.3MxComputeSimAnnealing(tsallis1996) evaluations 12148 fit -8334.49 change -224.3MxComputeSimAnnealing(tsallis1996) evaluations 12355 fit -8321.67 change -62.03MxComputeSimAnnealing(tsallis1996) evaluations 12562 fit -7023.51 change 1318  MxComputeSimAnnealing(tsallis1996) evaluations 12768 fit -5648.9 change 0    MxComputeSimAnnealing(tsallis1996) evaluations 12976 fit -8339.61 change -1321MxComputeSimAnnealing(tsallis1996) evaluations 13182 fit -8344.08 change -38.22MxComputeSimAnnealing(tsallis1996) evaluations 13389 fit -2685.32 change 5649  MxComputeSimAnnealing(tsallis1996) evaluations 13598 fit 5037.24 change 1.335e+04MxComputeSimAnnealing(tsallis1996) evaluations 13805 fit -8341.98 change -2773   MxComputeSimAnnealing(tsallis1996) evaluations 14014 fit -8280.68 change -1243MxComputeSimAnnealing(tsallis1996) evaluations 14222 fit 2860.02 change 1.064e+04MxComputeSimAnnealing(tsallis1996) evaluations 14432 fit 5260.68 change 0        MxComputeSimAnnealing(tsallis1996) evaluations 14638 fit -8345.82 change -0.2209MxComputeSimAnnealing(tsallis1996) evaluations 14845 fit -7083.51 change 1245   MxComputeSimAnnealing(tsallis1996) evaluations 15051 fit -3606.36 change 3428MxComputeSimAnnealing(tsallis1996) evaluations 15258 fit -5588.8 change -897 MxComputeSimAnnealing(tsallis1996) evaluations 15464 fit -8256.38 change -1648MxComputeSimAnnealing(tsallis1996) evaluations 15672 fit 5045.96 change 1.133e+04MxComputeSimAnnealing(tsallis1996) evaluations 15881 fit -7051.68 change 1292    MxComputeSimAnnealing(tsallis1996) evaluations 16088 fit -8342.5 change -1291MxComputeSimAnnealing(tsallis1996) evaluations 16295 fit -4501.45 change 3423MxComputeSimAnnealing(tsallis1996) evaluations 16503 fit -6125.04 change 1893MxComputeSimAnnealing(tsallis1996) evaluations 16708 fit -7699.34 change -4017MxComputeSimAnnealing(tsallis1996) evaluations 16915 fit -8345.47 change -1.369e+04MxComputeSimAnnealing(tsallis1996) evaluations 17121 fit -5561.58 change 1103      MxComputeSimAnnealing(tsallis1996) evaluations 17328 fit -7060.13 change 1214MxComputeSimAnnealing(tsallis1996) evaluations 17537 fit -8344.58 change -4362MxComputeSimAnnealing(tsallis1996) evaluations 17744 fit -2657.69 change 3552 MxComputeSimAnnealing(tsallis1996) evaluations 17953 fit -8346.17 change -31.36MxComputeSimAnnealing(tsallis1996) evaluations 18160 fit -7052.42 change 1287  MxComputeSimAnnealing(tsallis1996) evaluations 18365 fit -7174.15 change 1173MxComputeSimAnnealing(tsallis1996) evaluations 18573 fit -5824.32 change 2342MxComputeSimAnnealing(tsallis1996) evaluations 18780 fit -8329.23 change -41.88MxComputeSimAnnealing(tsallis1996) evaluations 18987 fit 5148.72 change 1.349e+04MxComputeSimAnnealing(tsallis1996) evaluations 19186 fit -45.1121 change 7388    MxComputeSimAnnealing(tsallis1996) evaluations 19383 fit -7046.45 change -1460MxComputeSimAnnealing(tsallis1996) evaluations 19573 fit -8224.59 change -1172MxComputeSimAnnealing(tsallis1996) evaluations 19749 fit -8100.77 change -4729MxComputeSimAnnealing(tsallis1996) evaluations 19923 fit 5257.41 change 1.356e+04MxComputeSimAnnealing(tsallis1996) evaluations 20099 fit -8340.83 change -1285   MxComputeSimAnnealing(tsallis1996) evaluations 20275 fit -8339.91 change 7.049MxComputeSimAnnealing(tsallis1996) evaluations 20452 fit -7818.91 change -2247MxComputeSimAnnealing(tsallis1996) evaluations 20627 fit -8346.82 change -1290MxComputeSimAnnealing(tsallis1996) evaluations 20793 fit -8346.54 change -2767MxComputeSimAnnealing(tsallis1996) evaluations 20968 fit -6002.98 change 2344 MxComputeSimAnnealing(tsallis1996) evaluations 21150 fit -7814.55 change -2245MxComputeSimAnnealing(tsallis1996) evaluations 21324 fit -8346.76 change -117.5MxComputeSimAnnealing(tsallis1996) evaluations 21503 fit 5288.76 change 0      MxComputeSimAnnealing(tsallis1996) evaluations 21682 fit -6642.27 change 1624                                                                             
#> MxComputeSimAnnealing(tsallis1996) evaluations 127 fit -7023.02 change 419.2MxComputeSimAnnealing(tsallis1996) evaluations 263 fit 12976.1 change 2306  MxComputeSimAnnealing(tsallis1996) evaluations 401 fit -7023.02 change 0  MxComputeSimAnnealing(tsallis1996) evaluations 539 fit 7739.7 change 2413MxComputeSimAnnealing(tsallis1996) evaluations 673 fit -7023.02 change 419.2MxComputeSimAnnealing(tsallis1996) evaluations 811 fit 3953.39 change 674.8 MxComputeSimAnnealing(tsallis1996) evaluations 945 fit -7442.24 change 0   MxComputeSimAnnealing(tsallis1996) evaluations 1080 fit 1136.84 change -8466MxComputeSimAnnealing(tsallis1996) evaluations 1216 fit -7442.24 change -3654MxComputeSimAnnealing(tsallis1996) evaluations 1342 fit -7442.24 change -3654MxComputeSimAnnealing(tsallis1996) evaluations 1478 fit 7623.72 change 1.332e+04MxComputeSimAnnealing(tsallis1996) evaluations 1612 fit -3788.3 change -8365    MxComputeSimAnnealing(tsallis1996) evaluations 1740 fit -3788.3 change 0    MxComputeSimAnnealing(tsallis1996) evaluations 1877 fit 2840.43 change 7544MxComputeSimAnnealing(tsallis1996) evaluations 2014 fit -7442.24 change -3654MxComputeSimAnnealing(tsallis1996) evaluations 2148 fit -7420.42 change 0    MxComputeSimAnnealing(tsallis1996) evaluations 2282 fit 889.307 change -2601MxComputeSimAnnealing(tsallis1996) evaluations 2416 fit -7023.02 change 419.2MxComputeSimAnnealing(tsallis1996) evaluations 2549 fit -174.13 change 3022  MxComputeSimAnnealing(tsallis1996) evaluations 2681 fit -181.358 change -1.183e+04MxComputeSimAnnealing(tsallis1996) evaluations 2830 fit -3788.3 change -5309      MxComputeSimAnnealing(tsallis1996) evaluations 2978 fit -3788.3 change 0    MxComputeSimAnnealing(tsallis1996) evaluations 3131 fit -7023.02 change 0MxComputeSimAnnealing(tsallis1996) evaluations 3286 fit 1110.56 change 2404MxComputeSimAnnealing(tsallis1996) evaluations 3437 fit 9870.53 change 1.188e+04MxComputeSimAnnealing(tsallis1996) evaluations 3703 fit -6080.6 change 1340     MxComputeSimAnnealing(tsallis1996) evaluations 3969 fit -7442.24 change 0  MxComputeSimAnnealing(tsallis1996) evaluations 4236 fit 12252.5 change 9756MxComputeSimAnnealing(tsallis1996) evaluations 4499 fit -7420.42 change 0  MxComputeSimAnnealing(tsallis1996) evaluations 4761 fit -1934.33 change -3339MxComputeSimAnnealing(tsallis1996) evaluations 5026 fit -4338.67 change 3084 MxComputeSimAnnealing(tsallis1996) evaluations 5289 fit -3741.74 change 0   MxComputeSimAnnealing(tsallis1996) evaluations 5552 fit -1465.64 change 6915MxComputeSimAnnealing(tsallis1996) evaluations 5816 fit -7466.21 change 0   MxComputeSimAnnealing(tsallis1996) evaluations 6083 fit 7381.67 change 1.235e+04MxComputeSimAnnealing(tsallis1996) evaluations 6344 fit -7362.41 change 1050    MxComputeSimAnnealing(tsallis1996) evaluations 6612 fit -3490.04 change 0   MxComputeSimAnnealing(tsallis1996) evaluations 6876 fit -7468.37 change -3013MxComputeSimAnnealing(tsallis1996) evaluations 7141 fit -7076.32 change 389.9MxComputeSimAnnealing(tsallis1996) evaluations 7408 fit -3490.3 change 278.5 MxComputeSimAnnealing(tsallis1996) evaluations 7672 fit -4718.13 change 2704MxComputeSimAnnealing(tsallis1996) evaluations 7936 fit -7458.97 change -3968MxComputeSimAnnealing(tsallis1996) evaluations 8201 fit -3197.56 change 5120 MxComputeSimAnnealing(tsallis1996) evaluations 8465 fit -7045.29 change 0.0001411MxComputeSimAnnealing(tsallis1996) evaluations 8732 fit -2766.2 change 22.47     MxComputeSimAnnealing(tsallis1996) evaluations 8996 fit -8338.9 change 139.6MxComputeSimAnnealing(tsallis1996) evaluations 9248 fit -3179.65 change 4037MxComputeSimAnnealing(tsallis1996) evaluations 9509 fit -2696.18 change 510.9MxComputeSimAnnealing(tsallis1996) evaluations 9770 fit -7539.57 change 0    MxComputeSimAnnealing(tsallis1996) evaluations 10029 fit -5183.9 change 2189MxComputeSimAnnealing(tsallis1996) evaluations 10289 fit -8388.72 change -812.8MxComputeSimAnnealing(tsallis1996) evaluations 10552 fit -8503.78 change -103.9MxComputeSimAnnealing(tsallis1996) evaluations 10812 fit -2206.76 change 6297  MxComputeSimAnnealing(tsallis1996) evaluations 11076 fit 957.97 change 6210  MxComputeSimAnnealing(tsallis1996) evaluations 11340 fit -8020.59 change -518.6MxComputeSimAnnealing(tsallis1996) evaluations 11596 fit -7791.73 change -437.9MxComputeSimAnnealing(tsallis1996) evaluations 11858 fit 8055.8 change 1.376e+04MxComputeSimAnnealing(tsallis1996) evaluations 12121 fit -8367.98 change 142    MxComputeSimAnnealing(tsallis1996) evaluations 12385 fit -1520.83 change -57.29MxComputeSimAnnealing(tsallis1996) evaluations 12645 fit -8262.86 change -933.7MxComputeSimAnnealing(tsallis1996) evaluations 12901 fit -5109.84 change 2623  MxComputeSimAnnealing(tsallis1996) evaluations 13160 fit 18.3874 change 8305 MxComputeSimAnnealing(tsallis1996) evaluations 13419 fit -8523.45 change -57.21MxComputeSimAnnealing(tsallis1996) evaluations 13675 fit -7806.01 change 723.9 MxComputeSimAnnealing(tsallis1996) evaluations 13935 fit -7619.43 change -5116MxComputeSimAnnealing(tsallis1996) evaluations 14195 fit -7464.42 change 276  MxComputeSimAnnealing(tsallis1996) evaluations 14457 fit -7178.14 change 1358MxComputeSimAnnealing(tsallis1996) evaluations 14719 fit -7423.37 change -6190MxComputeSimAnnealing(tsallis1996) evaluations 14982 fit -1394.91 change 7147 MxComputeSimAnnealing(tsallis1996) evaluations 15245 fit -7607.39 change 918.8MxComputeSimAnnealing(tsallis1996) evaluations 15507 fit -7570.15 change 939.4MxComputeSimAnnealing(tsallis1996) evaluations 15769 fit -8523.38 change -7998MxComputeSimAnnealing(tsallis1996) evaluations 16032 fit -7960.4 change -1261 MxComputeSimAnnealing(tsallis1996) evaluations 16294 fit -7532.05 change -7064MxComputeSimAnnealing(tsallis1996) evaluations 16555 fit -6964.03 change 923.4MxComputeSimAnnealing(tsallis1996) evaluations 16820 fit -8539.78 change -773.5MxComputeSimAnnealing(tsallis1996) evaluations 17086 fit -8377.61 change 124.7 MxComputeSimAnnealing(tsallis1996) evaluations 17349 fit -8103.03 change -636.5MxComputeSimAnnealing(tsallis1996) evaluations 17612 fit -5246.7 change 2349   MxComputeSimAnnealing(tsallis1996) evaluations 17874 fit -8018.72 change 532.9MxComputeSimAnnealing(tsallis1996) evaluations 18135 fit -6082.45 change -834.3MxComputeSimAnnealing(tsallis1996) evaluations 18399 fit -7434.61 change 1002  MxComputeSimAnnealing(tsallis1996) evaluations 18660 fit -8518.17 change 19.54MxComputeSimAnnealing(tsallis1996) evaluations 18923 fit -7454.17 change 1050 MxComputeSimAnnealing(tsallis1996) evaluations 19186 fit -8240.52 change 55.19MxComputeSimAnnealing(tsallis1996) evaluations 19449 fit -7429.76 change 0    MxComputeSimAnnealing(tsallis1996) evaluations 19713 fit -8374.94 change 152.4MxComputeSimAnnealing(tsallis1996) evaluations 19974 fit -7453.48 change 0    MxComputeSimAnnealing(tsallis1996) evaluations 20237 fit -7781.34 change -2675MxComputeSimAnnealing(tsallis1996) evaluations 20498 fit -7483.26 change 984  MxComputeSimAnnealing(tsallis1996) evaluations 20760 fit -8324.71 change 149.1MxComputeSimAnnealing(tsallis1996) evaluations 21022 fit -8554.85 change -0.8245MxComputeSimAnnealing(tsallis1996) evaluations 21286 fit -7033.89 change 283.8  MxComputeSimAnnealing(tsallis1996) evaluations 21549 fit -7476.76 change 1066 MxComputeSimAnnealing(tsallis1996) evaluations 21813 fit -7005.81 change -448.4MxComputeSimAnnealing(tsallis1996) evaluations 22075 fit -7882.34 change 570.3 MxComputeSimAnnealing(tsallis1996) evaluations 22337 fit -8535.18 change -361.3MxComputeSimAnnealing(tsallis1996) evaluations 22599 fit -8532.46 change -518.5MxComputeSimAnnealing(tsallis1996) evaluations 22863 fit -4660.12 change 3872  MxComputeSimAnnealing(tsallis1996) evaluations 23125 fit -8555.64 change -1074MxComputeSimAnnealing(tsallis1996) evaluations 23389 fit -379.869 change 746.1MxComputeSimAnnealing(tsallis1996) evaluations 23649 fit -7867.78 change -155.9MxComputeSimAnnealing(tsallis1996) evaluations 23913 fit -2553.79 change 5206  MxComputeSimAnnealing(tsallis1996) evaluations 24172 fit -7601.91 change 917.3MxComputeSimAnnealing(tsallis1996) evaluations 24435 fit -7636.68 change 916.8MxComputeSimAnnealing(tsallis1996) evaluations 24697 fit -8555.63 change -3.805MxComputeSimAnnealing(tsallis1996) evaluations 24958 fit -8448.89 change 103.7 MxComputeSimAnnealing(tsallis1996) evaluations 25222 fit -8549.57 change -1125MxComputeSimAnnealing(tsallis1996) evaluations 25484 fit -6753.94 change -7733MxComputeSimAnnealing(tsallis1996) evaluations 25744 fit -8552.24 change -1.253MxComputeSimAnnealing(tsallis1996) evaluations 26005 fit -8549.59 change -660.9MxComputeSimAnnealing(tsallis1996) evaluations 26264 fit -8271.81 change -2652 MxComputeSimAnnealing(tsallis1996) evaluations 26525 fit -8145.11 change -652.4MxComputeSimAnnealing(tsallis1996) evaluations 26788 fit -8528.57 change -1810                                                                               
#> MxComputeSimAnnealing(tsallis1996) evaluations 72 fit -7825.65 change 0MxComputeSimAnnealing(tsallis1996) evaluations 282 fit -7367.16 change 0MxComputeSimAnnealing(tsallis1996) evaluations 494 fit -7295.31 change 0MxComputeSimAnnealing(tsallis1996) evaluations 704 fit -7027.27 change 0MxComputeSimAnnealing(tsallis1996) evaluations 914 fit 21750.1 change 1.356e+04MxComputeSimAnnealing(tsallis1996) evaluations 1123 fit -7825.65 change 0      MxComputeSimAnnealing(tsallis1996) evaluations 1334 fit -3210.18 change 262.7MxComputeSimAnnealing(tsallis1996) evaluations 1544 fit -7295.31 change 0    MxComputeSimAnnealing(tsallis1996) evaluations 1754 fit -7027.27 change 0MxComputeSimAnnealing(tsallis1996) evaluations 1966 fit 164.466 change -5961MxComputeSimAnnealing(tsallis1996) evaluations 2176 fit -7465.85 change 0   MxComputeSimAnnealing(tsallis1996) evaluations 2385 fit 2905.45 change 7256MxComputeSimAnnealing(tsallis1996) evaluations 2596 fit -7825.65 change -530.3MxComputeSimAnnealing(tsallis1996) evaluations 2806 fit -7367.16 change 0     MxComputeSimAnnealing(tsallis1996) evaluations 3015 fit -423.135 change -7395MxComputeSimAnnealing(tsallis1996) evaluations 3224 fit -7465.85 change 359.8MxComputeSimAnnealing(tsallis1996) evaluations 3433 fit -2949.42 change 4418 MxComputeSimAnnealing(tsallis1996) evaluations 3644 fit -7298.48 change -3.172MxComputeSimAnnealing(tsallis1996) evaluations 3856 fit -7367.16 change 0     MxComputeSimAnnealing(tsallis1996) evaluations 4069 fit -7295.31 change 0MxComputeSimAnnealing(tsallis1996) evaluations 4279 fit -7027.27 change 0MxComputeSimAnnealing(tsallis1996) evaluations 4490 fit -137.433 change -2591MxComputeSimAnnealing(tsallis1996) evaluations 4699 fit -7465.85 change 359.8MxComputeSimAnnealing(tsallis1996) evaluations 4909 fit -5819.03 change 2089 MxComputeSimAnnealing(tsallis1996) evaluations 5121 fit -7825.65 change -530.3MxComputeSimAnnealing(tsallis1996) evaluations 5332 fit -7367.16 change 0     MxComputeSimAnnealing(tsallis1996) evaluations 5544 fit -7295.31 change 0MxComputeSimAnnealing(tsallis1996) evaluations 5753 fit -7027.27 change 0MxComputeSimAnnealing(tsallis1996) evaluations 5964 fit 6773.32 change 1.313e+04MxComputeSimAnnealing(tsallis1996) evaluations 6174 fit -7465.85 change 359.8   MxComputeSimAnnealing(tsallis1996) evaluations 6385 fit -1975.74 change 1312 MxComputeSimAnnealing(tsallis1996) evaluations 6595 fit -7295.31 change 0   MxComputeSimAnnealing(tsallis1996) evaluations 6805 fit -7367.16 change -339.9MxComputeSimAnnealing(tsallis1996) evaluations 7018 fit -7295.4 change -7954  MxComputeSimAnnealing(tsallis1996) evaluations 7229 fit -7225.16 change -209.4MxComputeSimAnnealing(tsallis1996) evaluations 7440 fit -8258.8 change -1.195e+04MxComputeSimAnnealing(tsallis1996) evaluations 7650 fit -8046.56 change -566.6   MxComputeSimAnnealing(tsallis1996) evaluations 7861 fit -2554.83 change 4015  MxComputeSimAnnealing(tsallis1996) evaluations 8072 fit -7791.96 change 0   MxComputeSimAnnealing(tsallis1996) evaluations 8281 fit -7375.84 change 0MxComputeSimAnnealing(tsallis1996) evaluations 8493 fit -7332.18 change -122.1MxComputeSimAnnealing(tsallis1996) evaluations 8703 fit -7004.29 change 0     MxComputeSimAnnealing(tsallis1996) evaluations 8917 fit -7348.25 change -8579MxComputeSimAnnealing(tsallis1996) evaluations 9124 fit -7490.08 change 302  MxComputeSimAnnealing(tsallis1996) evaluations 9333 fit -6570.2 change 820.3MxComputeSimAnnealing(tsallis1996) evaluations 9545 fit -7332.44 change 0   MxComputeSimAnnealing(tsallis1996) evaluations 9754 fit -7010.84 change 0MxComputeSimAnnealing(tsallis1996) evaluations 9968 fit -7332.94 change 1033MxComputeSimAnnealing(tsallis1996) evaluations 10175 fit -8440.84 change -886MxComputeSimAnnealing(tsallis1996) evaluations 10385 fit -6677.25 change 1596MxComputeSimAnnealing(tsallis1996) evaluations 10597 fit -7778.91 change 0   MxComputeSimAnnealing(tsallis1996) evaluations 10807 fit -7955.17 change -332.8MxComputeSimAnnealing(tsallis1996) evaluations 11018 fit -7316.11 change -7018 MxComputeSimAnnealing(tsallis1996) evaluations 11227 fit -7113.59 change 381.2MxComputeSimAnnealing(tsallis1996) evaluations 11437 fit 449.559 change -4445 MxComputeSimAnnealing(tsallis1996) evaluations 11647 fit -7776.45 change 0   MxComputeSimAnnealing(tsallis1996) evaluations 11857 fit -7497.63 change 912.8MxComputeSimAnnealing(tsallis1996) evaluations 12069 fit -7340.84 change 0    MxComputeSimAnnealing(tsallis1996) evaluations 12278 fit -7001.92 change 276.6MxComputeSimAnnealing(tsallis1996) evaluations 12489 fit -8275.3 change 162.7 MxComputeSimAnnealing(tsallis1996) evaluations 12698 fit -7791.49 change 373.6MxComputeSimAnnealing(tsallis1996) evaluations 12909 fit -8449.1 change -49.04MxComputeSimAnnealing(tsallis1996) evaluations 13119 fit -7316.53 change -0.02549MxComputeSimAnnealing(tsallis1996) evaluations 13330 fit -7510.76 change -457.9  MxComputeSimAnnealing(tsallis1996) evaluations 13539 fit -2839.05 change 1869  MxComputeSimAnnealing(tsallis1996) evaluations 13749 fit -7491.91 change 925 MxComputeSimAnnealing(tsallis1996) evaluations 13959 fit -7480.15 change 962.6MxComputeSimAnnealing(tsallis1996) evaluations 14171 fit -7839.06 change -575.7MxComputeSimAnnealing(tsallis1996) evaluations 14380 fit -7510.53 change -469.1MxComputeSimAnnealing(tsallis1996) evaluations 14590 fit -4607.23 change -2052 MxComputeSimAnnealing(tsallis1996) evaluations 14798 fit -7841.09 change -0.001076MxComputeSimAnnealing(tsallis1996) evaluations 15006 fit -8402.22 change -453.3   MxComputeSimAnnealing(tsallis1996) evaluations 15217 fit -4347.14 change 2174  MxComputeSimAnnealing(tsallis1996) evaluations 15425 fit -7631.04 change 0   MxComputeSimAnnealing(tsallis1996) evaluations 15635 fit -8507.45 change -100.8MxComputeSimAnnealing(tsallis1996) evaluations 15844 fit -7226.33 change 911.1 MxComputeSimAnnealing(tsallis1996) evaluations 16055 fit -7680.84 change -428.3MxComputeSimAnnealing(tsallis1996) evaluations 16265 fit -7796.1 change -1255  MxComputeSimAnnealing(tsallis1996) evaluations 16475 fit -7549.67 change 102.9MxComputeSimAnnealing(tsallis1996) evaluations 16684 fit -7237.17 change 1289 MxComputeSimAnnealing(tsallis1996) evaluations 16895 fit -7220.65 change 369.5MxComputeSimAnnealing(tsallis1996) evaluations 17105 fit -7812.95 change -346.5MxComputeSimAnnealing(tsallis1996) evaluations 17314 fit 790.05 change 8919    MxComputeSimAnnealing(tsallis1996) evaluations 17523 fit -8537.23 change -0.3071MxComputeSimAnnealing(tsallis1996) evaluations 17732 fit -8543.36 change -685.7 MxComputeSimAnnealing(tsallis1996) evaluations 17943 fit -7199.33 change -2126 MxComputeSimAnnealing(tsallis1996) evaluations 18151 fit -8544.55 change 0.1175MxComputeSimAnnealing(tsallis1996) evaluations 18360 fit -8529.73 change -2171 MxComputeSimAnnealing(tsallis1996) evaluations 18570 fit -8547.46 change -1364MxComputeSimAnnealing(tsallis1996) evaluations 18780 fit -8548.86 change -21.64MxComputeSimAnnealing(tsallis1996) evaluations 18990 fit -8476.83 change -6829 MxComputeSimAnnealing(tsallis1996) evaluations 19200 fit -7646.22 change -101.2MxComputeSimAnnealing(tsallis1996) evaluations 19410 fit -8458.88 change -4556 MxComputeSimAnnealing(tsallis1996) evaluations 19620 fit -7444.31 change 1022 MxComputeSimAnnealing(tsallis1996) evaluations 19829 fit -8452.09 change 53.92MxComputeSimAnnealing(tsallis1996) evaluations 20039 fit -5542.66 change 3013 MxComputeSimAnnealing(tsallis1996) evaluations 20246 fit -8001.77 change -846.9MxComputeSimAnnealing(tsallis1996) evaluations 20456 fit -7865.19 change 0     MxComputeSimAnnealing(tsallis1996) evaluations 20664 fit -8496.58 change -2859MxComputeSimAnnealing(tsallis1996) evaluations 20874 fit -7585.19 change 387.8MxComputeSimAnnealing(tsallis1996) evaluations 21082 fit -7872.01 change 55.34MxComputeSimAnnealing(tsallis1996) evaluations 21293 fit -7202.03 change 1357 MxComputeSimAnnealing(tsallis1996) evaluations 21502 fit -8500.14 change -979.7MxComputeSimAnnealing(tsallis1996) evaluations 21712 fit 437.521 change 8996   MxComputeSimAnnealing(tsallis1996) evaluations 21922 fit -8432.04 change 128.8MxComputeSimAnnealing(tsallis1996) evaluations 22131 fit -8317.26 change 244  MxComputeSimAnnealing(tsallis1996) evaluations 22339 fit 12578.8 change 1.875e+04MxComputeSimAnnealing(tsallis1996) evaluations 22546 fit -8024.97 change 467.9   MxComputeSimAnnealing(tsallis1996) evaluations 22754 fit -7544.49 change -34.24MxComputeSimAnnealing(tsallis1996) evaluations 22961 fit 9071.1 change 1.763e+04MxComputeSimAnnealing(tsallis1996) evaluations 23171 fit -8560.05 change -1340  MxComputeSimAnnealing(tsallis1996) evaluations 23380 fit -7886.26 change 676.5MxComputeSimAnnealing(tsallis1996) evaluations 23588 fit 1716.67 change 7376  MxComputeSimAnnealing(tsallis1996) evaluations 23795 fit -8536.58 change -48.31MxComputeSimAnnealing(tsallis1996) evaluations 24004 fit -7514.75 change 1039  MxComputeSimAnnealing(tsallis1996) evaluations 24213 fit -3972.96 change 2787MxComputeSimAnnealing(tsallis1996) evaluations 24420 fit -8563.4 change -1309MxComputeSimAnnealing(tsallis1996) evaluations 24628 fit -7518.25 change 0.0006365MxComputeSimAnnealing(tsallis1996) evaluations 24835 fit -8557.02 change 6.548    MxComputeSimAnnealing(tsallis1996) evaluations 25044 fit -7500.65 change 257.7MxComputeSimAnnealing(tsallis1996) evaluations 25250 fit -8471.18 change 62.16MxComputeSimAnnealing(tsallis1996) evaluations 25457 fit -8562.82 change -668.4MxComputeSimAnnealing(tsallis1996) evaluations 25667 fit -8540.36 change -812.7MxComputeSimAnnealing(tsallis1996) evaluations 25874 fit -7554.9 change 656.3  MxComputeSimAnnealing(tsallis1996) evaluations 26082 fit -7884.41 change 671.6MxComputeSimAnnealing(tsallis1996) evaluations 26290 fit -7226.98 change 1335 MxComputeSimAnnealing(tsallis1996) evaluations 26497 fit -8558.57 change 3.55MxComputeSimAnnealing(tsallis1996) evaluations 26705 fit -7896.71 change -379.9MxComputeSimAnnealing(tsallis1996) evaluations 26914 fit -8563.26 change -119.8MxComputeSimAnnealing(tsallis1996) evaluations 27121 fit -7811.89 change -484.1MxComputeSimAnnealing(tsallis1996) evaluations 27329 fit -7527.15 change 490.1 MxComputeSimAnnealing(tsallis1996) evaluations 27536 fit -8555.28 change -193.3MxComputeSimAnnealing(tsallis1996) evaluations 27744 fit -7375.62 change 1051  MxComputeSimAnnealing(tsallis1996) evaluations 27952 fit -8564.86 change -299MxComputeSimAnnealing(tsallis1996) evaluations 28159 fit -8555.73 change -16.88MxComputeSimAnnealing(tsallis1996) evaluations 28367 fit -8046.48 change 519   MxComputeSimAnnealing(tsallis1996) evaluations 28574 fit -8565.56 change -57.62MxComputeSimAnnealing(tsallis1996) evaluations 28781 fit -8565.86 change -94.44MxComputeSimAnnealing(tsallis1996) evaluations 28989 fit -8565.93 change -2493 MxComputeSimAnnealing(tsallis1996) evaluations 29196 fit -7705.08 change 520.8MxComputeSimAnnealing(tsallis1996) evaluations 29404 fit -7532.04 change 0    MxComputeSimAnnealing(tsallis1996) evaluations 29609 fit -8566.44 change -2.543MxComputeSimAnnealing(tsallis1996) evaluations 29817 fit -8566.5 change -93.54 MxComputeSimAnnealing(tsallis1996) evaluations 30026 fit -8566.63 change -728.1MxComputeSimAnnealing(tsallis1996) evaluations 30234 fit -8566.9 change -3.544 MxComputeSimAnnealing(tsallis1996) evaluations 30443 fit -8534.89 change -8436MxComputeSimAnnealing(tsallis1996) evaluations 30650 fit -8548.77 change -901.2MxComputeSimAnnealing(tsallis1996) evaluations 30858 fit -8567.78 change -686.4MxComputeSimAnnealing(tsallis1996) evaluations 31068 fit -8486.64 change -1.86e+04MxComputeSimAnnealing(tsallis1996) evaluations 31274 fit -8568.27 change -1096    MxComputeSimAnnealing(tsallis1996) evaluations 31482 fit -7890.52 change 656.8MxComputeSimAnnealing(tsallis1996) evaluations 31692 fit -8406.05 change 163.6MxComputeSimAnnealing(tsallis1996) evaluations 31899 fit -8569.92 change -1.381                                                                               
#> Beginning initial fit attemptFit attempt 0, fit=-5186.90412866336, new current best! (was -5186.90412866336)Beginning fit attempt 1 of at maximum 50 extra tries                           Beginning fit attempt 2 of at maximum 50 extra triesBeginning fit attempt 3 of at maximum 50 extra triesBeginning fit attempt 4 of at maximum 50 extra triesBeginning fit attempt 5 of at maximum 50 extra triesBeginning fit attempt 6 of at maximum 50 extra triesBeginning fit attempt 7 of at maximum 50 extra triesBeginning fit attempt 8 of at maximum 50 extra triesBeginning fit attempt 9 of at maximum 50 extra triesBeginning fit attempt 10 of at maximum 50 extra triesBeginning fit attempt 11 of at maximum 50 extra triesBeginning fit attempt 12 of at maximum 50 extra triesBeginning fit attempt 13 of at maximum 50 extra triesBeginning fit attempt 14 of at maximum 50 extra triesBeginning fit attempt 15 of at maximum 50 extra triesBeginning fit attempt 16 of at maximum 50 extra triesBeginning fit attempt 17 of at maximum 50 extra triesBeginning fit attempt 18 of at maximum 50 extra triesBeginning fit attempt 19 of at maximum 50 extra triesBeginning fit attempt 20 of at maximum 50 extra triesBeginning fit attempt 21 of at maximum 50 extra triesBeginning fit attempt 22 of at maximum 50 extra triesBeginning fit attempt 23 of at maximum 50 extra triesBeginning fit attempt 24 of at maximum 50 extra triesBeginning fit attempt 25 of at maximum 50 extra triesBeginning fit attempt 26 of at maximum 50 extra triesBeginning fit attempt 27 of at maximum 50 extra triesBeginning fit attempt 28 of at maximum 50 extra triesBeginning fit attempt 29 of at maximum 50 extra triesBeginning fit attempt 30 of at maximum 50 extra triesBeginning fit attempt 31 of at maximum 50 extra triesBeginning fit attempt 32 of at maximum 50 extra triesBeginning fit attempt 33 of at maximum 50 extra triesBeginning fit attempt 34 of at maximum 50 extra triesBeginning fit attempt 35 of at maximum 50 extra triesBeginning fit attempt 36 of at maximum 50 extra triesBeginning fit attempt 37 of at maximum 50 extra triesBeginning fit attempt 38 of at maximum 50 extra triesBeginning fit attempt 39 of at maximum 50 extra triesBeginning fit attempt 40 of at maximum 50 extra triesBeginning fit attempt 41 of at maximum 50 extra triesBeginning fit attempt 42 of at maximum 50 extra triesBeginning fit attempt 43 of at maximum 50 extra triesBeginning fit attempt 44 of at maximum 50 extra triesBeginning fit attempt 45 of at maximum 50 extra triesBeginning fit attempt 46 of at maximum 50 extra triesBeginning fit attempt 47 of at maximum 50 extra triesBeginning fit attempt 48 of at maximum 50 extra triesBeginning fit attempt 49 of at maximum 50 extra triesBeginning fit attempt 50 of at maximum 50 extra tries                                                     
```


Note that the first model showed convergence problems, throwing the error:
*The model does not satisfy the 
first-order optimality conditions to
the required accuracy, and no improved
point for the merit function could be
found during the final linesearch.*
To address this problem, we performed
additional iterations to  
find a better solution, using `OpenMx::mxTryHardWideSearch()`.
This also illustrates that `tidySEM` mixture models inherit from `OpenMx`'s `MxModel`,
and thus, different `OpenMx` functions can be used to act on models specified via `tidySEM`.

The fifth model also evidenced convergence problems, but this (as we will see) is because the solution is overfitted.

## Class enumeration

To determine the correct number of classes, we considered the following criteria:

1. We do not consider classes with, on average, fewer than 5 participants per parameter in a class due to potential local underidentification
1. Lower values for information criteria (AIC, BIC, saBIC) indicate better fit
1. Significant Lo-Mendell-Rubin LRT test indicates better fit for $k$ vs $k-1$ classes
1. We do not consider solutions with entropy < .90 because poor class separability compromises interpretability of the results
1. We do not consider solutions with minimum posterior classification probability < .90 because poor class separability compromises interpretability of the results


``` r
# Get fit table fit
tab_fit <- table_fit(res_step)
# Select columns
tab_fit[, c("Name", "Classes", "LL", "Parameters", "BIC", "Entropy",
    "prob_min", "n_min", "warning", "lmr_p")]
```


Table: Fit of LCGA models

| Name| Classes|   LL| Parameters|   BIC| Entropy| prob_min| n_min|
|----:|-------:|----:|----------:|-----:|-------:|--------:|-----:|
|    1|       1| 2593|          9| -5125|    1.00|     1.00|  1.00|
|    2|       2| 3876|         13| -7662|    0.94|     0.97|  0.24|
|    3|       3| 4174|         17| -8230|    0.93|     0.93|  0.06|
|    4|       4| 4278|         21| -8412|    0.89|     0.85|  0.04|
|    5|       5| 4315|         25| -8457|    0.86|     0.73|  0.04|



According to the Table, increasing the number of classes keeps increasing model fit according to all ICs except the BIC, which increased after 3 classes.

The first two LMR tests are significant,
indicating that a 2- and 3-class solution were a significant improvement over a 1- and 2-class solution, respectively.
However, solutions with >3 classes had entropy and minimum posterior classification probability below the pre-specified thresholds.
Models with >3 solutions also had fewer than five observations per parameter.
This suggests that the preferred model should be selected from 1-3 classes.

### Scree plot

A scree plot indicates that
the largest decrease in ICs occurs from 1-2 classes,
and the inflection point for all ICs is at 3 classes.
Moreover, the BIC increased after 3 classes.
A three-class solution thus appears to be the most parsimonious
solution with good fit.


``` r
plot(tab_fit, statistics = c("AIC", "BIC", "saBIC"))
```

<img src="lcga_plot_fit.png" alt="" width="80%" />

Based on the aforementioned criteria,
we selected a 3-class model for further analyses.
First, to prevent label switching,
we re-order these classes by the value of the intercept `i`.
Then, we report the estimated parameters.

``` r
res_final <- mx_switch_labels(res_step[[3]], param = "M[1,7]",
    decreasing = FALSE)
tab_res <- table_results(res_final, columns = NULL)
# Select rows and columns
tab_res <- tab_res[tab_res$Category %in% c("Means", "Variances"),
    c("Category", "lhs", "est", "se", "pval", "confint", "name")]
tab_res
```



Table: Results from 3-class LCGA model

|   |Category  |lhs  |   est|   se| pval|confint        |name          |
|:--|:---------|:----|-----:|----:|----:|:--------------|:-------------|
|16 |Means     |i    |  0.33| 0.00| 0.00|[0.32, 0.33]   |class1.M[1,7] |
|17 |Means     |step | -0.02| 0.01| 0.00|[-0.03, -0.01] |class1.M[1,8] |
|18 |Means     |s    |  0.00| 0.00| 0.00|[0.00, 0.01]   |class1.M[1,9] |
|19 |Variances |scl1 |  0.01| 0.00| 0.00|[0.01, 0.01]   |class1.S[1,1] |
|20 |Variances |scl2 |  0.01| 0.00| 0.00|[0.01, 0.01]   |class1.S[2,2] |
|21 |Variances |scl3 |  0.01| 0.00| 0.00|[0.01, 0.01]   |class1.S[3,3] |
|22 |Variances |scl4 |  0.01| 0.00| 0.00|[0.01, 0.01]   |class1.S[4,4] |
|23 |Variances |scl5 |  0.01| 0.00| 0.00|[0.01, 0.01]   |class1.S[5,5] |
|24 |Variances |scl6 |  0.01| 0.00| 0.00|[0.01, 0.02]   |class1.S[6,6] |
|40 |Means     |i    |  0.45| 0.01| 0.00|[0.43, 0.46]   |class2.M[1,7] |
|41 |Means     |step |  0.03| 0.01| 0.00|[0.01, 0.05]   |class2.M[1,8] |
|42 |Means     |s    |  0.02| 0.00| 0.00|[0.01, 0.02]   |class2.M[1,9] |
|43 |Variances |scl1 |  0.01| 0.00| 0.00|[0.01, 0.01]   |class2.S[1,1] |
|44 |Variances |scl2 |  0.01| 0.00| 0.00|[0.01, 0.01]   |class2.S[2,2] |
|45 |Variances |scl3 |  0.01| 0.00| 0.00|[0.01, 0.01]   |class2.S[3,3] |
|46 |Variances |scl4 |  0.01| 0.00| 0.00|[0.01, 0.01]   |class2.S[4,4] |
|47 |Variances |scl5 |  0.01| 0.00| 0.00|[0.01, 0.01]   |class2.S[5,5] |
|48 |Variances |scl6 |  0.01| 0.00| 0.00|[0.01, 0.02]   |class2.S[6,6] |
|64 |Means     |i    |  0.60| 0.01| 0.00|[0.57, 0.63]   |class3.M[1,7] |
|65 |Means     |step |  0.10| 0.02| 0.00|[0.07, 0.14]   |class3.M[1,8] |
|66 |Means     |s    |  0.01| 0.00| 0.08|[-0.00, 0.02]  |class3.M[1,9] |
|67 |Variances |scl1 |  0.01| 0.00| 0.00|[0.01, 0.01]   |class3.S[1,1] |
|68 |Variances |scl2 |  0.01| 0.00| 0.00|[0.01, 0.01]   |class3.S[2,2] |
|69 |Variances |scl3 |  0.01| 0.00| 0.00|[0.01, 0.01]   |class3.S[3,3] |
|70 |Variances |scl4 |  0.01| 0.00| 0.00|[0.01, 0.01]   |class3.S[4,4] |
|71 |Variances |scl5 |  0.01| 0.00| 0.00|[0.01, 0.01]   |class3.S[5,5] |
|72 |Variances |scl6 |  0.01| 0.00| 0.00|[0.01, 0.02]   |class3.S[6,6] |



As evident from these results, 
Class 1 started at a relatively lower level of depressive symptoms,
experienced a decrease after deployment,
followed by increase over time.
Class 2 started at a moderate level of depressive symptoms,
experienced an increase after deployment,
followed by significant increase over time from T2-T6.
Class 3 started at a relatively higher level,
experienced an increase after deployment, followed by stability.

## Wald tests

To test whether parameters are significantly different between classes,
we can use Wald tests.
Wald tests can be specified for all parameters in the model,
using the hypothesis syntax from the `bain` package for informative hypothesis testing.

To identify the names of parameters in the model,
we can use the `name` column of the results table above.
Alternatively, to see all parameters in the model, run:


``` r
names(coef(res_final))
```

```
#>  [1] "mix3.weights[1,2]" "mix3.weights[1,3]" "vscl1"            
#>  [4] "vscl2"             "vscl3"             "vscl4"            
#>  [7] "vscl5"             "vscl6"             "class1.M[1,7]"    
#> [10] "class1.M[1,8]"     "class1.M[1,9]"     "class2.M[1,7]"    
#> [13] "class2.M[1,8]"     "class2.M[1,9]"     "class3.M[1,7]"    
#> [16] "class3.M[1,8]"     "class3.M[1,9]"
```

Next, specify equality constrained hypotheses.
For example, a hypothesis that states that the mean intercept is equal across groups is specified as follows:

`"class1.M[1,7] = class2.M[1,7] & class1.M[1,7] = class3.M[1,7]`

It is also possible to consider comparisons between two classes at a time.
When conducting many significance tests,
consider correcting for multiple comparisons however.


``` r
wald_tests <- wald_test(res_final, "
                   class1.M[1,7] = class2.M[1,7]&
                   class1.M[1,7] = class3.M[1,7];
                   class1.M[1,8] = class2.M[1,8]&
                   class1.M[1,8] = class3.M[1,8];
                   class1.M[1,9] = class2.M[1,9]&
                   class1.M[1,9] = class3.M[1,9]")
# Rename the hypothesis
wald_tests$Hypothesis <- c("Mean i", "Mean step", "Mean slope")
knitr::kable(wald_tests, digits = 2, caption = "Wald tests")
```



Table: Wald tests

|Hypothesis | df| chisq|  p|
|:----------|--:|-----:|--:|
|Mean i     |  2|   468|  0|
|Mean step  |  2|    69|  0|
|Mean slope |  2|    13|  0|



All Wald tests are significant, indicating that there are significant differences between the intercepts, step function, and slopes of the three classes.

## Trajectory plot

Finally, we can plot the growth trajectories.
This can help interpret the results better,
as well as the residual heterogeneity around class trajectories.


``` r
p <- plot_growth(res_step[[3]], rawdata = TRUE, alpha_range = c(0,
    0.05))
# Add Y-axis breaks in original scale
brks <- seq(0, 1, length.out = 5)
labs <- round(invbc(scales::rescale(brks, from = c(0, 1), to = rng_bc),
    lambda))
p <- p + scale_y_continuous(breaks = seq(0, 1, length.out = 5),
    labels = labs) + ylab("SCL (rescaled from Box-Cox)")
p
```

<img src="plot_traj.png" alt="" width="80%" />

Note that the observed individual trajectories show very high variability within classes.
