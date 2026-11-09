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

# Model Estimation

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
#> MxComputeSimAnnealing(tsallis1996) evaluations 181 fit 12175.6 change 1963MxComputeSimAnnealing(tsallis1996) evaluations 493 fit 12175.6 change 4128MxComputeSimAnnealing(tsallis1996) evaluations 801 fit 3956.82 change 2908MxComputeSimAnnealing(tsallis1996) evaluations 1119 fit 12175.6 change 0  MxComputeSimAnnealing(tsallis1996) evaluations 1432 fit -1176.2 change -1.335e+04MxComputeSimAnnealing(tsallis1996) evaluations 1742 fit 12175.6 change 0         MxComputeSimAnnealing(tsallis1996) evaluations 2045 fit -1176.2 change 0MxComputeSimAnnealing(tsallis1996) evaluations 2348 fit -2996 change -1.033e+04MxComputeSimAnnealing(tsallis1996) evaluations 2666 fit 12175.6 change 0       MxComputeSimAnnealing(tsallis1996) evaluations 3004 fit 12175.6 change 885.5MxComputeSimAnnealing(tsallis1996) evaluations 3318 fit -1176.2 change 0    MxComputeSimAnnealing(tsallis1996) evaluations 3632 fit -6761.83 change -5586MxComputeSimAnnealing(tsallis1996) evaluations 3950 fit -2770.39 change 578  MxComputeSimAnnealing(tsallis1996) evaluations 4265 fit 12339.3 change 329.2MxComputeSimAnnealing(tsallis1996) evaluations 4579 fit -7712.21 change -6513MxComputeSimAnnealing(tsallis1996) evaluations 4892 fit -1320.85 change 0    MxComputeSimAnnealing(tsallis1996) evaluations 5208 fit -6793.55 change -1.094e+04MxComputeSimAnnealing(tsallis1996) evaluations 5517 fit -7498.25 change -6171     MxComputeSimAnnealing(tsallis1996) evaluations 5831 fit 5077.64 change 1.282e+04MxComputeSimAnnealing(tsallis1996) evaluations 6145 fit -1427.93 change 6300    MxComputeSimAnnealing(tsallis1996) evaluations 6456 fit -6380.82 change -5819MxComputeSimAnnealing(tsallis1996) evaluations 6775 fit -1435.72 change -1.499e+04MxComputeSimAnnealing(tsallis1996) evaluations 7085 fit -2096.29 change -1.566e+04MxComputeSimAnnealing(tsallis1996) evaluations 7398 fit -2938.47 change -1.684e+04MxComputeSimAnnealing(tsallis1996) evaluations 7708 fit 13683.2 change 1.951e+04  MxComputeSimAnnealing(tsallis1996) evaluations 8020 fit 13683.2 change -4378    MxComputeSimAnnealing(tsallis1996) evaluations 8322 fit -7738.75 change -22.22MxComputeSimAnnealing(tsallis1996) evaluations 8642 fit -4145.54 change 110.3 MxComputeSimAnnealing(tsallis1996) evaluations 8954 fit -6627.81 change -3996MxComputeSimAnnealing(tsallis1996) evaluations 9271 fit -1584.57 change -1.554e+04MxComputeSimAnnealing(tsallis1996) evaluations 9585 fit -1619.52 change 6002      MxComputeSimAnnealing(tsallis1996) evaluations 9898 fit -3485.72 change 2065MxComputeSimAnnealing(tsallis1996) evaluations 10215 fit -4764.79 change 2646MxComputeSimAnnealing(tsallis1996) evaluations 10523 fit -7674.34 change 75.42MxComputeSimAnnealing(tsallis1996) evaluations 10838 fit -7521.74 change -563.2MxComputeSimAnnealing(tsallis1996) evaluations 11153 fit -7749.32 change -1889 MxComputeSimAnnealing(tsallis1996) evaluations 11460 fit -7569.42 change -1.01e+04MxComputeSimAnnealing(tsallis1996) evaluations 11767 fit -7750.27 change -3046    MxComputeSimAnnealing(tsallis1996) evaluations 12079 fit -7750.52 change -2.219e+04MxComputeSimAnnealing(tsallis1996) evaluations 12398 fit 21013.8 change 2.865e+04  MxComputeSimAnnealing(tsallis1996) evaluations 12713 fit -5666.89 change 1063    MxComputeSimAnnealing(tsallis1996) evaluations 13026 fit -7749.53 change -2296MxComputeSimAnnealing(tsallis1996) evaluations 13340 fit -1966.79 change -1.632e+04MxComputeSimAnnealing(tsallis1996) evaluations 13654 fit -1568.18 change 4114      MxComputeSimAnnealing(tsallis1996) evaluations 13968 fit -7750.55 change -2264MxComputeSimAnnealing(tsallis1996) evaluations 14279 fit -7734.73 change -6162MxComputeSimAnnealing(tsallis1996) evaluations 14583 fit -860.974 change 6595 MxComputeSimAnnealing(tsallis1996) evaluations 14895 fit -7592.37 change 123.8MxComputeSimAnnealing(tsallis1996) evaluations 15208 fit -7680.02 change -1.173e+04MxComputeSimAnnealing(tsallis1996) evaluations 15518 fit -7729.24 change 21.68     MxComputeSimAnnealing(tsallis1996) evaluations 15829 fit -7702.2 change -2056 MxComputeSimAnnealing(tsallis1996) evaluations 16139 fit -7024.18 change 725.8MxComputeSimAnnealing(tsallis1996) evaluations 16452 fit -7401.98 change 349.1                                                                              
#> MxComputeSimAnnealing(tsallis1996) evaluations 113 fit 18598.1 change 4101MxComputeSimAnnealing(tsallis1996) evaluations 321 fit 586.668 change 0   MxComputeSimAnnealing(tsallis1996) evaluations 532 fit -6024.25 change 0MxComputeSimAnnealing(tsallis1996) evaluations 743 fit 3592.46 change -2346MxComputeSimAnnealing(tsallis1996) evaluations 956 fit -6024.25 change 0   MxComputeSimAnnealing(tsallis1996) evaluations 1171 fit 586.668 change 0MxComputeSimAnnealing(tsallis1996) evaluations 1379 fit -6193.33 change 0MxComputeSimAnnealing(tsallis1996) evaluations 1591 fit 10796.1 change 1.008e+04MxComputeSimAnnealing(tsallis1996) evaluations 1802 fit -6193.33 change -6780   MxComputeSimAnnealing(tsallis1996) evaluations 2018 fit 3352.88 change 150.1 MxComputeSimAnnealing(tsallis1996) evaluations 2229 fit -6193.33 change 21.43MxComputeSimAnnealing(tsallis1996) evaluations 2437 fit -2090.56 change 3934 MxComputeSimAnnealing(tsallis1996) evaluations 2651 fit 586.668 change 0    MxComputeSimAnnealing(tsallis1996) evaluations 2863 fit 5333.93 change 1.262e+04MxComputeSimAnnealing(tsallis1996) evaluations 3078 fit -6193.33 change 0       MxComputeSimAnnealing(tsallis1996) evaluations 3290 fit -3368.85 change -2977MxComputeSimAnnealing(tsallis1996) evaluations 3501 fit 586.668 change 0     MxComputeSimAnnealing(tsallis1996) evaluations 3711 fit -6024.25 change 0MxComputeSimAnnealing(tsallis1996) evaluations 3923 fit 4689.74 change -5362MxComputeSimAnnealing(tsallis1996) evaluations 4132 fit -6193.33 change 0   MxComputeSimAnnealing(tsallis1996) evaluations 4346 fit 7194.84 change 7692MxComputeSimAnnealing(tsallis1996) evaluations 4555 fit 586.668 change 2164MxComputeSimAnnealing(tsallis1996) evaluations 4767 fit -114.317 change 361.9MxComputeSimAnnealing(tsallis1996) evaluations 4978 fit 586.668 change -1082 MxComputeSimAnnealing(tsallis1996) evaluations 5189 fit -6024.25 change 0   MxComputeSimAnnealing(tsallis1996) evaluations 5400 fit -2545.77 change 4332MxComputeSimAnnealing(tsallis1996) evaluations 5612 fit -5910.87 change -78.6MxComputeSimAnnealing(tsallis1996) evaluations 5825 fit -4285.94 change -2068MxComputeSimAnnealing(tsallis1996) evaluations 6037 fit -5797.24 change 0    MxComputeSimAnnealing(tsallis1996) evaluations 6252 fit 4546.84 change 3890MxComputeSimAnnealing(tsallis1996) evaluations 6462 fit -5766.39 change 0  MxComputeSimAnnealing(tsallis1996) evaluations 6677 fit -7191.9 change -140.1MxComputeSimAnnealing(tsallis1996) evaluations 6884 fit 1936.27 change 0     MxComputeSimAnnealing(tsallis1996) evaluations 7095 fit -7501.06 change -1062MxComputeSimAnnealing(tsallis1996) evaluations 7307 fit 2017.44 change 5013  MxComputeSimAnnealing(tsallis1996) evaluations 7515 fit -5983.64 change 0  MxComputeSimAnnealing(tsallis1996) evaluations 7726 fit -7688.89 change -9177MxComputeSimAnnealing(tsallis1996) evaluations 7938 fit -7704.04 change -1830MxComputeSimAnnealing(tsallis1996) evaluations 8147 fit -8249.69 change -1278MxComputeSimAnnealing(tsallis1996) evaluations 8357 fit -3179.17 change 3363 MxComputeSimAnnealing(tsallis1996) evaluations 8569 fit -5601.78 change 0   MxComputeSimAnnealing(tsallis1996) evaluations 8784 fit -7068.31 change 855.8MxComputeSimAnnealing(tsallis1996) evaluations 8990 fit 5388.39 change 8868  MxComputeSimAnnealing(tsallis1996) evaluations 9207 fit -3748.07 change 4249MxComputeSimAnnealing(tsallis1996) evaluations 9423 fit -6955.71 change 1339MxComputeSimAnnealing(tsallis1996) evaluations 9636 fit 5378.9 change 1.059e+04MxComputeSimAnnealing(tsallis1996) evaluations 9846 fit -6933.64 change 1395   MxComputeSimAnnealing(tsallis1996) evaluations 10061 fit 4954.62 change 5484MxComputeSimAnnealing(tsallis1996) evaluations 10270 fit -5630.32 change 2655MxComputeSimAnnealing(tsallis1996) evaluations 10482 fit -8045.34 change -2210MxComputeSimAnnealing(tsallis1996) evaluations 10700 fit -8153.21 change 175.6MxComputeSimAnnealing(tsallis1996) evaluations 10910 fit -8284.57 change -3122MxComputeSimAnnealing(tsallis1996) evaluations 11110 fit -6474.28 change -5228MxComputeSimAnnealing(tsallis1996) evaluations 11323 fit -6568.39 change -1030MxComputeSimAnnealing(tsallis1996) evaluations 11515 fit -8339.29 change -109.3MxComputeSimAnnealing(tsallis1996) evaluations 11716 fit -7179.81 change -1545 MxComputeSimAnnealing(tsallis1996) evaluations 11928 fit -1540.98 change -378.5MxComputeSimAnnealing(tsallis1996) evaluations 12132 fit -6220.61 change -927.2MxComputeSimAnnealing(tsallis1996) evaluations 12337 fit -7166.21 change -1.49e+04MxComputeSimAnnealing(tsallis1996) evaluations 12514 fit -5659.27 change 108.4    MxComputeSimAnnealing(tsallis1996) evaluations 12715 fit -8299.33 change -1.318e+04MxComputeSimAnnealing(tsallis1996) evaluations 12914 fit -7732.62 change 610.5     MxComputeSimAnnealing(tsallis1996) evaluations 13107 fit -8188.47 change 130.4MxComputeSimAnnealing(tsallis1996) evaluations 13310 fit 5037.39 change 2252  MxComputeSimAnnealing(tsallis1996) evaluations 13512 fit 5037.32 change 9304MxComputeSimAnnealing(tsallis1996) evaluations 13705 fit -7037.14 change -1467MxComputeSimAnnealing(tsallis1996) evaluations 13903 fit 5037.09 change 1.314e+04MxComputeSimAnnealing(tsallis1996) evaluations 14095 fit -8323.03 change -2748   MxComputeSimAnnealing(tsallis1996) evaluations 14291 fit -8345.67 change -361.9MxComputeSimAnnealing(tsallis1996) evaluations 14498 fit 4685.39 change 1.258e+04MxComputeSimAnnealing(tsallis1996) evaluations 14690 fit -8274.06 change -2704   MxComputeSimAnnealing(tsallis1996) evaluations 14887 fit -4677.51 change -3007MxComputeSimAnnealing(tsallis1996) evaluations 15087 fit -8345.67 change -3673MxComputeSimAnnealing(tsallis1996) evaluations 15286 fit -8329.47 change -2769MxComputeSimAnnealing(tsallis1996) evaluations 15489 fit -5592.96 change 2578 MxComputeSimAnnealing(tsallis1996) evaluations 15692 fit -8341.78 change 4.499MxComputeSimAnnealing(tsallis1996) evaluations 15898 fit -7051.68 change -1466MxComputeSimAnnealing(tsallis1996) evaluations 16101 fit -5577.94 change 0    MxComputeSimAnnealing(tsallis1996) evaluations 16305 fit -7487.92 change 856MxComputeSimAnnealing(tsallis1996) evaluations 16509 fit -6736.63 change -993.5MxComputeSimAnnealing(tsallis1996) evaluations 16721 fit -2817.74 change 5525  MxComputeSimAnnealing(tsallis1996) evaluations 16928 fit -8346.07 change -278.3MxComputeSimAnnealing(tsallis1996) evaluations 17125 fit -8346.51 change -804.4MxComputeSimAnnealing(tsallis1996) evaluations 17329 fit -8346.39 change -1286 MxComputeSimAnnealing(tsallis1996) evaluations 17535 fit -8273.47 change 72.9 MxComputeSimAnnealing(tsallis1996) evaluations 17744 fit -2657.69 change 3552MxComputeSimAnnealing(tsallis1996) evaluations 17954 fit -7335.73 change 1010MxComputeSimAnnealing(tsallis1996) evaluations 18157 fit -7909.43 change 272.5MxComputeSimAnnealing(tsallis1996) evaluations 18361 fit -7178.64 change -1420MxComputeSimAnnealing(tsallis1996) evaluations 18566 fit -8200 change 146.7   MxComputeSimAnnealing(tsallis1996) evaluations 18768 fit -7815.19 change -1.087e+04MxComputeSimAnnealing(tsallis1996) evaluations 18969 fit -8324.51 change -6398     MxComputeSimAnnealing(tsallis1996) evaluations 19165 fit -4095.3 change 2954  MxComputeSimAnnealing(tsallis1996) evaluations 19367 fit -7911.93 change 435MxComputeSimAnnealing(tsallis1996) evaluations 19569 fit -8171.37 change 172.3MxComputeSimAnnealing(tsallis1996) evaluations 19772 fit -5569.04 change 223.9MxComputeSimAnnealing(tsallis1996) evaluations 19970 fit -3303.37 change -2391MxComputeSimAnnealing(tsallis1996) evaluations 20177 fit 75.8588 change 6670  MxComputeSimAnnealing(tsallis1996) evaluations 20379 fit -8346.82 change -314MxComputeSimAnnealing(tsallis1996) evaluations 20583 fit -8346.94 change -61.5MxComputeSimAnnealing(tsallis1996) evaluations 20787 fit -5056.55 change 3288 MxComputeSimAnnealing(tsallis1996) evaluations 20990 fit -8110.84 change -709.8MxComputeSimAnnealing(tsallis1996) evaluations 21188 fit -8346.82 change -1289 MxComputeSimAnnealing(tsallis1996) evaluations 21387 fit -8347.03 change -9.554MxComputeSimAnnealing(tsallis1996) evaluations 21592 fit -5571.97 change -0.01131                                                                                 
#> MxComputeSimAnnealing(tsallis1996) evaluations 11 fit 21978 change -2879MxComputeSimAnnealing(tsallis1996) evaluations 166 fit -7442.24 change -3654MxComputeSimAnnealing(tsallis1996) evaluations 322 fit -2509.13 change 4911 MxComputeSimAnnealing(tsallis1996) evaluations 477 fit 4759.94 change -9541MxComputeSimAnnealing(tsallis1996) evaluations 628 fit -7442.24 change -3654MxComputeSimAnnealing(tsallis1996) evaluations 781 fit -7420.42 change -397.4MxComputeSimAnnealing(tsallis1996) evaluations 935 fit 9728.18 change -2470  MxComputeSimAnnealing(tsallis1996) evaluations 1090 fit -7442.24 change -3654MxComputeSimAnnealing(tsallis1996) evaluations 1246 fit -3992.15 change 3428 MxComputeSimAnnealing(tsallis1996) evaluations 1402 fit -3788.3 change -6546MxComputeSimAnnealing(tsallis1996) evaluations 1552 fit -7442.24 change -3654MxComputeSimAnnealing(tsallis1996) evaluations 1705 fit -7420.42 change -397.4MxComputeSimAnnealing(tsallis1996) evaluations 1858 fit -1272.75 change 2306  MxComputeSimAnnealing(tsallis1996) evaluations 2012 fit -3788.3 change 0    MxComputeSimAnnealing(tsallis1996) evaluations 2166 fit -7023.02 change 0MxComputeSimAnnealing(tsallis1996) evaluations 2321 fit 7945.15 change 4760MxComputeSimAnnealing(tsallis1996) evaluations 2475 fit -3788.3 change 0   MxComputeSimAnnealing(tsallis1996) evaluations 2627 fit -7023.02 change 0MxComputeSimAnnealing(tsallis1996) evaluations 2779 fit -3959.27 change 3461MxComputeSimAnnealing(tsallis1996) evaluations 2940 fit -7442.24 change 0   MxComputeSimAnnealing(tsallis1996) evaluations 3092 fit -7420.42 change 0MxComputeSimAnnealing(tsallis1996) evaluations 3242 fit -1453.84 change 6390MxComputeSimAnnealing(tsallis1996) evaluations 3392 fit -622.021 change -566.2MxComputeSimAnnealing(tsallis1996) evaluations 3546 fit -3788.3 change 0      MxComputeSimAnnealing(tsallis1996) evaluations 3695 fit -7442.24 change 0MxComputeSimAnnealing(tsallis1996) evaluations 3852 fit 2689.25 change 385.2MxComputeSimAnnealing(tsallis1996) evaluations 4007 fit -3788.3 change 0    MxComputeSimAnnealing(tsallis1996) evaluations 4163 fit -7420.42 change 0MxComputeSimAnnealing(tsallis1996) evaluations 4321 fit -3788.3 change -3125MxComputeSimAnnealing(tsallis1996) evaluations 4473 fit -7442.24 change 0   MxComputeSimAnnealing(tsallis1996) evaluations 4619 fit -7443.71 change 0MxComputeSimAnnealing(tsallis1996) evaluations 4771 fit -7422.66 change -409.5MxComputeSimAnnealing(tsallis1996) evaluations 4928 fit -5323.36 change -7285 MxComputeSimAnnealing(tsallis1996) evaluations 5081 fit -7466.21 change 741.3MxComputeSimAnnealing(tsallis1996) evaluations 5226 fit -3741.74 change 0    MxComputeSimAnnealing(tsallis1996) evaluations 5375 fit -7466.21 change 0MxComputeSimAnnealing(tsallis1996) evaluations 5531 fit -8062.73 change 355.8MxComputeSimAnnealing(tsallis1996) evaluations 5676 fit -7422.66 change 0    MxComputeSimAnnealing(tsallis1996) evaluations 5826 fit -8477.34 change -4700MxComputeSimAnnealing(tsallis1996) evaluations 5979 fit -1236.29 change 847.8MxComputeSimAnnealing(tsallis1996) evaluations 6131 fit -7466.21 change 0    MxComputeSimAnnealing(tsallis1996) evaluations 6283 fit -7423.49 change -347.2MxComputeSimAnnealing(tsallis1996) evaluations 6438 fit -2237.89 change -6216 MxComputeSimAnnealing(tsallis1996) evaluations 6588 fit 5696.71 change 4478  MxComputeSimAnnealing(tsallis1996) evaluations 6739 fit -7466.21 change -3976MxComputeSimAnnealing(tsallis1996) evaluations 6891 fit -7076.32 change 0    MxComputeSimAnnealing(tsallis1996) evaluations 7041 fit -7423.49 change 0MxComputeSimAnnealing(tsallis1996) evaluations 7193 fit -4166.57 change 1299MxComputeSimAnnealing(tsallis1996) evaluations 7354 fit -7423.49 change -347.2MxComputeSimAnnealing(tsallis1996) evaluations 7506 fit 4009.1 change 1.249e+04MxComputeSimAnnealing(tsallis1996) evaluations 7660 fit -8020.71 change -3370  MxComputeSimAnnealing(tsallis1996) evaluations 7812 fit -7458.97 change 1016 MxComputeSimAnnealing(tsallis1996) evaluations 7964 fit -7871.82 change -449.5MxComputeSimAnnealing(tsallis1996) evaluations 8118 fit 8237.16 change 1.669e+04MxComputeSimAnnealing(tsallis1996) evaluations 8267 fit -2232.33 change 5281    MxComputeSimAnnealing(tsallis1996) evaluations 8417 fit -3462.12 change -0.004088MxComputeSimAnnealing(tsallis1996) evaluations 8568 fit -8125.56 change -458.5   MxComputeSimAnnealing(tsallis1996) evaluations 8722 fit -8472.88 change -1041 MxComputeSimAnnealing(tsallis1996) evaluations 8875 fit 2540.12 change -4684 MxComputeSimAnnealing(tsallis1996) evaluations 9028 fit -8486.25 change -5808MxComputeSimAnnealing(tsallis1996) evaluations 9186 fit -8381.57 change 93.41MxComputeSimAnnealing(tsallis1996) evaluations 9348 fit -7107.9 change 0     MxComputeSimAnnealing(tsallis1996) evaluations 9500 fit -7633 change -1983MxComputeSimAnnealing(tsallis1996) evaluations 9654 fit -7747.85 change -1877MxComputeSimAnnealing(tsallis1996) evaluations 9801 fit -4216.22 change 1747 MxComputeSimAnnealing(tsallis1996) evaluations 9950 fit -3186.04 change -438.1MxComputeSimAnnealing(tsallis1996) evaluations 10102 fit -7074.2 change 497.4 MxComputeSimAnnealing(tsallis1996) evaluations 10257 fit -8499.66 change -791.4MxComputeSimAnnealing(tsallis1996) evaluations 10408 fit -7594.39 change -3109 MxComputeSimAnnealing(tsallis1996) evaluations 10561 fit -7588 change -4893   MxComputeSimAnnealing(tsallis1996) evaluations 10713 fit -7909.01 change -578.1MxComputeSimAnnealing(tsallis1996) evaluations 10867 fit -8420.05 change -7509 MxComputeSimAnnealing(tsallis1996) evaluations 11021 fit -7161.46 change -4995MxComputeSimAnnealing(tsallis1996) evaluations 11174 fit -7197.47 change -9.018MxComputeSimAnnealing(tsallis1996) evaluations 11323 fit -7788.49 change -434.1MxComputeSimAnnealing(tsallis1996) evaluations 11476 fit 88.5272 change 8506   MxComputeSimAnnealing(tsallis1996) evaluations 11627 fit -3379.1 change 362.3MxComputeSimAnnealing(tsallis1996) evaluations 11781 fit -7549.6 change -24.68MxComputeSimAnnealing(tsallis1996) evaluations 11932 fit -8422.09 change -1050MxComputeSimAnnealing(tsallis1996) evaluations 12083 fit -6269.81 change 1804 MxComputeSimAnnealing(tsallis1996) evaluations 12237 fit -1005.83 change 6657MxComputeSimAnnealing(tsallis1996) evaluations 12389 fit -7566.12 change -21.29MxComputeSimAnnealing(tsallis1996) evaluations 12544 fit -6631.81 change 1061  MxComputeSimAnnealing(tsallis1996) evaluations 12698 fit -7940.45 change -2703MxComputeSimAnnealing(tsallis1996) evaluations 12846 fit -8431.78 change -2376MxComputeSimAnnealing(tsallis1996) evaluations 12998 fit -7551.41 change 712.1MxComputeSimAnnealing(tsallis1996) evaluations 13152 fit -7755.88 change 0    MxComputeSimAnnealing(tsallis1996) evaluations 13307 fit -8230.07 change 274.4MxComputeSimAnnealing(tsallis1996) evaluations 13459 fit -7546.55 change -5841MxComputeSimAnnealing(tsallis1996) evaluations 13612 fit -7808.09 change -476.2MxComputeSimAnnealing(tsallis1996) evaluations 13764 fit -8532.55 change -1.35e+04MxComputeSimAnnealing(tsallis1996) evaluations 13918 fit -8070.9 change -356      MxComputeSimAnnealing(tsallis1996) evaluations 14073 fit -7403.93 change 0  MxComputeSimAnnealing(tsallis1996) evaluations 14227 fit -7084.27 change 1450MxComputeSimAnnealing(tsallis1996) evaluations 14376 fit -8517.65 change -1.312e+04MxComputeSimAnnealing(tsallis1996) evaluations 14524 fit -8539.34 change -4672     MxComputeSimAnnealing(tsallis1996) evaluations 14680 fit -8536.82 change -111.9MxComputeSimAnnealing(tsallis1996) evaluations 14831 fit -8538.48 change -704.1MxComputeSimAnnealing(tsallis1996) evaluations 14981 fit -8541.51 change 0.1728MxComputeSimAnnealing(tsallis1996) evaluations 15134 fit -4170.69 change 696.6 MxComputeSimAnnealing(tsallis1996) evaluations 15285 fit -8171.23 change 234.6MxComputeSimAnnealing(tsallis1996) evaluations 15437 fit -8092.36 change -52.96MxComputeSimAnnealing(tsallis1996) evaluations 15588 fit -8042.11 change -208.2MxComputeSimAnnealing(tsallis1996) evaluations 15735 fit -7876.97 change 143.6 MxComputeSimAnnealing(tsallis1996) evaluations 15885 fit -8543.31 change -2092MxComputeSimAnnealing(tsallis1996) evaluations 16039 fit -8540.78 change -4938MxComputeSimAnnealing(tsallis1996) evaluations 16191 fit -8365.52 change -76.57MxComputeSimAnnealing(tsallis1996) evaluations 16343 fit -7899.38 change 0     MxComputeSimAnnealing(tsallis1996) evaluations 16492 fit -3663.68 change 4878MxComputeSimAnnealing(tsallis1996) evaluations 16646 fit -8517.38 change -3780MxComputeSimAnnealing(tsallis1996) evaluations 16803 fit -7434.34 change 0    MxComputeSimAnnealing(tsallis1996) evaluations 16957 fit -5109.61 change -1.741e+04MxComputeSimAnnealing(tsallis1996) evaluations 17110 fit -6083.84 change 2121      MxComputeSimAnnealing(tsallis1996) evaluations 17260 fit -7450.39 change -6895MxComputeSimAnnealing(tsallis1996) evaluations 17406 fit -8526.17 change 25.24MxComputeSimAnnealing(tsallis1996) evaluations 17556 fit -7438.99 change 1113 MxComputeSimAnnealing(tsallis1996) evaluations 17705 fit -7432.46 change 0   MxComputeSimAnnealing(tsallis1996) evaluations 17859 fit -8537.25 change 14.82MxComputeSimAnnealing(tsallis1996) evaluations 18008 fit -4197.79 change 3491 MxComputeSimAnnealing(tsallis1996) evaluations 18156 fit -8497.23 change 51.35MxComputeSimAnnealing(tsallis1996) evaluations 18308 fit -799.185 change 4026 MxComputeSimAnnealing(tsallis1996) evaluations 18458 fit -7413.85 change 0   MxComputeSimAnnealing(tsallis1996) evaluations 18609 fit -7450.86 change 1102MxComputeSimAnnealing(tsallis1996) evaluations 18760 fit -8552.95 change -37.86MxComputeSimAnnealing(tsallis1996) evaluations 18909 fit -8218.98 change 333.8 MxComputeSimAnnealing(tsallis1996) evaluations 19059 fit -8473.67 change -254.3MxComputeSimAnnealing(tsallis1996) evaluations 19209 fit -8529.87 change -497.3MxComputeSimAnnealing(tsallis1996) evaluations 19361 fit -8441.73 change -1034 MxComputeSimAnnealing(tsallis1996) evaluations 19511 fit -8214.6 change -777.1MxComputeSimAnnealing(tsallis1996) evaluations 19659 fit -8527.34 change -1081MxComputeSimAnnealing(tsallis1996) evaluations 19808 fit -7904.16 change 0    MxComputeSimAnnealing(tsallis1996) evaluations 19959 fit -8554.29 change 0.1441MxComputeSimAnnealing(tsallis1996) evaluations 20114 fit -8554.41 change -145.5MxComputeSimAnnealing(tsallis1996) evaluations 20271 fit -7953.37 change 600.9 MxComputeSimAnnealing(tsallis1996) evaluations 20424 fit -4920.81 change 2799 MxComputeSimAnnealing(tsallis1996) evaluations 20568 fit -8554.66 change -2.587MxComputeSimAnnealing(tsallis1996) evaluations 20720 fit -8546.52 change -1134 MxComputeSimAnnealing(tsallis1996) evaluations 20875 fit -7488.89 change 1049 MxComputeSimAnnealing(tsallis1996) evaluations 21025 fit -8541.03 change -1055MxComputeSimAnnealing(tsallis1996) evaluations 21177 fit -2636.59 change 5750 MxComputeSimAnnealing(tsallis1996) evaluations 21329 fit -7243.76 change 1295MxComputeSimAnnealing(tsallis1996) evaluations 21483 fit -8540.89 change 2.018MxComputeSimAnnealing(tsallis1996) evaluations 21630 fit -8458.3 change 96.85 MxComputeSimAnnealing(tsallis1996) evaluations 21780 fit -8555.28 change -2.093MxComputeSimAnnealing(tsallis1996) evaluations 21928 fit -8062.6 change -590   MxComputeSimAnnealing(tsallis1996) evaluations 22078 fit -848.14 change 7034MxComputeSimAnnealing(tsallis1996) evaluations 22229 fit -3884.22 change 3838MxComputeSimAnnealing(tsallis1996) evaluations 22377 fit -8545.83 change -6169MxComputeSimAnnealing(tsallis1996) evaluations 22537 fit -8555.4 change -1089 MxComputeSimAnnealing(tsallis1996) evaluations 22692 fit -8537.62 change -419.5MxComputeSimAnnealing(tsallis1996) evaluations 22843 fit -8555.57 change -12.58MxComputeSimAnnealing(tsallis1996) evaluations 22998 fit -7458.42 change 0     MxComputeSimAnnealing(tsallis1996) evaluations 23143 fit -7468.07 change -53.59MxComputeSimAnnealing(tsallis1996) evaluations 23299 fit 2741.24 change 1.123e+04MxComputeSimAnnealing(tsallis1996) evaluations 23448 fit -7527.25 change 491.9   MxComputeSimAnnealing(tsallis1996) evaluations 23598 fit -7758.57 change -3823MxComputeSimAnnealing(tsallis1996) evaluations 23747 fit -335.45 change 4824  MxComputeSimAnnealing(tsallis1996) evaluations 23898 fit -7422.5 change 1133MxComputeSimAnnealing(tsallis1996) evaluations 24040 fit -8543.78 change -514.6MxComputeSimAnnealing(tsallis1996) evaluations 24188 fit -8555.53 change -5690 MxComputeSimAnnealing(tsallis1996) evaluations 24343 fit -7885.63 change 670.1MxComputeSimAnnealing(tsallis1996) evaluations 24490 fit -8490.78 change -1012MxComputeSimAnnealing(tsallis1996) evaluations 24643 fit -484.173 change 1671 MxComputeSimAnnealing(tsallis1996) evaluations 24795 fit -8412.26 change -2264MxComputeSimAnnealing(tsallis1996) evaluations 24947 fit -8531.8 change 24.18 MxComputeSimAnnealing(tsallis1996) evaluations 25097 fit -7485.97 change 1070MxComputeSimAnnealing(tsallis1996) evaluations 25248 fit -8546.74 change 9.385MxComputeSimAnnealing(tsallis1996) evaluations 25398 fit -8023.49 change -1.309e+04MxComputeSimAnnealing(tsallis1996) evaluations 25550 fit -8530.59 change -5430     MxComputeSimAnnealing(tsallis1996) evaluations 25706 fit -8553.48 change 2.815MxComputeSimAnnealing(tsallis1996) evaluations 25861 fit -4077.23 change 4479 MxComputeSimAnnealing(tsallis1996) evaluations 26011 fit -7513.1 change 596.3MxComputeSimAnnealing(tsallis1996) evaluations 26161 fit -301.339 change 7460MxComputeSimAnnealing(tsallis1996) evaluations 26312 fit -7418.28 change 0   MxComputeSimAnnealing(tsallis1996) evaluations 26463 fit -7492.84 change 0MxComputeSimAnnealing(tsallis1996) evaluations 26613 fit -8110.27 change 446.1MxComputeSimAnnealing(tsallis1996) evaluations 26768 fit -8494.9 change 19.42                                                                              
#> MxComputeSimAnnealing(tsallis1996) evaluations 121 fit -7825.65 change -530.3MxComputeSimAnnealing(tsallis1996) evaluations 243 fit -7295.31 change -2.067e+04MxComputeSimAnnealing(tsallis1996) evaluations 372 fit -7825.65 change 0         MxComputeSimAnnealing(tsallis1996) evaluations 497 fit -7825.65 change 0MxComputeSimAnnealing(tsallis1996) evaluations 622 fit -7825.65 change 0MxComputeSimAnnealing(tsallis1996) evaluations 745 fit -7295.31 change 0MxComputeSimAnnealing(tsallis1996) evaluations 868 fit -7295.31 change -1.075e+04MxComputeSimAnnealing(tsallis1996) evaluations 994 fit -7295.31 change 0         MxComputeSimAnnealing(tsallis1996) evaluations 1109 fit -3019.98 change 1804MxComputeSimAnnealing(tsallis1996) evaluations 1231 fit -7367.16 change 0   MxComputeSimAnnealing(tsallis1996) evaluations 1351 fit -7465.85 change 0MxComputeSimAnnealing(tsallis1996) evaluations 1474 fit -7465.85 change 359.8MxComputeSimAnnealing(tsallis1996) evaluations 1596 fit -7825.65 change -530.3MxComputeSimAnnealing(tsallis1996) evaluations 1720 fit -7295.31 change 0     MxComputeSimAnnealing(tsallis1996) evaluations 1843 fit -7295.31 change -1.299e+04MxComputeSimAnnealing(tsallis1996) evaluations 1966 fit 164.466 change -5961      MxComputeSimAnnealing(tsallis1996) evaluations 2088 fit -5921.42 change -9882MxComputeSimAnnealing(tsallis1996) evaluations 2209 fit -6996.27 change -7612MxComputeSimAnnealing(tsallis1996) evaluations 2329 fit -7027.27 change 0    MxComputeSimAnnealing(tsallis1996) evaluations 2446 fit -7825.65 change -530.3MxComputeSimAnnealing(tsallis1996) evaluations 2565 fit 8725.34 change 9677   MxComputeSimAnnealing(tsallis1996) evaluations 2690 fit 1845.8 change -744.8MxComputeSimAnnealing(tsallis1996) evaluations 2809 fit -3056.62 change 4471MxComputeSimAnnealing(tsallis1996) evaluations 2931 fit -7367.16 change 0   MxComputeSimAnnealing(tsallis1996) evaluations 3052 fit -7027.27 change 438.6MxComputeSimAnnealing(tsallis1996) evaluations 3172 fit -7825.65 change 0    MxComputeSimAnnealing(tsallis1996) evaluations 3294 fit -7295.31 change 0MxComputeSimAnnealing(tsallis1996) evaluations 3422 fit -7825.65 change 0MxComputeSimAnnealing(tsallis1996) evaluations 3546 fit -7825.65 change -530.3MxComputeSimAnnealing(tsallis1996) evaluations 3671 fit -7825.65 change -530.3MxComputeSimAnnealing(tsallis1996) evaluations 3794 fit -7295.31 change 0     MxComputeSimAnnealing(tsallis1996) evaluations 3921 fit -7825.65 change -530.3MxComputeSimAnnealing(tsallis1996) evaluations 4049 fit -7465.85 change 359.8 MxComputeSimAnnealing(tsallis1996) evaluations 4170 fit -7295.31 change 0    MxComputeSimAnnealing(tsallis1996) evaluations 4291 fit -901.305 change 1911MxComputeSimAnnealing(tsallis1996) evaluations 4413 fit 2542.8 change -2072 MxComputeSimAnnealing(tsallis1996) evaluations 4535 fit -8368.99 change -6058MxComputeSimAnnealing(tsallis1996) evaluations 4659 fit -4810.17 change 567.5MxComputeSimAnnealing(tsallis1996) evaluations 4783 fit -6969.42 change 397.7MxComputeSimAnnealing(tsallis1996) evaluations 4905 fit -7367.16 change -339.9MxComputeSimAnnealing(tsallis1996) evaluations 5028 fit -7027.27 change 0     MxComputeSimAnnealing(tsallis1996) evaluations 5150 fit -7465.85 change 0MxComputeSimAnnealing(tsallis1996) evaluations 5275 fit -7465.85 change 0MxComputeSimAnnealing(tsallis1996) evaluations 5398 fit -7825.65 change 0MxComputeSimAnnealing(tsallis1996) evaluations 5522 fit -7825.65 change 0MxComputeSimAnnealing(tsallis1996) evaluations 5641 fit -2339.56 change 4558MxComputeSimAnnealing(tsallis1996) evaluations 5761 fit 665.553 change -7804MxComputeSimAnnealing(tsallis1996) evaluations 5885 fit -6081.25 change -3449MxComputeSimAnnealing(tsallis1996) evaluations 6008 fit -5076.65 change 2291 MxComputeSimAnnealing(tsallis1996) evaluations 6130 fit -7367.16 change -339.9MxComputeSimAnnealing(tsallis1996) evaluations 6252 fit -7027.27 change 438.6 MxComputeSimAnnealing(tsallis1996) evaluations 6373 fit -7825.65 change 0    MxComputeSimAnnealing(tsallis1996) evaluations 6495 fit -7295.31 change 0MxComputeSimAnnealing(tsallis1996) evaluations 6618 fit -7295.31 change -4631MxComputeSimAnnealing(tsallis1996) evaluations 6743 fit -7295.31 change -1998MxComputeSimAnnealing(tsallis1996) evaluations 6865 fit -3122.73 change 4558 MxComputeSimAnnealing(tsallis1996) evaluations 6990 fit 3838.03 change 1.22e+04MxComputeSimAnnealing(tsallis1996) evaluations 7108 fit -8271.74 change -896.2 MxComputeSimAnnealing(tsallis1996) evaluations 7231 fit -7375.63 change 293.5 MxComputeSimAnnealing(tsallis1996) evaluations 7355 fit -7375.63 change -359.9MxComputeSimAnnealing(tsallis1996) evaluations 7477 fit -7015.74 change 464.2 MxComputeSimAnnealing(tsallis1996) evaluations 7596 fit -7822.93 change -531.1MxComputeSimAnnealing(tsallis1996) evaluations 7716 fit -2794.84 change -3709 MxComputeSimAnnealing(tsallis1996) evaluations 7838 fit -6044.35 change -4323MxComputeSimAnnealing(tsallis1996) evaluations 7953 fit -7015.74 change 0    MxComputeSimAnnealing(tsallis1996) evaluations 8073 fit -7791.96 change 0MxComputeSimAnnealing(tsallis1996) evaluations 8194 fit -7332.18 change 0MxComputeSimAnnealing(tsallis1996) evaluations 8317 fit -6966.55 change -1.263e+04MxComputeSimAnnealing(tsallis1996) evaluations 8443 fit -7332.18 change -5759     MxComputeSimAnnealing(tsallis1996) evaluations 8568 fit -7332.18 change -6547MxComputeSimAnnealing(tsallis1996) evaluations 8686 fit -8437.74 change -4554MxComputeSimAnnealing(tsallis1996) evaluations 8805 fit -7390.49 change -386.2MxComputeSimAnnealing(tsallis1996) evaluations 8932 fit -7390.49 change 0     MxComputeSimAnnealing(tsallis1996) evaluations 9052 fit -7004.29 change 485.8MxComputeSimAnnealing(tsallis1996) evaluations 9173 fit -7792.07 change 0    MxComputeSimAnnealing(tsallis1996) evaluations 9295 fit -7332.29 change 0MxComputeSimAnnealing(tsallis1996) evaluations 9418 fit -7332.29 change -4842MxComputeSimAnnealing(tsallis1996) evaluations 9539 fit -3157.26 change -1238MxComputeSimAnnealing(tsallis1996) evaluations 9659 fit -8276.27 change -4118MxComputeSimAnnealing(tsallis1996) evaluations 9781 fit -7416.09 change -33.34MxComputeSimAnnealing(tsallis1996) evaluations 9904 fit -6980.4 change 0      MxComputeSimAnnealing(tsallis1996) evaluations 10025 fit -7563.75 change 874MxComputeSimAnnealing(tsallis1996) evaluations 10147 fit -7783.08 change 470.4MxComputeSimAnnealing(tsallis1996) evaluations 10268 fit -7314.35 change -1774MxComputeSimAnnealing(tsallis1996) evaluations 10388 fit -2483 change 5486    MxComputeSimAnnealing(tsallis1996) evaluations 10510 fit -1316.98 change 3006MxComputeSimAnnealing(tsallis1996) evaluations 10636 fit -8410.56 change 20.78MxComputeSimAnnealing(tsallis1996) evaluations 10758 fit -8443.16 change -992.3MxComputeSimAnnealing(tsallis1996) evaluations 10880 fit -8142.88 change -1029 MxComputeSimAnnealing(tsallis1996) evaluations 11003 fit -7113.59 change 0    MxComputeSimAnnealing(tsallis1996) evaluations 11125 fit -8027.72 change -532.9MxComputeSimAnnealing(tsallis1996) evaluations 11245 fit -7316.11 change 14.71 MxComputeSimAnnealing(tsallis1996) evaluations 11369 fit -7317.1 change 995.3 MxComputeSimAnnealing(tsallis1996) evaluations 11487 fit -3289.31 change 5150MxComputeSimAnnealing(tsallis1996) evaluations 11607 fit -7536.1 change 0    MxComputeSimAnnealing(tsallis1996) evaluations 11730 fit -7497.63 change -480.3MxComputeSimAnnealing(tsallis1996) evaluations 11848 fit -7777.18 change 0     MxComputeSimAnnealing(tsallis1996) evaluations 11972 fit -8114.98 change 332.5MxComputeSimAnnealing(tsallis1996) evaluations 12094 fit -7340.84 change 372.3MxComputeSimAnnealing(tsallis1996) evaluations 12218 fit -7358.85 change -1.049e+04MxComputeSimAnnealing(tsallis1996) evaluations 12342 fit 443.531 change 8736       MxComputeSimAnnealing(tsallis1996) evaluations 12460 fit -6349 change 1525  MxComputeSimAnnealing(tsallis1996) evaluations 12579 fit -7002.45 change 1054MxComputeSimAnnealing(tsallis1996) evaluations 12703 fit -8037.18 change -1032MxComputeSimAnnealing(tsallis1996) evaluations 12832 fit -8448.29 change -848 MxComputeSimAnnealing(tsallis1996) evaluations 12956 fit -7603.29 change -90.63MxComputeSimAnnealing(tsallis1996) evaluations 13079 fit -7342.09 change 1108  MxComputeSimAnnealing(tsallis1996) evaluations 13200 fit -7462.53 change -7.312MxComputeSimAnnealing(tsallis1996) evaluations 13321 fit -8095.71 change -793.8MxComputeSimAnnealing(tsallis1996) evaluations 13452 fit -7052.85 change 425.3 MxComputeSimAnnealing(tsallis1996) evaluations 13571 fit -8452.38 change -1190MxComputeSimAnnealing(tsallis1996) evaluations 13693 fit -7721.68 change 271.8MxComputeSimAnnealing(tsallis1996) evaluations 13815 fit 7021.73 change 1.071e+04MxComputeSimAnnealing(tsallis1996) evaluations 13936 fit -8451.95 change -15.8   MxComputeSimAnnealing(tsallis1996) evaluations 14059 fit -7751.69 change 684.6MxComputeSimAnnealing(tsallis1996) evaluations 14183 fit -8337.48 change -827 MxComputeSimAnnealing(tsallis1996) evaluations 14305 fit -7510.53 change -469.1MxComputeSimAnnealing(tsallis1996) evaluations 14426 fit -7500.52 change 894.8 MxComputeSimAnnealing(tsallis1996) evaluations 14548 fit -7815.72 change 628.4MxComputeSimAnnealing(tsallis1996) evaluations 14668 fit -7290.2 change -4373 MxComputeSimAnnealing(tsallis1996) evaluations 14790 fit -344.412 change -1.529e+04MxComputeSimAnnealing(tsallis1996) evaluations 14903 fit -7384.55 change -69.86    MxComputeSimAnnealing(tsallis1996) evaluations 15027 fit -8460.56 change -829.2MxComputeSimAnnealing(tsallis1996) evaluations 15150 fit -7631.79 change 278.5 MxComputeSimAnnealing(tsallis1996) evaluations 15271 fit -7952.58 change -690.7MxComputeSimAnnealing(tsallis1996) evaluations 15395 fit -8392.49 change -1140 MxComputeSimAnnealing(tsallis1996) evaluations 15516 fit -621.294 change 4274 MxComputeSimAnnealing(tsallis1996) evaluations 15638 fit -4377.94 change 4126MxComputeSimAnnealing(tsallis1996) evaluations 15758 fit -8144.89 change -466.7MxComputeSimAnnealing(tsallis1996) evaluations 15879 fit -7285.87 change 1229  MxComputeSimAnnealing(tsallis1996) evaluations 16003 fit -7274.51 change 18.18MxComputeSimAnnealing(tsallis1996) evaluations 16123 fit -8006.87 change 0    MxComputeSimAnnealing(tsallis1996) evaluations 16243 fit -7780.6 change -1.477e+04MxComputeSimAnnealing(tsallis1996) evaluations 16361 fit -8471.83 change -1268    MxComputeSimAnnealing(tsallis1996) evaluations 16486 fit 27224.5 change 2.083e+04MxComputeSimAnnealing(tsallis1996) evaluations 16607 fit -7823.32 change -15.83  MxComputeSimAnnealing(tsallis1996) evaluations 16730 fit -7806.14 change 363.9 MxComputeSimAnnealing(tsallis1996) evaluations 16852 fit -8528 change -13.03  MxComputeSimAnnealing(tsallis1996) evaluations 16971 fit -7971.31 change 119.1MxComputeSimAnnealing(tsallis1996) evaluations 17093 fit -7242.76 change -6291MxComputeSimAnnealing(tsallis1996) evaluations 17226 fit -8357.26 change -784.5MxComputeSimAnnealing(tsallis1996) evaluations 17347 fit -8526.56 change -302  MxComputeSimAnnealing(tsallis1996) evaluations 17472 fit -8541.27 change -4.428MxComputeSimAnnealing(tsallis1996) evaluations 17593 fit -7280.93 change 34.25 MxComputeSimAnnealing(tsallis1996) evaluations 17717 fit -8233.72 change -4227MxComputeSimAnnealing(tsallis1996) evaluations 17841 fit -2927.46 change 5616 MxComputeSimAnnealing(tsallis1996) evaluations 17969 fit -7198.92 change -0.2318MxComputeSimAnnealing(tsallis1996) evaluations 18091 fit -8527.25 change 17.56  MxComputeSimAnnealing(tsallis1996) evaluations 18211 fit -8532.02 change -777 MxComputeSimAnnealing(tsallis1996) evaluations 18331 fit -7863.62 change 683.4MxComputeSimAnnealing(tsallis1996) evaluations 18437 fit -7964.26 change -101.9MxComputeSimAnnealing(tsallis1996) evaluations 18557 fit -8514.66 change -651  MxComputeSimAnnealing(tsallis1996) evaluations 18682 fit -7880.84 change 666.1MxComputeSimAnnealing(tsallis1996) evaluations 18803 fit -7454.64 change 1093 MxComputeSimAnnealing(tsallis1996) evaluations 18927 fit -7469.08 change 119.1MxComputeSimAnnealing(tsallis1996) evaluations 19046 fit -8006.02 change 519.6MxComputeSimAnnealing(tsallis1996) evaluations 19168 fit -8528.38 change -366.3MxComputeSimAnnealing(tsallis1996) evaluations 19288 fit -5072.94 change 798.7 MxComputeSimAnnealing(tsallis1996) evaluations 19410 fit -8458.88 change -4556MxComputeSimAnnealing(tsallis1996) evaluations 19533 fit -8554.15 change -689.9MxComputeSimAnnealing(tsallis1996) evaluations 19653 fit -7458.77 change 0     MxComputeSimAnnealing(tsallis1996) evaluations 19775 fit -7577.11 change 845.9MxComputeSimAnnealing(tsallis1996) evaluations 19893 fit -7158.91 change -4746MxComputeSimAnnealing(tsallis1996) evaluations 20014 fit -8173.58 change -3594MxComputeSimAnnealing(tsallis1996) evaluations 20134 fit -3146.09 change 523.3MxComputeSimAnnealing(tsallis1996) evaluations 20256 fit -8536.23 change 20.61MxComputeSimAnnealing(tsallis1996) evaluations 20379 fit -7472.92 change -5.505e-08MxComputeSimAnnealing(tsallis1996) evaluations 20501 fit -8526.78 change 11.71     MxComputeSimAnnealing(tsallis1996) evaluations 20622 fit -7970.11 change 487.2MxComputeSimAnnealing(tsallis1996) evaluations 20746 fit -8214.98 change 259.1MxComputeSimAnnealing(tsallis1996) evaluations 20868 fit -7207.76 change -1644MxComputeSimAnnealing(tsallis1996) evaluations 20989 fit -6935.17 change -2492MxComputeSimAnnealing(tsallis1996) evaluations 21107 fit -7872.01 change 683.8MxComputeSimAnnealing(tsallis1996) evaluations 21228 fit -7482.07 change 756.9MxComputeSimAnnealing(tsallis1996) evaluations 21346 fit -8558.27 change -50.23MxComputeSimAnnealing(tsallis1996) evaluations 21470 fit -7214.39 change 1308  MxComputeSimAnnealing(tsallis1996) evaluations 21596 fit -7957.08 change -737.7MxComputeSimAnnealing(tsallis1996) evaluations 21718 fit -8088.77 change -1057 MxComputeSimAnnealing(tsallis1996) evaluations 21836 fit -8551.65 change -1055MxComputeSimAnnealing(tsallis1996) evaluations 21957 fit -7865 change 298.4   MxComputeSimAnnealing(tsallis1996) evaluations 22078 fit -7481.3 change 847.1MxComputeSimAnnealing(tsallis1996) evaluations 22198 fit -7952.8 change 535.1MxComputeSimAnnealing(tsallis1996) evaluations 22316 fit -8561.33 change -1.23e+04MxComputeSimAnnealing(tsallis1996) evaluations 22433 fit -8562.07 change -6.865   MxComputeSimAnnealing(tsallis1996) evaluations 22552 fit -7518.48 change 20.14 MxComputeSimAnnealing(tsallis1996) evaluations 22671 fit -8559.24 change -1339MxComputeSimAnnealing(tsallis1996) evaluations 22791 fit -8506.84 change -2992MxComputeSimAnnealing(tsallis1996) evaluations 22910 fit -8474.59 change -5757MxComputeSimAnnealing(tsallis1996) evaluations 23031 fit -7882.96 change 600.9MxComputeSimAnnealing(tsallis1996) evaluations 23150 fit -8561.69 change -14.79MxComputeSimAnnealing(tsallis1996) evaluations 23271 fit -7927.02 change -706.7MxComputeSimAnnealing(tsallis1996) evaluations 23393 fit -7224.5 change -4667  MxComputeSimAnnealing(tsallis1996) evaluations 23511 fit 715.737 change 9262 MxComputeSimAnnealing(tsallis1996) evaluations 23632 fit -8562.57 change -15.12MxComputeSimAnnealing(tsallis1996) evaluations 23757 fit -8302.54 change -404.6MxComputeSimAnnealing(tsallis1996) evaluations 23881 fit -8114.45 change 281.7 MxComputeSimAnnealing(tsallis1996) evaluations 24003 fit -8553.95 change -1044MxComputeSimAnnealing(tsallis1996) evaluations 24124 fit -8563.28 change -661.1MxComputeSimAnnealing(tsallis1996) evaluations 24244 fit -7832.22 change 725   MxComputeSimAnnealing(tsallis1996) evaluations 24363 fit -6903.89 change 1601MxComputeSimAnnealing(tsallis1996) evaluations 24484 fit -8563.49 change -1.028MxComputeSimAnnealing(tsallis1996) evaluations 24606 fit -7890.4 change 0      MxComputeSimAnnealing(tsallis1996) evaluations 24722 fit -8551.44 change -679.6MxComputeSimAnnealing(tsallis1996) evaluations 24845 fit -8563.19 change -1279 MxComputeSimAnnealing(tsallis1996) evaluations 24967 fit -8563.52 change -1.258e+04MxComputeSimAnnealing(tsallis1996) evaluations 25090 fit -7896.99 change 163.6     MxComputeSimAnnealing(tsallis1996) evaluations 25211 fit -5395.95 change 3156 MxComputeSimAnnealing(tsallis1996) evaluations 25333 fit -8563.78 change -670.3MxComputeSimAnnealing(tsallis1996) evaluations 25454 fit -8563.78 change -1048 MxComputeSimAnnealing(tsallis1996) evaluations 25574 fit -8562.55 change -703.7MxComputeSimAnnealing(tsallis1996) evaluations 25697 fit -8562.17 change -1.366MxComputeSimAnnealing(tsallis1996) evaluations 25821 fit -7857.48 change -552.4MxComputeSimAnnealing(tsallis1996) evaluations 25942 fit -3156.74 change -1942 MxComputeSimAnnealing(tsallis1996) evaluations 26061 fit -8550.89 change 5.501MxComputeSimAnnealing(tsallis1996) evaluations 26183 fit -8564.02 change -2.31MxComputeSimAnnealing(tsallis1996) evaluations 26303 fit -8392.51 change -895.4MxComputeSimAnnealing(tsallis1996) evaluations 26420 fit -8563.93 change -10.44MxComputeSimAnnealing(tsallis1996) evaluations 26540 fit -8562.1 change -78.21 MxComputeSimAnnealing(tsallis1996) evaluations 26669 fit -7334.57 change 1221 MxComputeSimAnnealing(tsallis1996) evaluations 26789 fit -8563.61 change -1636MxComputeSimAnnealing(tsallis1996) evaluations 26910 fit -8563.93 change -9.501MxComputeSimAnnealing(tsallis1996) evaluations 27031 fit -8545.04 change 19.72 MxComputeSimAnnealing(tsallis1996) evaluations 27149 fit -8564.62 change -619.7MxComputeSimAnnealing(tsallis1996) evaluations 27268 fit -8490.16 change -94.92MxComputeSimAnnealing(tsallis1996) evaluations 27388 fit -4137.48 change 4420  MxComputeSimAnnealing(tsallis1996) evaluations 27508 fit -8227.57 change -333.9MxComputeSimAnnealing(tsallis1996) evaluations 27628 fit -8565.05 change -965.7MxComputeSimAnnealing(tsallis1996) evaluations 27751 fit -8537.22 change -951.4MxComputeSimAnnealing(tsallis1996) evaluations 27871 fit -7765.21 change -366  MxComputeSimAnnealing(tsallis1996) evaluations 27991 fit -8565.26 change -29.36MxComputeSimAnnealing(tsallis1996) evaluations 28111 fit -8558.37 change -1.491e+04MxComputeSimAnnealing(tsallis1996) evaluations 28233 fit -6411.41 change 1488      MxComputeSimAnnealing(tsallis1996) evaluations 28352 fit -7532.22 change 59.3MxComputeSimAnnealing(tsallis1996) evaluations 28473 fit -8020.75 change -279.5MxComputeSimAnnealing(tsallis1996) evaluations 28596 fit -8565.57 change -41.8 MxComputeSimAnnealing(tsallis1996) evaluations 28716 fit -8558.5 change -6074 MxComputeSimAnnealing(tsallis1996) evaluations 28838 fit -1523.12 change 7043MxComputeSimAnnealing(tsallis1996) evaluations 28960 fit -8564.25 change 1.677MxComputeSimAnnealing(tsallis1996) evaluations 29080 fit -8565.77 change 0.08465MxComputeSimAnnealing(tsallis1996) evaluations 29201 fit -8558.73 change -36.16 MxComputeSimAnnealing(tsallis1996) evaluations 29321 fit -7673.9 change 892.2  MxComputeSimAnnealing(tsallis1996) evaluations 29443 fit -8557.29 change -517.9MxComputeSimAnnealing(tsallis1996) evaluations 29563 fit -8310.79 change -2228 MxComputeSimAnnealing(tsallis1996) evaluations 29684 fit -8535.33 change -2079MxComputeSimAnnealing(tsallis1996) evaluations 29805 fit -8564.41 change -1027MxComputeSimAnnealing(tsallis1996) evaluations 29928 fit -7506.52 change 0    MxComputeSimAnnealing(tsallis1996) evaluations 30048 fit -8475.69 change -824.7MxComputeSimAnnealing(tsallis1996) evaluations 30171 fit -8566.6 change -1102  MxComputeSimAnnealing(tsallis1996) evaluations 30297 fit -8565.31 change -989.2MxComputeSimAnnealing(tsallis1996) evaluations 30418 fit -7508.2 change -413.8 MxComputeSimAnnealing(tsallis1996) evaluations 30544 fit -8567.06 change -747.6MxComputeSimAnnealing(tsallis1996) evaluations 30668 fit -8567.61 change -45.43MxComputeSimAnnealing(tsallis1996) evaluations 30789 fit -2833.04 change 5733  MxComputeSimAnnealing(tsallis1996) evaluations 30908 fit -8567.92 change -679.4MxComputeSimAnnealing(tsallis1996) evaluations 31030 fit -8567.71 change -1046 MxComputeSimAnnealing(tsallis1996) evaluations 31149 fit -8567.96 change -11.82MxComputeSimAnnealing(tsallis1996) evaluations 31270 fit -7552.75 change 988.9 MxComputeSimAnnealing(tsallis1996) evaluations 31389 fit -4820.68 change -2369MxComputeSimAnnealing(tsallis1996) evaluations 31508 fit -8569.52 change -690.9MxComputeSimAnnealing(tsallis1996) evaluations 31627 fit -8569.61 change -0.06635MxComputeSimAnnealing(tsallis1996) evaluations 31747 fit -7416.97 change 845     MxComputeSimAnnealing(tsallis1996) evaluations 31868 fit -7611.87 change -4112MxComputeSimAnnealing(tsallis1996) evaluations 31988 fit -8557.05 change -8036                                                                              
#> Beginning initial fit attemptMxComputeNumericDeriv 28/45                           Fit attempt 0, fit=-5186.90412866336, new current best! (was -5186.90412866336)Beginning fit attempt 1 of at maximum 50 extra tries                           Beginning fit attempt 2 of at maximum 50 extra triesBeginning fit attempt 3 of at maximum 50 extra triesBeginning fit attempt 4 of at maximum 50 extra triesBeginning fit attempt 5 of at maximum 50 extra triesBeginning fit attempt 6 of at maximum 50 extra triesBeginning fit attempt 7 of at maximum 50 extra triesBeginning fit attempt 8 of at maximum 50 extra triesBeginning fit attempt 9 of at maximum 50 extra triesBeginning fit attempt 10 of at maximum 50 extra triesBeginning fit attempt 11 of at maximum 50 extra triesBeginning fit attempt 12 of at maximum 50 extra triesBeginning fit attempt 13 of at maximum 50 extra triesBeginning fit attempt 14 of at maximum 50 extra triesBeginning fit attempt 15 of at maximum 50 extra triesBeginning fit attempt 16 of at maximum 50 extra triesBeginning fit attempt 17 of at maximum 50 extra triesBeginning fit attempt 18 of at maximum 50 extra triesBeginning fit attempt 19 of at maximum 50 extra triesBeginning fit attempt 20 of at maximum 50 extra triesBeginning fit attempt 21 of at maximum 50 extra triesBeginning fit attempt 22 of at maximum 50 extra triesBeginning fit attempt 23 of at maximum 50 extra triesBeginning fit attempt 24 of at maximum 50 extra triesBeginning fit attempt 25 of at maximum 50 extra triesBeginning fit attempt 26 of at maximum 50 extra triesBeginning fit attempt 27 of at maximum 50 extra triesBeginning fit attempt 28 of at maximum 50 extra triesBeginning fit attempt 29 of at maximum 50 extra triesBeginning fit attempt 30 of at maximum 50 extra triesBeginning fit attempt 31 of at maximum 50 extra triesBeginning fit attempt 32 of at maximum 50 extra triesBeginning fit attempt 33 of at maximum 50 extra triesBeginning fit attempt 34 of at maximum 50 extra triesBeginning fit attempt 35 of at maximum 50 extra triesBeginning fit attempt 36 of at maximum 50 extra triesBeginning fit attempt 37 of at maximum 50 extra triesBeginning fit attempt 38 of at maximum 50 extra triesBeginning fit attempt 39 of at maximum 50 extra triesBeginning fit attempt 40 of at maximum 50 extra triesBeginning fit attempt 41 of at maximum 50 extra triesBeginning fit attempt 42 of at maximum 50 extra triesBeginning fit attempt 43 of at maximum 50 extra triesBeginning fit attempt 44 of at maximum 50 extra triesBeginning fit attempt 45 of at maximum 50 extra triesBeginning fit attempt 46 of at maximum 50 extra triesBeginning fit attempt 47 of at maximum 50 extra triesBeginning fit attempt 48 of at maximum 50 extra triesBeginning fit attempt 49 of at maximum 50 extra triesBeginning fit attempt 50 of at maximum 50 extra tries                                                     
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
#>  [1] "mix3.weights[1,2]" "mix3.weights[1,3]" "vscl1"             "vscl2"            
#>  [5] "vscl3"             "vscl4"             "vscl5"             "vscl6"            
#>  [9] "class1.M[1,7]"     "class1.M[1,8]"     "class1.M[1,9]"     "class2.M[1,7]"    
#> [13] "class2.M[1,8]"     "class2.M[1,9]"     "class3.M[1,7]"     "class3.M[1,8]"    
#> [17] "class3.M[1,9]"
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
