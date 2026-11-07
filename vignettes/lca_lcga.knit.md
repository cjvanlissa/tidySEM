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
#> MxComputeSimAnnealing(tsallis1996) evaluations 311 fit 12175.6 change -3857MxComputeSimAnnealing(tsallis1996) evaluations 831 fit 12175.6 change 1.118e+04MxComputeSimAnnealing(tsallis1996) evaluations 1355 fit -1176.2 change 0       MxComputeSimAnnealing(tsallis1996) evaluations 1876 fit -1176.2 change 0MxComputeSimAnnealing(tsallis1996) evaluations 2404 fit 12175.6 change 4790MxComputeSimAnnealing(tsallis1996) evaluations 2928 fit -1176.2 change 0   MxComputeSimAnnealing(tsallis1996) evaluations 3448 fit -1176.2 change 669.6MxComputeSimAnnealing(tsallis1996) evaluations 3973 fit 11445.6 change 7274 MxComputeSimAnnealing(tsallis1996) evaluations 4498 fit 12237.7 change 0   MxComputeSimAnnealing(tsallis1996) evaluations 5019 fit 12237.7 change 1.804e+04MxComputeSimAnnealing(tsallis1996) evaluations 5538 fit 13004.4 change 0        MxComputeSimAnnealing(tsallis1996) evaluations 6058 fit 13082.6 change 659.1MxComputeSimAnnealing(tsallis1996) evaluations 6578 fit -6465.09 change 1252MxComputeSimAnnealing(tsallis1996) evaluations 7098 fit -4764.74 change -1.833e+04MxComputeSimAnnealing(tsallis1996) evaluations 7618 fit -7734.98 change -9.474    MxComputeSimAnnealing(tsallis1996) evaluations 8139 fit 511.856 change 1461   MxComputeSimAnnealing(tsallis1996) evaluations 8657 fit -7710.85 change -1986MxComputeSimAnnealing(tsallis1996) evaluations 9179 fit 14039.5 change 2.164e+04MxComputeSimAnnealing(tsallis1996) evaluations 9696 fit -6689.99 change 700.3   MxComputeSimAnnealing(tsallis1996) evaluations 10217 fit 14122.1 change 1.889e+04MxComputeSimAnnealing(tsallis1996) evaluations 10734 fit -6686.06 change -1761   MxComputeSimAnnealing(tsallis1996) evaluations 11249 fit -1574.31 change 5329 MxComputeSimAnnealing(tsallis1996) evaluations 11761 fit -7025.25 change 634.6MxComputeSimAnnealing(tsallis1996) evaluations 12275 fit -1573.75 change 6177 MxComputeSimAnnealing(tsallis1996) evaluations 12790 fit -7112.5 change 579.7MxComputeSimAnnealing(tsallis1996) evaluations 13303 fit -7750.81 change -11.3MxComputeSimAnnealing(tsallis1996) evaluations 13811 fit -7750.83 change -0.9292MxComputeSimAnnealing(tsallis1996) evaluations 14321 fit -7749.38 change -0.1985MxComputeSimAnnealing(tsallis1996) evaluations 14829 fit -3693.5 change 3953    MxComputeSimAnnealing(tsallis1996) evaluations 15337 fit -7750.41 change -5.122MxComputeSimAnnealing(tsallis1996) evaluations 15845 fit -6702.73 change -470  MxComputeSimAnnealing(tsallis1996) evaluations 16349 fit 515.289 change 6950                                                                             
#> MxComputeSimAnnealing(tsallis1996) evaluations 155 fit -6193.33 change 0MxComputeSimAnnealing(tsallis1996) evaluations 501 fit 9147.45 change -5199MxComputeSimAnnealing(tsallis1996) evaluations 847 fit 586.668 change -359.3MxComputeSimAnnealing(tsallis1996) evaluations 1191 fit -6193.33 change 0   MxComputeSimAnnealing(tsallis1996) evaluations 1536 fit -284.892 change 5739MxComputeSimAnnealing(tsallis1996) evaluations 1883 fit 9905.47 change 1.328e+04MxComputeSimAnnealing(tsallis1996) evaluations 2229 fit -6193.33 change 21.43   MxComputeSimAnnealing(tsallis1996) evaluations 2580 fit 4458.25 change 6813  MxComputeSimAnnealing(tsallis1996) evaluations 2922 fit 586.668 change 0   MxComputeSimAnnealing(tsallis1996) evaluations 3269 fit -6024.25 change 0MxComputeSimAnnealing(tsallis1996) evaluations 3607 fit -6024.25 change 169.1MxComputeSimAnnealing(tsallis1996) evaluations 3953 fit -2346.85 change 277.6MxComputeSimAnnealing(tsallis1996) evaluations 4298 fit 586.668 change 3658  MxComputeSimAnnealing(tsallis1996) evaluations 4642 fit -6193.33 change 1292MxComputeSimAnnealing(tsallis1996) evaluations 4988 fit 2410.29 change 9551 MxComputeSimAnnealing(tsallis1996) evaluations 5332 fit -4767.31 change -4542MxComputeSimAnnealing(tsallis1996) evaluations 5677 fit 1061.87 change -5.414e-10MxComputeSimAnnealing(tsallis1996) evaluations 6024 fit -1853.6 change 4568      MxComputeSimAnnealing(tsallis1996) evaluations 6369 fit -3216.33 change 1333MxComputeSimAnnealing(tsallis1996) evaluations 6715 fit -5766.39 change -7652MxComputeSimAnnealing(tsallis1996) evaluations 7059 fit -8229.3 change 25    MxComputeSimAnnealing(tsallis1996) evaluations 7402 fit -4222.12 change 1004MxComputeSimAnnealing(tsallis1996) evaluations 7747 fit -2211.74 change 2025MxComputeSimAnnealing(tsallis1996) evaluations 8093 fit -5746.56 change 0   MxComputeSimAnnealing(tsallis1996) evaluations 8439 fit -6080.23 change 2189MxComputeSimAnnealing(tsallis1996) evaluations 8787 fit 5645.97 change 550.8MxComputeSimAnnealing(tsallis1996) evaluations 9132 fit -6952.87 change -155MxComputeSimAnnealing(tsallis1996) evaluations 9480 fit 1245.07 change 5173 MxComputeSimAnnealing(tsallis1996) evaluations 9825 fit 4982.33 change 1407MxComputeSimAnnealing(tsallis1996) evaluations 10173 fit -6942.46 change 402.1MxComputeSimAnnealing(tsallis1996) evaluations 10518 fit -8285.13 change -4360MxComputeSimAnnealing(tsallis1996) evaluations 10864 fit -6474.53 change 1579 MxComputeSimAnnealing(tsallis1996) evaluations 11208 fit -6945.91 change 1390MxComputeSimAnnealing(tsallis1996) evaluations 11549 fit -7113.78 change -70.38MxComputeSimAnnealing(tsallis1996) evaluations 11892 fit 10484.4 change 1.862e+04MxComputeSimAnnealing(tsallis1996) evaluations 12238 fit 4731.42 change 209.7    MxComputeSimAnnealing(tsallis1996) evaluations 12581 fit -5630.13 change 0   MxComputeSimAnnealing(tsallis1996) evaluations 12923 fit -7018.63 change 1324MxComputeSimAnnealing(tsallis1996) evaluations 13265 fit -7036.56 change 483.6MxComputeSimAnnealing(tsallis1996) evaluations 13612 fit -1699.18 change 6213 MxComputeSimAnnealing(tsallis1996) evaluations 13955 fit 5037.09 change 1.154e+04MxComputeSimAnnealing(tsallis1996) evaluations 14299 fit -8335.52 change -2782   MxComputeSimAnnealing(tsallis1996) evaluations 14644 fit -8345.71 change -290.3MxComputeSimAnnealing(tsallis1996) evaluations 14988 fit -4494.57 change 1900  MxComputeSimAnnealing(tsallis1996) evaluations 15328 fit -8345.79 change -22.06MxComputeSimAnnealing(tsallis1996) evaluations 15671 fit -6280.3 change -9666  MxComputeSimAnnealing(tsallis1996) evaluations 16014 fit -8218.41 change -1.351e+04MxComputeSimAnnealing(tsallis1996) evaluations 16356 fit -5572.63 change 2244      MxComputeSimAnnealing(tsallis1996) evaluations 16701 fit -8346.4 change -4.178MxComputeSimAnnealing(tsallis1996) evaluations 17040 fit -8345.97 change -1255MxComputeSimAnnealing(tsallis1996) evaluations 17384 fit -3531.77 change 130.6MxComputeSimAnnealing(tsallis1996) evaluations 17726 fit -8065.35 change 281.3MxComputeSimAnnealing(tsallis1996) evaluations 18070 fit -8323.44 change -1.354e+04MxComputeSimAnnealing(tsallis1996) evaluations 18410 fit -8283.71 change -1.127e+04MxComputeSimAnnealing(tsallis1996) evaluations 18751 fit -5732.32 change -1.088e+04MxComputeSimAnnealing(tsallis1996) evaluations 19092 fit -8343.69 change -2301     MxComputeSimAnnealing(tsallis1996) evaluations 19433 fit -8346.3 change -2760 MxComputeSimAnnealing(tsallis1996) evaluations 19775 fit -7055.45 change 0   MxComputeSimAnnealing(tsallis1996) evaluations 20115 fit -8345.78 change -42.05MxComputeSimAnnealing(tsallis1996) evaluations 20452 fit -7818.91 change -2247 MxComputeSimAnnealing(tsallis1996) evaluations 20795 fit -8327.03 change -1270MxComputeSimAnnealing(tsallis1996) evaluations 21136 fit -8317.81 change 29.09MxComputeSimAnnealing(tsallis1996) evaluations 21477 fit -8346.97 change -0.2455                                                                                
#> MxComputeSimAnnealing(tsallis1996) evaluations 37 fit -3788.3 change -1.948e+04MxComputeSimAnnealing(tsallis1996) evaluations 297 fit -7023.02 change 0       MxComputeSimAnnealing(tsallis1996) evaluations 558 fit 11789.2 change 2070MxComputeSimAnnealing(tsallis1996) evaluations 817 fit -7442.24 change -3654MxComputeSimAnnealing(tsallis1996) evaluations 1075 fit -7420.42 change -397.4MxComputeSimAnnealing(tsallis1996) evaluations 1336 fit 2228.53 change -1.287e+04MxComputeSimAnnealing(tsallis1996) evaluations 1595 fit -7442.24 change 0        MxComputeSimAnnealing(tsallis1996) evaluations 1855 fit -2518.78 change 4902MxComputeSimAnnealing(tsallis1996) evaluations 2116 fit -3788.3 change -1.231e+04MxComputeSimAnnealing(tsallis1996) evaluations 2372 fit -7442.24 change 0        MxComputeSimAnnealing(tsallis1996) evaluations 2631 fit -7420.42 change 0MxComputeSimAnnealing(tsallis1996) evaluations 2889 fit 1099.2 change -4022MxComputeSimAnnealing(tsallis1996) evaluations 3147 fit -3788.3 change 0   MxComputeSimAnnealing(tsallis1996) evaluations 3406 fit -7420.42 change -397.4MxComputeSimAnnealing(tsallis1996) evaluations 3665 fit 311.814 change -1996  MxComputeSimAnnealing(tsallis1996) evaluations 3925 fit -7442.24 change -3654MxComputeSimAnnealing(tsallis1996) evaluations 4186 fit -3474.95 change 3945 MxComputeSimAnnealing(tsallis1996) evaluations 4444 fit 1640.73 change 1183 MxComputeSimAnnealing(tsallis1996) evaluations 4701 fit -3741.74 change 0  MxComputeSimAnnealing(tsallis1996) evaluations 4959 fit -7013.12 change 0MxComputeSimAnnealing(tsallis1996) evaluations 5216 fit -5409.31 change 2972MxComputeSimAnnealing(tsallis1996) evaluations 5475 fit -3467.68 change -714.6MxComputeSimAnnealing(tsallis1996) evaluations 5734 fit -7013.12 change 453.1 MxComputeSimAnnealing(tsallis1996) evaluations 5993 fit -8328.22 change -3774MxComputeSimAnnealing(tsallis1996) evaluations 6251 fit -1697.16 change 3819 MxComputeSimAnnealing(tsallis1996) evaluations 6510 fit -7466.21 change 0   MxComputeSimAnnealing(tsallis1996) evaluations 6770 fit 2724.98 change 7065MxComputeSimAnnealing(tsallis1996) evaluations 7029 fit -3151.46 change -2786MxComputeSimAnnealing(tsallis1996) evaluations 7288 fit -8254.47 change -788.3MxComputeSimAnnealing(tsallis1996) evaluations 7541 fit -8248.99 change -1175 MxComputeSimAnnealing(tsallis1996) evaluations 7795 fit -7755.73 change -665.7MxComputeSimAnnealing(tsallis1996) evaluations 8046 fit -7090.06 change 0     MxComputeSimAnnealing(tsallis1996) evaluations 8298 fit -8480.46 change -1396MxComputeSimAnnealing(tsallis1996) evaluations 8560 fit -6018.76 change 2409 MxComputeSimAnnealing(tsallis1996) evaluations 8817 fit -2766.2 change 0    MxComputeSimAnnealing(tsallis1996) evaluations 9074 fit -7107.9 change 0MxComputeSimAnnealing(tsallis1996) evaluations 9333 fit 9242.92 change 1.676e+04MxComputeSimAnnealing(tsallis1996) evaluations 9591 fit -8457.43 change -7381   MxComputeSimAnnealing(tsallis1996) evaluations 9849 fit -7601.96 change 200.2MxComputeSimAnnealing(tsallis1996) evaluations 10105 fit -7549.3 change -475.1MxComputeSimAnnealing(tsallis1996) evaluations 10363 fit 1803.76 change -195.9MxComputeSimAnnealing(tsallis1996) evaluations 10622 fit -2379.32 change 6062 MxComputeSimAnnealing(tsallis1996) evaluations 10879 fit -7197.48 change 1181MxComputeSimAnnealing(tsallis1996) evaluations 11137 fit -2338.02 change 5291MxComputeSimAnnealing(tsallis1996) evaluations 11396 fit 13725.7 change 2.194e+04MxComputeSimAnnealing(tsallis1996) evaluations 11652 fit -1384.27 change 7126    MxComputeSimAnnealing(tsallis1996) evaluations 11909 fit -7372.06 change 9.823e-10MxComputeSimAnnealing(tsallis1996) evaluations 12165 fit -7690.71 change 0        MxComputeSimAnnealing(tsallis1996) evaluations 12421 fit -6787.7 change -1574MxComputeSimAnnealing(tsallis1996) evaluations 12677 fit -5242.28 change 3253MxComputeSimAnnealing(tsallis1996) evaluations 12932 fit -6193.54 change 745 MxComputeSimAnnealing(tsallis1996) evaluations 13189 fit -7345.65 change 205.8MxComputeSimAnnealing(tsallis1996) evaluations 13444 fit -8518.18 change -1175MxComputeSimAnnealing(tsallis1996) evaluations 13701 fit -8534.32 change -437.1MxComputeSimAnnealing(tsallis1996) evaluations 13957 fit -449.32 change 3018   MxComputeSimAnnealing(tsallis1996) evaluations 14212 fit -6478.84 change 788.4MxComputeSimAnnealing(tsallis1996) evaluations 14468 fit -8500.98 change -1051MxComputeSimAnnealing(tsallis1996) evaluations 14725 fit -8520.34 change -26.63MxComputeSimAnnealing(tsallis1996) evaluations 14981 fit -8541.51 change 0.1728MxComputeSimAnnealing(tsallis1996) evaluations 15234 fit -7199.26 change 1324  MxComputeSimAnnealing(tsallis1996) evaluations 15491 fit -6086.23 change -3426MxComputeSimAnnealing(tsallis1996) evaluations 15744 fit -3932.49 change 2284 MxComputeSimAnnealing(tsallis1996) evaluations 16000 fit -8542.36 change -8101MxComputeSimAnnealing(tsallis1996) evaluations 16257 fit -7493.26 change 745.5MxComputeSimAnnealing(tsallis1996) evaluations 16510 fit -7887.39 change 648.7MxComputeSimAnnealing(tsallis1996) evaluations 16766 fit -6680.21 change 1860 MxComputeSimAnnealing(tsallis1996) evaluations 17026 fit -561.891 change 7909MxComputeSimAnnealing(tsallis1996) evaluations 17280 fit -555.426 change 7982MxComputeSimAnnealing(tsallis1996) evaluations 17536 fit -7417.12 change 21.87MxComputeSimAnnealing(tsallis1996) evaluations 17789 fit -8551.71 change -1105MxComputeSimAnnealing(tsallis1996) evaluations 18042 fit -7507.28 change 1042 MxComputeSimAnnealing(tsallis1996) evaluations 18296 fit -7883.73 change 485.7MxComputeSimAnnealing(tsallis1996) evaluations 18552 fit -8552.5 change -0.5787MxComputeSimAnnealing(tsallis1996) evaluations 18806 fit -249.961 change 6860  MxComputeSimAnnealing(tsallis1996) evaluations 19061 fit -8527.83 change -54.16MxComputeSimAnnealing(tsallis1996) evaluations 19317 fit -563.631 change 7989  MxComputeSimAnnealing(tsallis1996) evaluations 19571 fit -7408.23 change 0   MxComputeSimAnnealing(tsallis1996) evaluations 19827 fit -7466.51 change 1069MxComputeSimAnnealing(tsallis1996) evaluations 20082 fit -8511.34 change 27.55MxComputeSimAnnealing(tsallis1996) evaluations 20337 fit -8529.69 change -8274MxComputeSimAnnealing(tsallis1996) evaluations 20592 fit -6225.53 change 2329 MxComputeSimAnnealing(tsallis1996) evaluations 20849 fit -1724.75 change -1417MxComputeSimAnnealing(tsallis1996) evaluations 21105 fit -7421.4 change 0     MxComputeSimAnnealing(tsallis1996) evaluations 21362 fit -8554.47 change -596.9MxComputeSimAnnealing(tsallis1996) evaluations 21618 fit -8537.72 change -213.5MxComputeSimAnnealing(tsallis1996) evaluations 21868 fit -8554.64 change -48.79MxComputeSimAnnealing(tsallis1996) evaluations 22127 fit -1004.45 change 7317  MxComputeSimAnnealing(tsallis1996) evaluations 22383 fit -3697.88 change 889.2MxComputeSimAnnealing(tsallis1996) evaluations 22638 fit -8550.97 change -915.7MxComputeSimAnnealing(tsallis1996) evaluations 22893 fit -7450.24 change 1.523 MxComputeSimAnnealing(tsallis1996) evaluations 23150 fit -778.978 change 7773 MxComputeSimAnnealing(tsallis1996) evaluations 23407 fit -8554.64 change -2761MxComputeSimAnnealing(tsallis1996) evaluations 23662 fit -354.315 change 1685 MxComputeSimAnnealing(tsallis1996) evaluations 23918 fit -8549.43 change -1127MxComputeSimAnnealing(tsallis1996) evaluations 24172 fit -7601.91 change 917.3MxComputeSimAnnealing(tsallis1996) evaluations 24430 fit -8555.72 change -671.3MxComputeSimAnnealing(tsallis1996) evaluations 24687 fit -8343.2 change 205.1  MxComputeSimAnnealing(tsallis1996) evaluations 24941 fit -6073.15 change -1384MxComputeSimAnnealing(tsallis1996) evaluations 25199 fit -7424.56 change 1131 MxComputeSimAnnealing(tsallis1996) evaluations 25453 fit -7485.61 change -57.91MxComputeSimAnnealing(tsallis1996) evaluations 25708 fit -8556.28 change -0.06952MxComputeSimAnnealing(tsallis1996) evaluations 25965 fit -8418.47 change -360.7  MxComputeSimAnnealing(tsallis1996) evaluations 26220 fit -8498.69 change -2.136e+04MxComputeSimAnnealing(tsallis1996) evaluations 26476 fit -274.907 change 8273      MxComputeSimAnnealing(tsallis1996) evaluations 26733 fit -7597.58 change -172.8                                                                               
#> MxComputeSimAnnealing(tsallis1996) evaluations 162 fit 26037.5 change 1.3e+04MxComputeSimAnnealing(tsallis1996) evaluations 369 fit -7295.31 change 0     MxComputeSimAnnealing(tsallis1996) evaluations 576 fit -7465.85 change 0MxComputeSimAnnealing(tsallis1996) evaluations 784 fit -2647.78 change -217.5MxComputeSimAnnealing(tsallis1996) evaluations 993 fit -7295.31 change -1.344e+04MxComputeSimAnnealing(tsallis1996) evaluations 1199 fit -7465.85 change 359.8    MxComputeSimAnnealing(tsallis1996) evaluations 1405 fit -7367.16 change -339.9MxComputeSimAnnealing(tsallis1996) evaluations 1611 fit -6036.98 change -9483 MxComputeSimAnnealing(tsallis1996) evaluations 1819 fit -7295.31 change 0    MxComputeSimAnnealing(tsallis1996) evaluations 2027 fit -7027.27 change 438.6MxComputeSimAnnealing(tsallis1996) evaluations 2233 fit -5782.61 change 1585 MxComputeSimAnnealing(tsallis1996) evaluations 2439 fit -163.683 change -1112MxComputeSimAnnealing(tsallis1996) evaluations 2646 fit -7825.65 change -530.3MxComputeSimAnnealing(tsallis1996) evaluations 2853 fit -7027.27 change 0     MxComputeSimAnnealing(tsallis1996) evaluations 3058 fit -6796.02 change 571.1MxComputeSimAnnealing(tsallis1996) evaluations 3265 fit -3134.23 change -4033MxComputeSimAnnealing(tsallis1996) evaluations 3470 fit -7295.31 change 0    MxComputeSimAnnealing(tsallis1996) evaluations 3677 fit -7027.27 change 438.6MxComputeSimAnnealing(tsallis1996) evaluations 3885 fit -5456.02 change -442.9MxComputeSimAnnealing(tsallis1996) evaluations 4095 fit -7295.31 change 0     MxComputeSimAnnealing(tsallis1996) evaluations 4302 fit -7027.27 change 438.6MxComputeSimAnnealing(tsallis1996) evaluations 4507 fit -7367.16 change 0    MxComputeSimAnnealing(tsallis1996) evaluations 4714 fit -3856.92 change -4492MxComputeSimAnnealing(tsallis1996) evaluations 4922 fit -7825.65 change 0    MxComputeSimAnnealing(tsallis1996) evaluations 5129 fit -7027.27 change 0MxComputeSimAnnealing(tsallis1996) evaluations 5337 fit -2828.48 change 5312MxComputeSimAnnealing(tsallis1996) evaluations 5543 fit -7295.31 change -3271MxComputeSimAnnealing(tsallis1996) evaluations 5747 fit -8074.13 change -248.5MxComputeSimAnnealing(tsallis1996) evaluations 5954 fit -7027.27 change 62.17 MxComputeSimAnnealing(tsallis1996) evaluations 6161 fit -6653.57 change 700.4MxComputeSimAnnealing(tsallis1996) evaluations 6369 fit -7295.31 change 0    MxComputeSimAnnealing(tsallis1996) evaluations 6574 fit -7465.85 change 359.8MxComputeSimAnnealing(tsallis1996) evaluations 6781 fit -7534.96 change -167.8MxComputeSimAnnealing(tsallis1996) evaluations 6990 fit 3838.03 change 1.22e+04MxComputeSimAnnealing(tsallis1996) evaluations 7198 fit -7822.93 change 0      MxComputeSimAnnealing(tsallis1996) evaluations 7404 fit -7015.74 change 591MxComputeSimAnnealing(tsallis1996) evaluations 7609 fit -5248.75 change -2623MxComputeSimAnnealing(tsallis1996) evaluations 7817 fit -4260.43 change -2846MxComputeSimAnnealing(tsallis1996) evaluations 8024 fit -7479.97 change 343  MxComputeSimAnnealing(tsallis1996) evaluations 8229 fit -7015.95 change 0  MxComputeSimAnnealing(tsallis1996) evaluations 8436 fit 15484.7 change 1.613e+04MxComputeSimAnnealing(tsallis1996) evaluations 8644 fit -7332.18 change 134.1   MxComputeSimAnnealing(tsallis1996) evaluations 8851 fit -7490.08 change 0    MxComputeSimAnnealing(tsallis1996) evaluations 9057 fit -7390.49 change 0MxComputeSimAnnealing(tsallis1996) evaluations 9265 fit -7665.65 change -1.298e+04MxComputeSimAnnealing(tsallis1996) evaluations 9472 fit -7792.07 change 0         MxComputeSimAnnealing(tsallis1996) evaluations 9677 fit -7010.84 change 479.4MxComputeSimAnnealing(tsallis1996) evaluations 9884 fit -6703.61 change 1724 MxComputeSimAnnealing(tsallis1996) evaluations 10093 fit -7331.19 change -1.783e+04MxComputeSimAnnealing(tsallis1996) evaluations 10297 fit -7771.93 change -0.0003639MxComputeSimAnnealing(tsallis1996) evaluations 10504 fit -8365.45 change -1198     MxComputeSimAnnealing(tsallis1996) evaluations 10711 fit 4900.9 change 1.334e+04MxComputeSimAnnealing(tsallis1996) evaluations 10918 fit -7317.38 change -8382  MxComputeSimAnnealing(tsallis1996) evaluations 11125 fit -8027.72 change -532.9MxComputeSimAnnealing(tsallis1996) evaluations 11331 fit -7471.88 change -21.02MxComputeSimAnnealing(tsallis1996) evaluations 11536 fit -601.115 change 7641  MxComputeSimAnnealing(tsallis1996) evaluations 11744 fit -7340.84 change 0.2644MxComputeSimAnnealing(tsallis1996) evaluations 11951 fit -7473.22 change 0     MxComputeSimAnnealing(tsallis1996) evaluations 12156 fit -8264.65 change -767MxComputeSimAnnealing(tsallis1996) evaluations 12361 fit -7213.27 change 1142MxComputeSimAnnealing(tsallis1996) evaluations 12566 fit -7406.44 change -7327MxComputeSimAnnealing(tsallis1996) evaluations 12771 fit -8249.25 change -927.5MxComputeSimAnnealing(tsallis1996) evaluations 12977 fit -7021.32 change 437.5 MxComputeSimAnnealing(tsallis1996) evaluations 13183 fit -8070.92 change -561.2MxComputeSimAnnealing(tsallis1996) evaluations 13390 fit -4626.46 change 2157  MxComputeSimAnnealing(tsallis1996) evaluations 13593 fit -8421.26 change -196.9MxComputeSimAnnealing(tsallis1996) evaluations 13797 fit -7843.43 change 608.6 MxComputeSimAnnealing(tsallis1996) evaluations 14003 fit -8452.26 change -1215MxComputeSimAnnealing(tsallis1996) evaluations 14209 fit -8401.81 change -1940MxComputeSimAnnealing(tsallis1996) evaluations 14413 fit -2795.69 change 5610 MxComputeSimAnnealing(tsallis1996) evaluations 14617 fit -7811.05 change -1.645e+04MxComputeSimAnnealing(tsallis1996) evaluations 14820 fit -7347.12 change 0         MxComputeSimAnnealing(tsallis1996) evaluations 15024 fit -7631.39 change 694.5MxComputeSimAnnealing(tsallis1996) evaluations 15230 fit -7616.11 change 883.8MxComputeSimAnnealing(tsallis1996) evaluations 15434 fit -8502.34 change -379.4MxComputeSimAnnealing(tsallis1996) evaluations 15640 fit -8030.4 change -3652  MxComputeSimAnnealing(tsallis1996) evaluations 15843 fit -8137.47 change -1228MxComputeSimAnnealing(tsallis1996) evaluations 16049 fit -8258.49 change -46.93MxComputeSimAnnealing(tsallis1996) evaluations 16253 fit -8499.19 change -406.1MxComputeSimAnnealing(tsallis1996) evaluations 16458 fit -5176.86 change 2619  MxComputeSimAnnealing(tsallis1996) evaluations 16665 fit -6407.2 change -1760MxComputeSimAnnealing(tsallis1996) evaluations 16868 fit -7220.65 change 903 MxComputeSimAnnealing(tsallis1996) evaluations 17072 fit -8539.12 change -44.53MxComputeSimAnnealing(tsallis1996) evaluations 17277 fit -7466.51 change 108.4 MxComputeSimAnnealing(tsallis1996) evaluations 17481 fit -8103.6 change 431.3 MxComputeSimAnnealing(tsallis1996) evaluations 17684 fit -8038.04 change 505.6MxComputeSimAnnealing(tsallis1996) evaluations 17888 fit -5810.76 change -3059MxComputeSimAnnealing(tsallis1996) evaluations 18093 fit -8377.74 change -1.112e+04MxComputeSimAnnealing(tsallis1996) evaluations 18294 fit -7189.38 change 0         MxComputeSimAnnealing(tsallis1996) evaluations 18501 fit -7577.39 change 951MxComputeSimAnnealing(tsallis1996) evaluations 18706 fit -7880.84 change 70.96MxComputeSimAnnealing(tsallis1996) evaluations 18910 fit -8526.06 change -4828MxComputeSimAnnealing(tsallis1996) evaluations 19116 fit 4884.14 change 1.114e+04MxComputeSimAnnealing(tsallis1996) evaluations 19324 fit -7564.51 change 886.8   MxComputeSimAnnealing(tsallis1996) evaluations 19530 fit -7855.55 change -393.9MxComputeSimAnnealing(tsallis1996) evaluations 19736 fit -8555.13 change -149.3MxComputeSimAnnealing(tsallis1996) evaluations 19940 fit 559.288 change 3136   MxComputeSimAnnealing(tsallis1996) evaluations 20144 fit -7147.32 change 1409MxComputeSimAnnealing(tsallis1996) evaluations 20351 fit -7548.81 change 1008MxComputeSimAnnealing(tsallis1996) evaluations 20555 fit -7870.95 change -391.4MxComputeSimAnnealing(tsallis1996) evaluations 20760 fit -8558.13 change -5291 MxComputeSimAnnealing(tsallis1996) evaluations 20964 fit -7917.16 change -5367MxComputeSimAnnealing(tsallis1996) evaluations 21170 fit -7201.8 change 469.7 MxComputeSimAnnealing(tsallis1996) evaluations 21374 fit -8525 change -7.437 MxComputeSimAnnealing(tsallis1996) evaluations 21581 fit -8560.3 change -10.97MxComputeSimAnnealing(tsallis1996) evaluations 21787 fit -5067.08 change 3464 MxComputeSimAnnealing(tsallis1996) evaluations 21993 fit -8560.96 change -1122MxComputeSimAnnealing(tsallis1996) evaluations 22199 fit -7540.33 change 412.5MxComputeSimAnnealing(tsallis1996) evaluations 22401 fit -7557.86 change 317.3MxComputeSimAnnealing(tsallis1996) evaluations 22605 fit -8461.01 change -942.5MxComputeSimAnnealing(tsallis1996) evaluations 22810 fit -8562.15 change 0.2975MxComputeSimAnnealing(tsallis1996) evaluations 23013 fit -7376.19 change -1001 MxComputeSimAnnealing(tsallis1996) evaluations 23219 fit -7564.57 change 993.6MxComputeSimAnnealing(tsallis1996) evaluations 23424 fit -7540.77 change 986.7MxComputeSimAnnealing(tsallis1996) evaluations 23628 fit -7507.48 change 1055 MxComputeSimAnnealing(tsallis1996) evaluations 23833 fit -8540.08 change 23.06MxComputeSimAnnealing(tsallis1996) evaluations 24037 fit -8522.31 change 40.96MxComputeSimAnnealing(tsallis1996) evaluations 24243 fit -8557.18 change -7790MxComputeSimAnnealing(tsallis1996) evaluations 24447 fit -7883.61 change 0    MxComputeSimAnnealing(tsallis1996) evaluations 24649 fit -7555.59 change 316.3MxComputeSimAnnealing(tsallis1996) evaluations 24855 fit -7890.47 change -380.8MxComputeSimAnnealing(tsallis1996) evaluations 25059 fit -8186.59 change 368.7 MxComputeSimAnnealing(tsallis1996) evaluations 25263 fit -5040.41 change 1071 MxComputeSimAnnealing(tsallis1996) evaluations 25468 fit -8563.73 change -582.7MxComputeSimAnnealing(tsallis1996) evaluations 25674 fit -8563.74 change -704.9MxComputeSimAnnealing(tsallis1996) evaluations 25879 fit -7514.61 change 883.5 MxComputeSimAnnealing(tsallis1996) evaluations 26084 fit 640.402 change 9204  MxComputeSimAnnealing(tsallis1996) evaluations 26289 fit -8562.41 change -6426MxComputeSimAnnealing(tsallis1996) evaluations 26493 fit -8509.13 change -18.62MxComputeSimAnnealing(tsallis1996) evaluations 26698 fit -8563.3 change -737.5 MxComputeSimAnnealing(tsallis1996) evaluations 26902 fit -8540.96 change -969.1MxComputeSimAnnealing(tsallis1996) evaluations 27108 fit -8564.72 change -0.1814MxComputeSimAnnealing(tsallis1996) evaluations 27314 fit -6315.98 change 2155   MxComputeSimAnnealing(tsallis1996) evaluations 27519 fit -7385.66 change -20.43MxComputeSimAnnealing(tsallis1996) evaluations 27723 fit -8062.66 change 233.1 MxComputeSimAnnealing(tsallis1996) evaluations 27928 fit -8552.33 change 12.32MxComputeSimAnnealing(tsallis1996) evaluations 28132 fit -7898.58 change 665.6MxComputeSimAnnealing(tsallis1996) evaluations 28337 fit -4987.56 change 3578 MxComputeSimAnnealing(tsallis1996) evaluations 28541 fit -8512.14 change -716.7MxComputeSimAnnealing(tsallis1996) evaluations 28745 fit -7433.41 change 1131  MxComputeSimAnnealing(tsallis1996) evaluations 28950 fit -7604.96 change 960.9MxComputeSimAnnealing(tsallis1996) evaluations 29154 fit -8216.9 change -695.2MxComputeSimAnnealing(tsallis1996) evaluations 29358 fit -5141.24 change 3424 MxComputeSimAnnealing(tsallis1996) evaluations 29561 fit -7433.08 change 1133MxComputeSimAnnealing(tsallis1996) evaluations 29766 fit -8566.46 change -234.6MxComputeSimAnnealing(tsallis1996) evaluations 29971 fit -8566.7 change -5.921 MxComputeSimAnnealing(tsallis1996) evaluations 30175 fit -8249.39 change 202.3MxComputeSimAnnealing(tsallis1996) evaluations 30377 fit -7534.5 change 119.2 MxComputeSimAnnealing(tsallis1996) evaluations 30581 fit -7883.57 change 683.9MxComputeSimAnnealing(tsallis1996) evaluations 30785 fit -8567.77 change -0.0328MxComputeSimAnnealing(tsallis1996) evaluations 30988 fit -8568.02 change -3.642 MxComputeSimAnnealing(tsallis1996) evaluations 31193 fit -7989.89 change -544.8MxComputeSimAnnealing(tsallis1996) evaluations 31395 fit -8512.53 change 56.67 MxComputeSimAnnealing(tsallis1996) evaluations 31599 fit -7728.33 change -368.6MxComputeSimAnnealing(tsallis1996) evaluations 31804 fit -8566.63 change -60.08MxComputeSimAnnealing(tsallis1996) evaluations 32009 fit -8569.18 change -3.069                                                                               
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
