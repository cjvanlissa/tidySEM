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
#> MxComputeSimAnnealing(tsallis1996) evaluations 364 fit 12175.6 change 0MxComputeSimAnnealing(tsallis1996) evaluations 901 fit -1176.2 change 0MxComputeSimAnnealing(tsallis1996) evaluations 1441 fit -3961.66 change -2328MxComputeSimAnnealing(tsallis1996) evaluations 1980 fit -1176.2 change 0     MxComputeSimAnnealing(tsallis1996) evaluations 2522 fit 12175.6 change 0MxComputeSimAnnealing(tsallis1996) evaluations 3063 fit -4102.94 change -1.196e+04MxComputeSimAnnealing(tsallis1996) evaluations 3602 fit 12175.6 change 0          MxComputeSimAnnealing(tsallis1996) evaluations 4141 fit -5229.97 change 2136MxComputeSimAnnealing(tsallis1996) evaluations 4684 fit -2644.87 change 4845MxComputeSimAnnealing(tsallis1996) evaluations 5222 fit -4902.6 change -2609MxComputeSimAnnealing(tsallis1996) evaluations 5763 fit -6984.99 change 700 MxComputeSimAnnealing(tsallis1996) evaluations 6304 fit 13082.6 change 1.473e+04MxComputeSimAnnealing(tsallis1996) evaluations 6840 fit -1435.72 change -1.499e+04MxComputeSimAnnealing(tsallis1996) evaluations 7375 fit -1451.75 change 0         MxComputeSimAnnealing(tsallis1996) evaluations 7909 fit -5902.46 change 1833MxComputeSimAnnealing(tsallis1996) evaluations 8446 fit -808.123 change -2195MxComputeSimAnnealing(tsallis1996) evaluations 8980 fit -1685.11 change 3082 MxComputeSimAnnealing(tsallis1996) evaluations 9516 fit 13442.7 change -575.2MxComputeSimAnnealing(tsallis1996) evaluations 10050 fit 14128 change 2.187e+04MxComputeSimAnnealing(tsallis1996) evaluations 10581 fit -7749.6 change -8.417 MxComputeSimAnnealing(tsallis1996) evaluations 11115 fit 14370.7 change 2.103e+04MxComputeSimAnnealing(tsallis1996) evaluations 11645 fit -7231.67 change -1.109e+04MxComputeSimAnnealing(tsallis1996) evaluations 12175 fit -6110.48 change 1039      MxComputeSimAnnealing(tsallis1996) evaluations 12709 fit 584.986 change 8178 MxComputeSimAnnealing(tsallis1996) evaluations 13240 fit -5664.24 change 2086MxComputeSimAnnealing(tsallis1996) evaluations 13767 fit -7750.76 change -160.9MxComputeSimAnnealing(tsallis1996) evaluations 14294 fit -6665.16 change 1085  MxComputeSimAnnealing(tsallis1996) evaluations 14823 fit -6721.03 change 1012MxComputeSimAnnealing(tsallis1996) evaluations 15351 fit -7692.26 change -546MxComputeSimAnnealing(tsallis1996) evaluations 15879 fit -7581.13 change -1086MxComputeSimAnnealing(tsallis1996) evaluations 16407 fit 14395.4 change 0                                                                              
#> MxComputeSimAnnealing(tsallis1996) evaluations 248 fit 7265.03 change -1.18e+04MxComputeSimAnnealing(tsallis1996) evaluations 603 fit 6554.33 change 489.6    MxComputeSimAnnealing(tsallis1996) evaluations 959 fit 4382.78 change 6129 MxComputeSimAnnealing(tsallis1996) evaluations 1315 fit -1267.73 change 4757MxComputeSimAnnealing(tsallis1996) evaluations 1671 fit -6024.25 change 0   MxComputeSimAnnealing(tsallis1996) evaluations 2027 fit -6024.25 change 0MxComputeSimAnnealing(tsallis1996) evaluations 2385 fit -6024.25 change 0MxComputeSimAnnealing(tsallis1996) evaluations 2744 fit -2943.16 change 3217MxComputeSimAnnealing(tsallis1996) evaluations 3105 fit -471.282 change -2871MxComputeSimAnnealing(tsallis1996) evaluations 3460 fit -3150.37 change 4861 MxComputeSimAnnealing(tsallis1996) evaluations 3818 fit -2670.46 change 4730MxComputeSimAnnealing(tsallis1996) evaluations 4172 fit 3739.68 change 9640 MxComputeSimAnnealing(tsallis1996) evaluations 4527 fit -6024.25 change 0  MxComputeSimAnnealing(tsallis1996) evaluations 4886 fit -6884.4 change -1713MxComputeSimAnnealing(tsallis1996) evaluations 5241 fit -7653.41 change -1629MxComputeSimAnnealing(tsallis1996) evaluations 5597 fit -6189.25 change 0    MxComputeSimAnnealing(tsallis1996) evaluations 5956 fit -7321.11 change -899MxComputeSimAnnealing(tsallis1996) evaluations 6311 fit -6422.1 change -0.0187MxComputeSimAnnealing(tsallis1996) evaluations 6668 fit -6437.69 change 0     MxComputeSimAnnealing(tsallis1996) evaluations 7025 fit -6438.46 change 0MxComputeSimAnnealing(tsallis1996) evaluations 7381 fit -8275.56 change -2285MxComputeSimAnnealing(tsallis1996) evaluations 7736 fit -5717.51 change 455.6MxComputeSimAnnealing(tsallis1996) evaluations 8092 fit -5746.56 change -1.139e+04MxComputeSimAnnealing(tsallis1996) evaluations 8446 fit -8285.04 change -2657     MxComputeSimAnnealing(tsallis1996) evaluations 8802 fit -5184.3 change 561.7 MxComputeSimAnnealing(tsallis1996) evaluations 9160 fit -8253.6 change -2802MxComputeSimAnnealing(tsallis1996) evaluations 9517 fit 5378.9 change 1.305e+04MxComputeSimAnnealing(tsallis1996) evaluations 9874 fit 4095.51 change 1.223e+04MxComputeSimAnnealing(tsallis1996) evaluations 10231 fit 4535.68 change 1.284e+04MxComputeSimAnnealing(tsallis1996) evaluations 10584 fit -3748.75 change 4139    MxComputeSimAnnealing(tsallis1996) evaluations 10939 fit 11602 change 1.987e+04MxComputeSimAnnealing(tsallis1996) evaluations 11292 fit -8304.88 change -44.18MxComputeSimAnnealing(tsallis1996) evaluations 11644 fit -4458.88 change 3839  MxComputeSimAnnealing(tsallis1996) evaluations 11998 fit 11081.9 change 1.942e+04MxComputeSimAnnealing(tsallis1996) evaluations 12353 fit -6442.63 change 225     MxComputeSimAnnealing(tsallis1996) evaluations 12707 fit -5013.11 change 2679MxComputeSimAnnealing(tsallis1996) evaluations 13060 fit -7018.81 change 1323MxComputeSimAnnealing(tsallis1996) evaluations 13413 fit -8338.03 change -1.332e+04MxComputeSimAnnealing(tsallis1996) evaluations 13769 fit -1269.33 change -6088     MxComputeSimAnnealing(tsallis1996) evaluations 14124 fit -8307.94 change -1110MxComputeSimAnnealing(tsallis1996) evaluations 14477 fit -8111.37 change 212.5MxComputeSimAnnealing(tsallis1996) evaluations 14829 fit -8338.38 change -1299MxComputeSimAnnealing(tsallis1996) evaluations 15182 fit -5560.31 change 0    MxComputeSimAnnealing(tsallis1996) evaluations 15533 fit -8341.31 change -886.7MxComputeSimAnnealing(tsallis1996) evaluations 15888 fit -8263.66 change -162.7MxComputeSimAnnealing(tsallis1996) evaluations 16241 fit -8345.72 change -294.3MxComputeSimAnnealing(tsallis1996) evaluations 16595 fit -7052.71 change 988.9 MxComputeSimAnnealing(tsallis1996) evaluations 16946 fit 4960.31 change 773.3 MxComputeSimAnnealing(tsallis1996) evaluations 17297 fit -4467.68 change 3879MxComputeSimAnnealing(tsallis1996) evaluations 17652 fit -8345.93 change -1023MxComputeSimAnnealing(tsallis1996) evaluations 18005 fit -5587.85 change 2758 MxComputeSimAnnealing(tsallis1996) evaluations 18357 fit -8013.26 change -6897MxComputeSimAnnealing(tsallis1996) evaluations 18711 fit -8340.61 change 2.912MxComputeSimAnnealing(tsallis1996) evaluations 19062 fit -7047.93 change 1297 MxComputeSimAnnealing(tsallis1996) evaluations 19414 fit -5586.25 change 2758MxComputeSimAnnealing(tsallis1996) evaluations 19766 fit -6360.49 change 1895MxComputeSimAnnealing(tsallis1996) evaluations 20116 fit -7427.4 change 918.4MxComputeSimAnnealing(tsallis1996) evaluations 20467 fit -8284.64 change 62.39MxComputeSimAnnealing(tsallis1996) evaluations 20817 fit 77.6068 change 8310  MxComputeSimAnnealing(tsallis1996) evaluations 21167 fit -8343.58 change -2774MxComputeSimAnnealing(tsallis1996) evaluations 21518 fit -8346.8 change -0.5115                                                                               
#> MxComputeSimAnnealing(tsallis1996) evaluations 142 fit -3788.3 change -1.588e+04MxComputeSimAnnealing(tsallis1996) evaluations 408 fit 13870.2 change 6811      MxComputeSimAnnealing(tsallis1996) evaluations 675 fit -7023.02 change 0  MxComputeSimAnnealing(tsallis1996) evaluations 943 fit -7442.24 change -3654MxComputeSimAnnealing(tsallis1996) evaluations 1212 fit -550.166 change -4952MxComputeSimAnnealing(tsallis1996) evaluations 1479 fit 2472.88 change -5151 MxComputeSimAnnealing(tsallis1996) evaluations 1746 fit -7023.02 change 0   MxComputeSimAnnealing(tsallis1996) evaluations 2013 fit -3788.3 change 0 MxComputeSimAnnealing(tsallis1996) evaluations 2282 fit 889.307 change -2601MxComputeSimAnnealing(tsallis1996) evaluations 2546 fit -7420.42 change 0   MxComputeSimAnnealing(tsallis1996) evaluations 2814 fit -7442.24 change 0MxComputeSimAnnealing(tsallis1996) evaluations 3081 fit 5778.59 change 2365MxComputeSimAnnealing(tsallis1996) evaluations 3347 fit 1526.75 change 8484MxComputeSimAnnealing(tsallis1996) evaluations 3615 fit -7023.02 change 0  MxComputeSimAnnealing(tsallis1996) evaluations 3882 fit -3788.3 change 0 MxComputeSimAnnealing(tsallis1996) evaluations 4151 fit -2480.93 change -1955MxComputeSimAnnealing(tsallis1996) evaluations 4417 fit -8464.57 change -1044MxComputeSimAnnealing(tsallis1996) evaluations 4683 fit -7466.21 change 0    MxComputeSimAnnealing(tsallis1996) evaluations 4951 fit -3741.74 change 1331MxComputeSimAnnealing(tsallis1996) evaluations 5216 fit -5409.31 change 2972MxComputeSimAnnealing(tsallis1996) evaluations 5481 fit -7466.21 change 0   MxComputeSimAnnealing(tsallis1996) evaluations 5749 fit -8465.77 change -8112MxComputeSimAnnealing(tsallis1996) evaluations 6016 fit 1289.14 change 8365  MxComputeSimAnnealing(tsallis1996) evaluations 6284 fit -7423.49 change 0  MxComputeSimAnnealing(tsallis1996) evaluations 6553 fit -7076.32 change 389.9MxComputeSimAnnealing(tsallis1996) evaluations 6819 fit 12623.3 change 7624  MxComputeSimAnnealing(tsallis1996) evaluations 7084 fit -5778.6 change 1645MxComputeSimAnnealing(tsallis1996) evaluations 7352 fit -7076.32 change 0  MxComputeSimAnnealing(tsallis1996) evaluations 7618 fit -3490.3 change 539.5MxComputeSimAnnealing(tsallis1996) evaluations 7884 fit -2192.1 change 1696 MxComputeSimAnnealing(tsallis1996) evaluations 8151 fit -7084.2 change 0   MxComputeSimAnnealing(tsallis1996) evaluations 8419 fit -7566.47 change -4104MxComputeSimAnnealing(tsallis1996) evaluations 8686 fit -5290.58 change -2793MxComputeSimAnnealing(tsallis1996) evaluations 8952 fit -7553.13 change 0    MxComputeSimAnnealing(tsallis1996) evaluations 9220 fit -7107.9 change 1022MxComputeSimAnnealing(tsallis1996) evaluations 9486 fit -8467.55 change -1.869e+04MxComputeSimAnnealing(tsallis1996) evaluations 9751 fit -7700.68 change -161.1    MxComputeSimAnnealing(tsallis1996) evaluations 10017 fit -7571.6 change 0     MxComputeSimAnnealing(tsallis1996) evaluations 10284 fit -8503.37 change -1260MxComputeSimAnnealing(tsallis1996) evaluations 10548 fit -7548.22 change 0    MxComputeSimAnnealing(tsallis1996) evaluations 10814 fit -7588.73 change 0MxComputeSimAnnealing(tsallis1996) evaluations 11083 fit -2166.65 change 5459MxComputeSimAnnealing(tsallis1996) evaluations 11348 fit -6361.06 change -933.9MxComputeSimAnnealing(tsallis1996) evaluations 11616 fit -7351.73 change 1157  MxComputeSimAnnealing(tsallis1996) evaluations 11882 fit -1940.57 change -436.4MxComputeSimAnnealing(tsallis1996) evaluations 12147 fit 15659.2 change 2.346e+04MxComputeSimAnnealing(tsallis1996) evaluations 12412 fit -7329.2 change 216.2    MxComputeSimAnnealing(tsallis1996) evaluations 12678 fit -8512.7 change -3270MxComputeSimAnnealing(tsallis1996) evaluations 12944 fit -8484.06 change 14.95MxComputeSimAnnealing(tsallis1996) evaluations 13210 fit -7345.65 change 1176 MxComputeSimAnnealing(tsallis1996) evaluations 13473 fit -8454.19 change -1.83e+04MxComputeSimAnnealing(tsallis1996) evaluations 13739 fit -7806.01 change 0        MxComputeSimAnnealing(tsallis1996) evaluations 14004 fit -8534.58 change 0.137MxComputeSimAnnealing(tsallis1996) evaluations 14270 fit -7208.77 change 1296 MxComputeSimAnnealing(tsallis1996) evaluations 14536 fit -7842.27 change -351.2MxComputeSimAnnealing(tsallis1996) evaluations 14803 fit -7423.37 change -6190 MxComputeSimAnnealing(tsallis1996) evaluations 15066 fit 1953.64 change 7948  MxComputeSimAnnealing(tsallis1996) evaluations 15330 fit -8534.9 change 7.96MxComputeSimAnnealing(tsallis1996) evaluations 15596 fit -8386.62 change -302.4MxComputeSimAnnealing(tsallis1996) evaluations 15860 fit -7949.06 change -72.9 MxComputeSimAnnealing(tsallis1996) evaluations 16125 fit -467.624 change 5394 MxComputeSimAnnealing(tsallis1996) evaluations 16390 fit 4533.75 change 1.308e+04MxComputeSimAnnealing(tsallis1996) evaluations 16654 fit -7440.81 change -15.35  MxComputeSimAnnealing(tsallis1996) evaluations 16922 fit -8539.72 change 7.39  MxComputeSimAnnealing(tsallis1996) evaluations 17189 fit 929.681 change 5300 MxComputeSimAnnealing(tsallis1996) evaluations 17454 fit -7417.09 change 869.8MxComputeSimAnnealing(tsallis1996) evaluations 17719 fit -798.326 change -1641MxComputeSimAnnealing(tsallis1996) evaluations 17983 fit -8084.64 change 461.4MxComputeSimAnnealing(tsallis1996) evaluations 18247 fit -7416.98 change -6618MxComputeSimAnnealing(tsallis1996) evaluations 18515 fit -1800.27 change 5019 MxComputeSimAnnealing(tsallis1996) evaluations 18777 fit -7450.65 change 1102MxComputeSimAnnealing(tsallis1996) evaluations 19042 fit -7108.23 change -1783MxComputeSimAnnealing(tsallis1996) evaluations 19308 fit -8088.56 change 464.6MxComputeSimAnnealing(tsallis1996) evaluations 19572 fit -8407.94 change -999.7MxComputeSimAnnealing(tsallis1996) evaluations 19837 fit -4499.06 change -6385 MxComputeSimAnnealing(tsallis1996) evaluations 20101 fit -7885.59 change -431.2MxComputeSimAnnealing(tsallis1996) evaluations 20365 fit -8530.42 change -685.7MxComputeSimAnnealing(tsallis1996) evaluations 20628 fit -7890.52 change 646   MxComputeSimAnnealing(tsallis1996) evaluations 20894 fit -8482.21 change -95.65MxComputeSimAnnealing(tsallis1996) evaluations 21159 fit 475.946 change 4189   MxComputeSimAnnealing(tsallis1996) evaluations 21422 fit -7495.05 change 1060MxComputeSimAnnealing(tsallis1996) evaluations 21687 fit -7960.88 change -116MxComputeSimAnnealing(tsallis1996) evaluations 21951 fit -8371.32 change -477.8MxComputeSimAnnealing(tsallis1996) evaluations 22218 fit -7425.32 change 806.3 MxComputeSimAnnealing(tsallis1996) evaluations 22481 fit 3045.17 change 7238  MxComputeSimAnnealing(tsallis1996) evaluations 22745 fit -8548.29 change -1092MxComputeSimAnnealing(tsallis1996) evaluations 23011 fit -486.712 change 7781 MxComputeSimAnnealing(tsallis1996) evaluations 23274 fit -7936.16 change 619.4MxComputeSimAnnealing(tsallis1996) evaluations 23537 fit -8555.76 change -5.126MxComputeSimAnnealing(tsallis1996) evaluations 23799 fit -7894.05 change 659.2 MxComputeSimAnnealing(tsallis1996) evaluations 24062 fit -8198.5 change -7863 MxComputeSimAnnealing(tsallis1996) evaluations 24327 fit -8410.98 change 144.9MxComputeSimAnnealing(tsallis1996) evaluations 24592 fit -8546.23 change -1123MxComputeSimAnnealing(tsallis1996) evaluations 24856 fit -2553.14 change 5903 MxComputeSimAnnealing(tsallis1996) evaluations 25120 fit -7962.12 change 593.9MxComputeSimAnnealing(tsallis1996) evaluations 25383 fit -8553.11 change -3344MxComputeSimAnnealing(tsallis1996) evaluations 25647 fit -8555.91 change -553.4MxComputeSimAnnealing(tsallis1996) evaluations 25911 fit -7569.22 change 986.5 MxComputeSimAnnealing(tsallis1996) evaluations 26172 fit -8411.92 change -524.3MxComputeSimAnnealing(tsallis1996) evaluations 26437 fit -7759.7 change 628.8  MxComputeSimAnnealing(tsallis1996) evaluations 26704 fit -8430.98 change 125.4                                                                              
#> MxComputeSimAnnealing(tsallis1996) evaluations 18 fit -7295.31 change -1.92e+04MxComputeSimAnnealing(tsallis1996) evaluations 230 fit -7367.16 change -339.9  MxComputeSimAnnealing(tsallis1996) evaluations 445 fit -7295.31 change 0     MxComputeSimAnnealing(tsallis1996) evaluations 658 fit -919.467 change 6448MxComputeSimAnnealing(tsallis1996) evaluations 871 fit -7825.65 change -530.3MxComputeSimAnnealing(tsallis1996) evaluations 1083 fit -3812.87 change 3554 MxComputeSimAnnealing(tsallis1996) evaluations 1296 fit -7825.65 change -530.3MxComputeSimAnnealing(tsallis1996) evaluations 1508 fit -4838.33 change 2529  MxComputeSimAnnealing(tsallis1996) evaluations 1721 fit -7825.65 change -530.3MxComputeSimAnnealing(tsallis1996) evaluations 1935 fit 3282.16 change 6249   MxComputeSimAnnealing(tsallis1996) evaluations 2148 fit -7825.65 change 0  MxComputeSimAnnealing(tsallis1996) evaluations 2360 fit 333.876 change 4429MxComputeSimAnnealing(tsallis1996) evaluations 2572 fit -7825.65 change 0  MxComputeSimAnnealing(tsallis1996) evaluations 2785 fit -3378.44 change 3636MxComputeSimAnnealing(tsallis1996) evaluations 2997 fit -7825.65 change 0   MxComputeSimAnnealing(tsallis1996) evaluations 3209 fit -2333.5 change 4074MxComputeSimAnnealing(tsallis1996) evaluations 3421 fit -7825.65 change -530.3MxComputeSimAnnealing(tsallis1996) evaluations 3634 fit -6596.4 change -597.4 MxComputeSimAnnealing(tsallis1996) evaluations 3848 fit -7825.65 change 0    MxComputeSimAnnealing(tsallis1996) evaluations 4064 fit 4789.51 change 9671MxComputeSimAnnealing(tsallis1996) evaluations 4277 fit -7027.27 change 438.6MxComputeSimAnnealing(tsallis1996) evaluations 4490 fit -137.433 change -2591MxComputeSimAnnealing(tsallis1996) evaluations 4701 fit -7465.85 change 0    MxComputeSimAnnealing(tsallis1996) evaluations 4918 fit -7295.31 change -2605MxComputeSimAnnealing(tsallis1996) evaluations 5128 fit -7027.27 change 0    MxComputeSimAnnealing(tsallis1996) evaluations 5343 fit -7295.31 change -9140MxComputeSimAnnealing(tsallis1996) evaluations 5555 fit -7367.16 change -339.9MxComputeSimAnnealing(tsallis1996) evaluations 5766 fit -7309.37 change -5821 MxComputeSimAnnealing(tsallis1996) evaluations 5978 fit -7027.27 change 0    MxComputeSimAnnealing(tsallis1996) evaluations 6192 fit -4181.41 change -1543MxComputeSimAnnealing(tsallis1996) evaluations 6404 fit -7027.27 change 0    MxComputeSimAnnealing(tsallis1996) evaluations 6618 fit -7295.31 change -4631MxComputeSimAnnealing(tsallis1996) evaluations 6830 fit -7367.16 change -339.9MxComputeSimAnnealing(tsallis1996) evaluations 7044 fit -7295.4 change 0      MxComputeSimAnnealing(tsallis1996) evaluations 7257 fit -7375.63 change 0MxComputeSimAnnealing(tsallis1996) evaluations 7470 fit -7291.86 change 0MxComputeSimAnnealing(tsallis1996) evaluations 7682 fit -7375.63 change 0MxComputeSimAnnealing(tsallis1996) evaluations 7896 fit -7822.93 change -531.1MxComputeSimAnnealing(tsallis1996) evaluations 8107 fit -7375.84 change 0     MxComputeSimAnnealing(tsallis1996) evaluations 8319 fit -7332.18 change 2.914e-07MxComputeSimAnnealing(tsallis1996) evaluations 8533 fit -254.141 change 7122     MxComputeSimAnnealing(tsallis1996) evaluations 8747 fit -7903.78 change -111.7MxComputeSimAnnealing(tsallis1996) evaluations 8959 fit -7490.19 change 52.1  MxComputeSimAnnealing(tsallis1996) evaluations 9171 fit -7792.07 change -459.8MxComputeSimAnnealing(tsallis1996) evaluations 9383 fit -8370.91 change -980.4MxComputeSimAnnealing(tsallis1996) evaluations 9596 fit -7792.22 change -459.8MxComputeSimAnnealing(tsallis1996) evaluations 9807 fit -7382.76 change 0     MxComputeSimAnnealing(tsallis1996) evaluations 10020 fit -7333.13 change 0MxComputeSimAnnealing(tsallis1996) evaluations 10231 fit -7385.01 change 0MxComputeSimAnnealing(tsallis1996) evaluations 10445 fit -7314.35 change 1127MxComputeSimAnnealing(tsallis1996) evaluations 10658 fit -6408.14 change 977.5MxComputeSimAnnealing(tsallis1996) evaluations 10871 fit -8262.16 change -946 MxComputeSimAnnealing(tsallis1996) evaluations 11083 fit -8442.77 change -991.9MxComputeSimAnnealing(tsallis1996) evaluations 11295 fit -7317.1 change 0      MxComputeSimAnnealing(tsallis1996) evaluations 11506 fit -7450.86 change 585.1MxComputeSimAnnealing(tsallis1996) evaluations 11719 fit -7989.6 change -649.3MxComputeSimAnnealing(tsallis1996) evaluations 11931 fit -7497.63 change 0    MxComputeSimAnnealing(tsallis1996) evaluations 12143 fit -7340.84 change -5402MxComputeSimAnnealing(tsallis1996) evaluations 12356 fit -7517.59 change 0    MxComputeSimAnnealing(tsallis1996) evaluations 12566 fit -7406.44 change -7327MxComputeSimAnnealing(tsallis1996) evaluations 12778 fit -7006.6 change 1013  MxComputeSimAnnealing(tsallis1996) evaluations 12993 fit -7316.1 change -3544MxComputeSimAnnealing(tsallis1996) evaluations 13204 fit -7029.4 change 412.7MxComputeSimAnnealing(tsallis1996) evaluations 13418 fit -7706.97 change -6471MxComputeSimAnnealing(tsallis1996) evaluations 13627 fit -8400.08 change -908.2MxComputeSimAnnealing(tsallis1996) evaluations 13840 fit -3760.71 change 3022  MxComputeSimAnnealing(tsallis1996) evaluations 14052 fit -8343.37 change -2.534MxComputeSimAnnealing(tsallis1996) evaluations 14265 fit -2306.86 change 6096  MxComputeSimAnnealing(tsallis1996) evaluations 14476 fit -7500.52 change 782.5MxComputeSimAnnealing(tsallis1996) evaluations 14687 fit -5145.69 change 3327 MxComputeSimAnnealing(tsallis1996) evaluations 14898 fit -7899.67 change 0   MxComputeSimAnnealing(tsallis1996) evaluations 15109 fit -8502.28 change -694.4MxComputeSimAnnealing(tsallis1996) evaluations 15321 fit -8399.72 change -1138 MxComputeSimAnnealing(tsallis1996) evaluations 15533 fit -8462.27 change -810.7MxComputeSimAnnealing(tsallis1996) evaluations 15745 fit -7219.56 change 1290  MxComputeSimAnnealing(tsallis1996) evaluations 15957 fit -7680.74 change 0   MxComputeSimAnnealing(tsallis1996) evaluations 16170 fit -7207.13 change 1313MxComputeSimAnnealing(tsallis1996) evaluations 16381 fit -8485.29 change -691.7MxComputeSimAnnealing(tsallis1996) evaluations 16591 fit -1828.26 change 2721  MxComputeSimAnnealing(tsallis1996) evaluations 16804 fit -7361.09 change 0   MxComputeSimAnnealing(tsallis1996) evaluations 17017 fit -7186.87 change -8665MxComputeSimAnnealing(tsallis1996) evaluations 17229 fit -7466.45 change 0    MxComputeSimAnnealing(tsallis1996) evaluations 17439 fit -5642.36 change 2886MxComputeSimAnnealing(tsallis1996) evaluations 17649 fit -8501.08 change -554.1MxComputeSimAnnealing(tsallis1996) evaluations 17861 fit -3678.2 change 4802   MxComputeSimAnnealing(tsallis1996) evaluations 18072 fit -8528.54 change -527.4MxComputeSimAnnealing(tsallis1996) evaluations 18281 fit -7863.62 change 0     MxComputeSimAnnealing(tsallis1996) evaluations 18494 fit -8547.63 change -245.3MxComputeSimAnnealing(tsallis1996) evaluations 18706 fit -7880.84 change 70.96 MxComputeSimAnnealing(tsallis1996) evaluations 18918 fit -7186.29 change 456.6MxComputeSimAnnealing(tsallis1996) evaluations 19129 fit -7445.74 change 0    MxComputeSimAnnealing(tsallis1996) evaluations 19343 fit -8548.96 change -3111MxComputeSimAnnealing(tsallis1996) evaluations 19554 fit -8554.37 change -6.557MxComputeSimAnnealing(tsallis1996) evaluations 19765 fit -7954.26 change -6318 MxComputeSimAnnealing(tsallis1996) evaluations 19975 fit -7543.67 change 244.6MxComputeSimAnnealing(tsallis1996) evaluations 20186 fit -8546.82 change 9.91 MxComputeSimAnnealing(tsallis1996) evaluations 20398 fit -7971.97 change 578.6MxComputeSimAnnealing(tsallis1996) evaluations 20607 fit -7870.95 change 686.5MxComputeSimAnnealing(tsallis1996) evaluations 20820 fit -7174.19 change 0    MxComputeSimAnnealing(tsallis1996) evaluations 21032 fit -7915.09 change 643.8MxComputeSimAnnealing(tsallis1996) evaluations 21243 fit -8559.12 change -2.003MxComputeSimAnnealing(tsallis1996) evaluations 21454 fit -7659.14 change 897.3 MxComputeSimAnnealing(tsallis1996) evaluations 21668 fit -7221.15 change 728.3MxComputeSimAnnealing(tsallis1996) evaluations 21878 fit -7455.92 change 820.9MxComputeSimAnnealing(tsallis1996) evaluations 22090 fit -729.568 change 7665 MxComputeSimAnnealing(tsallis1996) evaluations 22300 fit -7557.75 change 0   MxComputeSimAnnealing(tsallis1996) evaluations 22509 fit -8519.76 change 42.48MxComputeSimAnnealing(tsallis1996) evaluations 22718 fit -8562.19 change -1199MxComputeSimAnnealing(tsallis1996) evaluations 22928 fit -8548.98 change -61.89MxComputeSimAnnealing(tsallis1996) evaluations 23140 fit -2911.07 change -7167 MxComputeSimAnnealing(tsallis1996) evaluations 23350 fit -8549.14 change -105.9MxComputeSimAnnealing(tsallis1996) evaluations 23559 fit -962.903 change -1934 MxComputeSimAnnealing(tsallis1996) evaluations 23769 fit -8561.32 change -1319MxComputeSimAnnealing(tsallis1996) evaluations 23979 fit -7509.57 change 538  MxComputeSimAnnealing(tsallis1996) evaluations 24190 fit -8162.68 change -453MxComputeSimAnnealing(tsallis1996) evaluations 24400 fit -7544.1 change 956.5MxComputeSimAnnealing(tsallis1996) evaluations 24611 fit -8563.42 change -8836MxComputeSimAnnealing(tsallis1996) evaluations 24822 fit -8122.66 change -250.6MxComputeSimAnnealing(tsallis1996) evaluations 25034 fit -8563.51 change -7.499MxComputeSimAnnealing(tsallis1996) evaluations 25242 fit -8558.38 change -314.8MxComputeSimAnnealing(tsallis1996) evaluations 25452 fit -7516 change 37.38    MxComputeSimAnnealing(tsallis1996) evaluations 25663 fit -6806.37 change 1757MxComputeSimAnnealing(tsallis1996) evaluations 25874 fit -7554.9 change 656.3MxComputeSimAnnealing(tsallis1996) evaluations 26086 fit -8502.14 change 61.76MxComputeSimAnnealing(tsallis1996) evaluations 26297 fit -7842.54 change 238.3MxComputeSimAnnealing(tsallis1996) evaluations 26505 fit -7896.08 change -388.1MxComputeSimAnnealing(tsallis1996) evaluations 26718 fit -8562.45 change 0.1378MxComputeSimAnnealing(tsallis1996) evaluations 26926 fit -8476.6 change 87.73  MxComputeSimAnnealing(tsallis1996) evaluations 27138 fit 6032.65 change 1.459e+04MxComputeSimAnnealing(tsallis1996) evaluations 27349 fit -8565.01 change -653.6  MxComputeSimAnnealing(tsallis1996) evaluations 27559 fit -8430.92 change -3023 MxComputeSimAnnealing(tsallis1996) evaluations 27768 fit -7493.47 change 1071 MxComputeSimAnnealing(tsallis1996) evaluations 27979 fit -7524.14 change 1033MxComputeSimAnnealing(tsallis1996) evaluations 28189 fit -8318.02 change -8691MxComputeSimAnnealing(tsallis1996) evaluations 28399 fit -8565.55 change -840.3MxComputeSimAnnealing(tsallis1996) evaluations 28610 fit -8565.59 change -0.2167MxComputeSimAnnealing(tsallis1996) evaluations 28820 fit -7430.41 change 955.6  MxComputeSimAnnealing(tsallis1996) evaluations 29030 fit -8565.93 change -0.05701MxComputeSimAnnealing(tsallis1996) evaluations 29242 fit -3985.31 change 4580    MxComputeSimAnnealing(tsallis1996) evaluations 29452 fit -8544.78 change 21.34MxComputeSimAnnealing(tsallis1996) evaluations 29661 fit -8555.14 change 9.781MxComputeSimAnnealing(tsallis1996) evaluations 29871 fit -8144.55 change 420.6MxComputeSimAnnealing(tsallis1996) evaluations 30081 fit -8566.78 change -680.6MxComputeSimAnnealing(tsallis1996) evaluations 30291 fit -8566.11 change -44.37MxComputeSimAnnealing(tsallis1996) evaluations 30501 fit -8363.28 change 203.5 MxComputeSimAnnealing(tsallis1996) evaluations 30709 fit -8566.94 change 0.2129MxComputeSimAnnealing(tsallis1996) evaluations 30919 fit -8567.99 change -1027 MxComputeSimAnnealing(tsallis1996) evaluations 31129 fit -8567.64 change -1046MxComputeSimAnnealing(tsallis1996) evaluations 31338 fit -4879.09 change 3689 MxComputeSimAnnealing(tsallis1996) evaluations 31547 fit -8279.33 change -457.7MxComputeSimAnnealing(tsallis1996) evaluations 31756 fit -8569.92 change -683.1MxComputeSimAnnealing(tsallis1996) evaluations 31968 fit -7820.26 change -77.03                                                                               
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
