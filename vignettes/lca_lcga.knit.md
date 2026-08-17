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
#> MxComputeSimAnnealing(tsallis1996) evaluations 229 fit 7911.41 change -974.7MxComputeSimAnnealing(tsallis1996) evaluations 763 fit 3647.25 change 4151  MxComputeSimAnnealing(tsallis1996) evaluations 1300 fit 12175.6 change 0  MxComputeSimAnnealing(tsallis1996) evaluations 1837 fit -1176.2 change 0MxComputeSimAnnealing(tsallis1996) evaluations 2375 fit 7684.92 change 9797MxComputeSimAnnealing(tsallis1996) evaluations 2909 fit 12745 change 6882  MxComputeSimAnnealing(tsallis1996) evaluations 3448 fit -1176.2 change 669.6MxComputeSimAnnealing(tsallis1996) evaluations 3989 fit -4338.64 change -8809MxComputeSimAnnealing(tsallis1996) evaluations 4529 fit -4357.61 change -3158MxComputeSimAnnealing(tsallis1996) evaluations 5068 fit -5172.7 change -5169 MxComputeSimAnnealing(tsallis1996) evaluations 5603 fit 12786.1 change -228.5MxComputeSimAnnealing(tsallis1996) evaluations 6145 fit -1427.93 change 6300 MxComputeSimAnnealing(tsallis1996) evaluations 6678 fit -3356.98 change 1301MxComputeSimAnnealing(tsallis1996) evaluations 7211 fit 471.35 change 1776  MxComputeSimAnnealing(tsallis1996) evaluations 7747 fit -7743.02 change -1.271e+04MxComputeSimAnnealing(tsallis1996) evaluations 8279 fit -5560.51 change -813.8    MxComputeSimAnnealing(tsallis1996) evaluations 8814 fit 14194.5 change 88.94  MxComputeSimAnnealing(tsallis1996) evaluations 9348 fit 13965.8 change 0    MxComputeSimAnnealing(tsallis1996) evaluations 9880 fit 14085.9 change 1.487e+04MxComputeSimAnnealing(tsallis1996) evaluations 10414 fit 14370.8 change 3451    MxComputeSimAnnealing(tsallis1996) evaluations 10943 fit -6961.02 change -2466MxComputeSimAnnealing(tsallis1996) evaluations 11473 fit -7721.82 change -1.073e+04MxComputeSimAnnealing(tsallis1996) evaluations 12001 fit -1577.44 change 4445      MxComputeSimAnnealing(tsallis1996) evaluations 12532 fit 14537.9 change 2.212e+04MxComputeSimAnnealing(tsallis1996) evaluations 13062 fit -6051.85 change 1672    MxComputeSimAnnealing(tsallis1996) evaluations 13587 fit -7750.82 change -2.226e+04MxComputeSimAnnealing(tsallis1996) evaluations 14115 fit -7750.33 change -1.269e+04MxComputeSimAnnealing(tsallis1996) evaluations 14641 fit -7740.38 change -120.4    MxComputeSimAnnealing(tsallis1996) evaluations 15168 fit -2095.53 change 5648  MxComputeSimAnnealing(tsallis1996) evaluations 15695 fit -7750.51 change -1.913MxComputeSimAnnealing(tsallis1996) evaluations 16219 fit 3206.79 change 9773                                                                               
#> MxComputeSimAnnealing(tsallis1996) evaluations 110 fit 7922.37 change -1.906e+04MxComputeSimAnnealing(tsallis1996) evaluations 467 fit 11553.5 change -3883     MxComputeSimAnnealing(tsallis1996) evaluations 823 fit 263.257 change 392.7MxComputeSimAnnealing(tsallis1996) evaluations 1179 fit -3850.45 change 2174MxComputeSimAnnealing(tsallis1996) evaluations 1535 fit -6024.25 change 0   MxComputeSimAnnealing(tsallis1996) evaluations 1892 fit -6024.25 change 0MxComputeSimAnnealing(tsallis1996) evaluations 2250 fit -2193.95 change 3830MxComputeSimAnnealing(tsallis1996) evaluations 2608 fit 1298.05 change 3403 MxComputeSimAnnealing(tsallis1996) evaluations 2964 fit -873.012 change 5151MxComputeSimAnnealing(tsallis1996) evaluations 3322 fit 9145.55 change 1.738e+04MxComputeSimAnnealing(tsallis1996) evaluations 3678 fit -8025.6 change -2001    MxComputeSimAnnealing(tsallis1996) evaluations 4033 fit -6024.25 change 0   MxComputeSimAnnealing(tsallis1996) evaluations 4391 fit -6257.02 change -232.8MxComputeSimAnnealing(tsallis1996) evaluations 4747 fit -6122.49 change -98.24MxComputeSimAnnealing(tsallis1996) evaluations 5104 fit -6024.25 change 0     MxComputeSimAnnealing(tsallis1996) evaluations 5459 fit -6193.33 change 0MxComputeSimAnnealing(tsallis1996) evaluations 5817 fit -6189.25 change -357MxComputeSimAnnealing(tsallis1996) evaluations 6175 fit -6434.86 change 1850MxComputeSimAnnealing(tsallis1996) evaluations 6531 fit -6437.69 change -671.3MxComputeSimAnnealing(tsallis1996) evaluations 6888 fit -6437.69 change -671.3MxComputeSimAnnealing(tsallis1996) evaluations 7245 fit -6411.56 change 592.2 MxComputeSimAnnealing(tsallis1996) evaluations 7599 fit -5953.97 change -1.145e+04MxComputeSimAnnealing(tsallis1996) evaluations 7957 fit -5847.08 change 0         MxComputeSimAnnealing(tsallis1996) evaluations 8312 fit 5646.02 change 0 MxComputeSimAnnealing(tsallis1996) evaluations 8667 fit 5645.97 change 1.377e+04MxComputeSimAnnealing(tsallis1996) evaluations 9023 fit -7122.91 change -4267   MxComputeSimAnnealing(tsallis1996) evaluations 9365 fit 5322.2 change -83.1  MxComputeSimAnnealing(tsallis1996) evaluations 9648 fit 13909.5 change 1.014e+04MxComputeSimAnnealing(tsallis1996) evaluations 9997 fit -5699.37 change 2614    MxComputeSimAnnealing(tsallis1996) evaluations 10346 fit -6837.65 change 1484MxComputeSimAnnealing(tsallis1996) evaluations 10695 fit -5613.29 change 2603MxComputeSimAnnealing(tsallis1996) evaluations 11045 fit -7276.92 change -2.386e+04MxComputeSimAnnealing(tsallis1996) evaluations 11390 fit -5595.74 change -1.049e+04MxComputeSimAnnealing(tsallis1996) evaluations 11734 fit -8302.31 change -1259     MxComputeSimAnnealing(tsallis1996) evaluations 12083 fit -1219.72 change 6898 MxComputeSimAnnealing(tsallis1996) evaluations 12432 fit -7007.1 change 1334 MxComputeSimAnnealing(tsallis1996) evaluations 12777 fit 2174.72 change 1.002e+04MxComputeSimAnnealing(tsallis1996) evaluations 13125 fit -7024.89 change -1255   MxComputeSimAnnealing(tsallis1996) evaluations 13471 fit -8344.12 change -527 MxComputeSimAnnealing(tsallis1996) evaluations 13819 fit -7459.8 change 881.1MxComputeSimAnnealing(tsallis1996) evaluations 14167 fit -5891.55 change 1147MxComputeSimAnnealing(tsallis1996) evaluations 14516 fit -8345.71 change -1.36e+04MxComputeSimAnnealing(tsallis1996) evaluations 14862 fit -8344.26 change 1.578    MxComputeSimAnnealing(tsallis1996) evaluations 15209 fit -8229.06 change 31.74MxComputeSimAnnealing(tsallis1996) evaluations 15552 fit -8346.24 change -15.44MxComputeSimAnnealing(tsallis1996) evaluations 15900 fit -8346.14 change -1228 MxComputeSimAnnealing(tsallis1996) evaluations 16247 fit -7860.41 change -3960MxComputeSimAnnealing(tsallis1996) evaluations 16593 fit -8345.89 change 0.1186MxComputeSimAnnealing(tsallis1996) evaluations 16938 fit -8342.16 change -27.31MxComputeSimAnnealing(tsallis1996) evaluations 17284 fit -8163.43 change 167.1 MxComputeSimAnnealing(tsallis1996) evaluations 17632 fit -8305.36 change 22.99MxComputeSimAnnealing(tsallis1996) evaluations 17979 fit -3883.56 change 1847 MxComputeSimAnnealing(tsallis1996) evaluations 18325 fit -8203.63 change -1.338e+04MxComputeSimAnnealing(tsallis1996) evaluations 18670 fit -7054.98 change 1167      MxComputeSimAnnealing(tsallis1996) evaluations 19017 fit -6480.98 change -8735MxComputeSimAnnealing(tsallis1996) evaluations 19362 fit -8346.87 change -9190MxComputeSimAnnealing(tsallis1996) evaluations 19708 fit -7056.76 change 1279 MxComputeSimAnnealing(tsallis1996) evaluations 20053 fit -152.051 change 7121MxComputeSimAnnealing(tsallis1996) evaluations 20394 fit -8036.24 change 283.4MxComputeSimAnnealing(tsallis1996) evaluations 20739 fit -8347.03 change -1.36e+04MxComputeSimAnnealing(tsallis1996) evaluations 21085 fit -7621.55 change 725.5    MxComputeSimAnnealing(tsallis1996) evaluations 21431 fit 1069.63 change 9405  MxComputeSimAnnealing(tsallis1996) evaluations 21777 fit -5568.05 change -1.085e+04                                                                                   
#> MxComputeSimAnnealing(tsallis1996) evaluations 45 fit -7023.02 change 0MxComputeSimAnnealing(tsallis1996) evaluations 273 fit -7442.24 change 0MxComputeSimAnnealing(tsallis1996) evaluations 503 fit -7442.24 change 0MxComputeSimAnnealing(tsallis1996) evaluations 730 fit -3788.3 change -8257MxComputeSimAnnealing(tsallis1996) evaluations 958 fit -144.97 change -2287MxComputeSimAnnealing(tsallis1996) evaluations 1187 fit 3225.85 change -6967MxComputeSimAnnealing(tsallis1996) evaluations 1415 fit -2503.25 change 736.4MxComputeSimAnnealing(tsallis1996) evaluations 1643 fit -7420.42 change 0    MxComputeSimAnnealing(tsallis1996) evaluations 1871 fit -7023.02 change 0MxComputeSimAnnealing(tsallis1996) evaluations 2099 fit -7442.24 change 0MxComputeSimAnnealing(tsallis1996) evaluations 2326 fit -3788.3 change -2129MxComputeSimAnnealing(tsallis1996) evaluations 2553 fit -584.047 change -8741MxComputeSimAnnealing(tsallis1996) evaluations 2780 fit -4828.23 change -869 MxComputeSimAnnealing(tsallis1996) evaluations 3009 fit -7420.42 change 0   MxComputeSimAnnealing(tsallis1996) evaluations 3237 fit -7023.02 change 0MxComputeSimAnnealing(tsallis1996) evaluations 3463 fit -7442.24 change -3654MxComputeSimAnnealing(tsallis1996) evaluations 3693 fit -3788.3 change 0     MxComputeSimAnnealing(tsallis1996) evaluations 3922 fit -3788.3 change -1039MxComputeSimAnnealing(tsallis1996) evaluations 4152 fit -2643.8 change -162.9MxComputeSimAnnealing(tsallis1996) evaluations 4379 fit 1429.9 change 8419   MxComputeSimAnnealing(tsallis1996) evaluations 4606 fit -3863.32 change 3559MxComputeSimAnnealing(tsallis1996) evaluations 4832 fit -7013.12 change 0   MxComputeSimAnnealing(tsallis1996) evaluations 5059 fit -7466.21 change -3724MxComputeSimAnnealing(tsallis1996) evaluations 5286 fit -419.152 change 2851 MxComputeSimAnnealing(tsallis1996) evaluations 5515 fit 837.648 change 6542 MxComputeSimAnnealing(tsallis1996) evaluations 5738 fit -7422.66 change 0  MxComputeSimAnnealing(tsallis1996) evaluations 5966 fit -7013.12 change 0MxComputeSimAnnealing(tsallis1996) evaluations 6194 fit -7466.21 change 0MxComputeSimAnnealing(tsallis1996) evaluations 6421 fit -3490.04 change -7466MxComputeSimAnnealing(tsallis1996) evaluations 6647 fit -7928.65 change -3109MxComputeSimAnnealing(tsallis1996) evaluations 6873 fit -7423.49 change 0    MxComputeSimAnnealing(tsallis1996) evaluations 7101 fit -7076.32 change 0MxComputeSimAnnealing(tsallis1996) evaluations 7328 fit -7466.21 change 6.089e-07MxComputeSimAnnealing(tsallis1996) evaluations 7556 fit -3490.3 change 0         MxComputeSimAnnealing(tsallis1996) evaluations 7782 fit 3568.59 change 7097MxComputeSimAnnealing(tsallis1996) evaluations 8007 fit -7422.35 change -0.001232MxComputeSimAnnealing(tsallis1996) evaluations 8235 fit -7084.2 change 22.91     MxComputeSimAnnealing(tsallis1996) evaluations 8462 fit -8349.57 change -783.1MxComputeSimAnnealing(tsallis1996) evaluations 8690 fit -2766.2 change 0      MxComputeSimAnnealing(tsallis1996) evaluations 8917 fit -307.119 change -5713MxComputeSimAnnealing(tsallis1996) evaluations 9143 fit -4193.89 change 4088 MxComputeSimAnnealing(tsallis1996) evaluations 9370 fit -8462.67 change -1355MxComputeSimAnnealing(tsallis1996) evaluations 9596 fit -7600.96 change 0    MxComputeSimAnnealing(tsallis1996) evaluations 9822 fit -6774.05 change -6697MxComputeSimAnnealing(tsallis1996) evaluations 10049 fit -8487.65 change -4971MxComputeSimAnnealing(tsallis1996) evaluations 10273 fit -7540.01 change -352.1MxComputeSimAnnealing(tsallis1996) evaluations 10500 fit -7587.54 change 0     MxComputeSimAnnealing(tsallis1996) evaluations 10725 fit -7904.88 change -7132MxComputeSimAnnealing(tsallis1996) evaluations 10951 fit 7683.44 change 1.619e+04MxComputeSimAnnealing(tsallis1996) evaluations 11178 fit -7629.43 change 0       MxComputeSimAnnealing(tsallis1996) evaluations 11405 fit -7354.43 change 248.3MxComputeSimAnnealing(tsallis1996) evaluations 11632 fit -7781.46 change -5178MxComputeSimAnnealing(tsallis1996) evaluations 11857 fit -5705.66 change 757.8MxComputeSimAnnealing(tsallis1996) evaluations 12083 fit -6269.81 change 1804 MxComputeSimAnnealing(tsallis1996) evaluations 12308 fit -7329.2 change 500.2MxComputeSimAnnealing(tsallis1996) evaluations 12533 fit -1890.05 change 6481MxComputeSimAnnealing(tsallis1996) evaluations 12760 fit -6840.33 change 1133MxComputeSimAnnealing(tsallis1996) evaluations 12985 fit -7684.13 change 71.75MxComputeSimAnnealing(tsallis1996) evaluations 13211 fit -7720.02 change -374.4MxComputeSimAnnealing(tsallis1996) evaluations 13437 fit -1705.27 change 5229  MxComputeSimAnnealing(tsallis1996) evaluations 13662 fit -7361.08 change -559MxComputeSimAnnealing(tsallis1996) evaluations 13887 fit -8487.91 change -482.4MxComputeSimAnnealing(tsallis1996) evaluations 14113 fit -7433.82 change 1100  MxComputeSimAnnealing(tsallis1996) evaluations 14338 fit -1268.93 change 6841MxComputeSimAnnealing(tsallis1996) evaluations 14564 fit -6919.65 change -760.4MxComputeSimAnnealing(tsallis1996) evaluations 14791 fit -7576.58 change 314.2 MxComputeSimAnnealing(tsallis1996) evaluations 15017 fit -7442.21 change 90.7 MxComputeSimAnnealing(tsallis1996) evaluations 15241 fit -1237.12 change -2615MxComputeSimAnnealing(tsallis1996) evaluations 15466 fit -8519.66 change 16.97MxComputeSimAnnealing(tsallis1996) evaluations 15690 fit -7472.98 change 1071 MxComputeSimAnnealing(tsallis1996) evaluations 15915 fit -6940.43 change -6481MxComputeSimAnnealing(tsallis1996) evaluations 16139 fit -7238.33 change 111.3MxComputeSimAnnealing(tsallis1996) evaluations 16364 fit -7959.5 change -71.54MxComputeSimAnnealing(tsallis1996) evaluations 16588 fit -7417.95 change -6815MxComputeSimAnnealing(tsallis1996) evaluations 16814 fit -3501.94 change 4822 MxComputeSimAnnealing(tsallis1996) evaluations 17037 fit -7868.86 change 0   MxComputeSimAnnealing(tsallis1996) evaluations 17261 fit -7450.39 change 0MxComputeSimAnnealing(tsallis1996) evaluations 17486 fit -7825.26 change -115.9MxComputeSimAnnealing(tsallis1996) evaluations 17710 fit -8523.9 change -186.8 MxComputeSimAnnealing(tsallis1996) evaluations 17933 fit -7424.92 change 0    MxComputeSimAnnealing(tsallis1996) evaluations 18156 fit -8497.23 change 51.35MxComputeSimAnnealing(tsallis1996) evaluations 18381 fit -8003.44 change -119.7MxComputeSimAnnealing(tsallis1996) evaluations 18605 fit -7412.53 change 1136  MxComputeSimAnnealing(tsallis1996) evaluations 18828 fit 246.913 change -735.8MxComputeSimAnnealing(tsallis1996) evaluations 19052 fit -8518.82 change -71.79MxComputeSimAnnealing(tsallis1996) evaluations 19276 fit -7407.34 change -6844 MxComputeSimAnnealing(tsallis1996) evaluations 19499 fit -6681 change -1999   MxComputeSimAnnealing(tsallis1996) evaluations 19724 fit -8204.58 change -326.4MxComputeSimAnnealing(tsallis1996) evaluations 19948 fit -7405.77 change 1129  MxComputeSimAnnealing(tsallis1996) evaluations 20173 fit -8310.31 change -3545MxComputeSimAnnealing(tsallis1996) evaluations 20395 fit -8546.6 change -1063 MxComputeSimAnnealing(tsallis1996) evaluations 20618 fit -8342.63 change -112.8MxComputeSimAnnealing(tsallis1996) evaluations 20844 fit -5545.22 change 1686  MxComputeSimAnnealing(tsallis1996) evaluations 21065 fit -8554.24 change -51.84MxComputeSimAnnealing(tsallis1996) evaluations 21291 fit -8510.59 change -8172 MxComputeSimAnnealing(tsallis1996) evaluations 21514 fit -7190.61 change 460.8MxComputeSimAnnealing(tsallis1996) evaluations 21736 fit -8555.15 change -1135MxComputeSimAnnealing(tsallis1996) evaluations 21961 fit -494.353 change 8059 MxComputeSimAnnealing(tsallis1996) evaluations 22186 fit -8527.3 change 28.05MxComputeSimAnnealing(tsallis1996) evaluations 22409 fit -8555 change -1088  MxComputeSimAnnealing(tsallis1996) evaluations 22632 fit -8526.31 change -2708MxComputeSimAnnealing(tsallis1996) evaluations 22855 fit -8555.43 change -671.7MxComputeSimAnnealing(tsallis1996) evaluations 23079 fit -7415.42 change -3.67e-06MxComputeSimAnnealing(tsallis1996) evaluations 23305 fit -640.909 change 6089     MxComputeSimAnnealing(tsallis1996) evaluations 23526 fit -8553.36 change 2.417MxComputeSimAnnealing(tsallis1996) evaluations 23749 fit -8512.92 change -1652MxComputeSimAnnealing(tsallis1996) evaluations 23971 fit -8555.83 change -1.175e+04MxComputeSimAnnealing(tsallis1996) evaluations 24195 fit -7478.46 change 0         MxComputeSimAnnealing(tsallis1996) evaluations 24420 fit -8519.36 change -7790MxComputeSimAnnealing(tsallis1996) evaluations 24645 fit -7970.14 change -7486MxComputeSimAnnealing(tsallis1996) evaluations 24866 fit -8555.22 change -1077MxComputeSimAnnealing(tsallis1996) evaluations 25090 fit -357.743 change 7837 MxComputeSimAnnealing(tsallis1996) evaluations 25313 fit -8556.12 change -4001MxComputeSimAnnealing(tsallis1996) evaluations 25536 fit -7422.61 change 1134 MxComputeSimAnnealing(tsallis1996) evaluations 25760 fit -7358.95 change -3.595e+04MxComputeSimAnnealing(tsallis1996) evaluations 25981 fit -8555.76 change -1064     MxComputeSimAnnealing(tsallis1996) evaluations 26203 fit -8542.9 change -6755 MxComputeSimAnnealing(tsallis1996) evaluations 26430 fit -8436.87 change 118.8MxComputeSimAnnealing(tsallis1996) evaluations 26656 fit -8556.41 change -659.3MxComputeSimAnnealing(tsallis1996) evaluations 26881 fit -8556.01 change -95.89                                                                               
#> MxComputeSimAnnealing(tsallis1996) evaluations 148 fit -7825.65 change 0MxComputeSimAnnealing(tsallis1996) evaluations 358 fit -2938.75 change 4428MxComputeSimAnnealing(tsallis1996) evaluations 570 fit -7295.31 change 0   MxComputeSimAnnealing(tsallis1996) evaluations 780 fit -7367.16 change -339.9MxComputeSimAnnealing(tsallis1996) evaluations 993 fit -7295.31 change -1.344e+04MxComputeSimAnnealing(tsallis1996) evaluations 1202 fit -7027.27 change 438.6    MxComputeSimAnnealing(tsallis1996) evaluations 1414 fit 2005.93 change -6972 MxComputeSimAnnealing(tsallis1996) evaluations 1624 fit -7465.85 change 359.8MxComputeSimAnnealing(tsallis1996) evaluations 1834 fit -3027.75 change -2696MxComputeSimAnnealing(tsallis1996) evaluations 2046 fit -7825.65 change -530.3MxComputeSimAnnealing(tsallis1996) evaluations 2256 fit -7367.16 change 0     MxComputeSimAnnealing(tsallis1996) evaluations 2468 fit -7295.31 change -1.371e+04MxComputeSimAnnealing(tsallis1996) evaluations 2677 fit -7027.27 change 438.6     MxComputeSimAnnealing(tsallis1996) evaluations 2887 fit 5834.4 change 8881   MxComputeSimAnnealing(tsallis1996) evaluations 3097 fit -7825.65 change 0 MxComputeSimAnnealing(tsallis1996) evaluations 3308 fit -4513.35 change 2854MxComputeSimAnnealing(tsallis1996) evaluations 3518 fit -7295.31 change -1.453e+04MxComputeSimAnnealing(tsallis1996) evaluations 3729 fit -7027.27 change 0         MxComputeSimAnnealing(tsallis1996) evaluations 3943 fit -7295.31 change -4902MxComputeSimAnnealing(tsallis1996) evaluations 4154 fit -7027.27 change 0    MxComputeSimAnnealing(tsallis1996) evaluations 4365 fit 2937.15 change -4548MxComputeSimAnnealing(tsallis1996) evaluations 4575 fit -7465.85 change 0   MxComputeSimAnnealing(tsallis1996) evaluations 4785 fit -6080.46 change -2164MxComputeSimAnnealing(tsallis1996) evaluations 4997 fit -7825.65 change 0    MxComputeSimAnnealing(tsallis1996) evaluations 5208 fit -6389.14 change 978MxComputeSimAnnealing(tsallis1996) evaluations 5420 fit -7295.31 change 0  MxComputeSimAnnealing(tsallis1996) evaluations 5630 fit -7367.16 change -339.9MxComputeSimAnnealing(tsallis1996) evaluations 5839 fit 3667.84 change 2908   MxComputeSimAnnealing(tsallis1996) evaluations 6050 fit -7465.85 change 0  MxComputeSimAnnealing(tsallis1996) evaluations 6259 fit -3308.07 change 3815MxComputeSimAnnealing(tsallis1996) evaluations 6471 fit -7825.65 change -530.3MxComputeSimAnnealing(tsallis1996) evaluations 6681 fit -7367.16 change 0     MxComputeSimAnnealing(tsallis1996) evaluations 6893 fit -7295.31 change -8667MxComputeSimAnnealing(tsallis1996) evaluations 7103 fit -7027.27 change 234.6MxComputeSimAnnealing(tsallis1996) evaluations 7315 fit -781.407 change -4359MxComputeSimAnnealing(tsallis1996) evaluations 7525 fit -7726.88 change -246.9MxComputeSimAnnealing(tsallis1996) evaluations 7735 fit -8434.49 change -4860 MxComputeSimAnnealing(tsallis1996) evaluations 7947 fit -7822.93 change 0    MxComputeSimAnnealing(tsallis1996) evaluations 8157 fit -7375.84 change 0MxComputeSimAnnealing(tsallis1996) evaluations 8368 fit -7332.18 change -1686MxComputeSimAnnealing(tsallis1996) evaluations 8578 fit -7015.95 change 0    MxComputeSimAnnealing(tsallis1996) evaluations 8788 fit 201.41 change 3915MxComputeSimAnnealing(tsallis1996) evaluations 8999 fit -7490.08 change 302MxComputeSimAnnealing(tsallis1996) evaluations 9208 fit -6388.42 change 1002MxComputeSimAnnealing(tsallis1996) evaluations 9419 fit -7340.88 change -8.588MxComputeSimAnnealing(tsallis1996) evaluations 9629 fit -7010.84 change 0     MxComputeSimAnnealing(tsallis1996) evaluations 9840 fit 16050.3 change 2.428e+04MxComputeSimAnnealing(tsallis1996) evaluations 10049 fit -7608.66 change 174.4  MxComputeSimAnnealing(tsallis1996) evaluations 10257 fit -7385.01 change 0    MxComputeSimAnnealing(tsallis1996) evaluations 10469 fit -8438.96 change -1125MxComputeSimAnnealing(tsallis1996) evaluations 10680 fit -7385.65 change -265.9MxComputeSimAnnealing(tsallis1996) evaluations 10891 fit -7247.79 change -1744 MxComputeSimAnnealing(tsallis1996) evaluations 11102 fit -7113.59 change 1034 MxComputeSimAnnealing(tsallis1996) evaluations 11310 fit -5465.56 change 1594MxComputeSimAnnealing(tsallis1996) evaluations 11520 fit -7595.5 change 849.2MxComputeSimAnnealing(tsallis1996) evaluations 11730 fit -7497.63 change -480.3MxComputeSimAnnealing(tsallis1996) evaluations 11943 fit -7804.65 change -2782 MxComputeSimAnnealing(tsallis1996) evaluations 12150 fit -7473.22 change -7.758e-06MxComputeSimAnnealing(tsallis1996) evaluations 12359 fit -2229.96 change 2690      MxComputeSimAnnealing(tsallis1996) evaluations 12570 fit -8301.34 change -979.8MxComputeSimAnnealing(tsallis1996) evaluations 12779 fit -7006.6 change -0.0003664MxComputeSimAnnealing(tsallis1996) evaluations 12993 fit -7316.1 change -3544     MxComputeSimAnnealing(tsallis1996) evaluations 13202 fit -7029.4 change 425.8MxComputeSimAnnealing(tsallis1996) evaluations 13412 fit -8074.53 change 375.3MxComputeSimAnnealing(tsallis1996) evaluations 13621 fit -8104.89 change 61.95MxComputeSimAnnealing(tsallis1996) evaluations 13831 fit -8391.01 change -32.55MxComputeSimAnnealing(tsallis1996) evaluations 14043 fit -8445.11 change -7621 MxComputeSimAnnealing(tsallis1996) evaluations 14253 fit -7041.44 change 1109 MxComputeSimAnnealing(tsallis1996) evaluations 14461 fit -8454.09 change -2127MxComputeSimAnnealing(tsallis1996) evaluations 14671 fit -8450.1 change -872.5MxComputeSimAnnealing(tsallis1996) evaluations 14880 fit -7601.72 change -287 MxComputeSimAnnealing(tsallis1996) evaluations 15091 fit -8343.81 change -1874MxComputeSimAnnealing(tsallis1996) evaluations 15301 fit -7631.57 change 0    MxComputeSimAnnealing(tsallis1996) evaluations 15511 fit -8505.03 change -1.419e+04MxComputeSimAnnealing(tsallis1996) evaluations 15718 fit -7224.01 change 398.1     MxComputeSimAnnealing(tsallis1996) evaluations 15926 fit -7624.11 change 889.4MxComputeSimAnnealing(tsallis1996) evaluations 16135 fit -7730.03 change 785.4MxComputeSimAnnealing(tsallis1996) evaluations 16345 fit -7228.62 change 1296 MxComputeSimAnnealing(tsallis1996) evaluations 16553 fit -7348.52 change 1174MxComputeSimAnnealing(tsallis1996) evaluations 16766 fit -7278.82 change -2779MxComputeSimAnnealing(tsallis1996) evaluations 16977 fit -8528.34 change -122.6MxComputeSimAnnealing(tsallis1996) evaluations 17189 fit -2124.26 change 6390  MxComputeSimAnnealing(tsallis1996) evaluations 17398 fit -8539.53 change -590.8MxComputeSimAnnealing(tsallis1996) evaluations 17606 fit -7868.92 change 0     MxComputeSimAnnealing(tsallis1996) evaluations 17818 fit -7272.67 change 533.7MxComputeSimAnnealing(tsallis1996) evaluations 18028 fit -7491.63 change 0    MxComputeSimAnnealing(tsallis1996) evaluations 18236 fit 540.858 change 7919MxComputeSimAnnealing(tsallis1996) evaluations 18391 fit -8477.41 change -74.11MxComputeSimAnnealing(tsallis1996) evaluations 18600 fit -7583.51 change 0     MxComputeSimAnnealing(tsallis1996) evaluations 18810 fit -8535.51 change -1805MxComputeSimAnnealing(tsallis1996) evaluations 19021 fit -8166.86 change -980.5MxComputeSimAnnealing(tsallis1996) evaluations 19231 fit -8553.31 change -22.27MxComputeSimAnnealing(tsallis1996) evaluations 19442 fit -7291.29 change 767.1 MxComputeSimAnnealing(tsallis1996) evaluations 19651 fit -8548.27 change -973.7MxComputeSimAnnealing(tsallis1996) evaluations 19859 fit -8555.02 change -952.7MxComputeSimAnnealing(tsallis1996) evaluations 20069 fit -8402.08 change -828.8MxComputeSimAnnealing(tsallis1996) evaluations 20278 fit -8539.49 change 17.1  MxComputeSimAnnealing(tsallis1996) evaluations 20487 fit -3922.61 change 4521MxComputeSimAnnealing(tsallis1996) evaluations 20695 fit -8557.74 change -1359MxComputeSimAnnealing(tsallis1996) evaluations 20904 fit -7462.89 change 0    MxComputeSimAnnealing(tsallis1996) evaluations 21113 fit -1548.38 change 7011MxComputeSimAnnealing(tsallis1996) evaluations 21324 fit -7974.31 change -0.5268MxComputeSimAnnealing(tsallis1996) evaluations 21534 fit -7403.21 change 1144   MxComputeSimAnnealing(tsallis1996) evaluations 21745 fit -7218.6 change 1342 MxComputeSimAnnealing(tsallis1996) evaluations 21954 fit -7470.39 change 0  MxComputeSimAnnealing(tsallis1996) evaluations 22166 fit -8512.75 change -643.7MxComputeSimAnnealing(tsallis1996) evaluations 22372 fit -7928.81 change 0     MxComputeSimAnnealing(tsallis1996) evaluations 22581 fit -7883.21 change 679.1MxComputeSimAnnealing(tsallis1996) evaluations 22788 fit -4085.22 change -1.904e+04MxComputeSimAnnealing(tsallis1996) evaluations 22996 fit -8561.98 change -1339     MxComputeSimAnnealing(tsallis1996) evaluations 23206 fit -8562.73 change -2.052MxComputeSimAnnealing(tsallis1996) evaluations 23415 fit -4252.18 change -1487 MxComputeSimAnnealing(tsallis1996) evaluations 23623 fit -7907.9 change 0     MxComputeSimAnnealing(tsallis1996) evaluations 23831 fit -8416.77 change 91.14MxComputeSimAnnealing(tsallis1996) evaluations 24040 fit -8088.81 change -2128MxComputeSimAnnealing(tsallis1996) evaluations 24248 fit -8270.28 change -382.8MxComputeSimAnnealing(tsallis1996) evaluations 24457 fit -7894.77 change 667.4 MxComputeSimAnnealing(tsallis1996) evaluations 24668 fit -8527.64 change -140.4MxComputeSimAnnealing(tsallis1996) evaluations 24875 fit -7553.7 change -6.832e-09MxComputeSimAnnealing(tsallis1996) evaluations 25084 fit -8523.25 change 40.47    MxComputeSimAnnealing(tsallis1996) evaluations 25292 fit -8560.28 change -10.49MxComputeSimAnnealing(tsallis1996) evaluations 25500 fit -7553.39 change 932.7 MxComputeSimAnnealing(tsallis1996) evaluations 25710 fit -8545.67 change -797.4MxComputeSimAnnealing(tsallis1996) evaluations 25920 fit -7305.04 change 1131  MxComputeSimAnnealing(tsallis1996) evaluations 26128 fit -8563.48 change -1050MxComputeSimAnnealing(tsallis1996) evaluations 26335 fit -8564.1 change -0.001731MxComputeSimAnnealing(tsallis1996) evaluations 26543 fit -7333.26 change -2927   MxComputeSimAnnealing(tsallis1996) evaluations 26751 fit -7589.9 change -16.84MxComputeSimAnnealing(tsallis1996) evaluations 26960 fit -8564.54 change -1351MxComputeSimAnnealing(tsallis1996) evaluations 27169 fit -8564.02 change -74.61MxComputeSimAnnealing(tsallis1996) evaluations 27379 fit -7527.15 change 1038  MxComputeSimAnnealing(tsallis1996) evaluations 27588 fit 1091.28 change 9655 MxComputeSimAnnealing(tsallis1996) evaluations 27797 fit -7774.93 change 0  MxComputeSimAnnealing(tsallis1996) evaluations 28006 fit -8465.86 change -572.4MxComputeSimAnnealing(tsallis1996) evaluations 28214 fit -5612.56 change 1417  MxComputeSimAnnealing(tsallis1996) evaluations 28422 fit -7725.18 change 0   MxComputeSimAnnealing(tsallis1996) evaluations 28631 fit -7889.5 change 0 MxComputeSimAnnealing(tsallis1996) evaluations 28841 fit -8378.28 change 187.6MxComputeSimAnnealing(tsallis1996) evaluations 29049 fit -7613.42 change 945.7MxComputeSimAnnealing(tsallis1996) evaluations 29256 fit -8566.05 change -679.1MxComputeSimAnnealing(tsallis1996) evaluations 29466 fit -7774.54 change -2200 MxComputeSimAnnealing(tsallis1996) evaluations 29673 fit -7656.36 change 905  MxComputeSimAnnealing(tsallis1996) evaluations 29882 fit -7885.9 change 680.4MxComputeSimAnnealing(tsallis1996) evaluations 30093 fit -7464.2 change -370.1MxComputeSimAnnealing(tsallis1996) evaluations 30299 fit -8466.16 change -890 MxComputeSimAnnealing(tsallis1996) evaluations 30507 fit -8303.18 change 264.2MxComputeSimAnnealing(tsallis1996) evaluations 30715 fit -8567.44 change -1.105e+04MxComputeSimAnnealing(tsallis1996) evaluations 30924 fit -8431.87 change -902.6    MxComputeSimAnnealing(tsallis1996) evaluations 31132 fit -8568.04 change -681.4MxComputeSimAnnealing(tsallis1996) evaluations 31338 fit -4879.09 change 3689  MxComputeSimAnnealing(tsallis1996) evaluations 31547 fit -8279.33 change -457.7MxComputeSimAnnealing(tsallis1996) evaluations 31755 fit -7886.8 change 683.1  MxComputeSimAnnealing(tsallis1996) evaluations 31965 fit -8059.7 change -565.6                                                                              
#> Beginning initial fit attemptFit attempt 0, fit=-5186.90412866336, new current best! (was -5186.90412866336)Beginning fit attempt 1 of at maximum 50 extra tries                           Beginning fit attempt 2 of at maximum 50 extra triesBeginning fit attempt 3 of at maximum 50 extra triesBeginning fit attempt 4 of at maximum 50 extra triesBeginning fit attempt 5 of at maximum 50 extra triesBeginning fit attempt 6 of at maximum 50 extra triesBeginning fit attempt 7 of at maximum 50 extra triesBeginning fit attempt 8 of at maximum 50 extra triesBeginning fit attempt 9 of at maximum 50 extra triesBeginning fit attempt 10 of at maximum 50 extra triesBeginning fit attempt 11 of at maximum 50 extra triesBeginning fit attempt 12 of at maximum 50 extra triesBeginning fit attempt 13 of at maximum 50 extra triesBeginning fit attempt 14 of at maximum 50 extra triesBeginning fit attempt 15 of at maximum 50 extra triesBeginning fit attempt 16 of at maximum 50 extra triesBeginning fit attempt 17 of at maximum 50 extra triesBeginning fit attempt 18 of at maximum 50 extra triesBeginning fit attempt 19 of at maximum 50 extra triesBeginning fit attempt 20 of at maximum 50 extra triesBeginning fit attempt 21 of at maximum 50 extra triesBeginning fit attempt 22 of at maximum 50 extra triesBeginning fit attempt 23 of at maximum 50 extra triesBeginning fit attempt 24 of at maximum 50 extra triesBeginning fit attempt 25 of at maximum 50 extra triesBeginning fit attempt 26 of at maximum 50 extra triesBeginning fit attempt 27 of at maximum 50 extra triesBeginning fit attempt 28 of at maximum 50 extra triesBeginning fit attempt 29 of at maximum 50 extra triesBeginning fit attempt 30 of at maximum 50 extra triesBeginning fit attempt 31 of at maximum 50 extra triesMxComputeGradientDescent(SLSQP) evaluations 652 fit nan                                                       Beginning fit attempt 32 of at maximum 50 extra triesBeginning fit attempt 33 of at maximum 50 extra triesBeginning fit attempt 34 of at maximum 50 extra triesBeginning fit attempt 35 of at maximum 50 extra triesBeginning fit attempt 36 of at maximum 50 extra triesBeginning fit attempt 37 of at maximum 50 extra triesBeginning fit attempt 38 of at maximum 50 extra triesBeginning fit attempt 39 of at maximum 50 extra triesBeginning fit attempt 40 of at maximum 50 extra triesBeginning fit attempt 41 of at maximum 50 extra triesBeginning fit attempt 42 of at maximum 50 extra triesBeginning fit attempt 43 of at maximum 50 extra triesBeginning fit attempt 44 of at maximum 50 extra triesBeginning fit attempt 45 of at maximum 50 extra triesBeginning fit attempt 46 of at maximum 50 extra triesBeginning fit attempt 47 of at maximum 50 extra triesBeginning fit attempt 48 of at maximum 50 extra triesBeginning fit attempt 49 of at maximum 50 extra triesBeginning fit attempt 50 of at maximum 50 extra tries                                                     
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
