---
title: "Introduction to Latent Class Analysis"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Introduction to Latent Class Analysis}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---



This vignette covers the core ideas behind LCA,
and takes you through the estimation of models with continuous, ordinal, and dichotomous indicators, and combinations of these.
We focus on the practical decisions researchers face: evaluating model fit, assessing classification accuracy, interpreting model parameters, and reporting results clearly with publication-ready tables and figures.
After this vignette, you should be able to:

Note that this vignette roughly follows the steps outlined in the [SMART-LCA Checklist](https://cjvanlissa.github.io/tidySEM/articles/SMART_LCA_checklist.html) (Standards for More Accuracy in Reporting of different Types of Latent Class Analysis),
as introduced in [Van Lissa, C. J., Garnier-Villarreal, M., & Anadria, D. (2023). *Recommended Practices in Latent Class Analysis using the Open-Source R-Package tidySEM.* Structural Equation Modeling.](https://doi.org/10.1080/10705511.2023.2250920).


## Pre-Analysis

In this case, we're estimating LCAs as toy examples to become familiar with the technique. 
This application of LCA is purely **exploratory** (there is no theory about the number and kind of classes).
We are unsure if the indicators are valid and reliable for LCA,
and our sample size is predetermined by the data available for secondary analysis.
We will use data from [Zhao K, Zhao Y, Xu W. Relationship between middle school students' academic stress and physical exercise behavior from the perspective of Self-Determination Theory: The chained mediation of motivation and intention. PLoS One. 2025 Jan 3;20(1):e0316599](http://doi.org/10.1371/journal.pone.0316599). 

These data are available in the object `zhao_2025`, and we can load them as follows:


``` r
library(tidySEM)
library(OpenMx)
library(ggplot2)
df <- zhao_2025
```

Run `View(df)` to inspect the data.

### Using Your Own Data

If you are using your own data, you might be able to load them by using or modifying one of the following functions:


``` r
# For CSV data:
df <- read.csv("your_file.csv")
# For Excel spreadsheets:
df <- readxl::read_excel("your_file.xlsx", sheet = 1)
# For SPSS data:
df <- foreign::read.spss("your_file.sav", to.data.frame = TRUE)
# For Stata data:
df <- foreign::read.dta("your_file.dta")
```

## Examining Observed Data

Now, inspect descriptive statistics for the variables in the dataset.
This is an important initial data-screening step because it helps us
understand the structure of the data and identify features such as
variable ranges, means, standard deviations, missing values, and the
number of unique observed values.


``` r
table_desc <- descriptives(df)
table_desc
#>      name    type   n missing unique  mean median mode mode_value    sd  v min max range  skew
#> 1      ID numeric 290  0.0034    182 153.1    158  158         NA 84.82 NA   2 289   287 -0.14
#> 2   Grade numeric 291  0.0000      3   2.0      2    2         NA  0.89 NA   1   3     2 -0.02
#> 3     Age numeric 291  0.0000      5  13.7     14   14         NA  0.96 NA  12  15     3 -0.15
#> 4  Gender numeric 291  0.0000      2   1.6      2    2         NA  0.48 NA   1   2     1 -0.57
#> 5     TS1 numeric 291  0.0000      7   2.9      3    3         NA  1.52 NA   1   7     6  0.36
#> 6     TS2 numeric 291  0.0000      7   3.1      3    3         NA  1.46 NA   1   7     6  0.20
#> 7     TS3 numeric 290  0.0034      7   2.4      2    2         NA  1.56 NA   1   7     6  0.98
#> 8     TS4 numeric 291  0.0000      7   2.8      3    3         NA  1.56 NA   1   7     6  0.39
#> 9    SOS1 numeric 290  0.0034      7   4.7      5    5         NA  1.61 NA   1   7     6 -0.20
#> 10   SOS2 numeric 290  0.0034      7   3.3      3    3         NA  1.72 NA   1   7     6  0.45
#> 11   SOS3 numeric 290  0.0034      7   2.6      2    2         NA  1.75 NA   1   7     6  0.85
#> 12   SOS4 numeric 290  0.0034      7   2.9      3    3         NA  1.44 NA   1   7     6  0.51
#> 13    PS1 numeric 291  0.0000      7   4.5      4    4         NA  1.74 NA   1   7     6 -0.19
#> 14    PS2 numeric 291  0.0000      7   2.6      2    2         NA  1.65 NA   1   7     6  0.85
#> 15    PS3 numeric 287  0.0137      7   3.1      3    3         NA  1.70 NA   1   7     6  0.39
#> 16    PS4 numeric 291  0.0000      7   3.0      3    3         NA  1.85 NA   1   7     6  0.48
#> 17    PS5 numeric 288  0.0103      7   2.9      2    2         NA  1.71 NA   1   7     6  0.64
#> 18    AM1 numeric 290  0.0034      7   4.3      4    4         NA  1.65 NA   1   7     6 -0.44
#> 19    AM2 numeric 288  0.0103      7   4.7      5    5         NA  1.60 NA   1   7     6 -0.64
#> 20    AM3 numeric 290  0.0034      7   4.6      5    5         NA  1.61 NA   1   7     6 -0.45
#> 21    AM4 numeric 291  0.0000      7   4.5      4    4         NA  1.55 NA   1   7     6 -0.27
#> 22    AM5 numeric 290  0.0034      7   4.6      5    5         NA  1.57 NA   1   7     6 -0.53
#> 23    CM1 numeric 290  0.0034      7   3.0      3    3         NA  1.61 NA   1   7     6  0.72
#> 24    CM2 numeric 291  0.0000      7   2.9      3    3         NA  1.51 NA   1   7     6  0.42
#> 25    CM3 numeric 290  0.0034      7   3.4      4    4         NA  1.47 NA   1   7     6  0.33
#> 26    CM4 numeric 289  0.0069      7   3.5      4    4         NA  1.59 NA   1   7     6  0.34
#> 27    BI1 numeric 290  0.0034      7   4.6      5    5         NA  1.47 NA   1   7     6 -0.55
#> 28    BI2 numeric 289  0.0069      7   4.7      5    5         NA  1.40 NA   1   7     6 -0.78
#> 29    BI3 numeric 289  0.0069      7   4.8      5    5         NA  1.53 NA   1   7     6 -0.88
#> 30    BH1 numeric 291  0.0000      7   5.0      5    5         NA  1.74 NA   1   7     6 -0.53
#> 31    BH2 numeric 288  0.0103      7   3.3      3    3         NA  1.45 NA   1   7     6  0.65
#>    skew_2se   kurt kurt_2se
#> 1    -0.475 -1.237   -2.168
#> 2    -0.071 -1.738   -3.052
#> 3    -0.525 -0.984   -1.727
#> 4    -1.985 -1.690   -2.968
#> 5     1.253 -0.572   -1.004
#> 6     0.714 -0.739   -1.297
#> 7     3.415  0.132    0.231
#> 8     1.349 -0.841   -1.477
#> 9    -0.713 -0.803   -1.408
#> 10    1.580 -0.663   -1.162
#> 11    2.965 -0.348   -0.610
#> 12    1.765 -0.442   -0.775
#> 13   -0.657 -0.798   -1.401
#> 14    2.971 -0.187   -0.328
#> 15    1.367 -0.989   -1.725
#> 16    1.679 -0.927   -1.627
#> 17    2.234 -0.568   -0.992
#> 18   -1.523 -0.373   -0.653
#> 19   -2.242 -0.084   -0.146
#> 20   -1.555 -0.228   -0.400
#> 21   -0.940 -0.234   -0.411
#> 22   -1.868  0.060    0.105
#> 23    2.532  0.023    0.041
#> 24    1.476 -0.502   -0.881
#> 25    1.146  0.019    0.034
#> 26    1.175 -0.340   -0.594
#> 27   -1.923  0.265    0.465
#> 28   -2.709  0.606    1.061
#> 29   -3.067  0.460    0.804
#> 30   -1.868 -0.490   -0.861
#> 31    2.248  0.299    0.522
```

The `type` column tells you the measurement level of each variable. The `tidySEM` package supports latent class analysis with `numeric` (or `integer`) and `ordered` variables. 

Numeric and integer variables should be truly continuous (e.g., centimeters), or you must be willing to assume that they behave as continuous (scale mean scores in a questionnaire).

Ordinal indicators consist of categories that have a meaningful order, such as Likert responses ranging from *strongly disagree* to *strongly agree*.

Note that integer variables containing only a small number of values could be numeric score, but could also be ordered. The correct treatment depends on what the numbers mean, not merely on how R stores them. Check the `unique` column of `descriptives()` to see how many unique values there are; this may alert you to such issues.

If you have variables with other data types than numeric/integer/ordered - you will need to convert them.

One key issue is that variables are not always correctly stored. The `type` column may not correspond to the variable's substantive measurement level. For example, you might have a variable of type `integer` with values `1, 2`, but which actually represents employment, with values `Unemployed` and `Employed`.
Moreover, an integer variable may represent a truly continuous quantity (e.g., number of children), or an ordinal Likert scale, in which case it should be recoded to `ordered`. Similarly, a factor may represent an unordered categorical variable with several categories. For LCA, each category should be coded as a binary `ordered` variable.

You should therefore inspect both the variable type and the meaning of the variable before deciding how it should enter the model.

## Data Preprocessing

If a variable is incorrectly coded, you may be able to restore its correct type. Here are a few examples of type conversions:


``` r
# This is a character variable, because someone misstyped a t:
v1 <- c("5.1", "4.9", "4.7", "4.6t", "5", "5.4")
class(v1)
#> [1] "character"
# You can turn it into numeric; the mistyped value is lost:
as.numeric(v1)
#> [1] 5.1 4.9 4.7  NA 5.0 5.4
# This is an integer variable representing a Likert scale:
v2 <- c(2L, 3L, 2L, 5L, 3L, 4L)
class(v2)
#> [1] "integer"
# You can turn it into ordered:
ordered(v2)
#> [1] 2 3 2 5 3 4
#> Levels: 2 < 3 < 4 < 5
# This is a factor variable:
v3 <- factor(c("Medicine", "Humanities", "Humanities", "Medicine", "Social science", 
"Medicine"))
class(v3)
#> [1] "factor"
# You can encode it as multiple binary dummmies:
tidySEM::mx_dummies(v3)
#>   Humanities Medicine Social.science
#> 1          0        1              0
#> 2          1        0              0
#> 3          1        0              0
#> 4          0        1              0
#> 5          0        0              1
#> 6          0        1              0
```

The final command takes an unordered factor and represents it as a set of binary dummy variables; one for each category. These dummy variables can then be added to the analysis dataset.

Note that, in our example dataset, all variable `type`s are numeric, even though some must be integer (age), and others must be factors (Gender) or ordered (all Likert scales).
Note that all variables have just few  low number of unique values; this is another hint that some recoding is in order.

Here we recode 'gender' from its original numeric codes into
a factor variable. We also attach meaningful text labels to the numeric
categories so that subsequent tables and output are easier to understand.
The 'levels' argument specifies the original codes found in the dataset,
while 'labels' specifies the corresponding category names that R should
display instead.


``` r
df$Gender <- factor(df$Gender,
                    levels = c(1,2),
                    labels = c("M", "F"))
```

Several variables in the dataset are individual questionnaire items that
belong to broader multi-item scales. 

We can use these to practice two things:

1) Computing numeric scale mean scores
2) Encoding them as ordered variables

We will compute the scale scores first, because this still requires these variables to be `numeric`.

For example, the scale named TS consists of TS1 through TS4, while SOS
consists of SOS1 through SOS4. 
I like to store this information in a list, as this would allow you to, for example, easily perform psychometric analyses for every scale automatically (which is out of scope for the current tutorial).


``` r
# To prepare the scales list, I used:
scales_list <- grep("\\w{2}\\d", names(df), value = TRUE)
scalenames <- unique(gsub("\\d", "", scales_list))
scales_list <- lapply(scalenames, grep, x = names(df), value = TRUE)
names(scales_list) <- scalenames

# This gives the list below; you can also construct it for your dataset by hand:
scales_list <- list(
  TS = c("TS1", "TS2", "TS3", "TS4"),
  SOS = c("SOS1", "SOS2", "SOS3", "SOS4"),
  PS = c("PS1", "PS2", "PS3", "PS4", "PS5"),
  AM = c("AM1", "AM2", "AM3", "AM4", "AM5"),
  CM = c("CM1", "CM2", "CM3", "CM4"),
  BI = c("BI1", "BI2", "BI3"),
  BH = c("BH1", "BH2")
  )
```

To compute mean scale scores, we can use:


``` r
df_scales <- sapply(scales_list, function(items){
  rowMeans(df[, items, drop = FALSE], na.rm = TRUE)
})
# Add them to df:
df <- data.frame(df, df_scales)
```

Second, the encode these items as ordered variables, we could grab all relevant variables based on the naming pattern (these variable names end with a number),
or we can take all items in the `scales_list` using `unlist()`:


``` r
ord_vars <- grep("\\d$", names(df), value = TRUE)
ord_vars
#>  [1] "TS1"  "TS2"  "TS3"  "TS4"  "SOS1" "SOS2" "SOS3" "SOS4" "PS1"  "PS2"  "PS3"  "PS4"  "PS5" 
#> [14] "AM1"  "AM2"  "AM3"  "AM4"  "AM5"  "CM1"  "CM2"  "CM3"  "CM4"  "BI1"  "BI2"  "BI3"  "BH1" 
#> [27] "BH2"
# Or alternatively:
ord_vars <- unlist(scales_list)
```

Then, we can recode these variables as ordered:


``` r
df[ord_vars] <- lapply(df[ord_vars], ordered)
```

Inspect the cleaned dataset again before fitting the model. Check that numeric indicators really represent continuous quantities, ordered indicators have the intended category order, and binary indicators contain exactly two possible response categories. Also check for unexpected values, unused factor levels, miscoded missing values, and variables with little or no variation.

## Missing Data

This dataset has no missing data.
However, you can increase the challenge for yourself by adding some missingness.
If you want to do this - do it immediately after loading the data and removing the empty rows, with code like this:


``` r
library(mice)
df <- ampute(df)$amp
```

Regardless of the presence of missingness or not, `tidySEM` uses FIML which includes cases with partially missing data.

## Model Specification

The type of model you can specify depends partially on the measurement level of your indicators:

| Indicator type | Typical examples | tidySEM function | Class-specific quantities |
|---|---|---|---|
| Continuous | test scores, reaction time, symptom sum scores | `mx_profiles()` | means, variances, possibly covariances |
| Ordinal | Likert items, ordered severity categories | `mx_lca()` | category probabilities / thresholds |
| Dichotomous | yes/no, present/absent, correct/incorrect | `mx_lca()` | response probabilities |
| Mixed | a combination of the above | `mx_mixed_lca()` | parameters appropriate to each indicator type |



## Latent Profile Analysis (LPA) with Continuous Indicators {#sec-lpa}

We will use two scale scores from the Zhao dataset as a teaching example;
specifically, `c("AM", "CM")`, which represent autonomous and controlled motivation. 
You can increase the challenge level of this tutorial by using your own dataset, or by selecting different scales.
For example, `c("TS", "SOS", "PS")`, which represent three sources of stress/pressure: teacher stress, social stress, and parent stress.


``` r
df_lpa <- df[, c("AM", "CM")]
```

Note that standardizing indicators is not required and should not affect results, with two caveats: if continuous indicators are on very different scales (e.g., the variance differs by a factor 10+), this can cause problems for model convergence. Second, this can make plots harder to read because the plotting range of one variable could be much smaller than another.

### Model Specification

The arguments `variances` and `covariances` control the model specification for LPA.
For example, a model with equal variances and zero covariances assumpes that every indicator has the same residual variance across classes, and that indicators are "locally independent" (=uncorrelated) within each class, even if they should be correlated at the sample level.
Note that you can also specify different (co)variance structures, either for thorough exploration of because you have some theory about e.g., smaller variance in some classes, or different correlations between classes.
See the help file by running `?mx_profiles`. For example, you could specify the code below. 

Note that this analysis will take a very long time because these are many models and some of them are hard to estimate.
For any computationally intensive analyses like this, we can save the results to a file, so you can load the results from that file directly without having to wait for the code to run.
We do recommend that you experiment with running *some* analyses yourself, of course.


``` r
res_lpa <- mx_profiles(
  df_lpa,
  classes = 1:5,
  variances = c("equal", "varying"),
  covariances = c("zero", "equal")
)
#> MxComputeSimAnnealing(tsallis1996) evaluations 100 fit 3712.02 change 0MxComputeSimAnnealing(tsallis1996) evaluations 2213 fit 3411.67 change 1558MxComputeSimAnnealing(tsallis1996) evaluations 4289 fit 2109.3 change 203.1MxComputeSimAnnealing(tsallis1996) evaluations 6468 fit 1873.55 change -262MxComputeSimAnnealing(tsallis1996) evaluations 8493 fit 4569.39 change 2729                                                                           
#> MxComputeSimAnnealing(tsallis1996) evaluations 524 fit 2251.13 change -290.2MxComputeSimAnnealing(tsallis1996) evaluations 1955 fit 2075.99 change 0    MxComputeSimAnnealing(tsallis1996) evaluations 3402 fit 2391.45 change -52.05MxComputeSimAnnealing(tsallis1996) evaluations 4852 fit 2238.15 change -144.2MxComputeSimAnnealing(tsallis1996) evaluations 6224 fit 1800.71 change -798.9MxComputeSimAnnealing(tsallis1996) evaluations 7651 fit 2370.3 change -0.01142MxComputeSimAnnealing(tsallis1996) evaluations 9035 fit 1812.88 change 12.1   MxComputeSimAnnealing(tsallis1996) evaluations 10418 fit 1809.71 change 1.003MxComputeSimAnnealing(tsallis1996) evaluations 11794 fit 2045.27 change 3.433                                                                             
#> MxComputeSimAnnealing(tsallis1996) evaluations 567 fit 3856.48 change 460.1MxComputeSimAnnealing(tsallis1996) evaluations 1618 fit 2476.33 change 0   MxComputeSimAnnealing(tsallis1996) evaluations 2668 fit 2206.45 change 170.9MxComputeSimAnnealing(tsallis1996) evaluations 3720 fit 1802.45 change -260.9MxComputeSimAnnealing(tsallis1996) evaluations 4769 fit 1935.3 change -1934  MxComputeSimAnnealing(tsallis1996) evaluations 5780 fit 3010.41 change 1226MxComputeSimAnnealing(tsallis1996) evaluations 6820 fit 2293.39 change 483.9MxComputeSimAnnealing(tsallis1996) evaluations 7882 fit 2356.26 change 361.8MxComputeSimAnnealing(tsallis1996) evaluations 8938 fit 2030.3 change 247.7 MxComputeSimAnnealing(tsallis1996) evaluations 9970 fit 2031.44 change 151.8MxComputeSimAnnealing(tsallis1996) evaluations 11000 fit 2132.35 change 350 MxComputeSimAnnealing(tsallis1996) evaluations 12055 fit 1782.04 change -105.2MxComputeSimAnnealing(tsallis1996) evaluations 13095 fit 2372.44 change 590.6 MxComputeSimAnnealing(tsallis1996) evaluations 14089 fit 1781.63 change 0.001622MxComputeSimAnnealing(tsallis1996) evaluations 15144 fit 1940.99 change 159.7   MxComputeSimAnnealing(tsallis1996) evaluations 16153 fit 1971.33 change 190.2                                                                             
#> MxComputeSimAnnealing(tsallis1996) evaluations 310 fit 2371.1 change 350.4MxComputeSimAnnealing(tsallis1996) evaluations 1182 fit 2269.39 change -460.5MxComputeSimAnnealing(tsallis1996) evaluations 2042 fit 3971.37 change 438.4 MxComputeSimAnnealing(tsallis1996) evaluations 2899 fit 2169.21 change 0    MxComputeSimAnnealing(tsallis1996) evaluations 3759 fit 2108.59 change 43.71MxComputeSimAnnealing(tsallis1996) evaluations 4611 fit 2071.23 change 0    MxComputeSimAnnealing(tsallis1996) evaluations 5460 fit 1858.79 change 91.48MxComputeSimAnnealing(tsallis1996) evaluations 6324 fit 1863.05 change -6.145MxComputeSimAnnealing(tsallis1996) evaluations 7167 fit 1765.81 change -363.4MxComputeSimAnnealing(tsallis1996) evaluations 8026 fit 1766.64 change 0.704 MxComputeSimAnnealing(tsallis1996) evaluations 8884 fit 1863.9 change 97.83 MxComputeSimAnnealing(tsallis1996) evaluations 9740 fit 1779.71 change 14.17MxComputeSimAnnealing(tsallis1996) evaluations 10597 fit 1863.02 change 96.21MxComputeSimAnnealing(tsallis1996) evaluations 11485 fit 1792.63 change -6.347MxComputeSimAnnealing(tsallis1996) evaluations 12339 fit 2088.39 change 322.9 MxComputeSimAnnealing(tsallis1996) evaluations 13187 fit 1765.43 change -322.7MxComputeSimAnnealing(tsallis1996) evaluations 14065 fit 2200.1 change 0      MxComputeSimAnnealing(tsallis1996) evaluations 14944 fit 1765.41 change -0.09004MxComputeSimAnnealing(tsallis1996) evaluations 15795 fit 1766.6 change -322.5   MxComputeSimAnnealing(tsallis1996) evaluations 16626 fit 1765.42 change -4.011MxComputeSimAnnealing(tsallis1996) evaluations 17475 fit 2063.17 change 280.8 MxComputeSimAnnealing(tsallis1996) evaluations 18326 fit 1765.42 change -0.00418MxComputeSimAnnealing(tsallis1996) evaluations 19165 fit 1767.89 change -384    MxComputeSimAnnealing(tsallis1996) evaluations 20004 fit 1777.72 change 12.31                                                                             
#> MxComputeSimAnnealing(tsallis1996) evaluations 833 fit 2515.16 change -111.2MxComputeSimAnnealing(tsallis1996) evaluations 2989 fit 1806.67 change -1295MxComputeSimAnnealing(tsallis1996) evaluations 5174 fit 2462.36 change 534.4MxComputeSimAnnealing(tsallis1996) evaluations 7203 fit 2026.49 change 223.8MxComputeSimAnnealing(tsallis1996) evaluations 9283 fit 1848.91 change 46.29MxComputeSimAnnealing(tsallis1996) evaluations 11438 fit 1802.65 change -117.2                                                                              
#> MxComputeSimAnnealing(tsallis1996) evaluations 1124 fit 2571.57 change 517MxComputeSimAnnealing(tsallis1996) evaluations 2581 fit 3024.48 change 0  MxComputeSimAnnealing(tsallis1996) evaluations 4001 fit 2416.32 change 0MxComputeSimAnnealing(tsallis1996) evaluations 5435 fit 2454.88 change 0MxComputeSimAnnealing(tsallis1996) evaluations 6836 fit 2410.49 change 0MxComputeSimAnnealing(tsallis1996) evaluations 8280 fit 1778.82 change -0.6648MxComputeSimAnnealing(tsallis1996) evaluations 9632 fit 2769.49 change -138.3 MxComputeSimAnnealing(tsallis1996) evaluations 10997 fit 1867.04 change 63.08MxComputeSimAnnealing(tsallis1996) evaluations 12375 fit 2800.31 change 1001 MxComputeSimAnnealing(tsallis1996) evaluations 13760 fit 1793.56 change 15.95MxComputeSimAnnealing(tsallis1996) evaluations 15139 fit 1777.18 change -0.5101MxComputeSimAnnealing(tsallis1996) evaluations 16489 fit 2336.67 change 559.6  MxComputeSimAnnealing(tsallis1996) evaluations 17870 fit 1777.41 change 0.3276                                                                              
#> MxComputeSimAnnealing(tsallis1996) evaluations 72 fit 2628.82 change -6.346MxComputeSimAnnealing(tsallis1996) evaluations 1122 fit 2096.75 change 0   MxComputeSimAnnealing(tsallis1996) evaluations 2207 fit 2747.25 change 247.5MxComputeSimAnnealing(tsallis1996) evaluations 3269 fit 2089.82 change 110.4MxComputeSimAnnealing(tsallis1996) evaluations 4332 fit 2577.26 change 544.8MxComputeSimAnnealing(tsallis1996) evaluations 5400 fit 1765.69 change -30.72MxComputeSimAnnealing(tsallis1996) evaluations 6445 fit 2326.87 change 321.6 MxComputeSimAnnealing(tsallis1996) evaluations 7489 fit 3548.41 change 1553 MxComputeSimAnnealing(tsallis1996) evaluations 8562 fit 2080.71 change 320 MxComputeSimAnnealing(tsallis1996) evaluations 9639 fit 1770.69 change -465.7MxComputeSimAnnealing(tsallis1996) evaluations 10698 fit 2270.19 change 510.7MxComputeSimAnnealing(tsallis1996) evaluations 11767 fit 1760.56 change -471.6MxComputeSimAnnealing(tsallis1996) evaluations 12807 fit 1773.73 change 7.346 MxComputeSimAnnealing(tsallis1996) evaluations 13890 fit 2473.1 change 714.1 MxComputeSimAnnealing(tsallis1996) evaluations 14954 fit 1758.89 change -708.4MxComputeSimAnnealing(tsallis1996) evaluations 16016 fit 2017.21 change 252.6 MxComputeSimAnnealing(tsallis1996) evaluations 17080 fit 1815.47 change 24.4 MxComputeSimAnnealing(tsallis1996) evaluations 18124 fit 2215.44 change 73.72MxComputeSimAnnealing(tsallis1996) evaluations 19192 fit 2431.1 change 672.4 MxComputeSimAnnealing(tsallis1996) evaluations 20243 fit 1758.95 change -2.589MxComputeSimAnnealing(tsallis1996) evaluations 21305 fit 1758.61 change 0.002535MxComputeSimAnnealing(tsallis1996) evaluations 22369 fit 4223.58 change 2423    MxComputeSimAnnealing(tsallis1996) evaluations 23438 fit 2416.76 change 613.8MxComputeSimAnnealing(tsallis1996) evaluations 24443 fit 1758.75 change 0.1756MxComputeSimAnnealing(tsallis1996) evaluations 25486 fit 1758.58 change -1.257                                                                              
#> MxComputeSimAnnealing(tsallis1996) evaluations 284 fit 6423.61 change 2057MxComputeSimAnnealing(tsallis1996) evaluations 1155 fit 2025.44 change 0  MxComputeSimAnnealing(tsallis1996) evaluations 2048 fit 2126.59 change 0MxComputeSimAnnealing(tsallis1996) evaluations 2920 fit 2280.35 change 0MxComputeSimAnnealing(tsallis1996) evaluations 3792 fit 2280.35 change -86.74MxComputeSimAnnealing(tsallis1996) evaluations 4658 fit 2630.28 change 604.8 MxComputeSimAnnealing(tsallis1996) evaluations 5519 fit 2148.3 change 181.8 MxComputeSimAnnealing(tsallis1996) evaluations 6355 fit 1990.27 change 0   MxComputeSimAnnealing(tsallis1996) evaluations 7246 fit 1910.04 change 166.8MxComputeSimAnnealing(tsallis1996) evaluations 8108 fit 1911.8 change 169.8 MxComputeSimAnnealing(tsallis1996) evaluations 8974 fit 1907.45 change 11.29MxComputeSimAnnealing(tsallis1996) evaluations 9816 fit 2435.74 change 686.8MxComputeSimAnnealing(tsallis1996) evaluations 10681 fit 1740.22 change -16.2MxComputeSimAnnealing(tsallis1996) evaluations 11531 fit 2167.24 change 427.8MxComputeSimAnnealing(tsallis1996) evaluations 12393 fit 1755.09 change 16.07MxComputeSimAnnealing(tsallis1996) evaluations 13249 fit 1737.51 change -68.74MxComputeSimAnnealing(tsallis1996) evaluations 14094 fit 2022.61 change 281.1 MxComputeSimAnnealing(tsallis1996) evaluations 14932 fit 2206.65 change 471.2MxComputeSimAnnealing(tsallis1996) evaluations 15774 fit 1843.02 change 0    MxComputeSimAnnealing(tsallis1996) evaluations 16603 fit 1737.27 change 2.998MxComputeSimAnnealing(tsallis1996) evaluations 17466 fit 1755.5 change -4.965MxComputeSimAnnealing(tsallis1996) evaluations 18315 fit 2026.36 change -627.3MxComputeSimAnnealing(tsallis1996) evaluations 19177 fit 1733.34 change 0.08658MxComputeSimAnnealing(tsallis1996) evaluations 20017 fit 1965.67 change 232.5  MxComputeSimAnnealing(tsallis1996) evaluations 20889 fit 1733.08 change -0.7912MxComputeSimAnnealing(tsallis1996) evaluations 21756 fit 1732.99 change -456.9 MxComputeSimAnnealing(tsallis1996) evaluations 22612 fit 1733.01 change -50.4 MxComputeSimAnnealing(tsallis1996) evaluations 23431 fit 1732.91 change -62.62MxComputeSimAnnealing(tsallis1996) evaluations 24265 fit 1733.38 change 0.4922MxComputeSimAnnealing(tsallis1996) evaluations 25109 fit 1736.39 change -20.19MxComputeSimAnnealing(tsallis1996) evaluations 25959 fit 1746.12 change 13.35 MxComputeSimAnnealing(tsallis1996) evaluations 26808 fit 1767.9 change 35.13 MxComputeSimAnnealing(tsallis1996) evaluations 27643 fit 1959.92 change 227.2MxComputeSimAnnealing(tsallis1996) evaluations 28494 fit 1732.74 change -0.002563MxComputeSimAnnealing(tsallis1996) evaluations 29330 fit 1861.97 change 129.2    MxComputeSimAnnealing(tsallis1996) evaluations 30186 fit 1745.38 change 12.64MxComputeSimAnnealing(tsallis1996) evaluations 31050 fit 1732.73 change -15.42MxComputeSimAnnealing(tsallis1996) evaluations 31885 fit 1733.96 change 1.231                                                                              
saveRDS(res_lpa, "res_lpa_different_spect.rds")
```


``` r
res_lpa <- readRDS("res_lpa_different_spect.rds")
format_numeric(table_fit(res_lpa))
#>                     Name Classes      LL      n Parameters     AIC     BIC   saBIC Entropy
#> 1            equal var 1    1.00 -951.56 291.00          4 1911.12 1925.81 1913.13    1.00
#> 2            equal var 2    2.00 -914.57 291.00          7 1843.14 1868.86 1846.66    0.79
#> 3            equal var 3    3.00 -900.31 291.00         10 1820.62 1857.35 1825.64    0.68
#> 4            equal var 4    4.00 -887.52 291.00         13 1801.03 1848.79 1807.56    0.86
#> 5            equal var 5    5.00 -882.70 291.00         16 1797.41 1856.18 1805.44    0.73
#> 6  free var, equal cov 1    1.00 -920.19 291.00          5 1850.37 1868.74 1852.88    1.00
#> 7  free var, equal cov 2    2.00 -901.28 291.00         10 1822.56 1859.29 1827.58    0.58
#> 8  free var, equal cov 3    3.00 -888.53 291.00         15 1807.07 1862.17 1814.60    0.78
#> 9  free var, equal cov 4    4.00 -879.28 291.00         20 1798.56 1872.03 1808.60    0.78
#> 10 free var, equal cov 5    5.00 -866.36 291.00         25 1782.71 1874.54 1795.26    0.80
#>    prob_min prob_max n_min n_max np_ratio np_local
#> 1      1.00     1.00  1.00  1.00    72.75    72.75
#> 2      0.77     0.98  0.16  0.84    41.57    15.33
#> 3      0.82     0.87  0.12  0.44    29.10    13.12
#> 4      0.85     0.95  0.06  0.56    22.38     7.20
#> 5      0.76     0.96  0.07  0.32    18.19     7.92
#> 6      1.00     1.00  1.00  1.00    58.20    58.20
#> 7      0.87     0.88  0.48  0.52    29.10    31.33
#> 8      0.83     0.93  0.18  0.52    19.40    12.00
#> 9      0.79     0.92  0.13  0.37    14.55     8.71
#> 10     0.82     0.94  0.09  0.43    11.64     6.19
```

More complex models are not automatically better: extra parameters can create unstable solutions, tiny classes, or local maxima.
Think about which models make sense to estimate, and how many classes your data could reasonably support.

### Maximum Number of Classes

We can set an upper limit on the number of classes to estimate, for example, by saying we want to aim for 20 observations per parameter, if cases would be evenly split between classes. We could then estimate 290/20 = 14.5 parameters.
With 2 indicators, zero covariances, and equal variances, the number of parameters for our model is:

* k-1 mixture weights
* k*2 means
* 2 variances

Which would give this many cases per parameter:


``` r
k <- 1:10
nrow(df) / (1+3*k)
#>  [1] 72.8 41.6 29.1 22.4 18.2 15.3 13.2 11.6 10.4  9.4
```

So we could estimate at least 4 classes. Let's round up and estimate 1:5 class solutions:


``` r
res_lpa <- mx_profiles(
  df_lpa,
  classes = 1:5,
  variances = "equal",
  covariances = "zero"
)
#> MxComputeSimAnnealing(tsallis1996) evaluations 1354 fit 2481.96 change -1152MxComputeSimAnnealing(tsallis1996) evaluations 3452 fit 4035.01 change 2204 MxComputeSimAnnealing(tsallis1996) evaluations 5508 fit 1835.13 change -246.9MxComputeSimAnnealing(tsallis1996) evaluations 7535 fit 2197.38 change -2344                                                                             
#> MxComputeSimAnnealing(tsallis1996) evaluations 1113 fit 2483.15 change 0MxComputeSimAnnealing(tsallis1996) evaluations 2527 fit 3455.88 change 677.2MxComputeSimAnnealing(tsallis1996) evaluations 3928 fit 1837.6 change -4904 MxComputeSimAnnealing(tsallis1996) evaluations 5461 fit 1825.25 change -120.9MxComputeSimAnnealing(tsallis1996) evaluations 6882 fit 2458.68 change 608.2 MxComputeSimAnnealing(tsallis1996) evaluations 8329 fit 1786.78 change -0.04701MxComputeSimAnnealing(tsallis1996) evaluations 9748 fit 1787.22 change -287.5  MxComputeSimAnnealing(tsallis1996) evaluations 11051 fit 1790.05 change -267 MxComputeSimAnnealing(tsallis1996) evaluations 12390 fit 3148.31 change 1362                                                                            
#> MxComputeSimAnnealing(tsallis1996) evaluations 1014 fit 2565.46 change 0MxComputeSimAnnealing(tsallis1996) evaluations 2064 fit 4351.51 change -282.8MxComputeSimAnnealing(tsallis1996) evaluations 3112 fit 2211.72 change 412.8 MxComputeSimAnnealing(tsallis1996) evaluations 4146 fit 2368.81 change 324.6MxComputeSimAnnealing(tsallis1996) evaluations 5193 fit 1891.89 change 101  MxComputeSimAnnealing(tsallis1996) evaluations 6242 fit 1790.23 change -698.6MxComputeSimAnnealing(tsallis1996) evaluations 7314 fit 3790.95 change 2002  MxComputeSimAnnealing(tsallis1996) evaluations 8362 fit 1789.21 change -767.6MxComputeSimAnnealing(tsallis1996) evaluations 9404 fit 2071.6 change 279    MxComputeSimAnnealing(tsallis1996) evaluations 10464 fit 2353.96 change -746.7MxComputeSimAnnealing(tsallis1996) evaluations 11500 fit 1789.34 change -234.5MxComputeSimAnnealing(tsallis1996) evaluations 12518 fit 2356.39 change -322.6MxComputeSimAnnealing(tsallis1996) evaluations 13571 fit 1789.54 change 0.4175MxComputeSimAnnealing(tsallis1996) evaluations 14625 fit 2323.04 change 533.9 MxComputeSimAnnealing(tsallis1996) evaluations 15668 fit 1789.65 change -767.9                                                                              
#> MxComputeSimAnnealing(tsallis1996) evaluations 839 fit 2371.1 change 0MxComputeSimAnnealing(tsallis1996) evaluations 1682 fit 2253.64 change 19.33MxComputeSimAnnealing(tsallis1996) evaluations 2530 fit 2256.42 change 20.04MxComputeSimAnnealing(tsallis1996) evaluations 3374 fit 2278.19 change -1599MxComputeSimAnnealing(tsallis1996) evaluations 4210 fit 2097.58 change -7.089MxComputeSimAnnealing(tsallis1996) evaluations 5057 fit 2117.04 change 244.3 MxComputeSimAnnealing(tsallis1996) evaluations 5896 fit 3228.56 change 937.3MxComputeSimAnnealing(tsallis1996) evaluations 6746 fit 2670.7 change -224.1MxComputeSimAnnealing(tsallis1996) evaluations 7608 fit 1767.88 change -371.9MxComputeSimAnnealing(tsallis1996) evaluations 8479 fit 2121.69 change 0.06272MxComputeSimAnnealing(tsallis1996) evaluations 9336 fit 1769.6 change 0.7301  MxComputeSimAnnealing(tsallis1996) evaluations 10175 fit 1773.41 change 7.947MxComputeSimAnnealing(tsallis1996) evaluations 11029 fit 1790.23 change 22.93MxComputeSimAnnealing(tsallis1996) evaluations 11879 fit 1765.63 change 0.04868MxComputeSimAnnealing(tsallis1996) evaluations 12709 fit 1765.43 change -98.68 MxComputeSimAnnealing(tsallis1996) evaluations 13554 fit 1765.62 change 0.156 MxComputeSimAnnealing(tsallis1996) evaluations 14396 fit 1765.43 change 0.008216MxComputeSimAnnealing(tsallis1996) evaluations 15233 fit 1766.9 change -285.1   MxComputeSimAnnealing(tsallis1996) evaluations 16086 fit 1769.49 change -95.28MxComputeSimAnnealing(tsallis1996) evaluations 16946 fit 1767.76 change -0.9488MxComputeSimAnnealing(tsallis1996) evaluations 17782 fit 1767.65 change 2.236  MxComputeSimAnnealing(tsallis1996) evaluations 18649 fit 1765.42 change -169.1MxComputeSimAnnealing(tsallis1996) evaluations 19519 fit 1766.96 change -18.2 MxComputeSimAnnealing(tsallis1996) evaluations 20381 fit 1863.77 change 97.2                                                                             
saveRDS(res_lpa, "res_lpa_equal.rds")
```


``` r
res_lpa <- readRDS("res_lpa_equal.rds")
```

### Class Enumeration

First, extract a model fit table:


``` r
tab_lpa_fit <- table_fit(res_lpa)
tab_lpa_fit
#>          Name Classes   LL   n Parameters  AIC  BIC saBIC Entropy prob_min prob_max n_min
#> 1 equal var 1       1 -952 291          4 1911 1926  1913    1.00     1.00     1.00 1.000
#> 2 equal var 2       2 -915 291          7 1843 1869  1847    0.79     0.77     0.98 0.158
#> 3 equal var 3       3 -893 291         10 1807 1843  1812    0.84     0.90     0.94 0.127
#> 4 equal var 4       4 -895 291         13 1815 1863  1822    0.73     0.59     0.90 0.045
#> 5 equal var 5       5 -883 291         16 1797 1856  1805    0.73     0.76     0.96 0.065
#>   n_max np_ratio np_local
#> 1  1.00       73     72.8
#> 2  0.84       42     15.3
#> 3  0.58       29     13.9
#> 4  0.44       22      5.2
#> 5  0.32       18      7.9
```

#### BIC

One well-supported criterion for class enumeration is the BIC:
a relative fit index that rewards good fit and penalizes model complexity.
The absolute BIC value has no substantive meaning; you can compare BIC values of models fitted to the same data, and lower values are preferred.
We can see that the three-class model has the lowest BIC:


``` r
which.min(tab_lpa_fit$BIC)
#> [1] 3
```


#### BLRT

We can also conduct a bootstrapped likelihood ratio test, which asks whether a model with $k$ classes improves fit over a model with $k-1$ classes.
However, this test is extremely computationally expensive.
Before we do so, it is useful to check if we can eliminate some models from consideration (also see next section).
For example, note that `np_local`, the minimum number of cases assigned to a class divided by the number of parameters in that class, is as low as 2.4 for models with 4-5 classes.
You want a healthy number of cases per parameter, so we can exclude these two models, then perform our BLRT:


``` r
res_lpa_blrt <- res_lpa
res_lpa_blrt[c(4, 5)] <- NULL
res_blrt <- BLRT(res_lpa_blrt)
saveRDS(res_blrt, "res_blrt_intro.rds")
```


``` r
res_blrt <- readRDS("res_blrt_intro.rds")
res_blrt
#> Bootstrapped Likelihood Ratio Test:
#> 
#>  null  alt   lr df blrt_p samples
#>  mix1 mix2 74.0  3      0      99
#>  mix2 mix3 42.4  3      0      95
```

We see that adding classes leads to a significant improvement in model fit.


### Eliminating Models from Consideration {#sec-eliminating}

As mentioned before, we will eliminate the 4-5 class models because they have a very low number of observations per parameter.

We can also check model diagnostics for any problems with convergence, parameter values near the boundaries, et cetera.
If one of these checks fails, you can examine a detailed report by running `mx_diagnostics(res_lpa[[model_numer]])`, where `model_number` is the number of the failing model:


``` r
sapply(res_lpa, function(i) mx_diagnostics(i)$checks_pass)
#> equal var 1 equal var 2 equal var 3 equal var 4 equal var 5 
#>        TRUE        TRUE        TRUE        TRUE        TRUE
```

Another consideration is that classification diagnostics are poor for the 4-5 class models:


``` r
tab_lpa_fit[, c("Entropy", "prob_min", "prob_max")]
#>   Entropy prob_min prob_max
#> 1    1.00     1.00     1.00
#> 2    0.79     0.77     0.98
#> 3    0.84     0.90     0.94
#> 4    0.73     0.59     0.90
#> 5    0.73     0.76     0.96
```

The entropy statistic is an overall summary of class separation, where values approaching 1 indicate clear delineation of classes [@celeuxEntropyCriterionAssessing1996].

The statistics `prob_min` and `prob_max` give us the minumum and maximum of the diagonal of the `mostlikely.class` classification table (see `?class_prob`).
If C is the true class of an observation, and N is the most likely class based on the model, then this table shows the probability P(N==i|C==j).
The diagonal represents the probability that observations in each class will be correctly classified.
We see that `prob_min` is very low for the 4-5 class models; thus,
for some classes, there is a low probability that its members will be correctly classified.
This is also reflected in the entropy.

### Reporting Results

We already obtained the model fit table.
There are many ways to prepare it for a scientific paper.
The regular formatting function in R has some peculiar behavior; 
it does not guarantee that a certain number of decimals are displayed.
This is a design choice in R, and has to do with the principle of "significant figures".
However, APA style simply requires reporting to double digits.
So let's consider two options:


``` r
# Standard R formatting, some columns get 3 digits, some get 1, but they remain
# numeric type:
tab_lpa_fit_formatted <- format(tab_lpa_fit, digits = 2)
tab_lpa_fit_formatted
#>          Name Classes   LL   n Parameters  AIC  BIC saBIC Entropy prob_min prob_max n_min
#> 1 equal var 1       1 -952 291          4 1911 1926  1913    1.00     1.00     1.00 1.000
#> 2 equal var 2       2 -915 291          7 1843 1869  1847    0.79     0.77     0.98 0.158
#> 3 equal var 3       3 -893 291         10 1807 1843  1812    0.84     0.90     0.94 0.127
#> 4 equal var 4       4 -895 291         13 1815 1863  1822    0.73     0.59     0.90 0.045
#> 5 equal var 5       5 -883 291         16 1797 1856  1805    0.73     0.76     0.96 0.065
#>   n_max np_ratio np_local
#> 1  1.00       73     72.8
#> 2  0.84       42     15.3
#> 3  0.58       29     13.9
#> 4  0.44       22      5.2
#> 5  0.32       18      7.9
# Using format_numeric; reports 2 digits, but all columns become
# character type:
tab_lpa_fit_formatted <- format_numeric(tab_lpa_fit, digits = 2)
write.csv(tab_lpa_fit_formatted, "tab_lpa_fit.csv", row.names = FALSE)
```

Based on the BIC, the elimination of the 4-5 class models, and significant BLRT tests, we would retain a 3-class model.

The class proportions of this model are:


``` r
class_prob(res_lpa[[3]], "sum.posterior")
#> $sum.posterior
#>    class count proportion
#> 1 class1   167       0.57
#> 2 class2    86       0.29
#> 3 class3    38       0.13
```

We can examine the parameters of this model as follows:


``` r
tab_lpa_res <- table_results(res_lpa[[3]])
tab_lpa_res
#>                   label est_sig   se pval      confint  class
#> 1       Means.AM.class1 4.36*** 0.07 0.00 [4.23, 4.49] class1
#> 2       Means.CM.class1 3.24*** 0.08 0.00 [3.09, 3.40] class1
#> 3   Variances.AM.class1 0.39*** 0.04 0.00 [0.30, 0.48] class1
#> 4   Variances.CM.class1 0.89*** 0.08 0.00 [0.74, 1.04] class1
#> 5       Means.AM.class2 6.09*** 0.09 0.00 [5.91, 6.27] class2
#> 6       Means.CM.class2 2.56*** 0.11 0.00 [2.34, 2.78] class2
#> 7       Means.AM.class3 1.84*** 0.12 0.00 [1.60, 2.08] class3
#> 8       Means.CM.class3 4.22*** 0.16 0.00 [3.91, 4.53] class3
#> 9  mix3.weights[1,1].NA    1.00 <NA> <NA>         <NA>   <NA>
#> 10 mix3.weights[1,2].NA 0.51*** 0.09 0.00 [0.34, 0.69]   <NA>
#> 11 mix3.weights[1,3].NA 0.23*** 0.04 0.00 [0.14, 0.32]   <NA>
```

Notice that we get one estimate for the variances (because these are equal across classes), no covariances, and unique estimates for the means across classes.

The pattern of results is easier to see when plotting it:


``` r
plot_profiles(res_lpa[[3]])
```

![](lca_introduction_files/figure-html/unnamed-chunk-26-1.png)<!-- -->

``` r
plot_density(res_lpa[[3]])
```

![](lca_introduction_files/figure-html/unnamed-chunk-26-2.png)<!-- -->

Examining the class-specific means, we see that Class 1 has moderate values on both indicators, whereas Class 2 does not have high means on both autonomous motivation (AM) and controlled motivation (CM).

The marginal (whole-sample) distributions of both indicators are approximately normal.
Combined with the relatively low class separability, this raises an important question: Do the data really contain distinct latent classes?
It is possible that the identified classes merely represent differences along continuous dimensions rather than qualitatively distinct subgroups.
We should therefore seriously consider whether a model without latent classes might provide a more parsimonious explanation of the data.

## Latent Class Analysis with Ordinal indicators

We can conduct ordinal LCA by using some of the raw items underlying the scale scores, for example:


``` r
df_lca <- df[, c("AM1", "AM2", "AM3")]
```

For a model with ordinal indicators, there is (currently) only one parametrization available.
Specifically, we represent each ordinal variable as a latent normal variable with thresholds representing the quantiles of people who score a particular score or lower.
In these models, each variable will have a number of thresholds equal to the number of response options minus one.
It is easy to see that this model will have more parameters than the LPA model. If L is the number of levels of our ordinal indicators, and M is the number of indicators:

* k-1 mixture weights
* k*((L-1)*M) thresholds

Since our indicators have 7 categories, this gives:


``` r
k <- 1:10
L <- length(unique(df_lca$AM1))
M <- ncol(df_lca)
nrow(df) / ((k-1)+k*((L-1)*M))
#>  [1] 13.9  6.8  4.5  3.3  2.7  2.2  1.9  1.7  1.5  1.3
```

Note that even for a 1-class model, we have fewer than 20 observations per parameter.
Thus, we might doubt whether we have enough data here to estimate more than one class.
For educational purposes - we will proceed, but to save time estimating overly complex models, we omit the 4-5 class models.


``` r
res_lca <- mx_lca(
  df_lca,
  classes = 1:3
)
#> Beginning initial fit attemptFit attempt 0, fit=3076.55795135632, new current best! (was 3076.55795135632)Beginning fit attempt 1 of at maximum 10 extra tries                         Beginning fit attempt 2 of at maximum 10 extra triesBeginning fit attempt 3 of at maximum 10 extra triesMxComputeGradientDescent(SLSQP) evaluations 207 fit 3112.25 change -5.341                                                                         Beginning fit attempt 4 of at maximum 10 extra triesBeginning fit attempt 5 of at maximum 10 extra triesBeginning fit attempt 6 of at maximum 10 extra triesMxComputeGradientDescent(SLSQP) evaluations 970 fit 3085.47 change -1.325                                                                         Beginning fit attempt 7 of at maximum 10 extra triesBeginning fit attempt 8 of at maximum 10 extra triesBeginning fit attempt 9 of at maximum 10 extra triesBeginning fit attempt 10 of at maximum 10 extra triesMxComputeGradientDescent(SLSQP) evaluations 926 fit 3076.56 change -0.0001775                                                                             Final run, for Hessian and/or standard errors and/or confidence intervals                                                                         
#> Beginning initial fit attemptFit attempt 0, fit=3076.55795135632, new current best! (was 3076.55795135632)Beginning fit attempt 1 of at maximum 10 extra tries                         MxComputeGradientDescent(SLSQP) evaluations 951 fit 2822.94 change -7.403MxComputeGradientDescent(SLSQP) evaluations 3203 fit 2757.52 change -0.03195                                                                            Fit attempt 1, fit=2757.45486738943, new current best! (was 3076.55795135632)Beginning fit attempt 2 of at maximum 10 extra tries                         MxComputeGradientDescent(SLSQP) evaluations 1878 fit 2758.74 change -0.1122                                                                           Beginning fit attempt 3 of at maximum 10 extra triesMxComputeGradientDescent(SLSQP) evaluations 802 fit 2890.76 change -3.417MxComputeGradientDescent(SLSQP) evaluations 2756 fit 2764.91 change -0.8369                                                                           Fit attempt 3, fit=2763.82195749593, worse than previous best (2757.45486738943)Beginning fit attempt 4 of at maximum 10 extra tries                            MxComputeGradientDescent(SLSQP) evaluations 159 fit 2958.02 change -1944MxComputeGradientDescent(SLSQP) evaluations 2045 fit 2764.58 change -3.225MxComputeGradientDescent(SLSQP) evaluations 3951 fit 2757.45 change -1.688e-05                                                                              Beginning fit attempt 5 of at maximum 10 extra triesMxComputeGradientDescent(SLSQP) evaluations 1195 fit 2775.33 change -3.261MxComputeGradientDescent(SLSQP) evaluations 3052 fit 2757.45 change -0.004885                                                                             Fit attempt 5, fit=2757.45486737805, new current best! (was 2757.45486738943)Beginning fit attempt 6 of at maximum 10 extra tries                         MxComputeGradientDescent(SLSQP) evaluations 1504 fit 2760.04 change -0.7078                                                                           Beginning fit attempt 7 of at maximum 10 extra triesMxComputeGradientDescent(SLSQP) evaluations 320 fit 2867.74 change -118.7MxComputeGradientDescent(SLSQP) evaluations 2199 fit 2770.29 change -0.1139                                                                           Beginning fit attempt 8 of at maximum 10 extra triesMxComputeGradientDescent(SLSQP) evaluations 479 fit 2774.08 change -6.426MxComputeGradientDescent(SLSQP) evaluations 2352 fit 2757.46 change -0.04123                                                                            Fit attempt 8, fit=2757.45486737507, new current best! (was 2757.45486737805)Beginning fit attempt 9 of at maximum 10 extra tries                         MxComputeGradientDescent(SLSQP) evaluations 1424 fit 2824.88 change -1.414MxComputeGradientDescent(SLSQP) evaluations 3198 fit 2758.3 change -0.1323                                                                          Beginning fit attempt 10 of at maximum 10 extra triesMxComputeGradientDescent(SLSQP) evaluations 807 fit 3107.12 change -2.63MxComputeGradientDescent(SLSQP) evaluations 2916 fit 2854.89 change -8.738MxComputeGradientDescent(SLSQP) evaluations 5128 fit 2772.3 change -0.01489                                                                           Fit attempt 10, fit=2772.29504642962, worse than previous best (2757.45486737507)Final run, for Hessian and/or standard errors and/or confidence intervals        MxComputeNumericDeriv 246/703MxComputeNumericDeriv 561/703                                                                                                      
#> Beginning initial fit attemptFit attempt 0, fit=3076.55795135631, new current best! (was 3076.55795135632)Beginning fit attempt 1 of at maximum 10 extra tries                         MxComputeGradientDescent(SLSQP) evaluations 235 fit 2932.83 change -186.7MxComputeGradientDescent(SLSQP) evaluations 1984 fit 2658.95 change -0.6951MxComputeGradientDescent(SLSQP) evaluations 3491 fit 2639.13 change -2.165 MxComputeGradientDescent(SLSQP) evaluations 5100 fit 2628.29 change -0.1363MxComputeGradientDescent(SLSQP) evaluations 6696 fit 2627.93 change -0.002172MxComputeGradientDescent(SLSQP) evaluations 8406 fit 2627.92 change -1.492e-05                                                                              Fit attempt 1, fit=2627.92477729493, new current best! (was 3076.55795135631)Beginning fit attempt 2 of at maximum 10 extra tries                         MxComputeGradientDescent(SLSQP) evaluations 117 fit 6476.66MxComputeGradientDescent(SLSQP) evaluations 1533 fit 2760.78 change -4.523MxComputeGradientDescent(SLSQP) evaluations 3042 fit 2682.59 change -2.291MxComputeGradientDescent(SLSQP) evaluations 4542 fit 2630.45 change -1.191MxComputeGradientDescent(SLSQP) evaluations 6024 fit 2628.86 change -0.01091MxComputeGradientDescent(SLSQP) evaluations 7506 fit 2628.86 change -6.86e-07                                                                             Fit attempt 2, fit=2628.85548409314, worse than previous best (2627.92477729493)Beginning fit attempt 3 of at maximum 10 extra tries                            MxComputeGradientDescent(SLSQP) evaluations 838 fit 3607.68 change -68.68MxComputeGradientDescent(SLSQP) evaluations 2133 fit 2851.72 change -31.94MxComputeGradientDescent(SLSQP) evaluations 3535 fit 2735.67 change -3.563MxComputeGradientDescent(SLSQP) evaluations 4924 fit 2672.2 change -4.11  MxComputeGradientDescent(SLSQP) evaluations 6181 fit 2584.91 change -0.6791MxComputeGradientDescent(SLSQP) evaluations 7437 fit 2582.01 change -1.186 MxComputeGradientDescent(SLSQP) evaluations 8812 fit 2572.72 change -0.2211MxComputeGradientDescent(SLSQP) evaluations 10180 fit 2572.51 change -0.001475MxComputeGradientDescent(SLSQP) evaluations 11776 fit 2572.5 change -0.00118  MxComputeGradientDescent(SLSQP) evaluations 13144 fit 2572.49 change -4.074e-05MxComputeGradientDescent(SLSQP) evaluations 14401 fit 2572.48 change -9.709e-06MxComputeGradientDescent(SLSQP) evaluations 15656 fit 2572.48 change -0.0001146MxComputeGradientDescent(SLSQP) evaluations 16933 fit 2572.48 change -3.905e-05MxComputeGradientDescent(SLSQP) evaluations 18098 fit 2572.48 change -5.999e-05MxComputeGradientDescent(SLSQP) evaluations 19369 fit 2572.47 change -0.008841 MxComputeGradientDescent(SLSQP) evaluations 20527 fit 2572.44 change -0.004264MxComputeGradientDescent(SLSQP) evaluations 21789 fit 2572.38 change -4.333e-07MxComputeGradientDescent(SLSQP) evaluations 22929 fit 2572.38 change -1.649e-06                                                                               Fit attempt 3, fit=2572.37757501822, new current best! (was 2627.92477729493)Beginning fit attempt 4 of at maximum 10 extra tries                         MxComputeGradientDescent(SLSQP) evaluations 470 fit 2607.29 change -19.26MxComputeGradientDescent(SLSQP) evaluations 1751 fit 2580.35 change -0.5965MxComputeGradientDescent(SLSQP) evaluations 3025 fit 2573.65 change -0.1384MxComputeGradientDescent(SLSQP) evaluations 4294 fit 2572.66 change -0.09478                                                                            Fit attempt 4, fit=2572.3775750087, new current best! (was 2572.37757501822)Beginning fit attempt 5 of at maximum 10 extra tries                        MxComputeGradientDescent(SLSQP) evaluations 117 fit 6358.65MxComputeGradientDescent(SLSQP) evaluations 1176 fit 2716.57 change -3.293MxComputeGradientDescent(SLSQP) evaluations 2456 fit 2671.77 change -6.257MxComputeGradientDescent(SLSQP) evaluations 3733 fit 2608.11 change -5.298MxComputeGradientDescent(SLSQP) evaluations 4887 fit 2575.73 change -3.079MxComputeGradientDescent(SLSQP) evaluations 6027 fit 2572.42 change -0.01491                                                                            Fit attempt 5, fit=2572.37759073324, worse than previous best (2572.3775750087)Beginning fit attempt 6 of at maximum 10 extra tries                           MxComputeGradientDescent(SLSQP) evaluations 1176 fit 2735.5 change -4.991MxComputeGradientDescent(SLSQP) evaluations 2459 fit 2650.41 change -1.784MxComputeGradientDescent(SLSQP) evaluations 3731 fit 2638.56 change -0.1122MxComputeGradientDescent(SLSQP) evaluations 4880 fit 2636.48 change -0.7118MxComputeGradientDescent(SLSQP) evaluations 6139 fit 2609.97 change -3.217 MxComputeGradientDescent(SLSQP) evaluations 7279 fit 2602.93 change -0.003328MxComputeGradientDescent(SLSQP) evaluations 8533 fit 2602.92 change -4.604e-07                                                                              Fit attempt 6, fit=2602.92155913919, worse than previous best (2572.3775750087)Beginning fit attempt 7 of at maximum 10 extra tries                           MxComputeGradientDescent(SLSQP) evaluations 957 fit 2906.66 change -74.79MxComputeGradientDescent(SLSQP) evaluations 2252 fit 2768.76 change -11.54MxComputeGradientDescent(SLSQP) evaluations 3528 fit 2732.93 change -2.459MxComputeGradientDescent(SLSQP) evaluations 4798 fit 2672.16 change -8.534MxComputeGradientDescent(SLSQP) evaluations 6056 fit 2614.91 change -1.834MxComputeGradientDescent(SLSQP) evaluations 7310 fit 2574.41 change -0.01942MxComputeGradientDescent(SLSQP) evaluations 8564 fit 2573.42 change -0.01615MxComputeGradientDescent(SLSQP) evaluations 9818 fit 2572.48 change -7.702e-05                                                                              Fit attempt 7, fit=2572.48494290614, worse than previous best (2572.3775750087)Beginning fit attempt 8 of at maximum 10 extra tries                           MxComputeGradientDescent(SLSQP) evaluations 828 fit 2739.5 change -23.04MxComputeGradientDescent(SLSQP) evaluations 2112 fit 2684.26 change -5.996MxComputeGradientDescent(SLSQP) evaluations 3387 fit 2652.15 change -0.6092MxComputeGradientDescent(SLSQP) evaluations 4656 fit 2596.07 change -8.069 MxComputeGradientDescent(SLSQP) evaluations 5914 fit 2572.67 change -0.2454MxComputeGradientDescent(SLSQP) evaluations 7168 fit 2572.48 change -4.348e-07MxComputeGradientDescent(SLSQP) evaluations 8544 fit 2572.42 change -0.008128 MxComputeGradientDescent(SLSQP) evaluations 9798 fit 2572.38 change -4.734e-08                                                                              Beginning fit attempt 9 of at maximum 10 extra triesMxComputeGradientDescent(SLSQP) evaluations 1069 fit 3335.01 change -60.59MxComputeGradientDescent(SLSQP) evaluations 2245 fit 2957.53 change -120.5MxComputeGradientDescent(SLSQP) evaluations 3296 fit 2809.71 change -3.397MxComputeGradientDescent(SLSQP) evaluations 4329 fit 2751.45 change -2.638MxComputeGradientDescent(SLSQP) evaluations 5355 fit 2748.18 change -0.03322MxComputeGradientDescent(SLSQP) evaluations 6381 fit 2747.98 change -0.001428MxComputeGradientDescent(SLSQP) evaluations 7407 fit 2747.98 change -6.696e-07MxComputeGradientDescent(SLSQP) evaluations 8319 fit 2747.98 change -8.235e-06MxComputeGradientDescent(SLSQP) evaluations 9345 fit 2747.98 change -1.022e-05MxComputeGradientDescent(SLSQP) evaluations 10143 fit 2747.98 change -3.06e-06                                                                              Fit attempt 9, fit=2747.976691787, worse than previous best (2572.3775750087)Beginning fit attempt 10 of at maximum 10 extra tries                        MxComputeGradientDescent(SLSQP) evaluations 355 fit 7090.3 change -463.8MxComputeGradientDescent(SLSQP) evaluations 1421 fit 4031.23 change -42.1MxComputeGradientDescent(SLSQP) evaluations 2371 fit 3556.53 change -57.05MxComputeGradientDescent(SLSQP) evaluations 3315 fit 3099.63 change -25.54MxComputeGradientDescent(SLSQP) evaluations 4355 fit 2869.51 change -6.864MxComputeGradientDescent(SLSQP) evaluations 5392 fit 2805.67 change -9.376MxComputeGradientDescent(SLSQP) evaluations 6419 fit 2792.83 change -0.729MxComputeGradientDescent(SLSQP) evaluations 7445 fit 2788.71 change -0.5331MxComputeGradientDescent(SLSQP) evaluations 8471 fit 2785.46 change -0.06649MxComputeGradientDescent(SLSQP) evaluations 9497 fit 2784.63 change -0.1087 MxComputeGradientDescent(SLSQP) evaluations 10523 fit 2783.56 change -0.4024MxComputeGradientDescent(SLSQP) evaluations 11435 fit 2782.22 change -0.1345MxComputeGradientDescent(SLSQP) evaluations 12347 fit 2781.77 change -0.08574MxComputeGradientDescent(SLSQP) evaluations 13487 fit 2780.9 change -0.05752 MxComputeGradientDescent(SLSQP) evaluations 14399 fit 2780.74 change -0.0008138MxComputeGradientDescent(SLSQP) evaluations 15425 fit 2780.39 change -0.008286 MxComputeGradientDescent(SLSQP) evaluations 16452 fit 2779.6 change -0.3323   MxComputeGradientDescent(SLSQP) evaluations 17601 fit 2740.46 change -11.71MxComputeGradientDescent(SLSQP) evaluations 18641 fit 2635.63 change -2.771MxComputeGradientDescent(SLSQP) evaluations 19669 fit 2580.85 change -0.3936MxComputeGradientDescent(SLSQP) evaluations 20812 fit 2575.23 change -0.5824MxComputeGradientDescent(SLSQP) evaluations 21838 fit 2574.66 change -0.0009908MxComputeGradientDescent(SLSQP) evaluations 22978 fit 2574.64 change -0.0015   MxComputeGradientDescent(SLSQP) evaluations 24121 fit 2573.23 change -0.4036MxComputeGradientDescent(SLSQP) evaluations 25148 fit 2572.9 change -2.125e-05                                                                              Fit attempt 10, fit=2572.90464047946, worse than previous best (2572.3775750087)Final run, for Hessian and/or standard errors and/or confidence intervals       MxComputeNumericDeriv 82/1596MxComputeNumericDeriv 296/1596MxComputeNumericDeriv 506/1596MxComputeNumericDeriv 723/1596MxComputeNumericDeriv 923/1596MxComputeNumericDeriv 1130/1596MxComputeNumericDeriv 1332/1596MxComputeNumericDeriv 1543/1596                                                                                                        
saveRDS(res_lca, "res_lca_intro.rds")
```


``` r
res_lca <- readRDS("res_lca_intro.rds")
tab_lca_fit <- table_fit(res_lca)
#> MxComputeGradientDescent(SLSQP) evaluations 335 fit 2813.17 change -20.23MxComputeGradientDescent(SLSQP) evaluations 996 fit 2686.34 change -39.18MxComputeGradientDescent(SLSQP) evaluations 1622 fit 2561.72 change -0.007703                                                                             MxComputeGradientDescent(SLSQP) evaluations 163 fit 3126.19 change -117.7                                                                         
tab_lca_fit
#>   Name Classes    LL   n Parameters  AIC  BIC saBIC Entropy prob_min prob_max n_min n_max
#> 1    1       1 -1538 291         18 3113 3179  3122    1.00     1.00     1.00  1.00  1.00
#> 2    2       2 -1379 291         37 2831 2967  2850    0.94     0.97     0.99  0.23  0.77
#> 3    3       3 -1286 291         56 2684 2890  2712    0.93     0.95     0.99  0.23  0.45
#>   warning np_ratio np_local
#> 1      NA     16.2     16.2
#> 2    TRUE      7.9      3.7
#> 3    TRUE      5.2      3.7
```

The BIC indicates that adding classes improves model fit.
However, even for the 2-class model, the number of cases/parameters in the smallest class (`np_local`) is smaller than 10.
Thus, we might be skeptical about selecting more than 1 class for these data.
For educational purposes, however, let's examine the 2-class solution.


``` r
tab_lca_res <- table_results(res_lca[[2]])
tab_lca_res
#>                            label est_sig     se pval           confint  class
#> 1           Variances.AM1.class1    1.00   <NA> <NA>              <NA> class1
#> 2           Variances.AM2.class1    1.00   <NA> <NA>              <NA> class1
#> 3           Variances.AM3.class1    1.00   <NA> <NA>              <NA> class1
#> 4  class1.Thresholds[1,1].class1   -9.05  27.22 0.74   [-62.40, 44.29] class1
#> 5  class1.Thresholds[1,2].class1  -17.42  27.82 0.53   [-71.94, 37.10] class1
#> 6  class1.Thresholds[1,3].class1  -18.00  27.78 0.52   [-72.46, 36.46] class1
#> 7  class1.Thresholds[2,1].class1   -2.12  54.44 0.97 [-108.81, 104.58] class1
#> 8  class1.Thresholds[2,2].class1  -13.11  55.64 0.81  [-122.15, 95.94] class1
#> 9  class1.Thresholds[2,3].class1  -15.76  55.57 0.78  [-124.67, 93.15] class1
#> 10 class1.Thresholds[3,1].class1   -1.90  81.81 0.98 [-162.24, 158.44] class1
#> 11 class1.Thresholds[3,2].class1   -1.99  83.45 0.98 [-165.56, 161.57] class1
#> 12 class1.Thresholds[3,3].class1   -2.10  83.35 0.98 [-165.47, 161.27] class1
#> 13 class1.Thresholds[4,1].class1   -0.20 108.04 1.00 [-211.95, 211.55] class1
#> 14 class1.Thresholds[4,2].class1   -0.50 110.24 1.00 [-216.56, 215.55] class1
#> 15 class1.Thresholds[4,3].class1   -0.38 109.93 1.00 [-215.84, 215.08] class1
#> 16 class1.Thresholds[5,1].class1    0.37 125.42 1.00 [-245.46, 246.20] class1
#> 17 class1.Thresholds[5,2].class1    0.14 129.66 1.00 [-253.98, 254.27] class1
#> 18 class1.Thresholds[5,3].class1    0.26 128.24 1.00 [-251.09, 251.61] class1
#> 19 class1.Thresholds[6,1].class1    1.27 133.02 0.99 [-259.44, 261.98] class1
#> 20 class1.Thresholds[6,2].class1    1.11 138.75 0.99 [-270.85, 273.06] class1
#> 21 class1.Thresholds[6,3].class1    0.99 137.90 0.99 [-269.28, 271.27] class1
#> 22          Variances.AM1.class2    1.00   <NA> <NA>              <NA> class2
#> 23          Variances.AM2.class2    1.00   <NA> <NA>              <NA> class2
#> 24          Variances.AM3.class2    1.00   <NA> <NA>              <NA> class2
#> 25 class2.Thresholds[1,1].class2   -0.14  13.64 0.99   [-26.88, 26.60] class2
#> 26 class2.Thresholds[1,2].class2   -0.64  15.09 0.97   [-30.21, 28.94] class2
#> 27 class2.Thresholds[1,3].class2   -0.55  14.40 0.97   [-28.76, 27.67] class2
#> 28 class2.Thresholds[2,1].class2    0.18  27.14 0.99   [-53.02, 53.37] class2
#> 29 class2.Thresholds[2,2].class2   -0.01  28.66 1.00   [-56.18, 56.15] class2
#> 30 class2.Thresholds[2,3].class2   -0.02  27.69 1.00   [-54.30, 54.25] class2
#> 31 class2.Thresholds[3,1].class2    1.59  29.57 0.96   [-56.37, 59.55] class2
#> 32 class2.Thresholds[3,2].class2    0.46  38.56 0.99   [-75.12, 76.04] class2
#> 33 class2.Thresholds[3,3].class2    0.83  34.02 0.98   [-65.85, 67.51] class2
#> 34 class2.Thresholds[4,1].class2    7.71  29.57 0.79   [-50.24, 65.67] class2
#> 35 class2.Thresholds[4,2].class2    0.77  46.70 0.99   [-90.75, 92.30] class2
#> 36 class2.Thresholds[4,3].class2    1.62  36.55 0.96   [-70.03, 73.26] class2
#> 37 class2.Thresholds[5,1].class2    8.24  29.57 0.78   [-49.72, 66.19] class2
#> 38 class2.Thresholds[5,2].class2    1.42  50.65 0.98  [-97.85, 100.70] class2
#> 39 class2.Thresholds[5,3].class2    8.52  36.55 0.82   [-63.12, 80.16] class2
#> 40 class2.Thresholds[6,1].class2   10.10  29.57 0.73   [-47.86, 68.05] class2
#> 41 class2.Thresholds[6,2].class2    1.43  52.30 0.98 [-101.09, 103.94] class2
#> 42 class2.Thresholds[6,3].class2   11.65  36.55 0.75   [-59.99, 83.29] class2
#> 43          mix2.weights[1,1].NA    1.00   <NA> <NA>              <NA>   <NA>
#> 44          mix2.weights[1,2].NA    0.29  46.92 0.99   [-91.67, 92.26]   <NA>
#> 
#> Note: Standard errors are unavailable or non-finite.
#> Note: Hessian is not positive definite.
```

We see that all the parameters are thresholds (and a mixture weight).

These thresholds are z-values at which a standard normal distribution would be intersected so that the proportion of participants falling below the threshold corresponds to the proportion of particiants who selected that answer category or lower.
We can plot this to give you an intuitive understanding, for example, for the thresholds for variable AM1 in class 1:


``` r
library(ggplot2)
thresholds <- res_lca[[2]]$class1$Thresholds$result[, "AM1"]
ggplot(data.frame(x = c(-4, 4)), aes(x = x)) + 
  stat_function(fun = dnorm, color = "red") +
  geom_vline(xintercept = thresholds) +
  scale_x_continuous(breaks = thresholds, labels = 1:6) +
  theme_bw()
```

![](lca_introduction_files/figure-html/unnamed-chunk-32-1.png)<!-- -->

This figure implies that the most common response to AM1 in class 1 is 6, quite a few people score 5 or 7, and very few people score 4 or below.
We can do a sanity check to confirm this interpretation by assigning people to their most likely class (note that this ignores classification uncertainty), and then tabulating the observed scores of AM1 for the people who are assigned class 1:


``` r
individual_class <- predict_class(res_lca[[2]])
table(df$AM1[individual_class == 1])
#> 
#>  1  2  3  4  5  6  7 
#>  0  4  1 89 50 57 23
```


Another way to interpret the threshold parameters is to convert them to the estimated probability of choosing each response category:


``` r
tab_lca_prob <- table_prob(res_lca[[2]])
# Let's format it for easier visual interpretation:
format_numeric(tab_lca_prob, digits = 2)
#>    Variable Category Probability  group
#> 1       AM1        1        0.00 class1
#> 2       AM1        2        0.02 class1
#> 3       AM1        3        0.01 class1
#> 4       AM1        4        0.39 class1
#> 5       AM1        5        0.22 class1
#> 6       AM1        6        0.25 class1
#> 7       AM1        7        0.10 class1
#> 8       AM2        1        0.00 class1
#> 9       AM2        2        0.00 class1
#> 10      AM2        3        0.02 class1
#> 11      AM2        4        0.28 class1
#> 12      AM2        5        0.25 class1
#> 13      AM2        6        0.31 class1
#> 14      AM2        7        0.13 class1
#> 15      AM3        1        0.00 class1
#> 16      AM3        2        0.00 class1
#> 17      AM3        3        0.02 class1
#> 18      AM3        4        0.33 class1
#> 19      AM3        5        0.25 class1
#> 20      AM3        6        0.24 class1
#> 21      AM3        7        0.16 class1
#> 22      AM1        1        0.45 class2
#> 23      AM1        2        0.13 class2
#> 24      AM1        3        0.37 class2
#> 25      AM1        4        0.06 class2
#> 26      AM1        5        0.00 class2
#> 27      AM1        6        0.00 class2
#> 28      AM1        7        0.00 class2
#> 29      AM2        1        0.26 class2
#> 30      AM2        2        0.23 class2
#> 31      AM2        3        0.18 class2
#> 32      AM2        4        0.10 class2
#> 33      AM2        5        0.14 class2
#> 34      AM2        6        0.00 class2
#> 35      AM2        7        0.08 class2
#> 36      AM3        1        0.29 class2
#> 37      AM3        2        0.20 class2
#> 38      AM3        3        0.31 class2
#> 39      AM3        4        0.15 class2
#> 40      AM3        5        0.05 class2
#> 41      AM3        6        0.00 class2
#> 42      AM3        7        0.00 class2
```

We see that the majority of people in class 1 scored a 4 on all indicators,
and the majority of people in class 2 scored a 5 or 6 on all indicators.
Thus, we could call these classes the "moderate AM" class, and the "high AM" class, respectively.

We can also plot this information, leading to the same conclusion:


``` r
plot_prob(res_lca[[2]])
```

![](lca_introduction_files/figure-html/unnamed-chunk-35-1.png)<!-- -->


## Dichotomous indicators: `mx_lca()`

The Zhao dataset does not contain any binary indicators.
To practice LCA with binary indicators, we will dichotomize the indicators from the previous example to practice.
<!-- , or you can load and prepare a different dataset. -->
<!-- Choose your own adventure. -->

<!-- ### Dichotomizing the Zhao Data -->

Note that in real-world analyses, one should (almost) never artificially dichotomize variables, because this loses information.
Because we saw that the main differences between the two classes in the previous example was between people scoring mostly 4 (class 1) versus 5 and 6 (class 2),
it makes sense to cut the variables at this value:


``` r
df_bin <- df_lca
df_bin[] <- lapply(df_bin, function(x) ordered(as.integer(x <= 4)))
```

<!-- ### Loading a New Dataset -->

<!-- Alternatively, we can use a different dataset on depressive symptoms in adolescent athletes: -->

<!-- > Renton, Tian, 2018, "Depression & Concussion-Like Symptoms in Adolescent Athletes in the Absence of Recent Injury", https://doi.org/10.7910/DVN/5YF5XJ, Harvard Dataverse, V2 -->

<!-- Actually - these indicators of depression are also not binary; they're coded: -->

<!-- * Not true = 0 -->
<!-- * Sometimes = 1 -->
<!-- * True = 2 -->

<!-- For this exercise, we'll collapse any answers that exceed 0. -->

<!-- ```{r eval = FALSE} -->
<!-- df_dep <- readxl::read_xlsx("Normative Athlete Data - PCSS & MFQ.xlsx", sheet = 1) -->
<!-- df_bin <- df_dep[, grep("MFQ\\s?\\d", names(df_dep))][, -1] -->
<!-- names(df_bin) <- gsub(" ", "", names(df_bin)) -->
<!-- df_bin[] <- lapply(df_bin, function(x){ordered(x > 0)}) -->
<!-- ``` -->


### Model Fitting

The number of parameters is calculated the same as for the other ordinal LCA example, but now, the number of levels L is 2.
If we divide the number of cases by the number of parameters, we see that we could estimate up to 4 classes while still having, on average, about 20 observations per parameter:


``` r
k <- 1:10
L <- 2
M <- ncol(df_lca)
nrow(df) / ((k-1)+k*((L-1)*M))
#>  [1] 97.0 41.6 26.5 19.4 15.3 12.7 10.8  9.4  8.3  7.5
```

We can fit the model:


``` r
res_bin <- mx_lca(
  df_bin,
  classes = 1:4
)
#> Beginning initial fit attemptFit attempt 0, fit=1191.2490839595, new current best! (was 1191.2490839595)Beginning fit attempt 1 of at maximum 10 extra tries                       Beginning fit attempt 2 of at maximum 10 extra triesBeginning fit attempt 3 of at maximum 10 extra triesFit attempt 3, fit=1191.2490839595, new current best! (was 1191.2490839595)Beginning fit attempt 4 of at maximum 10 extra tries                       Beginning fit attempt 5 of at maximum 10 extra triesBeginning fit attempt 6 of at maximum 10 extra triesBeginning fit attempt 7 of at maximum 10 extra triesBeginning fit attempt 8 of at maximum 10 extra triesBeginning fit attempt 9 of at maximum 10 extra triesBeginning fit attempt 10 of at maximum 10 extra triesFinal run, for Hessian and/or standard errors and/or confidence intervals                                                                         
#> Beginning initial fit attemptFit attempt 0, fit=1191.2490839595, new current best! (was 1191.2490839595)Beginning fit attempt 1 of at maximum 10 extra tries                       Fit attempt 1, fit=1040.8033489588, new current best! (was 1191.2490839595)Beginning fit attempt 2 of at maximum 10 extra tries                       Beginning fit attempt 3 of at maximum 10 extra triesFit attempt 3, fit=1040.80334895879, new current best! (was 1040.8033489588)Beginning fit attempt 4 of at maximum 10 extra tries                        Beginning fit attempt 5 of at maximum 10 extra triesFit attempt 5, fit=1040.80334895879, new current best! (was 1040.80334895879)Beginning fit attempt 6 of at maximum 10 extra tries                         Beginning fit attempt 7 of at maximum 10 extra triesBeginning fit attempt 8 of at maximum 10 extra triesBeginning fit attempt 9 of at maximum 10 extra triesBeginning fit attempt 10 of at maximum 10 extra triesFinal run, for Hessian and/or standard errors and/or confidence intervals                                                                         
#> Beginning initial fit attemptFit attempt 0, fit=1191.2490839595, new current best! (was 1191.2490839595)Beginning fit attempt 1 of at maximum 10 extra tries                       Fit attempt 1, fit=985.934481289332, new current best! (was 1191.2490839595)Beginning fit attempt 2 of at maximum 10 extra tries                        Fit attempt 2, fit=985.934481289203, new current best! (was 985.934481289332)Beginning fit attempt 3 of at maximum 10 extra tries                         MxComputeGradientDescent(SLSQP) evaluations 54 fit 988.24 change -11.17                                                                       Beginning fit attempt 4 of at maximum 10 extra triesBeginning fit attempt 5 of at maximum 10 extra triesBeginning fit attempt 6 of at maximum 10 extra triesBeginning fit attempt 7 of at maximum 10 extra triesBeginning fit attempt 8 of at maximum 10 extra triesBeginning fit attempt 9 of at maximum 10 extra triesBeginning fit attempt 10 of at maximum 10 extra triesFinal run, for Hessian and/or standard errors and/or confidence intervals                                                                         
#> Beginning initial fit attemptFit attempt 0, fit=1191.2490839595, new current best! (was 1191.2490839595)Beginning fit attempt 1 of at maximum 10 extra tries                       Fit attempt 1, fit=985.93448128921, new current best! (was 1191.2490839595)Beginning fit attempt 2 of at maximum 10 extra tries                       Beginning fit attempt 3 of at maximum 10 extra triesBeginning fit attempt 4 of at maximum 10 extra triesBeginning fit attempt 5 of at maximum 10 extra triesBeginning fit attempt 6 of at maximum 10 extra triesFit attempt 6, fit=985.934481289206, new current best! (was 985.93448128921)Beginning fit attempt 7 of at maximum 10 extra tries                        MxComputeGradientDescent(SLSQP) evaluations 36 fit 994.158                                                          Beginning fit attempt 8 of at maximum 10 extra triesBeginning fit attempt 9 of at maximum 10 extra triesBeginning fit attempt 10 of at maximum 10 extra triesFinal run, for Hessian and/or standard errors and/or confidence intervals                                                                         
saveRDS(res_bin, "res_bin_intro.rds")
```


``` r
res_bin <- readRDS("res_bin_intro.rds")
tab_bin_fit <- table_fit(res_bin)
tab_bin_fit
#>   Name Classes   LL   n Parameters  AIC  BIC saBIC Entropy prob_min prob_max n_min n_max
#> 1    1       1 -596 291          3 1197 1208  1199    1.00     1.00     1.00  1.00  1.00
#> 2    2       2 -520 291          7 1055 1081  1058    0.83     0.93     0.99  0.39  0.61
#> 3    3       3 -493 291         11 1008 1048  1013    0.65     0.31     0.98  0.11  0.48
#> 4    4       4 -493 291         15 1016 1071  1023    0.56     0.00     1.00  0.00  0.42
#>   warning np_ratio np_local
#> 1      NA       97       97
#> 2    TRUE       42       38
#> 3    TRUE       26       11
#> 4    TRUE       19        0
```

Here, we see that the 2-class solution is best according to the BIC, and that `np_local` is near-zero for the 3- and 4-class solution.
This is happens because (almost) no observations are allocated to those classes, see `n_min`.
Thus, these classes are superfluous.

We can examine the results for the 2-class solution.
Note that, because there are only two response categories, we can ignore half the table because the response proportions sum to 1, so half the rows are just one minus the other half of the rows.
The table now only shows what percentage of participants scored 1 on each indicator:


``` r
tab_bin_prob <- table_prob(res_bin[[2]])
tab_bin_prob <- tab_bin_prob[!tab_bin_prob$Category == 1, -2]
# Let's format it for easier visual interpretation:
format_numeric(tab_bin_prob, digits = 2)
#>    Variable Probability  group
#> 2       AM1        0.99 class1
#> 4       AM2        0.80 class1
#> 6       AM3        1.00 class1
#> 8       AM1        0.32 class2
#> 10      AM2        0.21 class2
#> 12      AM3        0.21 class2
```

We see that people in class 2 tend to score 1 more often than people in class 1.
This tells us that AM1 is most discriminating (nearly all people in class 2 endorse it, but nearly nobody in class 1), whereas AM2 is less discriminating.

We can also plot this information:


``` r
plot_prob(res_bin[[2]])
```

![](lca_introduction_files/figure-html/unnamed-chunk-41-1.png)<!-- -->


## Mixed indicator types: `mx_mixed_lca()`

Many applied datasets contain continuous outcomes alongside ordered or binary indicators. `mx_mixed_lca()` is designed for this setting.
As an example, we can combine `df_bin` with the scale score of `CM`:


``` r
df_mixed <- data.frame(df[, "CM", drop = FALSE], df_bin)
```

The parameters are now given as:

* k-1 mixture weights
* k*((L-1)*ncol(df_lca)) thresholds
* k means for CM
* 1 variance for CM (if we keep it constant across classes)
* No covariances


``` r
k <- 1:10
L <- 2
M <- ncol(df_lca)
nrow(df) / ((k-1)+k*((L-1)*M) + 1 + k) 
#>  [1] 58.2 29.1 19.4 14.6 11.6  9.7  8.3  7.3  6.5  5.8
```

So following our rule of thumb, we could safely estimate up to 3 classes:


``` r
res_mixed <- mx_mixed_lca(
  df_mixed,
  classes = 1:3
)
#> MxComputeSimAnnealing(tsallis1996) evaluations 839 fit 3445.92 change 0MxComputeSimAnnealing(tsallis1996) evaluations 1883 fit 2586.2 change 0MxComputeSimAnnealing(tsallis1996) evaluations 2860 fit 1844.31 change 5.227MxComputeSimAnnealing(tsallis1996) evaluations 3832 fit 3323.57 change 0    MxComputeSimAnnealing(tsallis1996) evaluations 4764 fit 3043.16 change 1091MxComputeSimAnnealing(tsallis1996) evaluations 5712 fit 1824.02 change -1581MxComputeSimAnnealing(tsallis1996) evaluations 6655 fit 2534.14 change 709.9MxComputeSimAnnealing(tsallis1996) evaluations 7597 fit 3077.35 change 1243 MxComputeSimAnnealing(tsallis1996) evaluations 8517 fit 1824.91 change -180.9MxComputeSimAnnealing(tsallis1996) evaluations 9465 fit 1824.36 change 0.3513MxComputeSimAnnealing(tsallis1996) evaluations 10375 fit 1824.01 change -4.976MxComputeSimAnnealing(tsallis1996) evaluations 11304 fit 1824.08 change 0.07132MxComputeSimAnnealing(tsallis1996) evaluations 12226 fit 1824.01 change -28.23                                                                               
#> MxComputeSimAnnealing(tsallis1996) evaluations 182 fit 2210.3 change -731.2MxComputeSimAnnealing(tsallis1996) evaluations 854 fit 2941.54 change 0    MxComputeSimAnnealing(tsallis1996) evaluations 1525 fit 2854.66 change 0MxComputeSimAnnealing(tsallis1996) evaluations 2186 fit 2854.66 change 0MxComputeSimAnnealing(tsallis1996) evaluations 2848 fit 2778.4 change 419.2MxComputeSimAnnealing(tsallis1996) evaluations 3486 fit 2019.83 change 48.25MxComputeSimAnnealing(tsallis1996) evaluations 4120 fit 2466.54 change 0    MxComputeSimAnnealing(tsallis1996) evaluations 4745 fit 1884.81 change -78.1MxComputeSimAnnealing(tsallis1996) evaluations 5372 fit 1825.31 change -1523MxComputeSimAnnealing(tsallis1996) evaluations 6007 fit 7659.07 change 5751 MxComputeSimAnnealing(tsallis1996) evaluations 6645 fit 1818.43 change 0.8217MxComputeSimAnnealing(tsallis1996) evaluations 7279 fit 1925.15 change 0     MxComputeSimAnnealing(tsallis1996) evaluations 7897 fit 6510.39 change 4691MxComputeSimAnnealing(tsallis1996) evaluations 8515 fit 2157.38 change 339.1MxComputeSimAnnealing(tsallis1996) evaluations 9131 fit 1818.14 change -20.92MxComputeSimAnnealing(tsallis1996) evaluations 9747 fit 2224.04 change 406.5 MxComputeSimAnnealing(tsallis1996) evaluations 10362 fit 2344.94 change 0   MxComputeSimAnnealing(tsallis1996) evaluations 10948 fit 3611.29 change 1794MxComputeSimAnnealing(tsallis1996) evaluations 11544 fit 1817.24 change 0.05512MxComputeSimAnnealing(tsallis1996) evaluations 12116 fit 1828.43 change 11.21  MxComputeSimAnnealing(tsallis1996) evaluations 12706 fit 1818.96 change -1810MxComputeSimAnnealing(tsallis1996) evaluations 13341 fit 1817.28 change -95.15MxComputeSimAnnealing(tsallis1996) evaluations 13943 fit 1817.19 change -0.0006145MxComputeSimAnnealing(tsallis1996) evaluations 14548 fit 1819.68 change 2.492     MxComputeSimAnnealing(tsallis1996) evaluations 15120 fit 1819.91 change 2.245MxComputeSimAnnealing(tsallis1996) evaluations 15712 fit 1817.19 change -0.002278MxComputeSimAnnealing(tsallis1996) evaluations 16324 fit 1817.18 change 2.352e-06MxComputeSimAnnealing(tsallis1996) evaluations 16924 fit 1817.19 change -94.12   MxComputeSimAnnealing(tsallis1996) evaluations 17481 fit 2279.22 change 462   MxComputeSimAnnealing(tsallis1996) evaluations 18073 fit 3608.99 change 1792MxComputeSimAnnealing(tsallis1996) evaluations 18631 fit 1817.18 change -0.332MxComputeSimAnnealing(tsallis1996) evaluations 19216 fit 1817.25 change -1.152                                                                              
#> Beginning initial fit attemptFit attempt 0, fit=1817.17759927602, new current best! (was 1817.17759927603)Beginning fit attempt 1 of at maximum 10 extra tries                         MxComputeGradientDescent(SLSQP) evaluations 454 fit 1827.5 change -1.119MxComputeGradientDescent(SLSQP) evaluations 1457 fit 1812.42 change -0.0003981                                                                              Fit attempt 1, fit=1812.41758632867, new current best! (was 1817.17759927602)Beginning fit attempt 2 of at maximum 10 extra tries                         MxComputeGradientDescent(SLSQP) evaluations 811 fit 1837.4 change -0.6629MxComputeGradientDescent(SLSQP) evaluations 2241 fit 1830.04 change -0.0001283                                                                              Fit attempt 2, fit=1830.0374329713, worse than previous best (1812.41758632867)Beginning fit attempt 3 of at maximum 10 extra tries                           MxComputeGradientDescent(SLSQP) evaluations 446 fit 1812.48 change -0.2234                                                                          Beginning fit attempt 4 of at maximum 10 extra triesMxComputeGradientDescent(SLSQP) evaluations 1499 fit 2056.17 change -0.00143                                                                            Fit attempt 4, fit=2056.16156919076, worse than previous best (1812.41758632867)Beginning fit attempt 5 of at maximum 10 extra tries                            MxComputeGradientDescent(SLSQP) evaluations 378 fit 1813.69 change -1.001                                                                         Fit attempt 5, fit=1812.41760007133, worse than previous best (1812.41758632867)Beginning fit attempt 6 of at maximum 10 extra tries                            MxComputeGradientDescent(SLSQP) evaluations 967 fit 1836.16 change -0.0109MxComputeGradientDescent(SLSQP) evaluations 2391 fit 1833.97 change -3.164e-05                                                                              Fit attempt 6, fit=1829.80298406985, worse than previous best (1812.41758632867)Beginning fit attempt 7 of at maximum 10 extra tries                            MxComputeGradientDescent(SLSQP) evaluations 278 fit 1833.33 change -7.455                                                                         Fit attempt 7, fit=1812.41758614693, new current best! (was 1812.41758632867)Beginning fit attempt 8 of at maximum 10 extra tries                         MxComputeGradientDescent(SLSQP) evaluations 510 fit 1812.48 change -0.1278                                                                          Beginning fit attempt 9 of at maximum 10 extra triesMxComputeGradientDescent(SLSQP) evaluations 1085 fit 1827.5 change -0.04015MxComputeGradientDescent(SLSQP) evaluations 2493 fit 1827.38 change -1.906e-07                                                                              Fit attempt 9, fit=1827.37644993461, worse than previous best (1812.41758614693)Beginning fit attempt 10 of at maximum 10 extra tries                           Fit attempt 10, fit=1812.41758580936, new current best! (was 1812.41758614693)Final run, for Hessian and/or standard errors and/or confidence intervals     MxComputeNumericDeriv 10/120                                                                                                     
saveRDS(res_mixed, "res_mixed_intro.rds")
```


``` r
res_mixed <- readRDS("res_mixed_intro.rds")
tab_mixed_fit <- table_fit(res_mixed)
#> MxComputeGradientDescent(SLSQP) evaluations 248 fit 2082.62 change -2.512                                                                         
tab_mixed_fit
#>     Name Classes    LL   n Parameters  AIC  BIC saBIC Entropy prob_min prob_max n_min n_max
#> 1  equal       1 -1028 291          5 2066 2085  2069    1.00     1.00     1.00  1.00  1.00
#> 2 equal1       2  -912 291         10 1844 1881  1849    0.80     0.93     0.96  0.49  0.51
#> 3 equal2       3  -906 291         15 1842 1898  1850    0.93     0.94     1.00  0.18  0.41
#>   warning np_ratio np_local
#> 1      NA       58       58
#> 2      NA       29       32
#> 3    TRUE       19       12
```

Here, we see that the 2-class solution is best according to the BIC, and that `np_local` is low for the 3-class solution.
We can examine the results for the 2-class solution.
Note that these results will now be a combination of means/variances and probabilities (thresholds):


``` r
tab_mixed_res <- table_results(res_mixed[[2]])
tab_mixed_res
#>                            label  est_sig   se pval        confint  class
#> 1                Means.CM.class1  2.81*** 0.09 0.00   [2.63, 2.99] class1
#> 2            Variances.CM.class1  1.03*** 0.09 0.00   [0.86, 1.20] class1
#> 3           Variances.AM1.class1     1.00 <NA> <NA>           <NA> class1
#> 4           Variances.AM2.class1     1.00 <NA> <NA>           <NA> class1
#> 5           Variances.AM3.class1     1.00 <NA> <NA>           <NA> class1
#> 6  class1.Thresholds[1,1].class1  1.15*** 0.21 0.00   [0.74, 1.57] class1
#> 7  class1.Thresholds[1,2].class1  1.85*** 0.31 0.00   [1.25, 2.46] class1
#> 8  class1.Thresholds[1,3].class1  0.96*** 0.15 0.00   [0.66, 1.26] class1
#> 9                Means.CM.class2  3.50*** 0.09 0.00   [3.32, 3.67] class2
#> 10          Variances.AM1.class2     1.00 <NA> <NA>           <NA> class2
#> 11          Variances.AM2.class2     1.00 <NA> <NA>           <NA> class2
#> 12          Variances.AM3.class2     1.00 <NA> <NA>           <NA> class2
#> 13 class2.Thresholds[1,1].class2 -1.54*** 0.23 0.00 [-2.00, -1.09] class2
#> 14 class2.Thresholds[1,2].class2 -0.71*** 0.15 0.00 [-0.99, -0.42] class2
#> 15 class2.Thresholds[1,3].class2 -0.75*** 0.13 0.00 [-1.01, -0.49] class2
#> 16   equal var 2.weights[1,1].NA     1.00 <NA> <NA>           <NA>   <NA>
#> 17   equal var 2.weights[1,2].NA  1.11*** 0.17 0.00   [0.77, 1.45]   <NA>
```

We can plot the continuous indicators:


``` r
plot_density(res_mixed[[2]], variables = "CM")
```

![](lca_introduction_files/figure-html/unnamed-chunk-47-1.png)<!-- -->

And the response probabilities for the binary indicators:


``` r
plot_prob(res_mixed[[2]])
```

![](lca_introduction_files/figure-html/unnamed-chunk-48-1.png)<!-- -->


The interpretation now depends on the indicator: describe class-specific means for continuous variables, response-category probabilities for ordinal variables, and endorsement probabilities for dichotomous variables.

## Classification Accuracy Deep Dive

After estimating a mixture model, every participant has a vector of posterior class-membership probabilities (i.e., the probability that they belong to each class).
The function `class_prob()` gives you this matrix, and several other diagnostic tables based on it:


``` r
cp <- class_prob(res_lpa[[3]])
format_numeric(head(cp$individual))
#>      class1 class2 class3 predicted
#> [1,] "1.00" "0.00" "0.00" "1.00"   
#> [2,] "0.16" "0.84" "0.00" "2.00"   
#> [3,] "0.98" "0.02" "0.00" "1.00"   
#> [4,] "0.98" "0.02" "0.00" "1.00"   
#> [5,] "0.99" "0.01" "0.00" "1.00"   
#> [6,] "0.02" "0.00" "0.98" "3.00"
```

This table can be summarized in different ways to summarize the classifications, and to diagnose classification accuracy.

For example, `$sum.posterior` gives us the column sums of the individual classification probabilities; this indicates what proportion of your sample contributes to each class (and allows people to contribute fractionally to multiple classes):


``` r
format_numeric(head(cp$sum.posterior))
#>    class  count proportion
#> 1 class1 167.04       0.57
#> 2 class2  85.72       0.29
#> 3 class3  38.24       0.13
```

If you make a forced choice, assigning every person to the class for which they have the highest probability, you would get the following statistics. Note that these numbers now have measurement error baked into them:


``` r
format_numeric(head(cp$sum.mostlikely))
#>    class count.mostlikely count.Freq proportion.mostlikely proportion.Freq
#> 1 class1                1        169                     1            0.58
#> 2 class2                2         85                     2            0.29
#> 3 class3                3         37                     3            0.13
```

In @sec-eliminating, we already encountered the average classification probabilities for the most likely class membership, whose range is represented in the fit table as `prob_min` and `prob_max`.
We can see the full matrix by calling:


``` r
format_numeric(cp$mostlikely.class)
#>           assigned.1 assigned.2 assigned.3
#> avgprob.1 "0.94"     "0.05"     "0.01"    
#> avgprob.2 "0.10"     "0.90"     "0.00"    
#> avgprob.3 "0.06"     "0.00"     "0.94"
```

On the diagonal, we find the average probability that observations belongs to the classes they are assigned to.
If the classes are well-separated, you should see large diagonal values and small off-diagonal values.
The separation here is OK; some diagonal values are not that high (.87-.88), and some off-diagonal values are non-negligible (.12-.13).
For instance, you see that the people who are assigned to class 1 have an average posterior probability of .95 for belonging to that class - which is very high. However, they also have non-zero probabilities of belonging to class 2 or 3.


## Reporting Results

Going back to the LPA example, you might report the analysis as follows:

> We conducted exploratory latent profile analysis using `tidySEM` version 0.3.0 and `OpenMx` version 2.22.11.
We followed recommended practices by @vanlissaRecommendedPracticesLatent2024.
The two continuous indicators were AM and CM. Model parameters were class-varying means and fixed variances across classes. We assumed conditional independence of these indicators within classes. Based on the available sample size, we considered a maximum of five classes. This model has 16 parameters, which allows for a maximum of about 20 observations per parameter. We selected the best-fitting model based on the BIC and significant BLRT test. We eliminated solutions with classes where the number of observations to parameters was smaller than 10.
Based on these criteria, a 3-class solution was retained.  This model had reasonable classification accuracy: entropy was .82, and the probability that observations in each class were correctly classified ranged from [.87, .95]. The smallest class contained 9.7% of participants. The model parameters are show in Table 1, and are visually represented in Figure 1. The retained classes were characterized as [label 1], [label 2], and [label 3] based on their estimated [means / category-response probabilities / endorsement probabilities].

Here is the code to produce Table 1; we extract the means and paste the SE behind it (note that you can also use `est_sig` or another value column). We do not report the variances in this table, because they are fixed across classes.
We also write it to a spreadsheet, so we can easily import it into other desktop publishing software:


``` r
tab <- table_results(res_lpa[[2]], columns = NULL)
tab <- tab[tab$Category == "Means", c("lhs", "class", "est", "se")]
tab$Mean <- glue::glue("{tab$est} ({tab$se})")
tab <- reshape(tab[, c("class", "lhs", "Mean")], direction = "wide", idvar = "class", timevar = "lhs", sep = " ")
write.csv(tab, "tab_res.csv", row.names = FALSE)
tab
#>    class     Mean AM     Mean CM
#> 1 class1 5.02 (0.08) 2.93 (0.08)
#> 5 class2 2.42 (0.30) 4.21 (0.14)
```

To produce Figure 1, use the following code.
We're saving the file to SVG format, which is a (lossless) type of vector graphic.
Other vector graphic types include PDF and EPS.
Do not save to a raster graphic type (PNG, BMP, JPG), because these images will have low quality when reproduced in print or digital publication.
The dimensions of the figure, in millimeter, correspond to A6 paper format:


``` r
p <- plot_density(res_lpa[[2]])
```

![](lca_introduction_files/figure-html/unnamed-chunk-54-1.png)<!-- -->

``` r
ggsave(filename = "figure1.svg", plot = p, width = 148, height = 105, units = "mm", dpi = 300)
p
```

![](lca_introduction_files/figure-html/unnamed-chunk-54-2.png)<!-- -->


