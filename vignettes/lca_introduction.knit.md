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
#> MxComputeSimAnnealing(tsallis1996) evaluations 1912 fit 1871.64 change -214.2MxComputeSimAnnealing(tsallis1996) evaluations 4030 fit 3275.36 change 1049  MxComputeSimAnnealing(tsallis1996) evaluations 6101 fit 1829.2 change -371.2MxComputeSimAnnealing(tsallis1996) evaluations 8169 fit 1829.75 change -93.71                                                                             
#> MxComputeSimAnnealing(tsallis1996) evaluations 182 fit 2541.32 change -83.7MxComputeSimAnnealing(tsallis1996) evaluations 1545 fit 1902.47 change -173.5MxComputeSimAnnealing(tsallis1996) evaluations 2888 fit 1816.74 change -584.8MxComputeSimAnnealing(tsallis1996) evaluations 4232 fit 2400.4 change 25.28  MxComputeSimAnnealing(tsallis1996) evaluations 5566 fit 1801.72 change -243MxComputeSimAnnealing(tsallis1996) evaluations 6911 fit 1808.53 change 7.439MxComputeSimAnnealing(tsallis1996) evaluations 8316 fit 1803.47 change -241.6MxComputeSimAnnealing(tsallis1996) evaluations 9636 fit 2297.42 change 495.8 MxComputeSimAnnealing(tsallis1996) evaluations 10971 fit 1800.64 change -525.3MxComputeSimAnnealing(tsallis1996) evaluations 12306 fit 1872.44 change 71.81                                                                              
#> MxComputeSimAnnealing(tsallis1996) evaluations 993 fit 2476.33 change 66.82MxComputeSimAnnealing(tsallis1996) evaluations 2080 fit 2309.53 change 0   MxComputeSimAnnealing(tsallis1996) evaluations 3151 fit 2197.8 change -24.94MxComputeSimAnnealing(tsallis1996) evaluations 4204 fit 1827.06 change -438.4MxComputeSimAnnealing(tsallis1996) evaluations 5230 fit 2321.95 change 0     MxComputeSimAnnealing(tsallis1996) evaluations 6292 fit 2096.35 change 87.15MxComputeSimAnnealing(tsallis1996) evaluations 7395 fit 1784.19 change -3837MxComputeSimAnnealing(tsallis1996) evaluations 8456 fit 2301.2 change 0     MxComputeSimAnnealing(tsallis1996) evaluations 9529 fit 2042.06 change 259.6MxComputeSimAnnealing(tsallis1996) evaluations 10651 fit 1782.46 change -1.561MxComputeSimAnnealing(tsallis1996) evaluations 11694 fit 1921.82 change 139.7 MxComputeSimAnnealing(tsallis1996) evaluations 12746 fit 1782.06 change -492.5MxComputeSimAnnealing(tsallis1996) evaluations 13802 fit 1781.68 change 0.0004645MxComputeSimAnnealing(tsallis1996) evaluations 14853 fit 1781.35 change -0.4683  MxComputeSimAnnealing(tsallis1996) evaluations 15856 fit 1781.17 change 0.01445                                                                               
#> MxComputeSimAnnealing(tsallis1996) evaluations 52 fit 2020.65 change -233MxComputeSimAnnealing(tsallis1996) evaluations 901 fit 2020.65 change 0  MxComputeSimAnnealing(tsallis1996) evaluations 1738 fit 4481.99 change -437.2MxComputeSimAnnealing(tsallis1996) evaluations 2583 fit 2380.18 change 0     MxComputeSimAnnealing(tsallis1996) evaluations 3483 fit 2882.91 change -649.7MxComputeSimAnnealing(tsallis1996) evaluations 4340 fit 1801.68 change -269.4MxComputeSimAnnealing(tsallis1996) evaluations 5202 fit 1772.15 change -313  MxComputeSimAnnealing(tsallis1996) evaluations 6064 fit 2121.7 change -0.4286MxComputeSimAnnealing(tsallis1996) evaluations 6904 fit 1765.95 change -533.1MxComputeSimAnnealing(tsallis1996) evaluations 7758 fit 1765.82 change -2380 MxComputeSimAnnealing(tsallis1996) evaluations 8602 fit 2917.16 change 1147 MxComputeSimAnnealing(tsallis1996) evaluations 9415 fit 2309.12 change 542.1MxComputeSimAnnealing(tsallis1996) evaluations 10262 fit 1765.8 change -52.41MxComputeSimAnnealing(tsallis1996) evaluations 11078 fit 2307.14 change 530.2MxComputeSimAnnealing(tsallis1996) evaluations 11911 fit 1767.07 change -537.2MxComputeSimAnnealing(tsallis1996) evaluations 12763 fit 1766.23 change -2527 MxComputeSimAnnealing(tsallis1996) evaluations 13609 fit 3342.7 change 1230  MxComputeSimAnnealing(tsallis1996) evaluations 14439 fit 1811.81 change -496.6MxComputeSimAnnealing(tsallis1996) evaluations 15219 fit 2089.49 change 235.2 MxComputeSimAnnealing(tsallis1996) evaluations 16038 fit 2311.08 change 526.9MxComputeSimAnnealing(tsallis1996) evaluations 16887 fit 2309.3 change 412.8 MxComputeSimAnnealing(tsallis1996) evaluations 17709 fit 1765.47 change -444.6MxComputeSimAnnealing(tsallis1996) evaluations 18521 fit 1790.95 change 17.98 MxComputeSimAnnealing(tsallis1996) evaluations 19358 fit 1766.75 change 1.28 MxComputeSimAnnealing(tsallis1996) evaluations 20198 fit 2308.6 change 543.2                                                                            
#> MxComputeSimAnnealing(tsallis1996) evaluations 1263 fit 2621.65 change 600.3MxComputeSimAnnealing(tsallis1996) evaluations 3428 fit 3094.65 change 205.5MxComputeSimAnnealing(tsallis1996) evaluations 5545 fit 2123.44 change 319.4MxComputeSimAnnealing(tsallis1996) evaluations 7686 fit 1803.44 change -103.4MxComputeSimAnnealing(tsallis1996) evaluations 9819 fit 2797.16 change 0     MxComputeSimAnnealing(tsallis1996) evaluations 11907 fit 1802.56 change -991.1                                                                              
#> MxComputeSimAnnealing(tsallis1996) evaluations 1342 fit 4973.6 change 1972MxComputeSimAnnealing(tsallis1996) evaluations 2717 fit 1817.2 change -1207MxComputeSimAnnealing(tsallis1996) evaluations 4054 fit 1805.74 change -7.847MxComputeSimAnnealing(tsallis1996) evaluations 5371 fit 2816.62 change 1022  MxComputeSimAnnealing(tsallis1996) evaluations 6737 fit 1780.55 change -1231MxComputeSimAnnealing(tsallis1996) evaluations 8116 fit 1826.43 change -755.3MxComputeSimAnnealing(tsallis1996) evaluations 9487 fit 2122.16 change 344.4 MxComputeSimAnnealing(tsallis1996) evaluations 10870 fit 1819.62 change 42.19MxComputeSimAnnealing(tsallis1996) evaluations 12258 fit 1777.22 change -1.308MxComputeSimAnnealing(tsallis1996) evaluations 13576 fit 1796.77 change 13.77 MxComputeSimAnnealing(tsallis1996) evaluations 14891 fit 1779.36 change -823.3MxComputeSimAnnealing(tsallis1996) evaluations 16196 fit 2250.66 change 470.6 MxComputeSimAnnealing(tsallis1996) evaluations 17512 fit 2139 change 361.9   MxComputeSimAnnealing(tsallis1996) evaluations 18823 fit 1777.07 change -0.00752                                                                                
#> MxComputeSimAnnealing(tsallis1996) evaluations 779 fit 2005.01 change -288.6MxComputeSimAnnealing(tsallis1996) evaluations 1861 fit 2096.75 change 0    MxComputeSimAnnealing(tsallis1996) evaluations 2949 fit 3301.2 change -880MxComputeSimAnnealing(tsallis1996) evaluations 4034 fit 2595.83 change 0  MxComputeSimAnnealing(tsallis1996) evaluations 5112 fit 2573.75 change 442.8MxComputeSimAnnealing(tsallis1996) evaluations 6177 fit 2218.54 change -0.0009781MxComputeSimAnnealing(tsallis1996) evaluations 7246 fit 1914.42 change -391.9    MxComputeSimAnnealing(tsallis1996) evaluations 8317 fit 2199.89 change 278.8 MxComputeSimAnnealing(tsallis1996) evaluations 9398 fit 2218.99 change 0.002393MxComputeSimAnnealing(tsallis1996) evaluations 10453 fit 2489.04 change 288.1  MxComputeSimAnnealing(tsallis1996) evaluations 11443 fit 1942.55 change 90.7 MxComputeSimAnnealing(tsallis1996) evaluations 12558 fit 1759.16 change -0.1172MxComputeSimAnnealing(tsallis1996) evaluations 13609 fit 1762.05 change -2090  MxComputeSimAnnealing(tsallis1996) evaluations 14665 fit 1758.94 change -236 MxComputeSimAnnealing(tsallis1996) evaluations 15743 fit 1758.84 change 0.04024MxComputeSimAnnealing(tsallis1996) evaluations 16782 fit 1758.77 change -279.8 MxComputeSimAnnealing(tsallis1996) evaluations 17803 fit 1943.18 change 183.8 MxComputeSimAnnealing(tsallis1996) evaluations 18827 fit 2520.06 change 757.5MxComputeSimAnnealing(tsallis1996) evaluations 19878 fit 2395.4 change 636.7 MxComputeSimAnnealing(tsallis1996) evaluations 20960 fit 1806.6 change 47.67MxComputeSimAnnealing(tsallis1996) evaluations 22041 fit 1758.6 change 0.005585MxComputeSimAnnealing(tsallis1996) evaluations 23121 fit 1758.58 change -5.39  MxComputeSimAnnealing(tsallis1996) evaluations 24153 fit 1759.39 change -4.855MxComputeSimAnnealing(tsallis1996) evaluations 25209 fit 1760.19 change -544.1                                                                              
#> MxComputeSimAnnealing(tsallis1996) evaluations 60 fit 7308.2 change 867.2MxComputeSimAnnealing(tsallis1996) evaluations 933 fit 4080.55 change 2055MxComputeSimAnnealing(tsallis1996) evaluations 1789 fit 2363.22 change 55.8MxComputeSimAnnealing(tsallis1996) evaluations 2606 fit 2025.44 change 0   MxComputeSimAnnealing(tsallis1996) evaluations 3432 fit 2025.44 change 0MxComputeSimAnnealing(tsallis1996) evaluations 4207 fit 1878.83 change -146.6MxComputeSimAnnealing(tsallis1996) evaluations 5011 fit 2512.97 change -3372 MxComputeSimAnnealing(tsallis1996) evaluations 5827 fit 2138.26 change 379.4MxComputeSimAnnealing(tsallis1996) evaluations 6649 fit 1915.86 change 0    MxComputeSimAnnealing(tsallis1996) evaluations 7469 fit 2017.39 change 59.67MxComputeSimAnnealing(tsallis1996) evaluations 8265 fit 1742.67 change -648.6MxComputeSimAnnealing(tsallis1996) evaluations 8906 fit 2085.74 change 325.5 MxComputeSimAnnealing(tsallis1996) evaluations 9635 fit 1741.67 change 0.3069MxComputeSimAnnealing(tsallis1996) evaluations 10472 fit 1877.06 change 0    MxComputeSimAnnealing(tsallis1996) evaluations 11286 fit 1741.09 change -1702MxComputeSimAnnealing(tsallis1996) evaluations 12116 fit 2132.93 change -358.7MxComputeSimAnnealing(tsallis1996) evaluations 12906 fit 1737.58 change -103.1MxComputeSimAnnealing(tsallis1996) evaluations 13726 fit 1737.14 change -30.81MxComputeSimAnnealing(tsallis1996) evaluations 14544 fit 2025.62 change 0     MxComputeSimAnnealing(tsallis1996) evaluations 15373 fit 1808.45 change -31.65MxComputeSimAnnealing(tsallis1996) evaluations 16199 fit 1735.52 change -106.4MxComputeSimAnnealing(tsallis1996) evaluations 17030 fit 1779.13 change 44.98 MxComputeSimAnnealing(tsallis1996) evaluations 17858 fit 1734.17 change 0.3311MxComputeSimAnnealing(tsallis1996) evaluations 18677 fit 1733.42 change -3.198MxComputeSimAnnealing(tsallis1996) evaluations 19487 fit 2668.21 change 892.8 MxComputeSimAnnealing(tsallis1996) evaluations 20317 fit 1733.09 change -0.1388MxComputeSimAnnealing(tsallis1996) evaluations 21140 fit 2671.01 change 850.6  MxComputeSimAnnealing(tsallis1996) evaluations 21966 fit 1826.02 change -854.4MxComputeSimAnnealing(tsallis1996) evaluations 22780 fit 1732.96 change -0.2001MxComputeSimAnnealing(tsallis1996) evaluations 23601 fit 1760.53 change 27.63  MxComputeSimAnnealing(tsallis1996) evaluations 24430 fit 1733.7 change 0.8711MxComputeSimAnnealing(tsallis1996) evaluations 25242 fit 1732.79 change -990.2MxComputeSimAnnealing(tsallis1996) evaluations 26062 fit 1732.78 change 0.008462MxComputeSimAnnealing(tsallis1996) evaluations 26879 fit 1854.74 change -465.7  MxComputeSimAnnealing(tsallis1996) evaluations 27699 fit 1826.23 change 77.67 MxComputeSimAnnealing(tsallis1996) evaluations 28518 fit 1751.49 change 18.7 MxComputeSimAnnealing(tsallis1996) evaluations 29332 fit 2209.03 change 466.5MxComputeSimAnnealing(tsallis1996) evaluations 30142 fit 1732.73 change -0.0003026MxComputeSimAnnealing(tsallis1996) evaluations 30964 fit 1732.94 change 0.1548    MxComputeSimAnnealing(tsallis1996) evaluations 31788 fit 2199.48 change 466.7                                                                              
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
#> MxComputeSimAnnealing(tsallis1996) evaluations 783 fit 3324.63 change 473.4MxComputeSimAnnealing(tsallis1996) evaluations 2903 fit 2023.8 change -288.9MxComputeSimAnnealing(tsallis1996) evaluations 4972 fit 4548.91 change 0    MxComputeSimAnnealing(tsallis1996) evaluations 6949 fit 1852.3 change 23.14                                                                           
#> MxComputeSimAnnealing(tsallis1996) evaluations 784 fit 2234.81 change -303.4MxComputeSimAnnealing(tsallis1996) evaluations 2153 fit 2458.2 change 0     MxComputeSimAnnealing(tsallis1996) evaluations 3503 fit 2500.36 change 601.6MxComputeSimAnnealing(tsallis1996) evaluations 4856 fit 2891.65 change 1078 MxComputeSimAnnealing(tsallis1996) evaluations 6199 fit 1786.89 change -1884MxComputeSimAnnealing(tsallis1996) evaluations 7543 fit 1787.34 change 0.5561MxComputeSimAnnealing(tsallis1996) evaluations 8872 fit 1787.79 change 1.022 MxComputeSimAnnealing(tsallis1996) evaluations 10214 fit 1786.77 change -1.417MxComputeSimAnnealing(tsallis1996) evaluations 11520 fit 1786.77 change -0.0002009                                                                                  
#> MxComputeSimAnnealing(tsallis1996) evaluations 311 fit 2565.46 change -3483MxComputeSimAnnealing(tsallis1996) evaluations 1378 fit 2565.46 change 0   MxComputeSimAnnealing(tsallis1996) evaluations 2456 fit 1957.73 change -847.1MxComputeSimAnnealing(tsallis1996) evaluations 3528 fit 2114.62 change 200.1 MxComputeSimAnnealing(tsallis1996) evaluations 4583 fit 1972.3 change 180.6 MxComputeSimAnnealing(tsallis1996) evaluations 5653 fit 1798.04 change -81.6MxComputeSimAnnealing(tsallis1996) evaluations 6706 fit 1794.35 change -69.11MxComputeSimAnnealing(tsallis1996) evaluations 7762 fit 2553.97 change 764.8 MxComputeSimAnnealing(tsallis1996) evaluations 8795 fit 3065.41 change 992.5MxComputeSimAnnealing(tsallis1996) evaluations 9859 fit 1789.12 change 0.000989MxComputeSimAnnealing(tsallis1996) evaluations 10921 fit 2564.5 change 535.5   MxComputeSimAnnealing(tsallis1996) evaluations 11962 fit 1789.11 change -774.6MxComputeSimAnnealing(tsallis1996) evaluations 13007 fit 1789.31 change 0.1982MxComputeSimAnnealing(tsallis1996) evaluations 14053 fit 1789.11 change -0.0814MxComputeSimAnnealing(tsallis1996) evaluations 15085 fit 1789.12 change -63.49 MxComputeSimAnnealing(tsallis1996) evaluations 16136 fit 1789.59 change -730.3                                                                              
#> MxComputeSimAnnealing(tsallis1996) evaluations 403 fit 2253.64 change 0MxComputeSimAnnealing(tsallis1996) evaluations 1232 fit 2234.32 change -35.08MxComputeSimAnnealing(tsallis1996) evaluations 2064 fit 2234.32 change -35.08MxComputeSimAnnealing(tsallis1996) evaluations 2908 fit 2470.83 change -1884 MxComputeSimAnnealing(tsallis1996) evaluations 3738 fit 3186.85 change 1412 MxComputeSimAnnealing(tsallis1996) evaluations 4575 fit 2115.11 change 0   MxComputeSimAnnealing(tsallis1996) evaluations 5414 fit 2279.35 change 507.4MxComputeSimAnnealing(tsallis1996) evaluations 6250 fit 1902.27 change -1313MxComputeSimAnnealing(tsallis1996) evaluations 7086 fit 1770.59 change -488.7MxComputeSimAnnealing(tsallis1996) evaluations 7920 fit 2172.94 change 151.6 MxComputeSimAnnealing(tsallis1996) evaluations 8739 fit 1765.69 change -0.1161MxComputeSimAnnealing(tsallis1996) evaluations 9546 fit 1985.74 change 219.6  MxComputeSimAnnealing(tsallis1996) evaluations 10367 fit 1789.62 change -323.9MxComputeSimAnnealing(tsallis1996) evaluations 11189 fit 1765.46 change -65.49MxComputeSimAnnealing(tsallis1996) evaluations 12007 fit 1765.44 change -218.9MxComputeSimAnnealing(tsallis1996) evaluations 12823 fit 1766.13 change 0.6945MxComputeSimAnnealing(tsallis1996) evaluations 13648 fit 2192.83 change 86.45 MxComputeSimAnnealing(tsallis1996) evaluations 14477 fit 1766.41 change -4.419MxComputeSimAnnealing(tsallis1996) evaluations 15293 fit 2223.39 change -549.5MxComputeSimAnnealing(tsallis1996) evaluations 16109 fit 1765.42 change 0.006478MxComputeSimAnnealing(tsallis1996) evaluations 16928 fit 2203.02 change 437.6   MxComputeSimAnnealing(tsallis1996) evaluations 17740 fit 1772.58 change 7.17 MxComputeSimAnnealing(tsallis1996) evaluations 18569 fit 1857.16 change 91.58MxComputeSimAnnealing(tsallis1996) evaluations 19400 fit 1765.41 change -0.002844MxComputeSimAnnealing(tsallis1996) evaluations 20233 fit 2326.81 change -54.06                                                                                 
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
#> Beginning initial fit attemptFit attempt 0, fit=3076.55795135632, new current best! (was 3076.55795135632)Beginning fit attempt 1 of at maximum 10 extra tries                         Beginning fit attempt 2 of at maximum 10 extra triesBeginning fit attempt 3 of at maximum 10 extra triesMxComputeGradientDescent(SLSQP) evaluations 453 fit 3105.59 change -0.1974                                                                          Beginning fit attempt 4 of at maximum 10 extra triesBeginning fit attempt 5 of at maximum 10 extra triesBeginning fit attempt 6 of at maximum 10 extra triesBeginning fit attempt 7 of at maximum 10 extra triesBeginning fit attempt 8 of at maximum 10 extra triesBeginning fit attempt 9 of at maximum 10 extra triesBeginning fit attempt 10 of at maximum 10 extra triesMxComputeGradientDescent(SLSQP) evaluations 850 fit 3076.57 change -0.01959                                                                           Final run, for Hessian and/or standard errors and/or confidence intervals                                                                         
#> Beginning initial fit attemptFit attempt 0, fit=3076.55795135632, new current best! (was 3076.55795135632)Beginning fit attempt 1 of at maximum 10 extra tries                         MxComputeGradientDescent(SLSQP) evaluations 951 fit 2822.94 change -7.403MxComputeGradientDescent(SLSQP) evaluations 3355 fit 2757.48 change -0.01345                                                                            Fit attempt 1, fit=2757.45486738943, new current best! (was 3076.55795135632)Beginning fit attempt 2 of at maximum 10 extra tries                         MxComputeGradientDescent(SLSQP) evaluations 159 fit 2821.64 change -616.4MxComputeGradientDescent(SLSQP) evaluations 2032 fit 2758.54 change -0.07605                                                                            Beginning fit attempt 3 of at maximum 10 extra triesMxComputeGradientDescent(SLSQP) evaluations 1039 fit 2873.92 change -7.81MxComputeGradientDescent(SLSQP) evaluations 2832 fit 2764.25 change -0.6575                                                                           Fit attempt 3, fit=2763.82195749593, worse than previous best (2757.45486738943)Beginning fit attempt 4 of at maximum 10 extra tries                            MxComputeGradientDescent(SLSQP) evaluations 320 fit 2828.55 change -36.56MxComputeGradientDescent(SLSQP) evaluations 2199 fit 2761.33 change -2.379MxComputeGradientDescent(SLSQP) evaluations 3951 fit 2757.45 change -1.688e-05                                                                              Beginning fit attempt 5 of at maximum 10 extra triesMxComputeGradientDescent(SLSQP) evaluations 1117 fit 2778.59 change -3.378MxComputeGradientDescent(SLSQP) evaluations 2900 fit 2757.52 change -0.1831                                                                           Fit attempt 5, fit=2757.45486737805, new current best! (was 2757.45486738943)Beginning fit attempt 6 of at maximum 10 extra tries                         MxComputeGradientDescent(SLSQP) evaluations 1348 fit 2761.4 change -3.187                                                                         Beginning fit attempt 7 of at maximum 10 extra triesMxComputeGradientDescent(SLSQP) evaluations 159 fit 3306.44 change -2304MxComputeGradientDescent(SLSQP) evaluations 2044 fit 2770.42 change -0.06172                                                                            Beginning fit attempt 8 of at maximum 10 extra triesMxComputeGradientDescent(SLSQP) evaluations 479 fit 2774.08 change -6.426MxComputeGradientDescent(SLSQP) evaluations 2274 fit 2757.5 change -0.05139                                                                           Fit attempt 8, fit=2757.45486737507, new current best! (was 2757.45486737805)Beginning fit attempt 9 of at maximum 10 extra tries                         MxComputeGradientDescent(SLSQP) evaluations 1424 fit 2824.88 change -1.414MxComputeGradientDescent(SLSQP) evaluations 3198 fit 2758.3 change -0.1323                                                                          Beginning fit attempt 10 of at maximum 10 extra triesMxComputeGradientDescent(SLSQP) evaluations 727 fit 3109.75 change -12.15MxComputeGradientDescent(SLSQP) evaluations 2764 fit 2885.94 change -27.7MxComputeGradientDescent(SLSQP) evaluations 5052 fit 2772.31 change -0.1162                                                                           Fit attempt 10, fit=2772.29504642962, worse than previous best (2757.45486737507)Final run, for Hessian and/or standard errors and/or confidence intervals        MxComputeNumericDeriv 231/703MxComputeNumericDeriv 541/703                                                                                                      
#> Beginning initial fit attemptFit attempt 0, fit=3076.55795135631, new current best! (was 3076.55795135632)Beginning fit attempt 1 of at maximum 10 extra tries                         MxComputeGradientDescent(SLSQP) evaluations 117 fit 3119.52MxComputeGradientDescent(SLSQP) evaluations 1636 fit 2665.17 change -3.447MxComputeGradientDescent(SLSQP) evaluations 3259 fit 2642.95 change -1.304MxComputeGradientDescent(SLSQP) evaluations 4757 fit 2628.61 change -0.2215MxComputeGradientDescent(SLSQP) evaluations 6354 fit 2627.94 change -0.01976MxComputeGradientDescent(SLSQP) evaluations 8178 fit 2627.92 change -5.151e-06                                                                              Fit attempt 1, fit=2627.92477729493, new current best! (was 3076.55795135631)Beginning fit attempt 2 of at maximum 10 extra tries                         MxComputeGradientDescent(SLSQP) evaluations 1417 fit 2765.3 change -3.873MxComputeGradientDescent(SLSQP) evaluations 2810 fit 2688.08 change -4.141MxComputeGradientDescent(SLSQP) evaluations 4314 fit 2632.72 change -3.138MxComputeGradientDescent(SLSQP) evaluations 5796 fit 2628.89 change -0.01056MxComputeGradientDescent(SLSQP) evaluations 7278 fit 2628.86 change -3.319e-06                                                                              Fit attempt 2, fit=2628.85548409314, worse than previous best (2627.92477729493)Beginning fit attempt 3 of at maximum 10 extra tries                            MxComputeGradientDescent(SLSQP) evaluations 604 fit 3710.45 change -91.63MxComputeGradientDescent(SLSQP) evaluations 1899 fit 2904.45 change -5.129MxComputeGradientDescent(SLSQP) evaluations 3186 fit 2759.12 change -8.386MxComputeGradientDescent(SLSQP) evaluations 4577 fit 2691.08 change -1.174MxComputeGradientDescent(SLSQP) evaluations 5839 fit 2588.2 change -1.551 MxComputeGradientDescent(SLSQP) evaluations 7093 fit 2583.87 change -0.01118MxComputeGradientDescent(SLSQP) evaluations 8356 fit 2575.21 change -0.8964 MxComputeGradientDescent(SLSQP) evaluations 9724 fit 2572.52 change -0.002641MxComputeGradientDescent(SLSQP) evaluations 10978 fit 2572.51 change -0.001233MxComputeGradientDescent(SLSQP) evaluations 12346 fit 2572.49 change -0.001092MxComputeGradientDescent(SLSQP) evaluations 13714 fit 2572.48 change -1.806e-05MxComputeGradientDescent(SLSQP) evaluations 14971 fit 2572.48 change -8.739e-07MxComputeGradientDescent(SLSQP) evaluations 16231 fit 2572.48 change -0.0001914MxComputeGradientDescent(SLSQP) evaluations 17400 fit 2572.48 change -5.317e-06MxComputeGradientDescent(SLSQP) evaluations 18678 fit 2572.48 change -2.394e-05MxComputeGradientDescent(SLSQP) evaluations 19832 fit 2572.45 change -0.00237  MxComputeGradientDescent(SLSQP) evaluations 20989 fit 2572.4 change -0.003297MxComputeGradientDescent(SLSQP) evaluations 22245 fit 2572.38 change -1.325e-05MxComputeGradientDescent(SLSQP) evaluations 23385 fit 2572.38 change -1.11e-07                                                                               Fit attempt 3, fit=2572.37757501822, new current best! (was 2627.92477729493)Beginning fit attempt 4 of at maximum 10 extra tries                         MxComputeGradientDescent(SLSQP) evaluations 938 fit 2585.11 change -2.15MxComputeGradientDescent(SLSQP) evaluations 2215 fit 2574.94 change -0.6129MxComputeGradientDescent(SLSQP) evaluations 3487 fit 2573.17 change -0.1926MxComputeGradientDescent(SLSQP) evaluations 4754 fit 2572.48 change -0.07984                                                                            Fit attempt 4, fit=2572.3775750087, new current best! (was 2572.37757501822)Beginning fit attempt 5 of at maximum 10 extra tries                        MxComputeGradientDescent(SLSQP) evaluations 590 fit 2764.07 change -40.26MxComputeGradientDescent(SLSQP) evaluations 1760 fit 2704.49 change -1.62MxComputeGradientDescent(SLSQP) evaluations 3037 fit 2647.98 change -6.569MxComputeGradientDescent(SLSQP) evaluations 4197 fit 2589.66 change -2.177MxComputeGradientDescent(SLSQP) evaluations 5457 fit 2572.5 change -0.1095MxComputeGradientDescent(SLSQP) evaluations 6597 fit 2572.38 change -0.000446                                                                             Fit attempt 5, fit=2572.37759073324, worse than previous best (2572.3775750087)Beginning fit attempt 6 of at maximum 10 extra tries                           MxComputeGradientDescent(SLSQP) evaluations 472 fit 2889.56 change -192.2MxComputeGradientDescent(SLSQP) evaluations 1761 fit 2690.16 change -9.864MxComputeGradientDescent(SLSQP) evaluations 3038 fit 2640.5 change -0.6006MxComputeGradientDescent(SLSQP) evaluations 4307 fit 2637.78 change -0.2212MxComputeGradientDescent(SLSQP) evaluations 5453 fit 2626.77 change -2.594 MxComputeGradientDescent(SLSQP) evaluations 6709 fit 2603.49 change -0.3943MxComputeGradientDescent(SLSQP) evaluations 7963 fit 2602.92 change -9.091e-06                                                                              Fit attempt 6, fit=2602.92155913919, worse than previous best (2572.3775750087)Beginning fit attempt 7 of at maximum 10 extra tries                           MxComputeGradientDescent(SLSQP) evaluations 358 fit 4035.38 change -54.21MxComputeGradientDescent(SLSQP) evaluations 1429 fit 2838.44 change -6.14MxComputeGradientDescent(SLSQP) evaluations 2601 fit 2757.38 change -4.467MxComputeGradientDescent(SLSQP) evaluations 3759 fit 2726.65 change -3.603MxComputeGradientDescent(SLSQP) evaluations 4913 fit 2667.51 change -4.654MxComputeGradientDescent(SLSQP) evaluations 6056 fit 2614.91 change -1.834MxComputeGradientDescent(SLSQP) evaluations 7196 fit 2574.43 change -0.02763MxComputeGradientDescent(SLSQP) evaluations 8336 fit 2573.44 change -0.003354MxComputeGradientDescent(SLSQP) evaluations 9476 fit 2572.49 change -0.0007288                                                                              Fit attempt 7, fit=2572.48494290614, worse than previous best (2572.3775750087)Beginning fit attempt 8 of at maximum 10 extra tries                           MxComputeGradientDescent(SLSQP) evaluations 474 fit 2925.23 change -806MxComputeGradientDescent(SLSQP) evaluations 1764 fit 2696.87 change -3.97MxComputeGradientDescent(SLSQP) evaluations 2924 fit 2660.49 change -1.373MxComputeGradientDescent(SLSQP) evaluations 4195 fit 2625.92 change -1.891MxComputeGradientDescent(SLSQP) evaluations 5458 fit 2576.6 change -4.143 MxComputeGradientDescent(SLSQP) evaluations 6712 fit 2572.48 change -1.634e-05MxComputeGradientDescent(SLSQP) evaluations 8198 fit 2572.45 change -0.01323  MxComputeGradientDescent(SLSQP) evaluations 9456 fit 2572.38 change -0.000867                                                                             Beginning fit attempt 9 of at maximum 10 extra triesMxComputeGradientDescent(SLSQP) evaluations 711 fit 3421.82 change -340.3MxComputeGradientDescent(SLSQP) evaluations 1894 fit 3115.84 change -18.15MxComputeGradientDescent(SLSQP) evaluations 3064 fit 2815.07 change -4.688MxComputeGradientDescent(SLSQP) evaluations 4101 fit 2759.27 change -4.672MxComputeGradientDescent(SLSQP) evaluations 5013 fit 2748.44 change -0.2791MxComputeGradientDescent(SLSQP) evaluations 6039 fit 2747.99 change -0.006984MxComputeGradientDescent(SLSQP) evaluations 7065 fit 2747.98 change -1.185e-06MxComputeGradientDescent(SLSQP) evaluations 7977 fit 2747.98 change -7.14e-05 MxComputeGradientDescent(SLSQP) evaluations 8889 fit 2747.98 change -9.059e-05MxComputeGradientDescent(SLSQP) evaluations 9801 fit 2747.98 change -3.089e-07MxComputeGradientDescent(SLSQP) evaluations 10599 fit 2747.98 change -6.177e-07                                                                               Fit attempt 9, fit=2747.976691787, worse than previous best (2572.3775750087)Beginning fit attempt 10 of at maximum 10 extra tries                        MxComputeGradientDescent(SLSQP) evaluations 945 fit 4221.12 change -19.61MxComputeGradientDescent(SLSQP) evaluations 1898 fit 3818.34 change -5.02MxComputeGradientDescent(SLSQP) evaluations 2845 fit 3309.11 change -63.05MxComputeGradientDescent(SLSQP) evaluations 3779 fit 3034.48 change -12.94MxComputeGradientDescent(SLSQP) evaluations 4812 fit 2835.28 change -11.43MxComputeGradientDescent(SLSQP) evaluations 5735 fit 2798.59 change -1.503MxComputeGradientDescent(SLSQP) evaluations 6761 fit 2791.25 change -0.5461MxComputeGradientDescent(SLSQP) evaluations 7787 fit 2787.25 change -0.315 MxComputeGradientDescent(SLSQP) evaluations 8813 fit 2785.06 change -0.1491MxComputeGradientDescent(SLSQP) evaluations 9725 fit 2784.46 change -0.07014MxComputeGradientDescent(SLSQP) evaluations 10751 fit 2782.58 change -0.4551MxComputeGradientDescent(SLSQP) evaluations 11663 fit 2781.96 change -0.1041MxComputeGradientDescent(SLSQP) evaluations 12575 fit 2781.36 change -0.2421MxComputeGradientDescent(SLSQP) evaluations 13601 fit 2780.82 change -0.07627MxComputeGradientDescent(SLSQP) evaluations 14513 fit 2780.74 change -0.002055MxComputeGradientDescent(SLSQP) evaluations 15539 fit 2780.31 change -0.08306 MxComputeGradientDescent(SLSQP) evaluations 16566 fit 2779.33 change -0.2706 MxComputeGradientDescent(SLSQP) evaluations 17716 fit 2711.95 change -28.51 MxComputeGradientDescent(SLSQP) evaluations 18756 fit 2618.02 change -17.61MxComputeGradientDescent(SLSQP) evaluations 19783 fit 2580.44 change -0.4088MxComputeGradientDescent(SLSQP) evaluations 20926 fit 2574.98 change -0.2527MxComputeGradientDescent(SLSQP) evaluations 22066 fit 2574.66 change -0.0007885MxComputeGradientDescent(SLSQP) evaluations 23092 fit 2574.64 change -0.001733 MxComputeGradientDescent(SLSQP) evaluations 24121 fit 2573.23 change -0.4036  MxComputeGradientDescent(SLSQP) evaluations 25148 fit 2572.9 change -2.125e-05                                                                              Fit attempt 10, fit=2572.90464047946, worse than previous best (2572.3775750087)Final run, for Hessian and/or standard errors and/or confidence intervals       MxComputeNumericDeriv 78/1596MxComputeNumericDeriv 279/1596MxComputeNumericDeriv 479/1596MxComputeNumericDeriv 682/1596MxComputeNumericDeriv 881/1596MxComputeNumericDeriv 1085/1596MxComputeNumericDeriv 1295/1596MxComputeNumericDeriv 1505/1596                                                                                                        
saveRDS(res_lca, "res_lca_intro.rds")
```


``` r
res_lca <- readRDS("res_lca_intro.rds")
tab_lca_fit <- table_fit(res_lca)
#> MxComputeGradientDescent(SLSQP) evaluations 238 fit 2874.51 change -14.17MxComputeGradientDescent(SLSQP) evaluations 809 fit 2751.4 change -19.68 MxComputeGradientDescent(SLSQP) evaluations 1490 fit 2561.79 change -0.146MxComputeGradientDescent(SLSQP) evaluations 2106 fit 2561.71 change -4.596e-07                                                                              
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

This figure implies that the most common response to AM1 in class 1 is 4, quite a few people score 1, and very few people score 6 or 7.
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
#> Beginning initial fit attemptFit attempt 0, fit=1191.2490839595, new current best! (was 1191.2490839595)Beginning fit attempt 1 of at maximum 10 extra tries                       Fit attempt 1, fit=985.934481289332, new current best! (was 1191.2490839595)Beginning fit attempt 2 of at maximum 10 extra tries                        Fit attempt 2, fit=985.934481289203, new current best! (was 985.934481289332)Beginning fit attempt 3 of at maximum 10 extra tries                         Beginning fit attempt 4 of at maximum 10 extra triesBeginning fit attempt 5 of at maximum 10 extra triesBeginning fit attempt 6 of at maximum 10 extra triesBeginning fit attempt 7 of at maximum 10 extra triesBeginning fit attempt 8 of at maximum 10 extra triesBeginning fit attempt 9 of at maximum 10 extra triesBeginning fit attempt 10 of at maximum 10 extra triesMxComputeGradientDescent(SLSQP) evaluations 255 fit 985.937 change -0.008624                                                                            Final run, for Hessian and/or standard errors and/or confidence intervals                                                                         
#> Beginning initial fit attemptFit attempt 0, fit=1191.2490839595, new current best! (was 1191.2490839595)Beginning fit attempt 1 of at maximum 10 extra tries                       Fit attempt 1, fit=985.93448128921, new current best! (was 1191.2490839595)Beginning fit attempt 2 of at maximum 10 extra tries                       Beginning fit attempt 3 of at maximum 10 extra triesBeginning fit attempt 4 of at maximum 10 extra triesBeginning fit attempt 5 of at maximum 10 extra triesMxComputeGradientDescent(SLSQP) evaluations 172 fit 986.236 change -0.04738                                                                           Beginning fit attempt 6 of at maximum 10 extra triesFit attempt 6, fit=985.934481289206, new current best! (was 985.93448128921)Beginning fit attempt 7 of at maximum 10 extra tries                        Beginning fit attempt 8 of at maximum 10 extra triesBeginning fit attempt 9 of at maximum 10 extra triesBeginning fit attempt 10 of at maximum 10 extra triesFinal run, for Hessian and/or standard errors and/or confidence intervals                                                                         
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
#> MxComputeSimAnnealing(tsallis1996) evaluations 580 fit 3445.92 change 0MxComputeSimAnnealing(tsallis1996) evaluations 1573 fit 2905.6 change 0MxComputeSimAnnealing(tsallis1996) evaluations 2519 fit 3599.01 change 1610MxComputeSimAnnealing(tsallis1996) evaluations 3463 fit 3185.91 change 1349MxComputeSimAnnealing(tsallis1996) evaluations 4412 fit 1826.81 change -362.2MxComputeSimAnnealing(tsallis1996) evaluations 5345 fit 1911.17 change -1497 MxComputeSimAnnealing(tsallis1996) evaluations 6272 fit 1825.63 change -645.9MxComputeSimAnnealing(tsallis1996) evaluations 7175 fit 2705.19 change 876.3 MxComputeSimAnnealing(tsallis1996) evaluations 8090 fit 1825.46 change -1254MxComputeSimAnnealing(tsallis1996) evaluations 9015 fit 2653.52 change 563.5MxComputeSimAnnealing(tsallis1996) evaluations 9928 fit 1824.37 change 0.3621MxComputeSimAnnealing(tsallis1996) evaluations 10840 fit 1849.79 change 25.78MxComputeSimAnnealing(tsallis1996) evaluations 11742 fit 1824.01 change -0.003857MxComputeSimAnnealing(tsallis1996) evaluations 12647 fit 1824.01 change 0.007232                                                                                 
#> MxComputeSimAnnealing(tsallis1996) evaluations 440 fit 2210.3 change 0MxComputeSimAnnealing(tsallis1996) evaluations 1069 fit 2210.3 change 0MxComputeSimAnnealing(tsallis1996) evaluations 1700 fit 2210.3 change 0MxComputeSimAnnealing(tsallis1996) evaluations 2326 fit 1953.6 change -1254MxComputeSimAnnealing(tsallis1996) evaluations 2949 fit 2362.19 change 424.1MxComputeSimAnnealing(tsallis1996) evaluations 3585 fit 1845.2 change -991.1MxComputeSimAnnealing(tsallis1996) evaluations 4218 fit 1980.4 change 0     MxComputeSimAnnealing(tsallis1996) evaluations 4813 fit 3261.79 change 933MxComputeSimAnnealing(tsallis1996) evaluations 5424 fit 2032.27 change -5.804MxComputeSimAnnealing(tsallis1996) evaluations 6039 fit 2341.32 change -172  MxComputeSimAnnealing(tsallis1996) evaluations 6658 fit 3554.99 change 1728MxComputeSimAnnealing(tsallis1996) evaluations 7260 fit 1990.32 change 171.4MxComputeSimAnnealing(tsallis1996) evaluations 7872 fit 1822.78 change -500.2MxComputeSimAnnealing(tsallis1996) evaluations 8482 fit 1888.89 change -270.3MxComputeSimAnnealing(tsallis1996) evaluations 9092 fit 1817.2 change -0.0482MxComputeSimAnnealing(tsallis1996) evaluations 9697 fit 2525.58 change -197  MxComputeSimAnnealing(tsallis1996) evaluations 10298 fit 1817.51 change -764.2MxComputeSimAnnealing(tsallis1996) evaluations 10905 fit 3615.77 change 1799  MxComputeSimAnnealing(tsallis1996) evaluations 11511 fit 1817.18 change -0.008194MxComputeSimAnnealing(tsallis1996) evaluations 12115 fit 1817.22 change -3.573   MxComputeSimAnnealing(tsallis1996) evaluations 12712 fit 1817.19 change -5.764MxComputeSimAnnealing(tsallis1996) evaluations 13295 fit 1817.21 change -95.54MxComputeSimAnnealing(tsallis1996) evaluations 13910 fit 1817.21 change -95.6 MxComputeSimAnnealing(tsallis1996) evaluations 14521 fit 3624.12 change 1804 MxComputeSimAnnealing(tsallis1996) evaluations 15131 fit 1818.79 change 1.609MxComputeSimAnnealing(tsallis1996) evaluations 15741 fit 2620.07 change 802.6MxComputeSimAnnealing(tsallis1996) evaluations 16348 fit 1817.18 change 0.001595MxComputeSimAnnealing(tsallis1996) evaluations 16958 fit 1817.19 change -2620   MxComputeSimAnnealing(tsallis1996) evaluations 17542 fit 1934.7 change 117.5 MxComputeSimAnnealing(tsallis1996) evaluations 18139 fit 1817.19 change 1.937e-06MxComputeSimAnnealing(tsallis1996) evaluations 18736 fit 1817.23 change -175.1                                                                                 
#> Beginning initial fit attemptFit attempt 0, fit=1817.17759927602, new current best! (was 1817.17759927603)Beginning fit attempt 1 of at maximum 10 extra tries                         MxComputeGradientDescent(SLSQP) evaluations 583 fit 1821.21 change -0.1732MxComputeGradientDescent(SLSQP) evaluations 1585 fit 1812.42 change -1.878e-05                                                                              Fit attempt 1, fit=1812.41758632867, new current best! (was 1817.17759927602)Beginning fit attempt 2 of at maximum 10 extra tries                         MxComputeGradientDescent(SLSQP) evaluations 971 fit 1834.71 change -0.7867MxComputeGradientDescent(SLSQP) evaluations 2351 fit 1830.04 change -1.378e-05                                                                              Fit attempt 2, fit=1830.0374329713, worse than previous best (1812.41758632867)Beginning fit attempt 3 of at maximum 10 extra tries                           MxComputeGradientDescent(SLSQP) evaluations 510 fit 1812.42 change -0.0004774                                                                             Beginning fit attempt 4 of at maximum 10 extra triesMxComputeGradientDescent(SLSQP) evaluations 1627 fit 2056.16 change -0.0004645                                                                              Fit attempt 4, fit=2056.16156919076, worse than previous best (1812.41758632867)Beginning fit attempt 5 of at maximum 10 extra tries                            MxComputeGradientDescent(SLSQP) evaluations 412 fit 1813.16 change -0.5363                                                                          Fit attempt 5, fit=1812.41760007133, worse than previous best (1812.41758632867)Beginning fit attempt 6 of at maximum 10 extra tries                            MxComputeGradientDescent(SLSQP) evaluations 1070 fit 1836.13 change -0.001079MxComputeGradientDescent(SLSQP) evaluations 2487 fit 1833.97 change -0.0007831                                                                              Fit attempt 6, fit=1829.80298406985, worse than previous best (1812.41758632867)Beginning fit attempt 7 of at maximum 10 extra tries                            MxComputeGradientDescent(SLSQP) evaluations 311 fit 1827.68 change -5.654                                                                         Fit attempt 7, fit=1812.41758614693, new current best! (was 1812.41758632867)Beginning fit attempt 8 of at maximum 10 extra tries                         MxComputeGradientDescent(SLSQP) evaluations 542 fit 1812.42 change -0.06604                                                                           Beginning fit attempt 9 of at maximum 10 extra triesMxComputeGradientDescent(SLSQP) evaluations 1149 fit 1827.45 change -0.02292MxComputeGradientDescent(SLSQP) evaluations 2525 fit 1827.38 change -8.185e-08                                                                              Fit attempt 9, fit=1827.37644993461, worse than previous best (1812.41758614693)Beginning fit attempt 10 of at maximum 10 extra tries                           Fit attempt 10, fit=1812.41758580936, new current best! (was 1812.41758614693)Final run, for Hessian and/or standard errors and/or confidence intervals     MxComputeNumericDeriv 4/120                                                                                                    
saveRDS(res_mixed, "res_mixed_intro.rds")
```


``` r
res_mixed <- readRDS("res_mixed_intro.rds")
tab_mixed_fit <- table_fit(res_mixed)
#> MxComputeGradientDescent(SLSQP) evaluations 24 fit 4746.42                                                          
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


