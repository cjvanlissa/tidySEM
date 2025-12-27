# Generating syntax for structural equation models

`tidySEM` offers a user-friendly, tidy workflow for generating syntax
for SEM models. The workflow is top-down, meaning that syntax is
generated based on conceptual model elements. In many cases, the
generated syntax will suffice - but it is always customizable. The
workflow also tries to intelligently guess which variables go together,
but these defaults can be overridden.

## The tidySEM workflow

The workflow underlying syntax generation in `tidySEM` is as follows:

1.  Give the variables in your `data` object short, informative names,
    that are easily machine readable
2.  Convert the data to a `tidy_sem` object by running
    `model <- tidy_sem(data)`
3.  Add elements of syntax
    - E.g., `measurement(model)`
4.  *Optionally*, access the `dictionary`, `data`, and `syntax` elements
    in the `tidy_sem` object by calling `dictionary(model)`,
    `get_data(model)`, or `syntax(model)`
5.  *Optionally*, modify the `dictionary`, `data`, and `syntax` elements
    in the `tidy_sem` object `dictionary(model) <- ...`,
    `get_data(model) <- ...`, and `syntax(model) <- ...`
6.  Run the analysis, either by:
    - Converting the `tidy_sem` object to `lavaan` syntax using
      `as_lavaan(model)` and using that as input for the `lavaan`
      functions `sem`, `lavaan`, or `cfa`
    - Converting the `tidy_sem` object to `OpenMx` using
      `as_ram(model)`, and using that as input for `mxRun` or
      \`run_mx\`\`
    - Converting the `tidy_sem` object to `Mplus` using
      `as_mplus(model)`, and using that as input for
      [`MplusAutomation::mplusObject()`](https://michaelhallquist.github.io/MplusAutomation/reference/mplusObject.html)
    - Using the functions `estimate_lavaan(model)`,
      `estimate_mx(model)`, or `estimate_mplus(model)`

All elements of the `tidy_sem` object are “tidy” data, i.e., tabular
`data.frames`, and can be modified using the familiar suite of functions
in the ‘tidyverse’. Thus, the data, dictionary, and syntax are all
represented as `data.frame`s.

## Example: Running a CFA

### Step 1: Check the variable names

As an example, let’s make a graph for a classic `lavaan` tutorial
example for CFA. First, we check the data names:

``` r
df <- HolzingerSwineford1939
names(df)
#>  [1] "id"     "sex"    "ageyr"  "agemo"  "school" "grade"  "x1"     "x2"    
#>  [9] "x3"     "x4"     "x5"     "x6"     "x7"     "x8"     "x9"
```

These names are not informative, as the items named `x..` are indicators
of three different latent variables. We will rename them accordingly:

``` r
names(df)[grepl("^x", names(df))] <- c("vis_1", "vis_2", "vis_3", "tex_1", "tex_2", "tex_3", "spe_1", "spe_2", "spe_3")
```

### Guidelines for naming variables

In general, it is good practice to name variables using the following
information:

- Scale name (or if the variable is not part of a scale, observed
  variable name)
- Measurement occasion (if longitudinal)
- Respondent id (if multiple respondents completed the same scales)
- Scale item number, which `tidySEM` expects to be separated from the
  remainder of the variable name by a splitting character (e.g.,
  `scale_item`)

Roughly speaking, elements of the variable name should be ordered from
“slow-changing” to “fast-changing”; i.e.; there are only a few scales,
with possibly several measurement occasions or respondents, and many
items.

### Step 2: Generate a dictionary

A dictionary indicates which variables in the data belong to, for
example, the same scale. When the data have informative names, it is
possible to construct a data dictionary automatically:

``` r
model <- tidy_sem(df)
model
#> A tidy_sem object
#> v    $dictionary
#> v    $data
#> o    $syntax
```

### Step 3: Generate syntax

We can automatically add basic syntax to the `sem_syntax` object, by
passing it to a syntax-generating function like
[`measurement()`](https://cjvanlissa.github.io/tidySEM/reference/measurement.md),
which adds a measurement model for any scales in the object:

``` r
model |>
  measurement() -> model
model
```

    #> A tidy_sem object
    #> v    $dictionary
    #> v    $data
    #> v    $syntax

### Step 4: Run the model

The resulting model can be evaluated as ‘lavaan’ syntax, ‘OpenMx’
syntax, or ‘Mplus’ syntax, using the `as_lavaan`, `as_ram`, and
`as_mplus` functions. For example, using lavaan:

``` r
model |>
  estimate_lavaan()
```

    #> lavaan 0.6-21 ended normally after 35 iterations
    #> 
    #>   Estimator                                         ML
    #>   Optimization method                           NLMINB
    #>   Number of model parameters                        30
    #> 
    #>   Number of observations                           301
    #> 
    #> Model Test User Model:
    #>                                                       
    #>   Test statistic                                85.306
    #>   Degrees of freedom                                24
    #>   P-value (Chi-square)                           0.000

The same model can be estimated with ‘OpenMx’ through the R-package
`OpenMx`.

``` r
model |>
  estimate_mx()
```

The same model can be estimated in ‘Mplus’ through the R-package
`MplusAutomation`. This requires ‘Mplus’ to be installed.

``` r
library(MplusAutomation)
model |>
  estimate_mplus()
```

### Optional step 5: Access the dictionary, data, and syntax

The dictionary and syntax can be examined using `dictionary(model)` and
`syntax(model)`:

``` r
dictionary(model)
#>      name scale      type  label
#> 1      id  <NA>  observed     id
#> 2     sex  <NA>  observed    sex
#> 3   ageyr  <NA>  observed  ageyr
#> 4   agemo  <NA>  observed  agemo
#> 5  school  <NA>  observed school
#> 6   grade  <NA>  observed  grade
#> 7   vis_1   vis indicator  vis_1
#> 8   vis_2   vis indicator  vis_2
#> 9   vis_3   vis indicator  vis_3
#> 10  tex_1   tex indicator  tex_1
#> 11  tex_2   tex indicator  tex_2
#> 12  tex_3   tex indicator  tex_3
#> 13  spe_1   spe indicator  spe_1
#> 14  spe_2   spe indicator  spe_2
#> 15  spe_3   spe indicator  spe_3
#> 16    vis  <NA>    latent    vis
#> 17    tex  <NA>    latent    tex
#> 18    spe  <NA>    latent    spe
```

``` r
syntax(model)
#>      lhs op   rhs block group free label ustart plabel
#> 1    vis =~ vis_1     1     1    0            1   .p1.
#> 2    vis =~ vis_2     1     1    1           NA   .p2.
#> 3    vis =~ vis_3     1     1    1           NA   .p3.
#> 4    tex =~ tex_1     1     1    0            1   .p4.
#> 5    tex =~ tex_2     1     1    1           NA   .p5.
#> 6    tex =~ tex_3     1     1    1           NA   .p6.
#> 7    spe =~ spe_1     1     1    0            1   .p7.
#> 8    spe =~ spe_2     1     1    1           NA   .p8.
#> 9    spe =~ spe_3     1     1    1           NA   .p9.
#> 10 vis_1 ~~ vis_1     1     1    1           NA  .p10.
#> 11 vis_2 ~~ vis_2     1     1    1           NA  .p11.
#> 12 vis_3 ~~ vis_3     1     1    1           NA  .p12.
#> 13 tex_1 ~~ tex_1     1     1    1           NA  .p13.
#> 14 tex_2 ~~ tex_2     1     1    1           NA  .p14.
#> 15 tex_3 ~~ tex_3     1     1    1           NA  .p15.
#> 16 spe_1 ~~ spe_1     1     1    1           NA  .p16.
#> 17 spe_2 ~~ spe_2     1     1    1           NA  .p17.
#> 18 spe_3 ~~ spe_3     1     1    1           NA  .p18.
#> 19   vis ~~   vis     1     1    1           NA  .p19.
#> 20   tex ~~   tex     1     1    1           NA  .p20.
#> 21   spe ~~   spe     1     1    1           NA  .p21.
#> 22   vis ~~   tex     1     1    1           NA  .p22.
#> 23   vis ~~   spe     1     1    1           NA  .p23.
#> 24   tex ~~   spe     1     1    1           NA  .p24.
#> 25 vis_1 ~1           1     1    1           NA  .p25.
#> 26 vis_2 ~1           1     1    1           NA  .p26.
#> 27 vis_3 ~1           1     1    1           NA  .p27.
#> 28 tex_1 ~1           1     1    1           NA  .p28.
#> 29 tex_2 ~1           1     1    1           NA  .p29.
#> 30 tex_3 ~1           1     1    1           NA  .p30.
#> 31 spe_1 ~1           1     1    1           NA  .p31.
#> 32 spe_2 ~1           1     1    1           NA  .p32.
#> 33 spe_3 ~1           1     1    1           NA  .p33.
#> 34   vis ~1           1     1    0            0  .p34.
#> 35   tex ~1           1     1    0            0  .p35.
#> 36   spe ~1           1     1    0            0  .p36.
```

### Optional step 6: Modify the dictionary and syntax

At this stage, we may want to modify the basic syntax slightly. The
functions `dictionary(model) <- ...` and `syntax(model) <- ...` can be
used to modify the dictionary and syntax:

``` r
dictionary(model) |>
  mutate(label = ifelse(label == "vis", "Visual", label))
```

    #>      name scale      type  label
    #> 1      id  <NA>  observed     id
    #> 2     sex  <NA>  observed    sex
    #> 3   ageyr  <NA>  observed  ageyr
    #> 4   agemo  <NA>  observed  agemo
    #> 5  school  <NA>  observed school
    #> 6   grade  <NA>  observed  grade
    #> 7   vis_1   vis indicator  vis_1
    #> 8   vis_2   vis indicator  vis_2
    #> 9   vis_3   vis indicator  vis_3
    #> 10  tex_1   tex indicator  tex_1
    #> 11  tex_2   tex indicator  tex_2
    #> 12  tex_3   tex indicator  tex_3
    #> 13  spe_1   spe indicator  spe_1
    #> 14  spe_2   spe indicator  spe_2
    #> 15  spe_3   spe indicator  spe_3
    #> 16    vis  <NA>    latent Visual
    #> 17    tex  <NA>    latent    tex
    #> 18    spe  <NA>    latent    spe

For example, imagine we want to change the model, so that all items of
the “spe” subscale load on the “tex” latent variable. We would first
replace the latent variable “spe” with “tex”, and secondly remove all
mention of the “spe” latent variable:

``` r
syntax(model) |>
  mutate(lhs = ifelse(lhs == "spe" & op == "=~", "tex", lhs)) |>
  filter(!(lhs == "spe" | rhs == "spe")) -> syntax(model)
```

Remember that both of the original latent variables were identified by
fixing one indicator to be equal to 1, so we have to free up one of
them:

``` r
syntax(model) |>
  mutate(free = ifelse(rhs == "spe_1", 1, free),
  ustart = ifelse(rhs == "spe_1", NA, ustart)) -> syntax(model)
```

The modified model could then be run:

``` r
estimate_lavaan(model)
#> lavaan 0.6-21 ended normally after 28 iterations
#> 
#>   Estimator                                         ML
#>   Optimization method                           NLMINB
#>   Number of model parameters                        28
#> 
#>   Number of observations                           301
#> 
#> Model Test User Model:
#>                                                       
#>   Test statistic                               236.091
#>   Degrees of freedom                                26
#>   P-value (Chi-square)                           0.000
```

### Optional step 7: Adding paths

In addition to the way of editing the `data.frame` with model syntax
described in Step 6, it is also possible to add (or modify) paths by
adding `lavaan` syntax. For example, imagine that - instead of having
“vis” and “tex” correlate, we want to add a regression path between
them:

``` r
model |>
  add_paths("vis ~ tex") |>
  estimate_lavaan() |>
  summary(estimates = TRUE)
```

    #> lavaan 0.6-21 ended normally after 25 iterations
    #> 
    #>   Estimator                                         ML
    #>   Optimization method                           NLMINB
    #>   Number of model parameters                        28
    #> 
    #>   Number of observations                           301
    #> 
    #> Model Test User Model:
    #>                                                       
    #>   Test statistic                               236.091
    #>   Degrees of freedom                                26
    #>   P-value (Chi-square)                           0.000
    #> 
    #> Parameter Estimates:
    #> 
    #>   Standard errors                             Standard
    #>   Information                                 Expected
    #>   Information saturated (h1) model          Structured
    #> 
    #> Latent Variables:
    #>                    Estimate  Std.Err  z-value  P(>|z|)
    #>   vis =~                                              
    #>     vis_1             1.000                           
    #>     vis_2             0.551    0.103    5.335    0.000
    #>     vis_3             0.700    0.115    6.110    0.000
    #>   tex =~                                              
    #>     tex_1             1.000                           
    #>     tex_2             1.109    0.066   16.892    0.000
    #>     tex_3             0.927    0.056   16.636    0.000
    #>     spe_1             0.198    0.067    2.969    0.003
    #>     spe_2             0.198    0.062    3.192    0.001
    #>     spe_3             0.299    0.061    4.907    0.000
    #> 
    #> Regressions:
    #>                    Estimate  Std.Err  z-value  P(>|z|)
    #>   vis ~                                               
    #>     tex               0.447    0.068    6.569    0.000
    #> 
    #> Intercepts:
    #>                    Estimate  Std.Err  z-value  P(>|z|)
    #>    .vis_1             4.936    0.067   73.473    0.000
    #>    .vis_2             6.088    0.068   89.855    0.000
    #>    .vis_3             2.250    0.065   34.579    0.000
    #>    .tex_1             3.061    0.067   45.694    0.000
    #>    .tex_2             4.341    0.074   58.452    0.000
    #>    .tex_3             2.186    0.063   34.667    0.000
    #>    .spe_1             4.186    0.063   66.766    0.000
    #>    .spe_2             5.527    0.058   94.854    0.000
    #>    .spe_3             5.374    0.058   92.546    0.000
    #>    .vis               0.000                           
    #>     tex               0.000                           
    #> 
    #> Variances:
    #>                    Estimate  Std.Err  z-value  P(>|z|)
    #>    .vis_1             0.526    0.127    4.136    0.000
    #>    .vis_2             1.129    0.102   11.030    0.000
    #>    .vis_3             0.867    0.094    9.237    0.000
    #>    .tex_1             0.376    0.048    7.885    0.000
    #>    .tex_2             0.461    0.059    7.871    0.000
    #>    .tex_3             0.358    0.043    8.334    0.000
    #>    .spe_1             1.145    0.094   12.216    0.000
    #>    .spe_2             0.984    0.081   12.207    0.000
    #>    .spe_3             0.928    0.077   12.121    0.000
    #>    .vis               0.638    0.136    4.692    0.000
    #>     tex               0.975    0.112    8.714    0.000

This function accepts both quoted (character) and unquoted arguments.
So, for example, if we want to add a cross-loading from “spe_1” on
“vis”, in addition to the regression path before, we could use the
following code:

``` r
model |>
  add_paths("vis ~ tex", vis =~ spe_1) |>
  estimate_lavaan()
```

    #> lavaan 0.6-21 ended normally after 31 iterations
    #> 
    #>   Estimator                                         ML
    #>   Optimization method                           NLMINB
    #>   Number of model parameters                        28
    #> 
    #>   Number of observations                           301
    #> 
    #> Model Test User Model:
    #>                                                       
    #>   Test statistic                               288.451
    #>   Degrees of freedom                                26
    #>   P-value (Chi-square)                           0.000
