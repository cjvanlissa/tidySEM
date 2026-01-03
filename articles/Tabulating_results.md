# Tabulating results from structural equation models

``` r
library(tidySEM)
library(lavaan)
library(MplusAutomation)
```

`tidySEM` tabulates the results of different types of models in the same
uniform way. This facilitates parsing the output into Tables and Figures
for publication. The function to tabulate output is
[`table_results()`](https://cjvanlissa.github.io/tidySEM/reference/table_results.md).
The function to tabulate fit indices is
[`table_fit()`](https://cjvanlissa.github.io/tidySEM/reference/table_fit.md).

Let’s use a classic `lavaan` tutorial example for a multiple group
model, using the `HolzingerSwineford1939` data. The `tidySEM` package
has a function
[`measurement()`](https://cjvanlissa.github.io/tidySEM/reference/measurement.md)
to generate measurement models automatically. It guesses which latent
variable an observed variable belongs to by splitting the names (by
default, at the last `_` symbol), so it helps to rename the variables:

``` r
df <- HolzingerSwineford1939
names(df)[7:15] <- paste0(rep(c("vis", "tex", "spe"), each = 3), "_", rep(1:3, 3))
df |>
  subset(select = c("school", "vis_1", "vis_2", "vis_3", "tex_1", "tex_2", "tex_3", "spe_1", 
"spe_2", "spe_3")) -> df
```

Now, let’s construct the model.

``` r
df |>
  tidy_sem() |>
  measurement() -> model
```

## Output from lavaan

Now, let’s run the model in `lavaan`. You can either use `lavaan` to run
it,

``` r
model |>
  estimate_lavaan() -> fit_lav
```

The results can be tabulated using
[`table_results()`](https://cjvanlissa.github.io/tidySEM/reference/table_results.md):

``` r
table_results(fit_lav)
#>              label est_sig   se pval      confint
#> 1     vis.BY.vis_1    1.00 0.00 <NA> [1.00, 1.00]
#> 2     vis.BY.vis_2 0.55*** 0.10 0.00 [0.36, 0.75]
#> 3     vis.BY.vis_3 0.73*** 0.11 0.00 [0.52, 0.94]
#> 4     tex.BY.tex_1    1.00 0.00 <NA> [1.00, 1.00]
#> 5     tex.BY.tex_2 1.11*** 0.07 0.00 [0.98, 1.24]
#> 6     tex.BY.tex_3 0.93*** 0.06 0.00 [0.82, 1.03]
#> 7     spe.BY.spe_1    1.00 0.00 <NA> [1.00, 1.00]
#> 8     spe.BY.spe_2 1.18*** 0.16 0.00 [0.86, 1.50]
#> 9     spe.BY.spe_3 1.08*** 0.15 0.00 [0.79, 1.38]
#> 10 Variances.vis_1 0.55*** 0.11 0.00 [0.33, 0.77]
#> 11 Variances.vis_2 1.13*** 0.10 0.00 [0.93, 1.33]
#> 12 Variances.vis_3 0.84*** 0.09 0.00 [0.67, 1.02]
#> 13 Variances.tex_1 0.37*** 0.05 0.00 [0.28, 0.46]
#> 14 Variances.tex_2 0.45*** 0.06 0.00 [0.33, 0.56]
#> 15 Variances.tex_3 0.36*** 0.04 0.00 [0.27, 0.44]
#> 16 Variances.spe_1 0.80*** 0.08 0.00 [0.64, 0.96]
#> 17 Variances.spe_2 0.49*** 0.07 0.00 [0.34, 0.63]
#> 18 Variances.spe_3 0.57*** 0.07 0.00 [0.43, 0.70]
#> 19   Variances.vis 0.81*** 0.15 0.00 [0.52, 1.09]
#> 20   Variances.tex 0.98*** 0.11 0.00 [0.76, 1.20]
#> 21   Variances.spe 0.38*** 0.09 0.00 [0.21, 0.55]
#> 22    vis.WITH.tex 0.41*** 0.07 0.00 [0.26, 0.55]
#> 23    vis.WITH.spe 0.26*** 0.06 0.00 [0.15, 0.37]
#> 24    tex.WITH.spe 0.17*** 0.05 0.00 [0.08, 0.27]
#> 25     Means.vis_1 4.94*** 0.07 0.00 [4.80, 5.07]
#> 26     Means.vis_2 6.09*** 0.07 0.00 [5.96, 6.22]
#> 27     Means.vis_3 2.25*** 0.07 0.00 [2.12, 2.38]
#> 28     Means.tex_1 3.06*** 0.07 0.00 [2.93, 3.19]
#> 29     Means.tex_2 4.34*** 0.07 0.00 [4.19, 4.49]
#> 30     Means.tex_3 2.19*** 0.06 0.00 [2.06, 2.31]
#> 31     Means.spe_1 4.19*** 0.06 0.00 [4.06, 4.31]
#> 32     Means.spe_2 5.53*** 0.06 0.00 [5.41, 5.64]
#> 33     Means.spe_3 5.37*** 0.06 0.00 [5.26, 5.49]
#> 34       Means.vis    0.00 0.00 <NA> [0.00, 0.00]
#> 35       Means.tex    0.00 0.00 <NA> [0.00, 0.00]
#> 36       Means.spe    0.00 0.00 <NA> [0.00, 0.00]
```

``` r
table_fit(fit_lav)
#>      Name Parameters      fmin    chisq df       pvalue baseline.chisq
#> 1 fit_lav         30 0.1417035 85.30552 24 8.502553e-09       918.8516
#>   baseline.df baseline.pvalue       cfi       tli      nnfi       rfi       nfi
#> 1          36               0 0.9305597 0.8958395 0.8958395 0.8607411 0.9071607
#>        pnfi       ifi       rni        LL unrestricted.logl     aic      bic
#> 1 0.6047738 0.9314908 0.9305597 -3737.745         -3695.092 7535.49 7646.703
#>     n    bic2      rmsea rmsea.ci.lower rmsea.ci.upper rmsea.ci.level
#> 1 301 7551.56 0.09212148     0.07141849       0.113678            0.9
#>   rmsea.pvalue rmsea.close.h0 rmsea.notclose.pvalue rmsea.notclose.h0
#> 1 0.0006612368           0.05             0.8395529              0.08
#>          rmr rmr_nomean      srmr srmr_bentler srmr_bentler_nomean       crmr
#> 1 0.07502369 0.08218433 0.0595238    0.0595238          0.06520506 0.06520506
#>   crmr_nomean srmr_mplus srmr_mplus_nomean    cn_05   cn_01       gfi      agfi
#> 1  0.07290147  0.0595238        0.06520506 129.4902 152.654 0.9960745 0.9911676
#>        pgfi       mfi      ecvi
#> 1 0.4426998 0.9031773 0.4827426
```

## Output from OpenMx

Now, we’ll reproduce the same analysis in ‘OpenMx’. Note that this
requires loading the `OpenMx` package. First, we run the model:

``` r
library(OpenMx)
model |>
  estimate_mx() -> fit_mx
table_results(fit_mx)
table_fit(fit_mx)
```

    #> To take full advantage of multiple cores, use:
    #>   mxOption(key='Number of Threads', value=parallel::detectCores()) #now
    #>   Sys.setenv(OMP_NUM_THREADS=parallel::detectCores()) #before library(OpenMx)
    #>                       label est_sig   se pval      confint
    #> 1     Loadings.vis.BY.vis_1    1.00 <NA> <NA>         <NA>
    #> 2     Loadings.vis.BY.vis_2 0.55*** 0.11 0.00 [0.34, 0.77]
    #> 3     Loadings.vis.BY.vis_3 0.73*** 0.12 0.00 [0.50, 0.96]
    #> 4     Loadings.tex.BY.tex_1    1.00 <NA> <NA>         <NA>
    #> 5     Loadings.tex.BY.tex_2 1.11*** 0.06 0.00 [0.99, 1.24]
    #> 6     Loadings.tex.BY.tex_3 0.93*** 0.06 0.00 [0.82, 1.04]
    #> 7     Loadings.spe.BY.spe_1    1.00 <NA> <NA>         <NA>
    #> 8     Loadings.spe.BY.spe_2 1.18*** 0.15 0.00 [0.89, 1.47]
    #> 9     Loadings.spe.BY.spe_3 1.08*** 0.20 0.00 [0.70, 1.46]
    #> 10              Means.vis_1 4.94*** 0.07 0.00 [4.80, 5.07]
    #> 11              Means.vis_2 6.09*** 0.07 0.00 [5.96, 6.22]
    #> 12              Means.vis_3 2.25*** 0.07 0.00 [2.12, 2.38]
    #> 13              Means.tex_1 3.06*** 0.07 0.00 [2.93, 3.19]
    #> 14              Means.tex_2 4.34*** 0.07 0.00 [4.19, 4.49]
    #> 15              Means.tex_3 2.19*** 0.06 0.00 [2.06, 2.31]
    #> 16              Means.spe_1 4.19*** 0.06 0.00 [4.06, 4.31]
    #> 17              Means.spe_2 5.53*** 0.06 0.00 [5.41, 5.64]
    #> 18              Means.spe_3 5.37*** 0.06 0.00 [5.26, 5.49]
    #> 19          Variances.vis_1 0.55*** 0.12 0.00 [0.32, 0.78]
    #> 20            Variances.vis 0.81*** 0.15 0.00 [0.52, 1.10]
    #> 21 Covariances.vis.WITH.tex 0.41*** 0.08 0.00 [0.25, 0.56]
    #> 22 Covariances.vis.WITH.spe 0.26*** 0.06 0.00 [0.15, 0.37]
    #> 23            Variances.tex 0.98*** 0.11 0.00 [0.76, 1.20]
    #> 24 Covariances.tex.WITH.spe 0.17*** 0.05 0.00 [0.08, 0.27]
    #> 25            Variances.spe 0.38*** 0.09 0.00 [0.20, 0.56]
    #> 26          Variances.vis_2 1.13*** 0.10 0.00 [0.93, 1.34]
    #> 27          Variances.vis_3 0.84*** 0.10 0.00 [0.66, 1.03]
    #> 28          Variances.tex_1 0.37*** 0.05 0.00 [0.28, 0.47]
    #> 29          Variances.tex_2 0.45*** 0.06 0.00 [0.33, 0.56]
    #> 30          Variances.tex_3 0.36*** 0.04 0.00 [0.27, 0.44]
    #> 31          Variances.spe_1 0.80*** 0.09 0.00 [0.63, 0.97]
    #> 32          Variances.spe_2 0.49*** 0.09 0.00 [0.31, 0.67]
    #> 33          Variances.spe_3 0.57*** 0.09 0.00 [0.39, 0.74]
    #>   Minus2LogLikelihood   n Parameters observedStatistics   df saturatedDoF
    #> 1             7475.49 301         30               2709 2679         2655
    #>   independenceDoF saturatedParameters independenceParameters ChiDoF satDoF
    #> 1            2691                  54                     18     24   2655
    #>   indDoF RMSEANull modelName     AIC      BIC   saBIC        LL
    #> 1   2691      0.05     model 7535.49 7646.703 7551.56 -3737.745

## Output from Mplus

Now, we’ll reproduce the same analysis in ‘Mplus’. To illustrate the
fact that `tidySEM` is compatible with existing solutions, we will
specify the syntax for this example manually, using the package
`MplusAutomation`. This code will only work on your machine if you have
Mplus installed and R can find it. First, we run the model:

``` r
fit_mplus <- mplusModeler(mplusObject(VARIABLE = "grouping IS school (1 = GW 2 = Pas);",
                                MODEL = c("visual BY vis_1 vis_2 vis_3;",
                                          "textual BY tex_1 tex_2 tex_3;",
                                          "speed BY spe_1 spe_2 spe_3;"),
                                usevariables = names(df),
                                rdata = df),
                    modelout = "example.inp",
                    run = 1L)
table_results(fit_mplus)
```

``` r
table_results(fit_mplus)
#> Calculated confidence intervals from est and se.
#>                           label  est_sig   se pval        confint group
#> 1            VISUAL.BY.VIS_1.GW     1.00 <NA> <NA>           <NA>    GW
#> 2            VISUAL.BY.VIS_2.GW  0.58*** 0.11 0.00 [ 0.36,  0.79]    GW
#> 3            VISUAL.BY.VIS_3.GW  0.80*** 0.13 0.00 [ 0.54,  1.05]    GW
#> 4           TEXTUAL.BY.TEX_1.GW     1.00 <NA> <NA>           <NA>    GW
#> 5           TEXTUAL.BY.TEX_2.GW  1.12*** 0.07 0.00 [ 0.99,  1.25]    GW
#> 6           TEXTUAL.BY.TEX_3.GW  0.93*** 0.06 0.00 [ 0.82,  1.04]    GW
#> 7             SPEED.BY.SPE_1.GW     1.00 <NA> <NA>           <NA>    GW
#> 8             SPEED.BY.SPE_2.GW  1.13*** 0.14 0.00 [ 0.86,  1.40]    GW
#> 9             SPEED.BY.SPE_3.GW  1.01*** 0.16 0.00 [ 0.70,  1.32]    GW
#> 10       TEXTUAL.WITH.VISUAL.GW  0.43*** 0.10 0.00 [ 0.23,  0.62]    GW
#> 11         SPEED.WITH.VISUAL.GW  0.33*** 0.08 0.00 [ 0.16,  0.49]    GW
#> 12        SPEED.WITH.TEXTUAL.GW   0.24** 0.07 0.00 [ 0.09,  0.38]    GW
#> 13              Means.VISUAL.GW     0.00 <NA> <NA>           <NA>    GW
#> 14             Means.TEXTUAL.GW     0.00 <NA> <NA>           <NA>    GW
#> 15               Means.SPEED.GW     0.00 <NA> <NA>           <NA>    GW
#> 16          Intercepts.VIS_1.GW  4.85*** 0.09 0.00 [ 4.67,  5.04]    GW
#> 17          Intercepts.VIS_2.GW  6.07*** 0.08 0.00 [ 5.92,  6.22]    GW
#> 18          Intercepts.VIS_3.GW  2.15*** 0.08 0.00 [ 1.99,  2.32]    GW
#> 19          Intercepts.TEX_1.GW  3.35*** 0.09 0.00 [ 3.18,  3.53]    GW
#> 20          Intercepts.TEX_2.GW  4.68*** 0.10 0.00 [ 4.49,  4.87]    GW
#> 21          Intercepts.TEX_3.GW  2.46*** 0.08 0.00 [ 2.30,  2.63]    GW
#> 22          Intercepts.SPE_1.GW  4.07*** 0.08 0.00 [ 3.90,  4.23]    GW
#> 23          Intercepts.SPE_2.GW  5.43*** 0.08 0.00 [ 5.27,  5.59]    GW
#> 24          Intercepts.SPE_3.GW  5.29*** 0.08 0.00 [ 5.13,  5.44]    GW
#> 25          Variances.VISUAL.GW  0.71*** 0.16 0.00 [ 0.39,  1.03]    GW
#> 26         Variances.TEXTUAL.GW  0.87*** 0.13 0.00 [ 0.61,  1.13]    GW
#> 27           Variances.SPEED.GW  0.51*** 0.12 0.00 [ 0.27,  0.74]    GW
#> 28  Residual.Variances.VIS_1.GW  0.65*** 0.13 0.00 [ 0.40,  0.91]    GW
#> 29  Residual.Variances.VIS_2.GW  0.96*** 0.13 0.00 [ 0.72,  1.21]    GW
#> 30  Residual.Variances.VIS_3.GW  0.64*** 0.11 0.00 [ 0.42,  0.86]    GW
#> 31  Residual.Variances.TEX_1.GW  0.34*** 0.06 0.00 [ 0.22,  0.47]    GW
#> 32  Residual.Variances.TEX_2.GW  0.38*** 0.07 0.00 [ 0.23,  0.52]    GW
#> 33  Residual.Variances.TEX_3.GW  0.44*** 0.07 0.00 [ 0.30,  0.57]    GW
#> 34  Residual.Variances.SPE_1.GW  0.62*** 0.10 0.00 [ 0.42,  0.83]    GW
#> 35  Residual.Variances.SPE_2.GW  0.43*** 0.10 0.00 [ 0.24,  0.63]    GW
#> 36  Residual.Variances.SPE_3.GW  0.52*** 0.10 0.00 [ 0.32,  0.72]    GW
#> 37          VISUAL.BY.VIS_1.PAS     1.00 <NA> <NA>           <NA>   PAS
#> 38          VISUAL.BY.VIS_2.PAS  0.58*** 0.11 0.00 [ 0.36,  0.79]   PAS
#> 39          VISUAL.BY.VIS_3.PAS  0.80*** 0.13 0.00 [ 0.54,  1.05]   PAS
#> 40         TEXTUAL.BY.TEX_1.PAS     1.00 <NA> <NA>           <NA>   PAS
#> 41         TEXTUAL.BY.TEX_2.PAS  1.12*** 0.07 0.00 [ 0.99,  1.25]   PAS
#> 42         TEXTUAL.BY.TEX_3.PAS  0.93*** 0.06 0.00 [ 0.82,  1.04]   PAS
#> 43           SPEED.BY.SPE_1.PAS     1.00 <NA> <NA>           <NA>   PAS
#> 44           SPEED.BY.SPE_2.PAS  1.13*** 0.14 0.00 [ 0.86,  1.40]   PAS
#> 45           SPEED.BY.SPE_3.PAS  1.01*** 0.16 0.00 [ 0.70,  1.32]   PAS
#> 46      TEXTUAL.WITH.VISUAL.PAS  0.41*** 0.11 0.00 [ 0.20,  0.62]   PAS
#> 47        SPEED.WITH.VISUAL.PAS   0.18** 0.07 0.01 [ 0.05,  0.31]   PAS
#> 48       SPEED.WITH.TEXTUAL.PAS   0.18** 0.06 0.00 [ 0.06,  0.30]   PAS
#> 49             Means.VISUAL.PAS     0.15 0.13 0.24 [-0.10,  0.40]   PAS
#> 50            Means.TEXTUAL.PAS -0.58*** 0.12 0.00 [-0.81, -0.35]   PAS
#> 51              Means.SPEED.PAS     0.18 0.09 0.06 [-0.01,  0.36]   PAS
#> 52         Intercepts.VIS_1.PAS  4.85*** 0.09 0.00 [ 4.67,  5.04]   PAS
#> 53         Intercepts.VIS_2.PAS  6.07*** 0.08 0.00 [ 5.92,  6.22]   PAS
#> 54         Intercepts.VIS_3.PAS  2.15*** 0.08 0.00 [ 1.99,  2.32]   PAS
#> 55         Intercepts.TEX_1.PAS  3.35*** 0.09 0.00 [ 3.18,  3.53]   PAS
#> 56         Intercepts.TEX_2.PAS  4.68*** 0.10 0.00 [ 4.49,  4.87]   PAS
#> 57         Intercepts.TEX_3.PAS  2.46*** 0.08 0.00 [ 2.30,  2.63]   PAS
#> 58         Intercepts.SPE_1.PAS  4.07*** 0.08 0.00 [ 3.90,  4.23]   PAS
#> 59         Intercepts.SPE_2.PAS  5.43*** 0.08 0.00 [ 5.27,  5.59]   PAS
#> 60         Intercepts.SPE_3.PAS  5.29*** 0.08 0.00 [ 5.13,  5.44]   PAS
#> 61         Variances.VISUAL.PAS  0.80*** 0.19 0.00 [ 0.42,  1.17]   PAS
#> 62        Variances.TEXTUAL.PAS  0.88*** 0.13 0.00 [ 0.62,  1.14]   PAS
#> 63          Variances.SPEED.PAS  0.32*** 0.09 0.00 [ 0.16,  0.49]   PAS
#> 64 Residual.Variances.VIS_1.PAS  0.56*** 0.16 0.00 [ 0.25,  0.86]   PAS
#> 65 Residual.Variances.VIS_2.PAS  1.30*** 0.16 0.00 [ 0.98,  1.61]   PAS
#> 66 Residual.Variances.VIS_3.PAS  0.94*** 0.15 0.00 [ 0.65,  1.23]   PAS
#> 67 Residual.Variances.TEX_1.PAS  0.45*** 0.07 0.00 [ 0.30,  0.59]   PAS
#> 68 Residual.Variances.TEX_2.PAS  0.50*** 0.09 0.00 [ 0.33,  0.67]   PAS
#> 69 Residual.Variances.TEX_3.PAS  0.26*** 0.05 0.00 [ 0.16,  0.36]   PAS
#> 70 Residual.Variances.SPE_1.PAS  0.89*** 0.13 0.00 [ 0.64,  1.14]   PAS
#> 71 Residual.Variances.SPE_2.PAS  0.54*** 0.10 0.00 [ 0.35,  0.74]   PAS
#> 72 Residual.Variances.SPE_3.PAS  0.65*** 0.10 0.00 [ 0.46,  0.85]   PAS
table_fit(fit_mplus)
#>   Mplus.version Name AnalysisType   DataType Estimator   n NGroups
#> 1           8.6           GENERAL INDIVIDUAL        ML 301       2
#>   NDependentVars NIndependentVars NContinuousLatentVars Parameters ChiSqM_Value
#> 1              9                0                     3         48      164.103
#>   ChiSqM_DF ChiSqM_PValue ChiSqBaseline_Value ChiSqBaseline_DF
#> 1        60             0             957.769               72
#>   ChiSqBaseline_PValue        LL UnrestrictedLL   CFI   TLI      AIC      BIC
#> 1                    0 -3706.323      -3624.272 0.882 0.859 7508.647 7686.588
#>       aBIC RMSEA_Estimate RMSEA_90CI_LB RMSEA_90CI_UB RMSEA_pLT05  SRMR
#> 1 7534.359          0.107         0.088         0.127           0 0.087
#>       AICC    Filename
#> 1 7527.314 example.out
```
