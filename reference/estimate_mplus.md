# Estimate tidy_sem using 'Mplus'

This function is a wrapper for the functions
[`mplusObject`](https://michaelhallquist.github.io/MplusAutomation/reference/mplusObject.html)
and
[`mplusModeler`](https://michaelhallquist.github.io/MplusAutomation/reference/mplusModeler.html).
Using this function requires 'Mplus' to be installed.

## Usage

``` r
estimate_mplus(x, ...)
```

## Arguments

- x:

  An object of class `tidy_sem`.

- ...:

  Additional parameters passed to
  [`mplusObject`](https://michaelhallquist.github.io/MplusAutomation/reference/mplusObject.html)
  and
  [`mplusModeler`](https://michaelhallquist.github.io/MplusAutomation/reference/mplusModeler.html).
  These arguments are matched to the correct function by name. The
  arguments `rdata`, and `MODEL` cannot be edited, as they are
  determined from the `tidy_sem` object.

## Value

An object of class `mplusObject`.

## Details

The arguments `dataout`, `modelout`, and `run` are optional. If these
are not specified, the model will be run in
[`tempdir`](https://rdrr.io/r/base/tempfile.html).

## Examples

``` r
library(MplusAutomation)
#> Version:  1.2
#> We work hard to write this free software. Please help us get credit by citing: 
#> 
#> Hallquist, M. N. & Wiley, J. F. (2018). MplusAutomation: An R Package for Facilitating Large-Scale Latent Variable Analyses in Mplus. Structural Equation Modeling, 25, 621-638. doi: 10.1080/10705511.2017.1402334.
#> 
#> -- see citation("MplusAutomation").
model <- tidy_sem(iris, "\\.")
model <- measurement(model)
if (FALSE) { # \dontrun{
  estimate_mplus(model, run = 0L)
} # }
```
