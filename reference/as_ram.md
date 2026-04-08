# Convert lavaan syntax to RAM specification

Converts SEM models to RAM models for `OpenMx`.

## Usage

``` r
as_ram(x, ...)

# S3 method for class 'data.frame'
as_ram(x, groups = NULL, data = NULL, threshold_method = "mxThreshold", ...)
```

## Arguments

- x:

  An object for which a method exists, such as a `tidy_sem` object, or
  character vector describing the user-specified model using the lavaan
  model syntax.

- ...:

  Parameters passed on to other functions.

- groups:

  Character, either corresponding to the name of a column in `data`, or
  else a vector of group names.

- data:

  Optional, a `data.frame` which is added to the model.

- threshold_method:

  Character, one of `c("mxThreshold", "mx_deviances")`. Method used to
  specify thresholds for ordinal indicators. See the `mxThreshold`
  function in the `OpenMx` package, and
  [`mx_deviances`](https://cjvanlissa.github.io/tidySEM/reference/mx_deviances.md),
  respectively.

## Value

Returns an [`mxModel`](https://rdrr.io/pkg/OpenMx/man/mxModel.html).

## Details

For models specified using lavaan syntax, the procedure is as follows:

1.  Apply
    [`lavaanify`](https://rdrr.io/pkg/lavaan/man/model.syntax.html) to
    the `model`. The default arguments to
    [`lavaanify`](https://rdrr.io/pkg/lavaan/man/model.syntax.html)
    correspond to those of the
    [`sem`](https://rdrr.io/pkg/lavaan/man/sem.html) function.

2.  Apply the method for `data.frame` to the resulting lavaan parameter
    table. This uses
    [`mxPath`](https://rdrr.io/pkg/OpenMx/man/mxPath.html) for means,
    regression coefficients, and (co)variances, and the specified
    `threshold_method` for thresholds.

3.  Apply [`mxModel`](https://rdrr.io/pkg/OpenMx/man/mxModel.html) to
    the resulting list of `mxPath` and `mxMatrix` elements to create an
    `mxModel` using RAM specification.

## Examples

``` r
as_ram("y ~ x")
#> MxModel 'model' 
#> type : RAM 
#> $matrices : 'A', 'S', 'F', and 'M' 
#> $algebras : NULL 
#> $penalties : NULL 
#> $constraints : NULL 
#> $intervals : NULL 
#> $latentVars : none
#> $manifestVars : 'y' and 'x' 
#> $data : NULL
#> $submodels : NULL 
#> $expectation : MxExpectationRAM 
#> $fitfunction : MxFitFunctionML 
#> $compute : NULL 
#> $independent : FALSE 
#> $options :  
#> $output : FALSE 
```
