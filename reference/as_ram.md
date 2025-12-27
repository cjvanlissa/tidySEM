# Convert lavaan syntax to RAM specification

Converts SEM models to RAM models for `OpenMx`.

## Usage

``` r
as_ram(x, ...)
```

## Arguments

- x:

  An object for which a method exists, such as a `tidy_sem` object, or
  character vector describing the user-specified model using the lavaan
  model syntax.

- ...:

  Parameters passed on to other functions.

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

2.  Convert each row of the resulting lavaan parameter table to a
    [`mxPath`](https://rdrr.io/pkg/OpenMx/man/mxPath.html).

3.  Apply [`mxModel`](https://rdrr.io/pkg/OpenMx/man/mxModel.html) to
    the `mxPath`s to create an `OpenMx` model using RAM specification

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
