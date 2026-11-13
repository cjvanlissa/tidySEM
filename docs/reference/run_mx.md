# Run as OpenMx model with sensible defaults

This convenience function runs objects for which a method exists using
OpenMx, with sensible defaults. It is intended for use with `tidySEM`.
For instance, it will convert a `tidySEM` object to a `mxModel` and run
it, and it will try to ensure convergence for mixture models created
using
[`mx_mixture`](https://cjvanlissa.github.io/tidySEM/reference/mx_mixture.md).
Knowledgeable users may want to run models manually.

## Usage

``` r
run_mx(x, ...)
```

## Arguments

- x:

  An object for which a method exists.

- ...:

  Parameters passed on to other functions.

## Value

Returns an [`mxModel`](https://rdrr.io/pkg/OpenMx/man/mxModel.html) with
free parameters updated to their final values.

## Examples

``` r
df <- iris[1:3]
names(df) <- paste0("X_", 1:3)
run_mx(measurement(tidy_sem(df), meanstructure = TRUE))
#> MxModel 'model' 
#> type : RAM 
#> $matrices : 'A', 'S', 'F', and 'M' 
#> $algebras : NULL 
#> $penalties : NULL 
#> $constraints : NULL 
#> $intervals : NULL 
#> $latentVars : 'X' 
#> $manifestVars : 'X_1', 'X_2', and 'X_3' 
#> $data : 150 x 3 
#> $data means : NA
#> $data type: 'raw' 
#> $submodels : NULL 
#> $expectation : MxExpectationRAM 
#> $fitfunction : MxFitFunctionML 
#> $compute : MxComputeSequence 
#> $independent : FALSE 
#> $options :  
#> $output : TRUE 
```
