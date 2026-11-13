# Generate syntax for correlations

Generate syntax for correlations between variables.

## Usage

``` r
cors(x, ...)
```

## Arguments

- x:

  Object for which a method exists. If `x` is an object of class
  `tidy_sem`, then correlations between all observed and latent
  variables in the data dictionary of that object are computed, by
  default. If `x` is a character vector, all elements of the vector are
  used.

- ...:

  Optional additional character vectors of variables to be correlated.
  If `x` is an object of class `tidy_sem`, then up to two vectors can be
  provided. If `x` is a vector, then one more optional vector can be
  provided. When no additional vectors of variable names are provided,
  only the correlations among the elements of `x` are returned.

## Value

An object of class `tidy_sem`.

## Examples

``` r
dict <- tidy_sem(c("bfi_1", "bfi_2", "bfi_3", "bfi_4", "bfi_5"))
cors(dict, c("bfi_1", "bfi_2"))
#> A tidy_sem object
#> v    $dictionary
#> o    $data
#> v    $syntax
```
