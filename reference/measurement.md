# Generate syntax for a measurement model

Generate syntax for a measurement model for latent variables. This
function relies on
[`add_paths`](https://cjvanlissa.github.io/tidySEM/reference/add_paths.md)
to generate syntax.

## Usage

``` r
measurement(x, ...)
```

## Arguments

- x:

  An object for which a method exists, including `tidy_sem` (generated
  using
  [`dictionary`](https://cjvanlissa.github.io/tidySEM/reference/dictionary.md),
  or `data.frame` (for which
  [`dictionary`](https://cjvanlissa.github.io/tidySEM/reference/dictionary.md)
  will be run first).

- ...:

  Additional parameters passed to
  [`add_paths`](https://cjvanlissa.github.io/tidySEM/reference/add_paths.md).

## Value

An object of class `tidy_sem`.

## Examples

``` r
dict <- tidy_sem(c("bfi_1", "bfi_2", "bfi_3", "bfi_4", "bfi_5"))
measurement(dict)
#> A tidy_sem object
#> v    $dictionary
#> o    $data
#> v    $syntax
```
