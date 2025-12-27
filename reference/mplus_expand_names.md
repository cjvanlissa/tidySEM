# Expand abbreviated Mplus variable names

Expand the Mplus syntax for abbreviating lists of variable names.

## Usage

``` r
mplus_expand_names(x)
```

## Arguments

- x:

  Atomic character string containing the variable names section of an
  Mplus syntax file.

## Value

Character vector of names.

## Examples

``` r
mplus_expand_names("test1-test12")
#>  [1] "test1"  "test2"  "test3"  "test4"  "test5"  "test6"  "test7"  "test8" 
#>  [9] "test9"  "test10" "test11" "test12"
mplus_expand_names("testa-testb")
#> [1] "testa" "testb"
```
