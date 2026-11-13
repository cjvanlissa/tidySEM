# Extract dictionary from tidy_sem

Provides access to the `dictionary` element of a `tidy_sem` object. This
can be used to return or assign to the `dictionary` element.

## Usage

``` r
dictionary(x)

dictionary(x) <- value
```

## Arguments

- x:

  Object of class tidy_sem.

- value:

  A valid value for `dictionary(x)`.

## Value

data.frame

## Examples

``` r
dict <- tidy_sem(iris, split = "\\.")
dictionary(dict)
#>           name scale     type        label
#> 1 Sepal.Length Sepal observed Sepal.Length
#> 2  Sepal.Width Sepal observed  Sepal.Width
#> 3 Petal.Length Petal observed Petal.Length
#> 4  Petal.Width Petal observed  Petal.Width
#> 5      Species  <NA> observed      Species
```
