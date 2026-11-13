# Extract syntax from tidy_sem

Provides access to the `syntax` element of a `tidy_sem` object. This can
be used to return or assign to the `syntax` element.

## Usage

``` r
syntax(x)

syntax(x) <- value
```

## Arguments

- x:

  Object of class tidy_sem.

- value:

  A valid value for `syntax(x)`.

## Value

data.frame

## Examples

``` r
dict <- tidy_sem(iris, split = "\\.")
dict <- add_paths(dict, Sepal.Width ~~ Sepal.Length)
syntax(dict)
#>            lhs op          rhs block group free label ustart plabel
#> 1  Sepal.Width ~~ Sepal.Length     1     1    1           NA   .p1.
#> 2  Sepal.Width ~~  Sepal.Width     1     1    1           NA   .p2.
#> 3 Sepal.Length ~~ Sepal.Length     1     1    1           NA   .p3.
#> 4  Sepal.Width ~1                  1     1    1           NA   .p4.
#> 5 Sepal.Length ~1                  1     1    1           NA   .p5.
```
