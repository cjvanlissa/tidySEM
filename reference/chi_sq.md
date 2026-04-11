# Calculate Chi Square Statistic

Given two datasets with ordinal variables, computes the lambda
statistic, which is the chi-square statistic minus the degrees of
freedom.

## Usage

``` r
chi_sq(x, y)
```

## Arguments

- x:

  An object for which a method exists (e.g, `data.frame`).

- y:

  An object for which a method exists (e.g, `data.frame`).

## Value

`numeric`

## Examples

``` r
if (FALSE) { # \dontrun{
if(interactive()){
 chi_sq(iris[1:2], iris[3:4])
}
} # }
```
