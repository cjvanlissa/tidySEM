# Calculate Chi Square Statistic

Given two datasets with ordinal variables, computes the chi squared
statistic. To obtain the lambda statistic as used in
[`pmc()`](https://cjvanlissa.github.io/tidySEM/reference/pmc.md),
subtract the degrees of freedom.

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
