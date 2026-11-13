# Dummy Code Factor Variables

For each variable *v* that inherits `factor`, create a number of new
variables equal to `levels(v)` to indicate group membership (1) or
non-membership (0) of that level. The resulting dummies have class
`mxFactor`.

## Usage

``` r
mx_dummies(x, classes = c("factor", "character"), ...)
```

## Arguments

- x:

  An object for which a method exists.

- classes:

  Character vector, indicating which classes to dummy code. Defaults to
  `c("factor", "character")`.

- ...:

  Arguments

## Value

A `data.frame`.

## Examples

``` r
if(requireNamespace("OpenMx", quietly = TRUE)){
mx_dummies(iris[1:5,])
}
#> Warning: Some categories have fewer than 5% of cases: Species.csetosa, Species.cversicolor, Species.cvirginica
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width
#> 1          5.1         3.5          1.4         0.2
#> 2          4.9         3.0          1.4         0.2
#> 3          4.7         3.2          1.3         0.2
#> 4          4.6         3.1          1.5         0.2
#> 5          5.0         3.6          1.4         0.2
```
