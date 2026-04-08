# Calculate Standardized Root Mean Residual

Given two datasets, computes the correlation matrix for both, and then
calculates the standardized root mean residual difference between these
two correlation matrices.

## Usage

``` r
srmr(x, y)
```

## Arguments

- x:

  An object for which a method of
  [`stats::cor()`](https://rdrr.io/r/stats/cor.html) exists (e.g,
  `data.frame`).

- y:

  An object for which a method of
  [`stats::cor()`](https://rdrr.io/r/stats/cor.html) exists (e.g,
  `data.frame`).

## Value

`numeric`

## Examples

``` r
if (FALSE) { # \dontrun{
if(interactive()){
 srmr(iris[1:2], iris[3:4])
}
} # }
```
