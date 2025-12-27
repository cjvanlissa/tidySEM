# Wald Test for Linear Hypotheses

This function is a wrapper for the function
[`car::linearHypothesis()`](https://rdrr.io/pkg/car/man/linearHypothesis.html),
but which uses the
[`bain::bain()`](https://rdrr.io/pkg/bain/man/bain.html) syntax to parse
equality constrained hypotheses.

## Usage

``` r
wald_test(x, hypothesis, ...)
```

## Arguments

- x:

  An object for which a method exists.

- hypothesis:

  A character string with equality constrained hypotheses, specified
  according to the
  [`bain::bain()`](https://rdrr.io/pkg/bain/man/bain.html) syntax.

- ...:

  Additional arguments passed to
  [`car::linearHypothesis()`](https://rdrr.io/pkg/car/man/linearHypothesis.html).

## Value

A `data.frame` of class `wald_test`.

## See also

[`linearHypothesis`](https://rdrr.io/pkg/car/man/linearHypothesis.html)

## Examples

``` r
mod <- lm(Sepal.Length ~ Sepal.Width, data = iris)
coef(mod)
#> (Intercept) Sepal.Width 
#>   6.5262226  -0.2233611 
wald_test(mod, "Sepal.Width = 0")
#> Wald tests for linear hypotheses:
#>     Hypothesis Res.Df      RSS Df Sum.of.Sq        F         p
#>  Sepal.Width=0    148 100.7561  1  1.412238 2.074427 0.1518983
```
