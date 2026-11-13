# Conduct Bootstrapped Likelihood Ratio Test

Conduct Bootstrapped Likelihood Ratio Test to compare two mixture
models.

## Usage

``` r
BLRT(x, replications = 100, ...)
```

## Arguments

- x:

  An object for which a method exists.

- replications:

  Integer reflecting the number of bootstrapped replications, defaults
  to `100`.

- ...:

  further arguments to be passed to or from other methods.

## Value

A data.frame.

## Examples

``` r
if (FALSE) { # \dontrun{
df <- iris[, 1, drop = FALSE]
names(df) <- "x"
res <- mx_mixture(model = "x ~ m{C}*1
                           x ~~ v{C}*x", classes = 1:2, data = df)
BLRT(res, replications = 4)
} # }
```
