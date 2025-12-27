# Apply pattern replacement over a vector

`lsub` returns a list of the same length as `replacement`, each element
of which is the result of applying
[`gsub`](https://rdrr.io/r/base/grep.html) to `x` using
[`lapply`](https://rdrr.io/r/base/lapply.html).

## Usage

``` r
lsub(x, replacement = NULL, pattern = "{C}", fixed = TRUE, ...)
```

## Arguments

- x:

  A character vector where matches are sought.

- replacement:

  a character vector of length 1 or more. Each element is applied to `x`
  in turn. Default: NULL

- pattern:

  A character string containing a regular expression (or character
  string when `fixed = TRUE`). Default: `'{C}'`.

- fixed:

  logical. If TRUE, pattern is a string to be matched as is. Default:
  TRUE

- ...:

  Parameters passed on to [`gsub`](https://rdrr.io/r/base/grep.html).

## Value

A list of results returned by
[`gsub`](https://rdrr.io/r/base/grep.html).

## Examples

``` r
lsub("a{C}", 1:3)
#> [[1]]
#> [1] "a1"
#> 
#> [[2]]
#> [1] "a2"
#> 
#> [[3]]
#> [1] "a3"
#> 
```
