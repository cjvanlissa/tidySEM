# Concatenate Strings while omitting NA

Concatenate vectors after converting to character and removing `NA`
values. See [`paste`](https://rdrr.io/r/base/paste.html).

## Usage

``` r
paste2(..., sep = " ", collapse = NULL, na.rm = TRUE)
```

## Arguments

- ...:

  one or more R objects, to be converted to character vectors.

- sep:

  a character string to separate the terms. Not `NA_character_`.

- collapse:

  an optional character string to separate the results. Not
  `NA_character_`.

- na.rm:

  logical, indicating whether `NA` values should be stripped before
  concatenation. Not `NA_character_`.

## Value

A character vector of the concatenated values.

## Examples

``` r
paste2("word", NA)
#> [1] "word"
```
