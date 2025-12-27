# Estimate latent class analyses using OpenMx

This function simplifies the specification of latent class models:
models that estimate membership of a categorical latent variable based
on binary or ordinal indicators.

## Usage

``` r
mx_lca(data = NULL, classes = 1L, run = TRUE, ...)
```

## Arguments

- data:

  The data.frame to be used for model fitting.

- classes:

  A vector of integers, indicating which class solutions to generate.
  Defaults to 1L. E.g., `classes = 1:6`,

- run:

  Logical, whether or not to run the model. If `run = TRUE`, the
  function calls
  [`OpenMx::mxTryHardOrdinal()`](https://rdrr.io/pkg/OpenMx/man/mxTryHard.html).

- ...:

  Additional arguments, passed to functions.

## Value

Returns an
[`OpenMx::mxModel()`](https://rdrr.io/pkg/OpenMx/man/mxModel.html).

## References

Van Lissa, C. J., Garnier-Villarreal, M., & Anadria, D. (2023).
Recommended Practices in Latent Class Analysis using the Open-Source
R-Package tidySEM. Structural Equation Modeling.
[doi:10.1080/10705511.2023.2250920](https://doi.org/10.1080/10705511.2023.2250920)

## Examples

``` r
if (FALSE) { # \dontrun{
df <- data_mix_ordinal
df[1:4] <- lapply(df, ordered)
mx_lca(data = df,
       classes = 2) -> res
} # }
```
