# Estimate mixture models using OpenMx

Dynamically creates a batch of mixture models, with intelligent
defaults. See Details for more information.

## Usage

``` r
mx_mixture(model, classes = 1L, data = NULL, run = TRUE, ...)
```

## Arguments

- model:

  Syntax for the model; either a character string, or a list of
  character strings, or a list of `mxModel` objects. See Details.

- classes:

  A vector of integers, indicating which class solutions to generate.
  Defaults to 1L. E.g., `classes = 1:6`, `classes = c(1:4, 6:8)`.

- data:

  The data.frame to be used for model fitting.

- run:

  Logical, whether or not to run the model. If `run = TRUE`, the
  function calls
  [`mixture_starts`](https://cjvanlissa.github.io/tidySEM/reference/mixture_starts.md)
  and
  [`run_mx`](https://cjvanlissa.github.io/tidySEM/reference/run_mx.md).

- ...:

  Additional arguments, passed to functions.

## Value

Returns an
[`OpenMx::mxModel()`](https://rdrr.io/pkg/OpenMx/man/mxModel.html).

## Details

Model syntax can be specified in three ways, for ease of use and
flexibility:

1.  An atomic character string with lavaan syntax. Within this syntax,
    the character string `{C}` is dynamically substituted with the
    correct class number using
    [`lsub`](https://cjvanlissa.github.io/tidySEM/reference/lsub.md),
    for example to set unique parameter labels for each class, or to
    specify equality constraints. E.g., `x ~ m{C}*1` will be expanded to
    `x ~ m1*1` and `x ~ m2*1` when `classes = 2`. The resulting syntax
    for each class will be converted to an `mxModel` using
    [`as_ram`](https://cjvanlissa.github.io/tidySEM/reference/as_ram.md).

2.  A list of character strings with lavaan syntax. Each item of the
    list will be converted to a class-specific `mxModel` using
    [`as_ram`](https://cjvanlissa.github.io/tidySEM/reference/as_ram.md).

3.  A list of `mxModel` objects, specified by the user.

## References

Van Lissa, C. J., Garnier-Villarreal, M., & Anadria, D. (2023).
Recommended Practices in Latent Class Analysis using the Open-Source
R-Package tidySEM. Structural Equation Modeling.
[doi:10.1080/10705511.2023.2250920](https://doi.org/10.1080/10705511.2023.2250920)

## Examples

``` r
if (FALSE) { # \dontrun{
# Example 1: Dynamic model generation using {C}
df <- iris[, 1, drop = FALSE]
names(df) <- "x"
mx_mixture(model = "x ~ m{C}*1
                    x ~~ v{C}*x", classes = 1, data = df)
# Example 2: Manually specified class-specific models
df <- iris[1:2]
names(df) <- c("x", "y")
mx_mixture(model = list("y ~ a*x",
                        "y ~ b*x"),
                        meanstructure = TRUE,
                        data = df) -> res

# Example 3: Latent growth model
df <- empathy[1:6]
mx_mixture(model = "i =~ 1*ec1 + 1*ec2 + 1*ec3 +1*ec4 +1*ec5 +1*ec6
                    s =~ 0*ec1 + 1*ec2 + 2*ec3 +3*ec4 +4*ec5 +5*ec6",
                    classes = 2,
                    data = df) -> res
} # }
```
