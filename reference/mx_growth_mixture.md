# Estimate growth mixture models using OpenMx

This function is a wrapper around
[`mx_mixture`](https://cjvanlissa.github.io/tidySEM/reference/mx_mixture.md),
adding the default arguments of
[`lavaan::growth()`](https://rdrr.io/pkg/lavaan/man/growth.html) to
simplify the specification of growth mixture models. This function is
only useful if all the latent variables in the model are growth factors.

## Usage

``` r
mx_growth_mixture(model, classes = 1L, data = NULL, run = TRUE, ...)
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

## References

Van Lissa, C. J., Garnier-Villarreal, M., & Anadria, D. (2023).
Recommended Practices in Latent Class Analysis using the Open-Source
R-Package tidySEM. Structural Equation Modeling.
[doi:10.1080/10705511.2023.2250920](https://doi.org/10.1080/10705511.2023.2250920)

## Examples

``` r
if (FALSE) { # \dontrun{
data("empathy")
df <- empathy[1:6]
mx_growth_mixture(model = "i =~ 1*ec1 + 1*ec2 + 1*ec3 +1*ec4 +1*ec5 +1*ec6
                           s =~ 0*ec1 + 1*ec2 + 2*ec3 +3*ec4 +4*ec5 +5*ec6
                           ec1 ~~ vec1*ec1
                           ec2 ~~ vec2*ec2
                           ec3 ~~ vec3*ec3
                           ec4 ~~ vec4*ec4
                           ec5 ~~ vec5*ec5
                           ec6 ~~ vec6*ec6
                           i ~~ 0*i
                           s ~~ 0*s
                           i ~~ 0*s",
                  classes = 2,
                  data = df) -> res
} # }
```
