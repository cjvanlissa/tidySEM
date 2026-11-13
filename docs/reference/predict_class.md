# Predict Class Membership

Predict class membership for latent class analyses conducted with
`tidySEM` in `OpenMx`.

## Usage

``` r
predict_class(object, newdata, ...)
```

## Arguments

- object:

  An `MxModel` object with attribute `tidySEM = "mixture"`.

- newdata:

  A `data.frame` with the same column names and data types as the one
  used to estimate the model.

- ...:

  Other arguments passed to `predict.MxModel()`.

## Value

A `data.frame`.
