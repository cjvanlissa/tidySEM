# Register an External tidySEM Method

Allows extension packages to register implementations for tidySEM
generics.

## Usage

``` r
register_tidySEM_method(generic, type, fun)
```

## Arguments

- generic:

  Name of the generic function.

- type:

  Value of the model's `tidySEM` attribute.

- fun:

  Function implementing the method.
