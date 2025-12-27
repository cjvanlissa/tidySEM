# Switch LCA Class Labels

The order of class labels in LCA is arbitrary. This can result in a
phenomenon called 'label switching', where classes change places between
replications of an analysis. This function attempts to re-order classes
in a substantively meaningful way.

## Usage

``` r
mx_switch_labels(x, param = "weights", decreasing = TRUE, order = NULL)
```

## Arguments

- x:

  An `MxModel` estimated by `mx_mixture` or one of its wrappers.

- param:

  The parameter by which to order the classes, defaults to `'weights'`,
  which orders classes based on their sample size.

- decreasing:

  logical. Should the classes be sorted in increasing or decreasing
  order? Default: TRUE

- order:

  Integer, indicating the ordering of classes. Ignored when NULL
  (default).

## Value

An `MxModel` with `"tidySEM"` attribute: `"mixture"`

## Details

The argument `param` can accept either:

1.  The default string "weights", in which classes are sorted by size.

2.  The `OpenMx` matrix indicator for a specific model parameter; e.g.,
    the first mean is indicated by `"M[1,1]"`. These indicators can be
    viewed by running `table_results(x, columns = NULL)`.

3.  The letter indicating an `OpenMx` model matrix, e.g., `"M"` refers
    to the matrix of means. To account for all elements of the matrix,
    Euclidean distance to the origin is used.

## Examples

``` r
if (FALSE) { # \dontrun{
df <- iris[1:4]
names(df) <- letters[1:4]
res1 <- mx_profiles(data = df, classes = 2)
mx_switch_labels(res1, decreasing = FALSE)
} # }
```
