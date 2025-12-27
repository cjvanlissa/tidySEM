# Extract edges from a SEM model object

Attempts to extract edges from a SEM model object, where edges are
defined as regression paths and covariances between variables (nodes).

## Usage

``` r
get_edges(x, label = "est_sig", ...)
```

## Arguments

- x:

  A model object of class `mplusObject` or `lavaan`.

- label:

  Either a character, indicating which column to use for edge labels, or
  an expression. See Details. Defaults to `"est_sig"`, which labels
  edges with the estimated value with significance asterisks, as
  obtained from
  [`table_results`](https://cjvanlissa.github.io/tidySEM/reference/table_results.md).
  See Details and examples for more information.

- ...:

  Additional parameters passed to
  [`table_results`](https://cjvanlissa.github.io/tidySEM/reference/table_results.md).
  For example, users can pass the `digits` argument to control the
  number of digits in the edge label, or pass the `columns` argument to
  retain auxiliary columns in the `tidy_edges data.frame` for further
  processing (see Examples).

## Value

An object of class 'tidy_edges'

## Details

The function `get_edges` identifies all regression paths, latent
variable definitions, and covariances in the model as edges. The output
of `table_results` for those paths is used to label the edges.

### Custom labels

One way to create custom edge labels is by passing an expression to
`label`. When an expression is passed to `label`, it is evaluated in the
context of a `data.frame` containing the results of a call to
[`table_results`](https://cjvanlissa.github.io/tidySEM/reference/table_results.md)
on the `x` argument.

Another way to create custom labels is by requesting auxiliary variables
using the `columns` argument (which is passed to
[`table_results`](https://cjvanlissa.github.io/tidySEM/reference/table_results.md)),
and then using these columns to construct a new label. See examples.

## Examples

``` r
# Standard use
library(lavaan)
res <- sem("dist ~ speed", cars)
get_edges(res)
#>    from    to arrow     label connect_from connect_to curvature linetype   lhs
#> 1 speed  dist  last   3.93***           NA         NA        NA        1  dist
#> 2  dist  dist  both 227.07***           NA         NA        NA        1  dist
#> 3 speed speed  both     27.40           NA         NA        NA        1 speed
#>   op   rhs    est    se pval          confint   est_sig est_std se_std pval_std
#> 1  ~ speed   3.93  0.41 0.00     [3.13, 4.73]   3.93***    0.81   0.04     0.00
#> 2 ~~  dist 227.07 45.41 0.00 [138.06, 316.08] 227.07***    0.35   0.07     0.00
#> 3 ~~ speed  27.40  0.00 <NA>   [27.40, 27.40]     27.40    1.00   0.00     <NA>
#>    confint_std est_sig_std   label_results  show
#> 1 [0.73, 0.89]     0.81***   dist.ON.speed  TRUE
#> 2 [0.22, 0.48]     0.35***  Variances.dist  TRUE
#> 3 [1.00, 1.00]        1.00 Variances.speed FALSE

# Pass an expression to the 'label' argument for custom labels
get_edges(res, label = paste(est_sig, confint))
#>    from    to arrow                      label connect_from connect_to
#> 1 speed  dist  last       3.93*** [3.13, 4.73]           NA         NA
#> 2  dist  dist  both 227.07*** [138.06, 316.08]           NA         NA
#> 3 speed speed  both       27.40 [27.40, 27.40]           NA         NA
#>   curvature linetype   lhs op   rhs    est    se pval          confint
#> 1        NA        1  dist  ~ speed   3.93  0.41 0.00     [3.13, 4.73]
#> 2        NA        1  dist ~~  dist 227.07 45.41 0.00 [138.06, 316.08]
#> 3        NA        1 speed ~~ speed  27.40  0.00 <NA>   [27.40, 27.40]
#>     est_sig est_std se_std pval_std  confint_std est_sig_std   label_results
#> 1   3.93***    0.81   0.04     0.00 [0.73, 0.89]     0.81***   dist.ON.speed
#> 2 227.07***    0.35   0.07     0.00 [0.22, 0.48]     0.35***  Variances.dist
#> 3     27.40    1.00   0.00     <NA> [1.00, 1.00]        1.00 Variances.speed
#>    show
#> 1  TRUE
#> 2  TRUE
#> 3 FALSE

# Pass the argument 'columns' to table_results through '...' to retain
# auxiliary columns for further processing
edg <- get_edges(res, columns = c("est_sig", "confint"))
edg
#>    from    to arrow     label connect_from connect_to curvature linetype
#> 1 speed  dist  last   3.93***           NA         NA        NA        1
#> 2  dist  dist  both 227.07***           NA         NA        NA        1
#> 3 speed speed  both     27.40           NA         NA        NA        1
#>     est_sig          confint
#> 1   3.93***     [3.13, 4.73]
#> 2 227.07*** [138.06, 316.08]
#> 3     27.40   [27.40, 27.40]
edg <- within(edg, {label <- paste(est_sig, confint)})
edg
#>    from    to arrow                      label connect_from connect_to
#> 1 speed  dist  last       3.93*** [3.13, 4.73]           NA         NA
#> 2  dist  dist  both 227.07*** [138.06, 316.08]           NA         NA
#> 3 speed speed  both       27.40 [27.40, 27.40]           NA         NA
#>   curvature linetype   est_sig          confint
#> 1        NA        1   3.93***     [3.13, 4.73]
#> 2        NA        1 227.07*** [138.06, 316.08]
#> 3        NA        1     27.40   [27.40, 27.40]
```
