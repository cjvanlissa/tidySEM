# Extract nodes from a SEM model object

Attempts to extract nodes from a SEM model object, where nodes are
defined as observed or latent variables.

## Usage

``` r
get_nodes(x, label = paste2(name, est_sig, sep = "\n"), ...)
```

## Arguments

- x:

  A model object of class `mplusObject` or `lavaan`.

- label:

  Either a character, indicating which column to use for node labels, or
  an expression. See Details. Defaults to
  `paste(name, est_sig, sep = "\n"`, which gives the node name followed
  by the estimated value with significance asterisks.

- ...:

  Additional parameters passed to
  [`table_results`](https://cjvanlissa.github.io/tidySEM/reference/table_results.md).
  For example, users can pass the `digits` argument to control the
  number of digits in the node label, or pass the `columns` argument to
  retain auxiliary columns in the `tidy_nodes data.frame` for further
  processing (see Examples).

## Value

An object of class 'tidy_nodes'

## Details

The function `get_nodes` identifies all dependent and independent
variables in the model as nodes. If a mean structure / intercepts are
included in the model, the output of `table_results` for those means /
intercepts is used to label the nodes.

### Custom labels

One way to create custom node labels is by passing an expression to
`label`, as in the default value of the argument. When an expression is
passed to `label`, it is evaluated in the context of a `data.frame`
containing the results of a call to
[`table_results`](https://cjvanlissa.github.io/tidySEM/reference/table_results.md)
on the `x` argument, with an additional column labeled `name`, which
contains the node names.

Another way to create custom labels is by requesting auxiliary variables
using the `columns` argument (which is passed to
[`table_results`](https://cjvanlissa.github.io/tidySEM/reference/table_results.md)),
and then using these columns to construct a new label. See examples.

## Examples

``` r
# Standard use extracts node names and shape
# (rect for observed, oval for latent)
library(lavaan)
res <- sem("dist ~ speed", cars)
get_nodes(res)
#>    name shape label
#> 1  dist  rect  dist
#> 2 speed  rect speed

# To label nodes with mean values, include meanstructure in the model
# Note that it is possible to pass the argument 'digits' to table_results
# through '...'
res <- sem("dist ~ speed", cars, meanstructure = TRUE)
get_nodes(res, digits = 3)
#>    name shape           label   lhs op rhs     est    se  pval
#> 1  dist  rect dist\n-17.579**  dist ~1     -17.579 6.622 0.008
#> 2 speed  rect   speed\n15.400 speed ~1      15.400 0.000  <NA>
#>             confint   est_sig est_std se_std pval_std      confint_std
#> 1 [-30.558, -4.600] -17.579**  -0.689  0.217    0.002 [-1.115, -0.263]
#> 2  [15.400, 15.400]    15.400   2.942  0.000     <NA>   [2.942, 2.942]
#>   est_sig_std label_results
#> 1    -0.689**    Means.dist
#> 2       2.942   Means.speed

# Pass an expression to the 'label' argument for custom labels
get_nodes(res, label = paste0(name, " ", est_sig, "\n", confint))
#>    name shape                          label   lhs op rhs    est   se pval
#> 1  dist  rect dist -17.58**\n[-30.56, -4.60]  dist ~1     -17.58 6.62 0.01
#> 2 speed  rect    speed 15.40\n[15.40, 15.40] speed ~1      15.40 0.00 <NA>
#>           confint  est_sig est_std se_std pval_std    confint_std est_sig_std
#> 1 [-30.56, -4.60] -17.58**   -0.69   0.22     0.00 [-1.12, -0.26]     -0.69**
#> 2  [15.40, 15.40]    15.40    2.94   0.00     <NA>   [2.94, 2.94]        2.94
#>   label_results
#> 1    Means.dist
#> 2   Means.speed

# Pass the argument 'columns' to table_results through '...' to retain
# auxiliary columns for further processing
nod <- get_nodes(res, columns = c("est_sig", "confint"))
nod
#>    name shape          label  est_sig         confint
#> 1  dist  rect dist\n-17.58** -17.58** [-30.56, -4.60]
#> 2 speed  rect   speed\n15.40    15.40  [15.40, 15.40]
nod <- within(nod, {label <- paste0(name, " ", est_sig, "\n", confint)})
nod
#>    name shape                          label  est_sig         confint
#> 1  dist  rect dist -17.58**\n[-30.56, -4.60] -17.58** [-30.56, -4.60]
#> 2 speed  rect    speed 15.40\n[15.40, 15.40]    15.40  [15.40, 15.40]
```
