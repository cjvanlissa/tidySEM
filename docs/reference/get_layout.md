# Generate graph layout

Generate a tidy_layout for a SEM graph.

## Usage

``` r
# S3 method for class 'lavaan'
get_layout(x, ..., layout_algorithm = "sem")

get_layout(x, ...)

# Default S3 method
get_layout(x, ..., rows = NULL)
```

## Arguments

- x:

  An object for which a method exists; currently, methods exist for
  `character`, `lavaan`, and `mplus.model` objects.

- ...:

  Character arguments corresponding to layout elements. Use node names,
  empty strings (""), or NA values.

- layout_algorithm:

  Optional argument for fit model objects. Character string, indicating
  which `igraph` layout algorithm to apply to position the nodes.
  Defaults to `"sem"`; see details for more options.

- rows:

  Numeric, indicating the number of rows of the graph.

## Value

Object of class 'tidy_layout'

## Details

There are three ways to generate a layout:

1.  Specify the layout in the call to `get_layout()` by providing node
    names and the number of rows to create a layout matrix. Empty
    strings (`""`) or `NA` can be used for empty cells. See Example 1.

2.  Call `get_layout()` on a model object or `tidy_results` object. You
    can either use the argument `"sem"`, to generate a SEM-specific
    layout, or `"fr_grid"` to apply the Fruchterman-Reingold
    force-directed graph algorithm, or any layout function from the
    package `igraph` if it is installed. See Example 2.

3.  Instead of using `get_layout()`, just use a `matrix` or `data.frame`
    with your layout. For example, specify the layout in a spreadsheet
    program, and load it into R (see Example 3). Or, copy the layout to
    the clipboard from your spreadsheet program, and load it from the
    clipboard (see Example 4)

## Examples
