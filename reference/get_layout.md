# Generate graph layout

Generate a tidy_layout for a SEM graph.

## Usage

``` r
# S3 method for class 'lavaan'
get_layout(x, ..., layout_algorithm = "layout_as_tree")

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
  Defaults to `"layout_as_tree"`; see details for more options.

- rows:

  Numeric, indicating the number of rows of the graph.

## Value

Object of class 'tidy_layout'

## Details

There are three ways to generate a layout:

1.  Specify the layout in the call to `get_layout()` by providing node
    names and the number of rows to create a layout matrix. Empty
    strings (`""`) or `NA` can be used for empty cells. See Example 1.

2.  Call `get_layout()` on a model object or `tidy_results` object. It
    will use the function
    [`layout_as_tree`](https://r.igraph.org/reference/layout_as_tree.html),
    or any other layout function from the `igraph` package, to generate
    a rudimentary layout. See Example 2.

3.  Instead of using `get_layout()`, just use a `matrix` or `data.frame`
    with your layout. For example, specify the layout in a spreadsheet
    program, and load it into R (see Example 3). Or, copy the layout to
    the clipboard from your spreadsheet program, and load it from the
    clipboard (see Example 4)

The layout algorithms imported from `igraph` are:
`c("layout_as_star", "layout_as_tree", "layout_in_circle", "layout_nicely", "layout_on_grid", "layout_randomly", "layout_with_dh", "layout_with_fr", "layout_with_gem", "layout_with_graphopt", "layout_with_kk", "layout_with_lgl", "layout_with_mds")`.
These can be used by specifying the optional argument
`layout_algorithm = ""`.

## Examples

``` r
# Example 1
get_layout("c", NA,  "d",
           NA,  "e", NA, rows = 2)
#>      [,1] [,2] [,3]
#> [1,] "c"  NA   "d" 
#> [2,] NA   "e"  NA  
#> attr(,"class")
#> [1] "layout_matrix" "matrix"        "array"        

# Example 2
library(lavaan)
fit <- cfa(' visual  =~ x1 + x2 + x3 ',
           data = HolzingerSwineford1939[1:50, ])
#> Warning: lavaan->lav_object_post_check():  
#>    some estimated ov variances are negative
get_layout(fit)
#>      [,1] [,2]     [,3]
#> [1,] NA   "visual" NA  
#> [2,] "x1" "x2"     "x3"
#> attr(,"class")
#> [1] "layout_matrix" "matrix"        "array"        

if (FALSE) { # \dontrun{
# Example 3
# Here, we first write the layout to .csv, but you could create it in a
# spreadsheet program, and save the spreadsheet to .csv:
write.csv(matrix(c("c", "",  "d", "",  "e", ""), nrow = 2, byrow = TRUE),
          file = file.path(tempdir(), "example3.csv"), row.names = FALSE)
# Now, we load the .csv:
read.csv(file.path(tempdir(), "example3.csv"))

# Example 4
# For this example, make your layout in a spreadsheet program, select it, and
# copy to clipboard. Reading from the clipboard works differently in Windows
# and Mac. For this example, I used Microsoft Excel.
# On Windows, run:
read.table("clipboard", sep = "\t")
# On Mac, run:
read.table(pipe("pbpaste"), sep="\t")
} # }
```
