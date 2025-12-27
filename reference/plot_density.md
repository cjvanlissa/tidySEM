# Create density plots for mixture models

Creates mixture density plots. For each variable, a Total density plot
will be shown, along with separate density plots for each latent class,
where cases are weighted by the posterior probability of being assigned
to that class.

## Usage

``` r
plot_density(
  x,
  variables = NULL,
  bw = FALSE,
  conditional = FALSE,
  alpha = 0.2,
  facet_labels = NULL
)
```

## Arguments

- x:

  Object for which a method exists.

- variables:

  Which variables to plot. If NULL, plots all variables that are present
  in all models.

- bw:

  Logical. Whether to make a black and white plot (for print) or a color
  plot. Defaults to FALSE, because these density plots are hard to read
  in black and white.

- conditional:

  Logical. Whether to show a conditional density plot (surface area is
  divided among the latent classes), or a classic density plot (surface
  area of the total density plot is equal to one, and is divided among
  the classes).

- alpha:

  Numeric (0-1). Only used when bw and conditional are FALSE. Sets the
  transparency of geom_density, so that classes with a small number of
  cases remain visible.

- facet_labels:

  Named character vector, the names of which should correspond to the
  facet labels one wishes to rename, and the values of which provide new
  names for these facets. For example, to rename variables, in the
  example with the 'iris' data below, one could specify:
  `facet_labels = c("Pet_leng" = "Petal length")`.

## Value

An object of class 'ggplot'.

## Author

Caspar J. van Lissa

## Examples

``` r
if (FALSE) { # \dontrun{
dat <-
  iris[, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")]
names(dat) <- paste0("x", 1:4)
res <- mx_profiles(dat, 1:3)
plot_density(res)
} # }
```
