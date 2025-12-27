# Plot growth trajectories

Plots latent and observed trajectories for latent growth models.

## Usage

``` r
plot_growth(
  x,
  items = NULL,
  growth_variables = NULL,
  time_scale = NULL,
  bw = FALSE,
  rawdata = FALSE,
  estimated = TRUE,
  alpha_range = c(0, 0.1),
  jitter_lines = NULL
)
```

## Arguments

- x:

  An oject for which a method exists.

- items:

  Character vector. Indicate the names of the observed variables for the
  growth trajectory to plot. If NULL (default), all observed variables
  are used. Use this option to plot one trajectory when a model contains
  multiple latent growth trajectories.

- growth_variables:

  Character vector. Indicate the names of the latent variables for the
  growth trajectory to plot. If NULL (default), all latent growth
  variables are used. Use this option to plot one trajectory when a
  model contains multiple latent growth trajectories.

- time_scale:

  Numeric vector. In case some of the loadings of the growth model are
  freely estimated, provide the correct time scale here (e.g., c(0, 1,
  2)).

- bw:

  Logical. Should the plot be black and white (for print), or color?

- rawdata:

  Logical. Should raw data (observed trajectories) be plotted in the
  background? Setting this to TRUE might result in long plotting times.
  Requires including the Mplus syntax 'SAVEDATA: FILE IS "filename";
  SAVE = cprobabilities' in the Mplus input.

- estimated:

  Logical. Should the Mplus estimates growth trajectories be displayed?
  Defaults to TRUE.

- alpha_range:

  Numeric vector. The minimum and maximum values of alpha (transparency)
  for the raw data. Minimum should be 0; lower maximum values of alpha
  can help reduce overplotting.

- jitter_lines:

  Numeric. Indicate the amount (expressed in fractions of a standard
  deviation of all observed data) by which the observed trajectories
  should be vertically jittered. Like alpha_range, this parameter helps
  control overplotting.

## Value

An object of class 'ggplot'.

## Author

Caspar J. van Lissa

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
plot_growth(res, rawdata = FALSE)
} # }
```
