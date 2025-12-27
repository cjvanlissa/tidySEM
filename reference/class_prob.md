# Obtain latent class probabilities

Obtain latent class probabilities for an object for which a method
exists. See Details.

## Usage

``` r
class_prob(
  x,
  type = c("sum.posterior", "sum.mostlikely", "mostlikely.class", "avg.mostlikely",
    "individual"),
  ...
)
```

## Arguments

- x:

  An object for which a method exists.

- type:

  Character vector, indicating which types of probabilities to extract.
  See Details.

- ...:

  Further arguments to be passed to or from other methods.

## Value

A data.frame.

## Details

The following types are available:

- "sum.posterior":

  A summary table of the posterior class probabilities; this indicates
  what proportion of your data contributes to each class.

- "sum.mostlikely":

  A summary table of the most likely class membership, based on the
  highest posterior class probability. Note that this is subject to
  measurement error.

- "mostlikely.class":

  If C is the true class of an observation, and N is the most likely
  class based on the model, then this table shows the probability
  P(N==i\|C==j). The diagonal represents the probability that
  observations in each class will be correctly classified.

- "avg.mostlikely":

  Average posterior probabilities for each class, for the subset of
  observations with most likely class of 1:k, where k is the number of
  classes.

- "individual":

  The posterior probability matrix, with dimensions n (number of cases
  in the data) x k (number of classes).

## Examples

``` r
if (FALSE) { # \dontrun{
df <- iris[, 1, drop = FALSE]
names(df) <- "x"
res <- mx_mixture(model = "x ~ m{C}*1
                           x ~~ v{C}*x", classes = 1, data = df)
class_prob(res)
} # }
```
