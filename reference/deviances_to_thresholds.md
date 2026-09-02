# Convert deviances to thresholds

Converts an OpenMx model in which ordinal thresholds are parameterized
as deviances between successive thresholds to an equivalent model in
which the parameters are thresholds.

## Usage

``` r
deviances_to_thresholds(model)
```

## Arguments

- model:

  An [`MxModel`](https://rdrr.io/pkg/OpenMx/man/MxModel-class.html)
  containing ordinal indicators whose thresholds are specified using
  deviance parameterization, as created by
  [`as_ram`](https://cjvanlissa.github.io/tidySEM/reference/as_ram.md)
  with `threshold_method = "mx_deviances"`.

## Value

An [`MxModel`](https://rdrr.io/pkg/OpenMx/man/MxModel-class.html).

## See also

[`as_ram`](https://cjvanlissa.github.io/tidySEM/reference/as_ram.md)

## Examples

``` r
# Plain model
if(isTRUE(requireNamespace("OpenMx", quietly = TRUE))){
library(OpenMx)

set.seed(1)
dat <- data.frame(
  X1 = ordered(sample(1:2, 200, replace = TRUE)),
  X2 = ordered(sample(1:2, 200, replace = TRUE)),
  X3 = ordered(sample(1:3, 200, replace = TRUE))
)

mod <- as_ram(
  "
  X1 | t1
  X2 | t1
  X3 | t1
  X3 | t2
  ",
  data = dat,
  threshold_method = "mx_deviances"
)
res <- run_mx(mod)
coef(res)
res_thresholds <- deviances_to_thresholds(res)
coef(res_thresholds)

# Multigroup model
set.seed(2)
dat1 <- data.frame(
  X1 = ordered(sample(1:2, 100, replace = TRUE)),
  X2 = ordered(sample(1:2, 100, replace = TRUE)),
  X3 = ordered(sample(1:3, 100, replace = TRUE))
)

dat2 <- data.frame(
  X1 = ordered(sample(1:2, 100, replace = TRUE)),
  X2 = ordered(sample(1:2, 100, replace = TRUE)),
  X3 = ordered(sample(1:3, 100, replace = TRUE))
)

group1 <- as_ram(
  "
  X1 | t1
  X2 | t1
  X3 | t1
  X3 | t2
  ",
  data = dat1,
  threshold_method = "mx_deviances"
)
group1 <- mxModel(group1, name = "group1")

group2 <- as_ram(
  "
  X1 | t1
  X2 | t1
  X3 | t1
  X3 | t2
  ",
  data = dat2,
  threshold_method = "mx_deviances"
)
group2 <- mxModel(group2, name = "group2")

multigroup <- mxModel(
  "multigroup",
  group1,
  group2,
  mxFitFunctionMultigroup(c("group1", "group2"))
)
res2 <- run_mx(multigroup)
coef(res2)
res2_thresholds <- deviances_to_thresholds(res2)
coef(res2_thresholds)
}
#> group1.Thresholds[1,1] group1.Thresholds[1,2] group1.Thresholds[1,3] 
#>            -0.02506889             0.20189348            -0.64334539 
#> group1.Thresholds[2,3] group2.Thresholds[1,1] group2.Thresholds[1,2] 
#>             0.55338476             0.17637416            -0.02506890 
#> group2.Thresholds[1,3] group2.Thresholds[2,3] 
#>            -0.46769879             0.49585034 
```
