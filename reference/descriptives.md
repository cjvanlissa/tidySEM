# Describe a dataset

Provide descriptive statistics for a dataset.

## Usage

``` r
descriptives(x, ...)
```

## Arguments

- x:

  An object for which a method exists.

- ...:

  Additional arguments.

## Value

A `data.frame` with descriptive statistics for `x`. Its elements are:

|                |             |                                                                                                                                                                                                                                  |
|----------------|-------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| **name**       | `Character` | Variable name                                                                                                                                                                                                                    |
| **type**       | `character` | Data type in `R`, as obtained by `class(x)[1]`                                                                                                                                                                                   |
| **n**          | `Integer`   | Number of valid observations                                                                                                                                                                                                     |
| **missing**    | `Numeric`   | Proportion missing                                                                                                                                                                                                               |
| **unique**     | `Integer`   | Number of unique values                                                                                                                                                                                                          |
| **mean**       | `numeric`   | Mean value of non-missing entries, only defined for variables that can be coerced to numeric                                                                                                                                     |
| **median**     | `numeric`   | Median value of non-missing entries, only defined for numeric variables                                                                                                                                                          |
| **mode**       | `Integer`   | For numeric variables: The mode value. For factors: The frequency of the mode value                                                                                                                                              |
| **mode_value** | `Character` | For factors: value of the mode                                                                                                                                                                                                   |
| **sd**         | `numeric`   | Standard deviation of non-missing entries, only defined for variables that can be coerced to numeric                                                                                                                             |
| **v**          | `numeric`   | Variability coefficient V for factor variables (Agresti, 1990). V is the probability that two independent observations fall in different categories                                                                              |
| **min**        | `numeric`   | Minimum value for numeric variables                                                                                                                                                                                              |
| **max**        | `numeric`   | Maximum value for numeric variables                                                                                                                                                                                              |
| **range**      | `numeric`   | Range (distance between min and max) for numeric variables                                                                                                                                                                       |
| **skew**       | `numeric`   | Skewness. The normalized third central moment of a numeric variable, which reflects its skewness. A symmetric distribution has a skewness of zero                                                                                |
| **skew_2se**   | `numeric`   | Skewness, divided by two times its standard error. Values greater than one can be considered "significant" according to a Z-test with significance level of .05                                                                  |
| **kurt**       | `numeric`   | Kurtosis. The normalized fourth central moment of a numeric variable, which reflects its peakedness. A heavy-tailed distribution has high kurtosis, a light-tailed distribution has low kurtosis (sometimes called platykurtic). |
| **kurt_2se**   | `numeric`   | Kurtosis, divided by two times its standard error. Values greater than one can be considered "significant" according to a Z-test with significance level of .05                                                                  |

## References

Agresti, A. (2012). Categorical data analysis (Vol. 792). John Wiley &
Sons.

## Examples

``` r
descriptives(iris)
#>           name    type   n missing unique     mean median  mode mode_value
#> 1 Sepal.Length numeric 150       0     35 5.843333   5.80  5.80       <NA>
#> 2  Sepal.Width numeric 150       0     23 3.057333   3.00  3.00       <NA>
#> 3 Petal.Length numeric 150       0     43 3.758000   4.35  4.35       <NA>
#> 4  Petal.Width numeric 150       0     22 1.199333   1.30  1.30       <NA>
#> 5      Species  factor 150       0      4       NA     NA 50.00     setosa
#>          sd         v min max range       skew   skew_2se     kurt kurt_2se
#> 1 0.8280661        NA 4.3 7.9   3.6  0.3117531  0.7871027 2.426432 3.082490
#> 2 0.4358663        NA 2.0 4.4   2.4  0.3157671  0.7972372 3.180976 4.041048
#> 3 1.7652982        NA 1.0 6.9   5.9 -0.2721277 -0.6870579 1.604464 2.038279
#> 4 0.7622377        NA 0.1 2.5   2.4 -0.1019342 -0.2573597 1.663933 2.113826
#> 5        NA 0.6666667  NA  NA    NA         NA         NA       NA       NA
```
