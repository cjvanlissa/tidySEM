# Create a tidy_sem object

Create an object of class `tidy_sem`, which has the following elements:

- dictionary An overview of the variables in the `tidy_sem` object, and
  their assignment to scale/latent variables.

- data Optionally, the `data.frame` containing the data referenced in
  `$dictionary`.

- syntax Optionally, syntax defining a SEM-model by reference to the
  variables contained in `$data`.

## Usage

``` r
tidy_sem(x, split = "_")
```

## Arguments

- x:

  An object for which a method exists, e.g., a vector of variable names,
  or a data.frame.

- split:

  Character. Defining the regular expression used by
  [`strsplit`](https://rdrr.io/r/base/strsplit.html) to separate
  variable names into 1) the name of the scale/construct and 2) the
  number (or name) of the item.

## Value

An object of class "tidy_sem"

## Details

When `tidy_sem` is called on a `character` string or `data.frame`, it
attempts to assign variables to superordinate scale/latent variables
based on the variable name and the splitting character defined in the
`split` argument. Thus, the function will assign the variable
`"scale_01"` to a scale/latent variable called `"scale"` when
`split = "_"`. Alternatively, if the variable name is `"construct.1"`,
the split character `"\."` separates the `"construct"` name from item
number `"1"`. The character `"."` is escaped with a double backslash,
because it is a special character in regular expressions.

## Author

Caspar J. van Lissa

## Examples

``` r
tidy_sem(c("bfi_1", "bfi_2", "bfi_3", "bfi_4", "bfi_5",
"macqj_1", "macqj_2", "macqj_3", "macqj_4", "macqj_5", "macqj_6",
"macqj_7", "macqj_8", "macqj_9", "macqj_10", "macqj_11",
"macqj_12", "macqj_13", "macqj_14", "macqj_15", "macqj_16",
"macqj_17", "macqj_18", "macqj_19", "macqj_20", "macqj_21",
"macqr_1", "macqr_2", "macqr_3", "macqr_4", "macqr_5", "macqr_6",
"macqr_7", "macqr_8", "macqr_9", "macqr_10", "macqr_11",
"macqr_12", "macqr_13", "macqr_14", "macqr_15", "macqr_16",
"macqr_17", "macqr_18", "macqr_19", "macqr_20", "macqr_21", "sex"))
#> A tidy_sem object
#> v    $dictionary
#> o    $data
#> o    $syntax
suppressMessages(tidy_sem(c("bfi_1", "bfi_2", "bfi_3", "bfi_4", "bfi_5",
"mac_q_j_1", "mac_q_j_2", "mac_q_j_3", "mac_q_j_4", "mac_q_j_5", "mac_q_j_6",
"mac_q_j_7", "mac_q_j_8", "mac_q_j_9", "mac_q_j_10", "mac_q_j_11",
"mac_q_j_12", "mac_q_j_13", "mac_q_j_14", "mac_q_j_15", "mac_q_j_16",
"mac_q_j_17", "mac_q_j_18", "mac_q_j_19", "mac_q_j_20", "mac_q_j_21",
"mac_q_r_1", "mac_q_r_2", "mac_q_r_3", "mac_q_r_4", "mac_q_r_5", "mac_q_r_6",
"mac_q_r_7", "mac_q_r_8", "mac_q_r_9", "mac_q_r_10", "mac_q_r_11",
"mac_q_r_12", "mac_q_r_13", "mac_q_r_14", "mac_q_r_15", "mac_q_r_16",
"mac_q_r_17", "mac_q_r_18", "mac_q_r_19", "mac_q_r_20", "mac_q_r_21")))
#> A tidy_sem object
#> v    $dictionary
#> o    $data
#> o    $syntax
```
