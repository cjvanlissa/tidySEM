# tidy_sem

## $dictionary

A data_dict: data.frame with columns
* name        variable name as listed in data
* scale       which scale the name belongs to
* item        [TODO] possibly redundant
* label       Which label to use for this variable
* type        observed, indicator (observed that is indicator for latent) or latent
* measurement [TODO] Level of measurement: Categorical, ordinal, binary
* Wave        [TODO] Measurement wave for wide format


## $data

The observed data

## $syntax

A character vector with syntax elements


# Syntax (based on lavaanify)

c("lhs", "op", "rhs", "mod.idx", "block", "fixed", "start", "lower", "upper", "label", "prior", "efa")
