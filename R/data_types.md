# data_dict

data.frame with collumns
* name        variable name as listed in data
* scale       which scale the name belongs to
* item        [TODO] possibly redundant
* label       Which label to use for this variable
* type        observed, indicator (observed that is indicator for latent) or latent
* measurement [TODO] Level of measurement: Categorical, ordinal, binary

[TODO] Attributes: original names of data.frame? Original data.frame name?

# sem_syntax

## $data_dict

A data_dict

## $syntax

A character vector with syntax elements

## $sem_software

Atomic character vector with 'lavaan' or 'mplus'
