# Caregiver Compass Data

These simulated data are based on a study by Dijenborgh, Swildens, and
Zegwaard on different types of caregivers among those providing informal
care to outpatients receiving mental healthcare.

## Usage

``` r
data(zegwaard_carecompass)
```

## Format

A data frame with 513 rows and 10 variables.

## Details

|                  |           |                                                                                                                                                                                                |
|------------------|-----------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| **burdened**     | `numeric` | How strongly is the caregiver's life affected by their responsibilities? Scale score, based on 15 items with Likert-type response options. Example: "I never feel free of responsibilities"    |
| **trapped**      | `numeric` | Caregiver's cognitions regarding freedom of choice. Scale score, based on 3 items with Likert-type response options. Example: "I feel trapped by the affliction of my charge"                  |
| **negaffect**    | `numeric` | Different types of negative emotions experienced by the caregiver. Scale score, based on 9 items with Likert-type response options. Example: "I feel angry in the relationship with my charge" |
| **loneliness**   | `numeric` | Caregiver's perceived loneliness. Scale score, based on 11 items with Likert-type response options. Example: "I miss having people around"                                                     |
| **sex**          | `factor`  | Caregiver sex                                                                                                                                                                                  |
| **sexpatient**   | `factor`  | Sex of the patient                                                                                                                                                                             |
| **cohabiting**   | `factor`  | Whether or not the caregiver cohabits with the patient                                                                                                                                         |
| **distance**     | `numeric` | Travel time in minutes for the caregiver to reach the patient                                                                                                                                  |
| **freqvisit**    | `ordered` | Ordinal variable, indicating frequency of visits                                                                                                                                               |
| **relationship** | `factor`  | Type of relationship of patient with caregiver                                                                                                                                                 |
