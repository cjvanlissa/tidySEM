# National Identity, Discrimination and Depression

These synthetic data are based on a study by Maene and colleagues, which
conducted an LCA with ordinal indicators on National, Regional, and
Heritage Identities in Flemish (Belgian) high-school students with a
migration background, and examined between class differences in
perceived discrimination by teachers and depressive symptoms.

## Usage

``` r
data(maene_identity)
```

## Format

A data frame with 439 rows and 13 variables.

## Details

|                  |           |                                                                                                                                                                                     |
|------------------|-----------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| **Ethnic_1**     | `ordered` | when I introduce myself, I would definitely say I belong to this group, answered on a 5-point Likert scale                                                                          |
| **Ethnic_2**     | `ordered` | I have a strong sense of belonging to this group, answered on a 5-point Likert scale                                                                                                |
| **Ethnic_3**     | `ordered` | I see myself as a member of this group, answered on a 5-point Likert scale                                                                                                          |
| **Belgian**      | `ordered` | Do you feel a member of the Belgian group, answered on a 10-point Likert scale                                                                                                      |
| **Flemish**      | `ordered` | Do you feel a member of the Flemish group, answered on a 10-point Likert scale                                                                                                      |
| **age**          | `numeric` | Participant age                                                                                                                                                                     |
| **sex**          | `factor`  | Participant sex                                                                                                                                                                     |
| **ses**          | `numeric` | Socio-economic status, measured using the International Socio-Economic Index of Occupational Status (ISEI)                                                                          |
| **belgianborn**  | `factor`  | Whether or not the participant was born in Belgium                                                                                                                                  |
| **age_belgium**  | `numeric` | Age at which the participant migrated to Belgium                                                                                                                                    |
| **vict_bully**   | `factor`  | Whether or not the participant has ever been the victim of peer bullying for any reason                                                                                             |
| **vict_teacher** | `factor`  | Whether or not the participant has ever been insulted, threatened, pushed, treated unfairly or excluded by teachers because of their foreign descent, language use, and skin colour |
| **depression**   | `numeric` | Scale scores of self-reported depressive feelings, assessed using the a ten-item scale with 5-point Likert-type response options                                                    |

## References

Maene, C., D'hondt, F., Van Lissa, C. J., Thijs, J., & Stevens, P. A.
(2022). Perceived teacher discrimination and depressive feelings in
adolescents: the role of national, regional, and heritage identities in
Flemish schools. Journal of youth and adolescence, 51(12), 2281-2293.
[doi:10.1007/s10964-022-01665-7](https://doi.org/10.1007/s10964-022-01665-7)
