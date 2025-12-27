# Ocean Microplastics Data

These data were collected by Alkema during a cruise from 04/2018 to
06/2018 traversing the Atlantic Ocean from South Africa to Norway. A 500
micrometer meshed Manta Trawl was towed outside the wake of the ship for
1 h each day. Length, width, height and polymer type of 6.942 particles
were measured using infrared spectroscopy and image analysis.

## Usage

``` r
data(alkema_microplastics)
```

## Format

A data frame with 6942 rows and 11 variables.

## Details

|                |           |                                                                              |
|----------------|-----------|------------------------------------------------------------------------------|
| **current**    | `factor`  | Which ocean current the sample was taken from                                |
| **sample**     | `integer` | Sample ID                                                                    |
| **length**     | `numeric` | Particle length in mm                                                        |
| **width**      | `numeric` | Particle width in mm                                                         |
| **height_est** | `numeric` | Estimated particle height in mm                                              |
| **height_obs** | `numeric` | Observed particle height in mm. Height was only measured for large particles |
| **category**   | `factor`  | Particle category based on visual inspection                                 |
| **poly_type**  | `factor`  | Polymer type as determined by near infrared spectroscopy (NIR)               |
| **two_dim**    | `logical` | Whether or not the particle can be treated as two-dimensional                |
| **film**       | `logical` | Whether or not the particle appears to be a film                             |
| **line**       | `logical` | Whether or not the particle appears to be a line                             |

## References

Alkema, L. M., Van Lissa, C. J., Kooi, M., & Koelmans, A. A. (2022).
Maximizing Realism: Mapping Plastic Particles at the Ocean Surface Using
Mixtures of Normal Distributions. Environmental Science & Technology,
56(22), 15552-15562.
[doi:10.1021/acs.est.2c03559](https://doi.org/10.1021/acs.est.2c03559)
