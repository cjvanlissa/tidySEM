#' Ocean Microplastics Data
#'
#' These data were collected by Alkema during a cruise from 04/2018 to
#' 06/2018 traversing the Atlantic Ocean from South Africa to Norway.
#' A 500 μm meshed Manta Trawl was towed outside the wake of the ship for 1 h
#' each day. Length, width, height and polymer type of 6.942 particles
#' were measured using infrared spectroscopy and image analysis.
#'
#' \tabular{lll}{
#'   \strong{current} \tab \code{factor} \tab Which ocean current the sample was taken from\cr
#'   \strong{sample} \tab \code{integer} \tab Sample ID\cr
#'   \strong{length} \tab \code{numeric} \tab Particle length in mm\cr
#'   \strong{width} \tab \code{numeric} \tab Particle width in mm\cr
#'   \strong{height_est} \tab \code{numeric} \tab Estimated particle height in mm\cr
#'   \strong{height_obs} \tab \code{numeric} \tab Observed particle height in mm. Height was only measured for large particles\cr
#'   \strong{category} \tab \code{factor} \tab Particle category based on visual inspection\cr
#'   \strong{poly_type} \tab \code{factor} \tab Polymer type as determined by near infrared spectroscopy (NIR)\cr
#'   \strong{two_dim} \tab \code{logical} \tab Whether or not the particle can be treated as two-dimensional\cr
#'   \strong{film} \tab \code{logical} \tab Whether or not the particle appears to be a film\cr
#'   \strong{line} \tab \code{logical} \tab Whether or not the particle appears to be a line
#' }
#' @docType data
#' @keywords datasets
#' @name alkema_microplastics
#' @usage data(alkema_microplastics)
#' @references Alkema, L. M., Van Lissa, C. J., Kooi, M., & Koelmans, A. A. (2022).
#' Maximizing Realism: Mapping Plastic Particles at the Ocean Surface Using
#' Mixtures of Normal Distributions. Environmental Science & Technology,
#' 56(22), 15552-15562.
#' \doi{10.1021/acs.est.2c03559}
#' @format A data frame with 6942 rows and 11 variables.
NULL
