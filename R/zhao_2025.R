#' Academic Stress, Motivation, and Exercise Behavior
#'
#' This synthetic dataset contains measures of academic stress, motivation,
#' exercise intention, and exercise behavior. It was generated based on the real
#' data by Zhao and colleagues (2025), who conducted research among 290
#' middle-school students.
#'
#' @format A tibble with 291 observations and 31 variables:
#' \describe{
#'   \item{ID}{Participant identifier.}
#'   \item{Grade}{School grade, coded 1 = Grade 7,
#'     2 = Grade 8, and 3 = Grade 9.}
#'   \item{Age}{Age in years.}
#'   \item{Gender}{Gender, coded 1 = male and 2 = female.}
#'   \item{TS1-TS4}{Four items measuring teacher pressure
#'     (teacher-related academic stress).}
#'   \item{SOS1-SOS4}{Four items measuring social pressure
#'     or social stress.}
#'   \item{PS1-PS5}{Five items measuring parental pressure
#'     or parental stress.}
#'   \item{AM1-AM5}{Five items measuring autonomous motivation.}
#'   \item{CM1-CM4}{Four items measuring controlled motivation.}
#'   \item{BI1-BI3}{Three items measuring behavioral intention
#'     to engage in physical exercise.}
#'   \item{BH1-BH2}{Two items measuring physical exercise behavior.}
#' }
#'
#' @details
#' The dataset contains measures related to Self-Determination
#' Theory (SDT) in the context of academic stress and physical
#' exercise among middle-school students.
#'
#' The stress measures include teacher pressure (TS), social
#' pressure (SOS), and parental pressure (PS). Motivation is
#' represented by autonomous motivation (AM) and controlled
#' motivation (CM). Exercise-related variables include behavioral
#' intention (BI) and exercise behavior (BH).
#' @source
#' Zhao, K., Zhao, Y., & Xu, W. (2025).
#' Relationship between middle school students' academic stress
#' and physical exercise behavior from the perspective of
#' Self-Determination Theory: The chained mediation of
#' motivation and intention.
#' *PLOS ONE, 20*(1), e0316599.
#' \doi{10.1371/journal.pone.0316599}
#'
#' @examples
#' data(zhao_2025)
#' @name zhao_2025
NULL
