#' Plot growth trajectories
#'
#' Plots latent and observed trajectories for
#' latent growth models.
#' @param x An oject for which a method exists.
#' @param items Character vector. Indicate the
#' names of the observed variables for the growth
#' trajectory to plot. If NULL (default),
#' all observed variables are used.
#' Use this option to plot one trajectory when a
#' model contains multiple latent growth
#' trajectories.
#' @param growth_variables Character vector.
#' Indicate the names of the latent
#' variables for the growth trajectory to plot.
#' If NULL (default), all latent
#' growth variables are used.
#' Use this option to plot one trajectory when a
#' model contains multiple latent growth
#' trajectories.
#' @param time_scale Numeric vector. In case some of the loadings of the growth
#' model are freely
#' estimated, provide the correct time scale here (e.g., c(0, 1, 2)).
#' @param bw Logical. Should the plot be black and white (for print), or color?
#' @param rawdata Logical. Should raw data (observed trajectories) be plotted in the
#' background? Setting this to TRUE might result in long plotting times.
#' Requires including the Mplus syntax 'SAVEDATA: FILE IS "filename"; SAVE =
#' cprobabilities' in the Mplus input.
#' @param estimated Logical. Should the Mplus estimates growth trajectories be
#' displayed? Defaults to TRUE.
#' @param alpha_range Numeric vector. The minimum and maximum values of alpha
#' (transparency) for the raw data. Minimum should be 0; lower maximum values of
#' alpha can help reduce overplotting.
#' @param jitter_lines Numeric. Indicate the amount (expressed in fractions of a
#' standard deviation of all observed data) by which the observed trajectories
#' should be vertically jittered. Like alpha_range, this parameter helps control
#' overplotting.
#' @return An object of class 'ggplot'.
#' @author Caspar J. van Lissa
#' @export
#' @import ggplot2
#' @keywords internal
#' @examples
#' \dontrun{
#' data("empathy")
#' df <- empathy[1:6]
#' mx_growth_mixture(model = "i =~ 1*ec1 + 1*ec2 + 1*ec3 +1*ec4 +1*ec5 +1*ec6
#'                            s =~ 0*ec1 + 1*ec2 + 2*ec3 +3*ec4 +4*ec5 +5*ec6
#'                            ec1 ~~ vec1*ec1
#'                            ec2 ~~ vec2*ec2
#'                            ec3 ~~ vec3*ec3
#'                            ec4 ~~ vec4*ec4
#'                            ec5 ~~ vec5*ec5
#'                            ec6 ~~ vec6*ec6
#'                            i ~~ 0*i
#'                            s ~~ 0*s
#'                            i ~~ 0*s",
#'                   classes = 2,
#'                   data = df) -> res
#' plot_growth(res, rawdata = FALSE)
#' }
plot_growth <- function(x,
                        items = NULL,
                        growth_variables = NULL,
                        time_scale = NULL,
                        bw = FALSE,
                        rawdata = FALSE,
                        estimated = TRUE,
                        alpha_range = c(0, 0.1),
                        jitter_lines = NULL)
{
  submods <- names(x@submodels)
  if (is.null(growth_variables)) {
    growth_variables <- x[[submods[1]]]$latentVars
  }
  if (is.null(items)) {
    items <- x[[submods[1]]]$manifestVars
  }
  loadings <- lapply(submods, function(thismod) {
    x[[thismod]]$A$values[items, growth_variables]
  })

  estimates <- lapply(submods, function(thismod) {
    matrix(
      x[[thismod]]$M$values[1, growth_variables],
      nrow = nrow(loadings[[1]]),
      ncol = length(growth_variables),
      byrow = TRUE
    )
  })

  if (is.null(time_scale)) {
    time_scale <- seq_along(items)
  }

  predicted_trajectories <- do.call(rbind,
                                    lapply(1:length(submods), function(x) {
                                      data.frame(
                                        Time = time_scale,
                                        Value = rowSums(loadings[[x]] * estimates[[x]]),
                                        Class = submods[x]
                                      )
                                    }))
  predicted_trajectories$Class <-
    ordered(predicted_trajectories$Class, levels = submods)
  line_plot <- ggplot(NULL)

  if (rawdata) {
    rawdata <- x$data$observed[, items]
    cprobs <-
      class_prob(x, type = "individual")[["individual"]][, submods, drop = FALSE]
    rawdata <- cbind(rawdata, cprobs)
    names(rawdata)[match(submods, names(rawdata))] <-
      paste0("Probability.", seq_along(submods))
    rawdata <-
      reshape(
        rawdata,
        direction = "long",
        varying = paste0("Probability.", seq_along(submods)),
        timevar = "Class",
        idvar = "ID"
      )
    names(rawdata)[match(items, names(rawdata))] <-
      paste0("Value.", items)
    rawdata <- reshape(
      rawdata,
      direction = "long",
      varying = paste0("Value.", items),
      timevar = "Time"
    )[, c("ID", "Time", "Value", "Class", "Probability")]
    rawdata$Time <- ordered(rawdata$Time, levels = items)
    rawdata$Time <- time_scale[as.numeric(rawdata$Time)]
    rawdata$Class <- ordered(rawdata$Class, labels = submods)
    rawdata$ID <- paste(rawdata$Title, rawdata$Class,
                        rawdata$ID, sep = "")
    if (!is.null(jitter_lines)) {
      rawdata$Value <- rawdata$Value + stats::rnorm(nrow(rawdata),
                                                    sd = (jitter_lines * stats::sd(rawdata$Value,
                                                                                   na.rm = TRUE)))
    }
    if (bw) {
      line_plot <- line_plot + geom_path(data = rawdata,
                                         aes(
                                           x = .data[["Time"]],
                                           y = .data[["Value"]],
                                           group = .data[["ID"]],
                                           linetype = .data[["Class"]],
                                           alpha = .data[["Probability"]]
                                         )) +
        scale_alpha_continuous(range = alpha_range,
                               guide = "none")
    }
    else {
      line_plot <- line_plot + geom_path(
        data = rawdata,
        aes(
          x = .data[["Time"]],
          y = .data[["Value"]],
          group = .data[["ID"]],
          linetype = .data[["Class"]],
          colour = .data[["Class"]],
          alpha = .data[["Probability"]]
        )
      ) +
        scale_alpha_continuous(range = alpha_range, guide = "none")
    }
  }
  if (estimated) {
    if (bw) {
      line_plot <- line_plot + geom_point(
        data = predicted_trajectories,
        aes(
          x = .data[["Time"]],
          y = .data[["Value"]],
          group = .data[["Class"]],
          shape = .data[["Class"]]
        ),
        size = 2
      ) + geom_line(
        data = predicted_trajectories,
        aes(
          x = .data[["Time"]],
          y = .data[["Value"]],
          group = .data[["Class"]],
          linetype = .data[["Class"]]
        ),
        linewidth = 1
      )
    }
    else {
      line_plot <- line_plot + geom_point(
        data = predicted_trajectories,
        aes(
          x = .data[["Time"]],
          y = .data[["Value"]],
          group = .data[["Class"]],
          shape = .data[["Class"]],
          colour = .data[["Class"]]
        ),
        size = 2
      ) +
        geom_line(
          data = predicted_trajectories,
          aes(
            x = .data[["Time"]],
            y = .data[["Value"]],
            group = .data[["Class"]],
            linetype = .data[["Class"]],
            colour = .data[["Class"]]
          ),
          linewidth = 1
        )
    }
  }
  line_plot <-
    line_plot + theme_bw() + scale_x_continuous(expand = c(0,
                                                           0),
                                                breaks = time_scale,
                                                labels = time_scale) + scale_y_continuous(expand = c(0,
                                                                                                     0))
  return(line_plot)
}
