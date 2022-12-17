#' @method plot mixture_list
#' @export
plot.mixture_list <- function(x,
         y,
         ...,
         statistics = NULL) {
  if(is.null(statistics)) statistics <- "BIC"
  fits <- table_fit(x)
  p <- plot_modelstats(fits, statistics, ...)
  suppressMessages(suppressWarnings(print(p)))
  invisible(p)
}


plot_modelstats <- function(x,
         statistics = NULL) {
  plotdat <- x
  plotdat <- plotdat[, !duplicated(names(plotdat))]
  if(is.null(statistics)) statistics <- "BIC"
  lowerbetter <- c(
    "Minus2LogLikelihood" = " (lower is better)",
    "AIC" = " (lower is better)",
    "AWE" = " (lower is better)",
    "BIC" = " (lower is better)",
    "CAIC" = " (lower is better)",
    "CLC" = " (lower is better)",
    "KIC" = " (lower is better)",
    "SABIC" = " (lower is better"
  )
  higherbetter <- c("ICL" = " (higher is better)",
                    "Entropy" = " (higher is better)",
                    "LogLik" = " (higher is better)",
                    "LL" = " (higher is better)")
  neutral <- c("prob_min", "prob_max", "n_min", "n_max")

  if (any(!statistics %in% c(names(lowerbetter), names(higherbetter), names(neutral)))) {
    stop("Can not plot the following statistics: ",
         paste(statistics, collapse = ", "),
         ".")
  }

  plotdat$Name <- ordered(plotdat$Name, levels = plotdat$Name)

  if (length(statistics) > 1) {
    plotdat <-
      reshape(
        plotdat[, c("Name", statistics)],
        varying = list(Value = statistics),
        timevar = "Statistic",
        v.names = "Value",
        direction = "long"
      )
    plotdat$Statistic <-
      factor(plotdat$Statistic, labels = paste0(statistics, c(lowerbetter, higherbetter)[match(statistics, names(c(lowerbetter, higherbetter)))]))
    p <- ggplot(
      plotdat,
      aes(
        x = .data[["Name"]],
        y = .data[["Value"]],
        group = .data[["Statistic"]]
      )
    ) +
      geom_line(na.rm = TRUE) +
      geom_point(na.rm = TRUE) +
      theme_bw() +
      scale_color_discrete("")+
      facet_wrap(~ Statistic, scales = "free")

  } else {
    p <- ggplot(
      plotdat,
      aes(
        x = .data[["Name"]],
        y = .data[[statistics]],
        group = 1
      )
    ) +
      geom_line(na.rm = TRUE) +
      geom_point(na.rm = TRUE) +
      ylab(paste0(statistics, c(lowerbetter, higherbetter)[match(statistics, names(c(lowerbetter, higherbetter)))])) +
      theme_bw() +
      scale_color_discrete("")
  }
  return(p)
}
