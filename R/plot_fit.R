#' @method plot tidy_fit
#' @export
plot.tidy_fit <- function(x, y = NULL, statistics = "BIC", xaxis = "Name", ...) {
    statlow <- tolower(statistics)
    if(!xaxis %in% names(x)){
      thename <- as.character(1:nrow(x))
    } else {
      thename <- x[[xaxis]]
    }
    namelow <- tolower(names(x))
    names(x) <- namelow
    if(!all(statlow %in% namelow)){
        stop("Can not plot_fit the following statistics: ",
             paste(statistics[!(statlow %in% namelow)], collapse = ", "),
             ".")
    }
    plot_fitdat <- data.frame(Model = thename,
                              x[, namelow %in% statlow, drop = FALSE])
    lowerbetter <- c(
        "LogLik" = " (lower is better)",
        "AIC" = " (lower is better)",
        "AWE" = " (lower is better)",
        "BIC" = " (lower is better)",
        "CAIC" = " (lower is better)",
        "CLC" = " (lower is better)",
        "KIC" = " (lower is better)",
        "SABIC" = " (lower is better"
    )
    higherbetter <- c("ICL" = " (higher is better)",
                      "Entropy" = " (higher is better")
    neutral <- c("prob_min", "prob_max", "n_min", "n_max")
    thelabels <- statistics
    hasint <- statlow %in% tolower(c(names(lowerbetter), names(higherbetter), names(neutral)))
    if (any(hasint)) {
        thelabels[hasint] <- paste0(statistics[hasint], c(lowerbetter, higherbetter)[match(statlow[hasint], tolower(names(c(lowerbetter, higherbetter))))])
    }
    if (length(statistics) > 1) {
        plot_fitdat <- do.call(rbind, lapply(1:length(statlow), function(i){
            tmp <- plot_fitdat[, c("Model", names(plot_fitdat)[match(statlow[i], names(plot_fitdat))])]
            tmp$Statistic <- names(tmp)[2]
            names(tmp)[2] <- "Value"
            tmp
        }))
        plot_fitdat$Statistic <- factor(plot_fitdat$Statistic)
        levels(plot_fitdat$Statistic) <- thelabels[match(statlow, levels(plot_fitdat$Statistic))]
        ggplot(
            plot_fitdat,
            aes(
                x = Model,
                y = .data[["Value"]],
                color = .data[["Statistic"]],
                group = .data[["Statistic"]]
            )
        ) +
            geom_line(na.rm = TRUE) +
            geom_point(na.rm = TRUE) +
            theme_bw()

    } else {
        ggplot(
            plot_fitdat,
            aes(
                x = Model,
                y = .data[[statlow]],
                group = 1
            )
        ) +
            geom_line(na.rm = TRUE) +
            geom_point(na.rm = TRUE) +
            ylab(thelabels) +
            theme_bw()
    }
}
