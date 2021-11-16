#' Create latent profile plots
#'
#' Creates a profile plot (ribbon plot) according to best practices, focusing on
#' the visualization of classification uncertainty by showing:
#' \enumerate{
#' \item Bars reflecting a confidence interval for the class centroids
#' \item Boxes reflecting the standard deviations within each class; a box
#' encompasses +/- 64\% of the observations in a normal distribution
#' \item Raw data, whose transparency is weighted by the posterior class
#' probability, such that each observation is most clearly visible for the class
#' it is most likely to be a member of.
#' }
#' @param x An object containing the results of a mixture model analysis.
#' @param variables A character vectors with the names of the variables to be
#' plotted (optional).
#' @param ci Numeric. What confidence interval should the error bars span?
#' Defaults to a 95\% confidence interval. Set to NULL to remove erro rbars.
#' @param sd Logical. Whether to display a box encompassing +/- 1SD Defaults to
#' TRUE.
#' @param add_line Logical. Whether to display a line, connecting cluster
#' centroids belonging to the same latent class. Defaults to FALSE, as it is not
#' recommended to imply connectivity between the different variables on the
#' X-axis.
#' @param rawdata Should raw data be plotted in the background? Setting this to
#' TRUE might result in long plotting times.
#' @param bw Logical. Should the plot be black and white (for print), or color?
#' @param alpha_range The minimum and maximum values of alpha (transparency) for
#' the raw data. Minimum should be 0; lower maximum values of alpha can help
#' reduce overplotting.
#' @param ... Arguments passed to and from other functions.
#' @return An object of class 'ggplot'.
#' @author Caspar J. van Lissa
#' @keywords plot mixture
#' @examples
#' dat <- data.frame(x1 = iris$Petal.Length, x2 = iris$Sepal.Width)
#' dat <- dat[c(1:10, 140:150), ]
#' mixmod <- mx_profiles(dat,
#'                       classes = 1)
#'
#' plot_profiles(mixmod)
#' @rdname plot_profiles
#' @export
plot_profiles <- function(x, variables = NULL, ci = .95, sd = TRUE, add_line = FALSE, rawdata = TRUE, bw = FALSE, alpha_range = c(0, .1), ...){
    UseMethod("plot_profiles", x)
}

#' @rdname plot_profiles
#' @import ggplot2
#' @export
plot_profiles.default <- function(x, variables = NULL, ci = .95, sd = TRUE, add_line = FALSE, rawdata = TRUE, bw = FALSE, alpha_range = c(0, .1), ...){
    df_plot <- droplevels(x[["df_plot"]])
    if(rawdata){
        df_raw <- droplevels(x[["df_raw"]])
        # Check consistency of factor levels
        if(!all(unique(df_plot$Variable) %in% unique(df_raw$Variable))){
            stop("Could not match raw data to model estimates.")
        }
        df_raw$Variable <- as.numeric(df_raw$Variable)
    }

    level_labels <- levels(df_plot$Variable)
    df_plot$Variable <- as.numeric(df_plot$Variable)

    # Basic plot
    if (bw) {
        classplot <-
            ggplot(NULL,
                   aes_string(
                       x = "Variable",
                       y = "Value",
                       group = "Class",
                       linetype = "Class",
                       shape = "Class"
                   ))
    } else {
        classplot <-
            ggplot(
                NULL,
                aes_string(
                    x = "Variable",
                    y = "Value",
                    group = "Class",
                    linetype = "Class",
                    shape = "Class",
                    colour = "Class"
                )
            ) + scale_colour_manual(values = get_palette(max(df_plot$Classes)))
    }

    if (rawdata) {
        classplot <- classplot +
            geom_jitter(
                data = df_raw,
                width = .2,
                aes_string(
                    x = "Variable",
                    y = "Value",
                    shape = "Class",
                    alpha = "Probability"
                )
            ) +
            scale_alpha_continuous(range = alpha_range, guide = "none")
    }
    classplot <- classplot + geom_point(data = df_plot) +
        scale_x_continuous(breaks = 1:length(level_labels),
                           labels = level_labels) +
        theme_bw() +
        theme(panel.grid.minor.x = element_blank())

    if(add_line) classplot <- classplot + geom_line(data = df_plot)

    # Add errorbars
    if (!is.null(ci)) {
        ci <- qnorm(.5 * (1 - ci))
        df_plot$error_min <- df_plot$Value + ci*df_plot$se
        df_plot$error_max <- df_plot$Value - ci*df_plot$se

        classplot <-
            classplot + geom_errorbar(data = df_plot,
                                      aes_string(ymin = "error_min",
                                                 ymax = "error_max"),
                                      width = .4)
    }
    if(sd){
        df_plot$sd_xmin <- df_plot$Variable-.2
        df_plot$sd_xmax <- df_plot$Variable+.2
        df_plot$sd_ymin <- df_plot$Value - sqrt(df_plot$Value.Variances)
        df_plot$sd_ymax <- df_plot$Value + sqrt(df_plot$Value.Variances)

        if(bw){
            classplot <-
                classplot + geom_rect(
                    data = df_plot,
                    aes_string(
                        xmin = "sd_xmin",
                        xmax = "sd_xmax",
                        ymin = "sd_ymin",
                        ymax = "sd_ymax",
                        linetype = "Class"
                    ),
                    colour = "black",
                    fill=ggplot2::alpha("grey", 0),
                    inherit.aes=FALSE
                )
        } else {
            classplot <-
                classplot + geom_rect(
                    data = df_plot,
                    aes_string(
                        xmin = "sd_xmin",
                        xmax = "sd_xmax",
                        ymin = "sd_ymin",
                        ymax = "sd_ymax",
                        colour = "Class"
                    ),
                    fill=ggplot2::alpha("grey", 0),
                    inherit.aes=FALSE
                )
        }

    }

    if(length(unique(df_plot$Model)) > 1){
        classplot <- classplot + facet_wrap(~ Model, labeller = label_both)
    }
    suppressWarnings(print(classplot))
    return(invisible(classplot))
}

#' @method plot_profiles mixture_list
#' @export
plot_profiles.mixture_list <- function(x, variables = NULL, ci = .95, sd = TRUE, add_line = FALSE, rawdata = TRUE, bw = FALSE, alpha_range = c(0, .1), ...){
    Args <- as.list(match.call()[-1])
    names(x) <- make.unique(names(x))
    df_plot <- bind_list(lapply(names(x), function(n){
        out <- cbind(table_results(x[[n]], columns = NULL), Model = n)
        out$Classes <- length(unique(na.omit(out$class)))
        out
    }))

    names(df_plot)[match(c("est", "lhs"), names(df_plot))] <- c("Value", "Variable")
    if(is.null(df_plot[["class"]])){
        df_plot$Class <- "class1"
        if(all(df_plot$Classes == 0)) df_plot$Classes <- 1
    } else {
        df_plot$Class <- ordered(df_plot$class)
    }
    # if(!"Classes" %in% names(df_plot)){
    #     df_plot$Classes <- length(unique(df_plot$class))
    # }
    # Drop useless rows
    df_plot <- df_plot[!is.na(df_plot$Category), ]
    #table(df_plot$lhs, df_plot$op)
    df_plot$Variable <- ordered(df_plot$Variable, levels = unique(df_plot$Variable))
    # Select only requested variables, or else, all variables
    if(is.null(variables)) {
        variables <- unique(as.vector(sapply(unique(df_plot$Classes), function(c){
            tab <- table(df_plot$Variable[df_plot$Classes == c], df_plot$op[df_plot$Classes == c])
            rownames(tab)[all(apply(tab, 1, function(r){r == c}))]
        })))
    }
    df_plot <- df_plot[tolower(df_plot$Variable) %in% tolower(variables), ]
    df_plot$Variable <- droplevels(df_plot$Variable)
    variables <- levels(df_plot$Variable)
    df_plot$idvar <- paste0(df_plot$Model, df_plot$Classes, df_plot$Class, df_plot$Variable)
    # Drop useless columns
    df_plot <- df_plot[ , c("Variable", "Value", "se", "Class", "Classes", "Category", "Model", "idvar")]
    df_plot <- reshape(df_plot, idvar = "idvar", timevar = "Category", v.names = c("Value", "se"), direction = "wide")

    df_plot[["idvar"]] <- NULL
    # Get some classy names
    names(df_plot) <- gsub("\\.Means", "", names(df_plot))

    if (rawdata) {
        df_raw <- .get_long_data(x)
        df_raw <- df_raw[, c("Model", variables, "Class", "Class_prob", "Probability", "id")]
        df_raw$Class <- ordered(df_raw$Class_prob, labels =
                                    ifelse(is.null(levels(df_plot$Class)),
                                           unique(df_plot$Class),
                                           levels(df_plot$Class)))
        variable_names <- paste("Value", names(df_raw)[match(variables, names(df_raw))], sep = "...")
        names(df_raw)[match(variables, names(df_raw))] <- variable_names
        df_raw <- reshape(
            df_raw,
            varying = c(Variable = variable_names),
            idvar = "new_id",
            direction = "long",
            timevar = "Variable",
            sep = "..."
        )
        if(any(c("Class_prob", "id", "new_id") %in% names(df_raw))){
            df_raw <- df_raw[, -which(names(df_raw) %in% c("Class_prob", "id", "new_id"))]
        }

        df_raw$Variable <- ordered(df_raw$Variable,
                                   levels = levels(df_plot$Variable))
        #names(df_raw)[c(1,2)] <- c("Model", "Classes")
    } else {
        df_raw <- NULL
    }

    Args[["x"]] <- list(df_plot = df_plot, df_raw = df_raw)

    do.call(plot_profiles, Args)
}

#' @method plot_profiles MxModel
#' @export
plot_profiles.MxModel <- function(x, variables = NULL, ci = .95, sd = TRUE, add_line = FALSE, rawdata = TRUE, bw = FALSE, alpha_range = c(0, .1), ...){
   cl <- match.call()
   cl[[1L]] <- str2lang("tidySEM:::plot_profiles")
   x <- list(model = x)
   names(x) <- x$model@name
   class(x) <- c("mixture_list", class(x))
   cl[["x"]] <- x
   eval.parent(cl)
}

.get_long_data <- function (x, ...)
{
    if (inherits(x, "MxModel")) {
        x <- list(x)
    }
    out <- lapply(1:length(x), function(i) {
        thismod <- x[[i]]
        thename <- names(x)[i]
        if(is.null(thename)) thename <- thismod$name
        if(is.null(thename)) thename <- paste0("Model ", i)
        if (!is.null(thismod@data$observed)) {
            dt <- thismod@data$observed
            probs <- class_prob(thismod)
            #probs <- data.frame(probs)

            prob_names <- colnames(probs$individual)
            dt <- cbind(dt, Model = thename, Class = apply(probs$individual, 1, which.max), id = 1:nrow(dt))
            out <- lapply(1:ncol(probs$individual), function(c){
                cbind(dt,
                      Class_prob = c,
                      Probability = probs$individual[, c])
            })
            out <- bind_list(out)
        }
    })
    do.call(rbind, out)
}
