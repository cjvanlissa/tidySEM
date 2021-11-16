#' Create density plots for mixture models
#'
#' Creates mixture density plots. For
#' each variable, a Total density plot will be shown, along with separate
#' density plots for each latent class, where cases are weighted by the
#' posterior probability of being assigned to that class.
#' @param x Object for which a method exists.
#' @param variables Which variables to plot. If NULL, plots all variables that
#' are present in all models.
#' @param bw Logical. Whether to make a black and white plot (for print) or a
#' color plot. Defaults to FALSE, because these density plots are hard to read
#' in black and white.
#' @param conditional Logical. Whether to show a conditional density plot
#' (surface area is divided among the latent classes), or a classic density
#' plot (surface area of the total density plot is equal to one, and is
#' divided among the classes).
#' @param alpha Numeric (0-1). Only used when bw and conditional are FALSE. Sets
#' the transparency of geom_density, so that classes with a small number of
#' cases remain visible.
#' @param facet_labels Named character vector, the names of which should
#' correspond to the facet labels one wishes to rename, and the values of which
#' provide new names for these facets. For example, to rename variables, in the
#' example with the 'iris' data below, one could specify:
#' \code{facet_labels = c("Pet_leng" = "Petal length")}.
#' @return An object of class 'ggplot'.
#' @author Caspar J. van Lissa
#' @export
#' @import ggplot2
#' @keywords mixture density plot
#' @examples
#' \dontrun{
#' dat <-
#'   iris[, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")]
#' names(dat) <- paste0("x", 1:4)
#' res <- mx_profiles(dat, 1:3)
#' plot_density(res)
#' }
plot_density <-
    function(x,
             variables = NULL,
             bw = FALSE,
             conditional = FALSE,
             alpha = .2,
             facet_labels = NULL) {
        UseMethod("plot_density", x)
    }


#' @method plot_density default
#' @export
plot_density.default <-
    function(x,
             variables = NULL,
             bw = FALSE,
             conditional = FALSE,
             alpha = .2,
             facet_labels = NULL) {
        plot_df <- x
        if(!inherits(plot_df[["Title"]], "factor")){
            plot_df[["Title"]] <- factor(plot_df[["Title"]])
        }
        # Plot figure
        Args <- as.list(match.call()[-1])
        Args <- Args[which(names(Args) %in% c("variables", "bw", "conditional", "alpha"))]
        Args <- c(Args, list(plot_df = plot_df))
        density_plot <- do.call(.plot_density_fun, Args)
        # Relabel facets
        label_facets <- c(levels(plot_df$Variable), levels(plot_df$Title))
        names(label_facets) <- label_facets
        if(!is.null(facet_labels)){
            label_facets[which(tolower(names(label_facets)) %in% tolower(names(facet_labels)))] <- facet_labels[which(tolower(names(facet_labels)) %in% tolower(names(label_facets)))]
        }
        # Facet the plot
        if (length(unique(plot_df$Title)) > 1) {
            if (length(variables) > 1) {

                density_plot <- density_plot +
                    facet_grid(Title ~ Variable, labeller = labeller(Title = label_facets, Variable = label_facets), scales = "free_x")

            } else {
                density_plot <- density_plot +
                    facet_grid( ~ Title, labeller = labeller(Title = label_facets))
            }
        } else {
            if (length(variables) > 1) {
                density_plot <- density_plot +
                    facet_grid( ~ Variable, labeller = labeller(Variable = label_facets), scales = "free_x")
            }
        }

        density_plot <- density_plot +
            theme_bw()

        suppressWarnings(print(density_plot))
        return(invisible(density_plot))
    }


#' @method plot_density mixture_list
#' @export
plot_density.mixture_list <-
    function(x,
             variables = NULL,
             bw = FALSE,
             conditional = FALSE,
             alpha = .2,
             facet_labels = NULL,
             ...) {
        cl <- match.call()

        # If no variables have been specified, use all variables
        var_names <- x[[1]]@submodels[[1]]@manifestVars
        if (is.null(variables)) {
            variables <- var_names
        } else {
            variables <- variables[which((variables) %in% (var_names))]
        }
        if (!length(variables))
            stop("No valid variables provided.")

        cl[["variables"]] <- variables
        cl[[1L]] <- str2lang("tidySEM:::.extract_density_data")
        cl_extract <- cl[c(1L, match(c("x", "variables"), names(cl)))]
        cl[["x"]] <- eval.parent(cl_extract)
        cl[[1L]] <- str2lang("tidySEM::plot_density")
        eval.parent(cl)
    }

#' @method plot_density MxModel
#' @export
plot_density.MxModel <- function(x,
                                 variables = NULL,
                                 bw = FALSE,
                                 conditional = FALSE,
                                 alpha = .2,
                                 facet_labels = NULL,
                                 ...) {
    cl <- match.call()
    cl[[1L]] <- str2lang("tidySEM:::plot_density")
    x <- list(model = x)
    names(x) <- x$model@name
    class(x) <- c("mixture_list", class(x))
    cl[["x"]] <- x
    eval.parent(cl)
}


.extract_density_data <- function(x,
                                  variables = NULL, longform = TRUE){
    if(inherits(x, "tidyProfile")){
        x <- list(x)
    }
    x[sapply(x, function(i){is.null(i[["dff"]])})] <- NULL
    # Check if all variables (except CPROBs) are identical across models
    plot_df <- lapply(x, function(x)
        as.data.frame(x$dff))


    plot_df <-
        lapply(plot_df, function(x) {
            x <- x[, which(names(x) %in% c(grep("^CPROB", names(x), value = TRUE), variables))]
            names(x) <- gsub("^CPROB", "Probability.", names(x))
            data.frame(x, Probability.Total = 1)
        })

    for (i in names(plot_df)) {
        plot_df[[i]][, grep("^Probability", names(plot_df[[i]]))] <-
            lapply(plot_df[[i]][grep("^Probability", names(plot_df[[i]]))], function(x) {
                x / length(x)
            })
    }

    plot_df <- lapply(plot_df, function(x) {
        reshape(
            x,
            direction = "long",
            varying =
                grep("^Probability", names(x), value = TRUE),
            timevar = "Class",
            idvar = "ID"
        )
    })

    if(length(plot_df) > 1){
        plot_df <-
            do.call(rbind, lapply(names(plot_df), function(x) {
                data.frame(Title = gsub("_", " ", x), plot_df[[x]])
            }))
    } else {
        plot_df <- data.frame(Title = "", plot_df[[1]])
    }

    if(longform){
        variable_names <-
            which(!(
                names(plot_df) %in% c("Title", "Class", "Probability", "ID")
            ))

        names(plot_df)[variable_names] <-
            sapply(names(plot_df)[variable_names], function(x) {
                paste(c(
                    "Value_____",
                    toupper(substring(x, 1, 1)),
                    tolower(substring(x, 2))
                ), collapse = "")
            })

        plot_df <- reshape(
            plot_df,
            direction = "long",
            varying =
                grep("^Value", names(plot_df), value = TRUE),
            sep = "_____",
            timevar = "Variable"
        )[, c("Title", "Variable", "Value", "Class", "Probability")]

        plot_df$Variable <- factor(plot_df$Variable)
    }

    plot_df$Class <- factor(plot_df$Class)
    plot_df$Class <-
        ordered(plot_df$Class, levels = c("Total", levels(plot_df$Class)[-length(levels(plot_df$Class))]))
    plot_df
}

.plot_density_fun <- function(plot_df, variables, bw = FALSE, conditional = FALSE, alpha = .2){
    if (conditional) {
        if (bw) {
            plot_df <- plot_df[-which(plot_df$Class == "Total"),]
            density_plot <-
                ggplot(plot_df,
                       aes_string(x = "Value", y = "..count..", fill = "Class", weight = "Probability")) +
                geom_density(position = "fill") + scale_fill_grey(start = 0.2, end = 0.8)
        } else {
            plot_df <- plot_df[-which(plot_df$Class == "Total"),]
            density_plot <-
                ggplot(plot_df,
                       aes_string(x = "Value", y = "..count..", fill = "Class", weight = "Probability")) +
                scale_fill_manual(values = get_palette(length(levels(plot_df$Class))-1)) +
                geom_density(position = "fill")
        }
    } else {
        densities <- .get_dens_for_plot(plot_df)
        densities$alpha <- alpha
        densities$alpha[densities$Class == "Total"] <- 0
        densities$Class <- ordered(densities$Class, levels = c(levels(plot_df$Class)[-1][order(as.numeric(levels(plot_df$Class)[-1]))], levels(plot_df$Class)[1]))
        if (bw) {
            density_plot <-
                ggplot(densities,
                       aes_string(x = "x",
                                  y = "y",
                                  linetype = "Class"
                                  #size = "size"
                       )) + labs(x = "Value", y = "density")
            density_plot <- density_plot +
                geom_path()+
                scale_linetype_manual(values = c(2:length(levels(plot_df$Class)), 1))+
                scale_x_continuous(expand = c(0, 0))+
                scale_y_continuous(expand = c(0, 0))
        } else{
            density_plot <-
                ggplot(densities,
                       aes_string(x = "x",
                                  y = "y",
                                  fill = "Class",
                                  colour = "Class",
                                  alpha = "alpha"#,
                                  #size = "size"
                       )) + labs(x = "Value", y = "density")
            class_colors <- c(get_palette(length(levels(plot_df$Class))-1), "#000000")
            density_plot <- density_plot +
                scale_colour_manual(values = class_colors)+
                scale_fill_manual(values = class_colors) +
                scale_alpha_continuous(range = c(0, alpha), guide = "none")+
                scale_size_continuous(range = c(.5, 1), guide = "none")+
                geom_area(position = "identity")+
                scale_x_continuous(expand = c(0, 0))+
                scale_y_continuous(expand = c(0, 0))

        }
    }
    density_plot
}

#' @importFrom stats density
.get_dens_for_plot <- function(plot_df){
    vars <- unique(plot_df[["Variable"]])
    titles <- unique(plot_df[["Title"]])
    if(is.null(vars)) vars <- ""
    if(is.null(titles)) titles <- ""
    if(length(titles) < 2 ){
        if(length(vars) < 2){
            densities <- lapply(unique(plot_df$Class), function(thisclass){
                thedf <- plot_df[plot_df$Class == thisclass, ]
                thep <- thedf$Probability
                data.frame(Title = titles,
                           Variable = vars,
                           Class = thisclass,
                           suppressWarnings(density(as.numeric(thedf$Value), weights = thep))[c("x", "y")])
            })
            do.call(rbind, densities)
        } else {
            do.call(rbind, lapply(vars, function(thisvar){
                .get_dens_for_plot(plot_df[plot_df$Variable == thisvar, ])
            }))
        }
    } else {
        do.call(rbind, lapply(titles, function(thistit){
            .get_dens_for_plot(plot_df[plot_df$Title == thistit, ])
        }))
    }

}


.extract_density_data <- function (x, variables = NULL, longform = TRUE)
{
    if (inherits(x, "MxModel")) {
        x <- list(x)
        names(x) <- x[[1]]@name
    }
    plot_df <- do.call(rbind, lapply(names(x), function(n){
        i <- x[[n]]
        P <- class_prob(i, "individual")$individual
        colnames(P) <- 1:ncol(P)
        P <- cbind(P, Total = 1)
        P <- P/nrow(P)
        cbind(Title = n, do.call(rbind, lapply(1:ncol(P), function(c){
            cbind(i@data@observed, Class = colnames(P)[c], Probability = P[, c])
        })))
    }))

    # if (length(plot_df) > 1) {
    #     plot_df <- do.call(rbind, lapply(names(plot_df), function(x) {
    #         data.frame(Title = gsub("_", " ", x), plot_df[[x]])
    #     }))
    # }
    # else {
    #     plot_df <- data.frame(Title = "", plot_df[[1]])
    # }
    if (longform) {
        variable_names <- which(!(names(plot_df) %in% c("Title",
                                                        "Class", "Probability", "ID")))
        names(plot_df)[variable_names] <- sapply(names(plot_df)[variable_names],
                                                 function(x) {
                                                     paste(c("Value_____", toupper(substring(x, 1,
                                                                                             1)), tolower(substring(x, 2))), collapse = "")
                                                 })
        plot_df <- reshape(plot_df, direction = "long", varying = grep("^Value",
                                                                       names(plot_df), value = TRUE), sep = "_____", timevar = "Variable")[,
                                                                                                                                           c("Title", "Variable", "Value", "Class", "Probability")]
        plot_df$Variable <- factor(plot_df$Variable)
    }
    plot_df$Class <- factor(plot_df$Class)
    plot_df$Class <- ordered(plot_df$Class, levels = c("Total",
                                                       levels(plot_df$Class)[-length(levels(plot_df$Class))]))
    plot_df
}

