#' Plot categorical variable probabilities
#'
#' Creates a bar chart of categorical variable probabilities with bars
#' reflecting the probability of category membership for each category of the
#' observed variable.
#' @param x An object for which a method exists
#' @param variables A character vectors with the names of the variables to be
#' plotted (optional).
#' @param bars Atomic character, indicating what separate bars represent. One of
#' `c("Variable", "group", "class")`.
#' @param facet Atomic character, indicating what separate facets represent. One
#' of `c("group", "class", "Variable")`.
#' @param bw Logical. Should the plot be black and white (for print), or color?
#' @param ... Arguments passed to and from other functions.
#' @return An object of class 'ggplot'.
#' @author Caspar J. van Lissa
#' @keywords plot mixture
#' @examples
#' df_plot <- data.frame(Variable = rep(c("u1", "u2"), each = 3),
#' Category = rep(1:3, 2),
#' Probability = c(0.3381302605812, 0.148395173612088, 0.513474565806711,
#' 0.472337708760608, 0.118484201496432, 0.40917808974296))
#' plot_prob(df_plot)
#' @rdname plot_prob
#' @export
plot_prob <- function(x, variables = NULL, bars = c("Variable", "group", "class"), facet = c("group", "class", "Variable"), bw = FALSE, ...){
    UseMethod("plot_prob", x)
}

#' @method plot_prob MxModel
#' @export
plot_prob.MxModel <- function(x, variables = NULL, bars = c("Variable", "group", "class"), facet = c("group", "class", "Variable"), bw = FALSE, ...){
    cl <- match.call()
    cl[["x"]] <- table_prob(x)
    cl[[1]] <- quote(plot_prob)
    eval.parent(cl)
}

#' @import ggplot2
#' @method plot_prob default
#' @export
plot_prob.default <- function(x, variables = NULL, bars = c("Variable", "group", "class"), facet = c("group", "class", "Variable"), bw = FALSE, ...){
    cl <- match.call()
    df_plot <- x
    if(!is.null(variables)) df_plot <- subset(x, subset = x$Variable %in% variables)
    bars <- bars[bars %in% names(df_plot)][1]
    facet <- facet[facet %in% names(df_plot)][1]
    if(isTRUE(bars == facet)) facet <- NA
    if(nrow(df_plot)<2) stop("Insufficient data to plot.")
    if(is.na(bars) & is.na(facet)) stop("Cannot plot without either a valid 'bars' argument or 'facet' argument.")
    p <- ggplot(df_plot)
    if(!is.na(bars)){
        p <- p + aes_string(x = bars, y = "Probability", fill = "Category") + scale_x_discrete(expand = c(0,0))
    } else {
        p <- p + aes_string(x = 1, y = "Probability", fill = "Category")
    }
    if(!is.na(facet)) p <- p + facet_wrap(facet, scales = "free")
    p <- p + geom_bar(stat = "identity", position = position_fill(reverse = TRUE)) + ylab(NULL)  + scale_y_continuous(expand = c(0,0)) + theme_bw() + theme(legend.position = "none")
    if(bw) p <- p + scale_fill_grey()
    p
}
