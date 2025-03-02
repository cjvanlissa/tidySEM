.extract_rawdata <- function(x, select_vars, wide = TRUE){
  UseMethod(".extract_rawdata", x)
}

#' @importFrom stats reshape
.extract_rawdata.MxModel <- function(x, select_vars, wide = TRUE){
  df_raw <- .get_long_data(list(x))
  if(inherits(select_vars, "factor")) select_vars <- levels(select_vars)
  df_raw <- df_raw[, c("Model", select_vars, "Class", "Class_prob", "Probability")]
  df_raw$id <- 1:nrow(df_raw)
  if(!wide){
    variable_names <- paste("Value", names(df_raw)[-c(1,2, ncol(df_raw)-c(0:3))], sep = "...")
    names(df_raw)[-c(1,2, ncol(df_raw)-c(0:3))] <- variable_names
    df_raw <- reshape(
      df_raw,
      varying = c(Variable = variable_names),
      idvar = "new_id",
      direction = "long",
      timevar = "Variable",
      sep = "..."
    )
  }

  if(any(c("Class_prob", "id", "new_id") %in% names(df_raw))){
    df_raw <- df_raw[, -which(names(df_raw) %in% c("Class_prob", "id", "new_id"))]
  }
  df_raw
}


make_ellipsis <- function(r, xmean, ymean, sdx, sdy){
  r <- min(max(r, -1), 1)
  d <- acos(r)
  a <- seq(0, 2 * pi, len = 20)
  matrix(c(sdx * cos(a + d/2) + xmean, sdy * cos(a - d/2) + ymean), 20, 2, dimnames = list(NULL, c("x", "y")))
}

get_cordat <- function(x){
  UseMethod("get_cordat", x)
}

#' @export
get_cordat.MxModel <- function(x){
  classes <- names(x@submodels)
  df_cors <- do.call(rbind, lapply(classes, function(c){
    tmp <- x[[c]]@matrices$S$values
    sds <- diag(tmp)
    tmp[upper.tri(tmp)] <- NA
    diag(tmp) <- NA
    out <- as.data.frame.table(tmp, stringsAsFactors = FALSE)
    out <- out[!is.na(out$Freq), ]
    names(out) <- c("xvar", "yvar", "Correlation")
    out$xmean <- x[[c]]@matrices$M$values[1, out$xvar]
    out$ymean <- x[[c]]@matrices$M$values[1, out$yvar]
    out$xsd <- sds[out$xvar]
    out$ysd <- sds[out$yvar]
    out$Parameter <- paste0(out$xvar, ".WITH.", out$yvar)
    out$Class <- c
    out$Model <- x@name
    out
  }))
  df_cors$Classes <- length(unique(df_cors$Class))
  df_cors[, c("Parameter", "xvar", "yvar", "Class", "Model", "Classes", "Correlation", "xmean", "ymean", "xsd", "ysd")]
}

#' Create correlation plots for a mixture model
#'
#' Creates a faceted plot of two-dimensional correlation plots and
#' unidimensional density plots for a single mixture model.
#' @param x An object for which a method exists.
#' @param variables Which variables to plot. If NULL, plots all variables that
#' are present in the model.
#' @param sd Logical. Whether to show the estimated standard deviations as lines
#' emanating from the cluster centroid.
#' @param cors Logical. Whether to show the estimated correlation (standardized
#' covariance) as ellipses surrounding the cluster centroid.
#' @param rawdata Logical. Whether to plot raw data, weighted by posterior class
#' probability.
#' @param bw Logical. Whether to make a black and white plot (for print) or a
#' color plot. Defaults to FALSE, because these density plots are hard to read
#' in black and white.
#' @param alpha_range Numeric vector (0-1). Sets
#' the transparency of geom_density and geom_point.
#' @param ... Additional arguments.
#' @param return_list Logical. Whether to return a list of ggplot objects, or
#' just the final plot. Defaults to FALSE.
#' @return An object of class 'ggplot'.
#' @author Caspar J. van Lissa
#' @export
#' @examples
#' if(requireNamespace("OpenMx", quietly = TRUE)){
#' iris_sample <- iris[c(1:5, 145:150), c("Sepal.Length", "Sepal.Width")]
#' names(iris_sample) <- c("x", "y")
#' res <- mx_profiles(iris_sample, classes = 2)
#' plot_bivariate(res, rawdata = FALSE)
#' }
#' @keywords mixture correlation plot
#' @rdname plot_bivariate
#' @export
plot_bivariate <- function(x, variables = NULL, sd = TRUE, cors = TRUE, rawdata = TRUE, bw = FALSE, alpha_range = c(0, .1), return_list = FALSE, ...){
  UseMethod("plot_bivariate", x)
}

#' @method plot_bivariate mixture_list
#' @export
plot_bivariate.mixture_list <- function(x, variables = NULL, sd = TRUE, cors = TRUE, rawdata = TRUE, bw = FALSE, alpha_range = c(0, .1), return_list = FALSE, ...){
  Args <- match.call()
  if(length(x) == 1){
    Args$x <- x[[1]]
    Args[[1]] <- as.name("plot_bivariate")
    eval.parent(Args)
  } else {
    stop("plot_bivariate can only plot a single mixture model. This object contains ", length(x), " model objects. Extract one of these objects using '$' or '[[]]' and try again. E.g., \n  > plot_bivariate(", deparse(substitute(x)), "[[1]])")
  }
}

#' @method plot_bivariate MxModel
#' @export
plot_bivariate.MxModel <- function(x, variables = NULL, sd = TRUE, cors = TRUE, rawdata = TRUE, bw = FALSE, alpha_range = c(0, .1), return_list = FALSE, ...){
  dots <- list(...)
  df_plot <- get_cordat(x)
  if("label_class" %in% names(dots)){
    classlabs <- dots[["label_class"]]
    origlabs <- unique(df_plot$Class)
    if(isFALSE(length(classlabs) == length(origlabs))){
      stop("The vector 'label_class' must be the same length as the number of classes.")
    }
    if(isFALSE(all(names(classlabs) %in% unique(df_plot$Class)))){
      stop("The names of the vector 'label_class' must correspond to the class names.")
    }
    df_plot$Class <- classlabs[df_plot$Class]
  }
  df2 <- df_plot
  df2$Parameter <- paste0(df2$yvar, ".WITH.", df2$xvar)
  names(df2) <- gsub("^x", "xxx", names(df2))
  names(df2) <- gsub("^y", "x", names(df2))
  names(df2) <- gsub("^xxx", "y", names(df2))
  df_plot <- rbind(df_plot, df2[, names(df_plot)])
  df_plot$Class <- ordered(df_plot$Class)
  if(is.null(variables)){
    variables <- unique(c(df_plot$xvar, df_plot$yvar))
  }
  if(length(variables) < 2) stop("Function plot_bivariate() requires at least two variables.")
  if (rawdata) {
    df_raw <- .extract_rawdata(x, select_vars = variables)
    if("label_class" %in% names(dots)){
      df_raw$Class <- classlabs[df_raw$Class]
    }
    df_raw$Class <- ordered(df_raw$Class, labels = levels(df_plot$Class))
  }
  # Basic plot
  p <- .base_plot(ifelse(bw, 0, max(df_plot$Classes)))
  Args <- list(
    x = list("model" = x),
    variables = variables,
    longform = FALSE
  )
  n_vars <- length(Args$variables)
  model_mat <- matrix(1L:(n_vars*n_vars), nrow = n_vars)
  df_density <- do.call(.extract_density_data, Args)
  df_density$Class <- ordered(df_density$Class, levels = c(seq_along(levels(df_plot$Class)), "Total"), labels = c(levels(df_plot$Class), "Total"))
  args_dens <- list(plot_df = df_density,
                    variables = NULL)

  dens_plotlist <- lapply(Args$variables, function(thisvar){
    names(args_dens$plot_df)[which(names(args_dens$plot_df) == thisvar)] <- "Value"
    args_dens$variables <- thisvar
    do.call(.plot_density_fun, args_dens) + theme_bw() + labs(x = thisvar, y = thisvar)
  })
  cor_plotlist <- vector("list", length = length(Args$variables) * (length(Args$variables) - 1) / 2)
  xvars <- unlist(mapply(function(x, t){rep(x, t)}, x = Args$variables[-length(Args$variables)], t = (length(Args$variables)-1):1))
  yvars <- unlist(sapply(1:(length(Args$variables)-1), function(i){Args$variables[-1][i:(length(Args$variables)-1)]}))
  for(i in 1:length(cor_plotlist)){
    xv = xvars[i]
    yv = yvars[i]
    this_cor <- paste0(xv, ".WITH.", yv)
    if(this_cor %in% df_plot$Parameter){
      df_thiscor <- df_plot[df_plot$Parameter == this_cor, , drop = FALSE]
    } else {
      df_thiscor <- df_plot[df_plot$Parameter == paste0(yv, ".WITH.", xv), , drop = FALSE]
    }
    thisp <- p
    thisp <- thisp + geom_point(data = df_thiscor, aes(x = .data[["xmean"]], y = .data[["ymean"]]))
    if(sd){
      df_sd <- df_thiscor
      df_sd$sdminx <- df_sd$xmean - df_sd$xsd
      df_sd$sdmaxx <- df_sd$xmean + df_sd$xsd
      df_sd$sdminy <- df_sd$ymean - df_sd$ysd
      df_sd$sdmaxy <- df_sd$ymean + df_sd$ysd
      thisp <- thisp +
        geom_errorbar(data = df_sd, aes(
          x = .data[["xmean"]],
          ymin = .data[["sdminy"]],
          ymax = .data[["sdmaxy"]]),
          width = .0) +
        geom_errorbarh(data = df_sd, aes(
          y = .data[["ymean"]],
          xmin = .data[["sdminx"]],
          xmax = .data[["sdmaxx"]]),
          height = .0)
    }
    if(cors){
      # Make data.frame for elipses
      df_ellipse <- do.call(rbind, apply(df_thiscor, 1, function(x) {
        data.frame(do.call(make_ellipsis,
                           as.list(as.numeric(x[c(7:11)]))),
                   t(x[c(1:6)]))
      }))
      thisp <- thisp + geom_path(data = df_ellipse, aes(x = .data[["x"]],
                                                        y = .data[["y"]]))
    }
    if (rawdata) {
      thisp <- thisp +
        geom_point(
          data = df_raw,
          aes(
            x = .data[[as.character(df_thiscor$xvar[1])]],
            y = .data[[as.character(df_thiscor$yvar[1])]],
            alpha = .data[["Probability"]]
          )
        ) +
        scale_alpha_continuous(range = alpha_range, guide = "none")
    }
    cor_plotlist[[i]] <- thisp + labs(x = df_thiscor$xvar[1], y = df_thiscor$yvar[1])
    i <- i + 1

  }
  if(max(dens_plotlist[[1]]$data$y) < 1){
    dens_plotlist[[1]] <- suppressMessages({dens_plotlist[[1]] + scale_y_continuous(breaks= seq(0, max(dens_plotlist[[1]]$data$y), by = .2), labels = substring(sprintf("%4.1f", seq(0, max(dens_plotlist[[1]]$data$y), by = .2)), 3), expand = c(0, 0))})
  }
  plot_list <- vector("list", length = length(model_mat))
  plot_list[diag(model_mat)] <- dens_plotlist

  plot_list[which(lower.tri(model_mat))] <- cor_plotlist
  class(plot_list) <- c("plot_list", class(plot_list))
  if (return_list) return(plot_list)
  merge_corplots(plot_list)
}

#' @export
#' @method plot plot_list
plot.plot_list <- function(x, y, ...){
  plot(merge_corplots(x))
}

.base_plot <- function(num_colors) {
  p <- ggplot(NULL,
              aes(
                group = .data[["Class"]],
                linetype = .data[["Class"]],
                shape = .data[["Class"]]
              ))
  if(num_colors > 0){
    p <- p + aes(colour = .data[["Class"]]) +
      scale_colour_manual(values = get_palette(num_colors))
  }
  p + theme(
    legend.direction = "vertical",
    legend.box = "horizontal",
    legend.position = c(1, .997),
    legend.justification = c(1, 1)
  ) + theme_bw() +
    scale_x_continuous(expand = c(0, 0))+
    scale_y_continuous(expand = c(0, 0))

}


get_palette <- function(x){
  if(x < 10){
    switch(max(x-2, 1),
           c("#E41A1C", "#377EB8", "#4DAF4A"),
           c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3"),
           c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00"),
           c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33"),
           c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628"),
           c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628", "#F781BF"),
           c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999")
    )[1:x]
  } else {
    colrs <- grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
    c(get_palette(9), sample(colrs, (x-9)))
  }
}

#' @import grid gtable
#' @importFrom stats na.omit
merge_corplots <- function(plots, ...) {
  suppressWarnings({
    suppressMessages({

      n_vars <- sqrt(length(plots))

      null_grobs <- sapply(plots, inherits, what = "NULL")
      plots[null_grobs] <- lapply(1:sum(null_grobs), nullGrob)

      plot2_grobs <- ggplot_gtable(ggplot_build(plots[[2]]))
      grob_legend <-
        plot2_grobs$grobs[[which(sapply(plot2_grobs$grobs, `[[`, "name") == "guide-box")]]
      width_grob <- grobWidth(plot2_grobs$grobs[[grep("^axis.title.y.left", sapply(plot2_grobs$grobs, `[[`, "name"))]])


      model_mat <- matrix(1L:(n_vars * n_vars), nrow = n_vars)
      model_mat[upper.tri(model_mat)] <- NA

      no_x_y <- na.omit(as.vector(model_mat[-nrow(model_mat),-1]))
      keep_x <- model_mat[nrow(model_mat),-1, drop = TRUE]
      keep_y <- model_mat[-nrow(model_mat), 1, drop = TRUE]

      #     This is to remove legends and axis and adjust width
      plots[[n_vars]] <-
        ggplotGrob(plots[[n_vars]] + theme(legend.position = "none"))
      fixed_widths <- plots[[n_vars]]$widths
      fixed_heights <- plots[[n_vars]]$heights

      plots[keep_y] <- lapply(plots[keep_y], function(this_plot) {
        if (inherits(this_plot, "ggplot")) {
          ggplotGrob(
            this_plot + theme(
              axis.title.x = element_blank(),
              axis.text.x = element_blank(),
              axis.ticks.x = element_blank(),
              legend.position = "none"
            )
          )
        }
      })
      plots[keep_x] <- lapply(plots[keep_x], function(this_plot) {
        if (inherits(this_plot, "ggplot")) {
          ggplotGrob(
            this_plot + theme(
              axis.title.y = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              legend.position = "none"
            )
          )
        }
      })
      plots[no_x_y] <- lapply(plots[no_x_y], function(this_plot) {
        if (inherits(this_plot, "ggplot")) {
          ggplotGrob(
            this_plot + theme(
              axis.title.x = element_blank(),
              axis.text.x = element_blank(),
              axis.ticks.x = element_blank(),
              axis.title.y = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              legend.position = "none"
            )
          )
        }
      })

      for(x in 1:length(plots)){
        plots[[x]]$widths <- fixed_widths
        if(x > n_vars){
          plots[[x]]$widths[c(1,3)] <- unit(0, "cm")
          plots[[x]]$widths[4] <- plots[[x]]$widths[4]+width_grob
        }
        plots[[x]]$heights <- fixed_heights
        #Hier gaat iets mis. Ik weet niet waarom dit er oorspronkelijk stond..
        # if(!x %in% model_mat[nrow(model_mat), ]){
        #  plots[[x]]$heights[c(1)] <- unit(0, "cm")
        #  plots[[x]]$heights[8] <- plots[[x]]$heights[8]+width_grob
        # }
      }

      plots[[((n_vars - 1) * n_vars) + 1]] <- grob_legend

      gt <- gtable_matrix(
        "corr_plot",
        matrix(plots, nrow = n_vars, ncol = n_vars),
        widths = unit(rep(1, n_vars), "null"),
        heights = unit(rep(1, n_vars), "null")
      )


      #left <- textGrob(ylab, rot = 90, just = c(.5, .5))
      #gt <- gtable_add_cols(gt, widths = grobWidth(axes[[1]])+ unit(0.5, "line"), 0)
      #gt <- gtable_add_grob(gt, axes, t = 1, b = nrow(gt),
      #                       l = 1, r = 1, z = Inf)
      # gt <- gtable_add_cols(gt, widths = unit(0.5, "line"))

      grid.newpage()
      grid.draw(gt)
      invisible(gt)
    })
  })
}
