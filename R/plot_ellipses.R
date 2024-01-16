.error_arrow <- function(p, x, y, diameter, orientation, npoints = 20){
  radius <- diameter / 2
  switch(orientation,
         "up" = {
           xmean = x
           ymean = y + radius
           offset = 1.5
         },
         "down" = {
           xmean = x
           ymean = y - radius
           offset = .5
         },
         "left" = {
           xmean = x+ radius
           ymean = y
           offset = 0
         },
         "right" = {
           xmean = x- radius
           ymean = y
           offset = 1
         })

  point_seq <- seq((offset*pi),(2+offset)*pi,length.out = npoints) %% (2*pi)

  df_ellipse <- data.frame(matrix(
    c(xmean + radius * cos(point_seq), ymean + radius * sin(point_seq)),
    nrow = npoints, ncol = 2, dimnames = list(NULL, c("x", "y"))
  )
  )
  p + geom_path(data = df_ellipse, aes(x = .data[["x"]], y = .data[["y"]]), linetype = 1, arrow = ggplot2::arrow(angle = 25, length = unit(.1, "inches"), ends = "both", type = "closed"))
}

#' @importFrom ggplot2 geom_path geom_polygon
.oval_node <- function(p, df, oval_width, oval_height, npoints = 80){
  x <- df$x
  y <- df$y
  point_seq <- seq(0, 2*pi,length.out = npoints)

  df_ellipse <- matrix(
    c((.5*oval_width) * cos(point_seq), (.5*oval_height) * sin(point_seq)),
    nrow = npoints, ncol = 2)

  df_ellipse <- mapply(function(x, y){
    t(t(df_ellipse) + c(x, y))
  }, x = x, y = y, SIMPLIFY = FALSE)
  df_ellipse <- data.frame(do.call(rbind, df_ellipse))
  df_ellipse$grp <- rep(paste0("g", 1:length(x)), each = npoints)
  df$grp <- paste0("g", 1:length(x))
  df_ellipse <- merge(df_ellipse, df, by = "grp", all.y = TRUE)
  Args <- c("linetype", "size", "colour", "fill", "alpha", "linewidth")
  Args <- as.list(df_ellipse[which(names(df_ellipse) %in% Args)])

  Args <- c(list(
    data = df_ellipse,
    mapping = aes(x= .data[["X1"]], y= .data[["X2"]], group = .data[["grp"]])), Args)

  p + do.call(geom_polygon, Args)
}

#p <- ggplot(data = NULL)
#oval_node(p, x = c(0, 0), y = c(0, 3), oval_width = 2, oval_height = 1)
