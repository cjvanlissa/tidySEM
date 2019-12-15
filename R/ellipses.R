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
  p + geom_path(data = df_ellipse, aes(x= x, y= y), linetype = 1, arrow = arrow(angle = 25, length = unit(.1, "inches"), ends = "both", type = "closed"))
}

.oval_node <- function(p, x, y, ellipsis_a, ellipsis_b, npoints = 80){
  point_seq <- seq(0,2*pi,length.out = npoints)

  df_ellipse <- matrix(
    c((.5*ellipsis_a) * cos(point_seq), (.5*ellipsis_b) * sin(point_seq)),
    nrow = npoints, ncol = 2)

  df_ellipse <- mapply(function(x, y){
    t(t(df_ellipse) + c(x, y))
  }, x = x, y = y, SIMPLIFY = FALSE)
  df_ellipse <- data.frame(do.call(rbind, df_ellipse))
  df_ellipse$grp <- rep(letters[1:length(x)], each = npoints)

  p + geom_path(data = df_ellipse, aes_string(x= "X1", y= "X2", group = "grp"))
}

#p <- ggplot(data = NULL)
#oval_node(p, x = c(0, 0), y = c(0, 3), ellipsis_a = 2, ellipsis_b = 1)
