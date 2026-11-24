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
.oval_node <- function(p, df, npoints = 80) {

  if (nrow(df) == 0L) return(p)

  if (npoints < 3L) {
    stop("`npoints` must be at least 3.")
  }
  if (!"colour" %in% names(df)) {
    df$colour <- "black"
  }

  if (!"fill" %in% names(df)) {
    df$fill <- "white"
  }

  cx <- (df$node_xmin + df$node_xmax) / 2
  cy <- (df$node_ymin + df$node_ymax) / 2

  rx <- (df$node_xmax - df$node_xmin) / 2
  ry <- (df$node_ymax - df$node_ymin) / 2

  theta <- seq(0, 2 * pi, length.out = npoints)

  df_ellipse <- do.call(
    rbind,
    lapply(seq_len(nrow(df)), function(i) {

      data.frame(
        x = cx[i] + rx[i] * cos(theta),
        y = cy[i] + ry[i] * sin(theta),
        .sem_id = i,
        .sem_point = seq_along(theta)
      )
    })
  )
  metadata <- df[
    df_ellipse$.sem_id,
    setdiff(names(df), c("x", "y", ".sem_id")),
    drop = FALSE
  ]

  df_ellipse <- cbind(
    df_ellipse,
    metadata
  )

  rownames(df_ellipse) <- NULL

  aesthetics <- c(
    "linetype", "linewidth",
    "colour", "fill", "alpha"
  )

  args <- c(
    list(
      data = df_ellipse,
      mapping = ggplot2::aes(
        x = .data[["x"]],
        y = .data[["y"]],
        group = .data[[".sem_id"]]
      )
    ),
    as.list(
      df_ellipse[
        ,
        intersect(aesthetics, names(df_ellipse)),
        drop = FALSE
      ]
    )
  )

  p + do.call(ggplot2::geom_polygon, args)
}
