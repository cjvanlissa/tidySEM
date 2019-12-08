if(FALSE){
  df_cor <- df_edges[df_edges$arrow == "none", ]
  curve_multiplier <- .1
  df_cor <- cbind(df_cor,
                  t(
                    apply(df_cor[, c("edge_xmin", "edge_ymin", "edge_xmax", "edge_ymax")], 1, function(x){
                      out <- unlist(
                        approx(c(x[1], (x[3]+.000001)), c(x[2], (x[4]+.000001)), n = 4))[c(2, 6, 3, 7)] - c(0, 0, .000001, .000001)
                      if(x[1]-x[3] < 1e-10){
                        out[c(2, 4)] <- out[c(2, 4)] + curve_multiplier*(x[2]-x[4])
                      }
                      if(x[2]-x[4] < 1e-10){
                        out[c(1, 3)] <- out[c(1, 3)] + curve_multiplier*(x[1]-x[3])
                      }
                      out
                    }))
  )
  names(df_cor)[(length(df_cor)-3):length(df_cor)] <- c("seg_xmin", "seg_ymin", "seg_xmax", "seg_ymax")



  p <- ggplot(NULL) +
    #geom_point() +
    geom_rect(data = df_nodes[df_nodes$shape == "rect", ], aes_string(xmin = "node_xmin", xmax = "node_xmax", ymin = "node_ymin", ymax = "node_ymax"), fill = "white", colour = "black") +
    geom_ellipse(data = df_nodes[df_nodes$shape == "oval", ], aes_string(x0 = "x", y0 = "y",
                                                                  a = .5*ellipses_a,
                                                                  b = .5*ellipses_b), angle = 0, fill = "white", colour = "black") +
    geom_text(data = df_nodes, aes_string(x = x, y = y, label = label), size = text_size)+

    geom_segment(data = df_edges[!df_edges$arrow %in% c("none", "curve"), ], aes_string(x = edge_xmin,
                                                                                 xend = edge_xmax,
                                                                                 y = edge_ymin,
                                                                                 yend = edge_ymax
    ), arrow = arrow(angle = 25, length = unit(.1, "inches"), ends = df_edges[!df_edges$arrow == "none", ]$arrow, type = "closed"), arrow.fill = "black") +
    geom_segment(data = df_edges[df_edges$arrow == "none", ], aes_string(x = edge_xmin,
                                                                  xend = edge_xmax,
                                                                  y = edge_ymin,
                                                                  yend = edge_ymax
    )) +

    # Part segment, part curve ------------------------------------------------
  browser()
  p + geom_segment(data = df_cor, aes_string(x = seg_xmin,
                                      xend = seg_xmax,
                                      y = seg_ymin,
                                      yend = seg_ymax)) +
    geom_curve(data = df_cor, aes_string(x = edge_xmin, y = edge_ymin, xend = seg_xmin, yend = seg_ymin))

  geom_label(data = df_edges[!df_edges$arrow == "curve", ],
             aes_string(x = text_x, y = text_y, label = label), size = text_size, fill = "white", label.size = NA)

    # End part segment, part curve --------------------------------------------

}
