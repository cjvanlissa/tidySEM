make_ellipsis <- function(r, xmean, ymean, sdx, sdy){
    r <- min(max(r, -1), 1)
    d <- acos(r)
    a <- seq(0, 2 * pi, len = 20)
    matrix(c(sdx * cos(a + d/2) + xmean, sdy * cos(a - d/2) + ymean), 20, 2, dimnames = list(NULL, c("x", "y")))
}
# Make data.frame for elipses
if(FALSE){
    df_ellipse <- do.call(rbind, apply(df_plot, 1, function(x) {
        data.frame(do.call(make_ellipsis,
                           as.list(as.numeric(x[c(7:11)]))),
                   t(x[c(1:6)]))
    }))
    p <- p + geom_path(data = df_ellipse, aes_string(x = "x",
                                                     y = "y"))
}
