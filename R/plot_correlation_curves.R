#' @importFrom utils getFromNamespace
#' @import grid
calcControlPoints <- getFromNamespace("calcControlPoints", "grid")
calcOrigin <- getFromNamespace("calcOrigin", "grid")

if(FALSE){
    cor_curves <- function(p, x1, y1, x2, y2, lab, ...){
    # Sapply or something
    cps <- data.frame(calcControlPoints(x1, y1, x2, y2, angle = 90, curvature = 0.5, ncp = 1))
    df_curves <- data.frame(x1, y1, x2, y2, cps, lab)
    p + geom_curve(data = df_curves, aes_string(x = "x1", y = "y1", xend = "x", yend = "y"),
                   curvature = 0.5, angle = 90) +
        geom_curve(data = df_curves, aes_string(x = "x", y = "y", xend = "x2", yend = "y2"),
                   curvature = 0.5, angle = 90) +
        geom_label(data = df_curves, aes_string(x = "x", y = "y", label = "lab"))
    }


    ggplot(df_mid, aes(x = x1, y = y1)) +
        geom_point(size = 4) +
        geom_point(aes(x = x2, y = y2),
                   pch = 17, size = 4) +
        geom_curve(aes(x = x1, y = y1, xend = midx, yend = midy),
                   curvature = 0.5, angle = 90) +
        geom_curve(aes(x = midx, y = midy, xend = x2, yend = y2),
                   curvature = 0.5, angle = 90) +
        geom_label(aes(x = midx, y = midy, label = details))




make_ellipsis <- function(r, x, y, width, height, points = 20){
    r <- min(max(r, -1), 1)
    d <- acos(r)
    a <- seq(0, 2 * pi, len = points)
    matrix(c(width * cos(a + d/2) + x, height * cos(a - d/2) + y), points, 2, dimnames = list(NULL, c("x", "y")))
}

make_curve <- function(x1, x2, y1, y2, rhs = TRUE, ...){
    xm <- (x1 + x2)/2
    ym <- (y1 + y2)/2
    dx <- x2 - x1
    dy <- y2 - y1
    if(!dy | !dx){
        if(!dy){
            dy <- .5*dx
        } else {
            dx <- .5*dy
        }
        r <- 0
    } else {
        r <- cos(dy/dx)
    }
    df <- data.frame(make_ellipsis(r, xm, ym, .5*abs(dx), .5*abs(dy), ...))
    flips <- which(diff(sign(diff(df$x))) != 0)
    if(rhs){
        df <- df[(flips[1]+1):(flips[2]+1),]
    } else {
        df <- df[c((flips[2]+2):nrow(df), 1:flips[1]), ]
    }
    df
}
df <- make_curve(0, 0, 10, 5, rhs = T, points = 50)
ggplot(df, aes(x, y))+geom_path()

edge_xmin <- 1
edge_xmax <- 1
edge_ymin = -1
edge_ymax = 1
make_curve <- function(xmin, xmax, ymin, ymax){

    r <- min(max(r, -1), 1)
    d <- acos(r)
    a <- seq(0, 2 * pi, len = 20)
    matrix(c(width * cos(a + d/2) + mean(c(xmin, xmax)), height * cos(a - d/2) + mean(c(ymin, ymax))), 20, 2, dimnames = list(NULL, c("x", "y")))
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

calcControlPoints <- function (x1, y1, x2, y2, curvature, angle, ncp, debug = FALSE)
{
    xm <- (x1 + x2)/2
    ym <- (y1 + y2)/2
    dx <- x2 - x1
    dy <- y2 - y1
    slope <- dy/dx
    if (is.null(angle)) {
        angle <- ifelse(slope < 0, 2 * atan(abs(slope)), 2 *
                            atan(1/slope))
    }
    else {
        angle <- angle/180 * pi
    }
    sina <- sin(angle)
    cosa <- cos(angle)
    cornerx <- xm + (x1 - xm) * cosa - (y1 - ym) * sina
    cornery <- ym + (y1 - ym) * cosa + (x1 - xm) * sina
    if (debug) {
        grid.points(cornerx, cornery, default.units = "inches",
                    pch = 16, size = unit(3, "mm"), gp = gpar(col = "grey"))
    }
    beta <- -atan((cornery - y1)/(cornerx - x1))
    sinb <- sin(beta)
    cosb <- cos(beta)
    newx2 <- x1 + dx * cosb - dy * sinb
    newy2 <- y1 + dy * cosb + dx * sinb
    scalex <- (newy2 - y1)/(newx2 - x1)
    newx1 <- x1 * scalex
    newx2 <- newx2 * scalex
    ratio <- 2 * (sin(atan(curvature))^2)
    origin <- curvature - curvature/ratio
    if (curvature > 0)
        hand <- "right"
    else hand <- "left"
    oxy <- calcOrigin(newx1, y1, newx2, newy2, origin, hand)
    ox <- oxy$x
    oy <- oxy$y
    dir <- switch(hand, left = -1, right = 1)
    maxtheta <- pi + sign(origin * dir) * 2 * atan(abs(origin))
    theta <- seq(0, dir * maxtheta, dir * maxtheta/(ncp + 1))[c(-1,
                                                                -(ncp + 2))]
    costheta <- cos(theta)
    sintheta <- sin(theta)
    cpx <- ox + ((newx1 - ox) %*% t(costheta)) - ((y1 - oy) %*%
                                                      t(sintheta))
    cpy <- oy + ((y1 - oy) %*% t(costheta)) + ((newx1 - ox) %*%
                                                   t(sintheta))
    cpx <- cpx/scalex
    sinnb <- sin(-beta)
    cosnb <- cos(-beta)
    finalcpx <- x1 + (cpx - x1) * cosnb - (cpy - y1) * sinnb
    finalcpy <- y1 + (cpy - y1) * cosnb + (cpx - x1) * sinnb
    if (debug) {
        ox <- ox/scalex
        fox <- x1 + (ox - x1) * cosnb - (oy - y1) * sinnb
        foy <- y1 + (oy - y1) * cosnb + (ox - x1) * sinnb
        grid.points(fox, foy, default.units = "inches", pch = 16,
                    size = unit(1, "mm"), gp = gpar(col = "grey"))
        grid.circle(fox, foy, sqrt((ox - x1)^2 + (oy - y1)^2),
                    default.units = "inches", gp = gpar(col = "grey"))
    }
    list(x = as.numeric(t(finalcpx)), y = as.numeric(t(finalcpy)))
}

}
