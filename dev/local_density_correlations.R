if(FALSE){

y_spacing <- .5
lo <- get_layout("", "A", "",
                 "D", "", "B",
                 "", "C", "",
                 "", "T", "", rows = 4)

edg <- data.frame(from = c("A", "B", "C", "D", "C"),
                  to = c("B", "C", "D", "A", "T")
)

p <- prepare_graph(layout = lo, edges = edg)
p$edges

install.packages("RANN")


points1 <- matrix(c(0,1,1,2,2,1,1,0), ncol = 2, byrow = T)
points2 <- points1 + matrix(rep(c(2.5,3), each = nrow(points1)), ncol = 2)

indx1 <- RANN::nn2(points1, points2, k = 1, searchtype = "standard")
indx1 <- indx1$nn.idx[which.min(indx1$nn.dists)]
indx2 <- RANN::nn2(points2, points1, k = 1, searchtype = "standard")
indx2 <- indx2$nn.idx[which.min(indx2$nn.dists)]
ggplot(as.data.frame(rbind(points1, points2)), aes(x = V1, y = V2)) + geom_point() +
  geom_point(data = as.data.frame(rbind(points1[indx1,], points2[indx2, ])), color = "red")


p$edges$connect_from <- list("right", "bottom", "left", "top", "bottom")
p$edges$connect_to <- list("top", "right", "bottom", "left", "top")
p$edges$curvature <- c(rep(60, 4), NA)

g = plot(p)
#g+geom_point(aes(x = 5, y = 7), color = "red")
edg <- p$edges
nod <- p$nodes
df_edg <- data.frame(
  x1 = nod$x[match(edg$from, nod$name)],
  y1 = nod$y[match(edg$from, nod$name)],
  x2 = nod$x[match(edg$to, nod$name)],
  y2 = nod$y[match(edg$to, nod$name)]
)
df_edg <- df_edg[1:4, ]
x <- df_edg[1, ]
cor_candidates <- function(otherpoints, x, delta = 1){
  suppressMessages(attach(x))
  midp <- c(x1 + (x2 - x1) / 2, y1 + (y2 - y1) / 2)

  slop <- (y2 - y1) / (x2 - x1)
  #int <- c(x2, y2) - slop * c(x1, y1)
  new_slop <- -1.0 / slop
  #new_int <- c(x2, y2) - new_slop * c(x1, y1)
  p1 <- c(midp[1] + delta, midp[2] + delta * new_slop)
  p2 <- c(midp[1] - delta, midp[2] - delta * new_slop)
  list(p1, p2)[[(sum(pointdensity(rbind(otherpoints, p1), eps = nrow(otherpoints)-2)) > sum(pointdensity(rbind(otherpoints, p2), eps = nrow(otherpoints)-2))) + 1]]
}
midpoints <- matrix(nrow = nrow(df_edg), ncol = 2)
df_tmp <- nod[, c("x", "y")]
for(thisedg in seq(nrow(df_edg))){
  #if(thisedg == 3) browser()
  thisloc <- cor_candidates(otherpoints = df_tmp, x = df_edg[thisedg, ], delta = 1)
  df_tmp <- rbind(df_tmp, thisloc)
  midpoints[thisedg, 1:2] <- thisloc
}


# g+geom_point(aes(x = 5, y = 7), color = "red")+
#   geom_point(aes(x = p1[1], y = p1[2]), color = "blue") +
#   geom_point(aes(x = p2[1], y = p2[2]), color = "green")
g+geom_point(aes(x = 5, y = 7), color = "red")+
  geom_point(data = data.frame(midpoints), aes(x = X1, y = X2), color = "blue")

library(lavaan)
HS.model <- ' visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '
fit <- cfa(HS.model, data=HolzingerSwineford1939)
p <- prepare_graph(fit)
plot(p)
#p$edges$curvature[22] <- -60
edg <- p$edges
nod <- p$nodes
pts <- ggplot(data = nod, aes(x = x, y= y))+geom_point(color = "red")
this_row <- data.frame(edge_xmin = 4, edge_ymin = 4, edge_xmax = 12, edge_ymax = 6, curvature = 60)
A = matrix(c(this_row[["edge_xmin"]], this_row[["edge_ymin"]]), nrow = 1)
B = matrix(c(this_row[["edge_xmax"]], this_row[["edge_ymax"]]), nrow = 1)
M <- matrix(c(mean(c(this_row[["edge_xmin"]], this_row[["edge_xmax"]])),
              mean(c(this_row[["edge_ymin"]], this_row[["edge_ymax"]]))), nrow = 1)
radius <- dist(rbind(A, B))
AB <- B-A
N <- matrix(c(AB[2], -AB[1]), nrow=1)
C1 <- M + .5*(N * tan((this_row[["curvature"]]/180)*pi))
C2 <- M + .5*(N * tan(((-1 * this_row[["curvature"]])/180)*pi))

df_edg <- edg[!is.na(edg$curvature), ]
df_edg <- data.frame(
  x1 = nod$x[match(df_edg$from, nod$name)],
  y1 = nod$y[match(df_edg$from, nod$name)],
  x2 = nod$x[match(df_edg$to, nod$name)],
  y2 = nod$y[match(df_edg$to, nod$name)]
)
plot(nod$x, nod$y)
.flip_curve <- function(otherpoints, candidatepoints, eps = floor(sqrt(nrow(otherpoints)))){
  (sum(pointdensity(rbind(otherpoints, candidatepoints[[1]]), eps = eps)) > sum(pointdensity(rbind(otherpoints, candidatepoints[[2]]), eps = eps)))
}
midpoints <- matrix(nrow = nrow(df_edg), ncol = 2)
df_tmp <- nod[, c("x", "y")]
for(thisedg in seq(nrow(df_edg))){
  #if(thisedg == 3) browser()
  thisloc <- cor_candidates(otherpoints = df_tmp, x = df_edg[thisedg, ], delta = 1)
  df_tmp <- rbind(df_tmp, thisloc)
  midpoints[thisedg, 1:2] <- thisloc
}

g <- plot(p)
g + geom_point(data = data.frame(midpoints), aes(x = X1, y = X2), color = "blue")

.reflect_points <- function(points, start, end){
  b <- (end[1] * start[2] - start[1] * end[2])/(end[1] - start[1])
  m <- (end[2]-start[2])/(end[1]-start[1])
  m3 <- m1 <- diag(3)
  m1[2,3] <- b
  m3[2,3] <- -1 * b
  m2 <- matrix(c((1-m^2)/(1+m^2), 2*m/(1+m^2), 0,
                 2*m/(1+m^2), (m^2-1)/(1+m^2), 0,
                 0, 0, 1), nrow = 3, byrow = TRUE)
  m4 <- rbind(t(points), 1)
  out <- m1 %*% m2 %*% m3 %*% m4
  t(out)[,1:2]
}
}
