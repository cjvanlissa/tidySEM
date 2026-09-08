knn_impute <- function(x, k = 5) {
  stopifnot(is.data.frame(x), k >= 1, k <= nrow(x))

  ok_type <- sapply(x, inherits, what = c("ordered", "numeric", "integer"))

  if (!all(ok_type))
    stop("All columns must be numeric, integer, or ordered factors.")

  # Keep original classes
  original <- x

  # Numeric representation for calculating distances
  x[] <- lapply(x, as.numeric)

  # Scale columns so variables with large ranges don't dominate
  x[] <- lapply(x, scale)

  # Distance between two rows, using variables observed in both
  row_dist <- function(i, j) {
    use <- which(!is.na(x[i, ]) & !is.na(x[j, ]))
    if (length(use) == 0)
      return(Inf)

    sqrt(mean(unlist((x[i, use] - x[j, use]))^2))
  }


  for (col in 1:ncol(x)) {
    missing_rows <- is.na(x[[col]])
    observed_rows <- which(!missing_rows)
    missing_rows <- which(missing_rows)
    if (!length(missing_rows))
      next

    for (i in missing_rows) {
      d <- vapply(
        observed_rows,
        function(j) row_dist(i, j),
        numeric(1)
      )

      keep <- is.finite(d)

      if (!any(keep))
        next

      neighbours <- observed_rows[keep][
        order(d[keep])[seq_len(min(k, sum(keep)))]
      ]

      vals <- original[[col]][neighbours]
      original[[col]][i] <- switch(class(original[[col]])[1],
                            "ordered" = levels(original[[col]])[round(median(as.numeric(vals), na.rm = TRUE))],
                            "integer" = as.integer(round(mean(vals, na.rm = TRUE))),
                            mean(vals, na.rm = TRUE)
                            )
    }
  }
  return(original)
}

#x = iris[1:4]; x$Sepal.Width <- ordered(cut(x$Sepal.Length, 2)); x = mice::ampute(x)$amp
# knn_impute <- function(x, k = 5) {
#   if(!anyNA(x)) return(x)
#   stopifnot(is.data.frame(x), k >= 1, k <= nrow(x))
#
#   is_ordered <- sapply(x, inherits, what = "ordered")
#   if (!all(sapply(x, inherits, what = c("ordered", "numeric", "integer"))))
#     stop("All columns must be numeric, integer, or ordered factors.")
#
#   # Keep original classes
#   original <- x
#
#   # Numeric representation for calculating distances
#   x[] <- lapply(x, as.numeric)
#   x_imputed <- x
#   # Scale columns so variables with large ranges don't dominate
#   x[] <- lapply(x, scale)
#
#   # Distance between two rows, using variables observed in both
#   row_dist <- function(i, j) {
#     use <- which(!is.na(x[i, ]) & !is.na(x[j, ]))
#     if (length(use) == 0)
#       return(Inf)
#
#     sqrt(mean(unlist((x[i, use] - x[j, use]))^2))
#   }
#
#
#   for (col in 1:ncol(x)) {
#     missing_rows <- is.na(x[[col]])
#     observed_rows <- which(!missing_rows)
#     missing_rows <- which(missing_rows)
#     if (!length(missing_rows))
#       next
#
#     for (i in missing_rows) {
#       d <- vapply(
#         observed_rows,
#         function(j) row_dist(i, j),
#         numeric(1)
#       )
#
#       keep <- is.finite(d)
#
#       if (!any(keep))
#         next
#
#       neighbours <- observed_rows[keep][
#         order(d[keep])[seq_len(min(k, sum(keep)))]
#       ]
#
#       vals <- x_imputed[[col]][neighbours]
#       x_imputed[[col]][i] <- median(vals, na.rm = TRUE)
#     }
#   }
#   x_imputed[is_ordered] <- lapply(names(x_imputed)[is_ordered], function(n){
#     ordered(x_imputed[[n]], levels = seq_len(length(unique(x_imputed[[n]]))), labels = levels(original[[n]]))})
#   return(x_imputed)
# }
