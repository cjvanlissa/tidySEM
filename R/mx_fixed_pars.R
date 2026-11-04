
.mxfixedpars <- function(x) {

  out <- list()

  # Recursively extract fixed parameters
  submods <- names(x@submodels)

  for (themod in submods) {

    tmp <- .mxfixedpars(x[[themod]])

    if (is.null(tmp)) next

    tmp$matrix <- paste0(themod, ".", tmp$matrix)

    out[[length(out) + 1L]] <- tmp
  }

  # Extract fixed matrix elements
  mats <- x@matrices

  exclude <- c("F", "mat_dev", "mat_ones")

  for (thism in setdiff(names(mats), exclude)) {

    mat <- x[[thism]]

    values <- mat$values
    free <- mat$free

    # Identify fixed, nonzero elements
    keep <- !free & !is.na(values) & values != 0

    if (!any(keep)) next

    idx <- which(keep, arr.ind = TRUE)

    rn <- rownames(values)
    cn <- colnames(values)

    tmp <- data.frame(
      name = paste0(
        x@name, ".", thism, "[",
        idx[, 1], ",", idx[, 2], "]"
      ),
      matrix = thism,
      row = if (is.null(rn)) {
        as.character(idx[, 1])
      } else {
        rn[idx[, 1]]
      },
      col = if (is.null(cn)) {
        as.character(idx[, 2])
      } else {
        cn[idx[, 2]]
      },
      Estimate = values[keep],
      free = FALSE,
      stringsAsFactors = FALSE
    )

    out[[length(out) + 1L]] <- tmp
  }

  if (!length(out)) {
    return(NULL)
  }

  out <- bind_list(out)

  if (is.null(out) || !nrow(out)) {
    return(NULL)
  }

  out
}

