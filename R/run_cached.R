run_cached <- function(expr, type = c("rds", "png"),
                       width = 7, height = 4.5, dpi = 150) {
  expr <- substitute(expr)
  type <- match.arg(type)

  # Identify the Rmd being rendered
  input <- knitr::current_input(dir = TRUE)

  # Interactive execution: evaluate without caching
  if (is.null(input) || !nzchar(input)) {
    return(eval(expr, envir = parent.frame()))
  }
  input <- normalizePath(input, mustWork = TRUE)
  run_everything <- isTRUE(as.logical(toupper(
    Sys.getenv("TIDYSEM_RUN_EVERYTHING", "false")
  )))

  # Create vignette-specific cache directory
  input <- normalizePath(input, mustWork = TRUE)

  cache_dir <- file.path(
    dirname(input),
    paste0("cache_", tools::file_path_sans_ext(basename(input)))
  )

  label <- knitr::opts_current$get("label")

  if (is.null(label) || !nzchar(label)) {
    stop("run_cached() requires a labeled R Markdown chunk.")
  }

  path <- file.path(cache_dir, paste0(label, ".", type))

  if (run_everything) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

    result <- eval(expr, envir = parent.frame())

    if (type == "rds") {
      saveRDS(result, path)
    } else {
      ggplot2::ggsave(
        filename = path,
        plot = result,
        width = width,
        height = height,
        dpi = dpi
      )
    }

  } else {
    if (!file.exists(path)) {
      stop("Cached result not found: ", path)
    }

    result <- if (type == "rds") {
      readRDS(path)
    } else {
      knitr::include_graphics(path)
    }
  }

  result
}
