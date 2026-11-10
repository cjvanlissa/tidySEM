run_cached <- function(expr) {
  run_everything <- isTRUE(as.logical(toupper(
    Sys.getenv("TIDYSEM_RUN_EVERYTHING", "false")
  )))

  expr <- substitute(expr)

  # Identify the Rmd being rendered
  input <- knitr::current_input(dir = TRUE)

  if (is.null(input) || !nzchar(input)) {
    stop("run_cached() must be called while rendering an Rmd file.")
  }

  # Create a cache directory specific to the Rmd
  cache_dir <- file.path(
    dirname(input),
    paste0("cache_", tools::file_path_sans_ext(basename(input)))
  )

  # Get current chunk label
  label <- knitr::opts_current$get("label")

  if (is.null(label) || !nzchar(label)) {
    stop("run_cached() must be called inside a labeled R Markdown chunk.")
  }

  # Create cache directory if needed
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # Add cache directory to .gitignore
  gitignore <- file.path(dirname(input), "..", ".gitignore")

  ignore_rule <- paste0(
    "!vignettes/", basename(cache_dir), "/"
  )

  ignore_contents <- if (file.exists(gitignore)) {
    readLines(gitignore, warn = FALSE)
  } else {
    character()
  }

  if (!ignore_rule %in% ignore_contents) {
    cat(
      "\n", ignore_rule, "\n",
      paste0(ignore_rule, "**"), "\n",
      file = gitignore,
      append = TRUE,
      sep = ""
    )
  }

  path <- file.path(cache_dir, paste0(label, ".rds"))

  if (run_everything) {
    result <- eval(expr, envir = parent.frame())
    saveRDS(result, path)
  } else {
    if (!file.exists(path)) {
      stop("Cached result not found: ", path)
    }

    result <- readRDS(path)
  }

  result
}
