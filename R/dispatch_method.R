.tidySEM_methods <- new.env(parent = emptyenv())

#' Register an External tidySEM Method
#'
#' Allows extension packages to register implementations
#' for tidySEM generics.
#'
#' @param generic Name of the generic function.
#' @param type Value of the model's `tidySEM` attribute.
#' @param fun Function implementing the method.
#'
#' @export
register_tidySEM_method <- function(generic, type, fun) {

  stopifnot(
    is.character(generic),
    length(generic) == 1L,
    is.character(type),
    length(type) == 1L,
    is.function(fun)
  )

  key <- paste(generic, type, sep = "::")

  assign(key, fun, envir = .tidySEM_methods)

  invisible(NULL)
}

dispatch_method <- function(x, generic, ...) {

  type <- attr(x, "tidySEM")

  if (is.null(type)) {
    type <- "MxModel"
  }

  key <- paste(generic, type, sep = "::")

  # Check for registered external methods
  if (exists(key, envir = .tidySEM_methods,
             inherits = FALSE)) {

    fun <- get(
      key,
      envir = .tidySEM_methods,
      inherits = FALSE
    )

    return(fun(x, ...))
  }

  # Fall back to internal tidySEM implementations
  suffix <- switch(
    type,
    mixture = "mixture",
    MxModel = "MxModel",
    stop("No method registered for model type: ", type)
  )

  fun <- get(
    paste0(generic, "_", suffix),
    mode = "function"
  )

  fun(x, ...)
}

# table_fit <- function(x, ...) {
#
#   if (methods::is(x, "MxModel")) {
#     return(dispatch_method(x, "table_fit", ...))
#   }
#   # If it should handle other models too, then keep this to dispatch the regular
#   # S3 methods:
#   UseMethod("table_fit")
# }
