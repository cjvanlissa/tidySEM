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

  type <- as.character(attr(x, "tidySEM"))[1]

  if (is.null(type)) {
    type <- "MxModel"
  }

  # Check for registered external methods
  key <- paste(generic, type, sep = "::")

  if (exists(key, envir = .tidySEM_methods,
             inherits = FALSE)) {

    fun <- get(
      key,
      envir = .tidySEM_methods,
      inherits = FALSE
    )

    return(fun(x, ...))
  }

  # Internal method for this model type
  method <- paste0(generic, "_", type)

  if (exists(method, mode = "function", inherits = TRUE)) {
    fun <- get(method, mode = "function", inherits = TRUE)
    return(fun(x, ...))
  }

  # Fallback to generic MxModel method
  fallback <- paste0(generic, "_MxModel")

  if (exists(fallback, mode = "function", inherits = TRUE)) {
    fun <- get(fallback, mode = "function", inherits = TRUE)
    return(fun(x, ...))
  }

  stop(
    "No method found for '", generic,
    "' with model type '", type,
    "', and no MxModel fallback is available."
  )
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
