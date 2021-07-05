#' Estimate mixture models using OpenMx
#'
#' Dynamically creates a batch of mixture models, with intelligent
#' defaults. This function is a wrapper around \code{mplusObject} and
#' \code{mplusModeler}, and the respective arguments of those functions can be
#' passed on using \code{...}. For instance, passing the argument
#' \code{run = 1L} means that the models will be evaluated and returned.
#'
#' In the arguments \code{model_class_specific} and \code{SAVEDATA}, the
#' character string \dQuote{\{C\}} is substituted with the correct class number.
#' The character string \dQuote{\{filename_stem\}} is substituted with the
#' filename stem, for example, to name savedata in line with the input files.
#'
#' In all arguments to \code{mplusObject}, a double space (\dQuote{  }) is
#' replaced with a newline character. This can be used to obtain nicely
#' formatted Mplus syntax.
#' @param model Syntax for the model; either a character string, or a list of
#' character strings, or a list of \code{mxModel} objects. See Details.
# @param model_class_specific Character vector. Mplus syntax for the
# class-specific model(s) of one or more categorical latent variables. Each
# element of \code{model_class_specific} is used as the class-specific syntax
# of a different categorical latent variable. This allows one to easily specify
# latent transition analyses (see second example). The character string
# \dQuote{\{C\}} is substituted with the correct class number, for example to
# set unique parameter labels for each class, or to specify equality
# constraints.
#' @param classes A vector of integers, indicating which class solutions to
#' generate. Defaults to 1L. E.g., \code{classes = 1:6},
#' \code{classes = c(1:4, 6:8)}.
#' @param data The data.frame to be used for model fitting.
#' @param run Logical, whether or not to run the model.
#' @param ... Additional arguments, passed to functions.
#  \link{mplusObject}, such as syntax
# for other Mplus options.
#' @details Model syntax can be specified in three ways, for ease of use and
# flexibility:
# \enumerate{
#   \item An atomic character string with lavaan syntax. Within this syntax,
#   the character string \code{\{C\}} is dynamically substituted with the
#   correct class number using \code{\link{lsub}}, for example to set unique
#   parameter labels for each class, or to specify equality constraints.
#   \item
# }
#
#' @return Returns an \code{\link[OpenMx]{mxModel}} with free parameters updated
#' to their final values.
#' @export
#' @keywords mixture models openmx
#' @examples
#' \dontrun{
#' # Example 1: Dynamic model generation using {C}
#' df <- iris[, 1, drop = FALSE]
#' names(df) <- "x"
#' mx_mixture(model = "x ~ m{C}*1
#'                     x ~~ v{C}*x", classes = 1, data = df)
#' # Example 2: Manually specified class-specific models
#' df <- iris[1:2]
#' names(df) <- c("x", "y")
#' mx_mixture(model = list("y ~ a*x",
#'                         "y ~ b*x"),
#'                         meanstructure = TRUE,
#'                         data = df) -> res
#'
#' # Example 3: Latent growth model
#' df <- empathy[1:6]
#' mx_mixture(model = "i =~ 1*ec1 + 1*ec2 + 1*ec3 +1*ec4 +1*ec5 +1*ec6
#'                     s =~ 0*ec1 + 1*ec2 + 2*ec3 +3*ec4 +4*ec5 +5*ec6",
#'                     meanstructure = TRUE, int.ov.free = FALSE,
#'                     int.lv.free = TRUE, auto.fix.first = TRUE,
#'                     auto.fix.single = TRUE, auto.var = TRUE,
#'                     auto.cov.lv.x = TRUE, auto.efa = TRUE,
#'                     auto.th = TRUE, auto.delta = TRUE,
#'                     auto.cov.y = TRUE,
#'                     classes = 2,
#'                     data = df) -> res
#' }
#' @importFrom OpenMx mxPath mxModel mxRun mxTryHard
mx_mixture <- function(model,
                       classes = 1L,
                       data = NULL,
                       run = TRUE,
                       ...){
  UseMethod("mx_mixture", model)
}

#' Estimate growth mixture models using OpenMx
#'
#' This function is a wrapper around \code{\link{mx_mixture}}, adding the
#' default arguments of \code{\link[lavaan]{growth}}. This function is only
#' useful if all the latent variables in the model are growth factors.
#' @param model Syntax for the model; either a character string, or a list of
#' character strings, or a list of \code{mxModel} objects.
#' See \code{\link{mx_mixture}}.
#' @param classes A vector of integers, indicating which class solutions to
#' generate. Defaults to 1L. E.g., \code{classes = 1:6},
#' \code{classes = c(1:4, 6:8)}.
#' @param data The data.frame to be used for model fitting.
#' @param run Logical, whether or not to run the model.
#' @param ... Additional arguments, passed to functions.
#' @return Returns an \code{\link[OpenMx]{mxModel}}.
#' @export
#' @keywords mixture models openmx
#' @examples
#' \dontrun{
#' df <- empathy[1:6]
#' mx_growth_mixture(model = "i =~ 1*ec1 + 1*ec2 + 1*ec3 +1*ec4 +1*ec5 +1*ec6
#'                            s =~ 0*ec1 + 1*ec2 + 2*ec3 +3*ec4 +4*ec5 +5*ec6
#'                            ec1 ~~ vec1*ec1
#'                            ec2 ~~ vec2*ec2
#'                            ec3 ~~ vec3*ec3
#'                            ec4 ~~ vec4*ec4
#'                            ec5 ~~ vec5*ec5
#'                            ec6 ~~ vec6*ec6
#'                            i ~~ 0*i
#'                            s ~~ 0*s
#'                            i ~~ 0*s",
#'                   classes = 2,
#'                   data = df) -> res
#' }
mx_growth_mixture <- function(model,
                              classes = 1L,
                              data = NULL,
                              run = TRUE,
                              ...){
  browser()
  defaults <- list(meanstructure = TRUE, int.ov.free = FALSE,
                   int.lv.free = TRUE, auto.fix.first = TRUE,
                   auto.fix.single = TRUE, auto.var = TRUE,
                   auto.cov.lv.x = TRUE, auto.efa = TRUE,
                   auto.th = TRUE, auto.delta = TRUE,
                   auto.cov.y = TRUE)
  dots <- list(...)
  cl <- match.call()
  cl[names(defaults)[!names(defaults) %in% names(cl)]] <- defaults[!names(defaults) %in% names(cl)]
  cl[[1L]] <- str2lang("tidySEM:::mx_mixture")
  eval.parent(cl)
}


#' @method mx_mixture character
#' @export
mx_mixture.character <- function(model,
                                 classes = 1L,
                                 data = NULL,
                                 run = TRUE,
                                 ...){
  cl <- match.call()
  dots <- list(...)
  # Recursive function
  if(length(classes) > 1){
    out <- lapply(classes, function(i){
      cl[["classes"]] <- i
      cl[[1L]] <- quote(mx_mixture)
      eval.parent(cl)
      })
    attr(out, "tidySEM") <- "list"
    return(out)
  } else {
    dots_asram <- names(dots)[names(dots) %in% unique(c(formalArgs(lavaan::lavaanify), formalArgs(OpenMx::mxModel)))]
    dots_asram <- dots[dots_asram]
    model <- lsub(model, 1:classes)
    model <- lapply(1:length(model), function(i){
      do.call(as_ram, c(
        list(
          x = model[[i]],
          model = paste0("class", i)),
        dots_asram))
      })
    cl[["classes"]] <- classes
    cl[["model"]] <- model
    cl[[1L]] <- str2lang("tidySEM:::as_mx_mixture")
    out <- eval.parent(cl)
    if(run){
      cl[["model"]] <- out
      cl[[1L]] <- str2lang("tidySEM:::mixture_starts")
      cl[["model"]] <- eval.parent(cl)
      cl[[1L]] <- str2lang("tidySEM:::run_mx")
      return(eval.parent(cl))
    } else {
      out
    }
  }
}

#' @method mx_mixture list
#' @export
mx_mixture.list <- function(model,
                            classes = 1L,
                            data = NULL,
                            run = TRUE,
                            ...){
  cl <- match.call()
  dots <- list(...)
  if(length(classes) > 1 | classes != length(model)){
    message("When calling mx_mixture() on a list, the number of classes is inferred from the length of the list. Argument 'classes = ", deparse(classes), "' was ignored.")
  }
  classes <- length(model)
  if(all(sapply(model, inherits, "character"))){
    dots_asram <- names(dots)[names(dots) %in% unique(c(formalArgs(lavaan::lavaanify), formalArgs(OpenMx::mxModel)))]
    dots_asram <- dots[dots_asram]
    out <- lapply(1:length(model), function(i){
      do.call(as_ram, c(
        list(
          x = out[[i]],
          model = paste0("class", i)),
        dots_asram))
      })
  } else {
    if(!all(sapply(out, inherits, "MxModel"))){
      stop("Function mx_mixture.list() requires argument 'model' to be a list of lavaan syntaxes or MxModels.")
    }
  }
  if(run){
    cl[["model"]] <- out
    cl[[1L]] <- str2lang("tidySEM:::mixture_starts")
    cl[["model"]] <- eval.parent(cl)
    cl[[1L]] <- str2lang("tidySEM:::run_mx")
    return(eval.parent(cl))
  } else {
    out
  }
}

as_mx_mixture <- function(model,
                          classes,
                          data,
                          ...){
  # Prepare mixture model
  mix <- mxModel(
    model = paste0("mix", classes),
    lapply(model, function(x){ mxModel(x, mxFitFunctionML(vector=TRUE)) }),
    mxData(data, type = "raw"),
    mxMatrix(values=1, nrow=1, ncol=classes, free=c(FALSE,rep(TRUE, classes-1)), name="weights"),
    mxExpectationMixture(paste0("class", 1:classes), scale="softmax"),
    mxFitFunctionML())
  attr(mix, "tidySEM") <- "mixture"
  mix
}

#' Automatically set starting values for an OpenMx mixture model
#'
#' Automatically set starting values for an OpenMx mixture model. This function
#' was designed to work with mixture models created using \code{tidySEM}
#' functions like \code{\link{mx_mixture}}, and may not work with other
#' \code{mxModel}s.
#' @param model A mixture model of class \code{mxModel}.
#' @param splits Optional. A numeric vector of length equal to the number of
#' rows in the \code{\link{mxData}} used in the \code{model} object. The data
#' will be split by this vector. See Details for the default setting and
#' possible alternatives.
#' @param ... Additional arguments, passed to functions.
#  \link{mplusObject}, such as syntax
# for other Mplus options.
#' @details Starting values are derived by the following procedure:
#' \enumerate{
#'   \item The mixture model is converted to a multi-group model.
#'   \item The data are split along \code{splits}, and assigned to the
#'   corresponding groups of the multi-group model.
#'   \item The multi-group model is run, and the final values of each group are
#'   assigned to the corresponding mixture component as starting values.
#'   \item The mixture model is returned with these starting values.
#' }
#'
#' If the argument \code{splits} is not provided, the function will call
#' \code{cutree(hclust(dist(data)), k = classes))}, where \code{data} is
#' extracted from the \code{model} argument.
#'
#' Other sensible ways to split the data include:
#' \itemize{
#'   \item Using K-means clustering: \code{\link[stats]{kmeans}}\code{(x = data, centers = classes)$cluster}
#'   \item Using agglomerative hierarchical clustering: \code{hclass(}\code{\link[mclust]{hc}}\code{(data = data), G = classes)[, 1]}
#'   \item Using a random split: \code{\link{sample.int}}\code{(n = classes, size = nrow(data), replace = TRUE)}
#' }
#' @return Returns an \code{\link[OpenMx]{mxModel}} with starting values.
#' @export
#' @keywords mixture models openmx
#' @examples
#' \dontrun{
#' df <- iris[, 1, drop = FALSE]
#' names(df) <- "x"
#' mod <- mx_mixture(model = "x ~ m{C}*1
#'                            x ~~ v{C}*x",
#'                            classes = 2,
#'                            data = df,
#'                            run = FALSE)
#' mod <- mixture_starts(mod)
#' }
#' @importFrom OpenMx mxModel mxRun mxTryHard mxAutoStart
#' @importFrom methods hasArg
mixture_starts <- function(model,
                           splits,
                           ...){
  stopifnot("mxModel is not a mixture model." = inherits(model@expectation, "MxExpectationMixture"))
  stopifnot("mxModel must contain data to determine starting values." = !(is.null(model@data) | is.null(model@data$observed)))
  classes <- length(model@submodels)
  data <- model@data$observed
  if(!hasArg(splits)){
    splits <- cutree(hclust(dist(data)), k = classes)
  }

  if(!classes == length(unique(splits))){
    stop("Argument 'splits' does not identify a number of groups equal to 'classes'.")
  }
  if(!all(unique(splits) %in% 1:classes)){
    splits <- as.integer(as.factor(splits))
  }

  strts <- lapply(1:classes, function(i){
    thissub <- names(model@submodels)[i]
    mxModel(model[[thissub]],
            mxData(data[splits == i, , drop = FALSE], type = "raw"),
            mxFitFunctionML())
    })
  strts <- do.call(mxModel, c(list(model = "mg_starts", mxFitFunctionMultigroup(names(model@submodels)), strts)))
  strts <- mxAutoStart(strts, type = "ULS")
  tryCatch({
    strts <- mxRun(strts, silent = TRUE, suppressWarnings = TRUE)
  }, error = function(e){
    tryCatch({
      strts <- mxAutoStart(strts, type = "DWLS")
      strts <<- mxTryHard(strts, extraTries = 100,
                          silent = TRUE,
                          verbose = FALSE,
                          bestInitsOutput = FALSE)
    }, error = function(e2){
      stop("Could not derive suitable starting values for the ", classes, "-class model.")
    })
  })
  # Insert start values into mixture model
  for(i in names(model@submodels)){
    if(!is.null(model[[i]][["M"]])){
      model[[i]]$M$values <- strts[[i]]$M$values
    }
    if(!is.null(model[[i]][["S"]])){
      model[[i]]$S$values <- strts[[i]]$S$values
    }
    if(!is.null(model[[i]][["A"]])){
      model[[i]]$A$values <- strts[[i]]$A$values
    }
    if(!is.null(model[[i]][["F"]])){
      model[[i]]$F$values <- strts[[i]]$F$values
    }
  }
  return(model)
}


estimate_mx_mixture <- function(model,
                                classes = NULL,
                                data = NULL,
                                ...){
  # Prepare initial clustering
  browser()
  clusts <- hclust(dist(data[model[[1]]$manifestVars]))
  splits <- cutree(tree = clusts, k = classes)
  strts <- lapply(1:classes, function(i){
    mxModel(model[[i]],
            mxData(data[splits == i, , drop = FALSE], type = "raw"),
            mxFitFunctionML())
  })
  strts <- do.call(mxModel, c(list(model = "mg_starts", mxFitFunctionMultigroup(paste0("class", 1:classes)), strts)))
  strts <- mxAutoStart(strts, type = "ULS")
  tryCatch({
    strts <- mxRun(strts, silent = TRUE, suppressWarnings = TRUE)
  }, error = function(e){
    tryCatch({
      strts <- mxAutoStart(strts, type = "DWLS")
      strts <<- mxTryHard(strts, extraTries = 100,
                          silent = TRUE,
                          verbose = FALSE,
                          bestInitsOutput = FALSE)
    }, error = function(e2){
      stop("Could not derive suitable starting values for the ", classes, "-class model.")
    })
  })
  # Insert start values into mixture model
  model <- mapply(function(cls, strt){
    if(!is.null(cls[["M"]])){
      cls$M$values <- strts[[paste0("class", strt)]]$M$values
    }
    if(!is.null(cls[["S"]])){
      cls$S$values <- strts[[paste0("class", strt)]]$S$values
    }
    if(!is.null(cls[["A"]])){
      cls$A$values <- strts[[paste0("class", strt)]]$A$values
    }
    if(!is.null(cls[["F"]])){
      cls$F$values <- strts[[paste0("class", strt)]]$F$values
    }
    mxModel(cls, mxFitFunctionML(vector=TRUE))
  }, cls = model, strt = 1:classes)
  # Prepare mixture model
  mix <- mxModel(
    model = paste0("mix", classes),
    model,
    mxData(data, type = "raw"),
    mxMatrix(values=1, nrow=1, ncol=classes, free=c(FALSE,rep(TRUE, classes-1)), name="weights"),
    mxExpectationMixture(paste0("class", 1:classes), scale="softmax"),
    mxFitFunctionML())
  # Run analysis ------------------------------------------------------------
  mix_fit <- mxTryHard(mix,
                       extraTries = 100,
                       intervals=TRUE,
                       silent = TRUE,
                       verbose = FALSE,
                       bestInitsOutput = FALSE,
                       exhaustive = TRUE)
  attr(mix_fit, "tidySEM") <- "mixture"
  mix_fit
}
