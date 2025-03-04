#' Estimate mixture models using OpenMx
#'
#' Dynamically creates a batch of mixture models, with intelligent
#' defaults. See Details for more information.
#' @param model Syntax for the model; either a character string, or a list of
#' character strings, or a list of \code{mxModel} objects. See Details.
#' @param classes A vector of integers, indicating which class solutions to
#' generate. Defaults to 1L. E.g., \code{classes = 1:6},
#' \code{classes = c(1:4, 6:8)}.
#' @param data The data.frame to be used for model fitting.
#' @param run Logical, whether or not to run the model. If \code{run = TRUE},
#' the function calls \code{\link{mixture_starts}} and \code{\link{run_mx}}.
#' @param ... Additional arguments, passed to functions.
#' @details Model syntax can be specified in three ways, for ease of use and
#' flexibility:
#' \enumerate{
#'   \item An atomic character string with lavaan syntax. Within this syntax,
#'   the character string \code{\{C\}} is dynamically substituted with the
#'   correct class number using \code{\link{lsub}}, for example to set unique
#'   parameter labels for each class, or to specify equality constraints. E.g.,
#'   \code{x ~ m\{C\}*1} will be expanded to \code{x ~ m1*1} and \code{x ~ m2*1}
#'   when \code{classes = 2}. The resulting syntax for each class will be
#'   converted to an \code{mxModel} using \code{\link{as_ram}}.
#'   \item A list of character strings with lavaan syntax. Each item of the list
#'   will be converted to a class-specific \code{mxModel} using
#'   \code{\link{as_ram}}.
#'   \item A list of \code{mxModel} objects, specified by the user.
#' }
#'
#' @return Returns an \code{\link[OpenMx:mxModel]{OpenMx::mxModel()}}.
#' @export
#' @keywords mixture models openmx
#' @references Van Lissa, C. J., Garnier-Villarreal, M., & Anadria, D. (2023).
#' Recommended Practices in Latent Class Analysis using the Open-Source
#' R-Package tidySEM. Structural Equation Modeling.
#' \doi{10.1080/10705511.2023.2250920}
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
#'                     classes = 2,
#'                     data = df) -> res
#' }
# @importFrom OpenMx mxPath mxModel mxRun mxTryHard
mx_mixture <- function(model,
                       classes = 1L,
                       data = NULL,
                       run = TRUE,
                       ...){
  if(!isTRUE(requireNamespace("OpenMx", quietly = TRUE))) {
    return(NULL)
  }
  UseMethod("mx_mixture", model)
}

#' Estimate latent profile analyses using OpenMx
#'
#' This function is a wrapper around \code{\link{mx_mixture}} to simplify the
#' specification of latent profile models, also known as finite mixture models.
#' By default, the function estimates free means for all observed variables
#' across classes.
#' @param data The data.frame to be used for model fitting.
#' @param classes A vector of integers, indicating which class solutions to
#' generate. Defaults to 1L. E.g., \code{classes = 1:6},
#' @param variances Character vector. Specifies which variance components to
#' estimate. Defaults to "equal" (constrain variances across classes); the
#' other option is "varying" (estimate variances freely across classes). Each
#' element of this vector refers to one of the models you wish to run.
#' @param covariances Character vector. Specifies which covariance components to
#' estimate. Defaults to "zero" (covariances constrained to zero; this
#' corresponds
#' to an assumption of conditional independence of the indicators); other
#' options are "equal" (covariances between items constrained to be equal across
#' classes), and "varying" (free covariances across classes).
#' @param run Logical, whether or not to run the model. If \code{run = TRUE},
#' the function calls \code{\link{mixture_starts}} and \code{\link{run_mx}}.
#' @param expand_grid Logical, whether or not to estimate all possible combinations of the `variances` and `covariances` arguments. Defaults to `FALSE`.
#' @param ... Additional arguments, passed to functions.
#' @return Returns an \code{\link[OpenMx:mxModel]{OpenMx::mxModel()}}.
#' @export
#' @keywords mixture models openmx
#' @references Van Lissa, C. J., Garnier-Villarreal, M., & Anadria, D. (2023).
#' Recommended Practices in Latent Class Analysis using the Open-Source
#' R-Package tidySEM. Structural Equation Modeling.
#' \doi{10.1080/10705511.2023.2250920}
#' @examples
#' \dontrun{
#' data("empathy")
#' df <- empathy[1:6]
#' mx_profiles(data = df,
#'             classes = 2) -> res
#' }
# @importFrom OpenMx imxReportProgress
mx_profiles <- function(data = NULL,
                        classes = 1L,
                        variances = "equal",
                        covariances = "zero",
                        run = TRUE,
                        expand_grid = FALSE,
                        ...){
  if(!isTRUE(requireNamespace("OpenMx", quietly = TRUE))) {
    return(NULL)
  }
  if(expand_grid){
    grd <- expand.grid(variances, covariances, stringsAsFactors = FALSE)
    variances <- grd[[1]]
    covariances <- grd[[2]]
  }
  if(length(variances) > 0 & (!hasArg(covariances) | length(covariances) == 1)){
    covariances <- rep(covariances, length(variances))
  }
  if(length(covariances) > 0 & (!hasArg(variances) | length(variances) == 1)){
    variances <- rep(variances, length(covariances))
  }
  if (length(variances) != length(covariances)) {
    stop(
      "The 'variances' and 'covariances' arguments must be vectors of equal length. Together, they describe the models to be run."
    )
  }
  cl <- match.call()
  cl[[1L]] <- str2lang("tidySEM:::mx_mixture")
  if("variances" %in% names(cl)) cl[["variances"]] <- NULL
  if("covariances" %in% names(cl)) cl[["covariances"]] <- NULL
  cl[["data"]] <- data
  if(length(variances) == 1){
    cl[["model"]] <- profile_syntax(variances, covariances, names(data))
    out <- eval.parent(cl)
  } else {
    out <- mapply(function(v, c){
      cl[["model"]] <- profile_syntax(variances = v, covariances = c, names(data))
      eval.parent(cl)
    }, v = variances, c = covariances, SIMPLIFY = FALSE)
    out <- do.call(c, out)
  }
  vlab <- paste0(c(varying = "free", equal = "equal")[variances], " var")
  clab <- paste0(c(zero = "no", varying = "free", equal = "equal")[covariances], " cov")
  clab[clab == "no cov"] <- NA
  lbs <- gsub(", $", "", paste2(vlab, clab, sep = ", "))
  lbs <- paste(rep(lbs, each = length(classes)), rep(classes, length(lbs)))
  if(inherits(out, "list")){
    class(out) <- c("mixture_list", class(out))
    names(out) <- lbs
  }
  if(inherits(out, what = c("MxModel", "MxRAMModel"))){
    out <- OpenMx::mxModel(out, name = lbs)
  }
  out
}


#' Estimate growth mixture models using OpenMx
#'
#' This function is a wrapper around \code{\link{mx_mixture}}, adding the
#' default arguments of \code{\link[lavaan:growth]{lavaan::growth()}} to simplify the
#' specification of growth mixture models. This function is only
#' useful if all the latent variables in the model are growth factors.
#' @param model Syntax for the model; either a character string, or a list of
#' character strings, or a list of \code{mxModel} objects. See Details.
#' @param classes A vector of integers, indicating which class solutions to
#' generate. Defaults to 1L. E.g., \code{classes = 1:6},
#' \code{classes = c(1:4, 6:8)}.
#' @param data The data.frame to be used for model fitting.
#' @param run Logical, whether or not to run the model. If \code{run = TRUE},
#' the function calls \code{\link{mixture_starts}} and \code{\link{run_mx}}.
#' @param ... Additional arguments, passed to functions.
#' @return Returns an \code{\link[OpenMx:mxModel]{OpenMx::mxModel()}}.
#' @export
#' @keywords mixture models openmx
#' @references Van Lissa, C. J., Garnier-Villarreal, M., & Anadria, D. (2023).
#' Recommended Practices in Latent Class Analysis using the Open-Source
#' R-Package tidySEM. Structural Equation Modeling.
#' \doi{10.1080/10705511.2023.2250920}
#' @examples
#' \dontrun{
#' data("empathy")
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

#' Estimate latent class analyses using OpenMx
#'
#' This function simplifies the specification of latent class models:
#' models that estimate membership of a categorical latent variable based on
#' binary or ordinal indicators.
#' @param data The data.frame to be used for model fitting.
#' @param classes A vector of integers, indicating which class solutions to
#' generate. Defaults to 1L. E.g., \code{classes = 1:6},
#' @param run Logical, whether or not to run the model. If \code{run = TRUE},
#' the function calls \code{\link[OpenMx:mxTryHardOrdinal]{OpenMx::mxTryHardOrdinal()}}.
#' @param ... Additional arguments, passed to functions.
#' @return Returns an \code{\link[OpenMx:mxModel]{OpenMx::mxModel()}}.
#' @export
#' @keywords mixture models openmx
#' @references Van Lissa, C. J., Garnier-Villarreal, M., & Anadria, D. (2023).
#' Recommended Practices in Latent Class Analysis using the Open-Source
#' R-Package tidySEM. Structural Equation Modeling.
#' \doi{10.1080/10705511.2023.2250920}
#' @examples
#' \dontrun{
#' df <- data_mix_ordinal
#' df[1:4] <- lapply(df, ordered)
#' mx_lca(data = df,
#'        classes = 2) -> res
#' }
# mx_lca(data = df,
#        classes = 2, run = FALSE) -> res
# res$class1 <- mxModel(model = res$class1,
#                       mxAlgebra(pnorm(Thresholds), name = "Probscale"))
mx_lca <- function(data = NULL,
                   classes = 1L,
                   run = TRUE,
                   ...){
  if(!all(sapply(data, inherits, what = "ordered"))) stop("Function mx_lca() only accepts data of an ordinal (binary or ordered categorical) level of measurement.")
  cl <- match.call()
  dots <- list(...)

  # Recursive function
  if(length(classes) > 1){
    out <- lapply(classes, function(i){
      cl[["classes"]] <- i
      cl[[1L]] <- str2lang("tidySEM::mx_lca")
      eval.parent(cl)
    })
    attr(out, "tidySEM") <- "list"
    class(out) <- c("mixture_list", class(out))
    return(out)
  } else {
    # One class model
    thresh <- mx_thresholds(data)
    dots_mxmod <- names(dots)[names(dots) %in% formalArgs(OpenMx::mxModel)]
    dots_mxmod <- dots[dots_mxmod]
    c1 <- do.call(OpenMx::mxModel, c(
      list(
        model = "class1",
        type = "RAM",
        manifestVars = names(data),
        OpenMx::mxPath(from = "one", to = names(data), free = FALSE, values = 0),
        OpenMx::mxPath(from = names(data), to = names(data), free = FALSE, values = 1, arrows = 2),
        thresh),
      dots_mxmod))
    c1$expectation$thresholds <- "Thresholds"
    model <- lapply(1:classes, function(i){
      do.call(OpenMx::mxModel, list(
        model = c1,
        name = paste0("class", i)))
    })
    cl[["classes"]] <- classes
    cl[["model"]] <- model
    cl[["data"]] <- data
    cl[[1L]] <- str2lang("tidySEM:::as_mx_mixture")
    out <- eval.parent(cl)
    # cl[["model"]] <- out
    # cl[[1L]] <- str2lang("tidySEM:::mixture_starts")
    # out <- eval.parent(cl)
    if(run){
      cl[["model"]] <- out
      cl[["extraTries"]] <- 10
      cl[[1L]] <- str2lang("OpenMx::mxTryHardOrdinal")
      keep_these <- which(names(cl) %in% unique(c(formalArgs(OpenMx::mxTryHard), formalArgs(OpenMx::mxTryHardOrdinal))))
      cl <- cl[c(1, keep_these)]
      out <- eval.parent(cl)
      attr(out, "tidySEM") <- c(attr(out, "tidySEM"), "mixture")
      return(out)
    } else {
      out
    }
  }
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
      cl[[1L]] <- str2lang("tidySEM::mx_mixture")
      eval.parent(cl)
    })
    attr(out, "tidySEM") <- "list"
    class(out) <- c("mixture_list", class(out))
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
    cl[["data"]] <- data
    cl[[1L]] <- str2lang("tidySEM:::as_mx_mixture")
    out <- eval.parent(cl)
    cl[["model"]] <- out
    cl[[1L]] <- str2lang("tidySEM:::mixture_starts")
    out <- eval.parent(cl)
    if(run){
      cl[["x"]] <- out
      cl[["model"]] <- NULL
      cl[[1L]] <- str2lang("tidySEM:::run_mx")
      return(eval.parent(cl))
    } else {
      out
    }
  }
}

#' @method mx_mixture MxModel
#' @export
mx_mixture.MxModel <- function(model,
                            classes = 1L,
                            data = NULL,
                            run = TRUE,
                            ...){
  stop("This functionality has not yet been developed.") # Develop this functionality
}

#' @method mx_mixture list
#' @export
mx_mixture.list <- function(model,
                            classes = 1L,
                            data = NULL,
                            run = TRUE,
                            ...){
  #browser() # Check before CRAN
  cl <- match.call()
  dots <- list(...)
  if(length(classes) > 1){
    if(classes != length(model)){
      message("When calling mx_mixture() on a list, the number of classes is inferred from the length of the list. Argument 'classes = ", deparse(classes), "' was ignored.")
    }
  }
  classes <- length(model)
  if(all(sapply(model, inherits, "character"))){
    dots_asram <- names(dots)[names(dots) %in% unique(c(formalArgs(lavaan::lavaanify), formalArgs(OpenMx::mxModel)))]
    dots_asram <- dots[dots_asram]
    model <- lapply(1:length(model), function(i){
      do.call(as_ram, c(
        list(
          x = model[[i]],
          model = paste0("class", i)),
        dots_asram))
    })
  } else {
    if(!all(sapply(model, inherits, what = c("MxModel", "MxRAMModel")))){
      stop("Function mx_mixture.list() requires argument 'model' to be a list of lavaan syntaxes or MxModels.")
    }
    # Develop functionality for MxModels
    model <- lapply(1:length(model), function(i){
      OpenMx::mxModel(name = paste0("class", i),
              model[[i]])
    })
  }
  if(run){
    cl[["model"]] <- model
    cl[["classes"]] <- classes
    cl[["data"]] <- data
    cl[[1L]] <- str2lang("tidySEM:::as_mx_mixture")
    cl[["model"]] <- eval.parent(cl)
    cl[[1L]] <- str2lang("tidySEM:::mixture_starts")
    cl[["x"]] <- eval.parent(cl)
    cl[["model"]] <- NULL
    cl[[1L]] <- str2lang("tidySEM:::run_mx")
    return(eval.parent(cl))
  } else {
    model
  }
}

as_mx_mixture <- function(model,
                          classes,
                          data,
                          ...){
  # Prepare mixture model
  if(classes > 1){
    mix <- OpenMx::mxModel(
      model = paste0("mix", classes),
      lapply(model, function(x){ OpenMx::mxModel(x, OpenMx::mxFitFunctionML(vector=TRUE, rowDiagnostics = TRUE)) }),
      OpenMx::mxData(data, type = "raw"),
      OpenMx::mxMatrix(values=1, nrow=1, ncol=classes, lbound = 1e-4, free=c(FALSE,rep(TRUE, classes-1)), name="weights"),
      OpenMx::mxExpectationMixture(paste0("class", 1:classes), scale="sum"),
      OpenMx::mxFitFunctionML())
  } else {
    mix <- OpenMx::mxModel(
      model[[1]],
      OpenMx::mxData(data, type = "raw"),
      OpenMx::mxFitFunctionML(rowDiagnostics = TRUE),
      name = paste0("mix", classes))
  }
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
#' rows in the [OpenMx::mxData()] used in the \code{model} object. The data
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
#' \code{stats::kmeans(x = data, centers = classes)$cluster},
#' where \code{data} is extracted from the \code{model} argument.
#'
#' Sensible ways to split the data include:
#' \itemize{
#'   \item Using Hierarchical clustering:
#'    \code{cutree(hclust(dist(data)), k = classes))}
#'   \item Using K-means clustering:
#'   \code{stats::kmeans(x = data, centers = classes)$cluster}
#'   \item Using agglomerative hierarchical clustering:
#'   \code{mclust::hclass(data = data), G = classes)[, 1]}
#'   \item Using a random split:
#'   \code{sample.int(n = classes,
#'   size = nrow(data), replace = TRUE)}
#' }
#' @return Returns an [OpenMx::mxModel()] with starting values.
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
#' @references Shireman, E., Steinley, D. & Brusco, M.J. Examining the effect of
#' initialization strategies on the performance of Gaussian mixture modeling.
#' Behav Res 49, 282–293 (2017). \doi{10.3758/s13428-015-0697-6}
#'
#' Van Lissa, C. J., Garnier-Villarreal, M., & Anadria, D. (2023).
#' Recommended Practices in Latent Class Analysis using the Open-Source
#' R-Package tidySEM. Structural Equation Modeling.
#' \doi{10.1080/10705511.2023.2250920}
# @importFrom OpenMx mxModel mxRun mxTryHard mxAutoStart
#' @importFrom methods hasArg
#' @importFrom stats kmeans
mixture_starts <- function(model,
                           splits,
                           ...){
  if(!isTRUE(requireNamespace("OpenMx", quietly = TRUE))) {
    return(NULL)
  }
  stopifnot("mxModel is not a mixture model." = inherits(model@expectation, "MxExpectationMixture") | attr(model, "tidySEM") == "mixture")
  stopifnot("mxModel must contain data to determine starting values." = !(is.null(model@data) | is.null(model@data$observed)))
  classes <- length(model@submodels)
  if(classes < 2){
    strts <- try({simple_starts(model, type = "ULS")})
    if(inherits(strts, "try-error")){
      strts <- try({OpenMx::mxTryHardWideSearch(model)})
    }
    if(inherits(strts, "try-error")){
      stop("Could not derive suitable starting values for the 1-class model.")
    } else{
      return(strts)
    }
  }
  data <- model@data$observed
  isfac <- sapply(data, inherits, what = "factor")
  if(!hasArg(splits)){
    df_split <- data
    if(any(isfac)){
      df_split[which(isfac)] <- lapply(df_split[which(isfac)], as.integer)
    }
    splits <- try({kmeans(x = df_split, centers = classes)$cluster}, silent = TRUE)
    if(inherits(splits, "try-error")){
      message("Could not initialize clusters using K-means, switching to hierarchical clustering.")
      splits <- try({cutree(hclust(dist(df_split)), k = classes)}, silent = TRUE)
      if(inherits(splits, "try-error")){
        stop("Could not initialize clusters using hierarchical clustering. Consider using a different clustering method, or imputing missing data.")
      }
    }
    #
  }
  stopifnot("Number of unique values in splits must be identical to the number of latent classes." = length(unique(splits)) == length(names(model@submodels)))

  tab_split <- table(splits)
  small_cats <- tab_split < 2
  if(any(small_cats)){
    which_small <- which(small_cats)
    atleast <- 2+sum(tab_split[small_cats])
    choose_from <- which(tab_split > atleast)
    if(length(choose_from) == 0) stop("Some clusters were too small to determine sensible starting values in `mixture_starts()`. Either specify splits manually, or reduce the number of classes.")
    splits[sample(which(splits %in% choose_from), sum(tab_split[small_cats]))] <- splits[splits %in% names(small_cats)[small_cats]]
  }

  if(!classes == length(unique(splits))){
    stop("Argument 'splits' does not identify a number of groups equal to 'classes'.")
  }
  # Order splits from largest number to smallest
  tab_split <- sort(table(splits), decreasing = TRUE)
  splits <- as.integer(ordered(splits, levels = names(tab_split)))

  strts <- lapply(1:classes, function(i){
    thissub <- names(model@submodels)[i]
    data_split <- data[splits == i, , drop = FALSE]
    if(any(isfac)){
      tabfreq <- lapply(data_split[which(isfac)], table)
      if(any(unlist(tabfreq) == 0)){
        tabfreq <- lapply(tabfreq, function(t){t[t==0]})
        tabfreq <- tabfreq[sapply(tabfreq, length) > 0]
        for(thisfac in names(tabfreq)){
          for(thislev in names(tabfreq[[thisfac]])){
            addcases <- which(data[[thisfac]] == thislev)
            if(length(addcases) == 1){
              addcases <- data[addcases, , drop = FALSE]
            } else {
              addcases <- data[sample(x = addcases, size = 1), , drop = FALSE]
            }
            data_split <- rbind(data_split, addcases)
          }
        }
      }
    }
    OpenMx::mxModel(model[[thissub]],
                    OpenMx::mxData(data_split, type = "raw"),
                    OpenMx::mxFitFunctionML())
  })
  strts <- do.call(OpenMx::mxModel, c(list(model = "mg_starts", OpenMx::mxFitFunctionMultigroup(names(model@submodels)), strts)))
  strts_vals <- try(simple_starts(strts, type = "ULS"))
  if(inherits(strts_vals, "try-error")){
    strts_vals <- simple_starts(strts, type = "DWLS")
  }
  strts <- strts_vals
  subnamz <- names(strts@submodels)
  not_pd <- sapply(subnamz, function(n){ any(eigen(strts[[n]]$matrices$S$values)$values < 0)})
  if(any(not_pd)){
    for(n in names(not_pd)[not_pd]){
      strts[[n]][["S"]]$values <- Matrix::nearPD(strts[[n]][["S"]]$values, keepDiag = TRUE, maxit = 10000)$mat
    }
  }
  strts_vals <- try(OpenMx::mxRun(strts, silent = TRUE, suppressWarnings = TRUE), silent = TRUE)
  if(inherits(strts_vals, "try-error")){
    if(grepl("omxAssignFirstParameters", attr(strts_vals, "condition"), fixed = TRUE)){
      strts <- OpenMx::omxAssignFirstParameters(strts)
      strts_vals <- try(OpenMx::mxRun(strts, silent = TRUE, suppressWarnings = TRUE))
    } else {
      strts_vals <- try(OpenMx::mxTryHard(strts, extraTries = 100,
                              silent = TRUE,
                              verbose = FALSE,
                              bestInitsOutput = FALSE))
    }
  }

  if(inherits(strts_vals, "try-error")){
    stop("Could not derive suitable starting values for the ", classes, "-class model.")
  }
  strts <- strts_vals
  # Insert start values into mixture model
  for(i in names(model@submodels)){
    for(mtx in names(model[[i]]@matrices)){
      model[[i]][[mtx]]$values <- strts[[i]][[mtx]]$values
    }
  }
  model@matrices$weights$values[] <- tab_split/tab_split[1]
  return(model)
}


estimate_mx_mixture <- function(model,
                                classes = NULL,
                                data = NULL,
                                ...){
  # Prepare initial clustering
  clusts <- hclust(dist(data[model[[1]]$manifestVars]))
  splits <- cutree(tree = clusts, k = classes)
  strts <- lapply(1:classes, function(i){
    OpenMx::mxModel(model[[i]],
                    OpenMx::mxData(data[splits == i, , drop = FALSE], type = "raw"),
                    OpenMx::mxFitFunctionML())
  })
  strts <- do.call(OpenMx::mxModel, c(list(model = "mg_starts", OpenMx::mxFitFunctionMultigroup(paste0("class", 1:classes)), strts)))
  strts <- OpenMx::mxAutoStart(strts, type = "ULS")
  tryCatch({
    strts <- OpenMx::mxRun(strts, silent = TRUE, suppressWarnings = TRUE)
  }, error = function(e){
    tryCatch({
      strts <- OpenMx::mxAutoStart(strts, type = "DWLS")
      strts <<- OpenMx::mxTryHard(strts, extraTries = 100,
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
    OpenMx::mxModel(cls, OpenMx::mxFitFunctionML(vector=TRUE, rowDiagnostics = TRUE))
  }, cls = model, strt = 1:classes)
  # Prepare mixture model
  mix <- OpenMx::mxModel(
    model = paste0("mix", classes),
    model,
    OpenMx::mxData(data, type = "raw"),
    OpenMx::mxMatrix(values=1, nrow=1, ncol=classes, free=c(FALSE,rep(TRUE, classes-1)), lbound = 1e-4, name="weights"),
    OpenMx::mxExpectationMixture(paste0("class", 1:classes), scale="sum"),
    OpenMx::mxFitFunctionML())
  # Run analysis ------------------------------------------------------------
  mix_fit <- OpenMx::mxTryHard(mix,
                       extraTries = 100,
                       intervals=TRUE,
                       silent = TRUE,
                       verbose = FALSE,
                       bestInitsOutput = FALSE,
                       exhaustive = TRUE)
  attr(mix_fit, "tidySEM") <- "mixture"
  mix_fit
}

#param_names <- selected_variables <- names(df)
profile_syntax <- function(variances, covariances, parameters){
  mean_syntax <- paste0(paste0(parameters, " ~ m{C}", 1:length(parameters), " *1"), collapse = "\n")

  var_syntax <- switch(variances,
                       "equal" = paste0(paste0(parameters, " ~~ v", 1:length(parameters), " * ", parameters), collapse = "\n"),
                       "varying" = paste0(paste0(parameters, " ~~ v{C}", 1:length(parameters), " * ", parameters), collapse = "\n")
  )
  cor_syntax <- paste(syntax_cor_lavaan(parameters, generic_label = TRUE), collapse = "\n")
  cor_syntax <- switch(covariances,
                       "equal" = cor_syntax,
                       "varying" = gsub("~~ c", "~~ c{C}", cor_syntax, fixed = TRUE),
                       "zero" = gsub("~~ c\\d+", "~~ 0", cor_syntax)
  )

  paste(mean_syntax, var_syntax, cor_syntax, sep = "\n\n")
}


# @method mx_mixture data.frame
# @export
if(FALSE){
  mx_mixture.data.frame <- function(model,
                                    classes = 1L,
                                    data = NULL,
                                    run = TRUE,
                                    ...){
    browser() # Not run
    data <- model
    vars_cont <- names(data)[sapply(data, inherits, what = "numeric")]
    vars_bin <- names(data)[sapply(data, function(x){all(na.omit(x) %in% c(0, 1))})]
    vars_nom <- sapply(data, inherits, what = c("factor", "character"))
    vars_ord <- sapply(data, inherits, what = "ordered")
    vars_nom <- names(data)[vars_nom & !vars_ord]
    vars_ord <- names(data)[vars_ord]
    if(length(vars_nom) > 0){
      adddummies <- lapply(vars_nom, function(n){
        model.matrix(~.-1, data = data[, n, drop = FALSE])
      })
      adddummies <- do.call(cbind, adddummies)
      data <- cbind(data, adddummies)
      data[vars_nom] <- NULL
      vars_bin <- c(vars_bin, colnames(adddummies))
    }
    if(length(vars_bin) > 0){
      data[vars_bin] <- lapply(data[vars_bin], ordered)
    }
    mix_all <- mx_profiles(data, classes = 2, run = FALSE)
    mix_profiles <- mx_profiles(data[vars_cont], classes = 2, run = TRUE)
    df_ord <- data[c(vars_bin, vars_ord)]
    mix_ord <- mx_lca(df_ord, classes = 2, run = TRUE)
    nam_prof <- names(mix_profiles@submodels)
    nam_lca <- names(mix_ord@submodels)
    if(!all(nam_prof == nam_lca)) stop("Could not merge continuous and categorical models.")
    browser()  # Not run
    # Continuous
    for(n in nam_prof){
      for(m in names(mix_profiles[[n]]@matrices)){
        for(i in c("values", "labels", "free", "lbound", "ubound")){
          dims <- dim(mix_all[[n]][[m]][[i]])
          end <- dim(mix_profiles[[n]][[m]][[i]])
          start <- c(1, 1)
          if(!(length(start) ==0 |length(end) == 0)){
            mix_all[[n]][[m]][[i]][start[1]:end[1], start[2]:end[2]] <- mix_profiles[[n]][[m]][[i]]
          }
          end <- dim(mix_all[[n]][[m]][[i]])
          end[1] <- min(c(dims[1], end[1]))
          end[2] <- min(c(dims[2], end[2]))
          start <- dim(mix_profiles[[n]][[m]][[i]])+1
          start[1] <- min(c(dims[1], start[1]))
          start[2] <- min(c(dims[2], start[2]))
          if(!(length(start) ==0 |length(end) == 0)){
            mix_all[[n]][[m]][[i]][start[1]:end[1], start[2]:end[2]] <- mix_ord[[n]][[m]][[i]]
          }
        }
      }
      for(i in c("mat_dev", "mat_ones", "Thresholds")){
        mix_all[[n]][[i]] <- mix_ord[[n]][[i]]
      }
      mix_all[[n]]$expectation$thresholds <- mix_ord[[n]]$expectation$thresholds
    }

    browser()  # Not run

  }
}
