#' @title Add paths to an object of class 'tidy_sem'
#' @description Add paths to an object of class \code{tidy_sem}, or replace
#' existing paths. The paths must be specified as
#' \code{\link[lavaan]{model.syntax}}, and separated by commas.
#' @param model An object of class \code{tidy_sem}.
#' @param ... Paths to add or substitute, specified in
#' \code{\link{lavaan}{model.syntax}}, and separated by commas.
# @param strict_check Logical, indicating whether or not to throw an error if
# the variable names referenced in the new paths do not exist in the
# \code{data}, or in the \code{dictionary}, or in the existing \code{syntax}
# element. If \code{strict_check = FALSE}, the check is still performed, but
# throws a \code{warning} instead of an \code{error}.
# @param use_cols Character. Which columns to retain from the \code{lavaan}
# parameter table. Defaults to \code{c("lhs", "op", "rhs", "free", "label")}.
#' @return An object of class \code{tidy_sem}.
#' @details Currently, only the \code{\link{lavaan}{lavaan}} commands \code{~,
#' ~~, =~,} and \code{~1} are parsed.
#' @details  This function
#' relies on \code{lavaan \link[lavaan]{model.syntax}} to convert syntax strings
#' to \code{lavaan} parameter tables. By default, is uses the arguments
#' \code{int.ov.free = TRUE, int.lv.free = FALSE, auto.fix.first = TRUE,
#' auto.fix.single = TRUE, auto.var = TRUE, auto.cov.lv.x = TRUE,
#' auto.efa = TRUE, auto.th = TRUE, auto.delta = TRUE, auto.cov.y = TRUE,
#' meanstructure = TRUE}, in a similar way
#' to \code{\link[lavaan]{sem}} and \code{\link[lavaan]{cfa}}.
#' @examples
#' library(lavaan)
#' df <- iris[, 1:4]
#' names(df) <- paste0("x_", 1:4)
#' model <- tidy_sem(df)
#' model <- measurement(model)
#' model <- add_paths(model, x =~ a*x_1 + b*x_2 + a*x_3 + b*x_4)
#' res <- estimate_lavaan(model)
#' summary(res)
#' @seealso
#'  \code{\link[lavaan]{model.syntax}}
#' @rdname add_paths
#' @export
#' @importFrom lavaan lavParseModelString
# lavParseModelString() zet een string om in een data.frame
#   - Modifiers (bvb 3*x1) en constraints (bvb a > b) worden apart bijgehouden in als attributes.
# De output van lavPareModelString() gaat naar lavaanify(), die de volledige parameter tabel aanmaakt.
# De sem()/cfa() functies maken gebruik van de vele auto.* argumenten, om de tabel te vervolledigen
# De lavaan() functie doet dit opzettelijk niet.

# Indien je de lavaanify() functie rechtstreeks aanroept, is het handig om auto = TRUE te gebruiken, wat correspondeert met de sem/cfa functies. Bvb.
#
# lavaanify(HS.model, auto = TRUE)
#
# Daarnaast zijn er ook opties voor multiple groups (ngroups, group.equal,
#                                                    group.partial)
#
# Heel wat (publieke en interne) functies in lavaan voeren bewerkingen uit op deze parameter tabel:
#
#   lavaan:::lav_partable_*
#
#   > geschikte manier om bepaalde paden automatisch in te vullen,
# > bijvoorbeeld residual variance voor endogenous variables als de
# > gebruiker dat vergeten is. Ik wil hierbij ook zo min mogelijk dubbel
# > werk doen, en zoveel mogelijk aansluiten bij lavaan.
#
# Dit is precies wat lavaanify() doet, in combinatie met auto = TRUE, of fijnmaziger met de aparte argumenten. De lavaanify() functie is wat mij betreft het hart van lavaan.

add_paths <- function(model, ...){ #, strict_check = TRUE
  UseMethod("add_paths", model)
}

#structure(list(id = NULL, lhs = NULL, op = NULL, rhs = NULL, user = NULL, block = NULL, group = NULL, free = NULL, ustart = NULL, exo = NULL, label = NULL, plabel = NULL), class = "data.frame", row.names = c(NA, -0L))

#' @method add_paths tidy_sem
#' @export
add_paths.tidy_sem <- function(model, ...){ #, strict_check = TRUE
  cl <- match.call()
  cl["model"] <- list(model$syntax)
  if(!is.null(group_var(model))) cl[["ngroups"]] <- group_var(model, "ngroups")
  cl[[1L]] <- quote(add_paths)
  #Args <- c(list(model = model$syntax), as.list(match.call()[-c(1:2)]))
  model$syntax <- eval.parent(cl)
  return(model)
}

#' @method add_paths default
#' @export
#' @importFrom lavaan lavaanify lav_partable_complete lav_partable_merge
add_paths.default <- function(model, ...){ #, strict_check = TRUE
  if(!is.null(model)){
    if(!(is.data.frame(model) & all(c("lhs", "op", "rhs", "free") %in% names(model)))){
      stop("Model is not a valid lavaan parameterTable.", call. = FALSE)
    }
    #existing_pars <- max(unique(model$id))
  }
  # If use_cols is NULL, use all lav_partable columns
  use_cols <- c("lhs", "op", "rhs", "block", "group", "free", "label", "ustart", "plabel")
  if(is.null(use_cols)){
    use_cols <- names(lav_partable_complete(data.frame("lhs" = NA, "op" = NA, "rhs" = NA)))
  } else {
    if(!all(c("lhs", "op", "rhs", "free") %in% use_cols)){
      message("Columns 'lhs', 'op', 'rhs', and 'free' are required use_cols. These were added to use_cols.")
      use_cols <- unique(c("lhs", "op", "rhs", "free"), use_cols)
    }
  }

  model_old <- model
  # Clean dots
  Args_lav <- lav_from_dots(...)
  dots <- list(...)
  dots[names(Args_lav)] <- NULL
  # Now, check syntax arguments
  is_char <- sapply(dots, inherits, "character")
  if(any(!is_char)){
    no_char <- lapply(sys.call()[-c(1:2)][!is_char], deparse)
    has_name <- names(no_char) == ""
    no_char[!has_name] <- paste0(names(no_char[!has_name]), "=", no_char[!has_name])
    dots[!is_char] <- no_char
  }
  is_char <- sapply(dots, inherits, "character")
  if(any(!is_char)){
    warning("Some elements of '...' could not be parsed. Try entering these commands as a character string.")
    dots <- dots[is_char]
  }
  # Parse dots
  tab <- lavParseModelString(paste0(unlist(dots), collapse = ";"), as.data.frame. = TRUE)
  # Convert to lavaan
  tab <- do.call(tidysem_lavaanify, c(list(model = tab), Args_lav))
  # Drop == rows
  tab <- tab[!tab$op == "==", , drop = FALSE]
  tab$free[!tab$free==0] <- 1

  # Use use_cols
  tab <- tab[, use_cols]


  if(!is.null(model_old)){
    model_old$pid <- get_pid(model_old)
    tab$pid <- get_pid(tab)
    new_pids <- tab$pid[!tab$pid %in% model_old$pid]
    maxpid <- max(as.numeric(gsub("^\\.p(\\d+)\\.$", "\\1", model_old$plabel)))
    if(length(new_pids) > 0){
      for(this_col in c("lhs", "rhs", "plabel")){
        for(i in 1:length(new_pids)){
          this_plabel <- tab$plabel[tab$pid == new_pids[i]]
          tab[[this_col]][tab[[this_col]] == this_plabel] <- paste0(".p", maxpid + i, ".")
        }
      }
    }

    pid_dups <- tab[tab$pid %in% model_old$pid, ]
    if(nrow(pid_dups) > 0){
      tab <- tab[!tab$pid %in% model_old$pid, ]
      #model_old[match(pid_dups$pid, model_old$pid), -which(names(model_old) == "plabel")] <- pid_dups[, -which(names(pid_dups) == "plabel")]
    }
    # Remove rows where operator changed
    model_old$newopid <- get_pid(model_old, c("lhs", "rhs", "block"))
    tab$newopid <- get_pid(tab, c("lhs", "rhs", "block"))
    these <- tab$newopid %in% model_old$newopid
    if(any(these)){
      remove_plabels <- model_old$plabel[model_old$newopid %in% tab$newopid[these]]
      retain_rows <- apply(model_old, 1, function(x){!any(x %in% remove_plabels)})
      model_old <- model_old[retain_rows, ]
    }
    model_new <- lav_partable_merge(model_old, tab, remove.duplicated = TRUE, fromLast = TRUE, warn = FALSE)
    model_new[c("pid", "newopid")] <- NULL
  } else {
    model_new <- tab
  }

  return(model_new)
}


get_pid <- function(tab, pidcols = c("lhs", "rhs", "op", "block", "label")){
  apply(tab[, pidcols], 1, function(x){paste0(c(sort(x[c("lhs", "rhs")]), x[!names(x) %in% c("lhs", "rhs")]), collapse = "_X_")})
}


tidysem_lavaanify <- function(..., data = NULL){
  cl <- match.call()
  if(!is.null(data)) cl[["data"]] <- NULL
  cl[[1L]] <- str2lang("lavaan::lavaanify")
  x <- eval.parent(cl)
  if(!is.null(data)){
    obs <- vnames(x, type = "ov")
    ord <- sapply(data[obs], inherits, "ordered")
    cats <- sapply(data[obs], inherits, "factor") & !ord
    if(any(ord)){
      thres <- sapply(data[obs[ord]], function(i){length(levels(i))})-1
      thres <- thresh(thres)
      cl[["model"]] <- thres
      thres <- eval.parent(cl)
      x <- x[-which(x$op %in% c("~1", "|", "~*~", "~~") & x$lhs %in% obs[ord] & x$user == 0), ]
      x <- lav_partable_merge(x, thres, remove.duplicated = TRUE, fromLast = FALSE, warn = FALSE)
    }
  }
  return(x)
}

#' @importFrom stats runif
update_thresholds <- function(x, ...){
  nax <- is.na(x)
  if(any(!nax)){
    if(!all(sign(diff(na.omit(x))) == 1)){
      stop("The thresholds you are attempting to specify has starting values that are not strictly increasing, but type='RAM' models require them to be.")
    }
    out <- x
    for(i in 1:10){
      out[nax] <- sort(runif(length(x), min = min(out, na.rm = TRUE)-runif(1), max = max(out, na.rm = TRUE)+runif(1)))[nax]
      inc <- sign(diff(na.omit(out))) == 1
      if(all(inc)){
        break
      }
      if(i == 10) stop("Could not complete thresholds; either specify all thresholds by hand, or remove constraints.")
    }
    return(out)
  } else {
    return(mxNormalQuantiles(length(x)))
  }
}
