lgm <- function(y, loadings = 1:length(y), polynomials = 1, ...){
  UseMethod("lgm")
}

lgm.list <- function(y, loadings = 1:length(y), polynomials = 1, ...){
  cl <- match.call()
  if(!length(y) == 1) stop("Can only make lgm syntax for a scales_list of length 1.")
  if(!is.vector(y[[1]])) stop("Argument 'y' must be a list that contains a vector of variable names.")
  cl[["y"]] <- y[[1]]
  if(is.null(cl[["lv_name"]])) cl[["lv_name"]] <- names(y)[1]
  cl[[1]] <- as.name("lgm")
 eval.parent(cl)
}

lgm.character <- function(y, loadings = 1:length(y), polynomials = 1, lv_name = "", ...){
  lv_prefix <- c("i", "s", "q", "c")
  if(polynomials > 3){
    lv_prefix <- c(lv_prefix, paste0("p", 4:polynomials))
  }
  out <- NULL
  for(i in 0:polynomials){
    out <- c(out, paste0(lv_prefix[i+1], lv_name, " =~ ", paste0(loadings^i, "*", y, collapse = " + ")))
  }
  out
}

syntax_cfa_lavaan <- function(scales.list){
  variables <- names(scales.list)
  outlines <- do.call(c, list(sapply(variables, function(x){
    # x <- variables[1]
    c(paste0(x, " =~ NA*", scales.list[[x]][1], " + ",
           paste(scales.list[[x]][-1], collapse = " + ")),
           paste0(x, " ~~ 1*", x))
  }, USE.NAMES = FALSE)))
  outlines
}

syntax_cor_lavaan <- function(x, y = x, all = TRUE, label = TRUE){
  if(all){
    cors <- expand.grid(x, " ~~ ", y)
    if(label){
      cors <- cbind(cors, " ~~ c", "*")[, c(1, 4, 1,3,5,3)]
    }
    cors <- cors[!cors$Var1==cors$Var3, ]
    unname(apply(cors, 1, paste0, collapse = ""))
  } else {
    if(label){
      paste0(x, " ~~ c", x, y, "*", y)
    } else {
      paste0(x, " ~~ ", y)
    }
  }
}

#' @importFrom methods hasArg
syntax_cfa_mplus <- function(scales.list, fix_var = TRUE, fix_mean = TRUE, fix_first = FALSE, merge = TRUE, ...){
  variables <- names(scales.list)
  if(any(sapply(variables, nchar) > 8)){
    warning(paste0("Latent variable names longer than 8 characters will be truncated, specifically: ", paste(variables[which(sapply(variables, nchar) > 8)], collapse = ", ")))
    variables <- sapply(variables, substr, 1, 8)
    if(any(duplicated(variables))) stop("Truncation led to identical latent variable names.")
  }
  if(any(sapply(unique(unlist(scales.list)), nchar) > 8)){
    warning(paste0("Observed variable names longer than 8 characters will be truncated, specifically: ", paste(variables[which(sapply(variables, nchar) > 8)], collapse = ", ")))
    scales.list <- lapply(scales.list, sapply, substr, start = 1, stop = 8)
    if(any(duplicated(unlist(scales.list)))) stop("Truncation led to identical observed variable names.")
  }
  if(hasArg("standardize")){
    if(standardize){
      if(is.null(fix_first)) fix_first <- FALSE
      if(is.null(fix_var)) fix_var <- TRUE
      if(is.null(fix_mean)) fix_mean <- TRUE
    } else {
      if(is.null(fix_first)) fix_first <- TRUE
      if(is.null(fix_var)) fix_var <- FALSE
      if(is.null(fix_mean)) fix_mean <- FALSE
    }
  } else {
    standardize = FALSE
  }

    outlines <- sapply(variables, function(x){
      if(fix_first){
        out <- paste0(x, " BY ", scales.list[[x]][1], "@1;\n")
      } else {
        out <- paste0(x, " BY ", scales.list[[x]][1], "*;\n")
      }
      if(length(scales.list[[x]]) > 1) out <- c(out, paste0(x, " BY ", scales.list[[x]][-1], ";\n"))
      if(fix_mean) out <- c(out, paste0("[", x, "@0];\n"))
      if(fix_var) out <- c(out, paste0(x, "@1;\n"))
      out
    })
  #}
  #browser()
  if(merge){
    paste(unlist(outlines), collapse = "\n")
  } else {
    outlines
  }
}

syntax_cor_mplus <- function(x, y = NULL, all = TRUE){
  if(is.null(y)){
    y <- x
  }
  if(all){
    m <- matrix(paste0(rep(y, each = length(x)), " WITH ", rep(x, length(x)), ";\n"), ncol = length(x))
    m[upper.tri(m)]
    #apply(expand.grid(x, " WITH ", y, ";\n"), 1, paste, collapse = "")
  } else {
    paste(x, " WITH ", y, ";\n", collapse = "", sep = "")
  }
}

syntax_item_total_cor_mplus <- function(scales.list){
  list(usevariable =
  paste0("USEVARIABLE = \n",
         paste0(strwrap(paste0(unlist(scales.list), collapse = " "), width = 70), collapse = "\n"),
         "\n",
         paste0(strwrap(paste0(paste0("it", unlist(scales.list)), collapse = " "), width = 70), collapse = "\n"),
         ";"),

  define =
  paste0("\n\nDefine:\n",
         paste0(sapply(scales.list, function(all_items){
           all_items <- matrix(rep(all_items, length(all_items)), ncol = length(all_items), byrow = TRUE)
           with_items <- all_items
           diag(with_items) <- NA
           with_items <- t(matrix(t(with_items)[which(!is.na(with_items))], nrow = (nrow(with_items)-1), ncol = ncol(with_items)))

           paste0(paste0("it", diag(all_items), " = "),
                  apply(with_items, 1, paste, collapse = "+"), ";", collapse = "\n")
         }), collapse = "\n")
  ))
}


syntax_label <- function(x){
  intercepts <- grepl("\\[", x) & grepl("\\]", x)
  x[intercepts] <- paste0(gsub(";", "", x[intercepts]), " (i", gsub("(\\[|\\]|;| )", "", x[intercepts]), ");")

  cors <- grepl(" WITH ", x)
  x[cors] <- paste0(gsub(";", "", x[cors]), " (", gsub("^(.+?)\\s+?with.*$", "\\1", tolower(x[cors])), "W", gsub("^.+?with\\s+?(.*);$", "\\1", tolower(x[cors])), ");")

  loadings <- grepl(" BY ", x)
  x[loadings] <- paste0(gsub(";", "", x[loadings]), " (", gsub("^(.+?)\\s+?by.*$", "\\1", tolower(x[loadings])), "B", gsub("\\*", "", gsub("^.+?by\\s+?(.*);$", "\\1", tolower(x[loadings]))), ");")

  reg <- grepl(" ON ", x)
  x[reg] <- paste0(gsub(";", "", x[reg]), " (", gsub("^(.+?)\\s+?on.*$", "\\1", tolower(x[reg])), "O", gsub("^.+?on\\s+?(.*);$", "\\1", tolower(x[reg])), ");")

  x
}

syntax_categorical_cfa <- function(x, data, center_obs = TRUE, fix_scale = TRUE){
  out <- ""
  obs_vars <- unlist(x)
  if(center_obs){
    center_list <- unlist(x)
    names(center_list) <- paste0("c", center_list)
    out <- c(out,
             gsub("^(c.{3}\\d@)1", "\\10", syntax_cfa_mplus(center_list, fix_mean = T, fix_var = T, fix_first = T, merge = F)))
    indicators <- lapply(x, function(i) paste0("c", i))
  } else {
    indicators <- x
  }
  out <- c(out, syntax_cfa_mplus(indicators, fix_mean = T, fix_var = T, fix_first = F, merge = F))
  if(fix_scale){
    out <- c(out, paste0("{", unlist(x), "@1};\n"))
  }
  num_thresholds <- sapply(unlist(x), function(i){length(table(data[[i]]))})-1
  out <- c(out, unlist(mapply(function(var, thres){
    paste0("[", var, "$", 1:thres, "*];\n")
  }, var = unlist(x), thres = num_thresholds, SIMPLIFY = FALSE)))
  out
}
