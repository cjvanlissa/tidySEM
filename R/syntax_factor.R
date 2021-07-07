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


syntax_cor_lavaan <- function(x, y = x, all = TRUE, label = TRUE, generic_label = FALSE){
  if(all){
    cors <- c(x, y)
    cors <- unique(cors)
    cors <- t(combn(cors, 2))
    if(label){
      if(!generic_label){
        cors <- cbind(cors, " ~~ c", "*")[, c(1, 3, 1,2,4,2)]
      } else {
        cors <- cbind(cors, " ~~ ", paste0("c", 1:nrow(cors)), "*")[, c(1, 3, 4, 5, 2)]
      }

    }
    unname(apply(cors, 1, paste0, collapse = ""))
  } else {
    if(label){
      paste0(x, " ~~ c", x, y, "*", y)
    } else {
      paste0(x, " ~~ ", y)
    }
  }
}


syntax_cor_lav2 <- function(x, y = NULL, all = TRUE, label = TRUE){
  if(is.null(y)){
    y <- x
  }
  if(all){
    m <- matrix(paste0(rep(y, each = length(x)), " ~~ ", c(paste0("c", 1:((length(x)*(length(y)-1))/2)), paste0("v", 1:length(x)), paste0("c", 1:((length(x)*(length(y)-1))/2))), rep(x, length(x)), ";\n"), ncol = length(x))
    m[upper.tri(m)]
    #apply(expand.grid(x, " WITH ", y, ";\n"), 1, paste, collapse = "")
  } else {
    paste(x, " WITH ", y, ";\n", collapse = "", sep = "")
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

