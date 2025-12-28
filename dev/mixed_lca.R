df <- iris[, c(1,2,5)]
df <- data.frame(model.matrix(~., df)[, c(2:4)])
names(df) <- paste0("x", 1:3)
df$x3 <- OpenMx::mxFactor(df$x3, levels = c(0,1))
library(tidySEM)
library(OpenMx)
tidySEM::mx_mixture(model =
"
x1 ~ mx1{C}*1
x2 ~ mx2{C}*1
x1 ~~ vx1*x1
x2 ~~ vx2*x2
x3 | tx3{C}*t1", classes = c(1, 2), data = df, run = FALSE
) -> res

res[[1]]$Thresholds

tmp <- mxRun(res[[1]])
tmp2 <- mxRun(res[[2]])
table_results(tmp)
table_results(tmp2)


res_lca <- mx_lca(df[, "x3", drop = FALSE], classes = c(1,2))
res_lpa <- mx_profiles(df[, c(1:2), drop = FALSE], classes = c(1,2))

res_starts <- res
res_starts[[2]]$class1$M$values[1:2] <- res_lpa[[2]]$class1$M$values[1:2]
res_starts[[2]]$class2$M$values[1:2] <- res_lpa[[2]]$class2$M$values[1:2]

diag(res_starts[[2]]$class1$S$values)[1:2] <- diag(res_lpa[[2]]$class1$S$values)[1:2]
diag(res_starts[[2]]$class2$S$values)[1:2] <- diag(res_lpa[[2]]$class1$S$values)[1:2]

res_starts[[2]][["class1"]][["Thresholds"]] <- NULL
res_starts[[2]][["class2"]][["Thresholds"]] <- NULL

for(m in c("mat_dev", "mat_ones", "Thresholds")){
  res_starts[[2]][["class1"]][[m]] <- res_lca[[2]][["class1"]][[m]]
}

res_final <- mxRun(res_starts[[2]])

res_starts[[2]]$class1$mat_de mvalues[1,1] <- res_lca[[2]]$class1$mat_dev Thresholds$values[1,1]
res_starts[[2]]$class2$Thresholds$values[1,1] <- res_lca[[2]]$class2$Thresholds$values[1,1]

table_results(res_lca[[1]])





df <- data_mix_ordinal
df[1:4] <- lapply(df, mxFactor, levels = c(0,1,2))
mx_lca(data = df,
       classes = 2) -> res

res_mixture <- mx_mixture(model = paste0(sapply(1:4, function(i){glue::glue("u{i} | tu{i}1{{C}}*t1; u{i} | tu{i}2{{C}}*t2", )}), collapse = "; "),
            data = df, classes = 2, run = FALSE)



mx_mixed_lca <- function(data = NULL,
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

