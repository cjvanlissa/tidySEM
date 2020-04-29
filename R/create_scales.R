#' @importFrom psych scoreItems omega
create_scales <- function(data, keys.list, missing = TRUE, impute = "none",
                          omega = NULL, write_files = FALSE,
                          digits = 2, ...)
{
  scoredatanames <- as.vector(unlist(keys.list))
  scoredatanames <- unique(scoredatanames)
  data <- data[, names(data) %in% scoredatanames]
  keys <- make.keys(length(scoredatanames), keys.list = keys.list,
                    item.labels = scoredatanames)
  scores <- scoreItems(keys, data, missing = missing, impute = impute)


  table_descriptives <- data.frame(Subscale = colnames(scores$scores),
                                   Items = unlist(lapply(keys.list, length)), as.matrix(describe(scores$scores))[, c(2, 3, 4, 8, 9)])

  table_descriptives <- cbind(table_descriptives, skew_kurtosis(scores$scores, verbose = FALSE, se = FALSE))

  table_descriptives$Reliability <- as.vector(scores$alpha)
  two_items <- table_descriptives$Items < 3
  if(any(two_items)){
    table_descriptives$Reliability[two_items] <- sapply(keys.list[which(two_items)], function(this_scale){
      spearman_brown(data[this_scale])
    })
  }
  table_descriptives$Interpret = interpret(as.vector(scores$alpha))
  if (!is.null(omega)) {
    omegas <- sapply(names(keys.list), function(scale_name){
      tryCatch(omega(data[keys.list[[scale_name]]])[[omega]],
               error = function(e){warning(e); return(NA)},
               warning = function(w){warning(paste0("When computing Omega for ", scale_name, gsub("^.+?(?=:)", "", w, perl = TRUE)), call. = FALSE); return(NA)})

    })
    table_descriptives <- data.frame(table_descriptives,
                                     Omega = omegas, Interpret.O = interpret(omegas))
  }

  table_descriptives[, sapply(table_descriptives, is.numeric)] <- lapply(table_descriptives[,
                                                                                            sapply(table_descriptives, is.numeric)], formatC, digits = digits,
                                                                         format = "f")

  if(any(two_items)){
    table_descriptives$Reliability[two_items] <- paste0(table_descriptives$Reliability[two_items],
                                                        "(sb)")
  }
  if (write_files)
    write.csv(table_descriptives, "scale table.csv", row.names = FALSE)
  cordat <- data.frame(scores$scores)
  if (missing == FALSE) {
    cordat <- cordat[complete.cases(cordat), ]
  }
  combos <- expand.grid(names(cordat), names(cordat))
  cortab <- matrix(mapply(function(x, y) {
    tmp <- cor.test(cordat[[x]], cordat[[y]])
    paste0(formatC(tmp$estimate, digits = digits, format = "f"),
           ifelse(tmp$p.value < 0.05, "*", ""), ifelse(tmp$p.value <
                                                         0.01, "*", ""), ifelse(tmp$p.value < 0.001,
                                                                                "*", ""))
  }, x = combos$Var1, y = combos$Var2), nrow = ncol(cordat))
  colnames(cortab) <- rownames(cortab) <- names(cordat)
  cortab[upper.tri(cortab)] <- ""
  if (write_files) write.csv(cortab, "correlation table.csv")

  return(list(table_descriptives = table_descriptives, correlations = cortab,
              scores = scores$scores))
}

skew_kurtosis <- function(x, verbose = FALSE, se = FALSE, ...){
  UseMethod("skew_kurtosis", x)
}

skew_kurtosis.data.frame <- function(x, verbose = FALSE, se = FALSE, ...){
  t(sapply(x, skew_kurtosis))
}

skew_kurtosis.matrix <- function(x, verbose = FALSE, se = FALSE, ...){
  t(apply(x, 2, skew_kurtosis))
}

skew_kurtosis.numeric <- function(x, verbose = FALSE, se = FALSE, ...){
  x <- x[!is.na(x)]
  n <- length(x)
  if(n < 3){
    out <- rep(NA, 6)
    names(out) <- c("sk", "se_sk", "sk_2se", "ku", "se_ku", "ku_2se")
    out
  }
  if(n > 5000 & verbose) message("Sample size > 5000; skew and kurtosis will likely be significant.")
  sk <- sum((x-mean(x))^3)/(n*sqrt(var(x))^3)
  se_sk <- sqrt(6*n*(n-1)/(n-2)/(n+1)/(n+3))
  sk_2se <- sk/(2*se_sk)
  ku <- sum((x-mean(x))^4)/(n*var(x)^2) - 3
  se_ku <- sqrt(24*n*((n-1)^2)/(n-3)/(n-2)/(n+3)/(n+5))
  ku_2se <- ku/(2*se_ku)
  c(sk = sk, se_sk = se_sk, sk_2se = sk_2se, ku = ku, se_ku = se_ku, ku_2se = ku_2se)
}

skew_kurtosis.default <- function(x, verbose = FALSE, se = FALSE, ...){
  out <- rep(NA, 6)
  names(out) <- c("sk", "se_sk", "sk_2se", "ku", "se_ku", "ku_2se")
  out
}

interpret <- function(reliability = NULL) {
  interpretation <- rep(NA, length(reliability))
  interpretation[reliability < 0.5] <- "Unacceptable"
  interpretation[reliability >= 0.5] <- "Poor"
  interpretation[reliability >= 0.6] <- "Questionable"
  interpretation[reliability >= 0.7] <- "Acceptable"
  interpretation[reliability >= 0.8] <- "Good"
  interpretation[reliability >= 0.9] <- "Excellent"
  return(interpretation)
}

spearman_brown <- function(x, y = NULL, ...){
  UseMethod("spearman_brown", x)
}

spearman_brown.data.frame <- function(x, y = NULL, ...){
  if(ncol(x) > 2) stop("Spearman Brown is only appropriate as a reliability estimate for two-item scales.")
  cl <- as.list(match.call()[-1])
  cl$x <- x[, 1]
  cl$y <- x[, 2]
  do.call(spearman_brown, cl)
}

spearman_brown.matrix <- spearman_brown.data.frame

spearman_brown.default <- function(x, y, ...){
  r <- cor(x, y, use = "pairwise.complete.obs")
  1/(1+(1/((r/(1-r))+(r/(1-r)))))
}
