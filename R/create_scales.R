#' @importFrom psych scoreItems
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

  table_descriptives$Alpha <- as.vector(scores$alpha)
  table_descriptives$Interpret.a = interpret(as.vector(scores$alpha))
  if (!is.null(omega)) {
    omegas <- unlist(sapply(1:length(keys.list), function(x) {
      omega(data[keys.list[[x]]])[[omega]]
    }))
    table_descriptives <- data.frame(table_descriptives,
                                     Omega = omegas, Interpret.O = interpret(omegas))
  }

  table_descriptives[, sapply(table_descriptives, is.numeric)] <- lapply(table_descriptives[,
                                                                                            sapply(table_descriptives, is.numeric)], formatC, digits = digits,
                                                                         format = "f")

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
  interpretation <- rep("Unacceptable", length(reliability))
  interpretation[reliability >= 0.5] <- "Poor"
  interpretation[reliability >= 0.6] <- "Questionable"
  interpretation[reliability >= 0.7] <- "Acceptable"
  interpretation[reliability >= 0.8] <- "Good"
  interpretation[reliability >= 0.9] <- "Excellent"
  return(interpretation)
}
