entropy <- function(post_prob) {
  K <- ncol(post_prob)
  if (K == 1L) return(1)

  terms <- post_prob * log(post_prob)
  terms[post_prob == 0] <- 0

  1 + sum(terms) / (nrow(post_prob) * log(K))
}
