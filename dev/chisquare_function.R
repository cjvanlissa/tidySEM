chisq_function <- function(obs,   # obsrved data
                           pred,  # predicted data
                           dof    # degrees of freedom from the LCA model
){
  tab_obs  <- table(obs)
  tab_sim <- table(pred)
  term <- tab_obs*log(tab_obs/tab_sim)
  term[is.infinite(term) | is.nan(term) | is.na(term)] <- 0
  2*sum(term) - dof
}

chisq_function2 <- function(obs,   # obsrved data
                           pred,  # predicted data
                           dof    # degrees of freedom from the LCA model
){

  # Create contingency tables
  tab_obs  <- table(obs)
  tab_sim <- table(pred)

  # Flatten to vectors
  obs_counts  <- as.vector(tab_obs)
  pred_counts <- as.vector(tab_sim)

  # Scale expected counts
  total_obs      <- sum(obs_counts)
  total_pred     <- sum(pred_counts)
  expected_props <- pred_counts / total_pred
  expected_counts<- expected_props * total_obs

  ### calcualte the L2 - chi-square
  ni <- obs_counts
  mi <- expected_counts
  term <- ni*log(ni/mi)
  # Set to zero and NaN Inf terms due to 0 values in Estimated
  term[is.infinite(term)] <- 0
  term[ is.nan(term) ] <- 0
  L2 <- 2*sum(term)
  pv <- 1-pchisq(L2, dof)

  ## Estimated Non‑centrality Parameter - lambda_hat
  lambda_hat <- L2 - dof


  return(lambda_hat)

}

set.seed(1)
res_cat <- readRDS("dev/res_cat.RData")
obs <- res_cat[[1]]$data$observed
pred <- OpenMx::mxGenerateData(res_cat[[2]])
dof <- nrow(obs) - length(coef(res_cat[[2]]))
chisq_function(obs, pred, dof)
chisq_function2(obs, pred, dof)
