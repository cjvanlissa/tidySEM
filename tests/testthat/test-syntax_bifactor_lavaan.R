
test_that("Bifactor gives highly similar results in lavaan, psych, and openMx", {
  # psych::omegaFromSem and omega.lavaan are almost in agreement
  # The differences with openMx are a bit larger but below 0.02
  error_margin <- 0.02

  # data(Bechtoldt, Bechtoldt.1, Bechtoldt.2, Holzinger, Holzinger.9, Reise, Thurstone, Thurstone.33, Thurstone.33G, Thurstone.9, package = "psych")

  datasets_psych <- data(package = "psych")$results

  datasets_psych_bifactor <- data.frame(dataset_name = c("Bechtoldt.1", "Bechtoldt.2", "Holzinger", "Holzinger.9",
                                                         "Reise", "Thurstone", "Thurstone.33", "Thurstone.33G", "Thurstone.9"
  ), nfactors = c(6, 6, 4, NA, 5, 3, NA, NA, 3))

  # psych::Harman.Holzinger, 3

  create_bifactor_model <- function(x) {
    bifactor(tidy_sem(x), generalFactorName = "Z")
  }

  create_efa_based_varnames <- function(m, nfactors = 3) {
    loadings <- psych::omega(m = m, nfactors = nfactors, plot = F)$schmid$sl[,c(2:(1+nfactors))]
    allocations <- unlist(apply(loadings, 1, function(x) which(x == max(abs(x))), simplify = T))

    new_names <- matrix(NA_character_, nrow = nrow(loadings), ncol = 1, dimnames = list(
      rownames(m),
      "new_name"
    ))

    for(f in 1:nfactors) {
      w <- which(allocations == f)
      for(i in 1:length(w)) {
        old_name <- names(w[i])
        new_name <- paste0(letters[f], "_", i)
        new_names[old_name,] <- new_name
      }
    }

    new_names

  }

  create_dataset_from_m <- function(m) {
    x <- as.data.frame(MASS::mvrnorm(n = 100, mu = rep(0, ncol(m)), Sigma = m, empirical = T))
    names(x) <- colnames(m)

    x
  }

  run_lavaan_model <- function(m, bf) {

    # TODO: test cross loadings?
    f <- run_lavaan(bf)

    # Do a trick to make psych use a correlation matrix
    update(f, data = NULL, sample.nobs = 100, sample.cov = m, sample.mean = rep(0, ncol(m)))
  }


  results_psych_data <- data.frame(package = "psych",
                                   dataset = rep(NA_character_, length(datasets_psych_bifactor$dataset_name)),
                                   dataset_number = NA_integer_,
                                   a_b = NA_real_,
                                   a_c = NA_real_,
                                   b_c = NA_real_,
                                   max_error_margin = NA_real_,
                                   row.names =
                                     datasets_psych_bifactor$dataset_name
  )

  for(i in 1:nrow(datasets_psych_bifactor)) {

    # How seed sensitive is this?
    # set.seed(1337)

    dataset_name <- datasets_psych_bifactor[i,"dataset_name"]
    nfactors <- datasets_psych_bifactor[i,"nfactors"]

    if (!is.na(nfactors)) {

      # cat("Modelling: #", i, " ", dataset_name, "\n")

      e <- new.env()
      data(list = dataset_name, package = "psych", envir = e)
      m <- e[[dataset_name]]


      new_names <- create_efa_based_varnames(m, nfactors = nfactors)


      rownames(m) <- new_names[rownames(m),]
      colnames(m) <- new_names[colnames(m),]


      x <- create_dataset_from_m(m)

      bf <- create_bifactor_model(x)

      f_lavaan <- run_lavaan_model(m, bf)
      f_openmx <- run_mx(bf)

      a_psych <- psych::omegaFromSem(f_lavaan, plot = F)$omega.group
      astd <- a_psych[order(a_psych[,1], decreasing = T),]

      # At this point I can safely say that the standardization_method "psych"
      # together with total_variance_calculation = "observed"
      # is identical to psych::omegaFromSem
      b <- omega(f_lavaan, standardization_method = "psych", total_variance_calculation = "observed")[,-4]
      bstd <- b[order(b[,1], decreasing = T),]

      mx <- omega(f_openmx, total_variance_calculation = "observed")[,-4]
      cstd <- mx[order(mx[,1], decreasing = T),]

      a_b <- abs(astd - bstd)
      a_c <- abs(astd - cstd)
      b_c <- abs(bstd - cstd)

      perc_a_b <- sum(a_b < error_margin) / length(a_b)
      perc_a_c <- sum(a_c < error_margin) / length(a_c)
      perc_b_c <- sum(b_c < error_margin) / length(b_c)

      # print(paste(perc_a_b, perc_a_c, perc_b_c, sep = "; "))

      results_psych_data[dataset_name,"package"] <- "psych"
      results_psych_data[dataset_name,"dataset"] <- dataset_name
      results_psych_data[dataset_name,"dataset_number"] <- i
      results_psych_data[dataset_name,"a_b"] <- max(a_b)
      results_psych_data[dataset_name,"a_c"] <- max(a_c)
      results_psych_data[dataset_name,"b_c"] <- max(b_c)
      results_psych_data[dataset_name,"max_error_margin"] <- max(c(as.matrix(a_b), as.matrix(a_c), as.matrix(b_c)))
    }

  }

  expect_true(all(na.omit(results_psych_data$max_error_margin < error_margin)))
})

