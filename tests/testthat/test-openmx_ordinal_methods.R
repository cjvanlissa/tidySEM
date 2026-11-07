if(requireNamespace("OpenMx", quietly = TRUE) & requireNamespace("umx", quietly = TRUE)){
  tidySEM:::skip_if_not_local()

      require("OpenMx")
      require("umx")

      make_ordinal_mixture <- function(df, method = c("tidySEM", "OpenMx")) {

        method <- match.arg(method)

        make_class <- function(k) {

          args <- list(
            model = paste0("class", k),
            type = "RAM",
            manifestVars = names(df),
            OpenMx::mxPath(
              from = "one",
              to = names(df),
              free = FALSE,
              values = 0
            ),
            OpenMx::mxPath(
              from = names(df),
              to = names(df),
              free = FALSE,
              values = 1,
              arrows = 2
            )
          )

          if (method == "tidySEM") {
            args <- c(
              args,
              list(tidySEM:::mx_thresholds(df))
            )
          } else {
            args <- c(
              args,
              list(OpenMx::mxThreshold(
                vars = names(df),
                nThresh = vapply(df, nlevels, integer(1)) - 1L,
                free = TRUE
              ))
            )
          }

          args <- c(
            args,
            list(OpenMx::mxFitFunctionML(vector = TRUE))
          )

          model <- do.call(OpenMx::mxModel, args)

          if (method == "tidySEM") {
            model@expectation$thresholds <- "Thresholds"
          }

          model
        }

        classes <- lapply(1:2, make_class)

        # Distinguish unlabeled parameters across classes
        for (k in seq_along(classes)) {
          classes[[k]] <- tidySEM:::label_auxiliary_parameters(
            classes[[k]], k
          )
        }

        OpenMx::mxModel(
          "mix",
          classes[[1]],
          classes[[2]],
          OpenMx::mxData(df, type = "raw"),
          OpenMx::mxMatrix(
            type = "Full",
            values = matrix(c(0.5, 0.5), 1, 2),
            free = matrix(c(FALSE, TRUE), 1, 2),
            lbound = 1e-4,
            name = "weights"
          ),
          OpenMx::mxExpectationMixture(
            paste0("class", 1:2),
            weights = "weights",
            scale = "sum"
          ),
          OpenMx::mxFitFunctionML()
        )
      }


      test_that("mx_thresholds constructs valid ordinal thresholds", {

        skip_if_not_installed("OpenMx")

        df <- data_mix_ordinal
        df[] <- lapply(df, ordered)

        thresholds <- tidySEM:::mx_thresholds(df)

        expect_true(is.list(thresholds))

        # Construct a single-class model
        model <- OpenMx::mxModel(
          "ordinal",
          type = "RAM",
          manifestVars = names(df),
          OpenMx::mxPath(
            from = "one",
            to = names(df),
            free = FALSE,
            values = 0
          ),
          OpenMx::mxPath(
            from = names(df),
            to = names(df),
            free = FALSE,
            values = 1,
            arrows = 2
          ),
          thresholds,
          OpenMx::mxData(df, type = "raw"),
          OpenMx::mxFitFunctionML()
        )

        # Threshold algebra exists
        expect_true("Thresholds" %in% names(model@algebras))

        # Expected number of thresholds
        expected <- vapply(df, nlevels, integer(1)) - 1L

        th <- OpenMx::mxEvalByName(
          "Thresholds",
          model,
          compute = TRUE
        )

        expect_equal(ncol(th), ncol(df))
        expect_equal(nrow(th), max(expected))

        # All active thresholds are finite and increasing
        for (j in seq_along(expected)) {

          values <- th[seq_len(expected[j]), j]

          expect_true(all(is.finite(values)))
          expect_true(all(diff(values) > 0))
        }
      })

      test_that("ordinal threshold parameterizations give equivalent likelihoods", {

        skip_if_not_installed("OpenMx")

        df <- data_mix_ordinal
        df[] <- lapply(df, ordered)

        # Construct models
        model_tidy <- make_ordinal_mixture(df, "tidySEM")
        model_mx <- make_ordinal_mixture(df, "OpenMx")

        # Copy the evaluated thresholds into the OpenMx model
        for (k in 1:2) {

          class_name <- paste0("class", k)

          th <- OpenMx::mxEvalByName(
            paste0(class_name, ".Thresholds"),
            model_tidy,
            compute = TRUE
          )

          model_mx[[class_name]]$Thresholds$values[,] <- th
        }

        # Evaluate both likelihoods without optimization
        fit_tidy <- OpenMx::mxModel(
          model_tidy,
          OpenMx::mxComputeSequence(list(
            OpenMx::mxComputeOnce("fitfunction", "fit")
          ))
        )

        fit_mx <- OpenMx::mxModel(
          model_mx,
          OpenMx::mxComputeSequence(list(
            OpenMx::mxComputeOnce("fitfunction", "fit")
          ))
        )

        fit_tidy <- OpenMx::mxRun(fit_tidy, silent = TRUE)
        fit_mx <- OpenMx::mxRun(fit_mx, silent = TRUE)

        expect_equal(
          as.numeric(fit_tidy@output$fit),
          as.numeric(fit_mx@output$fit),
          tolerance = 1e-6
        )
      })


      test_that("ordinal threshold parameterizations give equivalent likelihoods", {

        skip_if_not_installed("OpenMx")

        df <- data_mix_ordinal
        df[] <- lapply(df, ordered)

        # Construct models
        model_tidy <- make_ordinal_mixture(df, "tidySEM")
        model_mx <- make_ordinal_mixture(df, "OpenMx")

        # Copy the evaluated thresholds into the OpenMx model
        for (k in 1:2) {

          class_name <- paste0("class", k)

          th <- OpenMx::mxEvalByName(
            paste0(class_name, ".Thresholds"),
            model_tidy,
            compute = TRUE
          )

          model_mx[[class_name]]$Thresholds$values[,] <- th
        }

        # Evaluate both likelihoods without optimization
        fit_tidy <- OpenMx::mxModel(
          model_tidy,
          OpenMx::mxComputeSequence(list(
            OpenMx::mxComputeOnce("fitfunction", "fit")
          ))
        )

        fit_mx <- OpenMx::mxModel(
          model_mx,
          OpenMx::mxComputeSequence(list(
            OpenMx::mxComputeOnce("fitfunction", "fit")
          ))
        )

        fit_tidy <- OpenMx::mxRun(fit_tidy, silent = TRUE)
        fit_mx <- OpenMx::mxRun(fit_mx, silent = TRUE)

        expect_equal(
          as.numeric(fit_tidy@output$fit),
          as.numeric(fit_mx@output$fit),
          tolerance = 1e-6
        )
      })

      test_that("table_results extracts ordinal mixture thresholds", {

        skip_if_not_installed("OpenMx")

        tidySEM:::skip_if_not_local()

        set.seed(123)

        df <- data_mix_ordinal
        df[] <- lapply(df, ordered)

        model <- make_ordinal_mixture(df, "tidySEM")

        fit <- OpenMx::mxRun(
          model,
          silent = TRUE
        )
        fit <- OpenMx::mxTryHardOrdinal(fit)
        attr(fit, "tidySEM") <- "mixture"

        tab <- table_results(
          fit,
          columns = NULL,
          format_numeric = FALSE
        )

        expect_true("Thresholds" %in% tab$matrix)

        th <- tab[
          tab$matrix == "Thresholds",
          ,
          drop = FALSE
        ]

        expect_gt(nrow(th), 0L)
        expect_true(all(is.finite(th$est)))

        # The threshold operator should be represented as |
        expect_true(all(th$op == "|"))
      })

}
