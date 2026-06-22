# Tests for table_results.MxModel and table_results.mixture_list
# These tests require OpenMx package

skip_if_not_installed("OpenMx")

library(OpenMx)

# Create a simple OpenMx model for testing
create_test_mx_model <- function() {
  # Simple path model
  data(demoOneFactor, package = "OpenMx")
  
  manifests <- names(demoOneFactor)
  latents <- c("G")
  
  factorModel <- mxModel(
    "One Factor",
    type = "RAM",
    manifestVars = manifests,
    latentVars = latents,
    mxPath(from = latents, to = manifests, values = 0.8, labels = paste0("load_", manifests)),
    mxPath(from = manifests, arrows = 2, values = 0.2, labels = paste0("var_", manifests)),
    mxPath(from = latents, arrows = 2, free = FALSE, values = 1.0),
    mxPath(from = "one", to = manifests, values = 0, labels = paste0("mean_", manifests)),
    mxData(demoOneFactor, type = "raw")
  )
  
  factorModel <- mxRun(factorModel, silent = TRUE)
  return(factorModel)
}

# Helper function to check if any value exists in label column
label_contains <- function(result, patterns) {
  if (!"label" %in% names(result)) return(FALSE)
  any(sapply(patterns, function(p) any(grepl(p, result$label, fixed = TRUE))))
}

test_that("table_results.MxModel works with default arguments", {
  fit <- create_test_mx_model()
  
  result <- table_results(fit)
  
  expect_s3_class(result, "tidy_results")
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_true("label" %in% names(result))
})

test_that("table_results.MxModel respects use_labels = FALSE", {
  fit <- create_test_mx_model()
  
  result <- table_results(fit, use_labels = FALSE)
  
  expect_s3_class(result, "tidy_results")
  expect_true(nrow(result) > 0)
  
  # Check that original variable names are in label column
  expect_true(label_contains(result, c("x1", "x2", "G")))
})

test_that("table_results.MxModel respects use_labels = TRUE without dictionary", {
  fit <- create_test_mx_model()
  
  # Without dictionary, should work but not replace labels
  result <- table_results(fit, use_labels = TRUE)
  
  expect_s3_class(result, "tidy_results")
  expect_true(nrow(result) > 0)
  
  # Original names should still be present since no dictionary provided
  expect_true(label_contains(result, c("x1", "G")))
})

test_that("table_results.MxModel respects use_labels = TRUE with dictionary", {
  fit <- create_test_mx_model()
  
  # Create a dictionary
  dict <- data.frame(
    name = c("x1", "x2", "x3", "x4", "x5", "G"),
    label = c("Item 1", "Item 2", "Item 3", "Item 4", "Item 5", "General Factor"),
    stringsAsFactors = FALSE
  )
  
  result <- table_results(fit, use_labels = TRUE, dictionary = dict)
  
  expect_s3_class(result, "tidy_results")
  expect_true(nrow(result) > 0)
  
  # Check that labels were applied
  expect_true(label_contains(result, c("Item 1", "Item 2", "General Factor")))
})

test_that("table_results.MxModel respects use_labels = TRUE with attached dictionary", {
  fit <- create_test_mx_model()
  
  # Attach dictionary to model
  dict <- data.frame(
    name = c("x1", "x2", "x3", "x4", "x5", "G"),
    label = c("Item 1", "Item 2", "Item 3", "Item 4", "Item 5", "General Factor"),
    stringsAsFactors = FALSE
  )
  attr(fit, "dictionary") <- dict
  
  result <- table_results(fit, use_labels = TRUE)
  
  expect_s3_class(result, "tidy_results")
  expect_true(nrow(result) > 0)
  
  # Check that labels were applied
  expect_true(label_contains(result, c("Item 1", "General Factor")))
})

test_that("table_results.MxModel dictionary argument overrides attached dictionary", {
  fit <- create_test_mx_model()
  
  # Attach one dictionary
  dict_attached <- data.frame(
    name = c("x1", "x2"),
    label = c("Attached 1", "Attached 2"),
    stringsAsFactors = FALSE
  )
  attr(fit, "dictionary") <- dict_attached
  
  # Pass a different dictionary
  dict_arg <- data.frame(
    name = c("x1", "x2"),
    label = c("Argument 1", "Argument 2"),
    stringsAsFactors = FALSE
  )
  
  result <- table_results(fit, use_labels = TRUE, dictionary = dict_arg)
  
  expect_s3_class(result, "tidy_results")
  
  # Check that argument dictionary was used, not attached
  expect_true(label_contains(result, c("Argument 1", "Argument 2")))
  expect_false(label_contains(result, c("Attached 1", "Attached 2")))
})

test_that("table_results.MxModel columns parameter works", {
  fit <- create_test_mx_model()
  
  result <- table_results(fit, columns = c("label", "est", "se", "pval"))
  
  expect_s3_class(result, "tidy_results")
  expect_true("label" %in% names(result))
})

test_that("table_results.MxModel digits parameter works", {
  fit <- create_test_mx_model()
  
  result3 <- table_results(fit, digits = 3, format_numeric = FALSE)
  result5 <- table_results(fit, digits = 5, format_numeric = FALSE)
  
  expect_s3_class(result3, "tidy_results")
  expect_s3_class(result5, "tidy_results")
})

test_that("table_results.MxModel format_numeric parameter works", {
  fit <- create_test_mx_model()
  
  result_formatted <- table_results(fit, format_numeric = TRUE)
  result_raw <- table_results(fit, format_numeric = FALSE)
  
  expect_s3_class(result_formatted, "tidy_results")
  expect_s3_class(result_raw, "tidy_results")
  
  # When format_numeric = FALSE, se should be numeric
  if ("se" %in% names(result_raw)) {
    expect_true(is.numeric(result_raw$se))
  }
})

test_that("table_results.MxModel throws error for unrun model", {
  data(demoOneFactor, package = "OpenMx")
  
  manifests <- names(demoOneFactor)
  latents <- c("G")
  
  factorModel <- mxModel(
    "One Factor",
    type = "RAM",
    manifestVars = manifests,
    latentVars = latents,
    mxPath(from = latents, to = manifests),
    mxPath(from = manifests, arrows = 2),
    mxPath(from = latents, arrows = 2, free = FALSE, values = 1.0),
    mxPath(from = "one", to = manifests),
    mxData(demoOneFactor, type = "raw")
  )
  
  # Model not run yet
  expect_error(table_results(factorModel), "not been run")
})

test_that("table_results.MxModel handles empty dictionary gracefully", {
  fit <- create_test_mx_model()
  
  # Empty dictionary
  dict <- data.frame(
    name = character(0),
    label = character(0),
    stringsAsFactors = FALSE
  )
  
  result <- table_results(fit, use_labels = TRUE, dictionary = dict)
  
  expect_s3_class(result, "tidy_results")
  expect_true(nrow(result) > 0)
})

test_that("table_results.MxModel handles partial dictionary", {
  fit <- create_test_mx_model()
  
  # Dictionary with only some variables
  dict <- data.frame(
    name = c("x1", "G"),
    label = c("Item 1", "General Factor"),
    stringsAsFactors = FALSE
  )
  
  result <- table_results(fit, use_labels = TRUE, dictionary = dict)
  
  expect_s3_class(result, "tidy_results")
  expect_true(nrow(result) > 0)
  
  # At least one label should be present
  expect_true(label_contains(result, c("Item 1", "General Factor")))
  
  # Unlabeled variables should keep original names
  expect_true(label_contains(result, c("x2", "x3")))
})

test_that("table_results.MxModel use_labels = FALSE ignores dictionary", {
  fit <- create_test_mx_model()
  
  dict <- data.frame(
    name = c("x1", "x2", "x3"),
    label = c("Label 1", "Label 2", "Label 3"),
    stringsAsFactors = FALSE
  )
  
  result <- table_results(fit, use_labels = FALSE, dictionary = dict)
  
  expect_s3_class(result, "tidy_results")
  
  # Labels should NOT be present when use_labels = FALSE
  expect_false(label_contains(result, c("Label 1", "Label 2", "Label 3")))
  
  # Original names should be present
  expect_true(label_contains(result, c("x1", "x2", "x3")))
})

# Tests for mixture_list
test_that("table_results.mixture_list works with manually created mixture_list", {
  # Create two separate models and combine into mixture_list
  data(demoOneFactor, package = "OpenMx")
  
  manifests <- names(demoOneFactor)
  latents <- c("G")
  
  # Model 1
  model1 <- mxModel(
    "mix1",
    type = "RAM",
    manifestVars = manifests,
    latentVars = latents,
    mxPath(from = latents, to = manifests, values = 0.8),
    mxPath(from = manifests, arrows = 2, values = 0.2),
    mxPath(from = latents, arrows = 2, free = FALSE, values = 1.0),
    mxPath(from = "one", to = manifests, values = 0),
    mxData(demoOneFactor, type = "raw")
  )
  model1 <- mxRun(model1, silent = TRUE)
  
  # Model 2
  model2 <- mxModel(
    "mix2",
    type = "RAM",
    manifestVars = manifests,
    latentVars = latents,
    mxPath(from = latents, to = manifests, values = 0.8),
    mxPath(from = manifests, arrows = 2, values = 0.2),
    mxPath(from = latents, arrows = 2, free = FALSE, values = 1.0),
    mxPath(from = "one", to = manifests, values = 0),
    mxData(demoOneFactor, type = "raw")
  )
  model2 <- mxRun(model2, silent = TRUE)
  
  # Create mixture_list manually
  mix_list <- list(model1, model2)
  class(mix_list) <- c("mixture_list", "list")
  
  # Test without labels
  result <- table_results(mix_list, use_labels = FALSE)
  expect_s3_class(result, "data.frame")
  expect_true("model" %in% names(result))
  expect_true(nrow(result) > 0)
  
  # Check both models are in results
  expect_true(all(c("mix1", "mix2") %in% result$model))
})

test_that("table_results.mixture_list works with use_labels = TRUE", {
  data(demoOneFactor, package = "OpenMx")
  
  manifests <- names(demoOneFactor)
  latents <- c("G")
  
  # Model 1
  model1 <- mxModel(
    "mix1",
    type = "RAM",
    manifestVars = manifests,
    latentVars = latents,
    mxPath(from = latents, to = manifests, values = 0.8),
    mxPath(from = manifests, arrows = 2, values = 0.2),
    mxPath(from = latents, arrows = 2, free = FALSE, values = 1.0),
    mxPath(from = "one", to = manifests, values = 0),
    mxData(demoOneFactor, type = "raw")
  )
  model1 <- mxRun(model1, silent = TRUE)
  
  # Create mixture_list manually
  mix_list <- list(model1)
  class(mix_list) <- c("mixture_list", "list")
  
  # Test with labels
  dict <- data.frame(
    name = c("x1", "x2", "x3", "x4", "x5", "G"),
    label = c("Item 1", "Item 2", "Item 3", "Item 4", "Item 5", "General Factor"),
    stringsAsFactors = FALSE
  )
  
  result <- table_results(mix_list, use_labels = TRUE, dictionary = dict)
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_true(label_contains(result, c("Item 1", "General Factor")))
})

test_that("table_results.mixture_list passes all parameters correctly", {
  data(demoOneFactor, package = "OpenMx")
  
  manifests <- names(demoOneFactor)
  latents <- c("G")
  
  model1 <- mxModel(
    "mix1",
    type = "RAM",
    manifestVars = manifests,
    latentVars = latents,
    mxPath(from = latents, to = manifests, values = 0.8),
    mxPath(from = manifests, arrows = 2, values = 0.2),
    mxPath(from = latents, arrows = 2, free = FALSE, values = 1.0),
    mxPath(from = "one", to = manifests, values = 0),
    mxData(demoOneFactor, type = "raw")
  )
  model1 <- mxRun(model1, silent = TRUE)
  
  mix_list <- list(model1)
  class(mix_list) <- c("mixture_list", "list")
  
  # Test with various parameters
  result <- table_results(
    mix_list,
    columns = c("label", "est", "se"),
    digits = 3,
    format_numeric = FALSE,
    use_labels = FALSE
  )
  
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  
  # When format_numeric = FALSE, est should be numeric
  if ("est" %in% names(result)) {
    expect_true(is.numeric(result$est))
  }
})

test_that("table_results.mixture_list with attached dictionary", {
  data(demoOneFactor, package = "OpenMx")
  
  manifests <- names(demoOneFactor)
  latents <- c("G")
  
  model1 <- mxModel(
    "mix1",
    type = "RAM",
    manifestVars = manifests,
    latentVars = latents,
    mxPath(from = latents, to = manifests, values = 0.8),
    mxPath(from = manifests, arrows = 2, values = 0.2),
    mxPath(from = latents, arrows = 2, free = FALSE, values = 1.0),
    mxPath(from = "one", to = manifests, values = 0),
    mxData(demoOneFactor, type = "raw")
  )
  model1 <- mxRun(model1, silent = TRUE)
  
  # Attach dictionary to model
  dict <- data.frame(
    name = c("x1", "x2", "G"),
    label = c("Item 1", "Item 2", "General Factor"),
    stringsAsFactors = FALSE
  )
  attr(model1, "dictionary") <- dict
  
  mix_list <- list(model1)
  class(mix_list) <- c("mixture_list", "list")
  
  result <- table_results(mix_list, use_labels = TRUE)
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_true(label_contains(result, c("Item 1", "General Factor")))
})

# Tests for mx_mixture function (requires multiple classes to return mixture_list)
test_that("table_results works with mx_mixture single class output", {
  # Create simple data for mixture model
  set.seed(123)
  df <- data.frame(
    x = c(rnorm(50, mean = 0, sd = 1), rnorm(50, mean = 3, sd = 1))
  )
  
  # Run mx_mixture with single class - returns MxModel, not mixture_list
  mix <- mx_mixture(
    model = "x ~ m{C}*1
             x ~~ v{C}*x",
    classes = 1,
    data = df
  )
  
  # Single class returns MxModel
  expect_true(inherits(mix, "MxModel"))
  
  # Test table_results without labels
  result <- table_results(mix, use_labels = FALSE)
  expect_s3_class(result, "tidy_results")
  expect_true(nrow(result) > 0)
  
  # Test table_results with labels
  dict <- data.frame(
    name = "x",
    label = "Test Variable",
    stringsAsFactors = FALSE
  )
  
  result_labels <- table_results(mix, use_labels = TRUE, dictionary = dict)
  expect_s3_class(result_labels, "tidy_results")
  expect_true(nrow(result_labels) > 0)
  expect_true(label_contains(result_labels, "Test Variable"))
})
