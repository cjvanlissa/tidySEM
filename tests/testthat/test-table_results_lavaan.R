# Tests for table_results.lavaan

skip_if_not_installed("lavaan")

library(lavaan)

# Create a simple lavaan model for testing
create_test_lavaan_model <- function() {
  model <- '
    # latent variable definitions
    visual  =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
    speed   =~ x7 + x8 + x9
  '
  
  fit <- cfa(model, data = HolzingerSwineford1939)
  return(fit)
}

# Helper function to check if any value exists in label column
label_contains <- function(result, patterns) {
  if (!"label" %in% names(result)) return(FALSE)
  any(sapply(patterns, function(p) any(grepl(p, result$label, fixed = TRUE))))
}

test_that("table_results.lavaan works with default arguments", {
  fit <- create_test_lavaan_model()
  
  result <- table_results(fit)
  
  expect_s3_class(result, "tidy_results")
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_true("label" %in% names(result))
})

test_that("table_results.lavaan respects use_labels = FALSE", {
  fit <- create_test_lavaan_model()
  
  result <- table_results(fit, use_labels = FALSE)
  
  expect_s3_class(result, "tidy_results")
  expect_true(nrow(result) > 0)
  
  # Check that original variable names are in label column
  expect_true(label_contains(result, c("x1", "x2", "x3", "visual", "textual", "speed")))
})

test_that("table_results.lavaan respects use_labels = TRUE without dictionary", {
  fit <- create_test_lavaan_model()
  
  # Without dictionary, should work but not replace labels
  result <- table_results(fit, use_labels = TRUE)
  
  expect_s3_class(result, "tidy_results")
  expect_true(nrow(result) > 0)
  
  # Original names should still be present since no dictionary provided
  expect_true(label_contains(result, c("x1", "visual")))
})

test_that("table_results.lavaan respects use_labels = TRUE with dictionary", {
  fit <- create_test_lavaan_model()
  
  # Create a dictionary
  dict <- data.frame(
    name = c("x1", "x2", "x3", "visual", "textual", "speed"),
    label = c("Item 1", "Item 2", "Item 3", "Visual", "Textual", "Speed"),
    stringsAsFactors = FALSE
  )
  
  result <- table_results(fit, use_labels = TRUE, dictionary = dict)
  
  expect_s3_class(result, "tidy_results")
  expect_true(nrow(result) > 0)
  
  # Check that labels were applied
  expect_true(label_contains(result, c("Item 1", "Item 2", "Visual", "Textual")))
})

test_that("table_results.lavaan respects use_labels = TRUE with attached dictionary", {
  fit <- create_test_lavaan_model()
  
  # Attach dictionary to model
  dict <- data.frame(
    name = c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9",
             "visual", "textual", "speed"),
    label = c("Item 1", "Item 2", "Item 3", "Item 4", "Item 5", "Item 6",
              "Item 7", "Item 8", "Item 9", "Visual", "Textual", "Speed"),
    stringsAsFactors = FALSE
  )
  attr(fit, "dictionary") <- dict
  
  result <- table_results(fit, use_labels = TRUE)
  
  expect_s3_class(result, "tidy_results")
  expect_true(nrow(result) > 0)
  
  # Check that labels were applied
  expect_true(label_contains(result, c("Item 1", "Visual")))
})

test_that("table_results.lavaan dictionary argument overrides attached dictionary", {
  fit <- create_test_lavaan_model()
  
  # Attach one dictionary
  dict_attached <- data.frame(
    name = c("x1", "x2", "x3"),
    label = c("Attached 1", "Attached 2", "Attached 3"),
    stringsAsFactors = FALSE
  )
  attr(fit, "dictionary") <- dict_attached
  
  # Pass a different dictionary
  dict_arg <- data.frame(
    name = c("x1", "x2", "x3"),
    label = c("Argument 1", "Argument 2", "Argument 3"),
    stringsAsFactors = FALSE
  )
  
  result <- table_results(fit, use_labels = TRUE, dictionary = dict_arg)
  
  expect_s3_class(result, "tidy_results")
  
  # Check that argument dictionary was used, not attached
  expect_true(label_contains(result, c("Argument 1", "Argument 2")))
  expect_false(label_contains(result, c("Attached 1", "Attached 2")))
})

test_that("table_results.lavaan columns parameter works", {
  fit <- create_test_lavaan_model()
  
  result <- table_results(fit, columns = c("label", "est", "se", "pval"))
  
  expect_s3_class(result, "tidy_results")
  expect_true("label" %in% names(result))
})

test_that("table_results.lavaan digits parameter works", {
  fit <- create_test_lavaan_model()
  
  result3 <- table_results(fit, digits = 3, format_numeric = FALSE)
  result5 <- table_results(fit, digits = 5, format_numeric = FALSE)
  
  expect_s3_class(result3, "tidy_results")
  expect_s3_class(result5, "tidy_results")
})

test_that("table_results.lavaan format_numeric parameter works", {
  fit <- create_test_lavaan_model()
  
  result_formatted <- table_results(fit, format_numeric = TRUE)
  result_raw <- table_results(fit, format_numeric = FALSE)
  
  expect_s3_class(result_formatted, "tidy_results")
  expect_s3_class(result_raw, "tidy_results")
  
  # Find a numeric column in raw result (e.g., se or pval)
  if ("se" %in% names(result_raw)) {
    expect_true(is.numeric(result_raw$se))
  }
})

test_that("table_results.lavaan works with multigroup model", {
  model <- '
    visual  =~ x1 + x2 + x3
  '
  
  fit <- cfa(model, data = HolzingerSwineford1939, group = "school")
  
  result <- table_results(fit)
  
  expect_s3_class(result, "tidy_results")
  expect_true(nrow(result) > 0)
})

test_that("table_results.lavaan works with multigroup model and use_labels", {
  model <- '
    visual  =~ x1 + x2 + x3
  '
  
  fit <- cfa(model, data = HolzingerSwineford1939, group = "school")
  
  dict <- data.frame(
    name = c("x1", "x2", "x3", "visual"),
    label = c("Item 1", "Item 2", "Item 3", "Visual"),
    stringsAsFactors = FALSE
  )
  
  result <- table_results(fit, use_labels = TRUE, dictionary = dict)
  
  expect_s3_class(result, "tidy_results")
  expect_true(nrow(result) > 0)
  expect_true(label_contains(result, c("Item 1", "Visual")))
})

test_that("table_results.lavaan works with SEM model", {
  model <- '
    # measurement model
    visual  =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
    
    # regression
    textual ~ visual
  '
  
  fit <- sem(model, data = HolzingerSwineford1939)
  
  result <- table_results(fit)
  
  expect_s3_class(result, "tidy_results")
  expect_true(nrow(result) > 0)
})

test_that("table_results.lavaan handles empty dictionary gracefully", {
  fit <- create_test_lavaan_model()
  
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

test_that("table_results.lavaan handles partial dictionary", {
  fit <- create_test_lavaan_model()
  
  # Dictionary with only some variables
  dict <- data.frame(
    name = c("x1", "visual"),
    label = c("Item 1", "Visual Factor"),
    stringsAsFactors = FALSE
  )
  
  result <- table_results(fit, use_labels = TRUE, dictionary = dict)
  
  expect_s3_class(result, "tidy_results")
  expect_true(nrow(result) > 0)
  
  # At least one label should be present
  expect_true(label_contains(result, c("Item 1", "Visual Factor")))
  
  # Unlabeled variables should keep original names
  expect_true(label_contains(result, c("x2", "textual")))
})

test_that("table_results.lavaan use_labels = FALSE ignores dictionary", {
  fit <- create_test_lavaan_model()
  
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