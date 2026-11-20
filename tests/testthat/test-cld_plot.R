library(testthat)
library(CLDassist)

# Example dataframes for testing
valid_relation_df <- data.frame(var1 = c("A", "B"), var2 = c("B", "C"), prob_causal = c(60, 70))
invalid_relation_df <- data.frame(var1 = c("A", "B"), var2 = c("B", "C"), prob_causal = c(60, -70))

valid_direction_df <- data.frame(var1 = c("A", "B"), var2 = c("B", "C"), prob_causal = c(60, 70),
                                 prob_var1_cause = c(80, 20), prob_var2_cause = c(20, 80))

valid_sign_df <- data.frame(var1 = c("A", "B"), var2 = c("B", "C"), prob_causal = c(60, 70),
                            prob_var1_cause = c(80, 20), prob_var1_pos = c(80, 20), prob_var1_neg = c(20, 80),
                            prob_var2_cause = c(20, 80), prob_var2_pos = c(20, 80), prob_var2_neg = c(80, 20))


test_that("cld_plot() works with all three dataframes", {
  output <- CLDassist::cld_plot(topic = "Test topic",
                                relation_df = valid_relation_df,
                                direction_df = valid_direction_df,
                                sign_df = valid_sign_df)
  testthat::expect_true(is.list(output))
  testthat::expect_true(all(c("rel_edge_list",
                              "dir_edge_list",
                              "sign_edge_list") %in% names(output)))
  testthat::expect_true(all(c("from", "to", "weight") %in% names(output$rel_edge_list)))
  testthat::expect_true(all(c("from", "to", "weight") %in% names(output$dir_edge_list)))
  testthat::expect_true(all(c("from", "to", "weight", "sign", "color") %in% names(output$sign_edge_list)))
})

test_that("cld_plot() works without plotting", {
  output <- CLDassist::cld_plot(topic = "Test topic",
                                sign_df = valid_sign_df,
                                plot = FALSE,
                                legend = FALSE)
  testthat::expect_true(is.list(output))
  testthat::expect_true("sign_edge_list" %in% names(output))
})

test_that("cld_plot() works with only a relation dataframe", {
  output <- CLDassist::cld_plot(topic = "Test topic",
                                relation_df = valid_relation_df)
  testthat::expect_true(is.list(output))
  testthat::expect_true("rel_edge_list" %in% names(output))
})

test_that("cld_plot() works with only a direction dataframe", {
  output <- CLDassist::cld_plot(topic = "Test topic",
                                direction_df = valid_direction_df)
  testthat::expect_true(is.list(output))
  testthat::expect_true("dir_edge_list" %in% names(output))
})

test_that("cld_plot() works with only a sign dataframe", {
  output <- CLDassist::cld_plot(topic = "Test topic",
                                sign_df = valid_sign_df)
  testthat::expect_true(is.list(output))
  testthat::expect_true("sign_edge_list" %in% names(output))
})

test_that("Combine parameter is handled", {
  output <- CLDassist::cld_plot(topic = "Test topic",
                                direction_df = valid_direction_df,
                                combine = FALSE)
  testthat::expect_true(is.list(output))
  testthat::expect_true("dir_edge_list" %in% names(output))
})

test_that("Null topic is handled", {
  output <- CLDassist::cld_plot(topic = NULL,
                                relation_df = valid_relation_df)
  testthat::expect_true(is.list(output))
  testthat::expect_true("rel_edge_list" %in% names(output))
})

test_that("No dataframe causes error", {
  testthat::expect_error(CLDassist::cld_plot(topic = "Test topic"),
                         "At least one dataframe should be inputted in the function.")
})

test_that("Invalid relation_df causes error", {
  testthat::expect_error(CLDassist::cld_plot(topic = "Test topic",
                                             relation_df = invalid_relation_df),
                         "All entries in 'relation_df\\$prob_causal' should be numeric and between 0 and 100.")
})

test_that("Invalid columns in sign_df cause error", {
  testthat::expect_error(CLDassist::cld_plot(topic = "Test topic",
                                             sign_df = valid_direction_df),
                         "'sign_df' should have nine columns")
})

test_that("Invalid plot parameter causes error", {
  testthat::expect_error(CLDassist::cld_plot(topic = "Test topic",
                                             relation_df = valid_relation_df,
                                             plot = "not_logical"),
                         "'plot' should be a logical value.")
})

test_that("Layout parameter validation", {
  testthat::expect_error(CLDassist::cld_plot(topic = "Test topic",
                                             relation_df = valid_relation_df,
                                             layout = "not_valid_layout"),
                         "'layout' should be either 'average' or 'circle'.")
})
