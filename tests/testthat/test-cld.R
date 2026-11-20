library(testthat)
library(vcr)
library(CLDassist)

test_that("cld() works", {
  skip_on_cran()
  vcr::use_cassette("cld", {
    output <- CLDassist::cld(topic = "health",
                             variable_list = c("Smoking", "Lung cancer"),
                             plot = TRUE,
                             LLM_model = "gpt-4o",
                             max_tokens = 1000)
  })
  testthat::expect_true(!is.null(output))
  testthat::expect_true(is.list(output))
  testthat::expect_true(all(c("raw_LLM",
                              "sign_df") %in% names(output)))

  # raw_LLM checks
  testthat::expect_true(is.data.frame(output$raw_LLM))
  testthat::expect_true(all(c("which_fun",
                              "var",
                              "relationship",
                              "iteration",
                              "LLM_model",
                              "prompt",
                              "system_prompt",
                              "content",
                              "finish_reason",
                              "prompt_tokens",
                              "answer_tokens",
                              "total_tokens",
                              "error") %in% names(output$raw_LLM)))

  # output checks
  testthat::expect_true(is.data.frame(output$sign_df))
  testthat::expect_true(all(c("var1",
                              "var2",
                              "prob_causal",
                              "prob_var1_cause",
                              "prob_var1_pos",
                              "prob_var1_neg",
                              "prob_var2_cause",
                              "prob_var2_pos",
                              "prob_var2_neg") %in% names(output$sign_df)))

  # edge_list checks
  testthat::expect_true(is.data.frame(output$edge_list))
  testthat::expect_true(all(c("from", "to", "weight", "sign", "color") %in% names(output$edge_list)))

})

test_that("Invalid variable_list parameter causes error", {
  testthat::expect_error(CLDassist::cld(topic = "Climate Change",
                                        variable_list = "not_a_vector",
                                        plot = TRUE,
                                        LLM_model = "gpt-4o",
                                        max_tokens = 1000),
                         "variable_list' should be a vector containing more than one variables.")
})

test_that("Invalid entries in variable_list cause error", {
  testthat::expect_error(CLDassist::cld(topic = "Climate Change",
                                        variable_list = list(123, "Temperature", "Sea Level"),
                                        plot = TRUE,
                                        LLM_model = "gpt-4o",
                                        max_tokens = 1000),
                         "All entries in 'variable_list' should be character strings.")
})

test_that("Invalid plot parameter causes error", {
  testthat::expect_error(CLDassist::cld(topic = "Climate Change",
                                        variable_list = c("CO2", "Temperature", "Sea Level"),
                                        plot = "not_logical",
                                        LLM_model = "gpt-4o",
                                        max_tokens = 1000),
                         "'plot' should be a logical value.")
})

test_that("Invalid LLM_model parameter causes error", {
  testthat::expect_error(CLDassist::cld(topic = "Climate Change",
                                        variable_list = c("CO2", "Temperature", "Sea Level"),
                                        plot = TRUE,
                                        LLM_model = "invalid_model",
                                        max_tokens = 1000),
                         "should be one of")
})

test_that("Invalid max_tokens parameter causes error for gpt-4o", {
  testthat::expect_error(CLDassist::cld(topic = "Climate Change",
                                        variable_list = c("CO2", "Temperature", "Sea Level"),
                                        plot = TRUE,
                                        LLM_model = "gpt-4o",
                                        max_tokens = 7000),
                         "For 'gpt-4o', 'max_tokens' should be a whole number above 0, and not higher than 6000.")
})

test_that("Invalid max_tokens parameter causes error for gpt-3.5-turbo", {
  testthat::expect_error(CLDassist::cld(topic = "Climate Change",
                                        variable_list = c("CO2", "Temperature", "Sea Level"),
                                        plot = TRUE,
                                        LLM_model = "gpt-3.5-turbo",
                                        max_tokens = "4000"),
                         "For 'gpt-3.5-turbo', 'max_tokens' should be a whole number above 0, and not higher than 3000.")
})

test_that("cld() returns the results so far if no causal relationship is found", {
  local_mocked_bindings(
    causal_relation = function(...) list(raw_LLM = data.frame(relationship = 1, LLM_model = "m"),
                                         relation_df = data.frame(var1 = "A", var2 = "B", prob_causal = 30)),
    .package = "CLDassist")
  expect_message(output <- CLDassist::cld(topic = "Test", variable_list = c("A", "B"), plot = FALSE),
                 "direction and sign steps and the plot are skipped")
  expect_true("relation_df" %in% names(output))
  expect_false("direction_df" %in% names(output))
})

test_that("cld() returns the results so far if no direction is found", {
  local_mocked_bindings(
    causal_relation = function(...) list(raw_LLM = data.frame(relationship = 1, LLM_model = "m"),
                                         relation_df = data.frame(var1 = "A", var2 = "B", prob_causal = 80)),
    causal_direction = function(...) list(raw_LLM = data.frame(var = "var1", relationship = 1, LLM_model = "m"),
                                          direction_df = data.frame(var1 = "A", var2 = "B", prob_causal = 80,
                                                                    prob_var1_cause = 40, prob_var2_cause = 10)),
    .package = "CLDassist")
  expect_message(output <- CLDassist::cld(topic = "Test", variable_list = c("A", "B"), plot = FALSE),
                 "sign step and the plot are skipped")
  expect_true("direction_df" %in% names(output))
})
