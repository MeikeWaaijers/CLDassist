library(testthat)
library(vcr)
library(CLDassist)

# Valid input used in the error tests
direction_df <- data.frame(var1 = c("A", "B"),
                           var2 = c("B", "C"),
                           prob_causal = c(60, 70),
                           prob_var1_cause = c(80, 20),
                           prob_var2_cause = c(10, 90))

test_that("causal_sign() works", {
  skip_on_cran()
  # use the pair with the most likely causal direction, so at least one direction is above the threshold
  dir_df <- CLDassist::dir$direction_df
  best <- which.max(pmax(dir_df$prob_var1_cause, dir_df$prob_var2_cause))

  vcr::use_cassette("causal_sign", {
    sign <- CLDassist::causal_sign(topic = "addiction",
                                   direction_df = dir_df[best, ])
  })

  testthat::expect_true(!is.null(sign))
  testthat::expect_true(is.list(sign))
  testthat::expect_true(all(c("raw_LLM",
                              "sign_df") %in% names(sign)))

  # raw_LLM checks
  testthat::expect_true(is.data.frame(sign$raw_LLM))
  testthat::expect_true(all(c("var",
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
                              "error") %in% names(sign$raw_LLM)))

  # sign_df checks
  testthat::expect_true(is.data.frame(sign$sign_df))
  testthat::expect_true(all(c("var1",
                              "var2",
                              "prob_causal",
                              "prob_var1_cause",
                              "prob_var1_pos",
                              "prob_var1_neg",
                              "prob_var2_cause",
                              "prob_var2_pos",
                              "prob_var2_neg") %in% names(sign$sign_df)))
})

test_that("Invalid topic parameter causes error", {
  testthat::expect_error(CLDassist::causal_sign(topic = 123,
                                                direction_df = direction_df),
                         "'topic' should be a single non-empty character string or NULL.")
})

test_that("Invalid direction_threshold parameter causes error", {
  testthat::expect_error(CLDassist::causal_sign(topic = "Climate Change",
                                                direction_df = direction_df,
                                                direction_threshold = -10),
                         "'direction_threshold' should be a number between 0 and 100")
})

test_that("Invalid direction_df parameter causes error", {
  testthat::expect_error(CLDassist::causal_sign(topic = "Climate Change",
                                                direction_df = "not_a_dataframe"),
                         "'direction_df' should be a dataframe.")
})

test_that("Invalid columns in direction_df cause error", {
  wrong_df <- data.frame(var1 = c("A", "B"), var2 = c("B", "C"), prob_causal = c(60, 70))
  testthat::expect_error(CLDassist::causal_sign(topic = "Climate Change",
                                                direction_df = wrong_df),
                         "'direction_df' should have five columns")
})

test_that("No direction above direction_threshold causes error", {
  testthat::expect_error(CLDassist::causal_sign(topic = "Climate Change",
                                                direction_df = direction_df,
                                                direction_threshold = 95),
                         "At least one variable should have a 'prob_var1_cause' or 'prob_var2_cause' above 'direction_threshold'.")
})

test_that("Invalid LLM_model parameter causes error", {
  testthat::expect_error(CLDassist::causal_sign(topic = "Climate Change",
                                                direction_df = direction_df,
                                                LLM_model = "invalid_model",
                                                max_tokens = 1000),
                         "should be one of")
})

test_that("Invalid max_tokens parameter causes error for gpt-4o", {
  testthat::expect_error(CLDassist::causal_sign(topic = "Climate Change",
                                                direction_df = direction_df,
                                                LLM_model = "gpt-4o",
                                                max_tokens = 7000),
                         "For 'gpt-4o', 'max_tokens' should be a whole number above 0, and not higher than 6000.")
})

test_that("Invalid max_tokens parameter causes error for gpt-3.5-turbo", {
  testthat::expect_error(CLDassist::causal_sign(topic = "Climate Change",
                                                direction_df = direction_df,
                                                LLM_model = "gpt-3.5-turbo",
                                                max_tokens = 4000),
                         "For 'gpt-3.5-turbo', 'max_tokens' should be a whole number above 0, and not higher than 3000.")
})
