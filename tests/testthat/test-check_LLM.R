library(testthat)
library(CLDassist)

# These tests replace the API call (.call_llm) with fake responses, so no API key is needed

fake_response <- function(tokens, probs, content = tokens[1], error = NULL) {
  list(top5_tokens = if (length(tokens) == 0) list() else
         list(data.frame(top5_tokens = tokens, logprob = log(probs / 100), probability = probs)),
       output = content,
       raw_content = list(content = content, finish_reason = "stop", error = error))
}

test_that("check_llm() accepts a suitable model", {
  local_mocked_bindings(.call_llm = function(prompt, ...) {
    if (grepl("Smoking", prompt)) fake_response(c("C", "N"), c(98, 2)) else fake_response(c("N", "C"), c(95, 5))
  })
  res <- suppressMessages(check_llm("openai/gpt-4o"))
  expect_true(res$suitable)
  expect_true(is.na(res$problem))
  expect_equal(res$tests$answer, c("C", "N"))
  expect_equal(res$tests$probability, c(98, 95))
})

test_that("check_llm() sums token variants", {
  local_mocked_bindings(.call_llm = function(prompt, ...) fake_response(c("C", " c", "N"), c(90, 5, 5)))
  res <- suppressMessages(check_llm("openai/gpt-4o"))
  expect_equal(res$tests$probability[1], 95)
})

test_that("check_llm() detects missing logprobs", {
  local_mocked_bindings(.call_llm = function(...) fake_response(character(), numeric(), content = "C"))
  res <- suppressMessages(check_llm("openai/gpt-4o"))
  expect_false(res$suitable)
  expect_equal(res$problem, "No log probabilities returned")
})

test_that("check_llm() detects when only the chosen token is returned", {
  local_mocked_bindings(.call_llm = function(...) fake_response("C", 99))
  res <- suppressMessages(check_llm("openai/gpt-4o"))
  expect_false(res$suitable)
  expect_equal(res$problem, "Only the chosen token returned (no alternatives)")
})

test_that("check_llm() detects empty answers (thinking)", {
  local_mocked_bindings(.call_llm = function(...) fake_response(character(), numeric(), content = ""))
  res <- suppressMessages(check_llm("openai/gpt-4o"))
  expect_equal(res$problem, "Empty answer")
})

test_that("check_llm() detects answers that are not a C/N token", {
  local_mocked_bindings(.call_llm = function(...) fake_response(c("The", "C"), c(80, 20), content = "The answer is C"))
  res <- suppressMessages(check_llm("openai/gpt-4o"))
  expect_equal(res$problem, "Answer is not a single C/N token")
})

test_that("check_llm() reports API errors with a hint", {
  local_mocked_bindings(.call_llm = function(...) fake_response(character(), numeric(), content = NULL,
                                                                error = list(message = "Incorrect API key provided")))
  expect_message(res <- check_llm("openai/gpt-4o"), "update_key = TRUE")
  expect_equal(res$problem, "API call failed")
})

test_that("check_llm() flags unexpected answers but still accepts the model", {
  local_mocked_bindings(.call_llm = function(...) fake_response(c("C", "N"), c(60, 40)))
  expect_message(res <- check_llm("openai/gpt-4o"), "unexpected answers")
  expect_true(res$suitable)
})

test_that("check_llm() checks its arguments", {
  expect_error(check_llm(LLM_model = 123))
  expect_error(check_llm(LLM_model = "unknownprovider/model"))
  expect_error(check_llm(update_key = "yes"))
})
