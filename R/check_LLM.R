# Program Name: CLDassist
# Description: This function checks whether a Large Language Model (LLM) can be used with CLDassist.
# Copyright (C) <2024> <Meike Waaijers>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
# See the GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program. If not, see <https://www.gnu.org/licenses/>.


#### CLDassist

#' Check whether a Large Language Model can be used with CLDassist
#'
#' @description
#' CLDassist reads the probability of each answer from the log probabilities (logprobs)
#' of the first answer token. Not every model or provider supports this.
#' \code{check_llm()} sends two short test prompts (the relation prompt that
#' \code{\link{causal_relation}} uses, for one clearly causal and one clearly non-causal
#' pair of variables) and reports whether the model:
#' \enumerate{
#'   \item can be reached (API key, credit and model name are correct),
#'   \item returns log probabilities, including alternative tokens (not only the chosen token),
#'   \item gives its answer as the first output token (no 'thinking' before the answer),
#'   \item gives sensible answers to the two test pairs.
#' }
#' If one of the first three checks fails, the model cannot be used with CLDassist, and
#' the output explains why and what to try. The check costs two very short API calls.
#'
#' @param LLM_model The model to check, written as in the other CLDassist functions:
#'   one of the built-in models (e.g. \code{"gpt-4o"}) or \code{"<provider>/<model name>"}
#'   (e.g. \code{"openai/gpt-4.1"} or \code{"huggingface/moonshotai/Kimi-K2.6:coreweave"}).
#' @param max_tokens The maximum number of tokens the model may return. Default is 5,
#'   which is enough for a one-token answer.
#' @param update_key Logical. If \code{TRUE}, you are asked to enter a new API key for the
#'   provider before the check. Default is \code{FALSE}.
#' @param custom_llm_fn Optional. Your own function to call an LLM, as described in
#'   \code{\link{var_list}}. If supplied, this function is checked instead of the
#'   built-in \code{LLM()} function, and the \code{LLM_model} argument is ignored.
#'
#' @return Invisibly, a list with:
#' \describe{
#'   \item{LLM_model}{The model that was checked.}
#'   \item{suitable}{\code{TRUE} if the model can be used with CLDassist, otherwise \code{FALSE}.}
#'   \item{problem}{\code{NA} if the model is suitable, otherwise a short description of the problem.}
#'   \item{tests}{A data frame with, per test pair, the expected answer, the answer given,
#'   its probability, the number of alternative tokens returned, and the tokens with
#'   their probabilities.}
#' }
#' The results are also printed as messages.
#'
#' @examples
#' \dontrun{
#' check_llm("gpt-4o")
#' check_llm("huggingface/moonshotai/Kimi-K2.6:coreweave")
#'
#' # Use the result in code
#' res <- check_llm("deepseek/deepseek-chat")
#' res$suitable
#' }
#'
#' @export
check_llm <- function(LLM_model = "gpt-4o",
                      max_tokens = 5,
                      update_key = FALSE,
                      custom_llm_fn = NULL) {

  ## Check arguments
  .validate_llm_args(LLM_model, max_tokens, custom_llm_fn)
  stopifnot("'update_key' should be TRUE or FALSE." =
              is.logical(update_key) && length(update_key) == 1L && !is.na(update_key))
  stopifnot("'custom_llm_fn' should be a function or NULL." =
              is.null(custom_llm_fn) || is.function(custom_llm_fn))

  model_label <- if (is.null(custom_llm_fn)) LLM_model else "custom_llm_fn"
  ok <- "\u2714"   # check mark
  fail <- "\u2716" # cross

  ## Test prompts: the relation prompt and system prompt that causal_relation() uses
  prompts_data <- utils::read.csv(system.file("extdata", "prompts.csv", package = "CLDassist"),
                                  sep = ";")
  rel_prompts <- prompts_data[prompts_data$Function == "relations", ]
  prompt_template <- gsub("\\n", "\n", rel_prompts$Prompt[1], fixed = TRUE)
  system_prompt <- gsub("\\n", "\n", rel_prompts$Sys.Prompt[1], fixed = TRUE)

  test_pairs <- data.frame(var1 = c("Smoking", "Eye color"),
                           var2 = c("Lung cancer", "Commute time"),
                           expected = c("C", "N"))



  ## Run the test prompts
  message(sprintf("Checking '%s' for use with CLDassist...", model_label))

  tests <- data.frame(var1 = test_pairs$var1, var2 = test_pairs$var2,
                      expected = test_pairs$expected, answer = NA_character_,
                      probability = NA_real_, n_alternatives = NA_integer_,
                      tokens = NA_character_)
  problem <- NA_character_
  hint <- NULL

  for (i in seq_len(nrow(test_pairs))) {

    prompt <- gsub("(pairs_df[i, 1])", test_pairs$var1[i], prompt_template, fixed = TRUE)
    prompt <- gsub("(pairs_df[i, 2])", test_pairs$var2[i], prompt, fixed = TRUE)

    # Warnings from the logprob helper are replaced by the clearer diagnosis below
    res <- tryCatch(
      suppressWarnings(.call_llm(prompt = prompt, system_prompt = system_prompt,
                                 LLM_model = LLM_model, max_tokens = max_tokens,
                                 update_key = update_key, custom_llm_fn = custom_llm_fn)),
      error = function(e) list(call_error = conditionMessage(e))
    )
    update_key <- FALSE # only ask for a new key once

    diagnosis <- .diagnose_llm_response(res)

    if (!is.null(diagnosis$last)) {
      last <- diagnosis$last
      tests$n_alternatives[i] <- nrow(last)
      tests$tokens[i] <- paste0(last$top5_tokens, " (", last$probability, "%)", collapse = ", ")
    }

    if (!is.na(diagnosis$problem)) {
      problem <- diagnosis$problem
      hint <- diagnosis$hint
      break
    }

    tests$answer[i] <- diagnosis$answer
    tests$probability[i] <- diagnosis$probability
  }


  ## Report
  steps <- c("API call works",
             "Log probabilities returned, including alternative tokens",
             "Answer is the first output token")
  failed_step <- switch(if (is.na(problem)) "none" else problem,
                        "API call failed" = 1,
                        "No log probabilities returned" = 2,
                        "Only the chosen token returned (no alternatives)" = 2,
                        # an empty answer without any tokens fails at the logprob step
                        "Empty answer" = if (all(is.na(tests$n_alternatives))) 2 else 3,
                        "Answer is not a single C/N token" = 3,
                        4)

  for (s in seq_along(steps)) {
    if (s < failed_step) {
      message(sprintf("%s %s", ok, steps[s]))
    } else if (s == failed_step) {
      message(sprintf("%s %s: %s", fail, steps[s], problem))
      break
    }
  }

  if (is.na(problem)) {
    answers <- sprintf("'%s' & '%s': %s (%.1f%%)", tests$var1, tests$var2,
                       tests$answer, tests$probability)
    if (all(tests$answer == tests$expected)) {
      message(sprintf("%s Sensible test answers: %s", ok, paste(answers, collapse = "; ")))
      message(sprintf("'%s' is suitable for CLDassist.", model_label))
    } else {
      message(sprintf("! Unexpected test answers (expected C and N): %s", paste(answers, collapse = "; ")))
      message(sprintf("'%s' works technically, but gave unexpected answers to clear-cut pairs, so its results may be less reliable.",
                      model_label))
    }
  } else {
    if (!is.null(hint)) message(hint)
    message(sprintf("'%s' cannot be used with CLDassist.", model_label))
  }

  invisible(list(LLM_model = model_label,
                 suitable = is.na(problem),
                 problem = problem,
                 tests = tests))
}


## Diagnose one LLM response for check_llm(): returns the problem (NA if none), a hint,
# the answer token, its probability and the logprob table of the answer position
.diagnose_llm_response <- function(res) {

  out <- list(problem = NA_character_, hint = NULL, answer = NA_character_,
              probability = NA_real_, last = NULL)

  # 1. Did the call work?
  err <- res$call_error
  if (is.null(err)) {
    api_err <- res$raw_content$error
    if (!is.null(api_err) && length(api_err) > 0) {
      err <- if (is.list(api_err) && !is.null(api_err$message)) api_err$message else paste(unlist(api_err), collapse = " ")
    }
  }
  if (!is.null(err)) {
    out$problem <- "API call failed"
    out$hint <- paste0("Message from the provider: ", err, "\n", .llm_error_hint(err))
    return(out)
  }

  content <- res$raw_content$content
  if (is.null(content)) content <- res$output
  has_content <- !is.null(content) && length(content) > 0 && !is.na(content[1]) && nzchar(trimws(content[1]))

  # 2. Were log probabilities returned?
  lp <- res$top5_tokens
  if (length(lp) == 0) {
    if (!has_content) {
      out$problem <- "Empty answer"
      out$hint <- .llm_thinking_hint()
    } else {
      out$problem <- "No log probabilities returned"
      out$hint <- "This model or provider does not return log probabilities, which CLDassist needs. Reasoning models (e.g. GPT-5 and later OpenAI models, Claude) generally do not support them. Try another model, or the same model at another provider."
    }
    return(out)
  }

  last <- lp[[length(lp)]]
  out$last <- last
  if (is.null(last) || nrow(last) < 2) {
    out$problem <- "Only the chosen token returned (no alternatives)"
    out$hint <- "This provider only returns the probability of the chosen token, so the probability of the other answer cannot be calculated. Try the same model at another provider (for Hugging Face, change the ':company' part of the model name)."
    return(out)
  }

  # 3. Is the answer a single C/N token?
  tokens <- trimws(tolower(last$top5_tokens))
  if (!nzchar(tokens[1]) && !has_content) {
    out$problem <- "Empty answer"
    out$hint <- .llm_thinking_hint()
    return(out)
  }
  if (!tokens[1] %in% c("c", "n")) {
    out$problem <- "Answer is not a single C/N token"
    out$hint <- paste0(sprintf("The model answered '%s' instead of C or N. ", if (has_content) trimws(content[1]) else last$top5_tokens[1]),
                       "CLDassist needs the answer as a single token. If the model 'thinks' before answering, switch this off (see below); otherwise this model does not follow the answer format.\n",
                       .llm_thinking_hint())
    return(out)
  }

  # 4. Answer and its probability (variants such as 'C' and ' c' are summed, as in causal_relation())
  out$answer <- toupper(tokens[1])
  out$probability <- min(sum(last$probability[tokens == tokens[1]]), 100)
  out
}


## Hint for models that 'think' before answering
.llm_thinking_hint <- function() {
  paste0("The model probably 'thinks' before answering, so no answer arrived within 'max_tokens'. ",
         "Switch thinking off; how depends on the provider, e.g.:\n",
         "  options(CLDassist.extra_body = list(reasoning = list(enabled = FALSE)))\n",
         "(this is already the default for 'together' and 'huggingface'). If thinking cannot be switched off for this model, choose another model.")
}


## Hint based on the error message of the provider
.llm_error_hint <- function(err) {
  e <- tolower(err)
  if (grepl("401|unauthori[sz]ed|invalid.*(api )?key|incorrect api key|expired", e)) {
    "The API key is missing, invalid or expired. Run check_llm() again with update_key = TRUE to enter a new key."
  } else if (grepl("402|insufficient|quota|credit|balance|billing|payment", e)) {
    "There is no credit left on the account. Add credit at the provider's website."
  } else if (grepl("404|not found|does not exist|unknown model|invalid model|model_not_found|not supported by any provider", e)) {
    "The model name was not found. Check the exact name on the provider's model page (for Hugging Face also the ':company' part)."
  } else if (grepl("429|rate limit|too many requests", e)) {
    "Too many requests. Wait a moment and try again."
  } else if (grepl("50[0-9]|bad gateway|service unavailable|overloaded|timeout|timed out", e)) {
    "The provider has a temporary problem. Try again later, or (for Hugging Face) another ':company'."
  } else if (grepl("reasoning is mandatory|reasoning.*cannot be disabled|thinking.*cannot be disabled", e)) {
    "This model or company only works with thinking switched on, which CLDassist cannot use. For Hugging Face, try another company by adding ':<company>' to the model name (e.g. ':together'), or choose another model."
  } else if (grepl("max_tokens|temperature|logprobs|unsupported parameter|not supported", e)) {
    "The model does not accept the settings CLDassist uses (logprobs, temperature 0 or max_tokens). This is typical of reasoning models, which cannot be used with CLDassist."
  } else {
    "Check the model name, API key and credit."
  }
}
