# Program Name: CLDassist
# Description: This function extracts the five highest log probabilities for every Large Language Model (LLM) token in the LLM output.
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

## logprob helper function

# Package-level state, used to show each warning only once per R session
.cldassist_state <- new.env(parent = emptyenv())

LLM_logprobs <- function(raw_content,
                         LLM_model = LLM_model) {

  logprobs_raw <- raw_content$choices[[1]]$logprobs

  # The format of the response is detected from the response itself (not from the model name):
  # - OpenAI format: logprobs$content[[j]]$top_logprobs (used by OpenAI and most other providers)
  # - Older format: logprobs$tokens / logprobs$token_logprobs (only the chosen token per position)
  if (!is.null(logprobs_raw$content)) {

    length <- length(logprobs_raw$content)
    logprobs_dfs <- vector("list", length = length)

    for (j in seq_along(logprobs_dfs)) {
      logprobs_list <- logprobs_raw$content[[j]]$top_logprobs

      # Some providers only return the chosen token (no alternatives); use that token instead
      if (length(logprobs_list) == 0) {
        chosen <- logprobs_raw$content[[j]]
        logprobs_list <- list(list(token = chosen$token, logprob = chosen$logprob))
      }

      top5_tokens <- vector("list", length = length(logprobs_list))
      top5_logprobs <- numeric(length(logprobs_list))
      top5_probabilities <- numeric(length(logprobs_list))

      for (i in seq_along(logprobs_list)) {
        top5_tokens[[i]] <- logprobs_list[[i]]$token
        top5_logprobs[i] <- logprobs_list[[i]]$logprob

        top5_probabilities[i] <- round(exp(top5_logprobs[i]) * 100, 2)
      }

      logprobs_dfs[[j]] <- data.frame(top5_tokens = unlist(top5_tokens),
                                      logprob = top5_logprobs,
                                      probability = top5_probabilities)
      logprobs_dfs[[j]] <- logprobs_dfs[[j]][!duplicated(logprobs_dfs[[j]]$top5_tokens), ] # some providers list the same token twice
    }

  } else if (!is.null(logprobs_raw$tokens)) {

    length <- length(logprobs_raw$tokens)
    logprobs_dfs <- vector("list", length = length)

    for (j in seq_along(logprobs_dfs)) {

      top5_tokens <- logprobs_raw$tokens[[j]]
      top5_logprobs <- logprobs_raw$token_logprobs[[j]]
      top5_probabilities <- round(exp(top5_logprobs) * 100, 2)

      logprobs_dfs[[j]] <- data.frame(top5_tokens = unlist(top5_tokens),
                                      logprob = top5_logprobs,
                                      probability = top5_probabilities)
      logprobs_dfs[[j]] <- logprobs_dfs[[j]][!duplicated(logprobs_dfs[[j]]$top5_tokens), ] # some providers list the same token twice
    }

  } else {
    # No logprobs in the response (e.g., the model or provider does not support them)
    logprobs_dfs <- list()

    warn_key <- paste0("no_logprobs_", LLM_model)
    if (!is.null(raw_content$choices) && is.null(.cldassist_state[[warn_key]])) {
      assign(warn_key, TRUE, envir = .cldassist_state)
      warning(sprintf("'%s' did not return any log probabilities, so no probabilities can be calculated. Choose a model that supports logprobs (reasoning models, e.g. GPT-5, generally do not). (This warning is shown once per session.)", LLM_model),
              call. = FALSE)
    }
  }

  # Remove special end-of-message tokens at the end (e.g. "<|im_end|>", "</s>", "<eos>"):
  # some providers return these as separate positions, while CLDassist reads the answer
  # from the last position
  is_special <- function(df) {
    nrow(df) > 0 && grepl("^(<\\|[^|]*\\|>|<\uff5c[^\uff5c]*\uff5c>|</s>|<eos>|<end_of_turn>|<\\|eot_id\\|>)$", trimws(df$top5_tokens[1]))
  }
  while (length(logprobs_dfs) > 1 && is_special(logprobs_dfs[[length(logprobs_dfs)]])) {
    logprobs_dfs[[length(logprobs_dfs)]] <- NULL
  }

  # Warn (once per model per session) if fewer than 2 alternatives were returned: the probability
  # of the answer options that were not chosen is then unknown and will be counted as 0
  n_alternatives <- vapply(logprobs_dfs, nrow, integer(1))
  warn_key <- paste0("few_alternatives_", LLM_model)
  if (length(n_alternatives) > 0 && any(n_alternatives < 2) && is.null(.cldassist_state[[warn_key]])) {
    assign(warn_key, TRUE, envir = .cldassist_state)
    warning(sprintf("'%s' returned fewer than 2 alternative tokens; probabilities of answer options that were not chosen are unknown and set to 0. If possible, choose a provider/model that returns 'top_logprobs'. (This warning is shown once per session.)", LLM_model),
            call. = FALSE)
  }

  return(logprobs_dfs)
}
