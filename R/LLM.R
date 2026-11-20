# Program Name: CLDassist
# Description: This function interacts with Large Language Model (LLM) APIs and extracts their output.
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


### CLDassist

## LLM helper function

LLM <- function(prompt = prompt,
                LLM_model = "gpt-4o",
                max_tokens = 2000,
                temperature = 0,
                # suffix = NULL,
                top_p = 1,
                logprobs = TRUE,
                top_logprobs = 5,
                # stop = NULL,
                # presence_penalty = 0,
                # frequency_penalty = 0,
                timeout_sec = 60,
                system_prompt = NULL,
                raw_output = TRUE,
                update_key = update_key){


  if (grepl("^[^/]+/.+", LLM_model)) {
    ## Any model from a supported provider, written as "<provider>/<model name>"
    # All these providers use the OpenAI chat completions format (see LLM_providers.R)
    parts <- .split_llm_model(LLM_model)
    provider <- parts$provider
    endpoint <- .llm_endpoint(provider)
    key_service <- .LLM_providers[[provider]]$key_service

    # Headers (providers without a key_service, e.g. local models, do not need an API key)
    if (is.null(key_service)) {
      headers <- httr::add_headers(`Content-Type` = "application/json")
    } else {
      api_key <- get_api_key(key_service,
                             update_key = update_key)
      headers <- httr::add_headers(Authorization = paste("Bearer", api_key),
                                   `Content-Type` = "application/json")
    }

    # Messages (system prompt only if provided)
    messages <- list()
    if (!is.null(system_prompt)) {
      messages <- list(list(role = "system", content = system_prompt))
    }
    messages <- c(messages, list(list(role = "user", content = prompt)))

    # Request body
    # (the name of the token limit differs per provider, e.g. "max_completion_tokens" for OpenAI)
    token_param <- .LLM_providers[[provider]]$token_param
    if (is.null(token_param)) token_param <- "max_tokens"
    request_body <- list(model = parts$model,
                         temperature = temperature,
                         messages = messages)
    request_body[[token_param]] <- max_tokens

    if (logprobs == TRUE) {
      request_body$logprobs <- TRUE
      request_body$top_logprobs <- top_logprobs
    }

    # Provider-specific fields and fields set via options(CLDassist.extra_body = list(...))
    request_body <- utils::modifyList(request_body, .llm_extra_body(provider))

    # Make the API request
    # (errors that will not be solved by retrying, e.g. a wrong key, no credit or a wrong model name, stop immediately)
    request <- httr::RETRY(verb = "POST",
                           url = endpoint,
                           body = request_body,
                           headers,
                           encode = "json",
                           times = 5,
                           terminate_on = c(400, 401, 402, 403, 404, 422),
                           httr::timeout(timeout_sec))

    # Extract the response
    raw_content <- content <- httr::content(request)
    if (!is.list(raw_content)) {
      raw_content <- content <- list(raw_text = raw_content)
    }

    # Make sure an unsuccessful request always comes with a readable error message
    status <- httr::status_code(request)
    if (status >= 400) {
      api_message <- tryCatch(content$error$message, error = function(e) NULL)
      if (is.null(api_message)) {
        api_message <- tryCatch(as.character(content$error), error = function(e) NULL)
      }
      hint <- switch(as.character(status),
                     "401" = " (invalid API key)",
                     "402" = " (insufficient credit on your account)",
                     "404" = " (model not found; check the model name)",
                     "429" = " (rate limit or quota reached)",
                     "")
      raw_content$error <- list(message = paste0("HTTP ", status, hint,
                                                 if (length(api_message) > 0 && nzchar(api_message[1])) paste0(": ", api_message[1]) else ""))
    }

    output <- content$choices[[1]]$message$content

  } else if (LLM_model == "gpt-4o" | LLM_model == "gpt-4" | LLM_model == "gpt-4-turbo" | LLM_model == "gpt-3.5-turbo") {

    api_key <- get_api_key("openai",
                           update_key = update_key)

    # API endpoint
    endpoint <- "https://api.openai.com/v1/chat/completions"

    if (logprobs == TRUE) {
      # Request body
      request_body <- list(
        model = LLM_model,
        max_tokens = max_tokens,
        temperature = temperature,
        # n = 1,
        # suffix = suffix,
        # top_p = top_p,
        top_logprobs = top_logprobs,
        logprobs = logprobs,
        # stop = stop,
        # presence_penalty = presence_penalty,
        # frequency_penalty = frequency_penalty,
        messages = list(
          list(role = "system", content = system_prompt),
          list(role = "user", content = prompt)
        )
      )

    } else if (logprobs == FALSE) {
      # Request body
      request_body <- list(
        model = LLM_model,
        max_tokens = max_tokens,
        temperature = temperature,
        # n = 1,
        # suffix = suffix,
        # top_p = top_p,
        logprobs = logprobs,
        # stop = stop,
        # presence_penalty = presence_penalty,
        # frequency_penalty = frequency_penalty,
        messages = list(
          list(role = "system", content = system_prompt),
          list(role = "user", content = prompt)
        )
      )

    }

    # Make the API request
    request <- httr::RETRY(verb = "POST",
                           url = endpoint,
                           body = request_body,
                           httr::add_headers(Authorization = paste("Bearer",
                                                                   api_key)),
                           encode = "json",
                           times = 5,
                           terminate_on = c(400, 401, 402, 403, 404, 422),
                           httr::timeout(timeout_sec))

    # Extract the response
    raw_content <- content <- httr::content(request)
    output <- content$choices[[1]]$message$content

  } else if (LLM_model == "gpt-4.1") {

    api_key <- get_api_key("openai",
                           update_key = update_key)

    # API endpoint
    endpoint <- "https://api.openai.com/v1/responses"

    # Request body
    request_body <- list(model = LLM_model,
                         tools = list(list(type = "web_search_preview")),
                         max_output_tokens = max_tokens,
                         temperature = temperature,
                         input = prompt)

    # Make the API request
    request <- httr::RETRY(verb = "POST",
                           url = endpoint,
                           body = request_body,
                           httr::add_headers(Authorization = paste("Bearer", api_key)),
                           encode = "json",
                           times = 5,
                           terminate_on = c(400, 401, 402, 403, 404, 422),
                           httr::timeout(timeout_sec))

    # Extract the response
    raw_content <- content <- httr::content(request)

    # The answer is in the output item of type "message" (web search calls, if any, come before it)
    msg <- Filter(function(x) identical(x$type, "message"), raw_content$output)
    msg_content <- if (length(msg) > 0) msg[[1]]$content[[1]] else NULL
    output <- if (is.null(msg_content$text)) "" else msg_content$text
    sources <- unlist(lapply(msg_content$annotations, function(a) a$url))

  }

  if (LLM_model == "gpt-4.1") {
    output <- list(raw_content = list(LLM_model = raw_content$model,
                                      content = output,
                                      finish_reason = if (length(msg) > 0) msg[[1]]$status else NA,
                                      prompt_tokens = tryCatch(raw_content$usage$input_tokens, error = function(e) NA),
                                      answer_tokens = tryCatch(raw_content$usage$output_tokens, error = function(e) NA),
                                      total_tokens = tryCatch(raw_content$usage$total_tokens, error = function(e) NA),
                                      error = raw_content$error),
                   sources = sources,
                   output = output)

  } else if (raw_output == TRUE && logprobs == TRUE) {
    top5_logprobs <- LLM_logprobs(raw_content = raw_content,
                                  LLM_model = LLM_model)
    output <- list(raw_content = list(LLM_model = raw_content$model,
                                      content = raw_content$choices[[1]]$message$content,
                                      finish_reason = raw_content$choices[[1]]$finish_reason,
                                      prompt_tokens = raw_content$usage$prompt_tokens,
                                      answer_tokens = raw_content$usage$completion_tokens,
                                      total_tokens = raw_content$usage$total_tokens,
                                      error = raw_content$error),
                   top5_tokens = top5_logprobs,
                   output = output)


  } else if (raw_output == TRUE && logprobs == FALSE) {
    output <- list(raw_content = list(LLM_model = raw_content$model,
                                      content = raw_content$choices[[1]]$message$content,
                                      finish_reason = raw_content$choices[[1]]$finish_reason,
                                      prompt_tokens = raw_content$usage$prompt_tokens,
                                      answer_tokens = raw_content$usage$completion_tokens,
                                      total_tokens = raw_content$usage$total_tokens,
                                      error = raw_content$error),
                   output = output)

  } else if (raw_output == FALSE && logprobs == TRUE) {
    top5_logprobs <- LLM_logprobs(raw_content = raw_content,
                                  LLM_model = LLM_model)
    output <- list(top5_tokens = top5_logprobs,
                   output = output)


  }

  return(output)
}



## LLM dispatcher: routes to custom function or built-in LLM()
.call_llm <- function(prompt, system_prompt, LLM_model, max_tokens,
                      update_key, custom_llm_fn) {
  if (!is.null(custom_llm_fn)) {
    res <- custom_llm_fn(prompt = prompt, system_prompt = system_prompt)
    if (is.null(res$raw_content))
      res$raw_content <- list(LLM_model = "custom", content = res$output,
                              finish_reason = NA, prompt_tokens = NA,
                              answer_tokens = NA, total_tokens = NA, error = NULL)
    if (is.null(res$top5_tokens)) res$top5_tokens <- NULL
    return(res)
  }
  LLM(prompt = prompt, LLM_model = LLM_model, max_tokens = max_tokens,
      temperature = 0, logprobs = TRUE, raw_output = TRUE,
      system_prompt = system_prompt, update_key = update_key)
}


## Replace a missing (NULL or empty) value by NA, e.g. for fields that are absent after an API error
.val <- function(x) if (is.null(x) || length(x) == 0) NA else x
