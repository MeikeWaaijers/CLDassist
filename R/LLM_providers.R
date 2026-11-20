# Program Name: CLDassist
# Description: Provider settings and input checks for the Large Language Model (LLM) APIs supported by CLDassist.
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

## Providers with an OpenAI-compatible chat completions API that can return logprobs.
# Models from these providers are selected with LLM_model = "<provider>/<model name>",
# e.g. "openai/gpt-4.1", "deepseek/deepseek-chat", "together/Qwen/Qwen3.5-9B" or
# "huggingface/deepseek-ai/DeepSeek-V4.1-Flash-1017:together" (Hugging Face: add the company after a colon)
# - endpoint:    address of the chat completions API
# - key_service: name under which the API key is stored (NULL = no key needed)
# - extra:       provider-specific fields that are added to every request
.LLM_providers <- list(
  openai = list(endpoint = "https://api.openai.com/v1/chat/completions",
                key_service = "openai",
                token_param = "max_completion_tokens", # newer OpenAI models no longer accept "max_tokens"
                extra = list()),
  deepseek = list(endpoint = "https://api.deepseek.com/chat/completions",
                  key_service = "deepseek",
                  extra = list()),

  huggingface = list(endpoint = "https://router.huggingface.co/v1/chat/completions",
                     key_service = "huggingface", # Hugging Face token (HF_TOKEN)

                     # switch off thinking: CLDassist needs the answer as the first output token
                     extra = list(reasoning = list(enabled = FALSE))),
  together = list(endpoint = "https://api.together.xyz/v1/chat/completions",
                  key_service = "togetherai",
                  # switch off thinking: CLDassist needs the answer as the first output token
                  extra = list(reasoning = list(enabled = FALSE)))
  # Local models via Ollama (not yet enabled; to enable, add a comma after the together entry
  # above, uncomment the lines below, and uncomment the Ollama part of .llm_endpoint()):
  # ollama = list(endpoint = NULL, # set at run time, see .llm_endpoint()
  #               key_service = NULL, # runs locally, no API key needed
  #               extra = list())
)


## Extra request fields for a request: provider defaults, plus fields set by the user with
# options(CLDassist.extra_body = list(...)), e.g. to switch off a thinking mode:
# options(CLDassist.extra_body = list(reasoning = list(enabled = FALSE)))
.llm_extra_body <- function(provider) {
  extra <- .LLM_providers[[provider]]$extra
  user_extra <- getOption("CLDassist.extra_body", default = list())
  utils::modifyList(extra, user_extra)
}

## Built-in OpenAI models, with their maximum number of tokens
.legacy_llm_models <- c("gpt-4o" = 6000,
                        "gpt-4" = 6000,
                        "gpt-4-turbo" = 6000,
                        "gpt-3.5-turbo" = 3000)


## Split "<provider>/<model name>" into its two parts
.split_llm_model <- function(LLM_model) {
  list(provider = sub("/.*$", "", LLM_model),
       model = sub("^[^/]+/", "", LLM_model))
}


## Endpoint of a provider
.llm_endpoint <- function(provider) {
  # Ollama (not yet enabled): the address can be changed with the OLLAMA_BASE_URL environment variable
  # if (provider == "ollama") {
  #   base_url <- Sys.getenv("OLLAMA_BASE_URL", unset = "http://localhost:11434")
  #   return(paste0(sub("/+$", "", base_url), "/v1/chat/completions"))
  # }
  .LLM_providers[[provider]]$endpoint
}


## Check the LLM_model and max_tokens arguments (used by all functions that call an LLM)
.validate_llm_args <- function(LLM_model, max_tokens, custom_llm_fn = NULL) {

  # A custom LLM function handles the model and token settings itself
  if (!is.null(custom_llm_fn)) {
    return(invisible(TRUE))
  }

  stopifnot("'LLM_model' should be a single character string." =
              is.character(LLM_model) && length(LLM_model) == 1L && !is.na(LLM_model))

  if (LLM_model %in% names(.legacy_llm_models)) {
    max_allowed <- .legacy_llm_models[[LLM_model]]
    if (!(is.numeric(max_tokens) && length(max_tokens) == 1L && max_tokens == floor(max_tokens) && max_tokens > 0 && max_tokens <= max_allowed)) {
      stop(sprintf("For '%s', 'max_tokens' should be a whole number above 0, and not higher than %d.", LLM_model, max_allowed), call. = FALSE)
    }

  } else if (grepl("^[^/]+/.+", LLM_model)) {
    provider <- .split_llm_model(LLM_model)$provider
    if (!provider %in% names(.LLM_providers)) {
      stop(sprintf("Unknown provider '%s' in 'LLM_model'. Use one of: %s (e.g., \"openai/gpt-4.1\"), or supply your own function via 'custom_llm_fn'.",
                   provider, paste(names(.LLM_providers), collapse = ", ")), call. = FALSE)
    }
    if (!(is.numeric(max_tokens) && length(max_tokens) == 1L && max_tokens == floor(max_tokens) && max_tokens > 0)) {
      stop("'max_tokens' should be a whole number above 0.", call. = FALSE)
    }

  } else {
    stop(sprintf("'LLM_model' should be one of %s, or a model from a supported provider written as \"<provider>/<model name>\" (providers: %s), e.g. \"huggingface/meta-llama/Llama-3.3-70B-Instruct\".",
                 paste0("'", names(.legacy_llm_models), "'", collapse = ", "),
                 paste(names(.LLM_providers), collapse = ", ")), call. = FALSE)
  }

  invisible(TRUE)
}
