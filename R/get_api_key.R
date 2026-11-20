# Program Name: CLDassist
# Description: This function retrieves the API key for a Large Language Model (LLM) service.
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

## get_api_key helper function

get_api_key <- function(service_name, update_key = FALSE) {

  # Environment variable and sign-up page per service
  env_vars <- c(openai = "OPENAI_API_KEY",
                togetherai = "TOGETHER_API_KEY",
                huggingface = "HF_TOKEN",
                deepseek = "DEEPSEEK_API_KEY")

  signup_pages <- c(openai = "https://platform.openai.com/account/api-keys",
                    togetherai = "https://api.together.xyz/settings/api-keys",
                    huggingface = "https://huggingface.co/settings/tokens",
                    deepseek = "https://platform.deepseek.com/api_keys")

  # 1. Use the environment variable for this service if it is set (e.g., on shinyapps.io, in CI, or set by the user)
  env_var <- if (service_name %in% names(env_vars)) env_vars[[service_name]] else toupper(paste0(service_name, "_API_KEY"))
  api_key <- Sys.getenv(env_var)
  if (nzchar(api_key) && !update_key) {
    return(api_key)
  }

  # Check if running on shinyapps.io or in a CI environment (no interactive input possible)
  shinyapps <- Sys.getenv("R_CONFIG_ACTIVE") == "shinyapps"
  ci <- nzchar(Sys.getenv("CI"))
  if (shinyapps || ci) {
    stop(sprintf("No API key found. Please set the '%s' environment variable.", env_var), call. = FALSE)
  }

  # 2. Otherwise use the keyring; ask for the key if it is not stored yet (or if update_key = TRUE)
  if (update_key || nrow(keyring::key_list(service = service_name)) == 0) {
    cat("To use this functionality, an API key needs to be set.\n")
    cat("Please follow these steps to resolve the issue:\n")
    if (service_name %in% names(signup_pages)) {
      cat(sprintf("1. Create an API key on %s \n", signup_pages[[service_name]]))
    } else {
      cat(sprintf("1. Create an API key for '%s' \n", service_name))
    }
    cat("2. Please enter your API key below to add/update it.")
    answer <- readline("API key = ")
    keyring::key_set_with_value(service = service_name, username = "user", password = answer)
  }

  return(keyring::key_get(service = service_name, username = "user"))
}
