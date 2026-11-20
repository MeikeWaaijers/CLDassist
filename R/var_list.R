# Program Name: CLDassist
# Description: In the var_list function a Large Language Model (LLM) is instructed to generate a list of important variables for a particular topic.
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
### Variable listing function

## Function manual

#' Create a Variable List
#'
#' @description
#' In \code{var_list()} a Large Language Model (LLM) is instructed to generate a list of important variables for a particular topic.
#'
#' The function prompts a LLM to compile two lists of variables with 2 different prompts.
#' The LLM is instructed to include only variables that can increase or decrease, are well-defined and measurable, do not overlap, and are neutrally worded.
#' The LLM then merges the two lists, removing duplicates and synonyms, and reviews the combined list to identify any missing crucial variables.
#' Finally, it refines the list by separating combined factors into distinct items.
#'
#' In addition, users can limit the final number of variables, prompting the LLM to select the N most important variables.
#'
#' @usage
#' var_list(topic,
#'          include_topic = FALSE,
#'          n_final = Inf,
#'          n_variables = "all",
#'          LLM_model = "gpt-4o",
#'          max_tokens = 2000,
#'          update_key = FALSE,
#'          custom_llm_fn = NULL)
#'
#' @details
#' To create a CLD from scratch, the functions in this R-package should be used in the following order:
#'
#' \code{\link{var_list}} --> \code{\link{causal_relation}} --> \code{\link{causal_direction}} --> \code{\link{causal_sign}} --> \code{\link{cld_plot}} --> \code{\link{search_literature}}
#'
#' @param topic A character string specifying the topic for which a CLD should be developed.
#' @param include_topic If \code{include_topic = FALSE} (default), the topic specified in the \code{"topic"} argument will not be included as a separate variable in the variable list.
#' @param n_variables Number of variables the LLM should generate in the first 2 variable lists. If \code{"all"} (default), the LLM is not limited to generate a specific number of variables, but is asked to create a list of "all" important variables.
#' @param n_final Number of variables to be included in the final variable list. If \code{Inf} (default), the final integrated variable list will not be limited to a certain number of variables.
#' @param LLM_model The LLM that should be used to generate output. Either one of the built-in OpenAI models (\code{"gpt-4o"} (default), \code{"gpt-4"}, \code{"gpt-4-turbo"}, \code{"gpt-3.5-turbo"}), or any model from a supported provider written as \code{"<provider>/<model name>"} (providers: \code{openai}, \code{deepseek}, \code{together}, \code{huggingface}; e.g., \code{"huggingface/meta-llama/Llama-3.3-70B-Instruct"} or \code{"huggingface/moonshotai/Kimi-K2.6"}). The model must return log probabilities; use \code{\link{check_llm}()} to check whether a model can be used.
#' @param max_tokens The maximum number of tokens the LLM should generate. Be careful when adjusting this argument. Reducing the maximum token limit will reduce the cost but may result in incomplete answers. Conversely, increasing the token limit can be advantageous for obtaining more detailed responses. For the built-in models, the maximum is \code{6000} for \code{"gpt-4o"}, \code{"gpt-4"} and \code{"gpt-4-turbo"}, and \code{3000} for \code{"gpt-3.5-turbo"}. For other models, any whole number above 0 is accepted; check the limit of the model you use.
#' @param update_key If \code{update_key = TRUE}, the function will prompt the user for a new API key and update the saved key. If \code{update_key = FALSE} (default), the function will use the existing API key if available.
#' @param custom_llm_fn Optionally, a custom function to use instead of the built-in LLM models. If provided, the \code{LLM_model}, \code{max_tokens}, and \code{update_key} arguments are ignored; handle those inside your custom function. The function must accept \code{prompt} and \code{system_prompt} as arguments and return a list with at minimum \code{$output} (the response text as a character string), and optionally \code{$top5_tokens} for functions that calculate probabilities from logprobs (namely the \code{\link{causal_relation}}, \code{\link{causal_direction}}, and \code{\link{causal_sign}} functions). \code{$top5_tokens} must be a list of dataframes, one per output token, each with columns \code{top5_tokens} (character), \code{logprob} (numeric), and \code{probability} (numeric).
#'
#' @returns
#' \itemize{
#'   \item \code{raw_LLM}: A dataframe containing the unprocessed LLM output along with some other LLM information, including:
#'     \itemize{
#'       \item \code{function_part}: Part of function from which LLM output originates.
#'       \item \code{iteration}: Iteration number.
#'       \item \code{LLM_model}: LLM model used.
#'       \item \code{prompt}: Prompt used.
#'       \item \code{system_prompt}: System prompt used.
#'       \item \code{content}: Unprocessed LLM output.
#'       \item \code{finish_reason}: Reason the LLM stopped generating output.
#'       \item \code{prompt_tokens}: Number of tokens used for the LLM prompt.
#'       \item \code{answer_tokens}: Number of tokens used for the LLM answer.
#'       \item \code{total_tokens}: Total number of tokens used.
#'       \item \code{error}: Error message, if any occurred.
#'     } \cr
#'   \item \code{all_vars}: A vector containing the integrated variable list with every variable generated by the LLM.
#'   \item \code{final_list}: A vector containing the final variable list. If the \code{n_final} argument is set to \code{Inf} or to a number higher than the number of variables in \code{all_vars}, the final list will be exactly the same as \code{all_vars}.
#' }
#'
#' @references \url{https://platform.openai.com}
#'
#' @author Meike Waaijers
#'
#' @note The function and its output should be approached with caution. Depending on the specific LLM used, there may be financial implications. Furthermore, we wish to emphasise that the answers generated by an LLM should not be taken as absolute truth.
#'
#' @seealso
#' \code{\link{cld}},
#' \code{\link{causal_relation}},
#' \code{\link{causal_direction}},
#' \code{\link{causal_sign}},
#' \code{\link{cld_plot}},
#' \code{\link{search_literature}}
#'
#' @examples
#' \dontrun{
#' # For a readily available, pre-made output example see: data("vars")
#' vars <- var_list(topic = "addiction",
#'                  n_final = 10,
#'                  n_variables = "all")
#'
#' # Check output
#' vars$all_vars
#' vars$final_list
#' }
#'
#' @import httr
#' @import utils
#' @import keyring
#' @export


## var_list function
var_list <- function(topic,
                     include_topic = FALSE,
                     n_final = Inf,
                     n_variables = "all",
                     LLM_model = "gpt-4o",
                     max_tokens = 2000,
                     update_key = FALSE,
                     custom_llm_fn = NULL) {


  #validate input
  stopifnot(
    "'topic' should be a single non-empty character string." =
      is.character(topic) && length(topic) == 1L && !is.na(topic) && nzchar(trimws(topic))
  )
  stopifnot("'include_topic' should be a logical value." = is.logical(include_topic))
  stopifnot("'n_final' should be a whole number above 0." =
              is.numeric(n_final) && n_final == floor(n_final) && n_final > 0)
  stopifnot("'n_variables' should be a whole number above 0 or the input should be 'all'." =
              ((is.numeric(n_variables) && n_variables == floor(n_variables) && n_variables > 0) || n_variables == "all"))
  .validate_llm_args(LLM_model, max_tokens, custom_llm_fn)
  stopifnot("'update_key' should be a logical value." = is.logical(update_key))

  if (!is.null(custom_llm_fn)) {
    stopifnot("'custom_llm_fn' must be a function accepting 'prompt' and 'system_prompt'." =
                is.function(custom_llm_fn) &&
                all(c("prompt", "system_prompt") %in% names(formals(custom_llm_fn))))
    if (update_key) message("'update_key' is ignored when 'custom_llm_fn' is provided.")
    message("'LLM_model' and 'max_tokens' are ignored when 'custom_llm_fn' is provided.")
  }

  ## Load and prepare prompt data
  prompt_file_path <- system.file("extdata", "prompts.csv", package = "CLDassist")
  prompts_data <- utils::read.csv(prompt_file_path, sep = ";")

  # replace '\\n' with '\n' in all text columns
  for (i in 1:length(prompts_data$Prompt)) {
    # Check if the prompt column is a character type
    if (is.character(prompts_data$Prompt[[i]])) {
      # Replace '\\n' with '\n' in the column
      prompts_data$Prompt[[i]] <- gsub("\\n", "\n",
                                       prompts_data$Prompt[[i]],
                                       fixed = TRUE)
    }

    # Check if the sys prompt column is a character type
    if (is.character(prompts_data$Sys.Prompt[[i]])) {
      # Replace '\\n' with '\n' in the column
      prompts_data$Sys.Prompt[[i]] <- gsub("\\n", "\n",
                                           prompts_data$Sys.Prompt[[i]],
                                           fixed = TRUE)
    }
  }

  var_prompts <- prompts_data[prompts_data$Function == "var_list", ]


  ## Create objects for tryCatch output
  # So somethings gets outputted even though an error occurs later on in the function
  raw1_LLM <- NULL
  raw2_LLM <- NULL
  raw3_LLM <- NULL
  raw4_LLM <- NULL
  raw5_LLM <- NULL
  logprobs1 <- NULL
  logprobs2 <- NULL
  logprobs3 <- NULL
  logprobs4 <- NULL
  logprobs5 <- NULL
  raw_LLM <- NULL
  logprobs_out <- NULL
  all_lists_df <- NULL
  variables_f <- NULL
  variables_ <- NULL
  extra_vars <- NULL
  cleaned_vars <- NULL
  variables <- NULL


  ## LLM
  runs <- NULL

  # First create 2 prompts
  prompt1 <- gsub("\\((n_variables)\\)", n_variables,
                  gsub("\\((topic)\\)", topic,
                       var_prompts$Prompt[1]))

  prompt2 <- gsub("\\((n_variables)\\)", n_variables,
                  gsub("\\((topic)\\)", topic,
                       var_prompts$Prompt[2]))

  prompt_database <- c(prompt1, prompt2)

  # Create system prompts
  system_prompt1 <- gsub("\\((topic)\\)", topic,
                         var_prompts$Sys.Prompt[1])

  system_prompt2 <- gsub("\\((topic)\\)", topic,
                         var_prompts$Sys.Prompt[2])

  sys_prompt_database <- c(system_prompt1, system_prompt2)


  for (i in 1:2){
    message(sprintf("Variable list: %d / %d", i, 2))

    prompt <- prompt_database[i]
    system_prompt <- sys_prompt_database[i]

    # LLM
    variable_list <- .call_llm(prompt = prompt,
                               system_prompt = system_prompt,
                               LLM_model = LLM_model,
                               max_tokens = max_tokens,
                               update_key = update_key,
                               custom_llm_fn = custom_llm_fn)

    update_key <- FALSE # make sure api key is only updated once
    runs[[i]] <- variable_list$output
    raw1_LLM[[i]] <- c(prompt = prompt_database[i], system_prompt = sys_prompt_database[i], variable_list$raw_content)
    logprobs1[[i]] <- variable_list$top5_tokens
  }

  # Clean the output text
  #tryCatch in case processing steps fail the raw output will still be outputted
  tryCatch({

    split_text <- NULL
    for (k in 1:2){
      split_text[k] <- strsplit(runs[[k]], split = "\n") # Split the string by newline character
    }
    all_lists <- lapply(split_text, function(x) gsub("\\*\\*", "", sub("^\\s*([0-9]+[.)]|[-*])\\s+", "", x)))
    all_lists <- lapply(all_lists, function(x) trimws(x))
    all_lists <- lapply(all_lists, function(x) x[nzchar(x)]) # remove empty lines

    # Get list lengths
    list_length <- NULL
    for (k in 1:2) {
      list_length[k] <- length(all_lists[[k]]) # make list of list lengths
    }

    # Make all list the same length so they can be put into the same df
    for (l in 1:2) {
      length(all_lists[[l]]) <- max(list_length)
    }

    all_lists_df <- data.frame(all_lists)

    #Change column names
    for (m in 1:2) {
      colnames(all_lists_df)[m] <- paste("List", m, sep = "_")
    }

    #replace na's in with "-"
    all_lists_df[is.na(all_lists_df)] <- "-"

  }, error = function(e){
    cat(paste0("Warning: Unable to process LLM output for list integration -> ", e$message, "."),
        "Only part of the output is returned.", sep = "\n")

  })


  # Integrating lists
  message("Integrating lists...")

  #tryCatch in case processing steps fail the raw output will still be outputted
  tryCatch({

    ## LLM for integrating lists
    # collapse all lists into one to put in the prompt
    prompt_list <- paste0(runs, collapse = "\n\n")

    # Create prompt
    prompt_int <- gsub("\\((prompt_list)\\)", prompt_list,
                       var_prompts$Prompt[3])

    # Create system prompt
    system_prompt <- var_prompts$Sys.Prompt[3]

    # LLM
    integrated <- .call_llm(prompt = prompt_int,
                            system_prompt = system_prompt,
                            LLM_model = LLM_model,
                            max_tokens = max_tokens,
                            update_key = update_key,
                            custom_llm_fn = custom_llm_fn)

    variables_ <- gsub("\n\n", "\n", integrated$output)  # replace empty lines in LLM output
    raw2_LLM <- c(prompt = prompt_int, system_prompt = system_prompt, integrated$raw_content)
    logprobs2 <- integrated$top5_tokens

  }, error = function(e){
    cat(paste0("Warning: List integration not possible -> ", e$message, "."),
        "Only part of the output is returned.", sep = "\n")
  })

  #tryCatch in case processing steps fail the raw output will still be outputted
  tryCatch({

    ## LLM for adding additional variables
    # Create prompt
    prompt_add <- gsub("\\((variables_)\\)", variables_,
                       gsub("\\((topic)\\)", topic,
                            var_prompts$Prompt[4]))

    # Create system prompt
    system_prompt <- var_prompts$Sys.Prompt[4]

    # LLM
    add_vars <- .call_llm(prompt = prompt_add,
                          system_prompt = system_prompt,
                          LLM_model = LLM_model,
                          max_tokens = max_tokens,
                          update_key = update_key,
                          custom_llm_fn = custom_llm_fn)

    extra_vars <- gsub("\n\n", "\n", add_vars$output)  # replace empty lines in LLM output
    raw3_LLM <- c(prompt = prompt_add, system_prompt = system_prompt, add_vars$raw_content)
    logprobs3 <- add_vars$top5_tokens

  }, error = function(e){
    cat(paste0("Warning: Unable to process LLM output for adding missing variables -> ", e$message, "."),
        "Only part of the output is returned.", sep = "\n")

  })


  ## Clean up variable list
  message("Cleaning up integrated list...")

  #tryCatch in case processing steps fail the raw output will still be outputted
  tryCatch({

    ## LLM for integrating lists
    # Create prompt
    prompt_clean <- gsub("\\((extra_vars)\\)", extra_vars,
                         var_prompts$Prompt[5])

    # Create system prompt
    system_prompt <- gsub("\\((topic)\\)", topic,
                          var_prompts$Sys.Prompt[5])

    # LLM
    clean <- .call_llm(prompt = prompt_clean,
                       system_prompt = system_prompt,
                       LLM_model = LLM_model,
                       max_tokens = max_tokens,
                       update_key = update_key,
                       custom_llm_fn = custom_llm_fn)

    cleaned_vars <- gsub("\n\n", "\n", clean$output)  # replace empty lines in LLM output
    raw4_LLM <- c(prompt = prompt_clean, system_prompt = system_prompt, clean$raw_content)
    logprobs4 <- clean$top5_tokens

  }, error = function(e){
    cat(paste0("Warning: Cleaning up the list not possible -> ", e$message, "."),
        "Only part of the output is returned.", sep = "\n")
  })


  tryCatch({

    #Clean the output text
    split_text <- strsplit(cleaned_vars, split = "\n") # Split the string by newline character
    variables <- lapply(split_text, function(x) gsub("\\*\\*", "", sub("^\\s*([0-9]+[.)]|[-*])\\s+", "", x)))
    variables <- lapply(variables, function(x) trimws(x))
    variables <- lapply(variables, function(x) x[nzchar(x)]) # remove empty lines


  }, error = function(e) {
    cat(paste0("Warning: Unable to process LLM output -> ", e$message, "."),
        "Only part of the output is returned.", sep = "\n")
  })

  tryCatch({
    ## LLM for limiting the final list to a list with the most ... important variables
    if (length(variables[[1]]) > n_final) {

      message(sprintf("Selecting the %d most important variables...", n_final))

      #tryCatch in case processing steps fail the raw output will still be outputted
      tryCatch({

        # Create prompt
        prompt_limit_int <- gsub("\\((cleaned_vars)\\)", cleaned_vars,
                                 gsub("\\((n_final)\\)", n_final,
                                      gsub("\\((topic)\\)", topic,
                                           var_prompts$Prompt[6])))

        # Create system prompt
        system_prompt <- var_prompts$Sys.Prompt[6]

        # LLM
        limit_integrated <- .call_llm(prompt = prompt_limit_int,
                                      system_prompt = system_prompt,
                                      LLM_model = LLM_model,
                                      max_tokens = max_tokens,
                                      update_key = update_key,
                                      custom_llm_fn = custom_llm_fn)

        variables_imp <- gsub("\n\n", "\n", limit_integrated$output)  # replace empty lines from LLM output
        raw5_LLM <- c(prompt = prompt_limit_int, system_prompt = system_prompt, limit_integrated$raw_content)
        logprobs5 <- limit_integrated$top5_tokens

      }, error = function(e){
        cat(paste0("Warning: Limiting integrated list not possible -> ", e$message, "."),
            "Only part of the output is returned.", sep = "\n")
      })

      #tryCatch in case processing steps fail the raw output will still be outputted
      tryCatch({
        #Clean the output text
        split_text <- strsplit(variables_imp, split = "\n") # Split the string by newline character
        variables_f <- lapply(split_text, function(x) gsub("\\*\\*", "", sub("^\\s*([0-9]+[.)]|[-*])\\s+", "", x)))
        variables_f <- lapply(variables_f, function(x) trimws(x))
        variables_f <- lapply(variables_f, function(x) x[nzchar(x)]) # remove empty lines

      }, error = function(e) {
        cat(paste0("Warning: Unable to process LLM output -> ", e$message, "."),
            "Only part of the output is returned.", sep = "\n")
      })

    } else {
      variables_f <- variables
      limit_integrated <- NULL
      raw5_LLM <- NULL
      logprobs5 <- NULL
    }

  }, error = function(e){
    cat(paste0("Warning: Unable to process LLM output -> ", e$message, "."),
        "Only part of the output is returned.", sep = "\n")
  })


  # Initialize the output list
  output <- list()

  # Add raw_LLM to output
  tryCatch({
    # Build one row of raw LLM output (missing fields after an error or a skipped step become NA)
    raw_row <- function(r, part, it) {
      data.frame(function_part = part,
                 iteration = it,
                 LLM_model = .val(r$LLM_model),
                 prompt = .val(r$prompt),
                 system_prompt = .val(r$system_prompt),
                 content = .val(r$content),
                 finish_reason = .val(r$finish_reason),
                 prompt_tokens = .val(r$prompt_tokens),
                 answer_tokens = .val(r$answer_tokens),
                 total_tokens = .val(r$total_tokens),
                 error = if (is.null(r$error)) NA else as.character(unlist(r$error)[1]),
                 stringsAsFactors = FALSE)
    }

    raw_rows <- list()
    for (i in seq_along(raw1_LLM)) {
      raw_rows[[length(raw_rows) + 1]] <- raw_row(raw1_LLM[[i]], "create_lists", i)
    }
    raw_rows[[length(raw_rows) + 1]] <- raw_row(raw2_LLM, "integrate_lists", 1)
    raw_rows[[length(raw_rows) + 1]] <- raw_row(raw3_LLM, "add_vars", 1)
    raw_rows[[length(raw_rows) + 1]] <- raw_row(raw4_LLM, "clean_list", 1)
    raw_rows[[length(raw_rows) + 1]] <- raw_row(raw5_LLM, "limit_list", if (is.null(raw5_LLM)) 0 else 1)

    # Combine all rows
    raw_LLM_df <- do.call(rbind, raw_rows)

    output$raw_LLM <- raw_LLM_df

  }, error = function(e) {
    cat(paste0("Warning: Unable to return raw LLM output -> ", e$message, "."),
        "Only part of the output is returned.", sep = "\n")

  })

  tryCatch({
    # Add cleaned list to output
    output$all_vars <- variables[[1]]

    # Add final_list to output
    output$final_list <- variables_f[[1]]

    # Include topic if include_topic = TRUE
    if (include_topic == TRUE) {
      output$final_list <- c(output$final_list, topic)
    }

    if (length(variables[[1]]) > n_final) {
      message("Total of LLM prompts: 6")

    } else {
      message("Total of LLM prompts: 5")

    }
  }, error = function(e) {
    cat(paste0("Warning: Unable to return raw LLM output -> ", e$message, "."),
        "Only part of the output is returned.", sep = "\n")

  })

  # give API error if no variables were produced at all
  if (length(output$all_vars) == 0) {
    errs <- unlist(c(lapply(raw1_LLM, function(r) r$error$message),
                     list(raw2_LLM$error$message, raw3_LLM$error$message,
                          raw4_LLM$error$message, raw5_LLM$error$message)))
    if (length(errs) > 0) stop(errs[1], call. = FALSE)
  }

  return(output)
}
