# Program Name: CLDassist
# Description: The cld function can be used to quickly generate a Causal Loop Diagram (CLD).
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
### CLD function

## Function manual

#' Quickly create a CLD using a LLM
#'
#' @description
#' The \code{cld()} function can be used to quickly generate a Causal Loop Diagram (CLD). This function uses the \code{\link{causal_relation}}, \code{\link{causal_direction}}, \code{\link{causal_sign}}, and \code{\link{cld_plot}} functions included in this R package. For greater control and access to more outputs, we recommend using these functions individually.
#'
#' @usage
#' cld(topic,
#'     variable_list,
#'     plot = TRUE,
#'     LLM_model = "gpt-4o",
#'     max_tokens = 2000,
#'     update_key = FALSE,
#'     custom_llm_fn = NULL)
#'
#' @details
#' To create a CLD from scratch, the functions in this R-package should be used in the following order:
#'
#' \code{\link{var_list}} --> \code{\link{causal_relation}} --> \code{\link{causal_direction}} --> \code{\link{causal_sign}} --> \code{\link{cld_plot}} --> \code{\link{search_literature}}
#'
#' @param topic A character vector specifying the topic for which a CLD should be developed. If it is not feasible to identify a particular topic, the parameter can be set to NULL.
#' @param variable_list A vector containing all variables that need to be included in the CLD.
#' @param plot If \code{plot = TRUE} (default), the function will generate a network plot visualizing the edge list. If \code{plot = FALSE}, the edge list is still returned but not plotted.
#' @inheritParams var_list
#'
#' @returns
#' If no variable pair has a probability of a causal relationship above 50\%, the function stops after \code{\link{causal_relation}} and returns \code{raw_LLM} and \code{relation_df}. If no causal direction has a probability above 50\%, it stops after \code{\link{causal_direction}} and returns \code{raw_LLM} and \code{direction_df}. Otherwise, the output is:
#' \itemize{
#'   \item \code{raw_LLM}: A dataframe containing the unprocessed LLM output along with some other LLM information, including:
#'     \itemize{
#'       \item \code{which_fun}: Which function the raw output comes from.
#'       \item \code{var}: Which variable is put in the prompt as cause variable. (NA for causal_relation function)
#'       \item \code{relationship}: Which variable pair.
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
#'     }
#' } \cr
#' \itemize{
#'   \item \code{sign_df}: A dataframe with nine columns:
#'     \itemize{
#'       \item \code{var1}: Variable 1 of a unique variable pair.
#'       \item \code{var2}: Variable 2 of a unique variable pair.
#'       \item \code{prob_causal}: Probability of the presence of a causal relationship between var1 and var2.
#'       \item \code{prob_var1_cause}: Probability of var1 being a cause variable.
#'       \item \code{prob_var1_pos}: Probability of a positive relationship where var1 is the cause.
#'       \item \code{prob_var1_neg}: Probability of a negative relationship where var1 is the cause.
#'       \item \code{prob_var2_cause}: Probability of var2 being a cause variable.
#'       \item \code{prob_var2_pos}: Probability of a positive relationship where var2 is the cause.
#'       \item \code{prob_var2_neg}: Probability of a negative relationship where var2 is the cause.
#'     }
#' } \cr
#' \itemize{
#'   \item \code{edge_list}: The \code{sign_edge_list} created by \code{\link{cld_plot}} (for more information about edge lists see \code{\link{cld_plot}}). An edge list with 5 columns:
#'     \itemize{
#'       \item \code{from}: Cause variable.
#'       \item \code{to}: Dependent variable.
#'       \item \code{weight}: The combined probability (the probability of a causal relationship multiplied by the direction probability).
#'       \item \code{sign}: Sign of the causal relationship (can be either "Positive", "Negative", or "Uncertain").
#'       \item \code{color}: Color of the edge in the plot (blue for positive, red for negative, black for uncertain).
#'     }
#' }
#'
#' @references \url{https://platform.openai.com}
#'
#' @author Meike Waaijers
#'
#' @note The function and its output should be approached with caution. Depending on the specific LLM used, there may be financial implications. Furthermore, we wish to emphasise that the answers generated by an LLM should not be taken as absolute truth.
#'
#' @seealso
#' \code{\link{var_list}},
#' \code{\link{causal_relation}},
#' \code{\link{causal_direction}},
#' \code{\link{causal_sign}},
#' \code{\link{cld_plot}},
#' \code{\link{search_literature}}
#'
#' @examples
#' \dontrun{
#' ## Example input (topic = "addiction")
#' data("vars")
#' vars$final_list
#'
#' #---------------------------------------------------------------------------
#' ## Default
#' # For a readily available, pre-made output example see: data("cld_example")
#'
#' cld_example <- cld(topic = "addiction",
#'                    variable_list = vars$final_list)
#'
#' # Check output
#' cld_example$sign_df
#' cld_example$edge_list
#' }
#'
#' @import httr
#' @import qgraph
#' @import keyring
#' @import shiny
#' @export


## cld function
cld <- function(topic,
                variable_list,
                plot = TRUE,
                LLM_model = "gpt-4o",
                max_tokens = 2000,
                update_key = FALSE,
                custom_llm_fn = NULL) {

  # Check if running in Shiny
  is_shiny <- shiny::isRunning()

  # Validate input
  stopifnot(
    "'topic' should be a single non-empty character string or NULL." =
      is.null(topic) ||
      (is.character(topic) && length(topic) == 1L && !is.na(topic) && nzchar(trimws(topic)))
  )
  stopifnot("'variable_list' should be a vector containing more than one variables." = is.vector(variable_list) && length(variable_list) > 1)
  stopifnot("All entries in 'variable_list' should be character strings." = all(sapply(variable_list, is.character)))
  stopifnot("'plot' should be a logical value." = is.logical(plot))
  .validate_llm_args(LLM_model, max_tokens, custom_llm_fn)
  stopifnot("'update_key' should be a logical value." = is.logical(update_key))

  if (!is.null(custom_llm_fn)) {
    stopifnot("'custom_llm_fn' must be a function accepting 'prompt' and 'system_prompt'." =
                is.function(custom_llm_fn) &&
                all(c("prompt", "system_prompt") %in% names(formals(custom_llm_fn))))
    if (update_key) message("'update_key' is ignored when 'custom_llm_fn' is provided.")
    message("'LLM_model' and 'max_tokens' are ignored when 'custom_llm_fn' is provided.")
    message("Note: this function requires '$top5_tokens' in the custom function output to calculate probabilities. Without it, only raw LLM output will be returned.")
  }


  # Initialize variables
  causrel <- NULL
  causdir <- NULL
  caussign <- NULL
  cldplot <- NULL

  # Message when a step is skipped (also shown as a notification in the Shiny app)
  skip_message <- function(msg) {
    message(msg)
    if (is_shiny) shiny::showNotification(msg, type = "warning", duration = NULL)
  }

  # Combine the raw LLM output of the steps that were run
  combine_raw <- function() {
    parts <- list()
    if (!is.null(causrel$raw_LLM)) parts[[length(parts) + 1]] <- cbind(which_fun = "causal_relation", var = NA, causrel$raw_LLM)
    if (!is.null(causdir$raw_LLM)) parts[[length(parts) + 1]] <- cbind(which_fun = "causal_direction", causdir$raw_LLM)
    if (!is.null(caussign$raw_LLM)) parts[[length(parts) + 1]] <- cbind(which_fun = "causal_sign", caussign$raw_LLM)
    tryCatch(do.call(rbind, parts), error = function(e) {
      if (is_shiny) {
        shiny::req(FALSE, paste0("Error in raw LLM data processing: ", e$message))
      } else {
        stop(paste0("Error in raw LLM data processing: ", e$message))
      }
    })
  }

  # Step 1: causal_relation
  tryCatch({
    causrel <- causal_relation(topic = topic,
                               variable_list = variable_list,
                               LLM_model = LLM_model,
                               max_tokens = max_tokens,
                               update_key = update_key,
                               custom_llm_fn = custom_llm_fn)
    update_key <- FALSE
  }, error = function(e) {
    if (is_shiny) {
      shiny::req(FALSE, paste0("Error in causal_relation: ", e$message))
    } else {
      stop(paste0("Error in causal_relation: ", e$message))
    }
  })

  # Stop early (and return what was found) if no variable pair is likely to be causal
  if (!any(causrel$relation_df$prob_causal > 50)) {
    skip_message("No variable pair has a probability of a causal relationship above 50%, so the direction and sign steps and the plot are skipped. The results so far are returned.")
    return(list(raw_LLM = combine_raw(),
                relation_df = causrel$relation_df))
  }

  # Step 2: causal_direction
  tryCatch({
    causdir <- causal_direction(topic = topic,
                                relation_df = causrel$relation_df,
                                LLM_model = LLM_model,
                                max_tokens = max_tokens,
                                update_key = update_key,
                                custom_llm_fn = custom_llm_fn)
  }, error = function(e) {
    if (is_shiny) {
      shiny::req(FALSE, paste0("Error in causal_direction: ", e$message))
    } else {
      stop(paste0("Error in causal_direction: ", e$message))
    }
  })

  # Stop early (and return what was found) if no causal direction is likely
  if (!any(causdir$direction_df$prob_var1_cause > 50 | causdir$direction_df$prob_var2_cause > 50)) {
    skip_message("No causal direction has a probability above 50%, so the sign step and the plot are skipped. The results so far are returned.")
    return(list(raw_LLM = combine_raw(),
                direction_df = causdir$direction_df))
  }

  # Step 3: causal_sign
  tryCatch({
    caussign <- causal_sign(topic = topic,
                            direction_df = causdir$direction_df,
                            LLM_model = LLM_model,
                            max_tokens = max_tokens,
                            update_key = update_key,
                            custom_llm_fn = custom_llm_fn)
  }, error = function(e) {
    if (is_shiny) {
      shiny::req(FALSE, paste0("Error in causal_sign: ", e$message))
    } else {
      stop(paste0("Error in causal_sign: ", e$message))
    }
  })

  # Step 4: cld_plot (edge list is always created; plot only if plot = TRUE)
  tryCatch({
    cldplot <- cld_plot(topic = topic,
                        sign_df = caussign$sign_df,
                        plot = plot,
                        legend = plot)
  }, error = function(e) {
    if (is_shiny) {
      shiny::req(FALSE, paste0("Error in cld_plot: ", e$message))
    } else {
      stop(paste0("Error in cld_plot: ", e$message))
    }
  })

  # Output
  output <- list()
  output$raw_LLM <- combine_raw()
  output$sign_df <- caussign$sign_df
  output$edge_list <- cldplot$sign_edge_list

  return(output)
}
