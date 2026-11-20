# Program Name: CLDassist
# Description: The cld_plot function creates an edge list for each probability dataframe provided. These edge lists are then transformed into network graphs using the qgraph package.
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
### CLD visualization function

## Function manual

#' Create a Network Plot Showing All Causal Relationships
#'
#' @description
#' \code{cld_plot()} creates an edge list for each probability dataframe provided. These edge lists are then transformed into network graphs using the \code{\link[qgraph]{qgraph}} package. An edge list is a list containing all the links between nodes. In this function, these nodes symbolise variables, while the edges denote the causal relationships that link these variables.
#'
#' @usage
#' cld_plot(topic,
#'          relation_df = NULL,
#'          direction_df = NULL,
#'          sign_df = NULL,
#'          combine = TRUE,
#'          relation_threshold = 50,
#'          direction_threshold = 50,
#'          combine_threshold = 50,
#'          sign_threshold = 50,
#'          plot = TRUE,
#'          layout = "average",
#'          legend = TRUE,
#'          not_plot = FALSE)
#'
#' @details
#' To create a CLD from scratch, the functions in this R-package should be used in the following order:
#'
#' \code{\link{var_list}} --> \code{\link{causal_relation}} --> \code{\link{causal_direction}} --> \code{\link{causal_sign}} --> \code{\link{cld_plot}} --> \code{\link{search_literature}}
#'
#' @param topic A character vector specifying the topic for which a CLD should be developed. If it is not feasible to identify a particular topic, the argument can be set to NULL.
#' @param relation_df A dataframe with a unique pair of variables on each row and the probability of the existence of a causal relationship between these variables. (The \code{relation_df} output of the \code{\link{causal_relation}} function).
#' @param direction_df A dataframe with on every row a unique variable pair, the probability of the presence of a causal relationship between these variables, and the cause variable probability for each variable in the pair (The \code{direction_df} output from the \code{\link{causal_direction}} function).
#' @param sign_df A dataframe with on every row a unique variable pair, the probability of the presence of a causal relationship between these variables, the cause variable probability for each variable in the pair, and the probability of a positive or negative causal relationship (the \code{sign_df} output of the \code{\link{causal_sign}} function).
#' @param combine This argument has to be set when a \code{direction_df} or \code{sign_df} is inputted. If \code{combine = TRUE} (default), the probabilities of each variable being the cause are multiplied by the probability of a causal relationship. This gives the combined probability. If \code{combine = FALSE}, only the pure direction probabilities will be used as the edge weights without multiplying them with the relation probabilities.
#' @param relation_threshold A number (defaults to \code{50}) that indicates the minimum probability of a causal relationship (\code{prob_causal}) required for an edge to be shown in the plot. Only used when a \code{relation_df} is inputted.
#' @param direction_threshold A number (defaults to \code{50}) that indicates the minimum direction probability (\code{prob_var1_cause} or \code{prob_var2_cause}) required for an edge to be shown in the plot. Only used when a \code{direction_df} or \code{sign_df} is inputted and \code{combine = FALSE}.
#' @param combine_threshold A number (defaults to \code{50}) that indicates the minimum combined probability (the probability of a causal relationship multiplied by the direction probability) required for an edge to be shown in the plot. Only used when a \code{direction_df} or \code{sign_df} is inputted and \code{combine = TRUE}.
#' @param sign_threshold A number (defaults to \code{50}) that indicates the minimum probability of the most likely sign (positive or negative) required to assign that sign to an edge. If neither sign exceeds this threshold, the sign is set to "Uncertain".
#' @param plot If \code{plot = TRUE} (default), the function will generate network plot(s) visualizing the edge list(s). If \code{plot = FALSE} only the edge list will be created but it will not be visualized.
#' @param layout This argument controls the layout of the plot and is very similar to the \code{layout} argument of the \code{\link[qgraph]{qgraph}} package. If \code{layout = "average"} and multiple dataframes are inputted, all nodes will be placed in a joint layout (see \code{\link[qgraph:averageLayout]{averageLayout}}). If \code{layout = "average"} and only one dataframe is inputted, the plot will default to a force-embedded layout (same as the "spring" layout in the \code{\link[qgraph]{qgraph}} package). If \code{layout = "circle"}, all nodes will be placed in a single circle (see \code{\link[qgraph]{qgraph}}).
#' @param legend If \code{legend = TRUE} (default), the network plot(s) will include a legend. If variable names are extremely long, it may be advisable to set the legend argument to FALSE to maintain plot readability. This argument will be ignored if \code{plot = FALSE}.
#' @param not_plot If \code{not_plot = TRUE}, the function will generate a "not plot", which is a network plot that visualizes edges that fall below a specified threshold (either the \code{relation_threshold}, \code{direction_threshold}, or \code{combine_threshold}, depending on the input dataframe and the \code{combine} argument). This effectively visualizes the relationships that are considered to be non-causal.
#'
#' @returns
#' Either one, two, or three edge lists depending on how many probability dataframes were inputted:
#'
#' \itemize{
#'   \item \code{rel_edge_list}: Edge list for a relation probability dataframe.
#'   \item \code{dir_edge_list}: Edge list for a direction probability dataframe.
#'   \item \code{sign_edge_list}: Edge list for a sign probability dataframe.
#'   } \cr
#'
#' The specific edge list structure varies based on the input:
#' \itemize{
#'   \item All edge lists will have a minimum of three distinct columns:
#'     \itemize{
#'       \item \code{from}: The "from" node, otherwise known as the "cause" variable.
#'       \item \code{to}: The "to" node, otherwise known as the "dependent" variable.
#'       \item \code{weight}: The weight of the edge. For a relation dataframe, this is the probability of a causal relationship. For a direction or sign dataframe, this is the combined probability (\code{combine = TRUE}) or the direction probability (\code{combine = FALSE}). Note that the edge list contains all variable pairs; the thresholds only determine which edges are shown in the plot.
#'     } \cr
#'   \item When a sign probability dataframe is entered, the corresponding edge list will include two more columns:
#'     \itemize{
#'       \item \code{sign}: The sign of the causal relationship (can be either "Positive", "Negative", or "Uncertain").
#'       \item \code{color}: Color of the edge in the plot (blue for positive, red for negative, black for uncertain).
#'     }
#'   \item If \code{not_plot = TRUE} (and \code{plot = TRUE}), the edge list will have an additional column:
#'     \itemize{
#'       \item \code{NOT_weight}: Contains values used to construct the "not plot" visualization. Edges with weights below a specified threshold (determined by \code{relation_threshold}, \code{direction_threshold}, or \code{combine_threshold}, based on the input dataframe and the \code{combine} argument) are assigned a value of 100, while edges meeting the threshold are assigned a value of 0. This makes it possible to visualize the relationships that are considered to be non-causal.
#'     }
#' }
#'
#' @references \url{https://platform.openai.com}
#'
#' @author Meike Waaijers
#'
#' @note The edge lists and plots generated by this function are based on the output of a Large Language Model (LLM) and it's important to note that these answers do not necessarily represent an absolute truth. The function and its output should therefore be used with caution.
#'
#' @seealso
#' \code{\link{cld}},
#' \code{\link{var_list}},
#' \code{\link{causal_relation}},
#' \code{\link{causal_direction}},
#' \code{\link{causal_sign}},
#' \code{\link{search_literature}},
#' \code{\link[qgraph]{qgraph}}
#'
#' @examples
#' ## Example input (topic = addiction).
#' # Relation probability dataframe input
#' data("rel")
#' rel$relation_df
#'
#' # Direction probability dataframe input
#' data("dir")
#' dir$direction_df
#'
#' # Sign probability dataframe input
#' data("sign")
#' sign$sign_df
#'
#' #---------------------------------------------------------------------------
#' ## Create all three edge lists & plots with default settings.
#' # For a readily available, pre-made output example see: data("edge_lists")
#' edge_lists <- cld_plot(topic = "addiction",
#'                        relation_df = rel$relation_df,
#'                        direction_df = dir$direction_df,
#'                        sign_df = sign$sign_df)
#'
#' # Check output
#' edge_lists$rel_edge_list
#' edge_lists$dir_edge_list
#' edge_lists$sign_edge_list
#'
#' #---------------------------------------------------------------------------
#' \dontrun{
#' ## Create all three edge lists & plots with a "circle" layout.
#' edge_lists <- cld_plot(topic = "addiction",
#'                        relation_df = rel$relation_df,
#'                        direction_df = dir$direction_df,
#'                        sign_df = sign$sign_df,
#'                        layout = "circle")
#'
#' #---------------------------------------------------------------------------
#' ## Create an edge list only for a direction dataframe and do not use the combined probability.
#' # Plot with direction_threshold set to 10.
#' dir_edge_list <- cld_plot(topic = "addiction",
#'                           direction_df = dir$direction_df,
#'                           combine = FALSE,
#'                           direction_threshold = 10)
#'
#' #---------------------------------------------------------------------------
#' ## Create an edge list only for a sign dataframe and use the combined probability.
#' # Plot with combine_threshold set to 80.
#' combine_edge_list <- cld_plot(topic = "addiction",
#'                               sign_df = sign$sign_df,
#'                               combine = TRUE,
#'                               combine_threshold = 80)
#'
#' #---------------------------------------------------------------------------
#' ## Create a NOT plot for a relation dataframe
#' not <- cld_plot(topic = "addiction",
#'                 relation_df = rel$relation_df,
#'                 not_plot = TRUE)
#' }
#'
#' @import qgraph
#' @export


## cld_plot function
cld_plot <- function(topic,
                     relation_df = NULL,
                     direction_df = NULL,
                     sign_df =  NULL,
                     combine = TRUE,
                     relation_threshold = 50,
                     direction_threshold = 50,
                     combine_threshold = 50,
                     sign_threshold = 50,
                     plot = TRUE,
                     layout = "average",
                     legend = TRUE,
                     not_plot = FALSE) {

  #validate input
  stopifnot(
    "'topic' should be a single non-empty character string or NULL." =
      is.null(topic) ||
      (is.character(topic) && length(topic) == 1L && !is.na(topic) && nzchar(trimws(topic)))
  )
  if (is.null(relation_df) && is.null(direction_df) && is.null(sign_df)) {
    test <- FALSE
  } else {
    test <- TRUE
  }

  stopifnot("At least one dataframe should be inputted in the function."
            = test)
  stopifnot("'plot' should be a logical value." = is.logical(plot))
  stopifnot("'layout' should be either 'average' or 'circle'." =
              is.character(layout) && layout == "average" | is.character(layout) && layout == "circle")
  stopifnot("'legend' should be a logical value." = is.logical(legend))
  if (!plot) {
    legend <- FALSE # no legend needed when no plot is made
  }

  stopifnot("'not_plot' should be a logical value." = is.logical(not_plot))

  # Create topic for title
  if (is.null(topic)) {
    topic <- "unspecified"
  }

  # Create empty output so things can be added on later
  output <- NULL

  ## Relation df
  if (is.null(relation_df) == FALSE) {

    #validate input
    stopifnot("'relation_df' should be a dataframe." = is.data.frame(relation_df))
    stopifnot("'relation_df' should have three columns named 'var1', 'var2' and 'prob_causal'." =
                ncol(relation_df) == 3 && all(paste(c("var1", "var2", "prob_causal"), collapse = ", ") == paste(names(relation_df), collapse = ", ")))
    stopifnot("All entries in 'relation_df$var1' and 'relation_df$var2' should be character strings." =
                all(sapply(relation_df$var1, is.character)) && all(sapply(relation_df$var2, is.character)))
    stopifnot("All entries in 'relation_df$prob_causal' should be numeric and between 0 and 100." =
                all(sapply(relation_df$prob_causal, function(x) is.numeric(x) && x >= 0 && x <= 100)))
    stopifnot("At least one variable pair should be classified as 'causal'." = sum(relation_df$prob_causal) > 0)
    stopifnot("'relation_threshold' should be a number between 0 and 100, and cannot have more than two decimal points." =
                is.numeric(relation_threshold) && relation_threshold >= 0 && relation_threshold <= 100 && round(relation_threshold, 2) == relation_threshold)


    # get names of the variables
    var_1 <- relation_df$var1
    var_2 <- relation_df$var2

    # Transform prob_relation df to edge list
    edge_list_cdf <- data.frame()
    edge_index <- 1
    var_index <- 1
    for (y in relation_df$prob_causal) {
      edge_list_cdf[edge_index, 1] <- var_1[var_index]
      edge_list_cdf[edge_index, 2] <- var_2[var_index]
      edge_list_cdf[edge_index, 3] <- y
      edge_index <- edge_index + 1
      #also other way around to create undirected edgelist
      edge_list_cdf[edge_index, 1] <- var_2[var_index]
      edge_list_cdf[edge_index, 2] <- var_1[var_index]
      edge_list_cdf[edge_index, 3] <- y
      edge_index <- edge_index + 1
      var_index <- var_index + 1
    }

    all_vars <- unique(c(var_1, var_2))

    #include variables that do not have an edge with any other variable
    not_edge <- all_vars[which(all_vars %in% unique(c(edge_list_cdf[,1], edge_list_cdf[,2])) == FALSE)]


    for(g in not_edge) {
      edge_list_cdf[edge_index, 1] <- g
      edge_list_cdf[edge_index, 2] <- g
      edge_list_cdf[edge_index, 3] <- 0
      edge_index <- edge_index + 1
    }

    #change column names
    colnames(edge_list_cdf) <- c("from", "to", "weight")

    if (plot) {
      #create plot object for output
      node_df <- data.frame(Variables = all_vars, nn = 1:length(all_vars))

      edge_list_cdf2 <- edge_list_cdf

      edge_list_cdf2$from <- match(edge_list_cdf2$from, node_df$Variables)
      edge_list_cdf2$to <- match(edge_list_cdf2$to, node_df$Variables)


      if (not_plot) {

        not <- edge_list_cdf2
        not_index <- which(not$weight <= relation_threshold)
        yes_index <- which(not$weight > relation_threshold)
        not$weight[not_index] <- 100
        not$weight[yes_index] <- 0

        p.cdf <- qgraph::qgraph(not,
                                edge.color = "black",
                                threshold = relation_threshold,
                                fade = TRUE,
                                maximum = 100,
                                minimum = 0,
                                vsize = 5,
                                esize = 2,
                                labels = node_df[,2],
                                nodeNames = node_df[,1],
                                legend = legend,
                                legend.cex = 0.4,
                                legend.mode = "names",
                                GLratio = 2,
                                title = paste0("NOT plot (causal relations): ", topic),
                                title.cex = 1.2,
                                directed = TRUE,
                                bidirectional = TRUE,
                                arrows = FALSE,
                                DoNotPlot = TRUE)

        edge_list_cdf$NOT_weight <- not$weight


      } else if (!not_plot) {
        p.cdf <- qgraph::qgraph(edge_list_cdf2,
                                edge.color = "black",
                                threshold = relation_threshold,
                                fade = TRUE,
                                maximum = 100,
                                minimum = 0,
                                vsize = 5,
                                esize = 2,
                                labels = node_df[,2],
                                nodeNames = node_df[,1],
                                legend = legend,
                                legend.cex = 0.4,
                                legend.mode = "names",
                                GLratio = 2,
                                title = paste0("Causal relation plot: ", topic),
                                title.cex = 1.2,
                                directed = TRUE,
                                bidirectional = TRUE,
                                arrows = FALSE,
                                DoNotPlot = TRUE)

      }
    }

    output <- c(output, list(rel_edge_list = edge_list_cdf))

  }


  ## Direction df
  if (is.null(direction_df) == FALSE){
    stopifnot("'direction_df' should be a dataframe." = is.data.frame(direction_df))
    stopifnot("'direction_df' should have five columns named 'var1', 'var2', 'prob_causal', 'prob_var1_cause', 'prob_var2_cause'." =
                ncol(direction_df) == 5 && all(paste(c('var1', 'var2', 'prob_causal', 'prob_var1_cause', 'prob_var2_cause'), collapse = ", ") == paste(names(direction_df), collapse = ", ")))
    stopifnot("All entries in 'direction_df$var1' and 'direction_df$var2' should be character strings." =
                all(sapply(direction_df$var1, is.character)) && all(sapply(direction_df$var2, is.character)))
    stopifnot("All entries in 'direction_df$prob_causal' should be numeric and between 0 and 100." =
                all(sapply(direction_df$prob_causal, function(x) is.numeric(x) && x >= 0 && x <= 100)))
    stopifnot("All entries in 'direction_df$prob_var1_cause' and 'direction_df$prob_var2_cause' should be numeric and between 0 and 100." =
                all(sapply(direction_df$prob_var1_cause, function(x) is.numeric(x) && x >= 0 && x <= 100)) &&
                all(sapply(direction_df$prob_var2_cause, function(x) is.numeric(x) && x >= 0 && x <= 100)))
    stopifnot("At least one variable pair should be classified as 'causal'." = sum(direction_df$prob_causal) > 0)
    stopifnot("At least one variable should be classified as a 'cause variable'." = sum(direction_df[,c(4,5)]) > 0)
    stopifnot("'combine' should be a logical value." = is.logical(combine))

    if (combine){
      stopifnot("'combine_threshold' should be a number between 0 and 100, and cannot have more than two decimal points." =
                  is.numeric(combine_threshold) && combine_threshold >= 0 && combine_threshold <= 100 && round(combine_threshold, 2) == combine_threshold)
    } else {
      stopifnot("'direction_threshold' should be a number between 0 and 100, and cannot have more than two decimal points." =
                  is.numeric(direction_threshold) && direction_threshold >= 0 && direction_threshold <= 100 && round(direction_threshold, 2) == direction_threshold)
    }


    # get names of the variables
    var_1 <- direction_df$var1
    var_2 <- direction_df$var2

    dir_times_rel_var1 <- NULL
    dir_times_rel_var2 <- NULL

    if (combine){
      # times relation probability times direction probability to weight the edge
      dir_times_rel_var1 <- round((direction_df$prob_var1_cause * direction_df$prob_causal) / 100, 2)
      dir_times_rel_var2 <- round((direction_df$prob_var2_cause * direction_df$prob_causal) / 100, 2)
    } else {
      dir_times_rel_var1 <- direction_df$prob_var1_cause
      dir_times_rel_var2 <- direction_df$prob_var2_cause

    }

    # Transform prob_direction df to edge list
    edge_list_ddf <- data.frame()
    edge_index <- 1
    var_index <- 1
    for (h in dir_times_rel_var1) {
      edge_list_ddf[edge_index, 1] <- var_1[var_index]
      edge_list_ddf[edge_index, 2] <- var_2[var_index]
      edge_list_ddf[edge_index, 3] <- h
      edge_index <- edge_index + 1
      var_index <- var_index + 1
    }

    var_index <- 1
    for (f in dir_times_rel_var2) {
      edge_list_ddf[edge_index, 1] <- var_2[var_index]
      edge_list_ddf[edge_index, 2] <- var_1[var_index]
      edge_list_ddf[edge_index, 3] <- f
      edge_index <- edge_index + 1
      var_index <- var_index + 1
    }

    #include variables that do not have an edge with any other variable
    all_vars <- unique(c(var_1, var_2))
    not_edge <- all_vars[which(all_vars %in% unique(c(edge_list_ddf[,1], edge_list_ddf[,2])) == FALSE)]

    for(e in not_edge) {
      edge_list_ddf[edge_index, 1] <- e
      edge_list_ddf[edge_index, 2] <- e
      edge_list_ddf[edge_index, 3] <- 0
      edge_index <- edge_index + 1
    }

    #change column names
    colnames(edge_list_ddf) <- c("from", "to", "weight")

    if(plot) {
      #create plot object for output
      node_df <- data.frame(Variables = all_vars, nn = 1:length(all_vars))

      edge_list_ddf2 <- edge_list_ddf

      edge_list_ddf2$from <- match(edge_list_ddf2$from, node_df$Variables)
      edge_list_ddf2$to <- match(edge_list_ddf2$to, node_df$Variables)

      # set correct threshold
      if (combine){
        threshold <- combine_threshold
      } else {
        threshold <- direction_threshold
      }

      if (not_plot) {

        not <- edge_list_ddf2
        not_index <- which(not$weight <= threshold)
        yes_index <- which(not$weight > threshold)
        not$weight[not_index] <- 100
        not$weight[yes_index] <- 0

        p.ddf <- qgraph::qgraph(not,
                                threshold = threshold,
                                edge.color = "black",
                                fade = TRUE,
                                maximum = 100,
                                minimum = 0,
                                vsize = 5,
                                esize = 2,
                                labels = node_df[,2],
                                nodeNames = node_df[,1],
                                legend = legend,
                                legend.cex = 0.4,
                                legend.mode = "names",
                                GLratio = 2,
                                title = ifelse(combine,
                                               paste0("NOT plot (causal combined): ", topic),
                                               paste0("NOT plot (causal direction): ", topic)),
                                title.cex = 1.2,
                                DoNotPlot = TRUE)

        edge_list_ddf$NOT_weight <- not$weight


      } else if (!not_plot) {

        p.ddf <- qgraph::qgraph(edge_list_ddf2,
                                threshold = threshold,
                                edge.color = "black",
                                fade = TRUE,
                                maximum = 100,
                                minimum = 0,
                                vsize = 5,
                                esize = 2,
                                labels = node_df[,2],
                                nodeNames = node_df[,1],
                                legend = legend,
                                legend.cex = 0.4,
                                legend.mode = "names",
                                GLratio = 2,
                                title = ifelse(combine,
                                               paste0("Causal combined plot: ", topic),
                                               paste0("Causal direction plot: ", topic)),
                                title.cex = 1.2,
                                DoNotPlot = TRUE)

      }
    }

    output <- c(output, list(dir_edge_list = edge_list_ddf))

  }

  ## Sign df
  if (is.null(sign_df) == FALSE) {

    #validate input
    stopifnot("'sign_df' should be a dataframe." = is.data.frame(sign_df))
    stopifnot("'sign_df' should have nine columns named 'var1', 'var2', 'prob_causal', 'prob_var1_cause', 'prob_var1_pos', 'prob_var1_neg', 'prob_var2_cause', 'prob_var2_pos', and 'prob_var2_neg'." =
                ncol(sign_df) == 9 && all(paste(c("var1", "var2", "prob_causal", "prob_var1_cause", "prob_var1_pos", "prob_var1_neg", "prob_var2_cause", "prob_var2_pos","prob_var2_neg"), collapse = ", ") == paste(names(sign_df), collapse = ", ")))
    stopifnot("All entries in 'sign_df$var1' and 'sign_df$var2' should be character strings." =
                all(sapply(sign_df$var1, is.character)) && all(sapply(sign_df$var2, is.character)))
    stopifnot("All entries in 'sign_df$prob_causal' should be numeric and between 0 and 100." =
                all(sapply(sign_df$prob_causal, function(x) is.numeric(x) && x >= 0 && x <= 100)))
    stopifnot("All entries in 'sign_df$prob_var1_cause' and 'sign_df$prob_var2_cause' should be numeric and between 0 and 100." =
                all(sapply(sign_df$prob_var1_cause, function(x) is.numeric(x) && x >= 0 && x <= 100)) &&
                all(sapply(sign_df$prob_var2_cause, function(x) is.numeric(x) && x >= 0 && x <= 100)))
    stopifnot("All entries in 'sign_df$prob_var1_pos', 'sign_df$prob_var1_neg' should be numeric and between 0 and 100." =
                all(sapply(sign_df$prob_var1_pos, function(x) is.numeric(x) && x >= 0 && x <= 100)) && all(sapply(sign_df$prob_var1_neg, function(x) is.numeric(x) && x >= 0 && x <= 100)))
    stopifnot("All entries in 'sign_df$prob_var2_pos', 'sign_df$prob_var2_neg' should be numeric and between 0 and 100." =
                all(sapply(sign_df$prob_var2_pos, function(x) is.numeric(x) && x >= 0 && x <= 100)) && all(sapply(sign_df$prob_var2_neg, function(x) is.numeric(x) && x >= 0 && x <= 100)))
    stopifnot("At least one variable pair should be classified as 'causal'." = sum(sign_df$prob_causal) > 0)
    stopifnot("At least one variable should be classified as a 'cause variable'." = sum(sign_df[,c(4,7)]) > 0)
    stopifnot("'combine' should be a logical value." = is.logical(combine))

    if (combine){
      stopifnot("'combine_threshold' should be a number between 0 and 100, and cannot have more than two decimal points." =
                  is.numeric(combine_threshold) && combine_threshold >= 0 && combine_threshold <= 100 && round(combine_threshold, 2) == combine_threshold)
    } else {
      stopifnot("'direction_threshold' should be a number between 0 and 100, and cannot have more than two decimal points." =
                  is.numeric(direction_threshold) && direction_threshold >= 0 && direction_threshold <= 100 && round(direction_threshold, 2) == direction_threshold)
    }

    stopifnot("'sign_threshold' should be a number between 0 and 100, and cannot have more than two decimal points." =
                is.numeric(sign_threshold) && sign_threshold >= 0 && sign_threshold <= 100 && round(sign_threshold, 2) == sign_threshold)


    # get names of the variables
    var_1 <- sign_df$var1
    var_2 <- sign_df$var2

    dir_times_rel_var1_S <- NULL
    dir_times_rel_var2_S <- NULL

    if (combine){
      # times relation probability times direction probability to weight the edge
      dir_times_rel_var1_S <- round((sign_df$prob_var1_cause * sign_df$prob_causal) / 100, 2)
      dir_times_rel_var2_S <- round((sign_df$prob_var2_cause * sign_df$prob_causal) / 100, 2)
    } else {
      dir_times_rel_var1_S <- sign_df$prob_var1_cause
      dir_times_rel_var2_S <- sign_df$prob_var2_cause
    }


    # Transform sign_df to edge list
    edge_list_sdf_dir <- data.frame()
    edge_index <- 1

    # VAR 1
    rel_index <- 1

    for (q in dir_times_rel_var1_S){
      edge_list_sdf_dir[edge_index, 1] <- var_1[rel_index]
      edge_list_sdf_dir[edge_index, 2] <- var_2[rel_index]
      edge_list_sdf_dir[edge_index, 3] <- q

      #add sign color to edge list
      if (sign_df$prob_var1_pos[rel_index] > sign_df$prob_var1_neg[rel_index]) {
        if (sign_df$prob_var1_pos[rel_index] > sign_threshold) {
          edge_list_sdf_dir[edge_index, 4] <- "Positive"
          edge_list_sdf_dir[edge_index, 5] <- "blue"
        } else {
          edge_list_sdf_dir[edge_index, 4] <- "Uncertain"
          edge_list_sdf_dir[edge_index, 5] <- "black"
        }
      } else if (sign_df$prob_var1_neg[rel_index] > sign_df$prob_var1_pos[rel_index]) {
        if (sign_df$prob_var1_neg[rel_index] > sign_threshold) {
          edge_list_sdf_dir[edge_index, 4] <- "Negative"
          edge_list_sdf_dir[edge_index, 5] <- "red"
        } else {
          edge_list_sdf_dir[edge_index, 4] <- "Uncertain"
          edge_list_sdf_dir[edge_index, 5] <- "black"
        }
      } else if (sign_df$prob_var1_pos[rel_index] == sign_df$prob_var1_neg[rel_index]) {
        edge_list_sdf_dir[edge_index, 4] <- "Uncertain"
        edge_list_sdf_dir[edge_index, 5] <- "black"
      }
      edge_index <- edge_index + 1
      rel_index <- rel_index + 1

    }


    # VAR 2
    rel_index <- 1

    for (q in dir_times_rel_var2_S) {
      edge_list_sdf_dir[edge_index, 1] <- var_2[rel_index]
      edge_list_sdf_dir[edge_index, 2] <- var_1[rel_index]
      edge_list_sdf_dir[edge_index, 3] <- q

      #add sign color to edge list
      if (sign_df$prob_var2_pos[rel_index] > sign_df$prob_var2_neg[rel_index]) {
        if (sign_df$prob_var2_pos[rel_index] > sign_threshold) {
          edge_list_sdf_dir[edge_index, 4] <- "Positive"
          edge_list_sdf_dir[edge_index, 5] <- "blue"
        } else {
          edge_list_sdf_dir[edge_index, 4] <- "Uncertain"
          edge_list_sdf_dir[edge_index, 5] <- "black"
        }
      } else if (sign_df$prob_var2_neg[rel_index] > sign_df$prob_var2_pos[rel_index]) {
        if (sign_df$prob_var2_neg[rel_index] > sign_threshold) {
          edge_list_sdf_dir[edge_index, 4] <- "Negative"
          edge_list_sdf_dir[edge_index, 5] <- "red"
        } else {
          edge_list_sdf_dir[edge_index, 4] <- "Uncertain"
          edge_list_sdf_dir[edge_index, 5] <- "black"
        }
      } else if (sign_df$prob_var2_pos[rel_index] == sign_df$prob_var2_neg[rel_index]) {
        edge_list_sdf_dir[edge_index, 4] <- "Uncertain"
        edge_list_sdf_dir[edge_index, 5] <- "black"
      }
      edge_index <- edge_index + 1
      rel_index <- rel_index + 1
    }


    all_vars <- unique(c(var_1, var_2))

    #include variables that do not have an edge with any other variable
    not_edge <- all_vars[which(all_vars %in% unique(c(edge_list_sdf_dir[,1], edge_list_sdf_dir[,2])) == FALSE)]

    for(g in not_edge) {
      edge_list_sdf_dir[edge_index, 1] <- g
      edge_list_sdf_dir[edge_index, 2] <- g
      edge_list_sdf_dir[edge_index, 3] <- 0
      edge_list_sdf_dir[edge_index, 4] <- "-"
      edge_list_sdf_dir[edge_index, 5] <- "-"

      edge_index <- edge_index + 1
    }


    #change column names
    colnames(edge_list_sdf_dir) <- c("from", "to", "weight", "sign", "color")


    if (plot) {
      #create plot object for output
      node_df <- data.frame(Variables = all_vars, nn = 1:length(all_vars))

      edge_list_sdf_dir2 <- edge_list_sdf_dir

      edge_list_sdf_dir2$from <- match(edge_list_sdf_dir2$from, node_df$Variables)
      edge_list_sdf_dir2$to <- match(edge_list_sdf_dir2$to, node_df$Variables)

      # set correct threshold
      if (combine){
        threshold <- combine_threshold
      } else {
        threshold <- direction_threshold
      }

      if (not_plot) {

        not <- edge_list_sdf_dir2
        not_index <- which(not$weight <= threshold)
        yes_index <- which(not$weight > threshold)
        not$weight[not_index] <- 100
        not$weight[yes_index] <- 0

        p.sdf <- qgraph::qgraph(not[,1:3],
                                threshold = threshold,
                                edge.color = "black",
                                fade = TRUE,
                                maximum = 100,
                                minimum = 0,
                                vsize = 5,
                                esize = 2,
                                labels = node_df[,2],
                                nodeNames = node_df[,1],
                                legend = legend,
                                legend.cex = 0.4,
                                legend.mode = "names",
                                GLratio = 2,
                                title = ifelse(combine,
                                               paste0("NOT plot (causal combined & sign): ", topic),
                                               paste0("NOT plot (causal direction & sign): ", topic)),
                                title.cex = 1.2,
                                DoNotPlot = TRUE)

        edge_list_sdf_dir$NOT_weight <- not$weight

      } else if (!not_plot) {

        p.sdf <- qgraph::qgraph(edge_list_sdf_dir2[,1:3],
                                threshold = threshold,
                                edge.color = edge_list_sdf_dir2[,5],
                                fade = TRUE,
                                maximum = 100,
                                minimum = 0,
                                vsize = 5,
                                esize = 2,
                                labels = node_df[,2],
                                nodeNames = node_df[,1],
                                legend = legend,
                                legend.cex = 0.4,
                                legend.mode = "names",
                                GLratio = 2,
                                title = ifelse(combine,
                                               paste0("Causal combined & sign plot: ", topic),
                                               paste0("Causal direction & sign plot: ", topic)),
                                title.cex = 1.2,
                                DoNotPlot = TRUE)
      }
    }

    output <- c(output, list(sign_edge_list = edge_list_sdf_dir))

  }

  ## Create average layout
  # Define the qgraph objects
  qgraph_names <- c("p.cdf", "p.ddf", "p.sdf")
  existing_qgraphs <- qgraph_names[sapply(qgraph_names, exists, envir = environment())]

  # Check for the number of existing qgraphs
  num_existing_qgraphs <- length(existing_qgraphs)

  if (num_existing_qgraphs >= 2 && layout == "average") {
    layout <- do.call(qgraph::averageLayout, mget(existing_qgraphs))
  } else if (num_existing_qgraphs == 1 && layout == "average") {
    layout <- "spring"
  }

  # plot with average layout
  if (exists("p.cdf", envir = environment())) {
    p.cdf <- qgraph::qgraph(p.cdf, layout = layout, DoNotPlot = F)
  }

  if (exists("p.ddf", envir = environment())) {
    p.ddf <- qgraph::qgraph(p.ddf, layout = layout, DoNotPlot = F)
  }

  if (exists("p.sdf", envir = environment())) {
    p.sdf <- qgraph::qgraph(p.sdf, layout = layout, DoNotPlot = F)
  }


  return(output)
}

