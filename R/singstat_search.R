#' Search for SingStat tables
#'
#' @description Passes each keyword provided to \code{singstat_resource}, making multiple calls to the resourceID API endpoint
#'     and aggregating the results
#'
#' @param keywords A character vector with each search query as an element.
#' @param search_options A character vector of the same length as \code{keywords} that provides the corresponding search option
#'     for a \code{singstat_resource} call using that keyword.
#' @param search_type A string argument that can be either "and" or "or" (defaults to "or"). If "or", results are returned
#'     if they are found for a search for any of the keywords. If "and", only results that match all valid queries (ones that did not
#'     throw an error) are returned.
#'
#' @return
#' A dataframe containing the aggregated search results. Format differs depending on search type. If "and", return object is similar
#'     to \code{singstat_resource}. If "or", a new column is made for each keyword showing if the particular table was returned during
#'     a search for that keyword.
#'
#' @seealso \code{singstat_resource}
#'
#' @examples
#' singstat_search(keywords = c("gdp", "government"),
#'                 search_options = c("all", "all"))
#'
#' singstat_search(keywords = c("gdp", "government"),
#'                 search_options = c("all", "all"),
#'                 search_type = "and")
#'
#' @importFrom rlang .data
#'
#' @export


singstat_search <- function(keywords, search_options, search_type = "or") {

  if (length(keywords) != length(search_options)) {
    stop("keywords and search_options are of unequal length. Each keyword must be paired with a search option.")
  }

  full_results <- vector(mode = "list", length = length(keywords))

  for (number in seq_along(keywords)) {

    keyword <- keywords[[number]]
    search_option <- search_options[[number]]

    #Use tryCatch() to deal with situations where singstat_resource would throw an error and stop the program
    result <- tryCatchLog::tryCatchLog({singstat_resource(keyword, searchOption = search_option)},
                                       error = function(cond) {
                                         cat('The search query "', keyword,
                                             '" returned an error. No results for this query are included. ', sep = ""
                                             )
                                         cat("See the error message log above:", as.character(cond), sep = "\n")

                                         return(data.frame(resourceId = NULL,
                                                           title = NULL))
                                         }, include.full.call.stack = FALSE
                                       )

    result <- result %>% dplyr::mutate(query = keyword)

    #Fill list with results
    full_results[[number]] <- result
  }

  #Bind into single dataframe
  df <- data.table::rbindlist(full_results, fill = TRUE)

  if (search_type == "or") {
    df <- df %>%
      dplyr::mutate(query1 = .data$query) %>%
      tidyr::pivot_wider(names_from = .data$query, values_from = .data$query1)

  } else if (search_type == "and") {

    #Find the valid queries from the full set provided
    valid_queries <- length(unique(df$query))

    df <- df %>% dplyr::group_by(.data$resourceId, .data$title) %>%
      dplyr::summarise(frequency = dplyr::n()) %>%
      dplyr::filter(.data$frequency == valid_queries) %>%
      dplyr::select(-.data$frequency)

  } else {
    stop('Invalid search type. Search type should be "or" or "and".')
  }

  #Return results
  df

}
