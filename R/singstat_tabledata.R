#' Access Singstat Tabledata Endpoint
#'
#' @description Constructs a request to the SingStat Table Builder Tabledata Endpoint and sends a GET request.
#' Returns a dataframe containing the variables and values of the requested table. Selected metadata about the table as
#' a whole are saved as attributes to this dataframe. This metadata may differ from what is returned by
#' \code{singstat_metadata} for the same resourceID.
#'
#' @param resourceID Compulsory argument. The resource ID for the table to request data for.
#' @param print_info Defaults to \code{TRUE}. When \code{TRUE}, prints the table metadata for the whole table.
#' @param variables A character vector specifying the variable codes one wants to obtain. Defaults to \code{NULL}, in which
#'     case all variables in the table are returned.
#' @param between A numeric vector of length 2. Specifies the start and end points (inclusive) of a range of data values
#'     to return. Values outside this range are filtered out Defaults to \code{NULL}, in which no filter is applied.
#' @param sort_by The data frame will give each variable, for each year, a single row as per the default object returned by the API.
#'     This will be sorted by variableCode by default in ascending order.
#'     One can also sort by time, value, level, or variable name, in ascending or descending order. See details.
#' @param offset Numeric variable. Can specify the first n number of records to be excluded in the returned result. Defaults
#'     to \code{NULL}.
#' @param limit the maximum number of records to beincluded in the returned result. Maximum provided by API is 2000.
#' @param time_filter A character vector. Returns records of specific time points (not a range) based on the type of selected table.
#'     See details.
#' @param var_search A character argument. API will returns only records of variables with a name containing the search string.
#'
#' @details
#' \code{sort_by} accepts a character argument of a specific format. One must specify the column to sort (time, level, value
#'     value, variableCode, variableName) followed by a space, then asc (for ascending) or desc (for descending). E.g.
#'     "variableCode asc" or "value desc"
#'
#' \code{time_filter} accepts a character argument of a specific format. This differs depending on whether the table is monthly,
#'     half-yearly, quarterly, or annual. Examples below:
#'     \itemize{
#'       \item monthly - c("2018 Mar", "2019 Mar")
#'       \item quarterly - c("2017 4Q", "2018 1Q")
#'       \item half yearly - c("2017 H1", "2018 H2")
#'       \item annual - c("2017", "2018")
#'    }
#'
#' @return
#' A dataframe containing the requested table with 6 columns. Also provides information regarding the table itself,
#' stored as attributes to the returned dataframe. When \code{print_info} is \code{TRUE} (the default), these
#' attributes are also printed to the console for easy reference as a message.
#'
#' More details on the attributes containing table information:
#' \itemize{
#'  \item url - Direct link to the data on the SingStat Table Builder webclient (not the JSON file)
#'  \item uom - The unit of measure
#'  \item total - total number of rows
#'  \item footnote - Footnotes
#' }
#' More details on the information on variables, contained in the dataframe:
#' \itemize{
#'  \item frequency -The interval of measurement. Monthly, quarterly, half-yearly, or annual
#'  \item time - the instance of measurement
#'  \item level - Variables in a table are organised in a hierarchicalised manner. This can also be seen
#'      in the variable codes - child variables have a variable code derived from the parent variable code. The
#'      levels indicate what level of the hierarchy a variable belongs to, with 1 being the top level variable.
#'  \item variableCode -The code that identifies a specific variable in the table
#'  \item variableName - The name of the variable
#'  \item value - the value of the measurement
#'  \item uom - The unit of measure
#'  \item footnote - Footnotes
#'
#' }
#'
#' @seealso \code{singstat_tidytable}, \code{singstat_metadata}, \code{singstat_craft_table}
#'
#' @examples
#' singstat_tabledata(16112)
#' singstat_tabledata(15808, print_info = FALSE, offset = 5)
#'
#' @importFrom rlang .data
#'
#' @export



singstat_tabledata <- function(resourceID, print_info = TRUE, variables = NULL, between = NULL,
                               sort_by = "key asc", offset = NULL, limit = NULL, time_filter = NULL,
                               var_search = NULL) {

  #Define query
  queries <- list("sortBy" = sort_by)

  #Add all the other queries
  if (!is.null(variables)) {
    var_query <- paste0(variables, collapse = ",")

    #No spaces are allowed in the query
    var_query <- stringr::str_replace_all(var_query, pattern = "[[:space:]]", replacement = "")

    #Append to query lists
    queries[["variables"]] <- var_query
  }

  if (!is.null(between)) {
    if(!is.numeric(between) | length(between) != 2) {
      stop("between should be a numeric vector of length 2")
    } else {
      bet_query <- paste0(between, collapse = ",")

      #No spaces are allowed in the query
      bet_query <- stringr::str_replace_all(bet_query, pattern = "[[:space:]]", replacement = "")

      #Append to query lists
      queries[["between"]] <- bet_query
    }
  }

  if (!is.null(offset)) {
    queries[["offset"]] <- offset
  }

  if (!is.null(limit)) {
    queries[["limit"]] <- limit
  }

  if (!is.null(search)) {
    queries[["search"]] <- var_search
  }

  if (!is.null(time_filter)) {
    time_query <- paste0(time_filter, collapse = ",")

    #Append to query lists
    queries[["timeFilter"]] <- time_query
  }

  #HTTP request
  url <- paste0(singstat_endpoint("tabledata"), "/", resourceID)
  request <- httr::GET(url = url, query = queries)


  #Error checking for Get request
  httr::stop_for_status(request, task = "get a valid response from the server. Look up the HTTP error code in this message for more details")

  #Parse response object
  response <- jsonlite::fromJSON(httr::content(request, as = 'text'), simplifyDataFrame = TRUE)

  #More error checking just in case - it seems the API likes to return a 200 with its own error message in the response body
  if (!is.null(response$code)) {
    stop("The response from the server is good, but Singstat table builder has returned an error code. Details:",
         "\n",
         "Error Code: ",
         response$code,
         "\n",
         "Error Message: ",
         response$message
    )
  }

  #Error check for empty response
  if (length(response$Data$row) == 0) {
    stop("The query returned no results")
  }

  #Create return object
  df <- response$Data$row
  #Set attributes
  attr(df, "url") <- request$url
  attr(df, "footnotes") <- response$Data$footnote


  #print_info
  if (print_info == TRUE) {
    info <- paste(names(attributes(df)[4:6]), attributes(df)[4:6], sep = ": ")
    message(paste0(info, collapse = "\n"))
  }

  #return df
  df

}
