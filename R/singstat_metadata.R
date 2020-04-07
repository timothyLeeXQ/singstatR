#' Access Singstat Metadata Endpoint
#'
#' @description Constructs a request to the SingStat Table Builder Metadata Endpoint and sends a GET request.
#' Returns a dataframe containing information about the table variables. Other metadata about the table as a whole
#' are saved as attributes to this dataframe.
#'
#' @param resourceID Compulsory argument. The resource ID for the table to request metadata for.
#' @param print_info Defaults to \code{TRUE}. When \code{TRUE}, prints the metadata for the whole table, also stored as
#'     attributes to the console as a message.
#' @param filter_level An integer argument. When not \code{NULL}, filters the dataframe to include only information on
#'     variables at the specified level, or above. See details.
#' @param filter_var A regex argument. When not \code{NULL}, filters the dataframe to include only variables with names
#'     containing the regex pattern. See details.
#' @param ignore_case Used with filter_var. When \code{TRUE}, filter_var is case-insensitive.
#'
#' @details
#' \code{filter_level} accepts an integer argument. If a numeric variable is passed, it is coerced into an integer using
#'     as.integer().
#'
#' \code{filter_var} is passed to stringr::str_detect as its pattern argument.
#'
#' Note that \code{filter_level} is executed before \code{filter_var}.
#'
#' @return
#' A dataframe containing metadata on individual variables in a SingStat table, based on its resource ID. In addition to
#' metadata on individual variables, the metadata API also provides information regarding the table itself. These are
#' stored as attributes to the returned dataframe. When \code{print_info} is \code{TRUE} (the default), these
#' attributes are also printed to the console for easy reference as a message.
#'
#' More details on the attributes containing table information:
#' \itemize{
#'  \item{\code{resourceID}} -The resourceID of the table.
#'  \item{\code{title}} - The table title
#'  \item{\code{frequency}} - The frequency of measurement
#'  \item{\code{uom}} - The unit of measure
#'  \item{\code{datasource}} - The source of the data
#'  \item{\code{url}} - Direct link to the data on the SingStat Table Builder webclient (not the JSON file)
#'  \item{\code{footnote}} - Footnotes
#'  \item{\code{startPeriod}} - The first year of data availability
#'  \item{\code{endPeriod}} - The most recent year of data availability
#'  \item{\code{termsOfUse}} - A link to the SingStat Table Builder Terms of Use.
#'  \item{\code{apiTermsOfService}} - A link to the Singapore Government's API Terms of Service.
#' }
#'
#'
#' More details on the information on variables, contained in the dataframe:
#' \itemize{
#'  \item{\code{variableCode}} -The code that identifies a specific variable in the table
#'  \item{\code{variableName}} - The name of the variable
#'  \item{\code{level}} - Variables in a table are organised in a hierarchicalised manner. This can also be seen
#'      in the variable codes - child variables have a variable code derived from the parent variable code. The
#'      levels indicate what level of the hierarchy a variable belongs to, with 1 being the top level variable.
#'  \item{\code{uom}} - The unit of measure
#'  \item{\code{footnote}} - Footnotes
#' }
#'
#'
#' @seealso \code{singstat_resource} \code{singstat_tabledata}
#'
#' @examples
#' singstat_metadata(16112)
#' singstat_metadata(16112, print_info = FALSE)
#'
#' singstat_metadata(16112, filter_level = 3)
#' singstat_metadata(16102, filter_var = "investment")
#'
#' @importFrom rlang .data
#'
#' @export


singstat_metadata <- function(resourceID, print_info = TRUE, filter_level = NULL, filter_var = NULL, ignore_case = TRUE) {

  #HTTP request
  url <- paste(singstat_endpoint("metadata"), resourceID, sep = "/", collapse = "")
  request <- httr::GET(url = url)

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

  #DF object for the variable info
  variable_info <- response$records$variables[[1]] %>% dplyr::arrange(.data$variableCode)

  #Level should be an integer
  variable_info$level <- as.integer(variable_info$level)

  #Set attributes - omit number 10 (download formats) since that is irrelevant for us - why would you want to know you can get
  #the file in XLSX when you're using an API?
  attribute_list <- response$records[c(1:9, 11, 12)]

  #Assign attributes
  for (attribute in seq_along(attribute_list)) {
    attr(variable_info, names(attribute_list[attribute])) <- attribute_list[[attribute]]
  }

  #These are years and really should be numeric
  attr(variable_info, "startPeriod") <- as.numeric(attr(variable_info, "startPeriod"))
  attr(variable_info, "endPeriod") <- as.numeric(attr(variable_info, "endPeriod"))

  #print_info
  if (print_info == TRUE) {
    info <- paste(names(attributes(variable_info)[4:14]), attributes(variable_info)[4:14], sep = ": ")
    message(paste0(info, collapse = "\n"))
  }

  #filter_level
  if (!is.null(filter_level)) {

    #Enforce integer input
    if (!is.integer(filter_level)) {
      as.integer(filter_level)
    }

    #Filter!
    variable_info <- dplyr::filter(variable_info, (.data$level <= filter_level))
  }

  #filter_var
  if (!is.null(filter_var)) {

    #Enforce regex input
    if (!is.character(filter_var)) {
      stop("value for filter_var should be a string. filter_var accepts a regex expression")
    }

    #Filter!
    variable_info <- dplyr::filter(variable_info,
                                   stringr::str_detect(.data$variableName,
                                                       pattern = stringr::regex(filter_var, ignore_case = ignore_case)
                                                       )
                                   )
  }

  #Return DF
  variable_info

}
