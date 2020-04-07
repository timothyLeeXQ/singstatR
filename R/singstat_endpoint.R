#' Singstat endpoint URL generator
#'
#' @description Generate the URL of SingStat table builder API endpoints
#'
#' @param API_type Compulsory argument. The API endpoint that the URL should be constructed for. Accepts "resourceId", "metadata", or "tabledata".
#'
#' @return The URL of the API endpoint passed as the argument.
#'
#' @examples
#' \dontrun{
#' singstat_endpoint("resourceId")
#' singstat_endpoint("invalid_endpoint") #error}
#'
#'
singstat_endpoint <- function(API_type) {
  if (stringr::str_detect(API_type, stringr::regex("^resourceId$|^metadata$|^tabledata$", ignore_case = TRUE))) {
    base <- "https://www.tablebuilder.singstat.gov.sg/publicfacing/rest/timeseries"
    endpoint <- paste(base, API_type, sep = "/", collapse = "")
    endpoint
  } else {
      stop('Endpoint provided is not a valid API endpoint for SingStat Table Builder')
  }
}
