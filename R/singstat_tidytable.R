#' Get a tidy data frame from SingStat
#'
#' @description Passes arguments to \code{singstat_tabledata}, wrangling its return object into a data frame where the
#'     time period are observations and the variables are columns.
#'
#' @param resourceID Compulsory argument. The resource ID for the table to request data for.
#' @param print_info Defaults to \code{TRUE}. When \code{TRUE}, prints the table metadata for the whole table.
#' @param ... Additional arguments to pass to \code{singstat_tabledata}. See documentation for \code{singstat_tabledata} for more
#'     details.
#' @param force_annual Defaults to \code{FALSE}. When TRUE for monthly, quarterly, or half-yearly data, sums or finds the mean
#'     values of all the measurements for that year to give an annual value. The frequency column in the table is mutated to "Annual".
#' @param force_op String to indicate the aggregation function used in force_annual. Either "sum" or "mean". Defaults to "sum".
#' @param na.rm logical. Should missing values (including NaN) be removed? Passed to \code{sum} or \code{mean} for \code{force_annual}.

#'
#' @return
#' A dataframe containing the requested table with time period (year, and div for quarterly, monthly, and half-yearly tables),
#'     frequency of measurement, and table variables as columns.
#'
#' @seealso \code{singstat_tabledata}, \code{sum}, \code{mean}
#'
#' @examples
#' singstat_tidytable(16112)
#' singstat_tidytable(15808, print_info = FALSE)
#' singstat_tidytable(14506, force_annual = TRUE, na.rm = TRUE)
#'
#' @importFrom rlang .data
#'
#' @export


singstat_tidytable <- function(resourceID, print_info = TRUE, ..., force_annual = FALSE, force_op = "sum", na.rm = FALSE) {
  untidy_table <- singstat_tabledata(resourceID, print_info = print_info, ...)

  #Wrangle the data into a tidy format
  tidy_table <- dplyr::select(untidy_table, -c(.data$footnote, .data$level)) %>%
    tidyr::unite(col = "var", .data$variableCode, .data$variableName, .data$uom) %>%
    tidyr::spread(key = .data$var, value = .data$value) %>%
    dplyr::select(.data$frequency,
                  year = .data$time,
                  tidyselect::everything())

  #Split time into year and div, where div is months/quarters/semesters
  if (any(stringr::str_detect(tidy_table$year, pattern = "[[:alpha:]]"))) {
    tidy_table <- tidy_table %>% tidyr::separate(col = .data$year, into = c("year", "div")) %>%
      dplyr::mutate(div = as.factor(.data$div))

    #Force non-annual tables
    if (force_annual == TRUE) {

      #Wrangle the DF to group by the year
      if (force_op == "sum" | force_op == "mean") {
        tidy_table <- tidy_table %>% dplyr::select(-c(.data$frequency, .data$div)) %>%
          dplyr::group_by(.data$year)
        # then execute a summarise on all the variable columns, with the operation dependent on force_op
        if (force_op == "sum") {
          tidy_table <- tidy_table %>% dplyr::summarise_all(sum, na.rm = na.rm)

        } else if (force_op == "mean") {
          tidy_table <- tidy_table %>% dplyr::summarise_all(mean, na.rm = na.rm)

        }
        #Finally, slap on "annual" and reorder the columns so that it looks just like an annual table
        tidy_table <- tidy_table %>% dplyr::mutate(frequency = "Annual") %>%
          dplyr::select(.data$frequency,
                        .data$year,
                        tidyselect::everything())
      } else {
          warning("argument for force_op was invalid - force_annual will be aborted")
      }
    }
  } else if (force_annual == TRUE) {
    warning("force_annual is only for tables that do not have an annual frequency of measurement.
            There is no difference between the output here and the output if force_annual == FALSE")
  }

  #Set attributes
  attr(tidy_table, "url") <- attr(untidy_table, "url")
  attr(tidy_table, "uom") <- attr(untidy_table, "uom")
  attr(tidy_table, "total") <- attr(untidy_table, "total")
  attr(tidy_table, "footnotes") <- attr(untidy_table, "footnotes")

  #Return df
  tidy_table

}

