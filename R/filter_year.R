#' Filter data by year range
#'
#' @param data A data frame with a year column
#' @param start_year First year to include
#' @param end_year Last year to include
#'
#' @return A filtered data frame
#' @export
#'
#' @importFrom dplyr filter
filter_year <- function(data, start_year = 2000, end_year = 2020) {
  data |>
    filter(
      year >= start_year,
      year <= end_year
    )
}
