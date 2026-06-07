#' Summarize GDP per capita by democracy status
#'
#' Computes mean GDP per capita grouped by democracy vs non-democracy status
#' for each year. Uses data.table for performance.
#'
#' @param data A tibble with columns `is_democracy`, `gdp_per_capita_nominal`, and `year`.
#'
#' @return A tibble with columns `year`, `democracy`, and `mean_gdp`.
#'
#' @importFrom data.table as.data.table
#' @export
summarize_gdp_by_democracy <- function(data = load_data()) {
  prepped <- data |>
    dplyr::filter(!is.na(gdp_per_capita_nominal), !is.na(is_democracy)) |>
    dplyr::mutate(
      democracy = dplyr::if_else(is_democracy == 1, "Democracy", "Non-Democracy")
    )

  dt <- data.table::as.data.table(prepped)

  result <- dt[, list(mean_gdp = mean(gdp_per_capita_nominal, na.rm = TRUE)),
               by = list(year, democracy)]

  dplyr::as_tibble(result)
}
