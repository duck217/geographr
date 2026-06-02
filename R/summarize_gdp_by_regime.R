#' Summarize GDP per capita by regime category
#'
#' Compute mean, median, and count of GDP per capita grouped by regime category, optionally filtered to
#' a specific year
#'
#' @param data A geopolitical data frame containing regime_category, gdp_per_capita_nominal and year
#' @param year Optional single numeric year to filter by; if NULL, use all years
#'
#' @return A dataframe/tibble with columns regime_category, mean_gdp, median_gdp, and n
#'
#' @export

summarize_gdp_by_regime <- function(data, year = NULL) {
  if (!is.null(year)) {
    data <- data |>
      dplyr::filter(year == {{ year }})
  }

  data |>
    dplyr::filter(!is.na(gdp_per_capita_nominal), regime_category != "NA") |>
    dplyr::group_by(regime_category) |>
    dplyr::summarize(
      mean_gdp   = mean(gdp_per_capita_nominal, na.rm = TRUE),
      median_gdp = median(gdp_per_capita_nominal, na.rm = TRUE),
      n          = dplyr::n(),
      .groups    = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(mean_gdp))
}
