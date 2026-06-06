#' Plot mean GDP per capita by regime group
#'
#' @param data Raw data with year, regime_category, and gdp_per_capita_nominal
#' @param start_year First year to include
#' @param end_year Last year to include
#'
#' @return A ggplot object
#' @export
#'
#' @importFrom dplyr filter group_by mutate if_else n first ungroup summarize
#' @importFrom ggplot2 ggplot aes geom_area scale_x_continuous scale_y_continuous
#' @importFrom ggplot2 facet_wrap labs theme_minimal theme element_blank
#' @importFrom scales comma
plot_regime_gdp <- function(data, start_year = 2000, end_year = 2020) {

  plot_data <- data |>
    filter(
      !is.na(gdp_per_capita_nominal),
      regime_category != "NA"
    ) |>
    group_by(regime_category) |>
    mutate(
      regime_group = if_else(
        n() < 100,
        "Territories & Colonies",
        first(regime_category)
      )
    ) |>
    ungroup() |>
    filter_year(
      start_year = start_year,
      end_year = end_year
    ) |>
    group_by(year, regime_group) |>
    summarize(
      mean_gdp = mean(gdp_per_capita_nominal, na.rm = TRUE),
      .groups = "drop"
    )

  ggplot(
    data = plot_data,
    mapping = aes(
      x = year,
      y = mean_gdp,
      fill = regime_group
    )
  ) +
    geom_area(alpha = 0.7) +
    scale_x_continuous(
      breaks = seq(start_year, end_year, by = 5)
    ) +
    scale_y_continuous(
      labels = comma
    ) +
    facet_wrap(~regime_group, scales = "fixed") +
    labs(
      x = "",
      y = "Mean GDP per Capita (USD)",
      title = "Mean GDP per Capita by Regime Type",
      subtitle = paste0("Averaged across countries, ", start_year, "–", end_year),
      fill = "Regime Type"
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",
      panel.grid.minor = element_blank()
    )
}
