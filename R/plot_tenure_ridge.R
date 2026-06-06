#' Plot presidential tenure by selected regime types
#'
#' Creates a density ridge plot showing the distribution of presidential tenure
#' for specific regime types
#'
#' @param data A data frame containing presidential tenure and regime data
#' @param regimes A character vector of regime categories to include
#'
#' @return A ggplot object.
#' @export
#'
#' @importFrom dplyr filter mutate
#' @importFrom stringr str_wrap
#' @importFrom ggplot2 ggplot aes after_stat labs theme_bw
#' @importFrom stats reorder median
#' @importFrom ggridges geom_density_ridges_gradient
#' @importFrom viridis scale_fill_viridis
plot_tenure_ridge <- function(regimes, data = load_data()) {
  tenure_data <- data |>
    mutate(
      president_tenure = year - president_accesion_year) |>
    filter(
      !is.na(president_tenure),
      !is.na(regime_category),
      is_presidential == TRUE,
      regime_category %in% regimes)

  tenure_data |>
    mutate(
      regime_category = str_wrap(regime_category, width = 25)) |>
    ggplot(
      aes(
        x = president_tenure,
        y = reorder(regime_category, president_tenure, FUN = median),
        fill = after_stat(x))) +
    geom_density_ridges_gradient(
      scale = 1.2,
      rel_min_height = 0.01,
      color = "white",
      alpha = 0.9) +
    scale_fill_viridis(option = "C") +
    labs(
      title = "Tenure by Presidential Regimes",
      x = "Tenure (years)",
      y = "Regime",
      fill = "Years") +
    theme_bw()
}
