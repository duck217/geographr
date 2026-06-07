#' Load geopolitical data
#'
#' Loads the geopolitical meta dataset used in projects 1 & 2 consisting of economic & health data as well as the original democracy data
#'
#' @return A tibble containing country-level data
#' @export
#'
#' @import readr
load_data <- function() {
  read_csv(
    system.file("meta_data.csv", package = "geographr"),
    show_col_types = FALSE)
}
