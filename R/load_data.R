#' Load geopolitical data
#'
#' Loads the geopolitical meta dataset used in projects 1 & 2 consisting of economic & health data as well as the original democracy data
#'
#' @return A tibble containing country-level data
#' @export
load_data <- function() {
  readr::read_csv(
    "https://raw.githubusercontent.com/jbened01/Project-1-Gov-Types/refs/heads/main/meta_data.csv",
    show_col_types = FALSE
  )
}
