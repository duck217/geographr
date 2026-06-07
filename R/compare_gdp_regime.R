#' Compare GDP per capita across regime types using ANOVA
#'
#' Performs a one-way ANOVA to test whether mean GDP per capita differs
#' significantly across regime categories, then returns pairwise Tukey HSD
#' post-hoc comparisons so the caller can see which pairs of regime types
#' drive the differences.
#'
#' @param data A data frame containing at least `regime_category` and
#'   `gdp_per_capita_nominal` columns (e.g. the output of [load_data()]).
#' @param year Optional single numeric year to restrict the analysis to.
#'   If `NULL` (default), all years are used.
#'
#' @return A list with two elements:
#'   \describe{
#'     \item{anova}{A tidy data frame with the ANOVA table (term, df, sumsq,
#'       meansq, statistic, p.value).}
#'     \item{tukey}{A data frame of Tukey HSD pairwise comparisons with columns
#'       contrast, estimate, conf.low, conf.high, and adj.p.value.}
#'   }
#'
#' @examples
#' \dontrun{
#' dat <- load_data()
#' results <- compare_gdp_regime(dat)
#' results$anova
#' results$tukey
#'
#' results_2015 <- compare_gdp_regime(dat, year = 2015)
#' }
#'
#' @importFrom dplyr filter mutate
#' @importFrom stats aov TukeyHSD
#' @export
compare_gdp_regime <- function(data, year = NULL) {

  required_cols <- c("regime_category", "gdp_per_capita_nominal")
  missing_cols  <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(
      "The following required columns are missing from `data`: ",
      paste(missing_cols, collapse = ", ")
    )
  }

  if (!is.null(year)) {
    if (!"year" %in% names(data)) {
      stop("`data` must contain a `year` column when the `year` argument is supplied.")
    }
    if (!is.numeric(year) || length(year) != 1L) {
      stop("`year` must be a single numeric value.")
    }
    data <- dplyr::filter(data, .data[["year"]] == !!year)
    if (nrow(data) == 0L) {
      stop("No rows remain after filtering to year = ", year, ".")
    }
  }

  data <- dplyr::filter(
    data,
    !is.na(.data[["regime_category"]]),
    !is.na(.data[["gdp_per_capita_nominal"]])
  )

  if (nrow(data) < 2L) {
    stop("Fewer than 2 complete observations remain; cannot fit ANOVA.")
  }

  n_groups <- length(unique(data[["regime_category"]]))
  if (n_groups < 2L) {
    stop("At least 2 distinct regime categories are required to compare groups.")
  }

  data <- dplyr::mutate(
    data,
    regime_category = factor(.data[["regime_category"]])
  )

  fit <- stats::aov(gdp_per_capita_nominal ~ regime_category, data = data)

  anova_raw <- summary(fit)[[1L]]
  anova_tbl <- data.frame(
    term      = trimws(rownames(anova_raw)),
    df        = anova_raw[["Df"]],
    sumsq     = anova_raw[["Sum Sq"]],
    meansq    = anova_raw[["Mean Sq"]],
    statistic = anova_raw[["F value"]],
    p.value   = anova_raw[["Pr(>F)"]],
    stringsAsFactors = FALSE
  )

  tukey_raw <- stats::TukeyHSD(fit)[["regime_category"]]
  tukey_tbl <- data.frame(
    contrast    = rownames(tukey_raw),
    estimate    = tukey_raw[, "diff"],
    conf.low    = tukey_raw[, "lwr"],
    conf.high   = tukey_raw[, "upr"],
    adj.p.value = tukey_raw[, "p adj"],
    stringsAsFactors = FALSE,
    row.names   = NULL
  )

  tukey_tbl <- tukey_tbl[order(tukey_tbl[["adj.p.value"]]), ]

  list(anova = anova_tbl, tukey = tukey_tbl)
}
