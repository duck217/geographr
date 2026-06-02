test_that("summarize_gdp_by_regime returns expected columns", {
  result <- summarize_gdp_by_regime(load_data())
  expect_s3_class(result, "tbl_df")
  expect_true(all(c("regime_category", "mean_gdp", "median_gdp", "n") %in% names(result)))
})

test_that("summarize_gdp_by_regime filters by year", {
  all_years <- summarize_gdp_by_regime(load_data())
  one_year  <- summarize_gdp_by_regime(load_data(), year = 2020)
  expect_true(sum(one_year$n) < sum(all_years$n))
})

test_that("summarize_gdp_by_regime returns sorted by mean_gdp descending", {
  result <- summarize_gdp_by_regime(load_data())
  expect_true(all(diff(result$mean_gdp) <= 0))
})

test_that("summarize_gdp_by_regime drops NA regime categories", {
  result <- summarize_gdp_by_regime(load_data())
  expect_false("NA" %in% result$regime_category)
  expect_false(any(is.na(result$regime_category)))
})

test_that("summarize_gdp_by_regime has no NA in mean_gdp", {
  result <- summarize_gdp_by_regime(load_data(), year = 2010)
  expect_false(any(is.na(result$mean_gdp)))
})
