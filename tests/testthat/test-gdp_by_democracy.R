test_that("summarize_gdp_by_democracy returns expected columns", {
  result <- summarize_gdp_by_democracy(load_data())
  expect_true(all(c("year", "democracy", "mean_gdp") %in% names(result)))
})

test_that("summarize_gdp_by_democracy has exactly two groups per year", {
  result <- summarize_gdp_by_democracy(load_data())
  counts <- dplyr::count(result, year)
  expect_true(all(counts$n == 2))
})

test_that("summarize_gdp_by_democracy labels are Democracy and Non-Democracy", {
  result <- summarize_gdp_by_democracy(load_data())
  expect_equal(sort(unique(result$democracy)), c("Democracy", "Non-Democracy"))
})
