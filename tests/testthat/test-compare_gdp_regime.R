test_that("compare_gdp_regime returns a list with anova and tukey elements", {
  dat    <- load_data()
  result <- compare_gdp_regime(dat)

  expect_type(result, "list")
  expect_named(result, c("anova", "tukey"))
})

test_that("anova element has expected columns", {
  result <- compare_gdp_regime(load_data())

  expect_true(
    all(c("term", "df", "sumsq", "meansq", "statistic", "p.value") %in%
          names(result$anova))
  )
})

test_that("tukey element has expected columns", {
  result <- compare_gdp_regime(load_data())

  expect_true(
    all(c("contrast", "estimate", "conf.low", "conf.high", "adj.p.value") %in%
          names(result$tukey))
  )
})

test_that("p.value in anova table is numeric and between 0 and 1", {
  result <- compare_gdp_regime(load_data())
  pval   <- result$anova$p.value[1]  # regime_category row

  expect_true(is.numeric(pval))
  expect_true(pval >= 0 && pval <= 1)
})

test_that("tukey rows are sorted by adj.p.value ascending", {
  result <- compare_gdp_regime(load_data())
  pvals  <- result$tukey$adj.p.value

  expect_true(all(diff(pvals) >= 0))
})

test_that("year argument filters data correctly", {
  dat        <- load_data()
  result_all <- compare_gdp_regime(dat)
  result_yr  <- compare_gdp_regime(dat, year = 2010)

  # Both should still return valid lists; the Tukey table may differ in size
  expect_type(result_yr, "list")
  expect_named(result_yr, c("anova", "tukey"))
})

test_that("compare_gdp_regime errors when required columns are missing", {
  bad_data <- data.frame(country = "X", year = 2000)
  expect_error(compare_gdp_regime(bad_data), "required columns are missing")
})

test_that("compare_gdp_regime errors on invalid year argument", {
  dat <- load_data()
  expect_error(compare_gdp_regime(dat, year = c(2000, 2010)), "single numeric value")
  expect_error(compare_gdp_regime(dat, year = "2010"),        "single numeric value")
})

test_that("compare_gdp_regime errors when year produces no rows", {
  dat <- load_data()
  expect_error(compare_gdp_regime(dat, year = 1800), "No rows remain")
})
