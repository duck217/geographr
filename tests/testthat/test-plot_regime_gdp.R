test_that("regime gdp plot works", {

  dat <- load_data()

  plot_simple <- plot_regime_gdp(
    data = dat
  )

  plot_start_year <- plot_regime_gdp(
    data = dat,
    start_year = 2000
  )

  plot_end_year <- plot_regime_gdp(
    data = dat,
    end_year = 2020
  )

  plot_all <- plot_regime_gdp(
    data = dat,
    start_year = 2000,
    end_year = 2020
  )

  expect_s3_class(plot_simple, "ggplot")
  expect_s3_class(plot_start_year, "ggplot")
  expect_s3_class(plot_end_year, "ggplot")
  expect_s3_class(plot_all, "ggplot")
})
