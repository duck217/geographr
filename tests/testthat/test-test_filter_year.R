test_that("filter_year filters correctly", {
  dat <- tibble::tibble(
    year = c(1990, 2000, 2005, 2020, 2025),
    value = 1:5)

  result <- filter_year(dat, start_year = 2000, end_year = 2020)

  expect_equal(result$year, c(2000, 2005, 2020))
})

test_that("filter_year returns the desired number of rows", {
  dat <- tibble::tibble(
    year = c(1990, 2000, 2005, 2020, 2025))

  result <- filter_year(dat, 2000, 2020)

  expect_equal(nrow(result), 3)

  test_that("filter_year includes start and end years", {
    dat <- tibble::tibble(
      year = c(1999, 2000, 2020, 2021))

    result <- filter_year(dat, 2000, 2020)

    expect_true(all(c(2000, 2020) %in% result$year))
  })
})
