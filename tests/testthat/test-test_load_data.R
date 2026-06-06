test_that("load_data returns a data frame", {
  dat <- load_data()

  expect_s3_class(dat, "data.frame")
})
