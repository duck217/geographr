test_that("tenure ridge plot works", {
  dat <- load_data()

  plot_simple <- plot_tenure_ridge(regimes = "Military dictatorship",
                                   data = dat)

  plot_multiple <- plot_tenure_ridge(
    regimes = c("Military dictatorship", "Presidential democracy"),
    data = dat)

  expect_s3_class(plot_simple, "ggplot")
  expect_s3_class(plot_multiple, "ggplot")
}
)
