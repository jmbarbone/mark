test_that("with_par() works", {
  skip_if_not(package_available("graphics"))
  skip_if_not(package_available("grDevices"))

  par0 <- graphics::par("mfrow")
  set.seed(42)
  df <- data.frame(a = stats::rnorm(100), b = stats::rnorm(100))
  # not testing for plot
  grDevices::dev.off()
  wuffle(with_par(
    graphics::plot(stats::lm(a ~ b, data = df)),
    ops = list(mfrow = c(2, 2))
  ))
  grDevices::dev.off()
  expect_identical(par0, graphics::par("mfrow"))
})
