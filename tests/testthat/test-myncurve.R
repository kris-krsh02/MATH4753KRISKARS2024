library(testthat)

test_that("mu is correct", {
  result <- MATH4753KRISKARS2024::myncurve(mu = 10, sigma = 5, a = 6)
  expect_equal(result$mu, 10)
})

test_that("sigma is correct", {
  result <- MATH4753KRISKARS2024::myncurve(mu = 10, sigma = 5, a = 6)
  expect_equal(result$sigma, 5)
})

test_that("area is correct", {
  result <- MATH4753KRISKARS2024::myncurve(mu = 10, sigma = 5, a = 6)
  expect_true(is.numeric(result$area))
  expect_true(result$area > 0 && result$area < 1) # Ensure the area is a valid probability
})
