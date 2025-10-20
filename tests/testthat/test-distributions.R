# Tests for distribution constructors

test_that("norm creates valid distribution spec", {
  dist <- norm(0, 1)

  expect_type(dist, "list")
  expect_equal(dist$type, "Normal")
  expect_equal(dist$params, c(0, 1))
})

test_that("norm validates inputs", {
  expect_error(norm("a", 1), "Must be of type 'number'")
  expect_error(norm(0, -1), "Element 1 is not >= 0")
})

test_that("truncnorm creates valid distribution spec", {
  dist <- truncnorm(0, 1, -2, 2)

  expect_type(dist, "list")
  expect_true(grepl("truncated", dist$type))
  expect_equal(length(dist$params), 4)
  expect_equal(dist$params[["mean"]], 0)
  expect_equal(dist$params[["lower"]], -2)
  expect_equal(dist$params[["upper"]], 2)
})

test_that("truncnorm validates bounds", {
  expect_error(truncnorm(0, 1, 2, -2), "Must be TRUE")
})

test_that("halfnorm creates valid distribution spec", {
  dist <- halfnorm(1)

  expect_type(dist, "list")
  expect_true(grepl("truncated", dist$type))
  expect_true(grepl("Normal", dist$type))
})

test_that("halfnorm validates inputs", {
  expect_error(halfnorm(-1), "Element 1 is not >= 0")
})

test_that("gamma_dist creates valid distribution spec", {
  dist <- gamma_dist(6.5, 0.62)

  expect_type(dist, "list")
  expect_equal(dist$type, "Gamma")
  expect_equal(length(dist$params), 2)
})

test_that("gamma_dist validates inputs", {
  expect_error(gamma_dist(-1, 1), "Element 1 is not >= 0")
  expect_error(gamma_dist(1, -1), "Element 1 is not >= 0")
})

test_that("lognorm creates valid distribution spec", {
  dist <- lognorm(1.6, 0.42)

  expect_type(dist, "list")
  expect_equal(dist$type, "LogNormal")
  expect_equal(dist$params, c(1.6, 0.42))
})

test_that("lognorm validates inputs", {
  expect_error(lognorm("a", 1), "Must be of type 'number'")
  expect_error(lognorm(0, -1), "Element 1 is not >= 0")
})

test_that("exponential creates valid distribution spec", {
  dist <- exponential(0.5)

  expect_type(dist, "list")
  expect_equal(dist$type, "Exponential")
  expect_equal(dist$params, 0.5)
})

test_that("exponential validates inputs", {
  expect_error(exponential(-1), "Element 1 is not >= 0")
})
