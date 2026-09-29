test_that("nuts() validates settings", {
  sampler <- nuts(draws = 100, warmup = 50, chains = 2)
  expect_s3_class(sampler, "cidm_nuts")
  expect_identical(sampler$draws, 100L)
  expect_identical(sampler$ad, "forwarddiff")
  expect_error(nuts(draws = 0))
  expect_error(nuts(target_acceptance = 2))
  expect_error(nuts(ad = "zygote"))
  expect_output(print(sampler), "Chains: 2")
})

test_that("fit() validates inputs before starting Julia", {
  model <- IDModel(DirectInfections(Z = RandomWalk()), PoissonError())
  expect_error(fit(PoissonError(), 1:10), "composed model")
  expect_error(fit(model, "a"))
  expect_error(fit(model, c(1, NA, 3)), "missing values")
  expect_error(fit(model, 1:10, method = list()), "nuts")
  expect_error(fit(model, 1:10, dates = Sys.Date()), "one entry")
  expect_error(fit(model, 1:10, dates = as.character(1:10)), "Date")
})

test_that("simulate() takes a model given as Julia code, but not a part", {
  # Dispatch reaches the method for every `julia()` role, so the role is
  # checked before Julia starts.
  expect_error(
    simulate(julia("Normal(0, 1)", role = "distribution"), n = 10),
    "composed model"
  )
  # A component has no method at all, so dispatch still rejects it
  expect_error(simulate(RandomWalk(), n = 10), "applicable method")
})

test_that(".trajectory_bands summarises draws and drops empty time points", {
  draws <- rbind(c(NaN, 1, 2), c(NaN, 3, 4), c(NaN, 5, 6))
  bands <- .trajectory_bands(draws, 1:3)
  expect_identical(bands$time, 2:3)
  expect_identical(bands$median, c(3, 4))
})

test_that(".with_rt adds Rt only for renewal models with the default link", {
  expect_null(.with_rt(list(Z_t = matrix(0, 1, 2)), julia("IDModel()"))$Rt)
  gen <- list(Z_t = matrix(0, 1, 2))
  renewal <- IDModel(Renewal(Gamma(shape = 2, rate = 1)), PoissonError())
  direct <- IDModel(DirectInfections(), PoissonError())
  linked <- IDModel(Renewal(Gamma(shape = 2, rate = 1),
                            transformation = julia("identity")),
                    PoissonError())
  expect_identical(.with_rt(gen, renewal)$Rt, matrix(1, 1, 2))
  expect_null(.with_rt(gen, direct)$Rt)
  expect_null(.with_rt(gen, linked)$Rt)
})

test_that("fit() rejects stratified observations", {
  model <- IDModel(DirectInfections(Z = RandomWalk()), PoissonError())
  expect_error(fit(model, matrix(1:10, nrow = 2)), "Stratified data")
})

test_that(".time_axis keeps the resolution of the dates it is given", {
  hourly <- as.POSIXct("2020-02-13 06:00:00", tz = "UTC") + 3600 * (0:4)
  axis <- .time_axis(list(dates = hourly), 7)
  expect_s3_class(axis, "POSIXct")
  expect_length(axis, 7)
  expect_identical(as.numeric(diff(axis)), rep(1, 6))

  daily <- as.Date("2020-02-13") + 0:4
  expect_s3_class(.time_axis(list(dates = daily), 5), "Date")
  expect_identical(.time_axis(list(dates = NULL), 3), 1:3)
})
