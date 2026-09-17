test_that("nuts() validates settings", {
  sampler <- nuts(draws = 100, warmup = 50, chains = 2)
  expect_s3_class(sampler, "epiaware_nuts")
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
})

test_that(".trajectory_bands summarises draws and drops empty time points", {
  draws <- rbind(c(NaN, 1, 2), c(NaN, 3, 4), c(NaN, 5, 6))
  bands <- .trajectory_bands(draws, 1:3)
  expect_identical(bands$time, 2:3)
  expect_identical(bands$median, c(3, 4))
})

test_that(".with_rt adds Rt only for renewal models with the default link", {
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
