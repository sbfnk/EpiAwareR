test_that("priors convert to Julia parameterisations", {
  expect_identical(as_julia(Normal(0, 1)), "Normal(0.0, 1.0)")
  expect_identical(
    as_julia(Gamma(shape = 6.5, scale = 0.62)), "Gamma(6.5, 0.62)"
  )
  expect_identical(as_julia(Gamma(mean = 4, sd = 2)), "Gamma(4.0, 1.0)")
  expect_identical(as_julia(Exponential(rate = 0.5)), "Exponential(2.0)")
  expect_identical(as_julia(LogNormal(1.6, 0.42)), "LogNormal(1.6, 0.42)")
  expect_identical(
    as_julia(Weibull(shape = 2, scale = 3)), "Weibull(2.0, 3.0)"
  )
  expect_identical(
    as_julia(truncated(Normal(0.8, 0.05), 0, 1)),
    "truncated(Normal(0.8, 0.05), 0.0, 1.0)"
  )
})

test_that("a maximum truncates a prior from above", {
  expect_identical(
    as_julia(AR(damp = Normal(0.8, 0.05, max = 1))),
    "AR(; damp = truncated(Normal(0.8, 0.05), -Inf, 1.0))"
  )
})

test_that("priors must be single distributions with fixed parameters", {
  expect_error(
    AR(damp = Normal(mean = Normal(0.8, 0.1), sd = 0.05)), "must be fixed"
  )
  expect_error(AR(damp = Fixed(0.5)), "cannot be used as a prior")
  expect_error(
    AR(damp = Normal(0, 1) + Normal(1, 1)), "combines 2 distributions"
  )
})

test_that("a delay's maximum sets the discretisation horizon", {
  delayed <- LatentDelay(PoissonError(), LogNormal(1.6, 0.42, max = 15))
  expect_identical(
    as_julia(delayed),
    "LatentDelay(PoissonError(), LogNormal(1.6, 0.42); D = 15.0)"
  )
  explicit <- LatentDelay(
    PoissonError(), LogNormal(1.6, 0.42, max = 15), D = 10
  )
  expect_match(as_julia(explicit), "D = 10.0")
})

test_that("a horizon cannot be given for an already discrete delay", {
  expect_error(
    LatentDelay(PoissonError(), c(0.5, 0.5), D = 3), "only apply"
  )
  expect_error(
    LatentDelay(PoissonError(), NonParametric(c(0.5, 0.5)), D = 3), "only apply"
  )
  expect_error(
    Renewal(generation_time = c(0.5, 0.5), D_gen = 10), "`D_gen`"
  )
  expect_error(
    Renewal(generation_time = NonParametric(c(0.5, 0.5)), delta_d = 0.5),
    "only apply"
  )
})

test_that("nonparametric and fixed delays become probability vectors", {
  expect_identical(
    as_julia(LatentDelay(PoissonError(), Fixed(2))),
    "LatentDelay(PoissonError(), [0.0, 0.0, 1.0])"
  )
  # A generation time drops the zero-day mass and renormalises
  expect_identical(
    as_julia(Renewal(NonParametric(c(0.5, 0.25, 0.25)))),
    "Renewal(; generation_time = [0.5, 0.5])"
  )
})

test_that("uncertain delays become inferred delays with bounded priors", {
  delay <- LogNormal(
    meanlog = Normal(1.6, 0.2), sdlog = Normal(0.4, 0.05), max = 15
  )
  expect_identical(
    as_julia(LatentDelay(PoissonError(), delay)),
    paste0(
      "LatentDelay(PoissonError(), UncertainDelay(LogNormal, ",
      "[Normal(1.6, 0.2), truncated(Normal(0.4, 0.05), 0.0, Inf)]; D = 15.0))"
    )
  )
})

test_that("unsupported uncertain delays fail with an explanation", {
  expect_error(
    LatentDelay(
      PoissonError(), LogNormal(meanlog = Normal(1.6, 0.2), sdlog = 0.4)
    ),
    "finite maximum"
  )
  expect_error(
    LatentDelay(
      PoissonError(),
      LogNormal(meanlog = Normal(1.6, 0.2), sdlog = 0.4, max = 15)
    ),
    "mixes fixed and uncertain"
  )
  expect_error(
    LatentDelay(
      PoissonError(),
      Gamma(shape = Normal(2, 0.5), rate = Normal(1, 0.1), max = 15)
    ),
    "parameterised differently"
  )
})
