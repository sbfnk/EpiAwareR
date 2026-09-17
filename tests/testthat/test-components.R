test_that("distributions validate their parameters", {
  expect_s3_class(HalfNormal(0.1), "epiaware_distribution")
  expect_error(HalfNormal(-1))
  expect_error(Gamma(shape = -1, rate = 1))
  expect_error(truncated(Normal(0, 1), 1, 0), "less than")
  expect_error(truncated(1, 0, 1), "distribution")
  expect_identical(
    as_julia(truncated(Normal(0.8, 0.05), 0, 1)),
    "truncated(Normal(0.8, 0.05), 0.0, 1.0)"
  )
})

test_that("latent models accept distributions, lists and latent models", {
  ar2 <- AR(
    damp = list(Normal(0.8, 0.05), Normal(0.1, 0.05)),
    init = Normal(0, 1)
  )
  expect_s3_class(ar2, "epiaware_latent")
  expect_match(as_julia(ar2), "damp = \\[Normal\\(0.8, 0.05\\), ")
  expect_s3_class(AR(damp = RandomWalk()), "epiaware_latent")
  expect_error(AR(damp = 0.5), "distribution")
  expect_error(AR(damp = list(Normal(0, 1), 1)), "damp\\[\\[2\\]\\]")
  expect_match(as_julia(MA(theta = Normal(0, 1))), "\u03b8 = Normal")
  expect_identical(
    as_julia(DiffLatentModel(AR(), init = Normal(0, 1))),
    "DiffLatentModel(; model = AR(), init = [Normal(0.0, 1.0)])"
  )
  expect_identical(as_julia(FixedIntercept(1)), "FixedIntercept(1.0)")
})

test_that("infection models check their arguments", {
  renewal <- Renewal(generation_time = c(0.25, 0.75), rt = RandomWalk())
  expect_s3_class(renewal, "epiaware_infection")
  expect_match(as_julia(renewal), "generation_time = \\[0.25, 0.75\\]")
  expect_match(as_julia(Renewal(generation_time = 1)), "\\[1.0\\]")
  expect_error(Renewal(generation_time = c(0.5, 0.2)), "sum to one")
  expect_error(Renewal(generation_time = Gamma(2), rt = PoissonError()))
  expect_match(
    as_julia(Renewal(Gamma(shape = 2, rate = 1), delta_d = 0.5), ascii = TRUE),
    "Symbol\\(\"\\\\u0394d\"\\) => 0.5"
  )
  expect_s3_class(DirectInfections(Z = RandomWalk()), "epiaware_infection")
  expect_s3_class(ExpGrowthRate(rt = AR()), "epiaware_infection")
  expect_error(
    DirectInfections(transformation = Normal(0, 1)),
    "Julia code from `julia\\(\\)`"
  )
})

test_that("observation models wrap other observation models", {
  delayed <- LatentDelay(PoissonError(), LogNormal(1.6, 0.42), D = 15)
  expect_identical(
    as_julia(delayed),
    "LatentDelay(PoissonError(), LogNormal(1.6, 0.42); D = 15.0)"
  )
  expect_error(LatentDelay(Normal(0, 1), c(0.5, 0.5)), "observation model")
  expect_error(LatentDelay(PoissonError(), c(0.5, 0.5), D = 3), "only apply")
  expect_match(
    as_julia(Ascertainment(PoissonError(), Normal(-1, 0.1))),
    "^Ascertainment\\(PoissonError\\(\\), Normal"
  )
  expect_s3_class(NormalError(std = HalfNormal()), "epiaware_observation")
})

test_that("IDModel composes an infection and an observation model", {
  model <- IDModel(DirectInfections(Z = RandomWalk()), PoissonError())
  expect_s3_class(model, "epiaware_model")
  expect_identical(
    as_julia(model),
    "IDModel(DirectInfections(; Z = RandomWalk()), PoissonError())"
  )
  expect_error(IDModel(PoissonError(), PoissonError()), "infection model")
  expect_error(IDModel(DirectInfections(), Normal(0, 1)), "observation model")
})

test_that("component() is the escape hatch for unwrapped constructors", {
  gp <- component("HilbertSpaceGP", role = "latent")
  expect_s3_class(
    Renewal(Gamma(shape = 2, rate = 1), rt = gp), "epiaware_infection"
  )
  expect_error(component("bad name", role = "latent"))
  expect_error(component("F"), "role")
  expect_error(component("F", role = "nonsense"))
})
