# Tests for Bayesian inference

# NUTS Sampler Configuration --------------------------------------------

test_that("nuts_sampler creates valid configuration", {
  sampler <- nuts_sampler()

  expect_s3_class(sampler, "epiaware_nuts")
  expect_s3_class(sampler, "epiaware_sampler")
  expect_equal(sampler$warmup, 1000)
  expect_equal(sampler$draws, 1000)
  expect_equal(sampler$chains, 4)
  expect_equal(sampler$target_acceptance, 0.9)
})

test_that("nuts_sampler accepts custom parameters", {
  sampler <- nuts_sampler(
    warmup = 500,
    draws = 500,
    chains = 2,
    target_acceptance = 0.85
  )

  expect_equal(sampler$warmup, 500)
  expect_equal(sampler$draws, 500)
  expect_equal(sampler$chains, 2)
  expect_equal(sampler$target_acceptance, 0.85)
})

test_that("nuts_sampler validates inputs", {
  expect_error(nuts_sampler(warmup = -100), "Element 1 is not >= 1")
  expect_error(nuts_sampler(draws = 0), "Element 1 is not >= 1")
  expect_error(nuts_sampler(chains = -1), "Element 1 is not >= 1")
  expect_error(nuts_sampler(target_acceptance = 1.5), "Element 1 is not <= 1")
})

test_that("nuts_sampler print method works", {
  sampler <- nuts_sampler(warmup = 100, draws = 100, chains = 2)

  expect_output(print(sampler), "EpiAware NUTS Sampler Configuration")
  expect_output(print(sampler), "Warmup: 100")
  expect_output(print(sampler), "Draws: 100")
  expect_output(print(sampler), "Chains: 2")
})

# fit() Function --------------------------------------------------------

test_that("fit validates model input", {
  skip_if_no_julia()

  data <- data.frame(y_t = rpois(50, 100))

  expect_error(
    fit(model = "not a model", data = data),
    "model must be an EpiProblem object"
  )
})

test_that("fit validates data input", {
  skip_if_no_julia()

  ar1 <- AR(
    order = 1,
    damp_priors = list(truncnorm(0.5, 0.1, 0, 1)),
    init_priors = list(norm(0, 0.1)),
    std_prior = halfnorm(0.1)
  )

  renewal <- Renewal(gen_distribution = gamma_dist(6.5, 0.62))
  negbin <- NegativeBinomialError(cluster_factor_prior = halfnorm(0.1))

  problem <- EpiProblem(
    epi_model = renewal,
    latent_model = ar1,
    observation_model = negbin,
    tspan = c(1, 50)
  )

  expect_error(
    fit(model = problem, data = "not a data frame"),
    "Assertion failed"
  )
})

test_that("fit returns epiaware_fit object", {
  skip_if_no_julia()
  skip_on_cran()
  skip("Long-running integration test")

  # Create simple model
  ar1 <- AR(
    order = 1,
    damp_priors = list(truncnorm(0.5, 0.1, 0, 1)),
    init_priors = list(norm(0, 0.1)),
    std_prior = halfnorm(0.1)
  )

  renewal <- Renewal(gen_distribution = gamma_dist(6.5, 0.62))
  negbin <- NegativeBinomialError(cluster_factor_prior = halfnorm(0.1))

  problem <- EpiProblem(
    epi_model = renewal,
    latent_model = ar1,
    observation_model = negbin,
    tspan = c(1, 50)
  )

  # Simulate some data
  set.seed(123)
  data <- data.frame(y_t = rpois(50, lambda = seq(50, 150, length.out = 50)))

  # Fit with minimal sampling for testing
  results <- fit(
    model = problem,
    data = data,
    method = nuts_sampler(warmup = 50, draws = 50, chains = 1)
  )

  expect_s3_class(results, "epiaware_fit")
  expect_true(!is.null(results$samples))
  expect_true(!is.null(results$summary))
  expect_true(!is.null(results$diagnostics))
  expect_identical(results$model, problem)
  expect_identical(results$data, data)
})

# Fit Object Methods ----------------------------------------------------

test_that("print.epiaware_fit works", {
  skip_if_no_julia()
  skip_on_cran()
  skip("Long-running integration test")
})

test_that("summary.epiaware_fit returns summary tibble", {
  skip_if_no_julia()
  skip_on_cran()
  skip("Long-running integration test")
})

test_that("plot.epiaware_fit creates ggplot objects", {
  skip_if_no_julia()
  skip_on_cran()
  skip("Long-running integration test")
})
