# Tests for EpiProblem composition

skip_if_no_julia <- function() {
  testthat::skip_if_not(epiaware_available(), "Julia/EpiAware not available")
}

test_that("EpiProblem composes models correctly", {
  skip_if_no_julia()

  ar1 <- AR(
    order = 1,
    damp_priors = list(truncnorm(0.5, 0.1, 0, 1)),
    init_priors = list(norm(0, 0.1)),
    std_prior = halfnorm(0.1)
  )

  renewal <- Renewal(
    gen_distribution = gamma_dist(6.5, 0.62),
    initialisation_prior = norm(log(1.0), 0.1)
  )

  negbin <- NegativeBinomialError(cluster_factor_prior = halfnorm(0.1))

  problem <- EpiProblem(
    epi_model = renewal,
    latent_model = ar1,
    observation_model = negbin,
    tspan = c(1, 50)
  )

  expect_s3_class(problem, "epiaware_problem")
  expect_s3_class(problem, "epiaware_model")
  expect_true(!is.null(problem$julia_ref))
  expect_equal(problem$tspan, c(1, 50))
  expect_type(problem$components, "list")
  expect_length(problem$components, 3)
})

test_that("EpiProblem validates component types", {
  skip_if_no_julia()

  ar1 <- AR(
    order = 1,
    damp_priors = list(truncnorm(0.5, 0.1, 0, 1)),
    init_priors = list(norm(0, 0.1)),
    std_prior = halfnorm(0.1)
  )

  renewal <- Renewal(
    gen_distribution = gamma_dist(6.5, 0.62)
  )

  negbin <- NegativeBinomialError(cluster_factor_prior = halfnorm(0.1))

  # Wrong epi_model type
  expect_error(
    EpiProblem(
      epi_model = "not an epi model",
      latent_model = ar1,
      observation_model = negbin,
      tspan = c(1, 50)
    ),
    "epi_model must be a infection model object"
  )

  # Wrong latent_model type
  expect_error(
    EpiProblem(
      epi_model = renewal,
      latent_model = "not a latent model",
      observation_model = negbin,
      tspan = c(1, 50)
    ),
    "latent_model must be a latent model object"
  )

  # Wrong observation_model type
  expect_error(
    EpiProblem(
      epi_model = renewal,
      latent_model = ar1,
      observation_model = "not an observation model",
      tspan = c(1, 50)
    ),
    "observation_model must be a observation model object"
  )
})

test_that("EpiProblem validates tspan", {
  skip_if_no_julia()

  ar1 <- AR(
    order = 1,
    damp_priors = list(truncnorm(0.5, 0.1, 0, 1)),
    init_priors = list(norm(0, 0.1)),
    std_prior = halfnorm(0.1)
  )

  renewal <- Renewal(gen_distribution = gamma_dist(6.5, 0.62))
  negbin <- NegativeBinomialError(cluster_factor_prior = halfnorm(0.1))

  # Non-numeric tspan
  expect_error(
    EpiProblem(
      epi_model = renewal,
      latent_model = ar1,
      observation_model = negbin,
      tspan = c("a", "b")
    ),
    "Must be of type 'numeric'"
  )

  # Wrong length
  expect_error(
    EpiProblem(
      epi_model = renewal,
      latent_model = ar1,
      observation_model = negbin,
      tspan = c(1, 50, 100)
    ),
    "Must have length 2"
  )

  # Invalid ordering
  expect_error(
    EpiProblem(
      epi_model = renewal,
      latent_model = ar1,
      observation_model = negbin,
      tspan = c(50, 1)
    ),
    "tspan\\[1\\] must be less than tspan\\[2\\]"
  )
})

test_that("EpiProblem print method works", {
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

  expect_output(print(problem), "EpiAware Epidemiological Model")
  expect_output(print(problem), "Time span: 1 to 50")
  expect_output(print(problem), "Infection model: epiaware_renewal")
  expect_output(print(problem), "Latent model: epiaware_ar")
  expect_output(print(problem), "Observation model: epiaware_negbin")
})
