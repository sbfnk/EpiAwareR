# Tests for model component constructors

# Skip all tests if Julia not available
skip_if_no_julia <- function() {
  testthat::skip_if_not(epiaware_available(), "Julia/EpiAware not available")
}

# Latent Models ---------------------------------------------------------

test_that("AR creates valid model object", {
  skip_if_no_julia()

  ar1 <- AR(
    order = 1,
    damp_priors = list(truncnorm(0.5, 0.1, 0, 1)),
    init_priors = list(norm(0, 0.1)),
    std_prior = halfnorm(0.1)
  )

  expect_s3_class(ar1, "epiaware_ar")
  expect_s3_class(ar1, "epiaware_latent")
  expect_s3_class(ar1, "epiaware_model")
  expect_true(!is.null(ar1$julia_ref))
  expect_type(ar1$spec, "list")
})

test_that("AR validates order and prior lengths", {
  skip_if_no_julia()

  expect_error(
    AR(
      order = 2,
      damp_priors = list(truncnorm(0.5, 0.1, 0, 1)), # Only 1 prior for order 2
      init_priors = list(norm(0, 0.1), norm(0, 0.1)),
      std_prior = halfnorm(0.1)
    ),
    "length of damp_priors must equal 2"
  )

  expect_error(
    AR(
      order = 2,
      damp_priors = list(truncnorm(0.5, 0.1, 0, 1), truncnorm(0.3, 0.1, 0, 1)),
      init_priors = list(norm(0, 0.1)), # Only 1 prior for order 2
      std_prior = halfnorm(0.1)
    ),
    "length of init_priors must equal 2"
  )
})

test_that("AR validates distribution specs", {
  skip_if_no_julia()

  expect_error(
    AR(
      order = 1,
      damp_priors = list("not a distribution"),
      init_priors = list(norm(0, 0.1)),
      std_prior = halfnorm(0.1)
    ),
    "Must be of type 'list'"
  )
})

test_that("AR print method works", {
  skip_if_no_julia()

  ar1 <- AR(
    order = 1,
    damp_priors = list(truncnorm(0.5, 0.1, 0, 1)),
    init_priors = list(norm(0, 0.1)),
    std_prior = halfnorm(0.1)
  )

  expect_output(print(ar1), "EpiAware AR\\(1\\) Latent Model")
})

# Infection Models ------------------------------------------------------

test_that("Renewal creates valid model object", {
  skip_if_no_julia()

  renewal <- Renewal(
    gen_distribution = gamma_dist(6.5, 0.62),
    initialisation_prior = norm(log(1.0), 0.1)
  )

  expect_s3_class(renewal, "epiaware_renewal")
  expect_s3_class(renewal, "epiaware_epi")
  expect_s3_class(renewal, "epiaware_model")
  expect_true(!is.null(renewal$julia_ref))
  expect_type(renewal$spec, "list")
})

test_that("Renewal validates generation distribution", {
  skip_if_no_julia()

  expect_error(
    Renewal(gen_distribution = "not a distribution"),
    "Must be of type 'list'"
  )
})

test_that("Renewal works without initialisation_prior", {
  skip_if_no_julia()

  renewal <- Renewal(gen_distribution = gamma_dist(6.5, 0.62))

  expect_s3_class(renewal, "epiaware_renewal")
  expect_null(renewal$spec$initialisation_prior)
})

test_that("Renewal print method works", {
  skip_if_no_julia()

  renewal <- Renewal(
    gen_distribution = gamma_dist(6.5, 0.62),
    initialisation_prior = norm(log(1.0), 0.1)
  )

  expect_output(print(renewal), "EpiAware Renewal Infection Model")
  expect_output(print(renewal), "Generation distribution: Gamma")
})

# Observation Models ----------------------------------------------------

test_that("NegativeBinomialError creates valid model object", {
  skip_if_no_julia()

  negbin <- NegativeBinomialError(cluster_factor_prior = halfnorm(0.1))

  expect_s3_class(negbin, "epiaware_negbin")
  expect_s3_class(negbin, "epiaware_observation")
  expect_s3_class(negbin, "epiaware_model")
  expect_true(!is.null(negbin$julia_ref))
  expect_type(negbin$spec, "list")
})

test_that("NegativeBinomialError validates prior", {
  skip_if_no_julia()

  expect_error(
    NegativeBinomialError(cluster_factor_prior = "not a distribution"),
    "Must be of type 'list'"
  )
})

test_that("NegativeBinomialError print method works", {
  skip_if_no_julia()

  negbin <- NegativeBinomialError(cluster_factor_prior = halfnorm(0.1))

  expect_output(print(negbin), "EpiAware Negative Binomial Observation Model")
})

test_that("LatentDelay creates valid model object", {
  skip_if_no_julia()

  negbin <- NegativeBinomialError(cluster_factor_prior = halfnorm(0.1))
  delayed <- LatentDelay(
    model = negbin,
    delay_distribution = lognorm(1.6, 0.42)
  )

  expect_s3_class(delayed, "epiaware_delay")
  expect_s3_class(delayed, "epiaware_observation")
  expect_s3_class(delayed, "epiaware_model")
  expect_true(!is.null(delayed$julia_ref))
  expect_s3_class(delayed$base_model, "epiaware_negbin")
})

test_that("LatentDelay validates inputs", {
  skip_if_no_julia()

  expect_error(
    LatentDelay(
      model = "not an observation model",
      delay_distribution = lognorm(1.6, 0.42)
    ),
    "model must be a observation model object"
  )

  negbin <- NegativeBinomialError(cluster_factor_prior = halfnorm(0.1))
  expect_error(
    LatentDelay(
      model = negbin,
      delay_distribution = "not a distribution"
    ),
    "Must be of type 'list'"
  )
})

test_that("LatentDelay can be composed hierarchically", {
  skip_if_no_julia()

  negbin <- NegativeBinomialError(cluster_factor_prior = halfnorm(0.1))
  incubation <- LatentDelay(negbin, delay_distribution = lognorm(1.6, 0.42))
  reporting <- LatentDelay(incubation, delay_distribution = lognorm(0.58, 0.47))

  expect_s3_class(reporting, "epiaware_delay")
  expect_s3_class(reporting$base_model, "epiaware_delay")
  expect_s3_class(reporting$base_model$base_model, "epiaware_negbin")
})

test_that("LatentDelay print method works", {
  skip_if_no_julia()

  negbin <- NegativeBinomialError(cluster_factor_prior = halfnorm(0.1))
  delayed <- LatentDelay(
    model = negbin,
    delay_distribution = lognorm(1.6, 0.42)
  )

  expect_output(print(delayed), "EpiAware Latent Delay Observation Model")
  expect_output(print(delayed), "Delay distribution: LogNormal")
})
