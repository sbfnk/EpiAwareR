# Integration tests running models in Julia.

direct_model <- function() {
  IDModel(
    DirectInfections(Z = RandomWalk(), initialisation = Normal(log(50), 0.5)),
    PoissonError()
  )
}

test_that("simulate() returns trajectories for each quantity", {
  skip_if_no_julia()
  sims <- simulate(direct_model(), nsim = 3, n = 20, seed = 1)
  expect_setequal(
    names(sims), c("generated_y_t", "expected_y_t", "I_t", "Z_t")
  )
  expect_identical(dim(sims$I_t), c(3L, 20L))
  expect_true(all(sims$generated_y_t >= 0))
  expect_identical(simulate(direct_model(), n = 20, seed = 1)$I_t,
                   sims$I_t[1, , drop = FALSE])
})

test_that("simulate() handles non-ASCII keywords and delays", {
  skip_if_no_julia()
  model <- IDModel(
    Renewal(
      generation_time = Gamma(shape = 6.5, scale = 0.62),
      rt = AR(epsilon_t = HierarchicalNormal(std = HalfNormal(0.05))),
      initialisation = Normal(log(10), 0.1)
    ),
    LatentDelay(PoissonError(), delay = c(0.2, 0.5, 0.3))
  )
  sims <- simulate(model, nsim = 2, n = 30, seed = 2)
  expect_identical(dim(sims$Rt), c(2L, 30L))
  expect_equal(sims$Rt, exp(sims$Z_t))
  # The delay leaves the first two time points unobserved, at the front
  expect_true(all(is.nan(sims$generated_y_t[, 1:2])))
  expect_false(any(is.nan(sims$generated_y_t[, 3:30])))
})

test_that("fit() recovers draws, generated quantities and forecasts", {
  skip_if_no_julia()
  model <- direct_model()
  y <- simulate(model, n = 25, seed = 3)$generated_y_t[1, ]
  fitted <- fit(
    model, y, method = nuts(draws = 50, warmup = 50, chains = 2), seed = 4
  )
  expect_s3_class(fitted, "epiaware_fit")
  expect_identical(posterior::ndraws(fitted$draws), 100L)
  expect_identical(posterior::nchains(fitted$draws), 2L)
  expect_identical(dim(fitted$generated$I_t), c(100L, 25L))
  expect_identical(dim(predict(fitted)), c(100L, 25L))
  expect_false(anyNA(predict(fitted)))
  # Draws are ordered chain by chain, and each generated trajectory belongs to
  # the draw on the same row. An interleaved or transposed reading breaks both.
  expect_identical(
    posterior::draw_ids(posterior::subset_draws(fitted$draws, chain = 2)),
    1:50
  )
  seeded <- posterior::extract_variable(fitted$draws, "init_incidence")
  expect_equal(
    fitted$generated$I_t[, 1], exp(seeded + fitted$generated$Z_t[, 1])
  )

  expect_s3_class(suppressWarnings(summary(fitted)), "draws_summary")
  expect_output(suppressWarnings(print(fitted)), "Divergent transitions")

  forecasts <- predict(fitted, horizon = 5, seed = 5)
  expect_identical(dim(forecasts), c(100L, 30L))
  expect_false(anyNA(forecasts[, 26:30]))

  # A handle from a previous Julia session would name a different fit
  stale <- fitted
  stale$julia <- list(handle = fitted$julia$handle, session = "other-session")
  expect_error(predict(stale, horizon = 2), "no longer available")

  expect_s3_class(plot(fitted), "ggplot")
  expect_s3_class(plot(fitted, type = "infections"), "ggplot")
  expect_s3_class(plot(fitted, type = "cases", horizon = 3), "ggplot")
  expect_error(plot(fitted, type = "Rt"), "Renewal")
})

test_that("an uncertain delay is inferred in Julia", {
  skip_if_no_julia()
  model <- IDModel(
    DirectInfections(Z = RandomWalk(), initialisation = Normal(log(50), 0.5)),
    LatentDelay(
      PoissonError(),
      delay = LogNormal(
        meanlog = Normal(1, 0.2), sdlog = Normal(0.4, 0.1), max = 10
      )
    )
  )
  sims <- simulate(model, nsim = 2, n = 25, seed = 6)
  expect_identical(dim(sims$generated_y_t), c(2L, 25L))
  y <- sims$generated_y_t[1, ]
  y <- y[!is.nan(y)]
  fitted <- fit(
    model, y, method = nuts(draws = 30, warmup = 30, chains = 1), seed = 7
  )
  expect_length(grep("^delay\\.", posterior::variables(fitted$draws)), 2)
})
