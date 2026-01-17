# Bayesian inference methods for EpiAware models

#' Configure NUTS Sampler
#'
#' Creates a configuration for the No-U-Turn Sampler (NUTS), a variant of
#' Hamiltonian Monte Carlo that automatically tunes step size and number of
#' steps.
#'
#' @param warmup Integer. Number of warmup/adaptation iterations. Default: 1000.
#' @param draws Integer. Number of post-warmup samples to draw. Default: 1000.
#' @param chains Integer. Number of MCMC chains to run. Default: 4.
#' @param target_acceptance Numeric. Target acceptance rate for adaptation.
#'   Default: 0.8.
#'
#' @return An S3 object of class \code{c("epiaware_nuts", "epiaware_sampler")}
#'   containing sampler configuration.
#'
#' @examples
#' \dontrun{
#' # Default NUTS configuration
#' sampler <- nuts_sampler()
#'
#' # Custom configuration for faster testing
#' test_sampler <- nuts_sampler(
#'   warmup = 100,
#'   draws = 100,
#'   chains = 2
#' )
#' }
#'
#' @export
nuts_sampler <- function(warmup = 1000, draws = 1000, chains = 4,
                         target_acceptance = 0.8) {
  # Validate inputs
  checkmate::assert_int(warmup, lower = 1)
  checkmate::assert_int(draws, lower = 1)
  checkmate::assert_int(chains, lower = 1)
  checkmate::assert_number(target_acceptance, lower = 0, upper = 1)

  structure(
    list(
      warmup = as.integer(warmup),
      draws = as.integer(draws),
      chains = as.integer(chains),
      target_acceptance = target_acceptance
    ),
    class = c("epiaware_nuts", "epiaware_sampler")
  )
}

#' Fit an EpiAware Model to Data
#'
#' Performs Bayesian inference on an epidemiological model using MCMC sampling.
#' This function generates a Turing.jl model from the EpiProblem specification,
#' runs the specified inference method, and returns posterior samples with
#' diagnostics.
#'
#' @param model An \code{EpiProblem} object specifying the complete model.
#' @param data A data frame or list containing observed data. Must have a
#'   column/element named \code{y_t} or \code{cases} with case counts.
#'   Optionally can include \code{dates}.
#' @param method A sampler configuration object (e.g., from
#'   \code{nuts_sampler}). Default: \code{nuts_sampler()}.
#' @param ... Additional arguments (currently unused).
#'
#' @return An S3 object of class \code{epiaware_fit} containing:
#' \describe{
#'   \item{samples}{posterior::draws_df object with MCMC samples}
#'   \item{summary}{tibble with parameter summaries}
#'   \item{diagnostics}{tibble with convergence diagnostics (Rhat, ESS)}
#'   \item{generated_quantities}{List with generated quantities (Rt,
#'     infections, etc.)}
#'   \item{model}{The original EpiProblem}
#'   \item{data}{The data used for inference}
#'   \item{method}{The inference method used}
#' }
#'
#' @examples
#' \dontrun{
#' # Load data
#' data <- read.csv("south_korea_data.csv")
#' training_data <- data[45:80, ]
#'
#' # Fit model
#' results <- fit(
#'   model = mishra_model,
#'   data = training_data,
#'   method = nuts_sampler(warmup = 1000, draws = 1000, chains = 4)
#' )
#'
#' # Examine results
#' print(results)
#' summary(results)
#' plot(results, type = "Rt")
#' }
#'
#' @export
fit <- function(model, data, method = nuts_sampler(), ...) {
  # Validate inputs
  .check_julia()
  .check_model_component(model, type = "problem")
  checkmate::assert(
    checkmate::check_data_frame(data),
    checkmate::check_list(data),
    combine = "or"
  )

  # Prepare data for Julia
  prepared_data <- .prepare_data_for_julia(data, model$tspan)

  # Generate Turing model
  message("Generating Turing.jl model...")
  julia_model <- tryCatch(
    {
      # Convert prepared data to Julia NamedTuple
      # Julia expects data with .y_t field access, not Dict[:y_t]
      JuliaCall::julia_assign("y_t_data", prepared_data$y_t)

      # Create NamedTuple in Julia
      julia_data <- if (!is.null(prepared_data$dates)) {
        JuliaCall::julia_assign("dates_data", prepared_data$dates)
        .eval_julia_code("(y_t=y_t_data, dates=dates_data)")
      } else {
        .eval_julia_code("(y_t=y_t_data,)")
      }

      # Now call generate_epiaware with the NamedTuple
      JuliaCall::julia_assign("epi_problem_tmp", model$julia_ref)
      JuliaCall::julia_assign("data_tmp", julia_data)
      .eval_julia_code("generate_epiaware(epi_problem_tmp, data_tmp)")
    },
    error = function(e) {
      stop(
        "Failed to generate Turing model:\n",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )

  # Run inference based on method
  if (inherits(method, "epiaware_nuts")) {
    message("Running NUTS sampling...")
    message("  Chains: ", method$chains)
    message("  Warmup: ", method$warmup)
    message("  Draws: ", method$draws)

    samples <- .run_nuts_sampling(julia_model, method)

    # Store chains in Julia for generated_observables
    JuliaCall::julia_assign("chains_tmp", samples)
  } else {
    stop(
      "Unknown inference method: ", class(method)[1],
      call. = FALSE
    )
  }

  # Convert to posterior::draws format
  message("Processing results...")
  draws_obj <- .julia_chains_to_draws(samples)

  # Compute diagnostics
  diagnostics <- .compute_diagnostics(draws_obj)

  # Generate quantities (Rt, infections, etc.) using EpiAware's generated_observables
  gen_quantities <- .generate_quantities(julia_model, samples)

  # Return results object
  structure(
    list(
      samples = draws_obj,
      summary = posterior::summarise_draws(draws_obj),
      diagnostics = diagnostics,
      generated_quantities = gen_quantities,
      model = model,
      data = data,
      method = method
    ),
    class = "epiaware_fit"
  )
}

#' Run NUTS sampling via Julia
#' @keywords internal
.run_nuts_sampling <- function(julia_model, method) {
  tryCatch(
    {
      # Import required Julia packages
      .eval_julia_code("using Turing, MCMCChains")

      # Run NUTS
      # Note: This is simplified - actual implementation would need to
      # properly pass the julia_model object and handle the result
      .call_julia_function(
        "sample",
        julia_model,
        .eval_julia_code(sprintf("NUTS(%f)", method$target_acceptance)),
        .eval_julia_code("MCMCThreads()"),
        method$draws,
        method$chains
      )
    },
    error = function(e) {
      stop(
        "NUTS sampling failed:\n",
        conditionMessage(e),
        "\n\nTry running with fewer chains or draws for testing.",
        call. = FALSE
      )
    }
  )
}

#' Generate quantities from fitted model
#' @keywords internal
.generate_quantities <- function(julia_model, julia_chains) {
  # Use EpiAware's generated_observables to compute derived quantities

  # This is the proper way to get Rt, infections, etc. from the posterior
  tryCatch(
    {
      # Store the model in Julia scope
      JuliaCall::julia_assign("turing_model_gq", julia_model)
      JuliaCall::julia_assign("chains_gq", julia_chains)

      # Try to generate observables using EpiAware's function
      # Note: data_tmp should still be in scope from the fit() call
      .eval_julia_code("using EpiAware")

      # Call generated_observables and extract fields
      obs_result <- tryCatch(
        {
          JuliaCall::julia_command(
            "global _gq_observables = generated_observables(turing_model_gq, data_tmp, chains_gq)"
          )
          JuliaCall::julia_command("global _gq_gen = _gq_observables.generated")

          # _gq_gen is a Matrix of NamedTuples, get fields from first element
          JuliaCall::julia_command("global _gq_fields = propertynames(_gq_gen[1])")
          fields <- unlist(JuliaCall::julia_eval("string.(_gq_fields)"))

          # Extract available quantities - iterate over matrix elements
          # I_t = infections, Z_t = latent (log Rt), generated_y_t = expected obs
          I_t <- if ("I_t" %in% fields) {
            JuliaCall::julia_eval("reduce(hcat, [g.I_t for g in _gq_gen])'")
          } else {
            NULL
          }

          Z_t <- if ("Z_t" %in% fields) {
            JuliaCall::julia_eval("reduce(hcat, [g.Z_t for g in _gq_gen])'")
          } else {
            NULL
          }

          generated_y_t <- if ("generated_y_t" %in% fields) {
            JuliaCall::julia_eval("reduce(hcat, [g.generated_y_t for g in _gq_gen])'")
          } else {
            NULL
          }

          list(
            success = TRUE,
            fields = fields,
            I_t = I_t,
            Z_t = Z_t,
            generated_y_t = generated_y_t
          )
        },
        error = function(e) {
          list(success = FALSE, error = conditionMessage(e), fields = character(0))
        }
      )

      if (isTRUE(obs_result$success)) {
        # Z_t is the latent process (log Rt for renewal models)
        # Julia's reduce(hcat, ...)' already returns a matrix
        # Convert to Rt by exponentiating
        rt_data <- if (!is.null(obs_result$Z_t)) {
          z_matrix <- as.matrix(obs_result$Z_t)
          exp(z_matrix)  # Convert log(Rt) to Rt
        } else {
          NULL
        }

        # I_t is infections - already a matrix from Julia
        infections_data <- if (!is.null(obs_result$I_t)) {
          as.matrix(obs_result$I_t)
        } else {
          NULL
        }

        # generated_y_t is expected observations - already a matrix from Julia
        predicted_data <- if (!is.null(obs_result$generated_y_t)) {
          as.matrix(obs_result$generated_y_t)
        } else {
          NULL
        }

        list(
          Rt = rt_data,
          infections = infections_data,
          predicted_cases = predicted_data,
          available_fields = obs_result$fields
        )
      } else {
        # Fall back to NULL if generated_observables fails
        list(
          Rt = NULL,
          infections = NULL,
          predicted_cases = NULL,
          error = obs_result$error
        )
      }
    },
    error = function(e) {
      # If anything fails, return empty list (plotting will use fallback)
      list(
        Rt = NULL,
        infections = NULL,
        predicted_cases = NULL,
        error = conditionMessage(e)
      )
    }
  )
}

#' Print method for NUTS sampler configuration
#'
#' @param x An \code{epiaware_nuts} object.
#' @param ... Additional arguments (currently unused).
#' @export
print.epiaware_nuts <- function(x, ...) {
  cat("<EpiAware NUTS Sampler Configuration>\n")
  cat("  Warmup:", x$warmup, "\n")
  cat("  Draws:", x$draws, "\n")
  cat("  Chains:", x$chains, "\n")
  cat("  Target acceptance:", x$target_acceptance, "\n")
  invisible(x)
}
