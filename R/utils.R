# Internal utility functions for EpiAwareR

#' Generic wrapper for calling Julia EpiAware functions
#'
#' Provides access to Julia EpiAware functions not yet wrapped in R.
#' This is an "escape hatch" for accessing newer or experimental EpiAware
#' features before they get explicit R wrappers.
#'
#' @param fn_name Character string. Name of the Julia function to call.
#' @param ... Named arguments to pass to the Julia function.
#' @param .param_map Named character vector mapping R parameter names to Julia
#'   names. Use this when Julia uses Greek letters or other characters not
#'   easily typed in R. Example: c(theta_priors = "θ_priors", eps_t = "ϵ_t").
#' @param .class Character vector. S3 classes to add to the returned object.
#'   Default is c("epiaware_generic", "epiaware_model").
#'
#' @return An S3 object containing the Julia reference and specifications.
#'
#' @examples
#' \dontrun{
#' # Call a hypothetical new EpiAware component not yet wrapped
#' custom_model <- epiaware_call(
#'   "NewLatentModel",
#'   param1 = 0.5,
#'   param2 = norm(0, 1)
#' )
#'
#' # For Julia functions with Greek letters, use parameter mapping
#' eps_model <- epiaware_call("HierarchicalNormal", halfnorm(0.1))
#' ma2 <- epiaware_call(
#'   "MA",
#'   theta_priors = list(norm(0, 0.1), norm(0, 0.1)),
#'   eps_t = eps_model,
#'   .param_map = c(theta_priors = "θ_priors", eps_t = "ϵ_t")
#' )
#' }
#'
#' @export
epiaware_call <- function(fn_name, ..., .param_map = NULL,
                          .class = c("epiaware_generic", "epiaware_model")) {
  # Validate fn_name
  if (!is.character(fn_name) || length(fn_name) != 1) {
    stop("fn_name must be a character string", call. = FALSE)
  }

  if (nchar(fn_name) == 0) {
    stop("fn_name must be a non-empty character string", call. = FALSE)
  }

  if (!epiaware_available()) {
    stop(
      "Julia is not available. Please run:\n",
      "  epiaware_setup_julia()\n",
      "to install and configure Julia.",
      call. = FALSE
    )
  }

  # Collect arguments
  args <- list(...)

  # Apply parameter name mapping if provided
  if (!is.null(.param_map)) {
    for (r_name in names(.param_map)) {
      if (r_name %in% names(args)) {
        julia_name <- .param_map[[r_name]]
        args[[julia_name]] <- args[[r_name]]
        args[[r_name]] <- NULL
      }
    }
  }

  # Convert any distribution specs to Julia
  args_converted <- lapply(args, function(arg) {
    if (is.list(arg) && !is.null(arg$type)) {
      # Single distribution spec
      .to_julia_dist(arg)
    } else if (is.list(arg) && .is_distribution_list(arg)) {
      # List of distribution specs
      .to_julia_dist_vector(arg)
    } else if (inherits(arg, "epiaware_model") && !is.null(arg$julia_ref)) {
      # EpiAware model object - extract julia_ref automatically
      arg$julia_ref
    } else {
      arg
    }
  })

  # Detect whether to use keyword or positional arguments
  # Use keywords if all arguments are named, otherwise use positional
  use_keywords <- !is.null(names(args_converted)) &&
    all(names(args_converted) != "")

  # Call Julia function using .call_julia_constructor
  # for better error handling
  julia_obj <- .call_julia_constructor(
    fn_name, args_converted, use_keywords = use_keywords
  )

  # Return S3 object
  structure(
    list(
      julia_ref = julia_obj,
      spec = args,
      fn_name = fn_name
    ),
    class = .class
  )
}

#' @keywords internal
.call_julia_function <- function(fn_name, ...) {
  tryCatch(
    {
      JuliaCall::julia_call(fn_name, ...)
    },
    error = function(e) {
      stop(
        "Julia function '", fn_name, "' failed:\n",
        conditionMessage(e), "\n\n",
        "Try running: epiaware_setup_julia()",
        call. = FALSE
      )
    }
  )
}

#' @keywords internal
.eval_julia_code <- function(code) {
  tryCatch(
    {
      JuliaCall::julia_eval(code)
    },
    error = function(e) {
      stop(
        "Julia evaluation failed:\n",
        "Code: ", code, "\n",
        "Error: ", conditionMessage(e),
        call. = FALSE
      )
    }
  )
}

#' Call a Julia constructor with automatic argument handling
#'
#' This helper function standardizes the pattern of calling Julia constructors
#' by handling argument assignment and error reporting consistently.
#'
#' @param constructor_name Character string. Name of the Julia constructor.
#' @param args Named list of arguments to pass. Names should match Julia
#'   parameter names, values are R objects to convert.
#' @param use_keywords Logical. If TRUE (default), use keyword arguments
#'   (e.g., Constructor(x=val)). If FALSE, use positional arguments.
#'
#' @return The Julia object returned by the constructor.
#' @keywords internal
.call_julia_constructor <- function(constructor_name, args,
                                    use_keywords = TRUE) {
  tryCatch(
    {
      # Assign all arguments to Julia with temporary names
      # For unnamed args, use indices; for named args, use the names
      if (is.null(names(args)) || all(names(args) == "")) {
        tmp_names <- paste0("arg", seq_along(args), "_tmp")
      } else {
        tmp_names <- paste0(names(args), "_tmp")
      }

      for (i in seq_along(args)) {
        JuliaCall::julia_assign(tmp_names[i], args[[i]])
      }

      # Build constructor call string
      if (use_keywords) {
        # Keyword arguments: Constructor(arg1=arg1_tmp, arg2=arg2_tmp)
        arg_strings <- paste0(names(args), "=", tmp_names)
      } else {
        # Positional arguments: Constructor(arg1_tmp, arg2_tmp)
        arg_strings <- tmp_names
      }

      julia_code <- sprintf(
        "%s(%s)", constructor_name, paste(arg_strings, collapse = ", ")
      )
      .eval_julia_code(julia_code)
    },
    error = function(e) {
      stop(
        "Failed to create ", constructor_name, ":\n",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )
}

#' Check if a list contains only distribution specifications
#' @keywords internal
.is_distribution_list <- function(x) {
  if (!is.list(x) || length(x) == 0) {
    return(FALSE)
  }

  # Check if all elements are distribution specs
  # (have 'type' and 'params' fields)
  all(vapply(x, function(elem) {
    is.list(elem) && !is.null(elem$type) && !is.null(elem$params)
  }, logical(1)))
}

#' Convert R distribution specification to Julia Distribution object
#' @keywords internal
.to_julia_dist <- function(dist_spec) {
  if (is.null(dist_spec)) {
    return(NULL)
  }

  # Build Julia code based on type
  type_str <- dist_spec$type
  params <- dist_spec$params

  # Check if type contains placeholders
  # (like "truncated(Normal(mean, sd), lower, upper)")
  if (grepl("\\(", type_str) && !is.null(names(params))) {
    # Type is a template with named parameters
    # Replace each parameter name with its value
    julia_code <- type_str
    for (param_name in names(params)) {
      # Replace parameter name with its value
      julia_code <- gsub(
        paste0("\\b", param_name, "\\b"),
        as.character(params[[param_name]]),
        julia_code
      )
    }
    julia_code <- paste0("Distributions.", julia_code)
  } else {
    # Simple distribution like "Normal" with positional parameters
    params_str <- paste(params, collapse = ", ")
    julia_code <- sprintf("Distributions.%s(%s)", type_str, params_str)
  }

  .eval_julia_code(julia_code)
}

#' Convert R list of Julia distributions to Julia vector
#' @keywords internal
.to_julia_dist_vector <- function(dist_list) {
  if (length(dist_list) == 0) {
    return(NULL)
  }

  # Convert each R distribution spec to Julia distribution
  julia_dists <- lapply(dist_list, .to_julia_dist)

  # Create Julia vector from the distributions
  # We need to assign each to a Julia variable then create vector
  julia_var_names <- paste0("dist_", seq_along(julia_dists))

  for (i in seq_along(julia_dists)) {
    JuliaCall::julia_assign(julia_var_names[i], julia_dists[[i]])
  }

  # Create Julia array from variables
  vector_code <- sprintf("[%s]", paste(julia_var_names, collapse = ", "))
  result <- .eval_julia_code(vector_code)

  # Clean up temporary variables
  for (var_name in julia_var_names) {
    .eval_julia_code(sprintf("%s = nothing", var_name))
  }

  result
}

#' Convert R vector to Julia vector
#' @keywords internal
.to_julia_vector <- function(x) {
  # JuliaCall handles this automatically for most types
  x
}

#' Convert R vector to Julia tuple
#' @keywords internal
.to_julia_tuple <- function(x) {
  # Convert R vector to Julia tuple
  .eval_julia_code(sprintf("(%s,)", paste(x, collapse = ", ")))
}

#' Convert Julia array to R vector
#' @keywords internal
.from_julia_array <- function(julia_obj) {
  # JuliaCall handles this automatically
  julia_obj
}

#' Convert Turing.jl chains to posterior::draws_df
#' @keywords internal
.julia_chains_to_draws <- function(chains) {
  # Convert MCMCChains.Chains object to R data frame
  # MCMCChains provides array access via indexing

  # Assign chains to Julia variable
  JuliaCall::julia_assign("chains_tmp", chains)

  # Get the chain data as a 3D array (iterations, parameters, chains)
  # Convert to DataFrame in Julia for easier extraction
  chain_df <- tryCatch(
    {
      # Use MCMCChains.DataFrame to convert
      .eval_julia_code("using DataFrames")
      .eval_julia_code("DataFrame(chains_tmp)")
    },
    error = function(e) {
      # Fallback: manually extract the array and convert
      # Get dimensions
      n_iter <- .eval_julia_code("size(chains_tmp, 1)")
      n_chains <- .eval_julia_code("size(chains_tmp, 3)")

      # Get parameter names
      param_names <- .eval_julia_code("string.(names(chains_tmp))")

      # Get chain values as 3D array (iterations, parameters, chains)
      # Use .value to get the underlying AxisArray data
      chain_array <- .eval_julia_code("Array(chains_tmp.value)")

      # Convert to data frame
      # chain_array is (iterations, parameters, chains)
      df_list <- list()

      for (chain_id in 1:n_chains) {
        for (iter in 1:n_iter) {
          row_data <- as.list(chain_array[iter, , chain_id])
          names(row_data) <- param_names
          row_data$.chain <- chain_id
          row_data$.iteration <- iter
          row_data$.draw <- (chain_id - 1) * n_iter + iter
          df_list[[length(df_list) + 1]] <- as.data.frame(
            row_data, stringsAsFactors = FALSE
          )
        }
      }

      do.call(rbind, df_list)
    }
  )

  # Convert to posterior::draws_df
  posterior::as_draws_df(chain_df)
}

#' Prepare R data frame for Julia
#' @keywords internal
.prepare_data_for_julia <- function(data, tspan) {
  # Detect case column (try multiple names)
  case_col <- NULL
  for (name in c("cases", "confirm", "y_t", "counts")) {
    if (name %in% names(data)) {
      case_col <- name
      break
    }
  }

  if (is.null(case_col)) {
    stop(
      "Could not find case count column. Expected one of: ",
      "cases, confirm, y_t, counts",
      call. = FALSE
    )
  }

  # Extract time window
  start_idx <- as.integer(tspan[1])
  end_idx <- as.integer(tspan[2])

  # Validate data length
  data_length <- length(data[[case_col]])
  expected_length <- end_idx - start_idx + 1

  if (data_length < end_idx) {
    stop(
      "Data length (", data_length, ") is less than tspan end index (",
      end_idx, ").\n",
      "Expected at least ", end_idx, " observations to match tspan = c(",
      start_idx, ", ", end_idx, ")",
      call. = FALSE
    )
  }

  # Create Julia-compatible list
  extracted_data <- data[[case_col]][start_idx:end_idx]

  if (length(extracted_data) != expected_length) {
    stop(
      "Extracted data length (", length(extracted_data),
      ") does not match expected length (", expected_length, ").\n",
      "tspan = c(", start_idx, ", ", end_idx, ") requires ",
      expected_length, " observations",
      call. = FALSE
    )
  }

  list(
    y_t = extracted_data,
    dates = if ("date" %in% names(data)) data$date[start_idx:end_idx] else NULL
  )
}

#' Compute MCMC diagnostics
#' @keywords internal
.compute_diagnostics <- function(draws_obj) {
  # Extract Rhat and ESS from posterior summary
  summ <- posterior::summarise_draws(draws_obj)

  tibble::tibble(
    parameter = summ$variable,
    rhat = summ$rhat,
    ess_bulk = summ$ess_bulk,
    ess_tail = summ$ess_tail
  )
}

#' Print method for generic EpiAware calls
#'
#' @param x An \code{epiaware_generic} object.
#' @param ... Additional arguments (currently unused).
#'
#' @export
print.epiaware_generic <- function(x, ...) {
  cat("<EpiAware Generic: ", x$fn_name, ">\n", sep = "")
  cat("  Parameters:", length(x$spec), "\n")
  invisible(x)
}
