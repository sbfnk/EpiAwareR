# Contributing to EpiAwareR

Thank you for your interest in contributing to EpiAwareR! This document provides guidelines for contributing to the project.

## Code of Conduct

By participating in this project, you agree to maintain a respectful and inclusive environment for all contributors.

## Types of Contributions

### Bug Reports

If you find a bug, please open an issue with:

- A clear, descriptive title
- A minimal reproducible example
- Your R and Julia versions (`sessionInfo()` output)
- Expected vs. actual behavior

### Feature Requests

We welcome suggestions for new features! Please:

- Check if the feature already exists in the Julia EpiAware package
- Describe the use case and expected behavior
- Consider whether it should be a dedicated R wrapper or accessible via `epiaware_call()`

### Code Contributions

1. **Fork and clone** the repository
2. **Create a branch** for your changes: `git checkout -b feature/my-feature`
3. **Make your changes** following the code style below
4. **Add tests** for any new functionality
5. **Update documentation** including roxygen2 comments
6. **Run checks**: `devtools::check()`
7. **Submit a pull request** with a clear description

## Development Setup

### Prerequisites

- R >= 4.0.0
- Julia >= 1.9.0
- Development tools:
  - `devtools`: `install.packages("devtools")`
  - `roxygen2`: `install.packages("roxygen2")`
  - `testthat`: `install.packages("testthat")`

### Setup

```r
# Clone the repository
git clone https://github.com/CDCgov/Rt-without-renewal.git
cd Rt-without-renewal/EpiAwareR

# Load package for development
devtools::load_all()

# Set up Julia
epiaware_setup_julia()
```

## Code Style

### R Code Style

Follow standard R conventions:

- **Function names**: `snake_case` for user-facing functions
- **Internal functions**: Prefix with `.` (e.g., `.internal_helper`)
- **Variable names**: `snake_case`
- **Constants**: `SCREAMING_SNAKE_CASE`
- **Line length**: Maximum 80 characters when practical
- **Indentation**: 2 spaces (no tabs)

### Documentation

All exported functions must have roxygen2 documentation:

```r
#' Brief Title
#'
#' Detailed description of what the function does.
#'
#' @param param1 Description of param1
#' @param param2 Description of param2
#'
#' @return Description of return value
#'
#' @examples
#' \dontrun{
#' # Example usage
#' result <- my_function(param1 = 1, param2 = 2)
#' }
#'
#' @export
my_function <- function(param1, param2) {
  # Implementation
}
```

### S3 Classes

EpiAwareR uses S3 classes for simplicity:

- **Constructor pattern**: Validate inputs, call Julia, return structured list with `julia_ref`
- **Class inheritance**: Use multiple inheritance (e.g., `c("epiaware_ar", "epiaware_latent", "epiaware_model")`)
- **Print methods**: Always provide informative `print()` methods

## Testing

### Writing Tests

- Place tests in `tests/testthat/test-*.R`
- Use descriptive test names: `test_that("function validates inputs", { ... })`
- Test edge cases and error conditions
- Skip long-running tests with `skip_on_cran()`
- Skip tests requiring Julia with `skip_if_not(epiaware_available())`

### Running Tests

```r
# Run all tests
devtools::test()

# Run specific test file
testthat::test_file("tests/testthat/test-components.R")

# Check package
devtools::check()
```

## Adding New Components

### Adding a New Wrapped Component

1. **Create constructor function** in appropriate file (e.g., `R/latent-models.R`)
2. **Validate inputs** in R
3. **Convert distributions** using `.to_julia_dist()`
4. **Call Julia constructor** using `.call_julia_function()`
5. **Return S3 object** with proper class inheritance
6. **Add print method**
7. **Write tests** in `tests/testthat/`
8. **Update documentation** in README and vignettes
9. **Add examples** to function documentation

Example structure:

```r
#' My New Model
#'
#' @param param1 Description
#' @export
MyNewModel <- function(param1) {
  # Validate
  if (!is.numeric(param1)) stop("param1 must be numeric")

  # Convert to Julia
  julia_obj <- .call_julia_function("MyNewModel", param1 = param1)

  # Return S3 object
  structure(
    list(julia_ref = julia_obj, spec = list(param1 = param1)),
    class = c("epiaware_mynew", "epiaware_latent", "epiaware_model")
  )
}

#' @export
print.epiaware_mynew <- function(x, ...) {
  cat("<EpiAware My New Model>\n")
  cat("  Param1:", x$spec$param1, "\n")
  invisible(x)
}
```

### Adding Access to Julia Features

For features not yet wrapped, users can use `epiaware_call()`:

```r
custom_model <- epiaware_call("JuliaFunction", param1 = value1)
```

Document these in the function reference with guidance on when explicit wrappers are warranted.

## Documentation

### Regenerating Documentation

```r
# Update roxygen documentation
devtools::document()

# Build vignettes
devtools::build_vignettes()
```

### Building README

If `README.Rmd` exists:

```r
# Knit README
knitr::knit("README.Rmd")
```

## Release Process

1. Update version in `DESCRIPTION`
2. Update `NEWS.md` with changes
3. Run `devtools::check()` with no errors, warnings, or notes
4. Build and check package: `devtools::check()`
5. Submit to CRAN or create GitHub release

## Questions?

If you have questions about contributing:

- Open a discussion on GitHub
- Contact the package maintainer: sebastian.funk@lshtm.ac.uk
- Refer to the [EpiAware Julia documentation](https://cdcgov.github.io/Rt-without-renewal/)

## License

By contributing, you agree that your contributions will be licensed under the MIT License.
