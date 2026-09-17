# Model components are held in R as lazy specifications and rendered to Julia
# source only when they are simulated from or fitted. Rendering is pure R, so
# models can be built, printed and inspected without starting Julia.

.roles <- c("distribution", "latent", "infection", "observation", "model")

#' Create a model component
#'
#' Low-level constructor behind every component in the package. Use it to
#' reach any `ComposableTuringIDModels.jl` (or `Distributions.jl`) constructor
#' that has no dedicated R wrapper.
#'
#' Arguments are rendered to Julia as follows: components and [julia()]
#' expressions are inserted as code, numeric vectors of length one become
#' scalars and longer ones become vectors, unnamed lists become vectors,
#' `NA` becomes `missing`, and character strings become Julia strings.
#' Integers (e.g. `2L`) render as Julia integers and doubles as floats.
#' `NULL` arguments are dropped, so the Julia default applies.
#'
#' @param fn Character string. Name of the Julia constructor.
#' @param ... Arguments to the constructor. Unnamed arguments are positional
#'   and named arguments become keyword arguments. Keyword names may contain
#'   non-ASCII characters.
#' @param role Character string. What the component is, used to check that
#'   components are composed sensibly: one of `"distribution"`, `"latent"`,
#'   `"infection"`, `"observation"` or `"model"`.
#'
#' @return An object of class `epiaware_component`.
#'
#' @family components
#' @examples
#' # A Frechet distribution, which distspec does not provide
#' component("Frechet", 3, 2, role = "distribution")
#'
#' # An exact Gaussian process latent model with default settings
#' component("ExactGP", role = "latent")
#' @export
component <- function(fn, ..., role) {
  checkmate::assert_string(fn, pattern = "^[A-Za-z_][A-Za-z0-9_.!]*$")
  role <- match.arg(role, .roles)
  dots <- list(...)
  arg_names <- names(dots)
  if (is.null(arg_names)) arg_names <- rep("", length(dots))
  keep <- !vapply(dots, is.null, logical(1))
  named <- nzchar(arg_names)
  structure(
    list(
      fn = fn,
      args = unname(dots[keep & !named]),
      kwargs = dots[keep & named]
    ),
    class = c(paste0("epiaware_", role), "epiaware_component")
  )
}

#' Embed Julia code in a model
#'
#' Marks a string as Julia source to be inserted verbatim when a model is
#' rendered, for arguments that cannot be expressed in R such as functions or
#' objects from other Julia packages.
#'
#' @param code Character string of Julia code.
#' @param role Optional role (see [component()]). Without a role the
#'   expression is accepted wherever a component is expected.
#'
#' @return An object of class `epiaware_julia`.
#'
#' @family components
#' @examples
#' # Model infections on the natural scale
#' DirectInfections(Z = RandomWalk(), transformation = julia("identity"))
#' @export
julia <- function(code, role = NULL) {
  checkmate::assert_string(code, min.chars = 1)
  if (!is.null(role)) role <- match.arg(role, .roles)
  structure(
    list(code = code),
    class = c(
      if (!is.null(role)) paste0("epiaware_", role),
      "epiaware_julia", "epiaware_component"
    )
  )
}

#' Render a component as Julia code
#'
#' @param x A model component, e.g. from [IDModel()] or [Renewal()].
#' @param ascii Logical. If `TRUE`, keyword names with non-ASCII characters
#'   are written with Unicode escapes so the code is pure ASCII. The default
#'   gives the more readable form that can be pasted into Julia.
#'
#' @return A character string of Julia code that constructs the component.
#'
#' @family components
#' @examples
#' as_julia(
#'   Renewal(generation_time = Gamma(shape = 6.5, scale = 0.62), rt = AR())
#' )
#' @export
as_julia <- function(x, ascii = FALSE) {
  .render(x, ascii = ascii)
}

#' Render an R value as Julia code
#'
#' @param x An R value or component.
#' @param ascii Logical. See [as_julia()].
#' @return A character string of Julia code.
#' @keywords internal
.render <- function(x, ascii = TRUE) {
  if (inherits(x, "dist_spec")) {
    # A distspec distribution outside a model slot, e.g. in `as_julia()`,
    # is read as a prior.
    return(.render(.as_prior(x), ascii = ascii))
  }
  if (inherits(x, "epiaware_julia")) {
    return(x$code)
  }
  if (inherits(x, "epiaware_component")) {
    return(.render_call(
      x$fn,
      vapply(x$args, .render, character(1), ascii = ascii),
      .render_kwargs(x$kwargs, ascii)
    ))
  }
  if (is.list(x)) {
    if (!is.null(names(x)) && any(nzchar(names(x)))) {
      stop("Named lists cannot be rendered as Julia values.", call. = FALSE)
    }
    return(.render_vector(vapply(x, .render, character(1), ascii = ascii)))
  }
  if (length(x) == 0) {
    stop("Cannot render an empty value as Julia code.", call. = FALSE)
  }
  scalars <- if (is.logical(x)) {
    ifelse(is.na(x), "missing", ifelse(x, "true", "false"))
  } else if (is.integer(x)) {
    ifelse(is.na(x), "missing", as.character(x))
  } else if (is.numeric(x)) {
    vapply(x, .render_float, character(1))
  } else if (is.character(x)) {
    vapply(x, .render_string, character(1), USE.NAMES = FALSE)
  } else {
    stop(
      "Cannot render an object of class '", class(x)[1], "' as Julia code.",
      call. = FALSE
    )
  }
  if (length(scalars) == 1) scalars else .render_vector(scalars)
}

#' Assemble a Julia call from rendered pieces
#'
#' @param fn Function name.
#' @param args Character vector of rendered positional arguments.
#' @param kwargs Character vector of rendered keyword arguments.
#' @return A character string.
#' @keywords internal
.render_call <- function(fn, args, kwargs) {
  inner <- paste(args, collapse = ", ")
  if (length(kwargs) > 0) {
    inner <- paste0(inner, "; ", paste(kwargs, collapse = ", "))
  }
  paste0(fn, "(", inner, ")")
}

#' Assemble a Julia vector from rendered elements
#'
#' @param elements Character vector of rendered elements.
#' @return A character string.
#' @keywords internal
.render_vector <- function(elements) {
  paste0("[", paste(elements, collapse = ", "), "]")
}

#' Render keyword arguments
#'
#' In ASCII mode, names with non-ASCII characters are splatted in as `Symbol`
#' pairs built from Unicode escapes, so the code survives transfer to Julia on
#' any platform.
#'
#' @param kwargs Named list of R values.
#' @param ascii Logical. See [as_julia()].
#' @return Character vector of rendered keyword arguments.
#' @keywords internal
.render_kwargs <- function(kwargs, ascii) {
  if (length(kwargs) == 0) {
    return(character())
  }
  kw_names <- names(kwargs)
  values <- vapply(kwargs, .render, character(1), ascii = ascii)
  out <- paste0(kw_names, " = ", values)
  escape <- ascii & grepl("[^ -~]", kw_names)
  out[escape] <- sprintf(
    "(Symbol(%s) => %s,)...",
    vapply(kw_names[escape], .render_string, character(1)), values[escape]
  )
  out
}

#' Render a double as a Julia float literal
#'
#' @param x A numeric scalar.
#' @return A character string.
#' @keywords internal
.render_float <- function(x) {
  if (is.nan(x)) {
    return("NaN")
  }
  if (is.na(x)) {
    return("missing")
  }
  if (is.infinite(x)) {
    return(if (x > 0) "Inf" else "-Inf")
  }
  out <- sprintf("%.15g", x)
  if (as.numeric(out) != x) out <- sprintf("%.17g", x)
  if (!grepl("[.e]", out)) out <- paste0(out, ".0")
  out
}

#' Render a string as an ASCII Julia string literal
#'
#' @param x A character scalar.
#' @return A character string.
#' @keywords internal
.render_string <- function(x) {
  chars <- vapply(utf8ToInt(enc2utf8(x)), function(code) {
    if (code %in% c(34L, 36L, 92L)) {
      paste0("\\", intToUtf8(code))
    } else if (code >= 32L && code <= 126L) {
      intToUtf8(code)
    } else {
      sprintf("\\u%04x", code)
    }
  }, character(1))
  paste0("\"", paste(chars, collapse = ""), "\"")
}

#' Format a component as indented Julia code
#'
#' Calls that fit within `width` stay on one line; longer ones put each
#' argument on its own line.
#'
#' @param x A component or R value.
#' @param width Maximum line width.
#' @param indent Current indentation level.
#' @return Character vector of lines.
#' @keywords internal
.format_code <- function(x, width = 78L, indent = 0L) {
  pad <- strrep("    ", indent)
  flat <- .render(x, ascii = FALSE)
  is_vector <- is.list(x) && !inherits(x, "epiaware_component")
  if (nchar(pad) + nchar(flat) <= width || inherits(x, "epiaware_julia") ||
        !(is_vector || inherits(x, "epiaware_component"))) {
    return(paste0(pad, flat))
  }
  if (is_vector) {
    elements <- lapply(x, .format_code, width = width, indent = indent + 1L)
    return(c(paste0(pad, "["), .join_lines(elements), paste0(pad, "]")))
  }
  inner_pad <- strrep("    ", indent + 1L)
  args <- lapply(x$args, .format_code, width = width, indent = indent + 1L)
  kwargs <- Map(function(name, value) {
    lines <- .format_code(value, width - nchar(name) - 3L, indent + 1L)
    lines[1] <- paste0(inner_pad, name, " = ", trimws(lines[1], "left"))
    lines
  }, names(x$kwargs), x$kwargs)
  open <- if (length(args) == 0 && length(kwargs) > 0) "(;" else "("
  c(
    paste0(pad, x$fn, open),
    .join_lines(args, if (length(kwargs) > 0) ";" else ""),
    .join_lines(unname(kwargs)),
    paste0(pad, ")")
  )
}

#' Join formatted arguments with separators
#'
#' @param pieces List of character vectors, one per argument.
#' @param last_sep Separator after the final argument.
#' @return Character vector of lines.
#' @keywords internal
.join_lines <- function(pieces, last_sep = "") {
  unlist(lapply(seq_along(pieces), function(i) {
    lines <- pieces[[i]]
    n <- length(lines)
    lines[n] <- paste0(lines[n], if (i < length(pieces)) "," else last_sep)
    lines
  }))
}

#' Check that an argument is a component of the given role(s)
#'
#' Julia expressions created by [julia()] without a role are accepted for any
#' role.
#'
#' @param x Object to check.
#' @param roles Character vector of accepted roles.
#' @param null_ok Logical. Whether `NULL` is accepted.
#' @param arg_name Name used in error messages.
#' @return Invisibly `TRUE`.
#' @keywords internal
.assert_role <- function(x, roles, null_ok = FALSE,
                         arg_name = deparse(substitute(x))) {
  untyped_julia <- identical(
    class(x), c("epiaware_julia", "epiaware_component")
  )
  if ((is.null(x) && null_ok) || untyped_julia ||
        inherits(x, paste0("epiaware_", roles))) {
    return(invisible(TRUE))
  }
  labels <- c(
    distribution = "a distribution (e.g. `Normal()`)",
    latent = "a latent model (e.g. `RandomWalk()`)",
    infection = "an infection model (e.g. `Renewal()`)",
    observation = "an observation model (e.g. `PoissonError()`)",
    model = "a composed model (from `IDModel()`)",
    julia = "Julia code from `julia()`"
  )
  stop(
    "`", arg_name, "` must be ", paste(labels[roles], collapse = " or "), ".",
    call. = FALSE
  )
}

#' @export
print.epiaware_component <- function(x, ...) {
  role <- sub("^epiaware_", "", class(x)[1])
  label <- if (role %in% .roles) paste(role, "component") else "Julia code"
  cat("<EpiAwareR ", label, ">\n", sep = "")
  cat(.format_code(x), sep = "\n")
  invisible(x)
}
