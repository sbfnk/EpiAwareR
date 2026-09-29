# Review criteria for composableIDModelR

composableIDModelR is a thin R interface to the Julia package
ComposableTuringIDModels.jl. R builds model specifications, renders them to
Julia source, and reads results back. Most defects worth finding live at that
boundary, where a wrong answer is silent: the model runs, the numbers look
plausible, and they are wrong.

Report correctness findings first. A finding needs a concrete failure: the
input, the resulting Julia code or value, and why it is wrong.

## The R to Julia boundary

- **Parameterisations.** distspec and Distributions.jl disagree, most often
  over rate against scale. Check every mapping in `.julia_distributions`
  against both sets of documentation, and check that a new one is rejected
  rather than guessed when the two cannot be reconciled (as for an uncertain
  `Gamma`, whose rate prior has no scale equivalent).
- **Index conventions.** A delay PMF starts at zero days and a generation
  interval at one. A conversion that keeps or drops the wrong element shifts an
  epidemic by a day.
- **Rendered values.** Doubles must round-trip, integers must stay integers
  where Julia expects an `Int`, and `Inf`, `NaN` and `NA` must render as Julia
  understands them. Keyword names with non-ASCII characters must be escaped in
  ASCII mode.
- **Draw ordering.** Chains flatten chain by chain, matching the `.chain` and
  `.iteration` columns. A transposed or interleaved result silently misaligns
  every generated quantity with its parameters.
- **Trajectory alignment.** Generated quantities shorter than the time axis are
  aligned to its end, and missing entries become `NaN`. Check padding whenever
  an observation model shortens the series.

## The bundled Julia code

- The pinned project in `inst/julia` and the R code must agree on which
  upstream version they target, including its compatibility bounds.
- Reaching into upstream internals (such as FlexiChains storage) needs a
  comment saying why no public API serves.
- Julia objects held behind handles must be released, and nothing may call
  Julia from an R finaliser.

## Errors and validation

- Arguments are validated in R, before Julia starts, with a message naming the
  argument and what would be valid.
- An upstream limitation should be reported as such, with the workaround, in
  preference to letting a Julia error surface.

## Tests

- Tests of construction, rendering and validation run without Julia.
- Tests that need Julia call `skip_if_no_julia()` and stay short.
- A fixed bug gains a test that fails without the fix. Behaviour verified by
  running a model in Julia counts; a test that only re-states the
  implementation does not.

## Documentation

- Examples that need Julia are wrapped in `\dontrun{}`.
- Documented argument names, parameterisations and defaults match the code, and
  where they differ from upstream the difference is stated.
- British English throughout.

## Out of scope

- Style that lintr already enforces, and formatting preferences.
- Upstream bugs. Note them and let the human open an issue upstream.
- Vignette and test runtime, unless a change makes it materially worse.
