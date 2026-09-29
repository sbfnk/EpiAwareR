test_that("scalars render as Julia literals", {
  expect_identical(.render(1), "1.0")
  expect_identical(.render(2L), "2")
  expect_identical(.render(0.1), "0.1")
  expect_identical(.render(1e-20), "1e-20")
  expect_identical(.render(-Inf), "-Inf")
  expect_identical(.render(NA_real_), "missing")
  expect_identical(.render(TRUE), "true")
  expect_identical(.render("a\"b"), "\"a\\\"b\"")
  expect_identical(.render("\u03f5"), "\"\\u03f5\"")
})

test_that("doubles round-trip exactly", {
  x <- 1 / 3
  expect_identical(as.numeric(.render(x)), x)
})

test_that("vectors and lists render as Julia vectors", {
  expect_identical(.render(c(0.2, 0.8)), "[0.2, 0.8]")
  expect_identical(.render(list(0.5)), "[0.5]")
  expect_identical(
    .render(list(Normal(0, 1), HalfNormal(2))),
    "[Normal(0.0, 1.0), HalfNormal(2.0)]"
  )
  expect_error(.render(list(a = 1)), "Named lists")
  expect_error(.render(numeric()), "empty")
  expect_error(.render(sum), "Cannot render")
})

test_that("components render positional and keyword arguments", {
  expect_identical(
    as_julia(component("F", 1, a = 2L, b = NULL, role = "latent")),
    "F(1.0; a = 2)"
  )
  expect_identical(as_julia(PoissonError()), "PoissonError()")
  expect_identical(
    as_julia(RandomWalk(init = Normal(0, 0.5))),
    "RandomWalk(; init = Normal(0.0, 0.5))"
  )
})

test_that("non-ASCII keywords are escaped only in ASCII mode", {
  rw <- RandomWalk(epsilon_t = HierarchicalNormal())
  expect_identical(
    as_julia(rw), "RandomWalk(; \u03f5_t = HierarchicalNormal())"
  )
  expect_identical(
    as_julia(rw, ascii = TRUE),
    "RandomWalk(; (Symbol(\"\\u03f5_t\") => HierarchicalNormal(),)...)"
  )
})

test_that("julia() inserts code verbatim and satisfies any role", {
  expect_identical(as_julia(julia("exp")), "exp")
  inf <- DirectInfections(Z = julia("RandomWalk()"))
  expect_identical(as_julia(inf), "DirectInfections(; Z = RandomWalk())")
  typed <- julia("PoissonError()", role = "observation")
  expect_error(DirectInfections(Z = typed), "latent model")
})

test_that("printing breaks long models over lines", {
  model <- IDModel(
    Renewal(generation_time = Gamma(shape = 6.5, scale = 0.62), rt = AR()),
    NegativeBinomialError(cluster_factor = HalfNormal(0.1))
  )
  lines <- .format_code(model)
  expect_gt(length(lines), 1)
  expect_true(all(nchar(lines) <= 78))
  expect_output(print(model), "<EpiAwareR model component>")
  expect_output(print(HalfNormal(0.1)), "HalfNormal\\(0.1\\)")
})

test_that("strings outside the basic plane use the eight-digit escape", {
  # Julia's \u takes at most four hex digits, so ὠ0 would be two
  # characters rather than one emoji.
  expect_identical(.render("a\U0001F600b"), "\"a\\U0001f600b\"")
  expect_identical(.render("café"), "\"caf\\u00e9\"")
})

test_that("arguments that would render as invalid or renumbered are refused", {
  expect_error(
    component("g", 1, NULL, 3, role = "latent"), "positional argument"
  )
  expect_error(
    component("F", `a b` = 1, role = "latent"), "Julia identifiers"
  )
  # Julia identifiers may use letters from any script
  expect_identical(
    as_julia(AR(epsilon_t = HierarchicalNormal())),
    "AR(; ϵ_t = HierarchicalNormal())"
  )
  expect_identical(as_julia(component("F", a = NULL, b = 2, role = "latent")),
                   "F(; b = 2.0)")
})
