test_that(".julia_project finds the bundled Julia project", {
  project <- .julia_project()
  expect_true(file.exists(file.path(project, "Project.toml")))
  expect_true(file.exists(file.path(project, "Manifest.toml")))
})

test_that(".manifest_julia_version reads the version from the Manifest", {
  expect_match(.manifest_julia_version(), "^[0-9]+\\.[0-9]+$")
  expect_null(.manifest_julia_version(tempdir()))
})

test_that("a handle belongs to the session that created it", {
  handle <- .julia_handle(7L, "session-token")
  expect_identical(handle$handle, 7L)
  expect_true(identical(handle$owner, .cidm_env))

  # Saving copies the environment by value, so a reloaded fit does not own
  # the Julia object and must not be taken for the original.
  file <- withr::local_tempfile()
  saveRDS(handle, file)
  expect_false(identical(readRDS(file)$owner, .cidm_env))
})

test_that("only the owning session queues a handle for release", {
  .cidm_env$released <- NULL
  local({
    owned <- .julia_handle(11L, "session-token")
    foreign <- .julia_handle(12L, "session-token")
    foreign$owner <- new.env()
    NULL
  })
  gc()
  queued <- vapply(.cidm_env$released, function(x) x$handle, integer(1))
  expect_true(11L %in% queued)
  expect_false(12L %in% queued)
  .cidm_env$released <- NULL
})

test_that("a fit loaded from disk says so rather than blaming Julia", {
  reloaded <- structure(
    list(
      generated = list(predicted_y_t = matrix(1:4, nrow = 2)),
      julia = list(handle = 1L, session = "session-token",
                   owner = new.env())
    ),
    class = "cidm_fit"
  )
  expect_error(predict(reloaded, horizon = 2), "loaded from disk")
  expect_identical(dim(predict(reloaded)), c(2L, 2L))
})

test_that("a Julia process that has gone away clears the setup flag", {
  was_ready <- .cidm_env$ready
  withr::defer(.cidm_env$ready <- was_ready)
  # Julia was set up and has since died, so the probe fails and the flag must
  # clear for the next call to set it up again.
  local_mocked_bindings(
    eval_julia = function(...) stop("Julia has gone away"),
    .package = "juliaready"
  )
  .cidm_env$ready <- TRUE
  expect_false(cidm_available())
  expect_false(isTRUE(.cidm_env$ready))
})
