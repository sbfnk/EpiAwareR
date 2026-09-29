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
  expect_true(identical(handle$owner, .epiaware_env))

  # Saving copies the environment by value, so a reloaded fit does not own
  # the Julia object and must not be taken for the original.
  file <- withr::local_tempfile()
  saveRDS(handle, file)
  expect_false(identical(readRDS(file)$owner, .epiaware_env))
})

test_that("only the owning session queues a handle for release", {
  .epiaware_env$released <- NULL
  local({
    owned <- .julia_handle(11L, "session-token")
    foreign <- .julia_handle(12L, "session-token")
    foreign$owner <- new.env()
    NULL
  })
  gc()
  queued <- vapply(.epiaware_env$released, function(x) x$handle, integer(1))
  expect_true(11L %in% queued)
  expect_false(12L %in% queued)
  .epiaware_env$released <- NULL
})

test_that("a fit loaded from disk says so rather than blaming Julia", {
  reloaded <- structure(
    list(
      generated = list(predicted_y_t = matrix(1:4, nrow = 2)),
      julia = list(handle = 1L, session = "session-token",
                   owner = new.env())
    ),
    class = "epiaware_fit"
  )
  expect_error(predict(reloaded, horizon = 2), "loaded from disk")
  expect_identical(dim(predict(reloaded)), c(2L, 2L))
})
