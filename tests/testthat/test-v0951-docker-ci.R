# =============================================================================
# Tests for v0.9.51 -- Dockerfile + GitHub Actions docker-build workflow.
#
# The full container build runs on CI (push tag -> ghcr.io/<owner>/soilKey:<tag>);
# locally we just lint the artefacts to make sure the build context is shipped
# correctly. These tests run unconditionally and are tier-0 fast.
# =============================================================================

.find_repo_root <- function() {
  cands <- c(".", "..", "../..", "../../..")
  for (c in cands) {
    if (file.exists(file.path(c, "DESCRIPTION")) &&
          file.exists(file.path(c, "NAMESPACE"))) {
      return(normalizePath(c))
    }
  }
  NULL
}


test_that("Dockerfile exists at repo root", {
  root <- .find_repo_root()
  if (is.null(root)) skip("Repo root not found from test working directory")
  expect_true(file.exists(file.path(root, "Dockerfile")))
})


test_that("Dockerfile pins a stable rocker base image", {
  root <- .find_repo_root()
  if (is.null(root)) skip("Repo root not found")
  txt <- readLines(file.path(root, "Dockerfile"))
  # Must FROM rocker/r-ver with a pinned tag (not 'latest')
  from_line <- grep("^FROM rocker/r-ver:", txt, value = TRUE)
  expect_true(length(from_line) >= 1L)
  expect_false(any(grepl(":latest$", from_line)))
})


test_that("Dockerfile installs the GDAL stack required by terra", {
  root <- .find_repo_root()
  if (is.null(root)) skip("Repo root not found")
  txt <- paste(readLines(file.path(root, "Dockerfile")), collapse = "\n")
  for (pkg in c("libgdal-dev", "libgeos-dev", "libproj-dev")) {
    expect_match(txt, pkg, fixed = TRUE,
                  info = sprintf("Dockerfile must install %s", pkg))
  }
})


test_that("Dockerfile installs key Suggests (terra, foreign, pls, munsellinterpol)", {
  root <- .find_repo_root()
  if (is.null(root)) skip("Repo root not found")
  txt <- paste(readLines(file.path(root, "Dockerfile")), collapse = "\n")
  for (pkg in c("terra", "foreign", "pls", "munsellinterpol", "shiny", "DT")) {
    expect_match(txt, sprintf("'%s'", pkg), fixed = TRUE,
                  info = sprintf("Dockerfile must install %s", pkg))
  }
})


test_that(".dockerignore excludes soil_data/ and other heavy artefacts", {
  root <- .find_repo_root()
  if (is.null(root)) skip("Repo root not found")
  ignore <- file.path(root, ".dockerignore")
  expect_true(file.exists(ignore))
  txt <- readLines(ignore)
  expect_true(any(grepl("^soil_data/?$", txt)))
  expect_true(any(grepl("^\\.git/?$", txt)))
  expect_true(any(grepl("^\\*\\.tif$", txt)))
})


test_that("docker.yaml workflow builds + pushes on tag", {
  root <- .find_repo_root()
  if (is.null(root)) skip("Repo root not found")
  yaml_path <- file.path(root, ".github", "workflows", "docker.yaml")
  expect_true(file.exists(yaml_path))
  txt <- paste(readLines(yaml_path), collapse = "\n")
  # YAML's "on" keyword parses as TRUE in some loaders, so we
  # regex-match instead of structure-match.
  expect_match(txt, "tags:\\s*\\n\\s*-\\s*\"?v\\*\"?")
  expect_match(txt, "ghcr.io/", fixed = TRUE)
  expect_match(txt, "docker/build-push-action", fixed = TRUE)
})


test_that("docker.yaml smoke-tests the published image", {
  root <- .find_repo_root()
  if (is.null(root)) skip("Repo root not found")
  txt <- paste(readLines(file.path(root, ".github", "workflows", "docker.yaml")),
                 collapse = "\n")
  # Final step: docker run + library(soilKey)
  expect_match(txt, "library\\(soilKey\\)|requireNamespace\\(.soilKey.\\)")
})
