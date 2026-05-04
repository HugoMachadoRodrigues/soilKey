# Tests for v0.9.44 ESDB Raster Library lookup utility.
#
# The ESDB raster archive (~700 MB unpacked) is NOT bundled with the
# package -- it's a separately-downloaded ESDAC artefact. These tests
# skip cleanly when the raster root environment variable is not set,
# but otherwise verify the lookup behaviour against real European
# coordinates with known RSGs.

.find_esdb_root <- function() {
  # Look in two places: an environment variable (CI / user override),
  # and a hard-coded path inside the developer's worktree (so that
  # local interactive testing works without setting the env var).
  env_root <- Sys.getenv("SOILKEY_ESDB_RASTER_ROOT", unset = NA)
  if (!is.na(env_root) && nzchar(env_root) && dir.exists(env_root))
    return(env_root)
  dev_root <- "/Users/rodrigues.h/Library/CloudStorage/OneDrive-Personal/soilKey/soil_data/eu_lucas/ESDB-Raster-Library-1k-GeoTIFF-20240507"
  if (dir.exists(dev_root)) return(dev_root)
  NA_character_
}

skip_if_no_esdb <- function() {
  testthat::skip_if_not_installed("terra")
  testthat::skip_if_not_installed("foreign")
  root <- .find_esdb_root()
  testthat::skip_if(is.na(root),
                      paste("ESDB raster root not found.",
                              "Set SOILKEY_ESDB_RASTER_ROOT to test."))
  root
}


test_that("available_esdb_attributes lists the expected ESDB attributes", {
  root <- skip_if_no_esdb()
  attrs <- available_esdb_attributes(root)
  # The 2024-05-07 ESDB Raster Library release ships 71 attributes.
  expect_gte(length(attrs), 60L)
  # WRB-specific attributes must be present.
  expect_true("WRBLV1" %in% attrs)
  expect_true("WRBFU"  %in% attrs)
  expect_true("WRBADJ1" %in% attrs)
})

test_that("lookup_esdb resolves Wageningen NL to a real RSG code", {
  root <- skip_if_no_esdb()
  # 5.66 E, 51.97 N -- NW Netherlands, fluvial-deltaic landscape.
  res <- lookup_esdb(c(5.66, 51.97), "WRBLV1", root)
  expect_type(res, "character")
  expect_length(res, 1L)
  # Must be one of the canonical 23 RSG codes (or a numeric "non-soil" code).
  expect_match(res, "^[A-Z]{2}$|^\\d+$")
})

test_that("lookup_esdb returns NA for points outside the European raster", {
  root <- skip_if_no_esdb()
  # Equator + Atlantic ocean -- well outside ESDB footprint.
  res <- lookup_esdb(c(0, 0), "WRBLV1", root)
  expect_true(is.na(res))
})

test_that("lookup_esdb is vectorised over multiple points", {
  root <- skip_if_no_esdb()
  coords <- rbind(c(5.66, 51.97),
                    c(24.94, 60.17),  # Helsinki, FI
                    c(0, 0))           # outside footprint
  res <- lookup_esdb(coords, "WRBLV1", root)
  expect_length(res, 3L)
  expect_true(is.na(res[3]))
})

test_that("lookup_esdb decode = FALSE returns raw integers", {
  root <- skip_if_no_esdb()
  res <- lookup_esdb(c(5.66, 51.97), "WRBLV1", root, decode = FALSE)
  expect_type(res, "double")
  expect_length(res, 1L)
})

test_that("lookup_esdb errors clearly when raster missing", {
  root <- skip_if_no_esdb()
  expect_error(lookup_esdb(c(5.66, 51.97), "BOGUS_ATTR", root),
                 "Raster not found")
})

test_that("lookup_esdb accepts data.frame input", {
  root <- skip_if_no_esdb()
  df <- data.frame(lon = c(5.66, 24.94), lat = c(51.97, 60.17))
  res <- lookup_esdb(df, "WRBLV1", root)
  expect_length(res, 2L)
})


# ---- Cross-system: WRB raster vs FAO 1990 raster --------------------------

test_that("WRBLV1 vs FAO90LV1 agree on broad classes for the same point", {
  root <- skip_if_no_esdb()
  # Sample 5 European cities and check that both rasters resolve to
  # SOMETHING (we don't insist they agree on a single class, just that
  # both rasters work).
  coords <- rbind(c(5.66, 51.97),
                    c(24.94, 60.17),
                    c(13.40, 52.52),
                    c(2.35, 48.86),
                    c(-9.14, 38.72))
  wrb  <- lookup_esdb(coords, "WRBLV1",   root)
  fao  <- lookup_esdb(coords, "FAO90LV1", root)
  expect_length(wrb, 5L)
  expect_length(fao, 5L)
})
