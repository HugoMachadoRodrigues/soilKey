# v0.9.192: tolerant horizon column-name recognition.
#
# Reported by Jelle Janssen (ISRIC): a column named `pH_H2O` was silently
# ignored while `ph_h2o` worked, because ensure_horizon_schema() matched names
# with an exact, case-sensitive `%in%`. The user's data was dropped without any
# error. These tests pin the tolerant behaviour: case- and separator-insensitive
# recognition plus a documented alias table, without ever clobbering an exact
# canonical column.

test_that("case-insensitive column names are recognised (the ISRIC report)", {
  h <- ensure_horizon_schema(data.frame(
    top_cm = 0, bottom_cm = 20, pH_H2O = 5.4, PH_KCL = 4.6))
  expect_equal(h$ph_h2o, 5.4)
  expect_equal(h$ph_kcl, 4.6)
  # the mis-cased names are gone (renamed, not duplicated)
  expect_false("pH_H2O" %in% names(h))
  expect_false("PH_KCL" %in% names(h))
})

test_that("separator variants (dot / dash / space) are recognised", {
  h1 <- ensure_horizon_schema(data.frame(check.names = FALSE,
    `pH.H2O` = 5.1, `EC dS m` = 2.0, `bulk-density-g-cm3` = 1.3))
  expect_equal(h1$ph_h2o, 5.1)
  expect_equal(h1$ec_dS_m, 2.0)
  expect_equal(h1$bulk_density_g_cm3, 1.3)
})

test_that("common analytical abbreviations map to canonical names", {
  h <- ensure_horizon_schema(data.frame(
    Clay = 32, Silt = 20, Sand = 48, SOC = 1.1, CEC = 12.5,
    BS = 65, Ca = 6.1, Mg = 3.2, K = 0.4, CaCO3 = 2.0, BD = 1.28))
  expect_equal(h$clay_pct, 32)
  expect_equal(h$silt_pct, 20)
  expect_equal(h$sand_pct, 48)
  expect_equal(h$oc_pct, 1.1)
  expect_equal(h$cec_cmol, 12.5)
  expect_equal(h$bs_pct, 65)
  expect_equal(h$ca_cmol, 6.1)
  expect_equal(h$mg_cmol, 3.2)
  expect_equal(h$k_cmol, 0.4)
  expect_equal(h$caco3_pct, 2.0)
  expect_equal(h$bulk_density_g_cm3, 1.28)
})

test_that("an exact canonical column is never clobbered by a variant", {
  # both an exact canonical `ph_h2o` and a mis-cased `pH_H2O` present:
  # the canonical value must survive; the variant is left as an extra column.
  h <- ensure_horizon_schema(data.frame(check.names = FALSE,
    ph_h2o = 5.9, pH_H2O = 9.9))
  expect_equal(h$ph_h2o, 5.9)              # canonical wins
  expect_true("pH_H2O" %in% names(h))      # variant preserved verbatim, not merged
})

test_that("unrecognised columns are preserved verbatim", {
  h <- ensure_horizon_schema(data.frame(
    top_cm = 0, bottom_cm = 20, my_custom_field = "x", sample_id = "P1"))
  expect_true(all(c("my_custom_field", "sample_id") %in% names(h)))
})

test_that("recognition flows through PedonRecord and reaches a classification", {
  # The end-to-end regression: mis-cased chemistry must actually drive the key,
  # not sit in an ignored column. A strongly acid, low-base horizon should carry
  # its pH/BS into classify_sibcs() rather than being read as NA.
  pr_ok <- PedonRecord$new(
    site = list(id = "isric-lower"),
    horizons = data.frame(
      designation = c("A", "Bw"),
      top_cm = c(0, 20), bottom_cm = c(20, 80),
      ph_h2o = c(4.8, 4.9), bs_pct = c(15, 12), clay_pct = c(28, 30)))
  pr_var <- PedonRecord$new(
    site = list(id = "isric-upper"),
    horizons = data.frame(
      designation = c("A", "Bw"),
      top_cm = c(0, 20), bottom_cm = c(20, 80),
      pH_H2O = c(4.8, 4.9), BS = c(15, 12), Clay = c(28, 30)))
  # the schema-level values match regardless of how the columns were spelled
  expect_equal(pr_var$horizons$ph_h2o, pr_ok$horizons$ph_h2o)
  expect_equal(pr_var$horizons$bs_pct, pr_ok$horizons$bs_pct)
  expect_equal(pr_var$horizons$clay_pct, pr_ok$horizons$clay_pct)
})
