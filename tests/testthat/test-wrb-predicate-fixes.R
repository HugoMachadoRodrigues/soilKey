# Tests for v0.9.125: WRB diagnostic corrections verified against the
# authoritative WRB 2022 (4th ed) text. See the audit report.

mk <- function(h) PedonRecord$new(horizons = h)

# ---- ornithogenic_material: WRB 2022 Ch 3.3.15 requires BOTH bird remnants
#      AND >= 750 mg/kg Mehlich-3 P (corrected from OR) -------------------------

test_that("ornithogenic needs both bird evidence AND >=750 mg/kg P", {
  both <- mk(data.frame(top_cm = c(0, 10), bottom_cm = c(10, 30),
                        designation = c("Aguano", "C"),
                        p_mehlich3_mg_kg = c(900, 50)))
  expect_true(ornithogenic_material(both)$passed)
})

test_that("ornithogenic fails on high P alone (no bird remnants)", {
  p_only <- mk(data.frame(top_cm = c(0, 10), bottom_cm = c(10, 30),
                          designation = c("A", "C"),
                          p_mehlich3_mg_kg = c(900, 50)))
  expect_false(ornithogenic_material(p_only)$passed)
})

test_that("ornithogenic fails on bird evidence alone (P < 750)", {
  bird_only <- mk(data.frame(top_cm = c(0, 10), bottom_cm = c(10, 30),
                             designation = c("Aguano", "C"),
                             p_mehlich3_mg_kg = c(200, 50)))
  expect_false(ornithogenic_material(bird_only)$passed)
})

# ---- plaggic: WRB 2022 Ch 3.1.29 criterion 2b is >= 100 mg/kg Mehlich-3 P ----

test_that("plaggic uses the canonical 100 mg/kg P (Mehlich-3) threshold", {
  expect_equal(formals(plaggic)$min_p_mehlich3, 100)
})
