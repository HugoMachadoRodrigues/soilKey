# v0.9.437: the units and phases of the FAO-Unesco Legend of 1974 as the
# audit of the FAO legends read them (Hugo, 2026-11-24, option (a) of R1, R4
# and R5, with B3): a unit gate reads the B the group gate accepted, its
# stand-ins and lower grade carried (R1); a calcic or gypsic horizon above
# the argillic B sends a soil past the Luvic units (R4); the petroferric
# phase asks an indurated layer with little or no organic matter, in both
# editions (B3, R5).

.f437 <- function(p, ed = "1974") suppressWarnings(classify_fao(p, edition = ed, on_missing = "silent"))
.ex437 <- function(code, nm = FALSE) make_fao_example(code, edition = "1974", near_miss = nm)
.edit437 <- function(p, ...) {
  h <- as.data.frame(p$horizons)
  v <- list(...)
  for (k in names(v)) h[[k]] <- v[[k]]
  suppressWarnings(PedonRecord$new(site = p$site, horizons = ensure_horizon_schema(data.table::as.data.table(h))))
}
.unit437 <- function(r) { lv <- r$levels; lv$code[lv$level == "units"][1L] }
.phase437 <- function(p, ed = "1974") {
  ph <- .f437(p, ed)$phases
  ph$status[ph$kind == "phase" & ph$code == "petroferric"]
}

test_that("R1: a unit gate reads the oxic B a stand-in gave the group, and carries the stand-in", {
  skip_on_cran()
  # the Xanthic Ferralsol of the examples with no weatherable minerals
  # recorded: the 1974 oxic B is undecided and oxic_horizon_usda() stands in
  p <- .edit437(.ex437("Fx"), weatherable_minerals_pct = NA_real_)
  ox <- soilKey:::oxic_b_fao74(p)
  expect_true(isTRUE(ox$passed))
  expect_true("oxic_horizon_usda" %in% ox$evidence$stand_in)
  # read again from the 1974 reader the B is undecided (up to 0.9.436)
  old <- soilKey:::b_colour_fao74(p, "xanthic", "oxic_b")
  expect_true(is.na(old$passed))
  # the B the gate accepted decides, and the stand-in travels with it
  new <- soilKey:::b_colour_fao74(p, "xanthic", "oxic_b", gate_b = TRUE)
  expect_true(isTRUE(new$passed))
  expect_true("oxic_horizon_usda" %in% new$evidence$stand_in)
  expect_true(isTRUE(new$evidence$lower_grade))
  expect_match(new$notes, "oxic_b_of_the_gate_by_stand_in", fixed = TRUE)
  r <- .f437(p)
  expect_identical(r$name, "Xanthic Ferralsols")
  expect_identical(r$evidence_grade, "C")
  expect_true(any(grepl("oxic_horizon_usda()", r$warnings, fixed = TRUE)))
})

test_that("R1: a B the 1974 reader decides reads as before, with no stand-in", {
  skip_on_cran()
  p <- .ex437("Fx")
  a <- soilKey:::b_colour_fao74(p, "xanthic", "oxic_b")
  b <- soilKey:::b_colour_fao74(p, "xanthic", "oxic_b", gate_b = TRUE)
  expect_identical(a$passed, b$passed)
  expect_length(b$evidence$stand_in, 0L)
  r <- .f437(p)
  expect_identical(r$name, "Xanthic Ferralsols")
  expect_identical(r$evidence_grade, "A")
})

test_that("R1: no layer of the accepted B within the depth asked is no B there", {
  skip_on_cran()
  p <- .edit437(.ex437("Fx"), weatherable_minerals_pct = NA_real_)
  r <- soilKey:::b_bs_below_fao74(p, kind = "oxic_b", lt = 50, to_cm = 10, gate_b = TRUE)
  expect_false(r$passed)
  expect_match(r$notes, "gate_oxic_b_below_10_cm", fixed = TRUE)
  # within 100 cm the base saturation of the accepted B is read (below 50 %)
  r100 <- soilKey:::b_bs_below_fao74(p, kind = "oxic_b", lt = 50, to_cm = 100, gate_b = TRUE)
  expect_true(isTRUE(r100$passed))
  expect_true("oxic_horizon_usda" %in% r100$evidence$stand_in)
})

test_that("R1: outside its group the predicate reads the 1974 B as before", {
  skip_on_cran()
  # a Chromic Luvisol has no oxic B: gate_b changes nothing
  p <- .ex437("Lc")
  for (k in c("oxic_b", "spodic_b")) {
    a <- soilKey:::b_colour_fao74(p, "chromic", k)
    b <- soilKey:::b_colour_fao74(p, "chromic", k, gate_b = TRUE)
    expect_identical(a$passed, b$passed, info = k)
  }
  expect_identical(soilKey:::ferralic_properties_fao74(.ex437("Be"), "cambisol")$passed,
                   soilKey:::ferralic_properties_fao74(.ex437("Be"), "cambisol", gate_b = TRUE)$passed)
})

test_that("R1: the unit lists read the B of the gate; the examples still key to themselves", {
  skip_on_cran()
  y <- function(f) yaml::read_yaml(system.file("rules", "fao1974", "units", f, package = "soilKey"))
  args <- function(f, grp, code) {
    u <- Find(function(z) identical(z$code, code), y(f)$units[[grp]])
    unlist(lapply(unlist(u$tests, recursive = FALSE), function(t) {
      if (is.list(t)) unlist(lapply(t, function(a) if (is.list(a)) a$gate_b))
    }))
  }
  units <- list(f_ferralsols.yaml = c(F = "Fh", F = "Fa", F = "Fr", F = "Fx"),
                d_podzoluvisols.yaml = c(D = "Dd"), n_nitosols.yaml = c(N = "Nh", N = "Nd"),
                l_luvisols.yaml = c(L = "Lc"), b_cambisols.yaml = c(B = "Bf", B = "Bd", B = "Bc"),
                p_podzols.yaml = c(P = "Ph", P = "Pf"), a_acrisols.yaml = c(A = "Ah"))
  for (f in names(units)) for (i in seq_along(units[[f]])) {
    code <- units[[f]][[i]]
    expect_true(any(args(f, names(units[[f]])[i], code) %in% TRUE), info = code)
    r <- .f437(.ex437(code))
    expect_identical(.unit437(r), code, info = code)
  }
  # the gate of the Acrisols still reads the 1974 B
  k <- yaml::read_yaml(system.file("rules", "fao1974", "key.yaml", package = "soilKey"))
  a <- Find(function(z) identical(z$code, "A"), k$major_groups)
  expect_null(a$tests$all_of[[3]]$b_bs_below_fao74$gate_b)
})

test_that("R4: a calcic horizon above the argillic B sends the soil past the Luvic unit", {
  skip_on_cran()
  # the Luvic Xerosol of the examples: A 0-20, Btk 20-70, Ck 70-130; the
  # calcic horizon at the top of the B is allowed
  p <- .ex437("Xl")
  expect_true(isTRUE(soilKey:::luvic_b_fao74(p)$passed))
  expect_identical(.unit437(.f437(p)), "Xl")
  # the same calcic horizon starting in the A, above the B: not Luvic
  q <- .edit437(p, caco3_pct = c(25, 25, 15))
  expect_true(isTRUE(soilKey:::calcic_fao74(q)$passed))
  lv <- soilKey:::luvic_b_fao74(q)
  expect_false(lv$passed)
  expect_match(lv$notes, "calcic_or_gypsic_above_the_argillic_b", fixed = TRUE)
  expect_true(isTRUE(soilKey:::argillic_b_fao74(q)$passed))
  r <- .f437(q)
  expect_identical(r$rsg_or_order, "Xerosols")
  expect_false(identical(.unit437(r), "Xl"))
  # the Luvic Chernozem and Kastanozem: a calcic horizon below the B is allowed
  for (cd in c("Cl", "Kl", "Yl")) expect_identical(.unit437(.f437(.ex437(cd))), cd, info = cd)
  # the Luvic units read luvic_b_fao74()
  for (f in c("c_chernozems.yaml", "k_kastanozems.yaml", "x_xerosols.yaml", "y_yermosols.yaml")) {
    u <- yaml::read_yaml(system.file("rules", "fao1974", "units", f, package = "soilKey"))$units[[1L]]
    lu <- Find(function(z) grepl("l$", z$code), u)
    expect_identical(names(lu$tests$all_of[[1L]]), "luvic_b_fao74", info = f)
  }
  # no argillic B: fails as the argillic B does
  expect_false(soilKey:::luvic_b_fao74(.ex437("Xk"))$passed)
})

test_that("B3 and R5: the petroferric phase asks an indurated layer with little or no organic matter", {
  skip_on_cran()
  # an ironstone pan within 100 cm under the Eutric Cambisol of the examples
  base <- function(...) {
    p <- .ex437("Be")
    h <- as.data.frame(p$horizons)
    n <- nrow(h)
    h <- h[c(seq_len(n), n), , drop = FALSE]
    h$top_cm[n] <- h$top_cm[n]; h$bottom_cm[n] <- 80
    h$top_cm[n + 1L] <- 80; h$bottom_cm[n + 1L] <- 110
    h$designation[n + 1L] <- "Bcm"
    h$pan_type <- c(rep(NA, n), "ironstone")
    h$plinthite_pct <- c(rep(0, n), 60)
    h$cementation_class <- c(rep(NA, n), "indurated")
    v <- list(...)
    for (k in names(v)) h[[k]][n + 1L] <- v[[k]]
    suppressWarnings(PedonRecord$new(site = p$site, horizons = ensure_horizon_schema(data.table::as.data.table(h))))
  }
  for (ed in c("1974", "1988")) {
    expect_identical(.phase437(base(), ed), "present", info = ed)
    expect_identical(.phase437(base(cementation_class = "strongly"), ed), "present", info = ed)
    # recorded weaker than strongly cemented: not indurated
    for (cc in c("none", "weakly", "moderately"))
      expect_identical(.phase437(base(cementation_class = cc), ed), "absent", info = paste(ed, cc))
    # a layer designated with h (organic matter)
    expect_identical(.phase437(base(designation = "Bhsm"), ed), "absent", info = ed)
    # a thin iron pan
    expect_identical(.phase437(base(pan_type = "thin iron pan"), ed), "absent", info = ed)
  }
})

test_that("B3 and R5: a layer of the spodic B is not petroferric", {
  skip_on_cran()
  p <- make_podzol_canonical()
  x <- soilKey:::.f74_ctx(p)
  sp <- soilKey:::.f88_member(x, soilKey:::.f88_hz_spodic_b(x)$units) %in% TRUE
  expect_true(any(sp))
  h <- as.data.frame(p$horizons)
  i <- x$idx[which(sp)[1L]]
  h$pan_type[i] <- "ironstone"
  h$cementation_class[i] <- "indurated"
  q <- suppressWarnings(PedonRecord$new(site = p$site, horizons = ensure_horizon_schema(data.table::as.data.table(h))))
  expect_identical(.phase437(q, "1974"), "absent")
})

test_that("R1, R4 and R5 are recorded in the 1974 readings", {
  skip_on_cran()
  rd <- yaml::read_yaml(system.file("rules", "fao1974", "readings.yaml", package = "soilKey"))$readings
  v437 <- Filter(function(e) identical(e$version, "0.9.437"), rd)
  expect_length(v437, 3L)
  txt <- vapply(v437, function(e) e$reading, "")
  expect_true(all(grepl("Hugo, 2026-11-24", txt, fixed = TRUE)))
  expect_true(any(grepl("FAO audit R1", txt, fixed = TRUE)))
  expect_true(any(grepl("FAO audit R4", txt, fixed = TRUE)))
  expect_true(any(grepl("B3 and R5", txt, fixed = TRUE)))
})
