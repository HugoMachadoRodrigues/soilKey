# v0.9.228: the SiBCS diagnostics read a horizon designation without the number
# of its material.
#
# A horizon below a lithologic discontinuity is written with the number of its
# material in front: "2Bt1" today, "IIBt1" in the surveys that numbered the
# materials in Roman numerals. The SiBCS diagnostics chose their horizons with
# patterns such as "^B" and "^A", which neither form matches, so a "IIBt" was
# not a B horizon for them: 56 patterns in 39 functions. In the national
# BDsolos export 1,984 horizons of 874 profiles carry such a number, 670 of
# them B horizons. The clearest loss was in Planossolos, whose B plânico is
# very often the top of a second material ("A1 A3 IIBt IIC"): the abrupt
# textural change passed in 186 of the 291 profiles with that reference order
# and the B plânico in 56.

.with_material <- function(p, prefix) {
  # the subsoil of the profile, from its first B or C horizon down, written as
  # the second material
  q <- p$clone(deep = TRUE)
  d <- as.character(q$horizons$designation)
  first <- which(grepl("^[BC]", d))[1]
  if (is.na(first)) return(NULL)
  i <- seq_along(d) >= first & grepl("^[A-Z]", d)
  d[i] <- paste0(prefix, d[i])
  q$horizons$designation <- d
  q
}

test_that(".desg_master() drops the number of the material, Arabic or Roman", {
  written <- c("Bt", "2Bt1", "IIBt", "IIB21t", "IIIC2g", "IVC4g", "VIC5", "IXC8g",
               "2Cr", "IIIR", "3R", " IIBt ", "10C")
  expect_equal(.desg_master(written),
               c("Bt", "Bt1", "Bt", "B21t", "C2g", "C4g", "C5", "C8g",
                 "Cr", "R", "R", "Bt", "C"))
  # nothing to drop
  same <- c("A1", "Ap", "Bt", "Bw2", "Cv", "R", "O2", "E", "BC", "B/C", "Cr")
  expect_equal(.desg_master(same), same)
  # a number with no horizon after it is left alone; NA stays NA
  expect_equal(.desg_master(c("II", "2", "", NA)), c("II", "2", "", NA))
  h <- data.table::data.table(designation = c("A", "IIBt"), top_cm = c(0, 20))
  expect_equal(.h_master(h)$designation, c("A", "Bt"))
  expect_equal(h$designation, c("A", "IIBt"))   # the data keep what was written
})

test_that("a B planico that is the top of a second material is a B planico", {
  skip_on_cran()
  p <- make_planossolo_canonical()
  expect_true(isTRUE(B_planico(p)$passed))
  for (prefix in c("II", "2")) {
    q <- .with_material(p, prefix)
    expect_true(any(grepl(paste0("^", prefix, "B"), q$horizons$designation)), info = prefix)
    b <- B_planico(q)
    expect_true(isTRUE(b$passed), info = prefix)
    expect_equal(b$layers, B_planico(p)$layers, info = prefix)
    expect_equal(classify_sibcs(q, on_missing = "silent")$rsg_or_order, "Planossolos",
                 info = prefix)
  }
})

test_that("the SiBCS tests answer the same for 'IIBt' and '2Bt' as for 'Bt'", {
  skip_on_cran()
  ns <- asNamespace("soilKey")
  tests <- c("horizonte_histico", "horizonte_A_chernozemico", "horizonte_A_humico",
             "B_incipiente", "B_nitico", "B_planico", "horizonte_concrecionario",
             "atividade_argila_alta", "eutrofico", "carater_alitico",
             "contato_litico_fragmentario", "carater_acrico", "carater_ebanico",
             "carater_espodico", "latossolo_ki_kr", "cerosidade", "carater_terrico",
             "carater_perferrico", "carater_sombrico", "carater_palico",
             "carater_rubrico", "carater_hidromorfico", "carater_cambissolico_arg",
             "carater_nitossolico", "carater_saprolitico", "carater_luvissolico",
             "carater_ferrico", "carater_cambissolico", "vertissolo",
             "espodossolo_humiluvico", "espodossolo_ferriluvico",
             "espodossolo_ferri_humiluvico", "neossolo_litolico", "organossolo_folico")
  expect_true(all(vapply(tests, exists, logical(1), envir = ns)))
  answer <- function(f, p) get(f, envir = ns)(p)$passed
  fx <- grep("^make_.*_canonical$", ls(ns), value = TRUE)
  for (f in fx) {
    p <- get(f, envir = ns)()
    for (prefix in c("II", "2")) {
      q <- .with_material(p, prefix)
      if (is.null(q)) next
      for (g in tests)
        expect_identical(answer(g, q), answer(g, p), info = paste(f, prefix, g))
    }
  }
})

test_that("the order of a canonical profile does not depend on how its materials are numbered", {
  skip_on_cran()
  ns <- asNamespace("soilKey")
  fx <- grep("^make_.*_canonical$", ls(ns), value = TRUE)
  order_of <- function(p) classify_sibcs(p, on_missing = "silent")$rsg_or_order
  for (f in fx) {
    p <- get(f, envir = ns)()
    for (prefix in c("II", "2")) {
      q <- .with_material(p, prefix)
      if (is.null(q)) next
      # v0.9.241: the number of the material is not only a way of writing. A
      # discontinuity between the A or E and the B is item (j) of the
      # B textural (SiBCS 2018, Cap 2, p. 56-57): the horizon then needs the
      # relacao textural of item (h) or the cerosidade of item (i). The B of
      # the canonical Solonetz is a B textural by its E horizon alone (item
      # f), and written as a second material it is none.
      ev <- B_textural(p)$evidence
      by_item_f_alone <- isTRUE(ev$item_f) && !isTRUE(ev$item_h) &&
        is.na(ev$item_i %||% NA_character_)
      if (by_item_f_alone) {
        expect_true(isTRUE(B_textural(p)$passed), info = paste(f, prefix))
        r <- B_textural(q)
        expect_true(r$evidence$item_j, info = paste(f, prefix))
        expect_false(isTRUE(r$passed), info = paste(f, prefix))
        next
      }
      expect_equal(order_of(q), order_of(p), info = paste(f, prefix))
    }
  }
})

test_that("a test that reads the master horizon reads no raw designation pattern", {
  skip_on_cran()
  # in every function that goes through .desg_master() or .h_master(), no
  # pattern is left applied to `h$designation` as written: one that was would
  # bring the dependence on the number of the material back. The functions are
  # read as loaded, so this holds for the installed package too.
  ns <- asNamespace("soilKey")
  src <- vapply(ls(ns, all.names = TRUE), function(f) {
    x <- get(f, envir = ns)
    if (is.function(x)) paste(deparse(x), collapse = "\n") else ""
  }, character(1))
  reads <- names(src)[grepl(".desg_master(", src, fixed = TRUE) |
                        grepl(".h_master(", src, fixed = TRUE)]
  reads <- setdiff(reads, c(".desg_master", ".h_master"))
  expect_gte(length(reads), 39L)
  raw <- 'grepl\\("[^"]*",\\s*h\\$designation|test_pattern_match\\(h, "designation"'
  for (f in reads) expect_false(grepl(raw, src[[f]]), info = f)
  # and the pattern does catch a raw one
  expect_true(grepl(raw, 'which(grepl("^B",\n    h$designation))'))
})
