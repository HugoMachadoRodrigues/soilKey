# =============================================================================
# v0.9.432 -- USDA family: the mineralogy classes and the human-altered and
# human-transported material classes (Soil Survey Staff 2022, Keys to Soil
# Taxonomy 13th ed., ch. 17, pp. 362-366).
#
# Three-valued throughout (TRUE / FALSE / NA). A field observation not
# recorded is absent; a measurement not recorded leaves the test undecided;
# an undecided class is value NULL with the missing columns in $missing and
# the reason in $evidence. The readings F1-F4 and F19 are those of the author
# (USDA audit, wave 5, 2026-11-23); see NEWS.md, 0.9.432.
#
# Each quantity of a layer is carried as an interval [lo, hi]: a recorded
# value is a point; a mineral not listed by a mineralogical analysis of the
# layer (mineralogy_analysed TRUE) is the point 0 (F2); anything else is
# [0, cap], the cap being 100 or a bound the record gives (Fe by dithionite
# never exceeds the total iron). The control section is averaged as in the
# particle-size key (.wavg_clip_usda(), reading F18): the recorded points
# when they cover half the section, else the interval of all its layers. A
# test is TRUE or FALSE only when the whole interval falls on one side.
# =============================================================================

.MIN_REF_USDA <- "Soil Survey Staff (2022), KST 13th ed., Ch. 17, pp. 363-366"
.HAHT_REF_USDA <- "Soil Survey Staff (2022), KST 13th ed., Ch. 17, pp. 362-363"

# Quantities of the mineralogy key and the columns that record them.
.MIN_COLS_USDA <- list(
  fe2o3 = "fe_dcb_pct", gib = c("gibbsite_pct", "clay_pct"),
  gb = c("gibbsite_pct", "clay_pct", "boehmite_pct"),
  fegib = c("fe_dcb_pct", "gibbsite_pct", "clay_pct"),
  anhy = "anhydrite_pct", gyps = c("caso4_pct", "gypsum_lt20mm_pct"),
  cg = c("caco3_pct", "caso4_pct", "gypsum_lt20mm_pct"),
  opal = "opaline_pct", glauc = "glauconite_pct", mgsil = "mg_silicates_pct",
  sf = c("si_ox_pct", "fe_ox_pct"), sdiff = c("si_ox_pct", "fe_ox_pct"),
  glass = "volcanic_glass_pct", mica = "mica_grains_pct",
  resist = "resistant_minerals_pct",
  kaol = "kaolinite_pct", hall = "halloysite_pct", smec = "smectite_pct",
  illi = "illite_pct", verm = "vermiculite_pct", allo = "allophane_pct",
  okao = "kaolin_other_pct", gibc = "gibbsite_pct",
  kgrp = c("kaolinite_pct", "halloysite_pct", "kaolin_other_pct", "gibbsite_pct"),
  hka = c("halloysite_pct", "kaolinite_pct", "allophane_pct"))

# The minerals a mineralogical analysis of the layer would list (F2): with
# mineralogy_analysed TRUE, one not recorded is a measured zero.
.MIN_F2_COLS_USDA <- c(
  "anhydrite_pct", "opaline_pct", "glauconite_pct", "mg_silicates_pct",
  "illite_pct", "vermiculite_pct", "allophane_pct", "kaolinite_pct",
  "halloysite_pct", "smectite_pct", "kaolin_other_pct", "gibbsite_pct",
  "boehmite_pct", "mica_grains_pct", "resistant_minerals_pct",
  "volcanic_glass_pct")

# The clay minerals compared by "more ... than any other single kind".
.MIN_CLAY_KINDS_USDA <- c(kaolinite = "kaol", halloysite = "hall",
                          smectite = "smec", illite = "illi",
                          vermiculite = "verm", allophane = "allo",
                          gibbsite = "gibc")


# ---- three-valued tests on intervals ---------------------------------------

.min_gt <- function(iv, t) if (iv[1] > t) TRUE else if (iv[2] <= t) FALSE else NA
.min_ge <- function(iv, t) if (iv[1] >= t) TRUE else if (iv[2] < t) FALSE else NA
.min_lt <- function(iv, t) if (iv[2] < t) TRUE else if (iv[1] >= t) FALSE else NA
.min_in <- function(iv, a, b)
  if (iv[1] >= a && iv[2] <= b) TRUE else if (iv[2] < a || iv[1] > b) FALSE else NA
.min_more <- function(a, b) if (a[1] > b[2]) TRUE else if (a[2] <= b[1]) FALSE else NA


# ---- per-layer quantities ------------------------------------------------

# A per-layer interval: recorded -> point; else 0 when `zero` (F2), else
# [0, cap].
.min_iv <- function(x, zero, cap = 100) {
  cap <- ifelse(is.na(cap), 100, cap)
  cbind(lo = ifelse(!is.na(x), x, 0),
        hi = ifelse(!is.na(x), x, ifelse(zero, 0, cap)))
}
.min_add <- function(...) {
  m <- list(...)
  cbind(lo = pmin(Reduce(`+`, lapply(m, function(x) x[, "lo"])), 100),
        hi = pmin(Reduce(`+`, lapply(m, function(x) x[, "hi"])), 100))
}

# Free carbonates of each layer: TRUE (caco3_pct above 0, a reaction with
# HCl, the suffix k, or secondary carbonates recorded), FALSE (caco3_pct 0
# or effervescence 'none'), NA (no record).
.min_free_carbonates <- function(h) {
  n <- nrow(h)
  cc <- .ps_num(h, "caco3_pct")
  ef <- .ps_txt(h, "effervescence_class")
  ex <- .ps_txt(h, "effervescence_extent")
  sc <- .ps_num(h, "secondary_carbonates_pct")
  d <- .usda_desig(h$designation %||% rep(NA_character_, n))
  yes <- (!is.na(cc) & cc > 0) |
    (!is.na(ef) & ef != "none" & ef != "" & !grepl("^non", ef)) |
    (!is.na(ex) & ex %in% c("local", "continuous")) |
    (!is.na(sc) & sc > 0) | (!is.na(d$suffix) & grepl("k", d$suffix))
  no <- (!is.na(cc) & cc == 0) | (!is.na(ef) & (ef == "none" | grepl("^non", ef))) |
    (!is.na(ex) & ex == "none")
  ifelse(yes, TRUE, ifelse(no, FALSE, NA))
}

# The quantities of every layer, as intervals (columns <q>_lo, <q>_hi), with
# the per-layer facts the isotic test reads.
.min_layers_usda <- function(pedon) {
  h <- as.data.frame(pedon$horizons)
  n <- nrow(h)
  num <- function(c) .ps_num(h, c)
  an <- as.logical(.usda_col(h, "mineralogy_analysed", NA)) %in% TRUE
  z <- function(col) an & col %in% .MIN_F2_COLS_USDA
  iv <- function(col, cap = 100) .min_iv(num(col), z(col), cap)
  clay <- num("clay_pct")
  # Fe2O3 = 1.43 x Fe by dithionite (p. 364); total iron (as Fe2O3) bounds it
  fe2o3 <- .min_iv(1.43 * num("fe_dcb_pct"), rep(FALSE, n), num("fe_total_pct"))
  # gibbsite_pct is % of the clay: % of the fine earth = gibbsite x clay / 100
  # (decision of the author); clay not recorded, gibbsite_pct bounds it
  gpc <- num("gibbsite_pct")
  gib <- cbind(lo = ifelse(!is.na(gpc) & !is.na(clay), gpc * clay / 100, 0),
               hi = ifelse(!is.na(gpc), ifelse(!is.na(clay), gpc * clay / 100, gpc),
                           ifelse(z("gibbsite_pct"), 0, 100)))
  # gypsum: the higher of the fine earth and the fraction < 20 mm (p. 365).
  # Not recorded: none where the layer has no field record of gypsum (the
  # reading of the particle-size key, v0.9.431), else undecided.
  d <- .usda_desig(h$designation %||% rep(NA_character_, n))
  gvis <- num("gypsum_visible_pct")
  ghint <- (!is.na(gvis) & gvis > 0) | (!is.na(d$suffix) & grepl("y", d$suffix))
  gfe <- num("caso4_pct"); g20 <- num("gypsum_lt20mm_pct")
  gmax <- suppressWarnings(pmax(gfe, g20, na.rm = TRUE))
  gyps <- .min_iv(gmax, is.na(gmax) & !ghint)
  # carbonates: more than 40 % with gypsum (p. 365); not recorded, none where
  # the layer shows no free carbonates in the field record (reading M1)
  fc <- .min_free_carbonates(h)
  cc <- num("caco3_pct")
  carb <- .min_iv(cc, is.na(cc) & !(fc %in% TRUE))
  si <- iv("si_ox_pct"); fo <- iv("fe_ox_pct")
  sf <- cbind(lo = 8 * si[, "lo"] + 2 * fo[, "lo"], hi = 8 * si[, "hi"] + 2 * fo[, "hi"])
  sdiff <- cbind(lo = 8 * si[, "lo"] - 2 * fo[, "hi"], hi = 8 * si[, "hi"] - 2 * fo[, "lo"])
  gibc <- iv("gibbsite_pct")
  q <- list(
    fe2o3 = fe2o3, gib = gib, gb = .min_add(gib, iv("boehmite_pct")),
    fegib = .min_add(fe2o3, gib),
    anhy = iv("anhydrite_pct"), gyps = gyps, cg = .min_add(carb, gyps),
    opal = iv("opaline_pct"), glauc = iv("glauconite_pct"), mgsil = iv("mg_silicates_pct"),
    sf = sf, sdiff = sdiff, glass = iv("volcanic_glass_pct"),
    mica = iv("mica_grains_pct"), resist = iv("resistant_minerals_pct"),
    kaol = iv("kaolinite_pct"), hall = iv("halloysite_pct"), smec = iv("smectite_pct"),
    illi = iv("illite_pct"), verm = iv("vermiculite_pct"), allo = iv("allophane_pct"),
    okao = iv("kaolin_other_pct"), gibc = gibc)
  q$kgrp <- .min_add(q$kaol, q$hall, q$okao, q$gibc)
  q$hka <- .min_add(q$hall, q$kaol, q$allo)
  # the clay minerals not listed: none after an analysis, else what the
  # listed ones leave of the clay
  ck <- Reduce(`+`, lapply(q[unname(.MIN_CLAY_KINDS_USDA)], function(x) x[, "lo"]))
  q$unlisted <- cbind(lo = 0, hi = ifelse(an, 0, pmax(100 - ck, 0)))
  # isotic (p. 366): no free carbonates, NaF pH 8.4 or more, 1500 kPa water
  # / measured clay 0.6 or more, each layer
  naf <- num("ph_naf"); w15 <- num("water_content_1500kpa")
  ratio <- ifelse(!is.na(w15) & !is.na(clay) & clay > 0, w15 / clay, NA)
  iso <- vapply(seq_len(n), function(i)
    .ps_and3(!fc[i], if (is.na(naf[i])) NA else naf[i] >= 8.4,
             if (is.na(ratio[i])) NA else ratio[i] >= 0.6), NA)
  org <- tryCatch(.usda_organic_status(pedon), error = function(e) rep(NA, n))
  list(h = h, q = q, iso = iso, analysed = an, organic = org,
       clay_word = .ps_txt(h, "clay_mineralogy"),
       sand_word = .ps_txt(h, "sand_mineralogy"))
}


# ---- one unit: the control section or a named part ------------------------

# The weighted quantities of a unit [lo, hi] (cm): the F18 average of the
# recorded points when they cover half the unit, else the interval of all.
.min_unit_usda <- function(M, lo, hi) {
  h <- M$h
  w <- pmax(pmin(h$bottom_cm, hi) - pmax(h$top_cm, lo), 0)
  w[is.na(w)] <- 0
  # organic layers are not mineral soil material (p. 365)
  if (length(M$organic) == length(w)) w[M$organic %in% TRUE] <- 0
  inside <- w > 0
  if (!any(inside)) return(NULL)
  agg <- function(m) {
    pt <- m[, "lo"] == m[, "hi"]
    r <- .wavg_clip_usda(NULL, NULL, ifelse(pt, m[, "lo"], NA), 0, 0, w = w)$value
    if (!is.na(r)) return(c(r, r))
    c(sum(m[, "lo"] * w) / sum(w), sum(m[, "hi"] * w) / sum(w))
  }
  A <- lapply(M$q, agg)
  # isotic: TRUE in more than half the thickness
  t_yes <- sum(w[M$iso %in% TRUE]); t_na <- sum(w[is.na(M$iso)]); tot <- sum(w)
  iso <- if (t_yes > tot / 2) TRUE else if (t_yes + t_na <= tot / 2) FALSE else NA
  # the dominant clay mineral recorded as a word (clay_mineralogy), over half
  # the unit with one word
  cw <- M$clay_word[inside]; cwt <- w[inside]
  rec <- !is.na(cw) & nzchar(cw)
  word <- if (sum(cwt[rec]) >= tot / 2 && length(unique(cw[rec])) == 1L) cw[rec][1L] else NA_character_
  sw <- M$sand_word[inside]
  list(A = A, iso = iso, word = word, rows = which(inside), w = w,
       sand_word = if (length(sw) && all(!is.na(sw)) && length(unique(sw)) == 1L) sw[1L] else NA_character_,
       recorded = function(col) {
         x <- M$h[[col]]
         !is.null(x) && all(!is.na(x[inside]))
       },
       analysed = all(M$analysed[inside]))
}

# "More <kind> than any other single kind of clay mineral" (pp. 365-366).
.min_dominant <- function(U, kind) {
  key <- .MIN_CLAY_KINDS_USDA[[kind]]
  x <- U$A[[key]]
  others <- setdiff(unname(.MIN_CLAY_KINDS_USDA), key)
  oiv <- U$A[others]
  # the other 1:1 minerals and the unlisted ones are groups: their upper bound
  # caps every single kind in them, their lower bound says nothing
  grp <- list(U$A$okao, U$A$unlisted)
  hi_other <- max(vapply(c(oiv, grp), function(v) v[2], 0))
  lo_other <- max(vapply(oiv, function(v) v[1], 0))
  r <- if (x[1] > hi_other) TRUE else if (x[2] <= lo_other) FALSE else NA
  if (is.na(r) && !is.na(U$word) && U$word %in% names(.MIN_CLAY_KINDS_USDA))
    r <- identical(U$word, kind)
  r
}


# ---- the key (pp. 364-366) -----------------------------------------------

# The items of a section, in order: name, test and the quantities read.
.min_items_usda <- function(section, U, fragmental = FALSE) {
  A <- U$A
  it <- function(cls, test, q) list(class = cls, test = test, q = q)
  dom <- function(k) .min_dominant(U, k)
  clayq <- c("kaol", "hall", "smec", "illi", "verm", "allo", "okao", "gibc")
  switch(section,
    A = list(
      it("ferritic", .min_gt(A$fe2o3, 40), "fe2o3"),
      it("gibbsitic", .min_gt(A$gib, 40), "gib"),
      it("sesquic", .ps_and3(.min_in(A$fe2o3, 18, 40), .min_in(A$gib, 18, 40)), c("fe2o3", "gib")),
      it("ferruginous", .min_in(A$fe2o3, 18, 40), "fe2o3"),
      it("allitic", .min_in(A$gib, 18, 40), "gib"),
      it("kaolinitic", .ps_and3(.min_gt(A$kgrp, 50), .min_lt(A$smec, 10),
                                { r <- .min_more(A$kaol, A$hall)
                                  if (is.na(r) && !is.na(U$word) && U$word %in% c("kaolinite", "halloysite"))
                                    r <- U$word == "kaolinite"
                                  r }),
         c("kgrp", "smec", "kaol", "hall")),
      it("halloysitic", .ps_and3(.min_gt(A$hka, 50), .min_lt(A$smec, 10)), c("hka", "smec")),
      it("mixed", TRUE, character(0))),
    B = list(
      it("hypergypsic", .min_ge(A$gyps, 40), "gyps"),
      it("opaline", .min_ge(A$opal, 30), "opal"),
      it("amorphic", .ps_and3(.min_ge(A$sf, 5), .min_gt(A$sdiff, 0)), c("sf", "sdiff")),
      it("ferrihydritic", .min_ge(A$sf, 5), "sf"),
      it("glassy", .min_ge(A$glass, 30), "glass"),
      it("mixed", TRUE, character(0))),
    C = c(list(
      it("anhydritic", .min_ge(A$anhy, 15), "anhy"),
      it("gypsic", .min_ge(A$gyps, 15), "gyps"),
      it("carbonatic", .min_gt(A$cg, 40), "cg")),
      if (!fragmental) list(
        it("ferritic", .min_gt(A$fe2o3, 40), "fe2o3"),
        it("gibbsitic", .min_gt(A$gb, 40), "gb"),
        it("magnesic", .min_gt(A$mgsil, 40), "mgsil"),
        it("glauconitic", .min_gt(A$glauc, 20), "glauc"))),
    D = list(
      it("parasesquic", .min_gt(A$fegib, 10), "fegib"),
      it("halloysitic", .ps_and3(.min_gt(A$hka, 50), dom("halloysite")), c("hka", clayq)),
      it("kaolinitic", .ps_and3(.min_gt(A$kgrp, 50), .min_lt(A$smec, 10)), c("kgrp", "smec")),
      it("smectitic", dom("smectite"), clayq),
      it("illitic", .min_gt(A$illi, 50), "illi"),
      it("vermiculitic", dom("vermiculite"), clayq),
      it("isotic", U$iso, "isotic"),
      it("mixed", TRUE, character(0))),
    E = list(
      it("micaceous", .min_gt(A$mica, 45), "mica"),
      it("parasesquic", .min_gt(A$fegib, 10), "fegib"),
      it("isotic", U$iso, "isotic"),
      it("siliceous", .min_gt(A$resist, 90), "resist"),
      it("mixed", TRUE, character(0))))
}

# The columns a quantity needs that the unit has not recorded in every layer.
.min_missing_cols <- function(qs, U) {
  cols <- unique(unlist(lapply(qs, function(q)
    if (q == "isotic") c("caco3_pct", "effervescence_class", "ph_naf",
                         "water_content_1500kpa", "clay_pct")
    else .MIN_COLS_USDA[[q]])))
  if ("clay_mineralogy" %in% cols || any(qs %in% c("smec", "illi", "verm")))
    cols <- c(cols, "clay_mineralogy")
  cols <- cols[!vapply(cols, U$recorded, NA)]
  if (any(cols %in% .MIN_F2_COLS_USDA) && !U$analysed) cols <- c(cols, "mineralogy_analysed")
  unique(cols)
}

# Run one section: list(state = TRUE (a class) / FALSE (none, section C) /
# NA, class, missing, tested).
.min_run_section <- function(section, U, fragmental = FALSE) {
  items <- .min_items_usda(section, U, fragmental)
  miss <- character(0); tested <- list()
  for (x in items) {
    tested[[x$class]] <- x$test
    if (is.na(x$test)) { miss <- c(miss, .min_missing_cols(x$q, U)); next }
    if (isTRUE(x$test)) {
      if (length(miss)) return(list(state = NA, class = NA_character_, missing = unique(miss),
                                    tested = tested, blocked_class = x$class))
      return(list(state = TRUE, class = x$class, missing = character(0), tested = tested))
    }
  }
  list(state = if (length(miss)) NA else FALSE, class = NA_character_,
       missing = unique(miss), tested = tested)
}

# The sections a unit may be keyed in (p. 364): A by taxon; B when its class
# is a substitute other than fragmental; then C; then D (clayey classes) or
# E. `cls` is the particle-size class of the unit (NA when undecided);
# `sub_possible` and `clayey_possible` are TRUE / FALSE / NA when it is.
.min_unit_class <- function(U, section_a, cls, sub_possible, clayey_possible) {
  run_ce <- function(clayey) {
    frag <- identical(cls, "fragmental")
    rc <- .min_run_section("C", U, fragmental = frag)
    if (isTRUE(rc$state)) return(c(rc, list(section = "C")))
    nxt <- if (isTRUE(clayey)) "D" else "E"
    rn <- .min_run_section(nxt, U)
    if (is.na(rc$state))
      return(list(state = NA, class = NA_character_, section = paste0("C/", nxt),
                  missing = unique(c(rc$missing, rn$missing)),
                  tested = c(rc$tested, rn$tested)))
    c(rn, list(section = nxt))
  }
  if (isTRUE(section_a)) return(c(.min_run_section("A", U), list(section = "A")))
  paths <- list()
  if (isTRUE(sub_possible)) {
    return(c(.min_run_section("B", U), list(section = "B")))
  }
  if (is.na(sub_possible)) paths$B <- c(.min_run_section("B", U), list(section = "B"))
  if (is.na(clayey_possible)) {
    paths$D <- run_ce(TRUE); paths$E <- run_ce(FALSE)
  } else paths$x <- run_ce(clayey_possible)
  if (length(paths) == 1L) return(paths[[1L]])
  cl <- vapply(paths, function(p) if (isTRUE(p$state)) p$class else NA_character_, "")
  if (all(!is.na(cl)) && length(unique(cl)) == 1L)
    return(list(state = TRUE, class = cl[[1L]], missing = character(0),
                section = paste(vapply(paths, function(p) p$section, ""), collapse = "|"),
                tested = list()))
  list(state = NA, class = NA_character_,
       missing = unique(c("particle-size class", unlist(lapply(paths, function(p) p$missing)))),
       section = paste(vapply(paths, function(p) p$section, ""), collapse = "|"),
       tested = list())
}

# Stand-ins for an undecided class (F3, F4; opt-in): CaCO3, Kr, CEC / clay,
# Ki, and a quartz-dominated sand_mineralogy, read only in the sections
# that name the class they stand for.
.min_suggest_usda <- function(pedon, U, section) {
  h <- pedon$horizons
  w <- U$w
  av <- function(col) {
    x <- .ps_num(h, col)
    .wavg_clip_usda(NULL, NULL, x, 0, 0, w = w)$value
  }
  caco3 <- av("caco3_pct"); clay <- av("clay_pct"); cec <- av("cec_cmol")
  sio2 <- av("sio2_sulfuric_pct"); al2o3 <- av("al2o3_sulfuric_pct"); fe2o3 <- av("fe2o3_sulfuric_pct")
  kr <- compute_kr(sio2, al2o3, fe2o3); ki <- compute_ki(sio2, al2o3)
  act <- if (!is.na(cec) && !is.na(clay) && clay > 0) cec / clay else NA_real_
  quartz <- !is.na(U$sand_word) && grepl("quartz|siliceous", U$sand_word)
  secs <- strsplit(section %||% "", "[/|]")[[1]]
  has <- function(s) any(secs %in% s)
  ev <- list(caco3_pct = caco3, kr = kr, ki = ki, cec_to_clay = act,
             sand_mineralogy = U$sand_word)
  value <- if (!is.na(caco3) && caco3 > 40 && has("C")) "carbonatic"
           else if (!is.na(kr) && kr < 0.75 && has(c("D", "E"))) "parasesquic"
           else if (!is.na(act) && act >= 0.60 && !is.na(clay) && clay >= 18 && has("D")) "smectitic"
           else if (!is.na(ki) && ki <= 2.2 && has(c("A", "D"))) "kaolinitic"
           else if (quartz && has("E")) "siliceous"
           else if (!all(is.na(c(kr, ki, act, caco3))) && has(c("A", "B", "D", "E"))) "mixed"
           else NULL
  list(value = value, evidence = ev)
}


#' USDA family: mineralogy class (KST 13th ed., Ch. 17)
#'
#' The key to mineralogy classes of pp. 363-366 over the control section of
#' the particle-size classes (p. 364): section A for Oxisols and the kandi
#' and kanhap great groups of Alfisols and Ultisols; B for a substitute class
#' other than fragmental; C for any other mineral soil (no mixed class); then
#' D for the clayey classes and E for the rest. Each section but C ends with
#' mixed, reached only when every earlier class of the section is ruled out.
#' The criteria are weighted averages; isotic reads more than half the
#' thickness. A strongly contrasting particle-size class gives each named
#' part its mineralogy, written once when they agree. Not named for
#' Quartzipsamments (p. 364, footnote).
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param order_code,great_group,subgroup The taxa assigned above the family.
#' @param particle_size The particle-size \code{FamilyAttribute} (computed
#'   when NULL).
#' @param suggest Opt-in stand-ins (Kr, Ki, CEC / clay, CaCO3, a quartz
#'   sand_mineralogy) for an undecided class (readings F3, F4).
#' @return A \code{\link{FamilyAttribute}}.
#' @noRd
family_mineralogy_usda <- function(pedon, order_code = NULL, great_group = NULL,
                                   subgroup = NULL, particle_size = NULL,
                                   suggest = FALSE) {
  ctx <- .ps_context_usda(order_code, great_group, subgroup)
  fa <- function(value, ev, missing = character(0))
    FamilyAttribute$new(name = "mineralogy", value = value, evidence = ev,
                        missing = unique(missing), reference = .MIN_REF_USDA)
  if (identical(ctx$great_group, "quartzipsamments"))
    return(fa(NULL, list(reason = "not named in this taxon")))
  if (ctx$histosol || ctx$histel)
    return(fa(NULL, list(reason = paste(
      "the mineralogy classes of Histosols and Histels (KST 13th ed., p. 371)",
      "are not keyed in this version"))))
  h <- pedon$horizons
  if (is.null(h) || !nrow(h)) return(fa(NULL, list(reason = "no layers"), "horizons"))
  ps <- particle_size %||% family_particle_size_usda(pedon, order_code, great_group, subgroup)
  cs <- ps$evidence$control_section
  if (is.null(cs) || is.na(cs$top) || is.na(cs$bottom))
    return(fa(NULL, list(reason = "control section undecided", particle_size = ps$value),
              ps$missing))
  if (length(ps$evidence$contrast_candidates))
    return(fa(NULL, list(reason = "strongly contrasting classes undecided",
                         control_section = cs), ps$missing))
  section_a <- ctx$oxisol || (ctx$order %in% c("AF", "UT") &&
                                grepl("^(kandi|kanhap)", ctx$great_group))
  M <- .min_layers_usda(pedon)
  L <- tryCatch(.ps_layers_usda(pedon, ctx), error = function(e) NULL)
  # the class of a unit and what may be unknown about it
  class_info <- function(cls, lo, hi) {
    clayey_set <- c("clayey", "clayey-skeletal", "fine", "very-fine")
    if (!is.null(cls) && !is.na(cls))
      return(list(cls = cls, sub = cls %in% .PS_SUBSTITUTES_USDA,
                  clayey = cls %in% clayey_set))
    if (is.null(L)) return(list(cls = NA_character_, sub = NA, clayey = NA))
    w <- pmax(pmin(L$bottom, hi) - pmax(L$top, lo), 0); w[is.na(w)] <- 0
    ss <- L$sub_state[w > 0]
    sub <- if (any(ss %in% TRUE) || any(is.na(ss))) NA else FALSE
    m <- .ps_material(L[w > 0, , drop = FALSE], w[w > 0])
    clayey <- if (is.na(m$clay) || is.na(m$frag)) NA
              else if (m$frag > 90) FALSE
              else if (ctx$vertisol) m$clay > 30 else m$clay >= 35
    list(cls = NA_character_, sub = sub, clayey = clayey)
  }
  units <- list()
  parts <- ps$evidence$contrast_parts
  if (!is.null(parts)) {
    for (nm in c("upper", "lower")) {
      p <- parts[[nm]]
      units[[nm]] <- list(lo = max(p$top, cs$top), hi = min(p$bottom, cs$bottom),
                          info = class_info(p$class, max(p$top, cs$top), min(p$bottom, cs$bottom)))
    }
  } else {
    cls <- ps$value %||% ps$evidence$class_reached
    if (!is.null(cls) && grepl(",", cls)) cls <- NULL
    units$whole <- list(lo = cs$top, hi = cs$bottom, info = class_info(cls, cs$top, cs$bottom))
  }
  res <- lapply(units, function(u) {
    U <- .min_unit_usda(M, u$lo, u$hi)
    if (is.null(U)) return(list(state = NA, class = NA_character_, missing = "horizons",
                                section = NA_character_, U = NULL))
    r <- .min_unit_class(U, section_a, u$info$cls, u$info$sub, u$info$clayey)
    r$U <- U; r$lo <- u$lo; r$hi <- u$hi
    r
  })
  ev <- list(control_section = cs,
             sections = vapply(res, function(r) r$section %||% NA_character_, ""),
             tested = lapply(res, function(r) r$tested))
  if (length(res) > 1L)
    ev$parts <- lapply(res, function(r) list(top = r$lo, bottom = r$hi,
                                             class = if (isTRUE(r$state)) r$class else NA_character_))
  miss <- unique(unlist(lapply(res, function(r) r$missing)))
  ok <- vapply(res, function(r) isTRUE(r$state), NA)
  if (all(ok)) {
    cl <- vapply(res, function(r) r$class, "")
    # p. 364: one term when the parts agree ("mixed over mixed" is not used)
    value <- if (length(unique(cl)) == 1L) cl[[1L]] else paste(cl, collapse = " over ")
    return(fa(value, ev))
  }
  ev$reason <- "mineralogy class undecided: an earlier class of the section could not be ruled out"
  if (isTRUE(suggest)) {
    sg <- lapply(res, function(r) {
      if (isTRUE(r$state)) return(list(value = r$class))
      if (is.null(r$U)) return(list(value = NULL))
      .min_suggest_usda(pedon, r$U, r$section)
    })
    sv <- lapply(sg, function(s) s$value)
    if (all(!vapply(sv, is.null, NA))) {
      sv <- unlist(sv)
      ev$suggestion <- TRUE
      ev$suggestion_evidence <- lapply(sg, function(s) s$evidence)
      value <- if (length(unique(sv)) == 1L) sv[[1L]] else paste(sv, collapse = " over ")
      return(fa(value, ev, miss))
    }
  }
  fa(NULL, ev, miss)
}


# ---- human-altered and human-transported material classes (pp. 362-363) ----

# Layers of human-altered material (KST ch. 3): a layer origin or anthropic
# material naming altered material, deep ploughing, trenching or excavation
# in place, or re-oriented pieces of diagnostic horizons recorded. A field
# observation: TRUE / FALSE per layer.
.usda_ham_layers <- function(pedon) {
  h <- pedon$horizons
  txt <- paste(.ps_txt(h, "layer_origin"), .ps_txt(h, "anthropic_material"))
  ar <- .ps_num(h, "araric_fragments_pct")
  grepl("alter|deep.?plou?gh|deep.?plow|trench|rigol|excavat", txt) | (!is.na(ar) & ar > 0)
}

# Runs of adjacent layers that meet `flag` inside [lo, hi]: the thickness of
# the thickest run (cm).
.haht_run_thk <- function(h, flag, lo, hi) {
  best <- 0; cur <- 0; last_b <- NA_real_
  for (i in order(h$top_cm)) {
    t <- max(h$top_cm[i], lo); b <- min(h$bottom_cm[i], hi)
    if (is.na(t) || is.na(b) || b <= t) next
    if (!isTRUE(flag[i])) { cur <- 0; last_b <- b; next }
    contiguous <- cur > 0 && !is.na(last_b) && abs(t - last_b) < 1e-6
    cur <- if (contiguous) cur + (b - t) else b - t
    last_b <- b
    best <- max(best, cur)
  }
  best
}

#' USDA family: human-altered and human-transported material class
#'
#' Used only in mineral soils where human-altered or human-transported
#' material extends from the soil surface to 50 cm or a root-limiting layer,
#' whichever is shallower, or in the Anthraltic, Anthraquic, Anthrodensic,
#' Anthropic, Anthroportic, Haploplaggic and Plaggic subgroups (KST 13th
#' ed., p. 362). Control section: the soil surface to 200 cm, the base of the
#' deepest human-altered or human-transported layer, or a lithic or
#' paralithic contact, whichever is shallowest (p. 363). Key A1 to A12 (pp.
#' 363): methanogenic, asphaltic, concretic, gypsifactic, combustic,
#' ashifactic, pyrocarbonic, artifactic, pauciartifactic, dredgic, spolic,
#' araric. All are field records: not recorded, absent (reading F19).
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param order_code,great_group,subgroup The taxa assigned above the family.
#' @return A \code{\link{FamilyAttribute}}.
#' @noRd
family_human_material_usda <- function(pedon, order_code = NULL, great_group = NULL,
                                       subgroup = NULL) {
  ctx <- .ps_context_usda(order_code, great_group, subgroup)
  fa <- function(value, ev)
    FamilyAttribute$new(name = "human_material", value = value, evidence = ev,
                        missing = character(0), reference = .HAHT_REF_USDA)
  h <- pedon$horizons
  if (ctx$histosol || ctx$histel)
    return(fa(NULL, list(reason = "not used in organic soils (p. 362)")))
  if (is.null(h) || !nrow(h)) return(fa(NULL, list(reason = "no layers")))
  haht <- (.usda_htm_layers(pedon) | .usda_ham_layers(pedon)) %in% TRUE
  surf <- min(h$top_cm, na.rm = TRUE)
  rl <- tryCatch(.root_limiting_usda(pedon)$top, error = function(e) Inf)
  depth <- min(surf + 50, rl)
  # use (p. 362): (1) the material from the surface to 50 cm or a
  # root-limiting layer; (2) the anthropogenic and plaggen subgroups
  run <- .usda_surface_run(h, haht)
  run_bot <- if (length(run)) max(h$bottom_cm[run], na.rm = TRUE) else surf
  use1 <- length(run) > 0L && min(h$top_cm[run]) <= surf + 1e-6 && run_bot >= depth - 1e-6
  use2 <- grepl("\\b(Anthraltic|Anthraquic|Anthrodensic|Anthropic|Anthroportic|Haploplaggic|Plaggic)\\b",
                ctx$subgroup)
  if (!use1 && !use2)
    return(fa(NULL, list(reason = "not used in this taxon (p. 362)")))
  # control section (p. 363)
  ct <- tryCatch(.kst_contact_top_cm(pedon, petroferric = FALSE), error = function(e) NA_real_)
  deepest <- if (any(haht)) max(h$bottom_cm[haht], na.rm = TRUE) else surf
  lo <- surf; hi <- min(surf + 200, deepest, if (is.na(ct)) Inf else ct)
  ev <- list(use = c(surface_material = use1, subgroup = use2),
             control_section = list(top = lo, bottom = hi))
  num <- function(c) { x <- .ps_num(h, c); x[is.na(x)] <- 0; x }
  inside <- !is.na(h$top_cm) & !is.na(h$bottom_cm) & h$bottom_cm > lo & h$top_cm < hi
  thick <- function(flag, t) .haht_run_thk(h, flag & inside, lo, hi) >= t
  meth <- as.logical(.usda_col(h, "methanogenic_evidence", NA)) %in% TRUE
  acp <- as.logical(.usda_col(h, "artefacts_cohesive_persistent", NA)) %in% TRUE
  art_cp <- ifelse(acp, num("artefacts_pct"), 0)
  txt <- paste(.ps_txt(h, "layer_origin"), .ps_txt(h, "anthropic_material"))
  strat <- .ps_txt(h, "stratification_pattern")
  fine_strat <- as.logical(.usda_col(h, "htm_finely_stratified", NA)) %in% TRUE |
    (grepl("dredg|irrigation sediment", txt) & !is.na(strat) &
       !strat %in% c("", "none", "absent", "lithologic_break"))
  htm <- .usda_htm_layers(pedon) %in% TRUE
  keys <- list(
    methanogenic = any(meth & inside),
    asphaltic = thick(num("artefacts_asphalt_pct") > 35, 7.5),
    concretic = thick(num("artefacts_concrete_pct") > 35, 7.5),
    gypsifactic = thick(num("artefacts_synthetic_gypsum_pct") > 40, 7.5),
    combustic = thick(num("artefacts_coal_combustion_pct") > 35, 7.5),
    ashifactic = thick(num("fly_ash_grains_pct") > 15, 7.5),
    pyrocarbonic = thick(num("pyrolysis_grains_pct") > 5, 7.5),
    artifactic = thick(art_cp >= 35, 50),
    pauciartifactic = thick(art_cp >= 15, 50),
    dredgic = thick(htm & fine_strat, 50),
    spolic = thick(htm, 50),
    araric = thick(num("araric_fragments_pct") >= 3, 7.5))
  hit <- names(keys)[vapply(keys, isTRUE, NA)]
  if (!length(hit))
    return(fa(NULL, c(ev, list(reason = "no human-altered or human-transported material class applies (p. 363, B)"))))
  fa(hit[1L], ev)
}
