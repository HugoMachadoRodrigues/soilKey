# =============================================================================
# v0.9.281 -- suborders of the Australian Soil Classification (3rd ed.) for
# the Organosols (the dominant peat), Podosols (saturation of the B, and
# sulfuric or hypersulfidic material), Hydrosols (tidal, inland water,
# playa and salinised settings, then mottling) and Calcarosols (gypsum,
# carbonate classes and the calcareous horizon). Three-valued; the readings
# are in inst/rules/asc3/readings.yaml.
# =============================================================================

# The settings of the Hydrosols, a salinised surface and a soil made of
# gypsum crystals are seen at the site, and the surveyor records them where
# they occur: one the record does not mention is read as absent (as the order
# key reads tidal flooding for the Organosols). A material that needs a test
# (sulfuric, hypersulfidic) is undecided when the test was not done.
.asc_unrecorded_site <- function() FALSE
.asc_untested_material <- function() NA

# ---- Organosols ------------------------------------------------------------------

# The kind of peat of each horizon: fibric, hemic or sapric. The ASC's own
# test is the squeeze test of its glossary ('Peat', a modified von Post
# test): H1-H3 fibric, H4-H6 hemic, H7-H10 sapric. Without it, the rubbed
# fibre content, as the ASC says its kinds are essentially those of Soil
# Taxonomy and the WRB: three quarters or more fibric (both), from one sixth
# to two fifths hemic (both), less than one sixth sapric; between two fifths
# and three quarters the two definitions part (Soil Taxonomy's fibric from
# two fifths with the pyrophosphate colour, the WRB's from two thirds), so
# it tells only that the peat is not sapric. Then a peat field texture (Field Handbook: IP fibric, HP
# hemic, AP sapric) and an Oi, Oe or Oa designation. NA when none tells.
.asc_peat_types <- function(x) {
  h <- x$h; n <- x$n
  vp <- suppressWarnings(as.numeric(h$von_post_index %||% rep(NA_real_, n)))
  rub <- suppressWarnings(as.numeric(h$fiber_content_rubbed_pct %||% rep(NA_real_, n)))
  tex <- toupper(gsub("[^A-Za-z]", "", as.character(h$field_texture %||% rep(NA_character_, n))))
  des <- as.character(h$designation %||% rep(NA_character_, n))
  by_tex <- c(IP = "fibric", HP = "hemic", AP = "sapric")
  by_des <- c(i = "fibric", e = "hemic", a = "sapric")
  out <- rep(NA_character_, n)
  for (i in seq_len(n)) {
    other <- if (!is.na(tex[i]) && tex[i] %in% names(by_tex)) by_tex[[tex[i]]]
             else if (!is.na(des[i]) && grepl("^[0-9]*O[iea]", des[i])) by_des[[sub("^[0-9]*O([iea]).*$", "\\1", des[i])]]
             else NA_character_
    out[i] <- if (!is.na(vp[i])) {
      if (vp[i] <= 3) "fibric" else if (vp[i] <= 6) "hemic" else "sapric"
    } else if (!is.na(rub[i]) && rub[i] >= 75) "fibric"
      else if (!is.na(rub[i]) && rub[i] < 100 / 6) "sapric"
      else if (!is.na(rub[i]) && rub[i] < 40) "hemic"
      # two fifths to three quarters: fibric or hemic, never sapric
      else if (!is.na(rub[i])) { if (other %in% c("fibric", "hemic")) other else NA_character_ }
      else other
  }
  out
}

#' Organosol suborders (Fibric, Hemic, Sapric)
#'
#' The kind of peat with the greatest thickness among the organic materials
#' of the profile; when two kinds tie for it (no kind dominant), the more
#' decomposed of them (the suborders' Comment). Organic horizons of unknown
#' kind, and horizons whose organic status is undecided, are thickness of
#' unknown kind: the suborder is decided only when they cannot change it.
#' @noRd
peat_suborder_asc <- function(pedon, peat = c("fibric", "hemic", "sapric")) {
  peat <- match.arg(peat)
  x <- .asc_horizons(pedon)
  om <- .asc_organic_material(x, pedon)
  kinds <- .asc_peat_types(x)
  th <- ifelse(is.na(x$thick), 0, pmax(0, x$thick))
  rank <- c(fibric = 1, hemic = 2, sapric = 3)
  amt <- vapply(names(rank), function(k) sum(th[om %in% TRUE & kinds %in% k]), 0)
  unk <- sum(th[(om %in% TRUE & is.na(kinds)) | is.na(om)]) +
         if (any(is.na(x$thick) & !(om %in% FALSE))) Inf else 0
  me <- amt[[peat]]; others <- amt[names(amt) != peat]
  # `peat` beats kind `o` when thicker, or as thick and more decomposed
  beats <- function(a, b, o) a > b || (a == b && rank[[peat]] > rank[[o]])
  must <- all(vapply(names(others), function(o) beats(me, others[[o]] + unk, o), NA))
  can <- all(vapply(names(others), function(o) beats(me + unk, others[[o]], o), NA))
  passed <- if (!any(om %in% TRUE) && unk == 0) FALSE else if (must) TRUE else if (!can) FALSE else NA
  .asc_result(paste0("peat_", peat), passed, which(om %in% TRUE & kinds %in% peat),
              list(fibric_cm = amt[["fibric"]], hemic_cm = amt[["hemic"]], sapric_cm = amt[["sapric"]],
                   unknown_cm = unk),
              if (is.na(passed)) c("von_post_index", "fiber_content_rubbed_pct", "field_texture", "oc_pct")
              else character(0),
              "Organosols, suborders and Comment; glossary 'Peat'")
}

# ---- Podosols --------------------------------------------------------------------

# Saturation of the B horizon of a Podosol, as the set of the classes still
# possible: "none" (free drainage), "short" (spells of several days to
# several weeks) or "long" (of the order of months). From the days of
# saturation of the B horizons when recorded (90 or more long; 60 to 89
# short or long; 3 to 59 short; 1 or 2 none or short; 0 none; when some B
# horizons lack them, the classes from those recorded up, narrowed by the
# drainage class); otherwise
# from the site drainage class (Field Handbook, NCST 2009): the suborders
# equate free drainage with rapidly drained, so rapidly drained (and the
# excessive classes) is none; well drained (water removed readily, some
# horizons wet for several days) none or short; moderately well and
# imperfectly drained (water removed somewhat slowly or slowly, wet for up to
# a week or for several weeks) short, as their drainage is restricted and
# their wetness is short of months; poorly drained (all horizons wet for
# several months) short or long; very poorly drained long.
.asc_b_saturation <- function(pedon) {
  x <- .asc_horizons(pedon); h <- x$h
  all3 <- c("none", "short", "long")
  by_days <- function(m) if (m >= 90) "long" else if (m >= 60) c("short", "long") else if (m >= 3) "short"
                         else if (m > 0) c("none", "short") else "none"
  by_drain <- {
    d <- pedon$site$drainage_class %||% NA
    if (length(d) != 1L || is.na(d)) all3
    else if (grepl("^\\s*rapid", tolower(as.character(d)))) "none"
    else {
      wet_end <- .drainage_class(d, "wetter"); dry_end <- .drainage_class(d, "drier")
      if (is.na(wet_end) || is.na(dry_end)) all3 else {
        map <- list(`1` = "none", `2` = "none", `3` = "none", `4` = c("none", "short"), `5` = "short",
                    `6` = "short", `7` = c("short", "long"), `8` = "long")
        unique(unlist(map[as.character(seq(dry_end, wet_end))]))
      }
    }
  }
  days <- h$water_saturation_days %||% rep(NA_real_, x$n)
  b <- !is.na(x$role) & grepl("^B", x$role) & !x$des$buried
  dd <- days[b & !is.na(days)]
  if (!length(dd)) return(by_drain)
  m <- by_days(max(dd))
  if (!anyNA(days[b])) return(m)
  # a B horizon without its days may be wetter: the classes from the
  # recorded ones up, narrowed by the drainage class when they agree
  up <- all3[seq(match(m[1L], all3), 3L)]
  both <- intersect(up, by_drain)
  if (length(both)) both else up
}

#' Podosol suborders by the saturation of the B horizon
#' @noRd
b_saturation_asc <- function(pedon, saturation = c("none", "short", "long")) {
  saturation <- match.arg(saturation)
  poss <- .asc_b_saturation(pedon)
  passed <- if (identical(poss, saturation)) TRUE else if (!saturation %in% poss) FALSE else NA
  .asc_result(paste0("b_saturation_", saturation), passed, integer(0), list(possible = poss),
              if (is.na(passed)) c("water_saturation_days", "site$drainage_class") else character(0),
              "Podosols, suborders (saturation of the B horizon)")
}

# Hypersulfidic material in a horizon (glossary): a field pH of 4 or more
# that falls below 4, by 0.5 unit or more, when incubated (incubation_ph).
# Sulfidic S below 0.01 % (not sulfidic material), a field pH below 4, or an
# incubated pH of 4 or more fail; no incubation is untested, and an
# incubated pH below 4 with no field pH undecided.
.asc_hypersulfidic_rows <- function(x) {
  h <- x$h
  inc <- suppressWarnings(as.numeric(h$incubation_ph %||% rep(NA_real_, x$n)))
  s <- suppressWarnings(as.numeric(h$sulfidic_s_pct %||% rep(NA_real_, x$n)))
  ph <- h$ph_h2o %||% rep(NA_real_, x$n)
  ifelse(!is.na(s) & s < 0.01, FALSE,
         ifelse(!is.na(ph) & ph < 4, FALSE,
                ifelse(is.na(inc), .asc_untested_material(),
                       ifelse(inc >= 4, FALSE,
                              ifelse(is.na(ph), NA, round(ph - inc, 6) >= 0.5)))))
}

# Sulfuric material in a horizon (glossary): a pH below 4 (1:1 in water)
# caused by the oxidation of sulfides, shown by jarosite (or another iron or
# aluminium sulfate or hydroxysulfate mineral), by 0.05 % or more water-
# soluble sulfate (not read: soilKey has no such attribute) or by
# hypersulfidic material below. A pH of 4 or more fails; a pH below 4 with
# jarosite, or over hypersulfidic material, passes; a pH below 4 with
# neither is untested (organic acids also bring the pH of a Podosol A below
# 4); no pH is undecided.
.asc_sulfuric_rows <- function(x) {
  h <- x$h; ph <- h$ph_h2o %||% rep(NA_real_, x$n)
  jar <- as.logical(h$jarosite_present %||% rep(NA, x$n))
  hyp <- .asc_hypersulfidic_rows(x)
  top <- h$top_cm
  below <- vapply(seq_len(x$n), function(i) {
    r <- which(!is.na(top) & !is.na(top[i]) & top > top[i])
    length(r) > 0L && any(hyp[r] %in% TRUE)
  }, NA)
  ifelse(is.na(ph), NA, ifelse(ph >= 4, FALSE, ifelse(jar %in% TRUE | below, TRUE, .asc_untested_material())))
}

#' Sulfuric material (at least 15 cm thick) or hypersulfidic material in the
#' A or B horizons (Aquic-Sulfuric and Aquic-Hypersulfidic Podosols)
#' @noRd
acid_sulfate_ab_asc <- function(pedon, kind = c("sulfuric", "hypersulfidic")) {
  kind <- match.arg(kind)
  x <- .asc_horizons(pedon)
  ab <- !x$des$buried & x$mineral & (grepl("^[AB]", x$role) | x$unknown_role | x$trailing)
  v <- if (kind == "sulfuric") .asc_sulfuric_rows(x) else .asc_hypersulfidic_rows(x)
  v[!ab] <- FALSE
  # a row of unknown role may or may not be an A or B horizon
  v[ab & (x$unknown_role | x$trailing) & v %in% TRUE] <- NA
  passed <- if (!any(ab)) FALSE else if (kind == "sulfuric") {
    .cssc_run_test(x, v, 15)$passed
  } else {
    if (any(v %in% TRUE)) TRUE else if (all(v %in% FALSE)) FALSE else NA
  }
  .asc_result(paste0(kind, "_ab"), passed, which(v %in% TRUE), list(kind = kind),
              if (is.na(passed)) c("ph_h2o", "jarosite_present", "incubation_ph", "sulfidic_s_pct") else character(0),
              sprintf("Podosols, suborders; glossary '%s'",
                      if (kind == "sulfuric") "Sulfuric materials" else "Hypersulfidic material"))
}

# ---- Hydrosols -------------------------------------------------------------------

.asc_site_text <- function(...) {
  v <- unlist(list(...), use.names = FALSE)
  v <- tolower(trimws(as.character(v[!is.na(v)])))
  paste(v[nzchar(v)], collapse = " ; ")
}

#' The setting of a Hydrosol (the first seven suborders)
#'
#' From the site record: \code{site$tidal_zone} ("subtidal", "intertidal",
#' "supratidal" or "extratidal"), \code{site$tidal_inundation},
#' \code{site$subaqueous} (an inland water body over the soil),
#' \code{site$salinised}, the landform (\code{site$landform},
#' \code{site$landform_element}) and the surface condition. A landform that
#' names the setting decides it; a coastal landform (tidal flat, marine,
#' chenier or beach ridge plain, delta, estuary, mangrove) without the zone,
#' or tidal flooding without the zone, leaves the tidal settings undecided; a
#' lake or lagoon leaves Subaqueous undecided and a playa plain or a
#' lacustrine plain leaves Hypersalic undecided (the profile may lie on the
#' playa or beside it); any other recorded landform rules them out. A saline
#' surface (salt crust) is Salic. A setting the record does not mention is
#' read as absent.
#' @noRd
hydrosol_setting_asc <- function(pedon, setting = c("subtidal", "subaqueous", "intertidal", "supratidal",
                                                     "extratidal", "hypersalic", "salic")) {
  setting <- match.arg(setting)
  s <- pedon$site
  zone <- .asc_site_text(s$tidal_zone)
  # each landform field on its own (a playa element on a plain pattern is a
  # playa)
  lfs <- c(.asc_site_text(s$landform), .asc_site_text(s$landform_element))
  lfs <- lfs[nzchar(lfs)]
  lf <- paste(lfs, collapse = " ; ")
  surf <- .asc_site_text(s$surface_condition)
  flood <- as.logical(s$tidal_inundation %||% NA)[1L]
  unrec <- .asc_unrecorded_site()
  # TRUE when a field names the setting, NA when one may hold it, FALSE when
  # landforms are recorded and none does, unrec when none is
  from_landform <- function(named, maybe) {
    if (any(vapply(lfs, function(t) named(t), NA))) TRUE
    else if (any(vapply(lfs, function(t) maybe(t), NA))) NA
    else if (length(lfs)) FALSE else unrec
  }
  tidal4 <- c("subtidal", "intertidal", "supratidal", "extratidal")
  coastal <- "tidal|mangrove|estuar|marine plain|chenier|delta|beach|\\breef|lagoon|salt ?marsh|coastal"
  passed <- if (setting %in% tidal4) {
    txt <- paste(zone, lf)
    named <- regmatches(txt, regexpr("subtidal|intertidal|supratidal|extratidal", txt))
    if (length(named)) identical(named, setting)
    else if (isTRUE(flood) || grepl(coastal, lf)) NA
    else if (nzchar(lf) || identical(flood, FALSE)) FALSE
    else unrec
  } else if (setting == "subaqueous") {
    sub <- as.logical(s$subaqueous %||% NA)[1L]
    if (!is.na(sub)) sub
    else from_landform(function(t) grepl("subaqueous|submerged", t),
                       function(t) grepl("\\blake|lagoon|\\bpond|reservoir|billabong", t))
  } else if (setting == "hypersalic") {
    playa <- "playa|salina|salt ?lake|salt ?pan"
    from_landform(function(t) grepl(playa, t) && !grepl("plain", t),
                  function(t) grepl(playa, t) || grepl("lacustrine", t))
  } else {
    sal <- as.logical(s$salinised %||% NA)[1L]
    saline_surf <- grepl("\\bsalin|\\bsalt", surf) && !grepl("non[- ]?salin|not salin|no salt", surf)
    if (!is.na(sal)) sal else if (saline_surf) TRUE else unrec
  }
  .asc_result(paste0("hydrosol_", setting), passed, integer(0),
              list(setting = setting, tidal_zone = zone, landform = lf, surface = surf),
              if (is.na(passed)) c("site$tidal_zone", "site$landform", "site$surface_condition", "site$salinised")
              else character(0),
              "Hydrosols, suborders (setting of the soil)")
}

# A mottled horizon (glossary): more than 10 % mottles of distinct or
# prominent contrast (biological or mechanical mixing and substrate
# inclusions are not mottles). 10 % or less, or only faint ones, is not;
# mottles recorded absent (0 %) are not.
.asc_mottled_rows <- function(h) {
  n <- nrow(h)
  pct <- h$redoximorphic_features_pct %||% rep(NA_real_, n)
  ctr <- tolower(as.character(h$mottle_contrast %||% rep(NA_character_, n)))
  ifelse(!is.na(pct) & pct <= 10, FALSE,
         ifelse(!is.na(ctr) & ctr == "faint", FALSE,
                ifelse(!is.na(pct) & pct > 10 & ctr %in% c("distinct", "prominent"), TRUE, NA)))
}

#' Redoxic (more than half of the profile mottled) and Oxyaquic (more than
#' half whole coloured, read as not mottled) Hydrosols
#' @noRd
mottled_profile_asc <- function(pedon, mottled = TRUE) {
  x <- .asc_horizons(pedon); h <- x$h
  rows <- which(!is.na(h$top_cm) & !is.na(h$bottom_cm) & !x$des$buried & !x$rock)
  zt <- if (length(rows)) min(h$top_cm[rows]) else NA
  zb <- if (length(rows)) max(h$bottom_cm[rows]) else NA
  m <- .asc_mottled_rows(h)
  ok <- if (isTRUE(mottled)) m else !m
  passed <- if (!length(rows)) NA else .asc_major_part(x, rows, ok, zt, zb)
  .asc_result(if (isTRUE(mottled)) "mottled_profile" else "whole_coloured_profile", passed,
              rows[m[rows] %in% TRUE], list(window_cm = c(zt, zb)),
              if (is.na(passed)) c("redoximorphic_features_pct", "mottle_contrast") else character(0),
              "Hydrosols, suborders Redoxic and Oxyaquic; glossary 'Mottled horizon'")
}

# ---- Calcarosols -----------------------------------------------------------------

# Soft and hard carbonate of each horizon (glossary 'Carbonate classes').
# Soft: the soft, finely divided carbonate, carbonate_soft_pct (visual), or
# the laboratory CaCO3 of the fine earth, the larger. Hard: calcrete
# fragments, nodules or concretions and coated gravel, carbonate_hard_pct.
.asc_carbonate <- function(h) {
  n <- nrow(h)
  soft_v <- h$carbonate_soft_pct %||% rep(NA_real_, n)
  lab <- h$caco3_pct %||% rep(NA_real_, n)
  soft <- ifelse(is.na(soft_v), lab, ifelse(is.na(lab), soft_v, pmax(soft_v, lab)))
  list(soft = soft, hard = h$carbonate_hard_pct %||% rep(NA_real_, n), soft_visual = soft_v)
}

# A calcareous horizon (glossary): usually a Bk, BCk, 2Bk or 2BCk, or one
# holding fragments of a cemented equivalent; the Field Handbook's k is
# usually given above 10 % carbonate segregations. Read as a B or BC
# horizon of the present soil (any lithologic prefix) marked k, or one with
# 10 % or more carbonate segregations recorded (a k left out); a B or BC
# horizon recorded without k and without them is not; a cemented one (km, a
# calcrete pan rather than a horizon holding its fragments) is undecided; a
# horizon of unknown role in the present soil may be one.
.asc_calcareous_horizons <- function(x) {
  h <- x$h; n <- x$n
  bc <- !is.na(x$role) & x$role %in% c("B1", "B2", "B3", "BC", "B/C") & !x$des$buried
  c <- .asc_carbonate(h)
  seg <- h$secondary_carbonates_pct %||% rep(NA_real_, n)
  z <- function(v) ifelse(is.na(v), 0, v)
  # visible carbonate: at least the larger of the recorded segregations and
  # the recorded soft and hard parts; known when the hard part (which holds
  # calcrete fragments, not segregations) and the segregations or the soft
  # part are recorded
  vis_lo <- pmax(z(seg), z(c$soft_visual) + z(c$hard))
  vis_known <- !is.na(c$hard) & (!is.na(seg) | !is.na(c$soft_visual))
  seg_ok <- vis_lo >= 10
  # rows of unknown role in the present soil, those below the description
  # included, may be one, unless they hold less than 2 % visible carbonate
  maybe <- x$maybe_soil
  ifelse(bc & x$des$k & x$des$m, NA,
         ifelse(bc & (x$des$k | seg_ok), TRUE,
                ifelse(bc, FALSE, ifelse(maybe & !(vis_known & vis_lo < 2), NA, FALSE))))
}

#' Calcarosol suborders
#'
#' \code{class}: "hypergypsic" (dominantly gypsum crystals: caso4_pct over
#' 50 % in the major part of the solum), "carbonatic" (calcareous throughout
#' the solum, and more than 40 % fine-earth carbonate in the major part of
#' its upper 0.1 m), "hypocalcic" (carbonate only as a slight to moderate
#' effervescence with no visible carbonate, and/or less than 2 % soft
#' carbonate, in every horizon of the solum, and less than 20 % hard
#' carbonate in each), and, for a calcareous horizon, "lithocalcic" (more than 50 %
#' hard carbonate), "supracalcic" (20 to 50 %), "hypercalcic" (more than 20 %
#' soft and less than 20 % hard) and "calcic" (any other).
#' @noRd
carbonate_suborder_asc <- function(pedon, class = c("hypergypsic", "carbonatic", "hypocalcic", "lithocalcic",
                                                    "supracalcic", "hypercalcic", "calcic", "hard_below_20")) {
  class <- match.arg(class)
  x <- .asc_horizons(pedon); h <- x$h
  c <- .asc_carbonate(h)
  # the mineral solum; rows of unknown role in the present soil may be part
  # of it, and one between two solum horizons is (as for the order)
  sol <- if (x$any_known) which(x$solum %in% TRUE) else which(x$mineral & !x$des$buried)
  sol <- sol[x$mineral[sol]]
  maybe <- if (x$any_known) which(x$maybe_soil) else integer(0)
  sw <- .asc_sandwiched(x, maybe, sol)
  sol <- sort(c(sol, sw)); maybe <- setdiff(maybe, sw)
  calc_h <- .asc_calcareous_horizons(x)
  # every horizon of the solum: FALSE when one of it fails, TRUE when all of
  # it and the rows that may belong to it pass
  every <- function(v) if (!length(sol)) NA else if (any(v[sol] %in% FALSE)) FALSE
                       else if (all(v[c(sol, maybe)] %in% TRUE)) TRUE else NA
  passed <- switch(class,
    hypergypsic = {
      g <- h$caso4_pct %||% rep(NA_real_, x$n)
      lab_c <- h$caco3_pct %||% rep(NA_real_, x$n)
      ok <- ifelse(!is.na(g), g > 50, ifelse(!is.na(lab_c) & lab_c >= 50, FALSE, .asc_unrecorded_site()))
      if (!length(sol)) NA else .asc_rows_major(x, sol, ok)
    },
    carbonatic = {
      thr <- every(.asc_calcareous(x))
      t0 <- if (length(sol)) suppressWarnings(min(h$top_cm[sol], na.rm = TRUE)) else NA
      top10 <- if (is.finite(t0)) sol[!is.na(h$top_cm[sol]) & h$top_cm[sol] < t0 + 10] else integer(0)
      eff <- tolower(as.character(h$effervescence_class %||% rep(NA_character_, x$n)))
      weak <- grepl("^(none|non|nil|slight|moderate)", eff)
      lab <- h$caco3_pct %||% rep(NA_real_, x$n)
      rich_row <- ifelse(!is.na(lab), lab > 40, ifelse(weak, FALSE, NA))
      rich <- if (!length(top10)) NA else .asc_major_part(x, top10, rich_row, t0, t0 + 10)
      .and3(thr, rich)
    },
    hypocalcic = {
      eff <- tolower(as.character(h$effervescence_class %||% rep(NA_character_, x$n)))
      eff_weak <- ifelse(is.na(eff) | !nzchar(eff), NA, grepl("^(none|non|nil|slight|moderate)", eff))
      seg <- h$secondary_carbonates_pct %||% rep(NA_real_, x$n)
      visible <- ifelse(!is.na(seg) & seg > 0, TRUE,
                        ifelse(!is.na(c$soft_visual) & c$soft_visual > 0, TRUE,
                               ifelse(!is.na(c$hard) & c$hard > 0, TRUE,
                                      ifelse(is.na(seg) & is.na(c$soft_visual) & is.na(c$hard), NA, FALSE))))
      weak_only <- every(.and3v(eff_weak, !visible))
      soft_low <- every(ifelse(is.na(c$soft), NA, c$soft < 2))
      hard_low <- every(ifelse(is.na(c$hard), NA, c$hard < 20))
      # the class is of soils whose carbonate is weak, not of soils with none
      # (the table gives soft carbonate above 0): some horizon of the solum
      # is calcareous (always so in a Calcarosol)
      calc <- .asc_calcareous(x)
      some <- if (!length(sol)) NA else if (any(calc[c(sol, maybe)] %in% TRUE)) TRUE
              else if (all(calc[sol] %in% FALSE) && !any(is.na(calc[maybe]) | calc[maybe] %in% TRUE)) FALSE else NA
      .and3(some, .any3(c(weak_only, soft_low)), hard_low)
    },
    {
      test <- switch(class,
        lithocalcic = ifelse(is.na(c$hard), NA, c$hard > 50),
        supracalcic = ifelse(is.na(c$hard), NA, c$hard >= 20 & c$hard <= 50),
        hypercalcic = ifelse(!is.na(c$hard) & c$hard >= 20, FALSE,
                             ifelse(!is.na(c$soft) & c$soft <= 20, FALSE,
                                    ifelse(is.na(c$soft) | is.na(c$hard), NA, TRUE))),
        calcic = rep(TRUE, x$n),
        # Calcenic Tenosols, Hypercalcic: less than 20 % hard carbonate
        hard_below_20 = ifelse(is.na(c$hard), NA, c$hard < 20))
      v <- .and3v(calc_h, test)
      if (any(v %in% TRUE)) TRUE else if (all(v %in% FALSE)) FALSE else NA
    })
  .asc_result(paste0("carbonate_", class), passed, which(calc_h %in% TRUE),
              list(soft_pct = c$soft, hard_pct = c$hard, calcareous_horizon = calc_h),
              if (is.na(passed)) c("carbonate_soft_pct", "carbonate_hard_pct", "caco3_pct", "caso4_pct",
                                   "effervescence_class", "designation")
              else character(0),
              "Calcarosols, suborders; glossary 'Carbonate classes', 'Calcareous horizon'")
}
