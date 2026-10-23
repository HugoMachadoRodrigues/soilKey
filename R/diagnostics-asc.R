# =============================================================================
# v0.9.277 -- diagnostics of the Australian Soil Classification (ASC, 3rd ed.,
# Isbell & NCST 2021, online version with errata) for the key to the 15
# orders. Three-valued: TRUE, FALSE or NA (undecided). The choices where the
# text is silent are listed in inst/rules/asc3/readings.yaml.
#
# Texture: the ASC writes some limits in field texture grades (sand, loamy
# sand and clayey sand for the deep sands; a clay field texture for the
# Vertosols) and others in % clay with field-texture equivalents (the clear
# or abrupt textural B, the 15 % clay of the Kandosols and Arenosols).
# field_texture is read first where the limit is a field texture grade; lab
# clay then stands in, undecided within 2 points of the limit. Where the
# limit is in % clay, lab clay decides, and a field texture stands in as the
# range of clay the glossary gives that grade.
# =============================================================================

.ASC_REF <- "Isbell, R.F. and NCST (2021). The Australian Soil Classification, 3rd ed. (online version, 25 June 2026)."

.asc_result <- function(name, passed, layers = integer(0), evidence = list(),
                        missing = character(0), where = "", notes = NA_character_) {
  DiagnosticResult$new(name = name, passed = passed, layers = as.integer(layers),
                       evidence = evidence, missing = unique(missing),
                       reference = sprintf("%s %s", .ASC_REF, where), notes = notes)
}

# ---- field texture ----------------------------------------------------------

# Field texture grade (NCST Field Handbook codes, as in NatSoil: "SL", "FSL",
# "LMC", "KSLC", "WCS", with "+" or "-" for heavy or light) to its main grade
# and the clay range the ASC glossary gives it (field texture table). Peats,
# gravel and stones have no clay range.
.asc_texture <- function(code) {
  code <- toupper(gsub("[[:space:]]+", "", as.character(code)))
  qual <- ifelse(grepl("\\+$", code), "+", ifelse(grepl("-$", code), "-", ""))
  base <- sub("[+-]$", "", code)
  grade <- rep(NA_character_, length(code))
  peat <- grepl("P$", base) & !grepl("^(MHC|HC|LC|LMC|MC)$", base)
  grade[peat] <- "peat"
  g <- function(re, val) { hit <- is.na(grade) & grepl(re, base); grade[hit] <<- val }
  g("^(S|SS|FS|MS|KS|FSS|MSS|KSS)$", "S")
  g("^L(F|M|K)?S$", "LS")
  g("^(VW|W)?C(F|M|K)?S$", "CS")
  g("^(F|M|K)?SL$", "SL")
  g("^L$", "L")
  g("^ZL$", "ZL")
  g("^(F|M|K)?SCL(F|M|K)?S?$", "SCL")
  g("^CL(F|M|K)?S$", "CLS")
  g("^CL$", "CL")
  g("^ZCL$", "ZCL")
  g("^((F|M|K)?S|Z)?LMC$", "LMC")
  g("^((F|M|K)?S|Z)?LC$", "LC")
  g("^((F|M|K)?S|Z)?MHC$", "MHC")
  g("^((F|M|K)?S|Z)?MC$", "MC")
  g("^((F|M|K)?S|Z)?HC$", "HC")
  g("^((F|M|K)?S|Z)?C$", "C")
  g("^(GR|ST)$", "coarse")
  lo <- c(S = 0, LS = 3, CS = 5, SL = 10, L = 20, ZL = 20, SCL = 20, CL = 30, CLS = 30,
          ZCL = 30, LC = 35, LMC = 40, MC = 45, MHC = 50, HC = 50, C = 35)
  hi <- c(S = 5, LS = 8, CS = 10, SL = 20, L = 30, ZL = 30, SCL = 30, CL = 35, CLS = 35,
          ZCL = 35, LC = 40, LMC = 45, MC = 55, MHC = 100, HC = 100, C = 100)
  clo <- unname(lo[grade]); chi <- unname(hi[grade])
  mid <- (clo + chi) / 2
  up <- qual == "+" & !is.na(clo) & chi < 100; dn <- qual == "-" & !is.na(clo) & chi < 100
  clo[up] <- mid[up]; chi[dn] <- mid[dn]
  list(grade = grade, qualifier = qual, clay_lo = clo, clay_hi = chi,
       sandy = ifelse(is.na(grade) | grade %in% c("peat", "coarse"), NA, grade %in% c("S", "LS", "CS")),
       clayey = ifelse(is.na(grade) | grade %in% c("peat", "coarse"), NA,
                       grade %in% c("LC", "LMC", "MC", "MHC", "HC", "C")))
}

# Clay range of each horizon: lab clay when measured, otherwise the range of
# its field texture grade, otherwise NA.
.asc_clay_range <- function(h) {
  tx <- .asc_texture(h$field_texture %||% rep(NA_character_, nrow(h)))
  clay <- h$clay_pct
  list(lo = ifelse(!is.na(clay), clay, tx$clay_lo), hi = ifelse(!is.na(clay), clay, tx$clay_hi),
       texture = tx)
}

# A field-texture class tested on each horizon (sandy: S, LS or CS): the
# field texture decides; without it lab clay stands in (S, LS and CS have up
# to 10 % clay), undecided within `band` points of that limit.
.asc_is_sandy <- function(h, band = 2) {
  tx <- .asc_texture(h$field_texture %||% rep(NA_character_, nrow(h)))
  clay <- h$clay_pct
  proxy <- ifelse(is.na(clay), NA, ifelse(clay <= 10 - band, TRUE, ifelse(clay > 10 + band, FALSE, NA)))
  ifelse(!is.na(tx$sandy), tx$sandy, proxy)
}

# Clay content exceeding 15 % (Kandosols, Arenosols), which the key equates
# with a heavy sandy loam (SL+) or heavier field texture: lab clay decides
# when measured; otherwise the field texture (S, LS, CS and SL or SL- do not
# exceed it; SL+ and the heavier grades do); otherwise NA.
.asc_over15 <- function(h) {
  tx <- .asc_texture(h$field_texture %||% rep(NA_character_, nrow(h)))
  light <- tx$grade %in% c("S", "LS", "CS") | (tx$grade %in% "SL" & tx$qualifier != "+")
  heavy <- !is.na(tx$grade) & !tx$grade %in% c("S", "LS", "CS", "SL", "peat", "coarse") |
           (tx$grade %in% "SL" & tx$qualifier == "+")
  by_tx <- ifelse(heavy, TRUE, ifelse(light, FALSE, NA))
  ifelse(!is.na(h$clay_pct), h$clay_pct > 15, by_tx)
}

# ---- the profile as the ASC reads it -----------------------------------------

.asc_horizons <- function(pedon) {
  h <- pedon$horizons
  n <- nrow(h)
  des <- .asc_designations(h$designation %||% rep(NA_character_, n), pedon$site)
  role <- des$role
  top <- h$top_cm
  # When the b of a row is burial:
  # - the older codes cb and sb with no lithologic prefix are the bleaching
  #   of the row, conspicuous or sporadic (read as e and j), not burial and
  #   not concretions or sesquioxides (with a prefix, 2A2cb, they are burial);
  # - a b needs a cover: a row marked b with no row above it that is not
  #   marked b is not buried;
  # - a b row in the same material as the row above it, with unmarked A or
  #   B horizons of that material below it, is not buried either (a b used
  #   as a subdivision letter, A2a A2b, or a misprint): a buried soil has no
  #   present soil below it.
  raw_b <- des$buried
  sfx_str <- vapply(des$suffixes, paste, "", collapse = "")
  ord <- order(top, na.last = NA)
  ab_live <- des$known & grepl("^[AB]", role) & !raw_b & !is.na(top)
  for (i in which(raw_b & !is.na(top))) {
    above <- ord[top[ord] < top[i]]
    if (grepl("[cs]b$", sfx_str[i]) && !nzchar(des$prefix[i])) {
      des$buried[i] <- FALSE
      if (grepl("cb$", sfx_str[i])) des$e[i] <- TRUE
      if (grepl("sb$", sfx_str[i])) { des$s[i] <- FALSE; des$j[i] <- TRUE }
    } else if (!any(!raw_b[above])) {
      des$buried[i] <- FALSE
    } else if (length(above) && identical(des$prefix[above[length(above)]], des$prefix[i]) &&
               any(ab_live & des$prefix == des$prefix[i] & top > top[i])) {
      des$buried[i] <- FALSE
    }
  }
  # a horizon right below a buried one, in the same material (the same
  # lithologic prefix), belongs to the buried soil even without its b
  o <- order(top, na.last = NA)
  for (k in seq_along(o)[-1L]) {
    i <- o[k]; j <- o[k - 1L]
    if (!des$buried[i] && des$buried[j] && nzchar(des$prefix[j]) && identical(des$prefix[i], des$prefix[j]))
      des$buried[i] <- TRUE
  }
  # rows with no designation below the last designated row lie outside the
  # description (often rows sampled only): their properties are read, their
  # role is not; a row with no designation, or one that cannot be read,
  # among designated rows may be any horizon
  last_des <- suppressWarnings(max(top[!is.na(role)], na.rm = TRUE))
  trailing <- is.na(role) & !is.na(top) & is.finite(last_des) & top > last_des
  unknown_role <- !des$known & !trailing
  oc <- h$oc_pct
  tx <- .asc_texture(h$field_texture %||% rep(NA_character_, n))
  organic_by_role <- role %in% c("O1", "O2", "P")
  organic <- ifelse(des$known, organic_by_role,
                    ifelse(!is.na(oc), oc >= 12, tx$grade %in% "peat"))
  rock <- role %in% c("R")
  mineral <- !organic & !rock
  cem <- tolower(as.character(h$cementation_class %||% rep(NA_character_, n)))
  hard <- rock | cem %in% c("moderately", "strongly", "indurated") | des$m
  thick <- h$bottom_cm - h$top_cm
  b_role <- !is.na(role) & grepl("^B", role) & !grepl("^B/", role) | role %in% c("A/B")
  # the solum: P, O2, A and B horizons and their transitions (AB, BA, A/B,
  # B/A, BC, B/C); O1 is left out (glossary 'Solum')
  solum <- ifelse(unknown_role, NA,
                  role %in% c("P", "O2", "A1", "A2", "A3", "AB", "BA", "A/B", "B/A",
                              "B1", "B2", "B3", "BC", "B/C") & !des$buried)
  b2 <- ifelse(unknown_role, NA, role %in% "B2" & !des$buried & !des$lamellae)
  # where a row of unknown role may be a horizon of the present soil: from
  # the top of its first A horizon (anywhere when it has none) to its first
  # C, D or R and above any buried soil. maybe_soil (solum, B for the
  # Rudosols, the textural B) includes rows below the last designated one;
  # maybe_desc (Podosol B) does not; maybe_b2 also lies below the A horizons.
  bur_top <- suppressWarnings(min(top[des$buried], na.rm = TRUE))
  in_soil <- !is.na(top) & !des$buried & top < bur_top
  ab_top <- suppressWarnings(min(top[in_soil & des$known & grepl("^[AB]", role)], na.rm = TRUE))
  # the first C, D or R below the first A or B (a C over the A is a cover)
  c_top <- suppressWarnings(min(top[in_soil & role %in% c("C", "D", "R") &
                                    (!is.finite(ab_top) | top > ab_top)], na.rm = TRUE))
  # the window starts at the A1 (rows above it are surface litter or old A0
  # codes); with no A1 first, at the surface
  a_rows <- which(in_soil & role %in% c("A1", "A2", "A3"))
  first_a <- if (length(a_rows)) a_rows[which.min(top[a_rows])] else NA
  a_top <- if (!is.na(first_a) && role[first_a] == "A1") top[first_a] else -Inf
  # the A horizons above the first B, in the material of the first A (a
  # second sequence, 2A1, does not count)
  b_top <- suppressWarnings(min(top[in_soil & des$known & grepl("^B", role)], na.rm = TRUE))
  first_pre <- if (!is.na(first_a)) des$prefix[first_a] else ""
  a_bot <- suppressWarnings(max(h$bottom_cm[in_soil & role %in% c("O1", "O2", "P", "A1", "A2", "A3") &
                                              top < b_top & des$prefix == first_pre], na.rm = TRUE))
  maybe_soil <- in_soil & !des$known & mineral & top >= a_top - 0.5 & top < c_top
  # a Podosol B lies below an A horizon (stronger colours than the A above)
  maybe_desc <- maybe_soil & !trailing & (is.na(first_a) | top >= top[first_a] - 0.5)
  maybe_b2 <- maybe_desc & top >= (if (is.finite(a_bot)) a_bot - 0.5 else -Inf)
  list(h = h, n = n, des = des, role = role, unknown_role = unknown_role, trailing = trailing,
       maybe_soil = maybe_soil, maybe_desc = maybe_desc, maybe_b2 = maybe_b2, c_top = c_top,
       organic = organic, rock = rock, hard = hard,
       mineral = mineral, thick = thick, solum = solum, b2 = b2, b_role = b_role,
       known = des$known, any_known = any(des$known),
       clay = .asc_clay_range(h))
}

# Thickness of a horizon inside a depth window.
.asc_overlap <- function(top, bottom, zt, zb) {
  pmax(0, pmin(bottom, zb) - pmax(top, zt))
}

# Three-valued "major part" (more than half of a thickness): `ok` per row
# (TRUE, FALSE, NA), weighed by the part of each row inside the window
# [zt, zb]; rows with unknown depths, and the part of the window no row
# covers, count as undecided.
.asc_major_part <- function(x, rows, ok, zt, zb) {
  if (!length(rows) || is.na(zt) || is.na(zb) || zb <= zt) return(NA)
  top <- x$h$top_cm[rows]; bot <- x$h$bottom_cm[rows]; o <- ok[rows]
  known <- !is.na(top) & !is.na(bot)
  top <- top[known]; bot <- bot[known]; o <- o[known]
  span <- zb - zt
  # the window cut at every row boundary: a segment is TRUE or FALSE when
  # the rows covering it all say so (overlapping rows that disagree, or no
  # row, leave it undecided), so that no depth is counted twice
  cuts <- sort(unique(c(zt, zb, top[top > zt & top < zb], bot[bot > zt & bot < zb])))
  t_true <- 0; t_false <- 0
  for (k in seq_len(length(cuts) - 1L)) {
    mid <- (cuts[k] + cuts[k + 1L]) / 2
    v <- o[top <= mid & bot > mid]
    if (!length(v) || anyNA(v)) next
    if (all(v)) t_true <- t_true + (cuts[k + 1L] - cuts[k])
    else if (!any(v)) t_false <- t_false + (cuts[k + 1L] - cuts[k])
  }
  if (t_true > span / 2) return(TRUE)
  if (t_false >= span / 2) return(FALSE)
  NA
}

# ---- chemistry ----------------------------------------------------------------

# Strongly acid (glossary): pHw (1:5 water) below 5.5, or, without pHw, pHCa
# below 4.6. soilKey's ph_h2o is read as the pHw.
.asc_strongly_acid <- function(h) {
  ifelse(!is.na(h$ph_h2o), h$ph_h2o < 5.5,
         ifelse(!is.na(h$ph_cacl2), h$ph_cacl2 < 4.6, NA))
}

# Exchangeable sodium percentage (glossary 'ESP'): Na over the CEC, the
# effective CEC where pHw is 5.5 or less (or the CEC is missing); without Na
# or a CEC, the ESP the laboratory reports (esp_pct). Where the
# CEC and Na are both very low (CEC <= 3 and Na <= 0.3 cmol/kg) the glossary
# advises against computing it; such a horizon is read as not sodic.
.asc_esp <- function(h) {
  acid <- !is.na(h$ph_h2o) & h$ph_h2o <= 5.5
  den <- ifelse(acid & !is.na(h$ecec_cmol), h$ecec_cmol,
                ifelse(!is.na(h$cec_cmol), h$cec_cmol, h$ecec_cmol))
  esp <- ifelse(!is.na(h$na_cmol) & !is.na(den) & den > 0, 100 * h$na_cmol / den, NA_real_)
  # an ESP the laboratory reports stands in where Na or the CEC is missing
  lab_esp <- h$esp_pct %||% rep(NA_real_, nrow(h))
  esp <- ifelse(is.na(esp), lab_esp, esp)
  low <- !is.na(den) & den <= 3 & !is.na(h$na_cmol) & h$na_cmol <= 0.3
  list(esp = esp, low_charge = low)
}

.asc_sodic <- function(h) {
  e <- .asc_esp(h)
  ifelse(e$low_charge, FALSE, ifelse(is.na(e$esp), NA, e$esp >= 6))
}

# Strongly subplastic (glossary): the field texture rises by more than two
# texture groups after 10 minutes of kneading. Recorded in `subplasticity`
# ("strongly subplastic", "subplastic", "none"); a horizon with no record is
# read as not strongly subplastic, as the observation is noted when made.
.asc_strongly_subplastic <- function(h) {
  s <- tolower(trimws(as.character(h$subplasticity %||% rep(NA_character_, nrow(h)))))
  grepl("strong", s)
}

# Calcareous (glossary): carbonate segregations, or a fine earth that
# effervesces with 1 M HCl. Read from effervescence_class, secondary
# carbonates, lab CaCO3 (0.5 % or more effervesces) and a k suffix. Without
# any carbonate record, a pHw below 7.0 (or a pHCa below 6.5) is read as not
# calcareous: free carbonate keeps the pH higher.
.asc_calcareous <- function(x) {
  h <- x$h
  eff <- tolower(trimws(as.character(h$effervescence_class %||% rep(NA_character_, x$n))))
  eff_yes <- grepl("slight|moderate|strong|high|very|calcareous", eff) & !grepl("^non|^none|^nil", eff)
  eff_no <- grepl("^non|^none|^nil|^n$", eff)
  seg <- h$secondary_carbonates_pct; caco3 <- h$caco3_pct
  yes <- eff_yes | (!is.na(seg) & seg > 0) | (!is.na(caco3) & caco3 >= 0.5) | x$des$k
  no_carb <- eff_no | (!is.na(caco3) & caco3 < 0.5 & (is.na(seg) | seg == 0))
  acid <- ifelse(!is.na(h$ph_h2o), h$ph_h2o < 7.0, !is.na(h$ph_cacl2) & h$ph_cacl2 < 6.5)
  no_record <- is.na(seg) & is.na(caco3) & (is.na(eff) | !nzchar(eff)) & !x$des$k
  no <- !yes & (no_carb | (no_record & acid %in% TRUE))
  ifelse(yes, TRUE, ifelse(no, FALSE, NA))
}

# ---- the clear or abrupt textural B horizon -----------------------------------

# The clay just below the boundary needed for a texture contrast (glossary
# 'Clear or abrupt textural B horizon'): above < 20 % clay, at least double
# and at least 20 %; 20 to < 35 %, at least 20 points more; 35 % or more,
# no contrast can be recognised. Non-decreasing in the clay above.
.asc_clay_needed <- function(a) {
  ifelse(is.na(a), NA_real_, ifelse(a < 20, pmax(2 * a, 20), ifelse(a < 35, a + 20, Inf)))
}

# The clay increase of the textural B between the horizon above (j) and the
# horizon below (i). Each horizon is read by its field texture grade when it
# has one, and by its lab clay only when it has none: the glossary gives its
# figures in clay with field-texture equivalents, and notes that where the
# two disagree the field texture may be the better guide to the soil's
# behaviour. With lab clay on both sides, the glossary's figures decide;
# otherwise its field-texture equivalents do: above sandy loam or lighter,
# the B must be sandy clay loam or heavier with about double the clay (the
# middle clay of each grade); above loam to clay loam, the B must be light
# medium clay or heavier; above a clay, no contrast. A horizon with neither
# a field texture nor lab clay leaves it undecided.
.asc_texture_contrast <- function(x, j, i) {
  clay <- x$h$clay_pct; tx <- x$clay$texture
  by_lab <- function(k) is.na(tx$grade[k]) || tx$grade[k] %in% c("peat", "coarse")
  lab_j <- by_lab(j); lab_i <- by_lab(i)
  if ((lab_j && is.na(clay[j])) || (lab_i && is.na(clay[i]))) return(NA)
  if (lab_j && lab_i) return(clay[i] >= .asc_clay_needed(clay[j]))
  ga <- tx$grade[j]; gb <- tx$grade[i]
  mid <- function(k, lab) if (lab) clay[k] else (tx$clay_lo[k] + tx$clay_hi[k]) / 2
  a <- mid(j, lab_j); b <- mid(i, lab_i)
  above_light <- if (lab_j) clay[j] < 20 else ga %in% c("S", "LS", "CS", "SL")
  above_medium <- if (lab_j) clay[j] >= 20 && clay[j] < 35
                  else ga %in% c("L", "ZL", "SCL", "CL", "CLS", "ZCL")
  if (above_light) {
    heavy_enough <- if (lab_i) clay[i] >= 20 else !gb %in% c("S", "LS", "CS", "SL", "L", "ZL")
    return(heavy_enough && b >= 2 * a)
  }
  if (above_medium) {
    if (lab_i) return(clay[i] - a >= 20)
    if (gb %in% "C") return(NA)
    return(gb %in% c("LMC", "MC", "MHC", "HC"))
  }
  FALSE
}

#' Clear or abrupt textural B horizon (ASC)
#'
#' A B horizon (normally the B2t) whose upper boundary is clear, abrupt or
#' sharp under a horizon more than 30 mm thick, with the clay increase of
#' the glossary across it. The boundary is the lower boundary recorded for
#' the horizon above (boundary_distinctness; a recorded "sharp" passes). Each
#' horizon is read by its field texture grade (the glossary's equivalents),
#' and by its lab clay only when it has no field texture
#' (\code{.asc_texture_contrast}).
#' @noRd
textural_b_asc <- function(pedon) {
  x <- .asc_horizons(pedon); h <- x$h
  ord <- order(h$top_cm, na.last = NA)
  bnd <- tolower(trimws(as.character(h$boundary_distinctness)))
  sharp <- ifelse(bnd %in% c("abrupt", "clear", "sharp", "a", "c", "s"), TRUE,
                  ifelse(bnd %in% c("gradual", "diffuse", "g", "d"), FALSE, NA))
  # buried horizons are not read (the soil above them is keyed)
  live <- which(!x$des$buried)
  ord <- ord[ord %in% live]
  lower_top <- suppressWarnings(min(h$top_cm[live][x$role[live] %in% c("B2", "B3", "BC", "B/C") |
                                                    (x$role[live] %in% c("C", "D", "R") & h$top_cm[live] >= x$c_top)],
                                    na.rm = TRUE))
  out <- list(); res <- logical(0)
  for (k in seq_along(ord)[-1L]) {
    i <- ord[k]; j <- ord[k - 1L]
    if (!x$mineral[i] || !x$mineral[j]) next
    # the horizon above is 'normally an A but occasionally a B1'; the one
    # below is a B (normally the B2t, but any B under the boundary counts:
    # the glossary sets the clay minimum 'at the top of the B horizon'),
    # clay lamellae excepted. Rows whose role is unknown may be either.
    unk_i <- !x$des$known[i]; unk_j <- !x$des$known[j]
    # a row of unknown role is a possible A or B1 above the boundary only
    # above the first B2, B3, BC, C, D or R, and a possible B below it only
    # where it may be part of the present soil
    if (unk_j && !(x$maybe_soil[j] && h$top_cm[j] < lower_top)) next
    if (unk_i && !x$maybe_soil[i]) next
    above_a <- x$role[j] %in% c("A1", "A2", "A3", "AB", "A/B", "AC")
    above_b1 <- x$role[j] %in% c("B1", "BA", "B/A")
    if (!unk_j && !above_a && !above_b1) next
    if (x$des$lamellae[i]) next
    below_set <- if (!unk_j && above_b1) c("B2", "B3", "BC", "B/C") else c("B1", "B2", "B3", "BA", "B/A", "BC", "B/C")
    if (!unk_i && !x$role[i] %in% below_set) next
    is_b <- if (unk_i || unk_j) NA else TRUE
    # a gap, an overlap or a missing depth between them leaves it open
    contiguous <- !is.na(h$top_cm[i]) && !is.na(h$bottom_cm[j]) && abs(h$top_cm[i] - h$bottom_cm[j]) <= 0.5
    if (!contiguous) {
      res <- c(res, NA); out[[length(out) + 1L]] <- list(row = i, above = j, passed = NA); next
    }
    thick_ok <- if (is.na(x$thick[j])) NA else x$thick[j] > 3
    clay_ok <- .asc_texture_contrast(x, j, i)
    r <- .and3(is_b %||% NA, thick_ok, sharp[j], clay_ok)
    res <- c(res, r); out[[length(out) + 1L]] <- list(row = i, above = j, passed = r)
  }
  # no B in the description: the textural B may lie below it when the
  # description stops in an A or a B1 (as for the B2)
  stops_short <- x$any_known && is.null(.asc_b2_rows(x)) &&
                 !any(x$role[live] %in% c("B2", "B3", "BC", "B/C"))
  passed <- if (any(res %in% TRUE)) TRUE else if (any(is.na(res)) || stops_short) NA else FALSE
  rows <- vapply(out, function(o) o$row, 0L)
  hit <- rows[vapply(out, function(o) isTRUE(o$passed), TRUE)]
  .asc_result("textural_b_asc", passed, if (length(hit)) hit[1L] else integer(0),
              list(candidates = out, notation = x$des$notation$why),
              if (is.na(passed)) c("clay_pct", "field_texture", "boundary_distinctness", "designation")
              else character(0),
              "Glossary, 'Clear or abrupt textural B horizon'")
}

# The upper B2t: the top 0.2 m of the B2t (the B2 horizons from the
# textural B down; a B1 or B3 is not the B2t; a gap in the record is a part
# of unknown value, not the end of the B2t), or the whole B2t when it is
# thinner. With no B2 at or below the textural B (a B3 over rock, say), the
# textural B horizon itself.
.asc_upper_b2t <- function(x, b_row, extra = integer(0)) {
  cand <- x$b2 %in% TRUE
  cand[extra] <- TRUE
  .asc_window(x, cand, x$h$top_cm[b_row], 20, fallback = b_row)
}

# An order gate on the upper B2t of the textural B: the textural B must be
# present, then `ok` (per row) over the major part of its upper 0.2 m.
.asc_textural_gate <- function(pedon, name, ok_fun, where, negate = character(0)) {
  tb <- textural_b_asc(pedon)
  x <- .asc_horizons(pedon)
  if (!isTRUE(tb$passed)) {
    return(.asc_result(name, tb$passed, integer(0), list(textural_b = tb$passed),
                       tb$missing, where))
  }
  z <- .asc_upper_b2t(x, tb$layers[1L])
  ok <- ok_fun(x)
  # rows of unknown role below the textural B may be part of the B2t: the
  # answer must hold whichever of them are (up to 8 such rows are tried)
  b_top <- x$h$top_cm[tb$layers[1L]]
  maybe <- .asc_b2_maybe(x)
  maybe <- maybe[!is.na(x$h$top_cm[maybe]) & x$h$top_cm[maybe] >= b_top - 0.5]
  # a row starting 0.2 m or more below the first known B2 under the
  # textural B can be in no window
  t0 <- suppressWarnings(min(x$h$top_cm[x$b2 %in% TRUE & x$h$top_cm >= b_top - 0.5], na.rm = TRUE))
  if (is.finite(t0)) maybe <- maybe[x$h$top_cm[maybe] < t0 + 20]
  subsets <- if (length(maybe) <= 8L) {
    lapply(seq_len(2^length(maybe)) - 1L, function(k) maybe[bitwAnd(k, 2^(seq_along(maybe) - 1L)) > 0])
  } else NULL
  mp <- lapply(ok, function(v) {
    if (is.null(subsets)) return(NA)
    ans <- vapply(subsets, function(sub) {
      zz <- .asc_upper_b2t(x, tb$layers[1L], extra = sub)
      .asc_major_part(x, zz$rows, v, zz$top, zz$bottom)
    }, NA)
    if (all(ans %in% TRUE)) TRUE else if (all(ans %in% FALSE)) FALSE else NA
  })
  passed <- do.call(.and3, lapply(names(mp), function(k) if (k %in% negate) !mp[[k]] else mp[[k]]))
  .asc_result(name, passed, z$rows, c(list(textural_b = TRUE, zone_cm = c(z$top, z$bottom)), mp),
              if (is.na(passed)) c("ph_h2o", "ph_cacl2", "na_cmol", "cec_cmol", "ecec_cmol")
              else character(0), where)
}

#' Kurosols, Sodosols and Chromosols (ASC orders, key steps 6-8)
#' @noRd
kurosol_asc <- function(pedon) {
  .asc_textural_gate(pedon, "kurosol_asc",
                     function(x) list(strongly_acid = .asc_strongly_acid(x$h)),
                     "Key to orders, step 6; Kurosols")
}

#' @noRd
sodosol_asc <- function(pedon) {
  .asc_textural_gate(pedon, "sodosol_asc",
                     function(x) list(sodic_not_strongly_subplastic =
                                        .and3v(.asc_sodic(x$h), !.asc_strongly_subplastic(x$h))),
                     "Key to orders, step 7; Sodosols")
}

#' @noRd
chromosol_asc <- function(pedon) {
  tb <- textural_b_asc(pedon)
  .asc_result("chromosol_asc", tb$passed, tb$layers, list(textural_b = tb$passed),
              tb$missing, "Key to orders, step 8; Chromosols")
}

# ---- Anthroposols ---------------------------------------------------------------

#' Anthroposols (ASC order, key step 1)
#'
#' Human-made or profoundly altered soils, recognised from a record of human
#' origin: artefacts (artefacts_pct above 0) or an anthropic layer origin,
#' in material from the surface. Where it buries a pre-existing soil (a
#' buried horizon, or an A or B horizon, below it) it must be at least 0.3 m
#' thick; thinner material there is a phase of the soil below. Without such a
#' record the soil is not an Anthroposol.
#' @noRd
anthroposol_asc <- function(pedon) {
  x <- .asc_horizons(pedon); h <- x$h
  origin <- tolower(as.character(h$layer_origin %||% rep(NA_character_, x$n)))
  anthro <- (!is.na(h$artefacts_pct) & h$artefacts_pct > 0) |
            grepl(paste0("\\b(anthropic|anthropogenic|technic|artefacts?|artifacts?|spolic|garbic|urbic|",
                         "dredged|dredge spoil|hortic|landfill|artificial fill|fill material|made ground|spoil|tailings|mine waste)\\b"),
                  origin)
  ord <- order(h$top_cm, na.last = NA)
  # an O1 (litter) at the surface is outside the solum: the run starts below
  start <- 0
  for (i in ord) { if (x$role[i] %in% "O1" && !anthro[i] && !is.na(h$bottom_cm[i]) && abs(h$top_cm[i] - start) <= 0.5) start <- h$bottom_cm[i] else break }
  ord <- ord[!(x$role[ord] %in% "O1" & h$top_cm[ord] < start)]
  run <- 0; prev <- NA_real_; rows <- integer(0)
  for (i in ord) {
    if (!anthro[i]) break
    if (!is.na(prev) && abs(h$top_cm[i] - prev) > 0.5) break
    if (is.na(prev) && abs(h$top_cm[i] - start) > 0.5) break
    if (is.na(h$bottom_cm[i])) { run <- Inf; rows <- c(rows, i); break }
    run <- run + (h$bottom_cm[i] - h$top_cm[i]); prev <- h$bottom_cm[i]; rows <- c(rows, i)
  }
  below <- setdiff(ord, rows)
  soil_below <- any(x$des$buried[below]) ||
                any(x$role[below] %in% c("A1", "A2", "A3", "AB", "A/B", "BA", "B/A", "B1", "B2", "B3", "BC", "B/C"))
  # rows of unknown role below may be a buried soil
  buries <- if (soil_below) TRUE else if (any(!x$des$known[below])) NA else FALSE
  passed <- if (!length(rows)) FALSE else if (run >= 30) TRUE
            else if (isTRUE(buries)) FALSE else if (identical(buries, FALSE)) run > 0 else NA
  .asc_result("anthroposol_asc", passed, rows, list(anthropic_thickness_cm = run, buries_a_soil = buries),
              character(0), "Key to orders, step 1; Anthroposols")
}

# ---- Organosols -----------------------------------------------------------------

# Organic materials (glossary): organic C at least 12 % plus 0.1 x clay %
# (to 18 % at 60 % clay) where saturated for long periods or drained, or at
# least 20 % where saturated for a few days at most. When the saturation is
# not known, 20 % or more passes and less than the clay-scaled limit fails.
# An O or P designation counts where organic C is not measured.
.asc_organic_material <- function(x, pedon) {
  h <- x$h
  wet <- .asc_organic_saturation(pedon)
  clay <- pmin(ifelse(is.na(h$clay_pct), 60, h$clay_pct), 60)
  need_a <- 12 + 0.1 * clay
  need_a_lo <- 12 + 0.1 * pmin(ifelse(is.na(h$clay_pct), 0, h$clay_pct), 60)
  oc <- h$oc_pct
  by_oc <- if (isTRUE(wet)) ifelse(is.na(oc), NA, ifelse(oc >= need_a, TRUE, ifelse(oc < need_a_lo, FALSE, NA)))
           else if (identical(wet, FALSE)) ifelse(is.na(oc), NA, oc >= 20)
           else ifelse(is.na(oc), NA, ifelse(oc >= 20, TRUE, ifelse(oc < need_a_lo, FALSE, NA)))
  # measured organic C decides, or leaves the horizon undecided; the
  # designation speaks only where organic C is not measured
  tx <- x$clay$texture
  by_tx <- ifelse(is.na(tx$grade), NA, tx$grade == "peat")
  ifelse(!is.na(oc), by_oc,
         ifelse(x$des$known, x$role %in% c("O1", "O2", "P"),
                ifelse(!is.na(by_tx), by_tx, NA)))
}

# Saturation of 2-3 months or more (Hydrosols, Aquic Vertosols) from the
# site drainage class, whose Field Handbook definitions speak of wetness, not
# saturation: very poorly drained (free water at or near the surface for
# most of the year) saturates; poorly drained (all horizons wet for several
# months) is undecided, as wet is not yet saturated (and most poorly drained
# profiles of the Australian surveys are not Hydrosols); imperfectly drained
# (some horizons wet for several weeks, short of 2-3 months) or better,
# rapidly drained included, do not.
.asc_long_saturation <- function(pedon) {
  d <- pedon$site$drainage_class %||% NA
  if (length(d) == 1L && !is.na(d) && grepl("^\\s*rapid", tolower(as.character(d)))) return(FALSE)
  wet_end <- .drainage_class(d, "wetter"); dry_end <- .drainage_class(d, "drier")
  if (!is.na(dry_end) && dry_end >= 8) return(TRUE)
  if (!is.na(wet_end) && wet_end <= 6) return(FALSE)
  NA
}

# Saturation "for long periods" in the definition of organic materials,
# which the glossary says is that of Soil Taxonomy: 30 days or more,
# cumulative, in normal years. From the drainage class: very poorly drained
# saturates; poorly drained (wet for several months) and imperfectly drained
# (wet for several weeks) are undecided, as weeks or months of wetness may
# or may not be 30 days of saturation; moderately well drained (wet for up
# to about a week) or better, rapidly drained included, do not.
.asc_organic_saturation <- function(pedon) {
  d <- pedon$site$drainage_class %||% NA
  if (length(d) == 1L && !is.na(d) && grepl("^\\s*rapid", tolower(as.character(d)))) return(FALSE)
  wet_end <- .drainage_class(d, "wetter"); dry_end <- .drainage_class(d, "drier")
  if (!is.na(dry_end) && dry_end >= 8) return(TRUE)
  if (!is.na(wet_end) && wet_end <= 5) return(FALSE)
  NA
}

#' Organosols (ASC order, key step 2)
#'
#' Not regularly flooded by saline tides (site$tidal_inundation), and either
#' more than 0.4 m of organic materials within the upper 0.8 m, or organic
#' materials from the surface to at least 0.1 m resting directly on rock, a
#' hard layer or saprolite, or on mineral material no thicker than the
#' organic materials above it.
#' @noRd
organosol_asc <- function(pedon) {
  x <- .asc_horizons(pedon); h <- x$h
  om <- .asc_organic_material(x, pedon)
  tidal <- isTRUE(as.logical(pedon$site$tidal_inundation %||% NA))
  top <- h$top_cm; bot <- h$bottom_cm
  w <- ifelse(!is.na(top) & !is.na(bot), .asc_overlap(top, bot, 0, 80), NA)
  t_true <- sum(w[om %in% TRUE], na.rm = TRUE)
  # the part of the upper 0.8 m below the description is unknown too,
  # unless the description ends on rock or a hard layer
  unseen <- if (.asc_ends_hard(x)) 0 else max(0, 80 - sum(w, na.rm = TRUE))
  t_open <- sum(w[is.na(om)], na.rm = TRUE) + unseen +
            if (any(is.na(w) & !(om %in% FALSE))) Inf else 0
  thick <- if (t_true > 40) TRUE else if (t_true + t_open <= 40) FALSE else NA
  # branch 2: organic materials from the surface, at least 10 cm, on rock,
  # a hard layer, saprolite, or mineral material no thicker than them;
  # decided only when the layers of unknown organic status do not matter
  ord <- order(top, na.last = NA)
  hard <- x$hard
  branch2_with <- function(org) {
    run <- 0; prev <- 0; k <- 0L
    for (i in ord) {
      if (!org[i] || is.na(top[i]) || abs(top[i] - prev) > 0.5) break
      if (is.na(bot[i])) return(NA)
      run <- run + bot[i] - top[i]; prev <- bot[i]; k <- k + 1L
    }
    if (run < 10) return(FALSE)
    below <- if (k < length(ord)) ord[(k + 1L):length(ord)] else integer(0)
    if (!length(below)) return(NA)
    if (hard[below[1L]]) return(TRUE)
    # mineral layers down to rock or a hard layer, no thicker than the run
    m_thick <- 0; reached <- FALSE
    for (i in below) {
      if (hard[i]) { reached <- TRUE; break }
      m_thick <- m_thick + (bot[i] - top[i])
    }
    if (reached && !is.na(m_thick)) m_thick <= run
    else if (!is.na(m_thick) && m_thick > run) FALSE else NA
  }
  b_lo <- branch2_with(om %in% TRUE); b_hi <- branch2_with(!(om %in% FALSE))
  branch2 <- if (identical(b_lo, b_hi)) b_lo else NA
  run <- { r <- 0; prev <- 0
    for (i in ord) { if (!(om[i] %in% TRUE) || is.na(top[i]) || is.na(bot[i]) || abs(top[i] - prev) > 0.5) break
      r <- r + bot[i] - top[i]; prev <- bot[i] }
    r }
  passed <- if (tidal) FALSE else .any3(c(thick, branch2))
  .asc_result("organosol_asc", passed, which(om %in% TRUE),
              list(organic_cm_0_80 = t_true, from_surface_cm = run, tidal = tidal),
              if (is.na(passed)) c("oc_pct", "designation", "site$drainage_class") else character(0),
              "Key to orders, step 2; Organosols; glossary 'Organic materials'")
}

# ---- Podosols -------------------------------------------------------------------

#' Podosols (ASC order, key step 3)
#'
#' A Podosol diagnostic B horizon: a Bs, Bhs or Bh horizon, alone or in
#' combination, recognised by its designation (the ASC deliberately uses no
#' chemical criteria). A thin ironpan (Bsm, without h) does not count. Only
#' B horizons of the present soil count: not a buried horizon, not a
#' transition (BC, B/C), and not a horizon also marked t (a B with silicate
#' clay accumulation, as in Bts, is a textural B, not an illuvial
#' organic-aluminium or sesquioxide B). A row with no designation, or one
#' that cannot be read, among the designated rows leaves the order
#' undecided; rows with no designation below the last designated one lie
#' outside the description.
#' @noRd
podosol_asc <- function(pedon) {
  x <- .asc_horizons(pedon)
  des <- x$des
  bpod <- x$role %in% c("B1", "B2", "B3") & !des$buried & !des$t &
          (des$s | des$h) & !(des$s & des$m & !des$h)
  open_rows <- any(x$maybe_desc)
  passed <- if (any(bpod)) TRUE else if (!x$any_known || open_rows) NA else FALSE
  .asc_result("podosol_asc", passed, which(bpod),
              list(route = "designations", notation = des$notation$why),
              if (is.na(passed)) "designation" else character(0),
              "Key to orders, step 3; glossary 'Podosol diagnostic horizons'")
}

# ---- Vertosols ------------------------------------------------------------------

#' Vertosols (ASC order, key step 4)
#'
#' A clay field texture or 35 % clay or more throughout the solum (a surface
#' horizon of 30 mm or less excepted); cracks at least 5 mm wide when dry in
#' most years (cracks_width_cm of a horizon, or a cracking surface condition
#' in site$surface_condition: the Field Handbook's cracked surface has cracks
#' 6 mm or wider reaching 0.3 m or deeper); and slickensides or lenticular
#' peds in the solum. The key's "unless too moist" waives the cracks when the
#' major part of the solum was described moderately moist, moist or wet
#' (soil_water_status). With no record of cracks and a dry or unknown
#' description the cracks are undecided; narrower recorded cracks do not rule
#' the order out. Recorded slickensides absent everywhere with no lenticular
#' peds rule it out.
#' @noRd
vertosol_asc <- function(pedon) {
  x <- .asc_horizons(pedon); h <- x$h
  sol <- if (x$any_known) which(x$solum %in% TRUE) else which(x$mineral)
  # a peaty (organic) surface horizon is not tested for clay: the key lets
  # the cracks stop at its base
  sol <- sol[x$mineral[sol]]
  sol <- sol[order(h$top_cm[sol])]
  if (length(sol) > 1L && !is.na(x$thick[sol[1L]]) && x$thick[sol[1L]] <= 3) sol <- sol[-1L]
  # mineral rows of unknown role above the first C, D or R may belong to the
  # solum too
  maybe <- if (x$any_known) which(x$maybe_soil) else integer(0)
  sw <- .asc_sandwiched(x, maybe, sol)
  sol <- sort(c(sol, sw)); maybe <- setdiff(maybe, sw)
  tx <- x$clay$texture
  clayey <- ifelse(tx$clayey %in% TRUE | (!is.na(h$clay_pct) & h$clay_pct >= 35), TRUE,
                   ifelse(!is.na(h$clay_pct) | !is.na(tx$clayey), FALSE, NA))
  c_sol <- if (!length(sol)) NA else if (any(clayey[sol] %in% FALSE)) FALSE
           else if (all(clayey[c(sol, maybe)] %in% TRUE)) TRUE else NA
  surf <- tolower(paste(unlist(pedon$site$surface_condition %||% character(0)), collapse = " "))
  surf_crack <- grepl("crack", surf) &
                !grepl("uncrack|non[- ]?crack|\\bno[nt]?\\s+crack|crack\\w*\\s+(absent|none|not)", surf)
  hor_crack <- any(!is.na(h$cracks_width_cm) & h$cracks_width_cm >= 0.5)
  # "unless too moist": a solum described moist (its major part moderately
  # moist, moist or wet) cannot show its cracks, and the requirement is waived
  ws <- tolower(trimws(as.character(h$soil_water_status %||% rep(NA_character_, x$n))))
  moist <- ifelse(is.na(ws) | !nzchar(ws), NA, ifelse(grepl("moist|wet", ws), TRUE, ifelse(grepl("dry", ws), FALSE, NA)))
  too_moist <- if (length(sol)) .asc_rows_major(x, sol, moist) else NA
  cracks_route <- if (hor_crack) "horizon cracks" else if (surf_crack) "cracking surface"
                  else if (isTRUE(too_moist)) "described moist (waived)" else "no record"
  cracks <- if (hor_crack || surf_crack || isTRUE(too_moist)) TRUE else NA
  sk <- tolower(trimws(as.character(h$slickensides)))
  st <- tolower(as.character(h$structure_type))
  shrink <- (!is.na(sk) & sk %in% c("few", "common", "many", "continuous", "present", "yes")) |
            grepl("lenticular", st)
  rows_s <- if (length(sol)) sol else seq_len(x$n)
  rows_a <- c(rows_s, maybe)
  known_absent <- all(!is.na(sk[rows_a]) & sk[rows_a] %in% c("absent", "none", "no", "0") &
                      !grepl("lenticular", st[rows_a]))
  structures <- if (any(shrink[rows_s])) TRUE else if (known_absent) FALSE else NA
  passed <- .and3(c_sol, cracks, structures)
  .asc_result("vertosol_asc", passed, sol,
              list(clayey_solum = c_sol, cracks = cracks, cracks_route = cracks_route,
                   slickensides_or_lenticular = structures),
              if (is.na(passed)) c("clay_pct", "field_texture", "cracks_width_cm", "site$surface_condition",
                                   "soil_water_status", "slickensides")
              else character(0),
              "Key to orders, step 4; Vertosols")
}

# ---- Hydrosols ------------------------------------------------------------------

#' Hydrosols (ASC order, key step 5)
#'
#' More than half of the profile saturated for 2-3 months or more in most
#' years: from the days of saturation of the horizons (90 days or more
#' passes, less than 60 fails, between is undecided), otherwise from the site
#' drainage class (very poorly drained passes, poorly drained is undecided,
#' imperfectly drained or better fail), or tidal flooding
#' (site$tidal_inundation).
#' @noRd
hydrosol_asc <- function(pedon) {
  x <- .asc_horizons(pedon); h <- x$h
  if (isTRUE(as.logical(pedon$site$tidal_inundation %||% NA)))
    return(.asc_result("hydrosol_asc", TRUE, integer(0), list(route = "tidal"),
                       character(0), "Key to orders, step 5; Hydrosols"))
  days <- h$water_saturation_days
  by_days <- NA
  if (any(!is.na(days))) {
    ok <- ifelse(is.na(days), NA, ifelse(days >= 90, TRUE, ifelse(days < 60, FALSE, NA)))
    rows <- which(!is.na(h$top_cm) & !is.na(h$bottom_cm))
    zb <- suppressWarnings(max(h$bottom_cm[rows]))
    by_days <- if (length(rows) && is.finite(zb)) .asc_major_part(x, rows, ok, min(h$top_cm[rows]), zb) else NA
  }
  by_drain <- .asc_long_saturation(pedon)
  passed <- if (!is.na(by_days)) by_days else by_drain
  .asc_result("hydrosol_asc", passed, integer(0),
              list(route = if (!is.na(by_days)) "saturation days" else "drainage class",
                   by_days = by_days, by_drainage = by_drain),
              if (is.na(passed)) c("water_saturation_days", "site$drainage_class") else character(0),
              "Key to orders, step 5; Hydrosols")
}

# ---- Calcarosols ----------------------------------------------------------------

# Does the description end on rock or a hard layer (so that nothing below
# it is soil)?
.asc_ends_hard <- function(x) {
  h <- x$h
  ok <- which(!is.na(h$top_cm))
  if (!length(ok)) return(FALSE)
  isTRUE(x$hard[ok[which.max(h$top_cm[ok])]])
}

# Thickness of sandy (S, LS, CS) material in the upper 1.0 m: three-valued
# test of "at least 0.8 m". The part below the description is open, unless
# the description ends on rock or a hard layer.
.asc_deep_sandy <- function(x, need_cm = 80) {
  h <- x$h
  sandy <- .asc_is_sandy(h)
  w <- ifelse(!is.na(h$top_cm) & !is.na(h$bottom_cm), .asc_overlap(h$top_cm, h$bottom_cm, 0, 100), NA)
  t_true <- sum(w[sandy %in% TRUE & x$mineral], na.rm = TRUE)
  covered <- sum(w, na.rm = TRUE)
  t_open <- sum(w[is.na(sandy) & x$mineral], na.rm = TRUE) +
            if (.asc_ends_hard(x)) 0 else max(0, 100 - covered)
  if (t_true >= need_cm) TRUE else if (t_true + t_open < need_cm) FALSE else NA
}

#' Calcarosols (ASC order, key step 9)
#'
#' Calcareous throughout the solum, or from the horizon directly below the A1
#' or Ap or from 0.2 m, whichever is shallower; not a deep sandy profile
#' (sand, loamy sand or clayey sand over 0.8 m or more of the upper 1.0 m).
#' The carbonate is taken as pedogenic unless the layer origin says shell,
#' coquina, limestone or marl.
#' @noRd
calcarosol_asc <- function(pedon) {
  x <- .asc_horizons(pedon); h <- x$h
  calc <- .asc_calcareous(x)
  origin <- tolower(as.character(h$layer_origin %||% rep(NA_character_, x$n)))
  litho <- grepl("shell|coquina|limestone|marl", origin)
  calc[litho] <- FALSE
  ord <- order(h$top_cm, na.last = NA)
  # the A1 or Ap from the surface
  a_bottom <- NA_real_
  if (x$any_known) {
    for (i in ord) {
      if (!x$mineral[i]) next
      if (!(x$role[i] %in% "A1")) break
      a_bottom <- h$bottom_cm[i]
    }
  }
  d <- if (is.na(a_bottom)) 20 else min(a_bottom, 20)
  at_d <- which(!is.na(h$top_cm) & !is.na(h$bottom_cm) & h$top_cm <= d + 0.01 & h$bottom_cm > d + 0.01 & !x$organic)
  if (!length(at_d)) at_d <- which(!is.na(h$top_cm) & abs(h$top_cm - d) < 0.5 & !x$organic)
  # rock or a hard layer there: no calcareous soil below the A1
  shallow <- if (!length(at_d)) NA else if (isTRUE(x$hard[at_d[1L]])) FALSE else calc[at_d[1L]]
  sol <- which(x$solum %in% TRUE)
  # rows of unknown role in the present soil may be solum; one between two
  # solum horizons is
  maybe <- which(x$maybe_soil)
  sw <- .asc_sandwiched(x, maybe, sol)
  sol <- sort(c(sol, sw)); maybe <- setdiff(maybe, sw)
  throughout <- if (!length(sol)) NA else if (any(calc[sol] %in% FALSE)) FALSE
                else if (all(calc[c(sol, maybe)] %in% TRUE)) TRUE else NA
  calcareous <- .any3(c(shallow, throughout))
  sandy <- .asc_deep_sandy(x)
  passed <- .and3(calcareous, !sandy)
  .asc_result("calcarosol_asc", passed, if (length(at_d)) at_d[1L] else integer(0),
              list(test_depth_cm = d, calcareous = calcareous, deep_sandy = sandy),
              if (is.na(passed)) c("effervescence_class", "caco3_pct", "secondary_carbonates_pct",
                                   "field_texture", "clay_pct") else character(0),
              "Key to orders, step 9; Calcarosols; glossary 'Calcareous'")
}

# ---- Ferrosols, Dermosols, Kandosols --------------------------------------------

# The B2 horizon(s): rows with the B2 role (all subdivisions together).
# NULL (undecided) when the profile has no designations, or has no B2 and
# its description stops in an A or a B1 horizon: a B1 is dominated by the
# properties of an underlying B2 (glossary), and the Kandosols note B2t
# horizons below 1 m under thick A or B1 horizons, so the B2 may lie deeper
# than the description. A profile described through to a B3, BC, C, D or R
# with no B2 has none.
.asc_b2_rows <- function(x) {
  if (!x$any_known) return(NULL)
  rows <- which(x$b2 %in% TRUE)
  if (length(rows)) return(rows)
  top <- x$h$top_cm
  live <- which(!x$des$buried & !is.na(top) & !x$trailing)
  if (!length(live)) return(NULL)
  # a row of unknown role may be the B2
  if (length(.asc_b2_maybe(x))) return(NULL)
  last <- live[which.max(top[live])]
  # a buried soil below: the present soil ends above it
  if (any(x$des$buried & !is.na(top) & top >= top[last])) return(integer(0))
  if (x$unknown_role[last] || x$role[last] %in% c("A1", "A2", "A3", "AB", "BA", "A/B", "B/A", "B1"))
    return(NULL)
  integer(0)
}

# Rows of unknown role among the designated ones that may be part of the B2:
# mineral, in the present soil, below its A horizons and above its first C,
# D or R (x$maybe_b2).
.asc_b2_maybe <- function(x) {
  if (!x$any_known) return(integer(0))
  which(x$maybe_b2)
}

.asc_b2_major <- function(x, ok) {
  rows <- .asc_b2_rows(x)
  if (is.null(rows)) return(NA)
  if (!length(rows)) return(FALSE)
  .asc_rows_major_open(x, rows, .asc_b2_maybe(x), ok)
}

# The major part of `rows` when `maybe` rows may or may not belong to them:
# TRUE only if it holds with every maybe row that is not TRUE added (and the
# TRUE ones left out), FALSE only if it holds with every maybe row that is not
# FALSE added; otherwise undecided.
.asc_rows_major_open <- function(x, rows, maybe, ok) {
  if (!length(maybe)) return(.asc_rows_major(x, rows, ok))
  r_t <- .asc_rows_major(x, c(rows, maybe[!(ok[maybe] %in% TRUE)]), ok)
  if (isTRUE(r_t)) return(TRUE)
  r_f <- .asc_rows_major(x, c(rows, maybe[!(ok[maybe] %in% FALSE)]), ok)
  if (identical(r_f, FALSE)) return(FALSE)
  NA
}

# The rows of `maybe` that lie between two rows of `sol` (a row of unknown
# role between two solum horizons is part of the solum).
.asc_sandwiched <- function(x, maybe, sol) {
  top <- x$h$top_cm
  if (!length(maybe) || length(sol) < 2L) return(integer(0))
  maybe[vapply(maybe, function(i) !is.na(top[i]) && any(top[sol] < top[i], na.rm = TRUE) &&
                                  any(top[sol] > top[i], na.rm = TRUE), NA)]
}

# Three-valued "major part" of a set of rows, weighed by their own
# thicknesses (the window is the rows themselves); a row of unknown
# thickness leaves it undecided.
.asc_rows_major <- function(x, rows, ok) {
  if (!length(rows)) return(NA)
  w <- x$thick[rows]
  if (anyNA(w)) return(NA)
  tot <- sum(w)
  if (tot <= 0) return(NA)
  t_true <- sum(w[ok[rows] %in% TRUE]); t_false <- sum(w[ok[rows] %in% FALSE])
  if (t_true > tot / 2) TRUE else if (t_false >= tot / 2) FALSE else NA
}

# Vertic properties of a horizon (glossary): clayey (a clay field texture or
# 35 % clay or more) with slickensides or lenticular peds.
.asc_vertic_rows <- function(x) {
  h <- x$h
  tx <- x$clay$texture
  clayey <- ifelse(tx$clayey %in% TRUE | (!is.na(h$clay_pct) & h$clay_pct >= 35), TRUE,
                   ifelse(!is.na(h$clay_pct) | !is.na(tx$clayey), FALSE, NA))
  sk <- tolower(trimws(as.character(h$slickensides)))
  st <- tolower(as.character(h$structure_type))
  shr <- ifelse((!is.na(sk) & sk %in% c("few", "common", "many", "continuous", "present", "yes")) |
                  grepl("lenticular", st), TRUE,
                ifelse(!is.na(sk), FALSE, NA))
  .and3v(clayey, shr)
}

# Element-wise three-valued AND of two vectors.
.and3v <- function(a, b) ifelse(a %in% FALSE | b %in% FALSE, FALSE, ifelse(a %in% TRUE & b %in% TRUE, TRUE, NA))

#' Ferrosols (ASC order, key step 10)
#'
#' More than 5 % free iron oxide (as Fe, fe_dcb_pct) in the fine earth of the
#' major part of the B2, unless at least 0.3 m of the B2 has vertic
#' properties.
#' @noRd
ferrosol_asc <- function(pedon) {
  x <- .asc_horizons(pedon); h <- x$h
  fe <- ifelse(is.na(h$fe_dcb_pct), NA, h$fe_dcb_pct > 5)
  high_fe <- .asc_b2_major(x, fe)
  rows <- .asc_b2_rows(x)
  vert <- .asc_vertic_rows(x)
  v_cm <- if (is.null(rows) || !length(rows)) 0 else sum(x$thick[rows][vert[rows] %in% TRUE], na.rm = TRUE)
  maybe <- .asc_b2_maybe(x)
  v_open <- if (is.null(rows) || !length(rows)) 0 else
    sum(x$thick[rows][is.na(vert[rows])], na.rm = TRUE) + sum(x$thick[maybe][!(vert[maybe] %in% FALSE)], na.rm = TRUE)
  vertic30 <- if (v_cm >= 30) TRUE else if (v_cm + v_open < 30) FALSE else NA
  passed <- .and3(high_fe, !vertic30)
  .asc_result("ferrosol_asc", passed, rows %||% integer(0),
              list(free_fe_major_part = high_fe, vertic_b2_cm = v_cm),
              if (is.na(passed)) c("fe_dcb_pct", "designation", "slickensides") else character(0),
              "Key to orders, step 10; Ferrosols; glossary 'Free iron oxide'")
}

# Grade of pedality of each horizon: moderate or strong (or a recorded "weak
# to moderate", footnote 1 of the Dermosols) TRUE; weak, structureless,
# massive or single grain FALSE.
.asc_pedal <- function(h) {
  g <- tolower(trimws(as.character(h$structure_grade)))
  t <- tolower(trimws(as.character(h$structure_type)))
  ifelse(grepl("moderate|strong", g), TRUE,
         ifelse(grepl("weak|structureless|none|apedal", g) | grepl("massive|single", t), FALSE, NA))
}

#' @noRd
dermosol_asc <- function(pedon) {
  x <- .asc_horizons(pedon)
  passed <- .asc_b2_major(x, .asc_pedal(x$h))
  .asc_result("dermosol_asc", passed, .asc_b2_rows(x) %||% integer(0), list(),
              if (is.na(passed)) c("structure_grade", "designation") else character(0),
              "Key to orders, step 11; Dermosols; glossary 'Grade of pedality'")
}

#' Kandosols (ASC order, key step 12)
#'
#' A B2 that is massive or weakly structured over most of its thickness (a
#' single-grain B2 is read as apedal like a massive one) with more than 15 %
#' clay somewhere in it (lab clay, or a field texture of SL+ or heavier, as
#' the key equates them).
#' @noRd
kandosol_asc <- function(pedon) {
  x <- .asc_horizons(pedon)
  weak <- .asc_pedal(x$h)
  weak_major <- .asc_b2_major(x, ifelse(is.na(weak), NA, !weak))
  rows <- .asc_b2_rows(x)
  o15 <- .asc_over15(x$h)
  maybe <- .asc_b2_maybe(x)
  clay15 <- if (is.null(rows)) NA else if (!length(rows)) FALSE
            else if (any(o15[rows] %in% TRUE)) TRUE
            else if (all(o15[c(rows, maybe)] %in% FALSE)) FALSE else NA
  passed <- .and3(weak_major, clay15)
  .asc_result("kandosol_asc", passed, rows %||% integer(0),
              list(weak_or_massive = weak_major, clay_over_15 = clay15),
              if (is.na(passed)) c("structure_grade", "clay_pct", "field_texture", "designation")
              else character(0),
              "Key to orders, step 12; Kandosols")
}

# ---- Arenosols, Rudosols ---------------------------------------------------------

#' Arenosols (ASC order, key step 13)
#'
#' Within the upper 1.0 m: sand, loamy sand or clayey sand in layers totalling
#' at least 0.8 m; no layer with more than 15 % clay (lab clay, or a field
#' texture of SL+ or heavier) apart from argic horizons (clay lamellae,
#' designated with "lamellae"); coarse fragments at most 10 % as a weighted
#' average (a missing record is read as none); no hard layer (rock, or a
#' moderately or more cemented layer).
#' @noRd
arenosol_asc <- function(pedon) {
  x <- .asc_horizons(pedon); h <- x$h
  sandy <- .asc_deep_sandy(x)
  within1m <- !is.na(h$top_cm) & h$top_cm < 100
  in1m <- within1m & x$mineral
  lam <- grepl("lamell", tolower(as.character(h$designation)))
  over15 <- ifelse(lam, FALSE, .asc_over15(h))
  clay_ok <- if (any(over15[in1m] %in% TRUE)) FALSE else if (all(over15[in1m] %in% FALSE)) TRUE else NA
  w <- ifelse(!is.na(h$top_cm) & !is.na(h$bottom_cm), .asc_overlap(h$top_cm, h$bottom_cm, 0, 100), 0)
  cf <- ifelse(is.na(h$coarse_fragments_pct), 0, h$coarse_fragments_pct)
  cf_avg <- if (sum(w) > 0) sum(cf * w) / sum(w) else NA
  cf_ok <- if (is.na(cf_avg)) NA else cf_avg <= 10
  hard <- x$hard
  deepest <- suppressWarnings(max(h$bottom_cm, na.rm = TRUE))
  hard_ok <- if (any(hard & within1m)) FALSE else if (is.finite(deepest) && deepest >= 100) TRUE else NA
  passed <- .and3(sandy, clay_ok, cf_ok, hard_ok)
  .asc_result("arenosol_asc", passed, which(in1m),
              list(sandy_80cm = sandy, no_layer_over_15_clay = clay_ok,
                   coarse_fragments_avg = cf_avg, no_hard_layer = hard_ok),
              if (is.na(passed)) c("field_texture", "clay_pct", "bottom_cm") else character(0),
              "Key to orders, step 13; Arenosols")
}

#' Rudosols (ASC order, key step 14)
#'
#' Negligible pedologic organisation: no B horizon (a C/B with B material in
#' rock fissures is a Tenosol), no A2, and an A1 that is single grain,
#' massive or weakly structured (or no A1). Read from the designations;
#' buried horizons are not read, and a mineral row of unknown role above the
#' first C, D or R may be a B.
#' @noRd
rudosol_asc <- function(pedon) {
  x <- .asc_horizons(pedon); h <- x$h
  if (!x$any_known)
    return(.asc_result("rudosol_asc", NA, integer(0), list(), "designation",
                       "Key to orders, step 14; Rudosols"))
  r <- x$role
  live <- !x$des$buried
  # rows of unknown role in the present soil may be B horizons
  maybe_b <- x$maybe_soil
  has_b <- if (any(live & x$des$known & (grepl("^B", r) | r %in% c("C/B", "CB", "A/B", "AB", "lamellae")))) TRUE
           else if (any(maybe_b)) NA else FALSE
  has_a2 <- any(live & r %in% c("A2", "A3")) || any(live & x$des$e)
  a1 <- which(r %in% "A1" & live)
  pedal <- .asc_pedal(h)
  a1_ok <- if (!length(a1)) TRUE else if (any(pedal[a1] %in% TRUE)) FALSE
           else if (all(pedal[a1] %in% FALSE)) TRUE else NA
  passed <- .and3(!has_b, !has_a2, a1_ok)
  .asc_result("rudosol_asc", passed, integer(0),
              list(b_horizon = has_b, a2 = has_a2, a1_weak_or_apedal = a1_ok),
              if (is.na(passed)) c("structure_grade", "designation") else character(0),
              "Key to orders, step 14; Rudosols")
}
