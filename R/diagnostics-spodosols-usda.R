# =============================================================
# USDA Soil Taxonomy 13th edition (2022)
# Helpers for Spodosols Suborder/GG/SG keys -- Chapter 14, pp 311-320
# =============================================================
#
# Spodosols are soils with a spodic horizon (illuvial accumulation of
# organic matter, Al, and/or Fe). The 5 Suborders are:
#   Aquods   -- aquic conditions within 50 cm
#   Gelods   -- gelic STR (very cold)
#   Cryods   -- cryic STR (cold)
#   Humods   -- 6%+ OC in spodic horizon
#   Orthods  -- catch-all
#
# Reference: Soil Survey Staff (2022), KST 13ed, Ch. 14.
# =============================================================


# ---- Shared readings (v0.9.418) -------------------------------------

# Three-valued helpers: TRUE / FALSE / NA vectors combine with R's & and |,
# which follow Kleene's logic (TRUE | NA is TRUE, FALSE & NA is FALSE).
# "Any" of a three-valued vector: TRUE if one is TRUE, FALSE if all are FALSE.
.any3u <- function(x) {
  if (any(x %in% TRUE)) TRUE else if (length(x) == 0L || all(x %in% FALSE)) FALSE else NA
}
.all3u <- function(x) {
  if (any(x %in% FALSE)) FALSE else if (all(x %in% TRUE)) TRUE else NA
}

# v0.9.418: organic soil material, layer by layer (KST 13th ed., p. 7: 12 %
# organic carbon or more). A measured organic carbon decides; without one an
# O or H master designation is the surveyor's record of organic material and
# any other master designation of mineral material (reading L10). No
# designation and no organic carbon: undecided.
.usda_organic_status <- function(pedon) {
  h <- pedon$horizons
  d <- .desg_master(h$designation, site = pedon$site)
  des <- ifelse(is.na(d) | !nzchar(trimws(d)), NA, grepl("^[OH]", d))
  oc <- suppressWarnings(as.numeric(h$oc_pct))
  ifelse(!is.na(oc), oc >= 12, des)
}

# The mineral soil surface (KST 13th ed., p. 7): the top of the first layer,
# from the top, that is not organic soil material.
.usda_mineral_surface <- function(pedon, org = .usda_organic_status(pedon)) {
  h <- pedon$horizons
  if (nrow(h) == 0L) return(0)
  ord <- order(h$top_cm)
  k <- ord[!(org[ord] %in% TRUE)]
  if (length(k)) h$top_cm[k[1L]] else max(h$bottom_cm, na.rm = TRUE)
}

# Hue tests of the spodic colours (KST 13th ed., p. 38): TRUE / FALSE / NA.
.usda_hue_parts <- function(hue) {
  x <- toupper(gsub("\\s+", "", as.character(hue)))
  num <- suppressWarnings(as.numeric(sub("^([0-9.]+).*$", "\\1", x)))
  page <- sub("^[0-9.]+", "", x)
  list(x = x, num = num, page = page)
}
.usda_hue_5yr_or_redder <- function(hue) {
  p <- .usda_hue_parts(hue)
  ifelse(is.na(hue), NA,
         ifelse(p$page == "R", TRUE,
                ifelse(p$page == "YR", !is.na(p$num) & p$num <= 5, FALSE)))
}
.usda_hue_is <- function(hue, what) {
  p <- .usda_hue_parts(hue)
  ifelse(is.na(hue), NA, p$x %in% what)
}
.usda_hue_neutral <- function(hue) {
  p <- .usda_hue_parts(hue)
  ifelse(is.na(hue), NA, grepl("^N", p$x))
}

# Loamy fine sand or coarser (the sands and loamy sands of the KST, p. 68):
# silt + 2 x clay < 30 from the measured fractions (silt as 100 - clay - sand
# where only sand is given), else the field texture class
# (texture_class_usda); NA where neither decides.
.usda_lfs_or_coarser <- function(h) {
  n <- nrow(h)
  out <- rep(NA, n)
  for (i in seq_len(n)) {
    ft <- .field_texture_fallback(h, i)
    if (!is.null(ft)) { out[i] <- ft$lfs_or_coarser; next }
    cl <- h$clay_pct[i]; si <- h$silt_pct[i]; sd <- h$sand_pct[i]
    if (is.na(si) && !is.na(cl) && !is.na(sd)) si <- 100 - cl - sd
    if (!is.na(cl) && !is.na(si)) out[i] <- si + 2 * cl < 30
    else if (!is.na(cl) && cl >= 15) out[i] <- FALSE
  }
  out
}

# Pedogenic cementation recorded in the field (reading L6: weakly cemented
# counts). A field observation: not recorded, not cemented.
.USDA_CEMENTED <- c("extremely weakly", "very weakly", "weakly", "moderately",
                    "strongly", "very strongly", "indurated")
.usda_cemented <- function(h) {
  cem <- tolower(trimws(as.character(h$cementation_class)))
  pan <- tolower(trimws(as.character(h$pan_type %||% rep(NA, nrow(h)))))
  (!is.na(cem) & cem %in% .USDA_CEMENTED) |
    (!is.na(pan) & pan %in% c("ortstein", "duripan", "ironpan", "placic", "organic pan"))
}

# The share (%) of each pedon in which a cemented layer is cemented (reading
# L9, "in 90 percent or more of each pedon", read laterally): the recorded
# share (duripan_pct, which the ISIS reader fills from the continuity of a
# duripan), else 95 for a designation with the suffix m (continuous or nearly
# continuous cementation), else unknown (NA).
.usda_cemented_share <- function(pedon) {
  h <- pedon$horizons
  d <- .desg_master(h$designation, site = pedon$site)
  suffix <- sub("^[A-Z]+", "", ifelse(is.na(d), "", d))
  share <- suppressWarnings(as.numeric(h$duripan_pct))
  share[!is.na(share) & share <= 0] <- NA_real_
  m <- is.na(share) & grepl("m", suffix)
  share[m] <- 95
  share
}


# ---- Spodic horizon (KST 13th ed., pp. 31, 38-39) ---------------------

#' Spodic horizon (USDA, KST 13th ed.)
#'
#' v0.9.418: the Keys to Soil Taxonomy, 13th edition, read as written; up to
#' v0.9.417 the WRB \code{spodic()} decided. A spodic horizon (p. 31) has 85
#' percent or more spodic materials in a layer 2.5 cm or more thick that is
#' not part of an Ap horizon. Spodic materials (pp. 38-39) are mineral soil
#' materials with:
#' \enumerate{
#'   \item a pH (water) of 5.9 or less and 0.6 percent organic carbon or
#'     more; and
#'   \item either (a) directly under an albic horizon, a moist colour of hue
#'     5YR or redder, of 7.5YR with value 5 or less and chroma 4 or less, of
#'     10YR or N with value and chroma 2 or less, or 10YR 3/1; or (b) one of
#'     those colours, or 7.5YR with value 5 or less and chroma 5 or 6, and one
#'     of: pedogenic cementation (any recorded cementation class, reading L6),
#'     10 percent or more cracked coatings on sand grains, Al + 1/2 Fe
#'     (oxalate) of 0.50 or more and half that or less in an overlying A or E
#'     horizon (the epipedon or albic horizon), or an ODOE of 0.25 or more and
#'     half that or less in an overlying A or E horizon.
#' }
#' Missing data: a colour, pH, organic carbon, oxalate extract or ODOE not
#' recorded leaves the layer undecided (reading L5 for ODOE), and the result
#' is \code{NA} when an undecided layer could make the horizon; cementation
#' and cracked coatings are field observations, absent when not recorded. The
#' albic horizon is read from albic materials (p. 32) in an E horizon; a dry
#' value that rules out albic materials decides even without the moist colour
#' (reading L8: the moist value is never higher than the dry one). Layers of
#' an argillic or kandic horizon are not excluded here.
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}}: \code{layers} the layers of the
#'   spodic horizon, \code{evidence$maybe_layers} the undecided ones and
#'   \code{evidence$table} the reading of every layer.
#' @noRd
spodic_horizon_usda <- function(pedon) {
  .memo(pedon, "spodic_horizon_usda", list(),
        .spodic_horizon_usda_uncached(pedon = pedon))
}

# The albic materials of each layer (KST 13th ed., pp. 31-32), TRUE / FALSE /
# NA, and the albic horizon (an eluvial E horizon of albic materials).
.usda_albic_status <- function(pedon, org = .usda_organic_status(pedon)) {
  h <- pedon$horizons
  mv <- suppressWarnings(as.numeric(h$munsell_value_moist))
  dv <- suppressWarnings(as.numeric(h$munsell_value_dry))
  mc <- suppressWarnings(as.numeric(h$munsell_chroma_moist))
  dc <- suppressWarnings(as.numeric(h$munsell_chroma_dry))
  ch <- ifelse(is.na(mc), dc, mc)
  mat <- (ch <= 2 & ((mv == 3 & dv >= 6) | (mv >= 4 & dv >= 5))) |
         (ch <= 3 & (mv >= 6 | dv >= 7))
  # L8: the moist value is at most the dry value; a dry value under 5 rules
  # albic materials out when the moist value is not recorded
  mat[is.na(mat) & is.na(mv) & !is.na(dv) & dv < 5] <- FALSE
  pct <- suppressWarnings(as.numeric(h$albic_materials_pct %||% rep(NA, nrow(h))))
  mat[!is.na(pct) & pct < 85] <- FALSE
  d <- .desg_master(h$designation, site = pedon$site)
  eluv <- ifelse(is.na(d) | !nzchar(trimws(d)), NA, grepl("^E", d))
  hz <- mat & eluv & !(org %in% TRUE)
  hz[org %in% TRUE] <- FALSE
  list(materials = mat, horizon = hz)
}

.spodic_horizon_usda_uncached <- function(pedon) {
  ref <- "Soil Survey Staff (2022), KST 13th ed., p. 31 (spodic horizon) and pp. 38-39 (spodic materials)"
  h <- pedon$horizons
  n <- nrow(h)
  if (n == 0L) {
    return(DiagnosticResult$new(name = "spodic_horizon_usda", passed = FALSE,
                                layers = integer(0),
                                evidence = list(reason = "no horizons"),
                                missing = character(0), reference = ref))
  }
  org <- .usda_organic_status(pedon)
  alb <- .usda_albic_status(pedon, org)
  d <- .desg_master(h$designation, site = pedon$site)
  d0 <- ifelse(is.na(d), "", d)
  num <- function(x) suppressWarnings(as.numeric(x))
  ph <- num(h$ph_h2o); oc <- num(h$oc_pct)
  hue <- h$munsell_hue_moist
  v <- num(h$munsell_value_moist); cr <- num(h$munsell_chroma_moist)
  chem <- ph <= 5.9 & oc >= 0.6
  h10 <- .usda_hue_is(hue, "10YR"); h75 <- .usda_hue_is(hue, "7.5YR")
  colA <- .usda_hue_5yr_or_redder(hue) | (h75 & v <= 5 & cr <= 4) |
          ((h10 | .usda_hue_neutral(hue)) & v <= 2 & cr <= 2) |
          (h10 & v == 3 & cr == 1)
  colB <- colA | (h75 & v <= 5 & cr >= 5 & cr <= 6)
  cem <- .usda_cemented(h)
  share <- .usda_cemented_share(pedon)
  cem50 <- cem & (is.na(share) | share >= 50)
  ccp <- num(h$cracked_coatings_pct %||% rep(NA, n))
  cracked <- !is.na(ccp) & ccp >= 10
  alfe <- num(h$al_ox_pct) + 0.5 * num(h$fe_ox_pct)
  odoe <- num(h$odoe %||% rep(NA, n))
  ord <- order(h$top_cm)
  pos <- integer(n); pos[ord] <- seq_len(n)
  mineral <- !(org %in% TRUE)
  over_ae <- grepl("^[AE]", d0) | (alb$horizon %in% TRUE)
  # an overlying horizon with half the value or less (items b(3), b(4))
  halved <- function(val, i, min_val) {
    above <- ord[seq_len(pos[i] - 1L)]
    above <- above[mineral[above] & over_ae[above]]
    if (!length(above)) return(FALSE)
    if (is.na(val[i])) return(NA)
    if (val[i] < min_val) return(FALSE)
    .any3u(val[above] <= val[i] / 2)
  }
  ox <- vapply(seq_len(n), function(i) halved(alfe, i, 0.50), NA)
  od <- vapply(seq_len(n), function(i) halved(odoe, i, 0.25), NA)
  # item a: directly under an albic horizon (the next mineral layer above)
  under_albic <- vapply(seq_len(n), function(i) {
    above <- ord[seq_len(pos[i] - 1L)]
    above <- above[mineral[above]]
    if (!length(above)) return(FALSE)
    alb$horizon[above[length(above)]]
  }, NA)
  pa <- under_albic & colA
  pb <- colB & (cem50 | cracked | ox | od)
  spm <- chem & (pa | pb)
  spm[org %in% TRUE] <- FALSE
  smp <- num(h$spodic_materials_pct %||% rep(NA, n))
  spm85 <- spm
  spm85[!is.na(smp) & smp < 85] <- FALSE
  ap <- grepl("^Ap", d0)
  # A layer 2.5 cm or more thick, not part of an Ap horizon: adjoining layers
  # count together.
  thk <- num(h$bottom_cm) - num(h$top_cm)
  in_run <- function(ok) {
    res <- rep(FALSE, n); run <- integer(0); bot <- NA_real_
    flush <- function(run) {
      if (length(run) && sum(thk[run], na.rm = TRUE) >= 2.5) res[run] <<- TRUE
    }
    for (i in ord) {
      if (ok[i]) {
        if (length(run) && !is.na(bot) && abs(h$top_cm[i] - bot) > 1e-6) { flush(run); run <- integer(0) }
        run <- c(run, i); bot <- h$bottom_cm[i]
      } else { flush(run); run <- integer(0) }
    }
    flush(run)
    res
  }
  yes <- in_run(spm85 %in% TRUE & !ap)
  may <- in_run(!(spm85 %in% FALSE) & !ap)
  layer <- ifelse(yes, TRUE, ifelse(may, NA, FALSE))
  passed <- .any3u(layer)
  sure <- ord[ord %in% which(layer %in% TRUE)]
  maybe <- ord[ord %in% which(is.na(layer))]
  miss <- character(0)
  if (is.na(passed)) {
    for (i in maybe) {
      if (is.na(chem[i])) miss <- c(miss, if (is.na(ph[i])) "ph_h2o", if (is.na(oc[i])) "oc_pct")
      if (is.na(colB[i]) || is.na(colA[i]))
        miss <- c(miss, if (is.na(hue[i])) "munsell_hue_moist",
                  if (is.na(v[i])) "munsell_value_moist", if (is.na(cr[i])) "munsell_chroma_moist")
      if (is.na(ox[i])) miss <- c(miss, "al_ox_pct", "fe_ox_pct")
      if (is.na(od[i])) miss <- c(miss, "odoe")
      if (is.na(under_albic[i])) miss <- c(miss, "munsell_value_dry")
    }
  }
  tab <- data.frame(layer = seq_len(n), designation = h$designation,
                    top_cm = h$top_cm, bottom_cm = h$bottom_cm,
                    organic = org, albic = alb$horizon, chemistry = chem,
                    colour_a = colA, colour_b = colB, under_albic = under_albic,
                    cemented = cem50, cracked_coatings = cracked,
                    oxalate = ox, odoe = od, spodic_materials = spm,
                    spodic_horizon = layer, stringsAsFactors = FALSE)
  DiagnosticResult$new(
    name = "spodic_horizon_usda", passed = passed, layers = sure,
    evidence = list(maybe_layers = maybe, table = tab,
                    spodic_materials = spm85, albic_horizon = alb$horizon,
                    organic = org, alfe = alfe),
    missing = unique(miss), reference = ref)
}

# The layers the great-group and subgroup tests read: the spodic horizon,
# or, when it is undecided, its undecided layers (a result read on them is
# at best undecided).
.spodic_layers_for_tests <- function(pedon) {
  sp <- spodic_horizon_usda(pedon)
  if (isTRUE(sp$passed)) return(list(layers = sp$layers, sure = TRUE, sp = sp))
  if (is.na(sp$passed)) return(list(layers = sp$evidence$maybe_layers, sure = FALSE, sp = sp))
  list(layers = integer(0), sure = TRUE, sp = sp)
}

# A share of a horizon's thickness: TRUE when the layers that pass make
# `min_share` or more of it, FALSE when those that pass or are undecided make
# less, NA otherwise.
.share3 <- function(ok, w, min_share) {
  tot <- sum(w, na.rm = TRUE)
  if (tot <= 0) return(FALSE)
  yes <- sum(w[ok %in% TRUE], na.rm = TRUE) / tot
  may <- sum(w[!(ok %in% FALSE)], na.rm = TRUE) / tot
  if (yes >= min_share - 1e-9) TRUE else if (may < min_share - 1e-9) FALSE else NA
}


# ---- Plaggen-like surface (KST 13th ed., p. 17) ----------------------

# v0.9.418: a surface horizon of mineral soil material with the plaggen
# epipedon's colour (value 4 or less moist, 5 or less dry, chroma 2 or less)
# and organic carbon (0.6 % or more), anthropogenic evidence (artefacts, or a
# plaggen designation such as Apl), and a thickness in [min_cm, max_cm).
# Used for the plaggen exclusion of the Spodosols (p. 68, min 50) and the
# Haploplaggic subgroups (25 to 50 cm). Artefacts and the designation are
# field observations (not recorded: absent); colour and carbon not recorded
# leave it undecided.
.usda_plaggen_surface <- function(pedon, min_cm = 50, max_cm = Inf) {
  h <- pedon$horizons
  if (nrow(h) == 0L) return(FALSE)
  org <- .usda_organic_status(pedon)
  ms <- .usda_mineral_surface(pedon, org)
  ord <- order(h$top_cm)
  ord <- ord[!(org[ord] %in% TRUE) & h$top_cm[ord] >= ms - 1e-6]
  num <- function(x) suppressWarnings(as.numeric(x))
  art <- num(h$artefacts_pct)
  dsg <- ifelse(is.na(h$designation), "", h$designation)
  anthro <- (!is.na(art) & art > 0) | grepl("^[0-9]*Apl|plagg", dsg, ignore.case = TRUE)
  col <- num(h$munsell_value_moist) <= 4 & num(h$munsell_value_dry) <= 5 &
         num(h$munsell_chroma_moist) <= 2
  ok <- anthro & col & num(h$oc_pct) >= 0.6
  run <- integer(0)
  for (i in ord) {
    if (ok[i] %in% FALSE) break
    run <- c(run, i)
  }
  if (!length(run) || !any(anthro[run])) return(FALSE)
  thk <- max(h$bottom_cm[run]) - ms
  sure <- run[cumsum(!(ok[run] %in% TRUE)) == 0]
  thk_sure <- if (length(sure)) max(h$bottom_cm[sure]) - ms else 0
  if (thk_sure >= min_cm && thk_sure < max_cm && length(sure) == length(run)) return(TRUE)
  if (thk < min_cm) return(FALSE)
  if (thk_sure >= max_cm) return(FALSE)
  NA
}

#' Haploplaggic subgroups (KST 13th ed., pp. 313-317, 184)
#'
#' v0.9.418: a surface horizon between 25 and 50 cm thick that meets all the
#' requirements of a plaggen epipedon except thickness (see
#' \code{.usda_plaggen_surface}). The subgroup lists call it in a later
#' release.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
haploplaggic_subgroup_usda <- function(pedon) {
  p <- .usda_plaggen_surface(pedon, min_cm = 25, max_cm = 50)
  DiagnosticResult$new(
    name = "haploplaggic_subgroup_usda", passed = p, layers = integer(0),
    evidence = list(),
    missing = if (is.na(p)) c("munsell_value_moist", "munsell_value_dry",
                              "munsell_chroma_moist", "oc_pct") else character(0),
    reference = "Soil Survey Staff (2022), KST 13th ed., p. 17 (plaggen epipedon) and p. 313")
}


# ---- Albic horizon helper -------------------------------------------

#' Albic horizon (USDA, KST 13ed Ch 3)
#'
#' Pass when an albic horizon (light-colored, eluvial; chroma <= 2,
#' value >= 4) >= 1 cm thick is present. Delegates to WRB albic
#' diagnostic.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @noRd
albic_horizon_usda <- function(pedon) {
  res <- albic(pedon)
  res$name <- "albic_horizon_usda"
  res
}


# ---- Placic horizon helper ------------------------------------------

#' Placic horizon (USDA, KST 13th ed., p. 30)
#'
#' v0.9.418: the text of p. 30. A placic horizon is pedogenically cemented
#' or indurated with iron, or iron and manganese, and organic matter; it is
#' laterally continuous; it is 1 mm or more thick and, where associated with
#' spodic materials, less than 25 mm thick. Read here as a recorded thin iron
#' pan (pan_type), or a recorded cementation (any class, reading L6) in a
#' layer whose designation carries the suffix m together with s (or the older
#' ir), as in Bsm: cemented by iron. A layer whose top is within
#' \code{max_top_cm} of the mineral soil surface, 0.1 cm thick or more, and
#' less than 2.5 cm where it is or touches a layer of spodic materials. Up to
#' v0.9.417 it took any suffix m with a strong or indurated class, and always
#' 0.1 to 2.5 cm. Cementation and pans are field observations: not recorded,
#' absent.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_top_cm Default 100 (from the mineral soil surface).
#' @return A \code{\link{DiagnosticResult}}.
#' @noRd
placic_horizon_usda <- function(pedon, max_top_cm = 100) {
  ref <- "Soil Survey Staff (2022), KST 13th ed., p. 30"
  h <- pedon$horizons
  n <- nrow(h)
  if (n == 0L) {
    return(DiagnosticResult$new(name = "placic_horizon_usda", passed = FALSE,
                                layers = integer(0), evidence = list(),
                                missing = character(0), reference = ref))
  }
  org <- .usda_organic_status(pedon)
  ms <- .usda_mineral_surface(pedon, org)
  d <- .desg_master(h$designation, site = pedon$site)
  suffix <- sub("^[A-Z]+", "", ifelse(is.na(d), "", d))
  pan <- tolower(trimws(as.character(h$pan_type %||% rep(NA, n))))
  thin_pan <- !is.na(pan) & pan %in% c("ironpan", "thin ironpan", "placic")
  iron_m <- grepl("m", suffix) & grepl("s|ir", suffix)
  cand <- which(!is.na(h$top_cm) & h$top_cm - ms < max_top_cm &
                  (thin_pan | (.usda_cemented(h) & iron_m)))
  thk <- h$bottom_cm[cand] - h$top_cm[cand]
  spm <- .spodic_materials_any(pedon)
  near_spodic <- vapply(cand, function(i) {
    adj <- which(abs(h$bottom_cm - h$top_cm[i]) < 1e-6 |
                   abs(h$top_cm - h$bottom_cm[i]) < 1e-6)
    any(spm[c(i, adj)] %in% TRUE)
  }, NA)
  ok <- !is.na(thk) & thk >= 0.1 & (!near_spodic | thk < 2.5)
  passing <- cand[ok]
  DiagnosticResult$new(
    name = "placic_horizon_usda", passed = length(passing) > 0L,
    layers = passing,
    evidence = list(candidates = cand, thicknesses_cm = thk,
                    next_to_spodic_materials = near_spodic,
                    max_top_cm = max_top_cm),
    missing = character(0), reference = ref)
}

# Spodic materials layer by layer (TRUE / FALSE / NA), from the reading of
# the spodic horizon.
.spodic_materials_any <- function(pedon) {
  sp <- spodic_horizon_usda(pedon)
  sp$evidence$spodic_materials %||% rep(FALSE, nrow(pedon$horizons))
}


# ---- Fragipan helper -------------------------------------------------

#' Fragipan (USDA, KST 13ed Ch 3, p 38)
#'
#' Pass when a horizon has fragic soil properties:
#' \itemize{
#'   \item rupture_resistance class >= "firm" (firm, very firm,
#'         extremely firm); OR
#'   \item NASIS pediagfeatures has a "Fragipan" entry (v0.9.31:
#'         the surveyor's field-identified fragipan -- direct evidence,
#'         used as a tie-breaker when rupture_resistance is missing
#'         from the lab data); AND
#'   \item thickness >= 15 cm.
#' }
#' KSSL pedons rarely carry rupture_resistance; NASIS pediagfeatures
#' carries 13 500 entries including "Fragipan" tags from surveyors.
#' v0.9.31 adds the NASIS path so fragipan can be detected on KSSL+
#' NASIS pedons (closing the Fragiudults / Fragiudalfs / Fragiaqualfs
#' confusion documented in the v0.9.25 Great Group analysis).
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_top_cm Default 100.
#' @return A \code{\link{DiagnosticResult}}.
#' @noRd
fragipan_usda <- function(pedon, max_top_cm = 100) {
  h <- pedon$horizons
  cand <- which(!is.na(h$top_cm) & h$top_cm < max_top_cm)
  rr <- h$rupture_resistance[cand]
  miss <- if (all(is.na(rr))) "rupture_resistance" else character(0)
  firm_classes <- c("firm", "very firm", "extremely firm")
  passing <- cand[!is.na(rr) & tolower(rr) %in% firm_classes]
  thk_lab <- if (length(passing) > 0L)
               sum(pmax(h$bottom_cm[passing] - h$top_cm[passing], 0),
                     na.rm = TRUE)
             else 0
  passed_lab <- thk_lab >= 15

  # v0.9.31: NASIS pediagfeatures Fragipan flag.
  nasis_fragipan <- .has_nasis_feature(pedon, "fragipan")

  passed <- isTRUE(passed_lab) || isTRUE(nasis_fragipan)
  evidence_source <- if (isTRUE(passed_lab)) "rupture_resistance"
                     else if (isTRUE(nasis_fragipan)) "nasis_pediagfeatures"
                     else NA_character_

  DiagnosticResult$new(
    name = "fragipan_usda", passed = passed, layers = passing,
    evidence = list(thickness_cm = thk_lab, max_top_cm = max_top_cm,
                      nasis_fragipan_flag = nasis_fragipan,
                      evidence_source = evidence_source,
                      note = "v0.9.31: NASIS pediagfeatures Fragipan accepted as tie-breaker"),
    missing = if (isTRUE(passed)) character(0) else miss,
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 3, p 38"
  )
}


# ---- Duripan helper --------------------------------------------------

#' Duripan (USDA Soil Taxonomy, KST 13th ed., pp. 23-24)
#'
#' A subhorizon cemented or indurated in more than 50 \% of its volume,
#' with evidence of silica accumulation, whose lateral continuity limits the
#' roots. v0.9.419: up to v0.9.418 only an indurated layer with
#' \code{duripan_pct} of 50 or more, so a recorded duripan of another
#' cementation class, or with no cementation class, was missed (audit I9).
#' Read layer by layer:
#' \itemize{
#'   \item the cemented share: \code{duripan_pct} (the volume of the
#'     silica-cemented pan) above 50; without it, a designation with q and m
#'     (the m of KST Ch. 18 is cementation of more than 90 \% of the
#'     horizon) or a recorded duripan or silcrete pan (\code{pan_type});
#'     a q designation in a layer recorded as cemented, with no share, is
#'     undecided;
#'   \item silica: a q in the designation, a recorded duripan share above 0
#'     or a duripan or silcrete pan; the slaking test of item 2 is not
#'     recorded and these field records stand for it.
#' }
#' Nothing recorded: no duripan (a field observation).
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_top_cm Upper boundary within this depth (cm).
#' @return A \code{\link{DiagnosticResult}}.
#' @noRd
duripan_usda <- function(pedon, max_top_cm = 100) {
  ref <- "Soil Survey Staff (2022), KST 13th ed., Ch. 3, duripan (pp. 23-24)"
  h <- pedon$horizons
  cand <- which(!is.na(h$top_cm) & h$top_cm < max_top_cm)
  d <- .usda_desig(h$designation)
  dp <- .usda_col(h, "duripan_pct", NA_real_)
  pan <- tolower(as.character(.usda_col(h, "pan_type", NA_character_)))
  cem <- tolower(as.character(.usda_col(h, "cementation_class", NA_character_)))
  q <- !is.na(d$suffix) & grepl("q", d$suffix)
  qm <- q & grepl("m", d$suffix)
  pan_d <- !is.na(pan) & grepl("duripan|silcrete", pan)
  cemented <- !is.na(cem) & cem %in% c("weakly", "moderately", "strongly", "indurated")
  share <- ifelse(!is.na(dp), dp > 50,
           ifelse(qm | pan_d, TRUE, ifelse(q & cemented, NA, FALSE)))
  silica <- q | (!is.na(dp) & dp > 0) | pan_d
  lay <- ifelse(silica, share, FALSE)
  v <- lay[cand]
  passed <- if (any(v %in% TRUE)) TRUE else if (anyNA(v)) NA else FALSE
  DiagnosticResult$new(
    name = "duripan_usda", passed = passed,
    layers = cand[if (isTRUE(passed)) v %in% TRUE else !(v %in% FALSE)],
    evidence = list(max_top_cm = max_top_cm, cemented_share_over_50 = share[cand],
                    silica = silica[cand]),
    missing = if (is.na(passed)) "duripan_pct" else character(0),
    reference = ref
  )
}


# ---- Duric Subgroup helper (Aquods/Cryods) --------------------------

#' Duric Subgroup helper (USDA Spodosols)
#'
#' Pass when a pedogenically cemented horizon (extremely weakly
#' coherent or stronger) is present in 90\%+ of the pedon within
#' 100 cm. v0.8 proxy: any horizon with cementation_class >= "weakly".
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_top_cm Default 100.
#' @return A \code{\link{DiagnosticResult}}.
#' @noRd
duric_subgroup_usda <- function(pedon, max_top_cm = 100) {
  h <- pedon$horizons
  cand <- which(!is.na(h$top_cm) & h$top_cm < max_top_cm)
  cem <- h$cementation_class[cand]
  miss <- if (all(is.na(cem))) "cementation_class" else character(0)
  cemented <- c("weakly", "moderately", "strongly", "indurated")
  is_cem  <- !is.na(cem) & tolower(cem) %in% cemented
  passing <- cand[is_cem]
  # KST 13ed Ch. 14: in 90% or more of each pedon, a (pedogenically) cemented
  # horizon within max_top_cm. Corrected from "any single cemented layer" to a
  # cumulative-thickness >= 90% test (clipping layers to the depth window).
  clip <- function(idx) sum(pmax(pmin(h$bottom_cm[idx], max_top_cm) -
                                   pmax(h$top_cm[idx], 0), 0), na.rm = TRUE)
  thk_cem   <- clip(passing)
  thk_total <- clip(cand)
  passed    <- thk_total > 0 && thk_cem >= 0.90 * thk_total
  DiagnosticResult$new(
    name = "duric_subgroup_usda", passed = passed, layers = passing,
    evidence = list(max_top_cm = max_top_cm, cemented_cm = thk_cem,
                    total_cm = thk_total),
    missing = miss,
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 14"
  )
}


# ---- Cemented horizon in 90 % of each pedon (Spodosols) -------------

#' Pedogenically cemented horizon in a share of each pedon (Spodosols)
#'
#' v0.9.418: the Duraquods, Duricryods, Durihumods and Durorthods (KST 13th
#' ed., pp. 311, 313, 316) have, in 90 percent or more of each pedon, a
#' pedogenically cemented horizon (extremely weakly coherent or more
#' coherent) within 100 cm of the mineral soil surface. "Of each pedon" is
#' read laterally (reading L9): the share of the pedon the cemented layer
#' covers, from the recorded share (duripan_pct, filled by the ISIS reader
#' from the continuity of a duripan) or the suffix m (continuous cementation,
#' 95 %). Any recorded cementation class counts (reading L6). A cemented
#' layer whose share is not recorded leaves the test undecided (NA);
#' cementation not recorded is absent. \code{duric_subgroup_usda()} (used by
#' other orders) still reads 90 % of the thickness.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_top_cm Default 100 (from the mineral soil surface).
#' @param min_share Default 90 (percent of the pedon).
#' @return A \code{\link{DiagnosticResult}}.
#' @noRd
cemented_horizon_usda <- function(pedon, max_top_cm = 100, min_share = 90) {
  h <- pedon$horizons
  org <- .usda_organic_status(pedon)
  ms <- .usda_mineral_surface(pedon, org)
  cand <- which(!is.na(h$top_cm) & h$top_cm - ms < max_top_cm & .usda_cemented(h))
  share <- .usda_cemented_share(pedon)[cand]
  ok <- ifelse(is.na(share), NA, share >= min_share)
  passed <- .any3u(ok)
  DiagnosticResult$new(
    name = "cemented_horizon_usda", passed = passed,
    layers = cand[ok %in% TRUE],
    evidence = list(cemented_layers = cand, share_pct = share,
                    min_share = min_share, max_top_cm = max_top_cm),
    missing = if (is.na(passed)) "duripan_pct" else character(0),
    reference = "Soil Survey Staff (2022), KST 13th ed., pp. 311, 313, 316 (in 90 percent or more of each pedon)")
}


# ---- Kandic horizon helper ------------------------------------------

#' Kandic horizon (USDA, KST 13ed Ch 3, pp. 26-27)
#'
#' v0.9.309: the criteria of Keys to Soil Taxonomy (13th ed., 2022). Until
#' v0.9.308 soilKey took the WRB argic horizon plus a CEC of 16 or less per kg
#' clay, which is not the kandic horizon of the text. The kandic horizon:
#' \itemize{
#'   \item has its upper boundary where the clay increases with depth within
#'     15 cm by 3\% (absolute) or more if the overlying horizon has < 15\%
#'     clay, 20\% (relative) if 15 to 40\%, 8\% (absolute) if > 40\%; at a
#'     depth within 100 cm if the overlying horizon has 20\% clay or more,
#'     within 125 cm otherwise, or between 100 and 200 cm if the upper 100 cm
#'     is sandy throughout;
#'   \item lies under a coarser surface horizon at least 18 cm thick (5 cm
#'     if the transition is abrupt);
#'   \item is 30 cm thick or more (15 cm over a densic, lithic, paralithic
#'     or petroferric contact within 50 cm);
#'   \item is loamy very fine sand or finer;
#'   \item has an apparent CEC of 16 cmol(+) or less per kg clay (NH4OAc,
#'     pH 7) in 50\% or more of its thickness between the clay increase and
#'     100 cm below it; the clay is the measured clay or 2.5 x (water at 1500
#'     kPa - organic carbon), whichever is higher, at most 100;
#'   \item has no fine stratification (organic carbon decreasing regularly).
#' }
#' Clay films are not required.
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @noRd
kandic_horizon_usda <- function(pedon) {
  # v0.9.400: memoised within a classification (R/memo.R)
  .memo(pedon, "kandic_horizon_usda", list(),
        .kandic_horizon_usda_uncached(pedon = pedon))
}

# The computation of kandic_horizon_usda().
.kandic_horizon_usda_uncached <- function(pedon) {
  ref <- "Soil Survey Staff (2022), KST 13ed, Ch. 3, kandic horizon (pp. 26-27)"
  h <- pedon$horizons
  if (is.null(h) || nrow(h) < 2L)
    return(DiagnosticResult$new(name = "kandic_horizon_usda", passed = FALSE,
             layers = integer(0), evidence = list(reason = "fewer than two layers"),
             missing = character(0), reference = ref))
  h <- as.data.frame(h)
  o <- order(h$top_cm); idx <- seq_len(nrow(h))[o]
  top <- h$top_cm[o]; bot <- h$bottom_cm[o]; clay <- h$clay_pct[o]
  ms <- .q_mineral_surface_cm(pedon$horizons); if (is.na(ms)) ms <- 0
  w15 <- if (!is.null(h$water_content_1500kpa)) h$water_content_1500kpa[o] else rep(NA_real_, length(o))
  oc <- if (!is.null(h$oc_pct)) h$oc_pct[o] else rep(NA_real_, length(o))
  # the apparent clay of the CEC criterion
  clay_app <- pmin(100, pmax(clay, 2.5 * (w15 - ifelse(is.na(oc), 0, oc)), na.rm = TRUE))
  cec <- h$cec_cmol[o]
  sand <- if (!is.null(h$sand_pct)) h$sand_pct[o] else rep(NA_real_, length(o))
  sandy <- !is.na(sand) & !is.na(clay) & sand >= 70 & clay < 15        # loamy fine sand or coarser
  # the contact that shortens the thickness and the CEC window (the reading
  # before v0.9.315: the WRB cemented layers added then are not KST contacts)
  contact <- tryCatch(.barrier_top_cm(pedon, other_cemented = FALSE),
                      error = function(e) NA_real_)
  u100 <- which(!is.na(top) & top < ms + 100)
  upper100_sandy <- length(u100) > 0L && all(sandy[u100])
  miss <- character(0); cand_tops <- numeric(0)
  bdist <- if (!is.null(h$boundary_distinctness)) tolower(as.character(h$boundary_distinctness[o])) else rep(NA_character_, length(o))
  undecided <- function(at, what) { miss <<- c(miss, what); cand_tops <<- c(cand_tops, at) }
  for (k in seq_along(o)[-1]) {
    # the overlying horizon is mineral: a layer above the mineral soil surface
    # (organic) is not compared with the first mineral layer; and no kandic
    # horizon starts below 200 cm, so deeper layers decide nothing
    if (is.na(top[k]) || top[k] < ms || is.na(top[k - 1]) || top[k - 1] < ms) next
    if (top[k] > ms + 200) next
    # the surface horizon above is at least 18 cm thick after mixing, or 5 cm
    # if the transition is abrupt
    abrupt <- !is.na(bdist[k - 1]) && grepl("abrupt", bdist[k - 1])
    if (top[k] - ms < (if (abrupt) 5 else 18)) next
    # the increase is reached within a vertical distance of 15 cm: not across
    # a diffuse boundary (> 15 cm)
    if (!is.na(bdist[k - 1]) && grepl("diffuse", bdist[k - 1])) next
    if (is.na(clay[k]) || is.na(clay[k - 1])) { undecided(top[k], "clay_pct"); next }
    over <- clay[k - 1]
    inc <- if (over < 15) clay[k] - over >= 3 else if (over <= 40) clay[k] >= 1.2 * over
           else clay[k] - over >= 8
    if (!inc) next
    depth_ok <- if (upper100_sandy) top[k] >= ms + 100 && top[k] <= ms + 200
                else if (over >= 20) top[k] <= ms + 100 else top[k] <= ms + 125
    if (!depth_ok) next
    # the run of layers from k down that keep the increase (no drop below the overlying clay)
    run <- k
    j <- k + 1L
    # (a layer with no recorded lower boundary ends the run: what follows it
    # cannot be shown to be contiguous)
    while (j <= length(o) && !is.na(clay[j]) && clay[j] >= over && !sandy[j] &&
           !is.na(top[j]) && isTRUE(abs(top[j] - bot[run[length(run)]]) < 1e-6)) { run <- c(run, j); j <- j + 1L }
    # KST 13 p 26, item 3: "Has a thickness of either: a. 30 cm or more; or
    # b. 15 cm or more" over a contact within 50 cm. A layer of the run with
    # no recorded lower boundary (an open bottom layer, as many KSSL records
    # have) has an unknown thickness: the known layers can meet the minimum,
    # otherwise the thickness is undecided. Until v0.9.314 the sum was NA
    # and the test stopped with an error.
    thk <- bot[run] - top[run]
    thick <- sum(thk, na.rm = TRUE)
    min_thk <- if (!is.na(contact) && contact <= ms + 50) 15 else 30
    if (thick < min_thk) {
      if (anyNA(thk)) undecided(top[k], "bottom_cm")
      next
    }
    if (any(sandy[run])) next
    # CEC <= 16 per kg apparent clay in >= 50% of the thickness from the increase to 100 cm below
    lo <- top[k]; hi <- top[k] + 100
    if (!is.na(contact)) hi <- min(hi, contact)
    ov <- pmax(0, pmin(bot, hi) - pmax(top, lo)); ov[is.na(ov)] <- 0
    win <- which(ov > 0)
    ratio <- 100 * cec[win] / clay_app[win]
    ok_cm <- sum(ov[win][!is.na(ratio) & ratio <= 16]); bad_cm <- sum(ov[win][!is.na(ratio) & ratio > 16])
    na_cm <- sum(ov[win][is.na(ratio)]) + max(0, (hi - lo) - sum(ov[win]))
    half <- (hi - lo) / 2
    cec_ok <- if (ok_cm >= half - 1e-6) TRUE else if (ok_cm + na_cm < half - 1e-6) FALSE else NA
    if (is.na(cec_ok)) { undecided(top[k], "cec_cmol"); next }
    if (!cec_ok) next
    # no fine stratification: organic carbon does not rise again with depth
    oc_run <- oc[seq_len(max(run))]
    strat <- any(diff(oc_run[!is.na(oc_run)]) > 0.3)
    if (isTRUE(strat)) next
    return(DiagnosticResult$new(name = "kandic_horizon_usda", passed = TRUE,
             layers = idx[run],
             evidence = list(upper_boundary_cm = top[k], overlying_clay_pct = over,
                             thickness_cm = thick, cec_low_cm = ok_cm, window = c(lo, hi)),
             missing = character(0), reference = ref))
  }
  passed <- if (length(miss)) NA else FALSE
  DiagnosticResult$new(name = "kandic_horizon_usda", passed = passed,
    layers = integer(0), evidence = list(reason = "no layer meets the kandic criteria",
                                         candidate_tops_cm = unique(cand_tops)),
    missing = unique(miss), reference = ref)
}


# ---- Argic / Kandic combined helper for Spodosols ----------------

#' Argillic-or-Kandic helper (USDA, used in Spodosols Subgroups)
#'
#' Pass when EITHER an argillic OR a kandic horizon is present
#' within \code{max_top_cm}.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_top_cm Default 200.
#' @param min_bs Optional minimum BS for "Alfic" subgroups.
#' @return A \code{\link{DiagnosticResult}}.
#' @noRd
argillic_or_kandic_usda <- function(pedon, max_top_cm = 200,
                                         min_bs = NULL) {
  arg <- argillic_within_usda(pedon, max_top_cm = max_top_cm)
  ka  <- kandic_horizon_usda(pedon)
  passed <- isTRUE(arg$passed) || isTRUE(ka$passed)
  if (passed && !is.null(min_bs)) {
    h <- pedon$horizons
    layers <- unique(c(arg$layers, ka$layers))
    bs <- h$bs_pct[layers]
    bs_ok <- any(!is.na(bs) & bs >= min_bs)
    passed <- isTRUE(bs_ok)
    if (!passed) {
      return(DiagnosticResult$new(
        name = "argillic_or_kandic_usda", passed = FALSE,
        layers = integer(0),
        evidence = list(argillic = arg, kandic = ka,
                          min_bs = min_bs),
        missing = character(0),
        reference = "Soil Survey Staff (2022), KST 13ed, Ch. 14"
      ))
    }
  }
  DiagnosticResult$new(
    name = "argillic_or_kandic_usda", passed = passed,
    layers = if (passed) unique(c(arg$layers, ka$layers))
             else integer(0),
    evidence = list(argillic = arg, kandic = ka, min_bs = min_bs),
    missing = unique(c(arg$missing %||% character(0),
                          ka$missing %||% character(0))),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 14"
  )
}


#' Alfic Subgroup helper (Spodosols): argillic or kandic with BS >= 35\%
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
alfic_subgroup_usda <- function(pedon) {
  res <- argillic_or_kandic_usda(pedon, max_top_cm = 200, min_bs = 35)
  res$name <- "alfic_subgroup_usda"
  res
}


#' Ultic Subgroup helper: argillic or kandic (any BS).
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
ultic_subgroup_usda <- function(pedon) {
  res <- argillic_or_kandic_usda(pedon, max_top_cm = 200)
  res$name <- "ultic_subgroup_usda"
  res
}


#' Argic Subgroup helper (Endoaquods/Fragiaquods): argillic or kandic.
#' Synonym of ultic at this level. Re-exported for naming clarity.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
argic_subgroup_usda <- function(pedon) {
  res <- argillic_or_kandic_usda(pedon, max_top_cm = 200)
  res$name <- "argic_subgroup_usda"
  res
}


# ---- Arenic / Grossarenic Subgroup helpers --------------------------

#' Arenic / Grossarenic Subgroup helper (Spodosols)
#'
#' Pass when texture class (fine-earth fraction) is sandy
#' throughout from the surface to the top of the spodic horizon
#' AND the spodic top depth falls in
#' \code{[min_spodic_top, max_spodic_top]}.
#'
#' Standard cuts:
#' - "Arenic":      75-125 cm
#' - "Grossarenic": 125+ cm (use min_spodic_top=125, max=Inf)
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_spodic_top Default 75.
#' @param max_spodic_top Default 125.
#' @return A \code{\link{DiagnosticResult}}.
#' @noRd
arenic_subgroup_usda <- function(pedon,
                                      min_spodic_top = 75,
                                      max_spodic_top = 125) {
  sp <- spodic_horizon_usda(pedon)
  if (!isTRUE(sp$passed) || length(sp$layers) == 0L) {
    return(DiagnosticResult$new(
      name = "arenic_subgroup_usda", passed = FALSE,
      layers = integer(0),
      evidence = list(spodic = sp,
                        reason = "no spodic horizon"),
      missing = sp$missing,
      reference = "Soil Survey Staff (2022), KST 13ed, Ch. 14"
    ))
  }
  h <- pedon$horizons
  spodic_top <- min(h$top_cm[sp$layers], na.rm = TRUE)
  depth_ok <- spodic_top >= min_spodic_top &&
                spodic_top <= max_spodic_top
  # All horizons above spodic must be sandy
  above <- which(!is.na(h$top_cm) & h$top_cm < spodic_top)
  texture_ok <- TRUE
  for (i in above) {
    cl <- h$clay_pct[i]; sd <- h$sand_pct[i]
    if (is.na(cl) || is.na(sd)) next
    is_sandy <- !isTRUE(.is_finer_than_loamy_fine_sand(cl, h$silt_pct[i],
                                                          sd))
    if (!is_sandy) {
      texture_ok <- FALSE
      break
    }
  }
  passed <- isTRUE(depth_ok) && isTRUE(texture_ok)
  DiagnosticResult$new(
    name = "arenic_subgroup_usda", passed = passed,
    layers = if (passed) above else integer(0),
    evidence = list(spodic_top_cm = spodic_top,
                      depth_ok = depth_ok,
                      texture_ok = texture_ok,
                      min_spodic_top = min_spodic_top,
                      max_spodic_top = max_spodic_top),
    missing = character(0),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 14"
  )
}


#' Grossarenic Subgroup helper: sandy throughout, spodic >= 125 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
grossarenic_subgroup_usda <- function(pedon) {
  res <- arenic_subgroup_usda(pedon, min_spodic_top = 125,
                                  max_spodic_top = Inf)
  res$name <- "grossarenic_subgroup_usda"
  res
}


# ---- Entic / Aeric Subgroup helpers --------------------------------

#' Entic Subgroup helper (Spodosols, KST 13th ed., pp. 312-320)
#'
#' v0.9.418: the KST has four Entic definitions in the Spodosols, chosen by
#' \code{rule}:
#' \describe{
#'   \item{\code{"cryaquod"}}{Entic Cryaquods (p. 312): a spodic horizon less
#'     than 10 cm thick (in 50 percent or more of each pedon).}
#'   \item{\code{"haplocryod"}}{Entic Haplocryods (p. 314): less than 1.2
#'     percent organic carbon in the upper 10 cm of the spodic horizon.}
#'   \item{\code{"alorthod"}}{Entic Alorthods and Entic Grossarenic Alorthods
#'     (p. 317): less than 3.0 percent organic carbon in the upper 2 cm of
#'     the spodic horizon (in 10 percent or more of each pedon; the profile
#'     described is taken as that part).}
#'   \item{\code{"orthod"}}{Entic Fragiorthods, Entic Lithic, Aquentic and
#'     Entic Haplorthods (pp. 318-320): a spodic horizon of very fine sand,
#'     loamy very fine sand or finer, 10 cm thick or less, with less than 1.2
#'     percent organic carbon (weighted average) and a moist value or chroma
#'     of 4 or more within its upper 7.5 cm; or a spodic horizon of loamy fine
#'     sand or coarser with a moist value or chroma of 4 or more in its upper
#'     2.5 cm.}
#' }
#' With \code{rule = NULL} (the subgroup lists of 0.9.418 pass none) the rule
#' of the great group is taken from the profile when it has a spodic horizon
#' (or one that is undecided): a cryic regime with Aquods conditions is
#' "cryaquod", a cryic regime "haplocryod", a fragipan "orthod", less than
#' 0.10 percent oxalate iron in the spodic horizon "alorthod", and otherwise
#' "orthod". A profile with no spodic horizon (the Entic subgroups of other
#' orders) keeps the reading of v0.9.417: a spodic-like horizon under 10 cm
#' thick or under 1.2 percent organic carbon at its top. Up to v0.9.417 that
#' reading served every Spodosol.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param rule \code{NULL} or one of the four rules above.
#' @return A \code{\link{DiagnosticResult}}.
#' @noRd
entic_subgroup_usda <- function(pedon, rule = NULL) {
  ref <- "Soil Survey Staff (2022), KST 13th ed., pp. 312-320 (Entic subgroups of Spodosols)"
  sl <- .spodic_layers_for_tests(pedon)
  if (is.null(rule)) {
    if (isFALSE(sl$sp$passed)) return(.entic_subgroup_legacy(pedon))
    rule <- .entic_rule_usda(pedon)
  }
  rule <- match.arg(rule, c("cryaquod", "haplocryod", "alorthod", "orthod"))
  h <- pedon$horizons
  L <- sl$layers
  if (!length(L)) {
    return(DiagnosticResult$new(name = "entic_subgroup_usda", passed = FALSE,
                                layers = integer(0),
                                evidence = list(rule = rule, reason = "no spodic horizon"),
                                missing = character(0), reference = ref))
  }
  L <- L[order(h$top_cm[L])]
  top <- h$top_cm[L[1L]]
  thk <- sum(h$bottom_cm[L] - h$top_cm[L], na.rm = TRUE)
  oc <- suppressWarnings(as.numeric(h$oc_pct))
  v <- suppressWarnings(as.numeric(h$munsell_value_moist))
  cr <- suppressWarnings(as.numeric(h$munsell_chroma_moist))
  # weighted organic carbon of the spodic horizon within `depth` cm of its top
  oc_upper <- function(depth) {
    w <- pmax(pmin(h$bottom_cm[L], top + depth) - h$top_cm[L], 0)
    k <- L[w > 0]; w <- w[w > 0]
    if (anyNA(oc[k])) return(NA_real_)
    sum(oc[k] * w) / sum(w)
  }
  upper_layers <- function(depth) L[h$top_cm[L] < top + depth]
  bright <- function(k) .all3u(v[k] >= 4 | cr[k] >= 4)
  passed <- switch(rule,
    cryaquod = thk < 10,
    haplocryod = { x <- oc_upper(10); if (is.na(x)) NA else x < 1.2 },
    alorthod = { x <- oc_upper(2); if (is.na(x)) NA else x < 3.0 },
    orthod = {
      sandy <- .usda_lfs_or_coarser(h)[L[1L]]
      fine_case <- {
        x <- oc_upper(thk)
        (thk <= 10) & (if (is.na(x)) NA else x < 1.2) & bright(upper_layers(7.5))
      }
      coarse_case <- bright(upper_layers(2.5))
      if (is.na(sandy)) {
        if (isTRUE(fine_case) && isTRUE(coarse_case)) TRUE
        else if (isFALSE(fine_case) && isFALSE(coarse_case)) FALSE else NA
      } else if (sandy) coarse_case else fine_case
    })
  if (!sl$sure && isTRUE(passed)) passed <- NA
  DiagnosticResult$new(
    name = "entic_subgroup_usda", passed = passed, layers = L,
    evidence = list(rule = rule, spodic_thickness_cm = thk, spodic_top_cm = top),
    missing = if (is.na(passed)) c("oc_pct", "munsell_value_moist", "munsell_chroma_moist") else character(0),
    reference = ref)
}

# The Spodosol great group whose Entic definition applies (see
# entic_subgroup_usda()).
.entic_rule_usda <- function(pedon) {
  if (isTRUE(soil_temperature_regime_usda(pedon, "cryic")$passed))
    return(if (isTRUE(aquod_qualifying_usda(pedon)$passed)) "cryaquod" else "haplocryod")
  if (isTRUE(fragipan_usda(pedon)$passed)) return("orthod")
  if (isTRUE(al_rich_spodic_usda(pedon, fe_only = TRUE)$passed)) return("alorthod")
  "orthod"
}

# The reading of v0.9.417 (WRB spodic horizon under 10 cm thick, or under
# 1.2 % organic carbon at its top), kept for the Entic subgroups of other
# orders, whose lists call this helper.
.entic_subgroup_legacy <- function(pedon) {
  sp <- spodic(pedon)
  h <- pedon$horizons
  if (!isTRUE(sp$passed) || length(sp$layers) == 0L) {
    return(DiagnosticResult$new(
      name = "entic_subgroup_usda", passed = FALSE, layers = integer(0),
      evidence = list(reason = "no spodic horizon"),
      missing = character(0),
      reference = "Soil Survey Staff (2022), KST 13ed, Ch. 14"
    ))
  }
  thk <- sum(pmax(h$bottom_cm[sp$layers] - h$top_cm[sp$layers], 0), na.rm = TRUE)
  upper <- sp$layers[order(h$top_cm[sp$layers])][1]
  oc_upper <- h$oc_pct[upper]
  thin <- thk < 10
  oc_low <- !is.na(oc_upper) && oc_upper < 1.2
  passed <- isTRUE(thin) || isTRUE(oc_low)
  DiagnosticResult$new(
    name = "entic_subgroup_usda", passed = passed, layers = sp$layers,
    evidence = list(spodic_thickness_cm = thk, oc_upper_pct = oc_upper,
                    thin = thin, oc_low = oc_low),
    missing = character(0),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 14"
  )
}


#' Aeric Subgroup helper (Aquods)
#' Pass when ochric epipedon is present (vs. histic/umbric/etc).
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
aeric_subgroup_usda <- function(pedon) {
  res <- ochric_epipedon_usda(pedon)
  res$name <- "aeric_subgroup_usda"
  res
}


#' Histic Subgroup helper (in Spodosols, Aquods)
#' Pass when histic_epipedon_usda passes.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
histic_subgroup_usda <- function(pedon) {
  res <- histic_epipedon_usda(pedon)
  res$name <- "histic_subgroup_usda"
  res
}


#' Umbric Subgroup helper (in Spodosols)
#' Pass when umbric_epipedon_usda passes.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
umbric_subgroup_usda <- function(pedon) {
  res <- umbric_epipedon_usda(pedon)
  res$name <- "umbric_subgroup_usda"
  res
}


# ---- Oxyaquic Subgroup helper ---------------------------------------

#' Oxyaquic Subgroup helper (all orders)
#'
#' KST 13th ed. (e.g. p. 178): saturation with water in one or more layers
#' within \code{max_depth_cm} (100 cm in most great groups) of the mineral
#' soil surface for 20 or more consecutive days or 30 or more cumulative
#' days in normal years. A measurement of the water regime, read from
#' \code{water_saturation_days} (cumulative days a year): 30 or more in a
#' layer within the depth passes; under 20 in every layer within it fails
#' (fewer cumulative days cannot hold 20 consecutive ones); and from
#' \code{site$flooding_days_growing_period}, 30 days or more of standing
#' water on the surface. With no saturation record the test is undecided
#' (audit R2, reading L17): redox features belong to the Aquic subgroups,
#' and the drainage class is not a measure of the days of saturation.
#' v0.9.419: up to v0.9.418 redox features of 2 \% or more with chroma of
#' 4 or less (or a g designation with chroma of 3 or less) within 100 cm
#' passed, on 96 ISIS profiles, 26 of them well drained.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_depth_cm Depth (cm below the mineral soil surface) within
#'   which the saturation is read.
#' @return A \code{\link{DiagnosticResult}}.
#' @noRd
oxyaquic_subgroup_usda <- function(pedon, max_depth_cm = 100) {
  ref <- "Soil Survey Staff (2022), KST 13th ed., Oxyaquic subgroups (e.g. p. 178): saturation 20 consecutive or 30 cumulative days"
  h <- pedon$horizons
  ms <- .mineral_soil_surface_cm(h); if (is.na(ms)) ms <- 0
  cand <- which(!is.na(h$top_cm) & h$top_cm < ms + max_depth_cm & h$bottom_cm > ms)
  sd <- .usda_col(h, "water_saturation_days", NA_real_)[cand]
  fl <- suppressWarnings(as.numeric(pedon$site$flooding_days_growing_period %||% NA_real_))[1]
  passed <- if (any(sd >= 30, na.rm = TRUE) || (!is.na(fl) && fl >= 30)) TRUE
            else if (length(cand) && !anyNA(sd) && all(sd < 20)) FALSE
            else NA
  DiagnosticResult$new(
    name = "oxyaquic_subgroup_usda", passed = passed,
    layers = if (isTRUE(passed)) cand[!is.na(sd) & sd >= 30] else integer(0),
    evidence = list(water_saturation_days = sd, flooding_days = fl,
                    max_depth_cm = max_depth_cm),
    missing = if (is.na(passed)) "water_saturation_days" else character(0),
    reference = ref
  )
}


# ---- Compound Subgroup helpers (combinations) -----------------------

#' Aquandic Subgroup helper (USDA, KST 13th ed., e.g. p. 225)
#'
#' The aquic conditions of the subgroup (\code{aquic_subgroup_usda()}) and,
#' throughout one or more horizons with a total thickness of 18 cm or more
#' within 75 cm of the mineral soil surface, the andic or the vitrandic
#' criteria (e.g. Aquandic Dystrudepts, p. 225). v0.9.417: the vitrandic
#' branch (up to v0.9.416 only the andic one), and three-valued.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
aquandic_subgroup_usda <- function(pedon) {
  aq <- aquic_subgroup_usda(pedon)
  h <- pedon$horizons
  L <- if (nrow(h)) .andic_subgroup_layers_usda(h) else NULL
  an <- if (nrow(h)) .andic_subgroup_18_usda(h, L$andic | L$vitrandic)
        else list(passed = FALSE, layers = integer(0))
  passed <- aq$passed & an$passed
  DiagnosticResult$new(
    name = "aquandic_subgroup_usda", passed = passed,
    layers = if (isTRUE(passed)) union(aq$layers, an$layers) else integer(0),
    evidence = list(aquic = aq, andic_or_vitrandic = an),
    missing = if (is.na(passed))
                unique(c(if (is.na(aq$passed)) aq$missing %||% character(0),
                         if (is.na(an$passed)) c(L$missing_andic, L$missing_vitrandic)))
              else character(0),
    reference = "Soil Survey Staff (2022), KST 13th ed., p. 225 (e.g. Aquandic Dystrudepts)"
  )
}


# ---- Gelic Materials / Turbic helper --------------------------------

#' Turbic Subgroup helper (Gelods)
#' Pass when gelic materials are present within 200 cm.
#' Implementation: cryoturbation + permafrost within 200 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
turbic_subgroup_usda <- function(pedon) {
  cr <- cryoturbation_usda(pedon)
  pf <- permafrost_within_usda(pedon, max_top_cm = 200)
  passed <- isTRUE(cr$passed) || isTRUE(pf$passed)
  DiagnosticResult$new(
    name = "turbic_subgroup_usda", passed = passed,
    layers = c(cr$layers, pf$layers),
    evidence = list(cryoturbation = cr, permafrost = pf),
    missing = pf$missing,
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 14"
  )
}


# ---- Gelic STR helper -----------------------------------------------

#' Gelic soil temperature regime (USDA, KST 13th ed., p. 53)
#'
#' v0.9.415: read from \code{site$soil_temperature_regime} like every other
#' regime test: TRUE for a recorded gelic (or pergelic) regime, FALSE for
#' another recorded regime. With none recorded, permafrost within 100 cm
#' (a layer at or below 0 C, or a frozen designation) still passes, since the
#' soil is then frozen; otherwise the test is undecided (NA) and lists
#' \code{site$soil_temperature_regime}. Up to v0.9.414 it read permafrost
#' only and was FALSE whenever none was recorded.
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @noRd
str_gelic_usda <- function(pedon) {
  st <- soil_temperature_regime_usda(pedon, "gelic")
  pf <- permafrost_within_usda(pedon, max_top_cm = 100)
  passed <- if (!is.na(st$passed)) st$passed else if (isTRUE(pf$passed)) TRUE else NA
  DiagnosticResult$new(
    name = "str_gelic_usda", passed = passed,
    layers = if (isTRUE(passed)) pf$layers else integer(0),
    evidence = list(temperature_regime = st, permafrost = pf),
    missing = if (is.na(passed)) st$missing else character(0),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 3, p 53"
  )
}


# ---- Lamellic Subgroup helper ---------------------------------------

#' Lamellic Subgroup helper (Spodosols Haplorthods)
#'
#' Pass when 2+ lamellae (clay-rich bands < 7.5 cm thick) are
#' present below the spodic horizon. v0.8: detected via designation
#' containing "&" (lamella notation in KST) OR multiple thin
#' clay-bumps in clay_pct.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @noRd
lamellic_subgroup_usda <- function(pedon) {
  h <- pedon$horizons
  des_lam <- which(!is.na(h$designation) &
                       grepl("&", h$designation, fixed = TRUE))
  passed <- length(des_lam) >= 2L
  DiagnosticResult$new(
    name = "lamellic_subgroup_usda", passed = passed,
    layers = des_lam,
    evidence = list(note = "v0.8: detected via '&' in designation"),
    missing = character(0),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 3, pp 35-36"
  )
}


# ---- Folistic Subgroup helper (already defined; re-exported) -------
# folistic_subgroup_usda is in diagnostics-gelisols-usda.R


# ---- Andic Subgroup helper (already in diagnostics-gelisols-usda.R)
# andic_subgroup_usda is reused.

# ---- Aquic Subgroup helper (already defined) -----------------------
# aquic_subgroup_usda is reused.


# ---- Aluminum-rich Spodic check (Alaquods, Alorthods) -------------

#' Aluminum-rich spodic horizon (Alaquods, Alorthods; KST 13th ed.)
#'
#' Alaquods (p. 311): less than 0.10 percent oxalate iron, or at least 3
#' times as much oxalate aluminum as iron, in 75 percent or more of the
#' spodic horizon. Alorthods (p. 317): less than 0.10 percent oxalate iron
#' in 75 percent or more of the spodic horizon (\code{fe_only = TRUE}).
#' v0.9.418: the share is of the thickness of the spodic horizon, and the
#' Alorthods no longer take the aluminum alternative; a layer with the
#' oxalate extract not recorded is undecided, and the test is NA when the
#' undecided layers could reach 75 percent.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param fe_only \code{TRUE} for the Alorthods.
#' @return A \code{\link{DiagnosticResult}}.
#' @noRd
al_rich_spodic_usda <- function(pedon, fe_only = FALSE) {
  ref <- if (fe_only) "Soil Survey Staff (2022), KST 13th ed., p. 317 (Alorthods)"
         else "Soil Survey Staff (2022), KST 13th ed., p. 311 (Alaquods)"
  sl <- .spodic_layers_for_tests(pedon)
  if (!length(sl$layers)) {
    return(DiagnosticResult$new(
      name = "al_rich_spodic_usda", passed = FALSE, layers = integer(0),
      evidence = list(reason = "no spodic horizon"),
      missing = character(0), reference = ref))
  }
  h <- pedon$horizons
  L <- sl$layers
  fe <- suppressWarnings(as.numeric(h$fe_ox_pct[L]))
  al <- suppressWarnings(as.numeric(h$al_ox_pct[L]))
  ok <- fe < 0.10
  if (!fe_only) ok <- ok | al >= 3 * fe
  w <- pmax(h$bottom_cm[L] - h$top_cm[L], 0)
  passed <- .share3(ok, w, 0.75)
  if (!sl$sure && isTRUE(passed)) passed <- NA
  DiagnosticResult$new(
    name = "al_rich_spodic_usda", passed = passed, layers = L,
    evidence = list(fe_pct = fe, al_pct = al, layer_ok = ok, fe_only = fe_only),
    missing = if (is.na(passed)) c(if (anyNA(fe)) "fe_ox_pct", if (!fe_only && anyNA(al)) "al_ox_pct",
                                   if (!sl$sure) sl$sp$missing) else character(0),
    reference = ref)
}


# ---- Humic-spodic check (Humods, Humicryods) ------------------------

#' Humic spodic horizon (Humods, Humigelods, Humicryods; KST 13th ed.,
#' pp. 311, 313, 315)
#'
#' 6.0 percent or more organic carbon in a layer 10 cm or more thick within
#' the spodic horizon. v0.9.418: adjoining layers of the spodic horizon count
#' together, and organic carbon not recorded leaves the test undecided.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
humic_spodic_usda <- function(pedon) {
  ref <- "Soil Survey Staff (2022), KST 13th ed., p. 311 (Humods)"
  sl <- .spodic_layers_for_tests(pedon)
  if (!length(sl$layers)) {
    return(DiagnosticResult$new(
      name = "humic_spodic_usda", passed = FALSE, layers = integer(0),
      evidence = list(reason = "no spodic horizon"),
      missing = character(0), reference = ref))
  }
  h <- pedon$horizons
  L <- sl$layers[order(h$top_cm[sl$layers])]
  oc <- suppressWarnings(as.numeric(h$oc_pct[L]))
  rich <- oc >= 6
  run_thk <- function(ok) {
    best <- 0; cur <- 0; bot <- NA_real_
    for (k in seq_along(L)) {
      i <- L[k]
      if (ok[k]) {
        cur <- if (cur > 0 && !is.na(bot) && abs(h$top_cm[i] - bot) < 1e-6)
                 cur + (h$bottom_cm[i] - h$top_cm[i]) else (h$bottom_cm[i] - h$top_cm[i])
        bot <- h$bottom_cm[i]; best <- max(best, cur)
      } else cur <- 0
    }
    best
  }
  yes <- run_thk(rich %in% TRUE); may <- run_thk(!(rich %in% FALSE))
  passed <- if (yes >= 10) TRUE else if (may < 10) FALSE else NA
  if (!sl$sure && isTRUE(passed)) passed <- NA
  DiagnosticResult$new(
    name = "humic_spodic_usda", passed = passed,
    layers = if (isTRUE(passed)) L[rich %in% TRUE] else integer(0),
    evidence = list(oc_pct = oc, rich_thickness_cm = yes),
    missing = if (is.na(passed)) "oc_pct" else character(0),
    reference = ref)
}


#' Humic Subgroup helper (Humic Duricryods / Humic Placocryods)
#' Pass when spodic horizon has >= 6\% OC in 10+ cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
humic_subgroup_usda <- function(pedon) {
  res <- humic_spodic_usda(pedon)
  res$name <- "humic_subgroup_usda"
  res
}


# ---- Aquods (KST 13th ed., p. 311) ------------------------------------

#' Aquods suborder qualifier (KST 13th ed., p. 311)
#'
#' v0.9.418: aquic conditions (or artificial drainage) in one or more
#' horizons within 50 cm of the mineral soil surface, and either a histic
#' epipedon or, within 50 cm of the mineral soil surface, aquic conditions in
#' an albic or a spodic horizon. Up to v0.9.417 the first part alone decided.
#' Aquic conditions are read by \code{aquic_conditions_usda()} (redoximorphic
#' features and low chroma; the drainage class is not read, reading L12).
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @noRd
aquod_qualifying_usda <- function(pedon) {
  h <- pedon$horizons
  org <- .usda_organic_status(pedon)
  ms <- .usda_mineral_surface(pedon, org)
  aq <- aquic_conditions_usda(pedon, max_top_cm = ms + 50)
  drained <- .wrb_site_drained(pedon)
  aq_any <- isTRUE(aq$passed) || drained
  if (!aq_any) {
    return(DiagnosticResult$new(
      name = "aquod_qualifying_usda", passed = FALSE, layers = integer(0),
      evidence = list(aquic = aq), missing = aq$missing,
      reference = "Soil Survey Staff (2022), KST 13th ed., p. 311"))
  }
  hi <- histic_epipedon_usda(pedon)
  sp <- spodic_horizon_usda(pedon)
  alb <- sp$evidence$albic_horizon
  host <- (alb %in% TRUE) | seq_len(nrow(h)) %in% sp$layers
  host_may <- host | is.na(alb) | seq_len(nrow(h)) %in% sp$evidence$maybe_layers
  in50 <- !is.na(h$top_cm) & h$top_cm - ms < 50
  item2 <- if (any(aq$layers %in% which(host & in50))) TRUE
           else if (any(aq$layers %in% which(host_may & in50))) NA else FALSE
  passed <- hi$passed | item2
  DiagnosticResult$new(
    name = "aquod_qualifying_usda", passed = passed,
    layers = intersect(aq$layers, which(host_may & in50)),
    evidence = list(aquic = aq, drained = drained, histic = hi, aquic_in_albic_or_spodic = item2),
    missing = if (is.na(passed)) unique(c(hi$missing, sp$missing)) else character(0),
    reference = "Soil Survey Staff (2022), KST 13th ed., p. 311")
}


# ---- Episaturation / Endosaturation ---------------------------------

#' Episaturation helper (USDA, KST 13ed Ch 3, p 41)
#' Pass when aquic conditions PLUS perched water (saturation type
#' "episaturation").
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
episaturation_usda <- function(pedon) {
  aq <- aquic_conditions_usda(pedon, max_top_cm = 200)
  passed <- isTRUE(aq$passed) &&
              identical(aq$evidence$saturation_type, "episaturation")
  DiagnosticResult$new(
    name = "episaturation_usda", passed = passed,
    layers = aq$layers,
    evidence = list(aquic = aq),
    missing = aq$missing,
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 3, p 41"
  )
}
