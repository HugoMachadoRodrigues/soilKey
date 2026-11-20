# =============================================================
# USDA Soil Taxonomy 13ed -- Inceptisols helpers (Cap 11, pp 207-246)
# =============================================================
#
# Inceptisols are weakly developed soils: item K of the Key to Soil Orders
# (KST 13th ed., p. 70), mostly through the cambic horizon (pp. 22-23).
# 6 Suborders.
#
# v0.9.419 (Soil Taxonomy audit, section 2.3): the cambic horizon is a USDA
# function of its own (cambic_horizon_usda), no longer the WRB reading;
# item K is read in full; the Aquepts, Halaquepts and Eutrudepts follow
# their key text. All three-valued: NA (with the missing attributes) when
# the missing data could change the answer.
# =============================================================


# ---- three-valued helpers shared by the Inceptisol, Entisol and Aridisol
# ---- tests of v0.9.419 ------------------------------------------------------

# Any TRUE -> TRUE; otherwise any NA -> NA; otherwise FALSE (FALSE when empty).
.usda_any3 <- function(v) {
  v <- as.logical(unlist(v))
  if (any(v %in% TRUE)) TRUE else if (anyNA(v)) NA else FALSE
}

# Any FALSE -> FALSE; otherwise any NA -> NA; otherwise TRUE (TRUE when empty).
.usda_all3 <- function(v) {
  v <- as.logical(unlist(v))
  if (any(v %in% FALSE)) FALSE else if (anyNA(v)) NA else TRUE
}

.usda_not3 <- function(x) if (is.na(x)) NA else !x

# One TRUE / FALSE / NA from a DiagnosticResult (NA when it errs or carries no
# single logical).
.usda_pv <- function(r) {
  if (is.null(r)) return(NA)
  v <- tryCatch(r$passed, error = function(e) NA)
  if (length(v) != 1L || !is.logical(v)) NA else v
}

# Call a diagnostic, NULL when it errs (so a failing helper leaves the test
# undecided instead of stopping the key).
.usda_try <- function(expr) tryCatch(expr, error = function(e) NULL)

# A horizon column, or NA of the right length when the column is absent.
.usda_col <- function(h, col, na = NA) {
  x <- h[[col]]
  if (is.null(x)) rep(na, nrow(h)) else x
}

# Parts of a KST horizon designation: discontinuity numbers and the caret of
# human-transported material removed; master = the capital letters (with /),
# suffix = the lower-case letters after them.
.usda_desig <- function(des) {
  des <- as.character(des)
  core <- sub("^\\^?[0-9]*\\^?", "", des)
  master <- ifelse(is.na(core), NA_character_,
                   sub("^([A-Z/]*).*$", "\\1", core))
  suffix <- ifelse(is.na(core), NA_character_,
                   gsub("[^a-z]", "", sub("^[A-Z/]*", "", core)))
  list(core = core, master = master, suffix = suffix,
       caret = !is.na(des) & grepl("^[0-9]*\\^", des))
}

# Top (cm) of the shallowest densic, lithic or paralithic contact recorded in
# the designations (R; Cr; a C with d), NA when none. A field observation.
.usda_contact_top_cm <- function(h) {
  d <- .usda_desig(h$designation)
  hit <- !is.na(d$master) &
    (d$master == "R" | (d$master == "C" & grepl("[rd]", d$suffix)))
  if (!any(hit)) return(NA_real_)
  min(h$top_cm[hit], na.rm = TRUE)
}

# Exchangeable sodium percentage of each layer: esp_pct, or Na / CEC x 100.
.usda_esp <- function(h) {
  esp <- .usda_col(h, "esp_pct", NA_real_)
  na <- .usda_col(h, "na_cmol", NA_real_); cec <- .usda_col(h, "cec_cmol", NA_real_)
  calc <- ifelse(!is.na(na) & !is.na(cec) & cec > 0, 100 * na / cec, NA_real_)
  ifelse(is.na(esp), calc, esp)
}

# Lab texture of each layer as "S" (sand), "LS" (loamy sand) or "finer"; NA
# when clay and sand (or silt) are not both known.
.usda_lab_sandiness <- function(h) {
  cl <- h$clay_pct; sa <- h$sand_pct; si <- h$silt_pct
  si <- ifelse(is.na(si) & !is.na(cl) & !is.na(sa), 100 - cl - sa, si)
  out <- rep(NA_character_, nrow(h))
  ok <- !is.na(cl) & !is.na(si)
  out[ok] <- ifelse(si[ok] + 1.5 * cl[ok] < 15, "S",
             ifelse(si[ok] + 2 * cl[ok] < 30, "LS", "finer"))
  out
}

# "Very fine sand, loamy very fine sand, or finer" (the KST cambic, p. 22) and
# "loamy fine sand or coarser" (the Psamments, p. 165), layer by layer,
# TRUE / FALSE / NA. The laboratory texture decides where clay and sand are
# measured; a laboratory sand or loamy sand is of no known sand grade, and
# is read as coarser than the very fine grade unless the field texture class
# (texture_class_usda, read by .texture_class_reading()) names a very fine
# sand or loamy very fine sand. Without the laboratory, clay over 15 % or
# silt of 30 % or more is finer than a loamy sand, and otherwise the field
# texture class decides; nothing recorded, NA.
.usda_texture_grades <- function(h) {
  n <- nrow(h)
  lab <- .usda_lab_sandiness(h)
  tc <- .usda_col(h, "texture_class_usda", NA_character_)
  fr <- .texture_class_reading(tc)
  cl <- h$clay_pct; si <- h$silt_pct
  vfs <- rep(NA, n); lfs <- rep(NA, n)
  for (i in seq_len(n)) {
    fvfs <- !is.na(fr$class[i]) && fr$class[i] %in% c("VFS", "LVFS")
    if (!is.na(lab[i])) {
      if (lab[i] == "finer") { vfs[i] <- TRUE; lfs[i] <- FALSE }
      else { vfs[i] <- fvfs; lfs[i] <- !fvfs }
    } else if ((!is.na(cl[i]) && cl[i] > 15) || (!is.na(si[i]) && si[i] >= 30)) {
      vfs[i] <- TRUE; lfs[i] <- FALSE
    } else if (!is.na(fr$class[i])) {
      vfs[i] <- fr$vfs_or_finer[i]; lfs[i] <- fr$lfs_or_coarser[i]
    }
  }
  list(vfs_or_finer = vfs, lfs_or_coarser = lfs)
}

# Hue as a number on the Munsell circle (R = 0, YR = 10, Y = 20 ...), NA for
# neutral or unreadable hues: lower is redder.
.usda_hue_rank <- function(hue) {
  hue <- toupper(gsub(" ", "", as.character(hue)))
  out <- rep(NA_real_, length(hue))
  m <- regmatches(hue, regexec("^([0-9.]+)(R|YR|Y|GY|G|BG|B|PB|P|RP)$", hue))
  base <- c(RP = -10, R = 0, YR = 10, Y = 20, GY = 30, G = 40, BG = 50,
            B = 60, PB = 70, P = 80)
  for (i in seq_along(m)) if (length(m[[i]]) == 3L)
    out[i] <- base[[m[[i]][3]]] + as.numeric(m[[i]][2])
  out
}

# Runs of adjoining layers (in depth order) whose flag is TRUE, as a list of
# index vectors.
.usda_runs <- function(ok, ord) {
  out <- list(); cur <- integer(0)
  for (i in c(ord, NA_integer_)) {
    if (!is.na(i) && isTRUE(ok[i])) cur <- c(cur, i)
    else { if (length(cur)) out[[length(out) + 1L]] <- cur; cur <- integer(0) }
  }
  out
}

.usda_run_thickness <- function(h, r) {
  sum(pmax(h$bottom_cm[r] - h$top_cm[r], 0), na.rm = TRUE)
}

# Does a TRUE result have a layer starting above `depth` cm? TRUE / FALSE /
# NA; a TRUE result that names no layer counts as within.
.usda_within3 <- function(r, h, depth) {
  v <- .usda_pv(r)
  if (!isTRUE(v)) return(v)
  l <- r$layers
  if (!length(l)) return(TRUE)
  any(h$top_cm[l] < depth, na.rm = TRUE)
}


# ---- the cambic horizon (KST 13th ed., pp. 22-23) --------------------------

#' Cambic horizon (USDA Soil Taxonomy, KST 13th ed., pp. 22-23)
#'
#' The cambic horizon of the Keys to Soil Taxonomy, read criterion by
#' criterion and layer by layer, three-valued. Until v0.9.418 the USDA key
#' borrowed the WRB reading (\code{.cambic_impl()}); the WRB side is
#' unchanged.
#' \enumerate{
#'   \item 15 cm or more thick: adjoining layers that meet 2 to 6.
#'   \item A texture class of very fine sand, loamy very fine sand or finer
#'     (laboratory texture, else the field texture class).
#'   \item Soil structure or the absence of rock structure in more than half
#'     the volume: a recorded structure, massive and single grain included,
#'     passes; rock structure or fine stratification fails; structure not
#'     recorded is undecided.
#'   \item Evidence of alteration: (a) aquic conditions within 50 cm (or
#'     artificial drainage) and the colours of item 4a(2); or (b) not that
#'     combination, soil structure or the absence of rock structure, and a
#'     higher chroma, higher value, redder hue or more clay than the
#'     underlying horizon or an overlying one (an A horizon included, as
#'     printed), or evidence of the removal of carbonates or gypsum (less
#'     than half the carbonate, or gypsum, of a layer below that has 1 \% or
#'     more). Redox concentrations are a field observation: not recorded,
#'     absent.
#'   \item Not part of an anthropic, histic, folistic, melanic, mollic,
#'     plaggen or umbric epipedon, a duripan or fragipan, or an argillic,
#'     calcic, gypsic, natric, oxic, petrocalcic, petrogypsic, placic,
#'     salic, spodic or sulfuric horizon (the layers of each that is
#'     present).
#'   \item Not part of an Ap horizon (brittleness is not recorded: absent).
#' }
#' The horizon is an altered B: a layer designated E or C is not cambic (the
#' C is unaltered material), nor an A in a soil with an Ap horizon; in an
#' unplowed soil an A is read by its properties (reading N11); transitional
#' horizons with a B (AB, BC, CB ...) and undesignated layers are read by
#' their properties. Organic layers and R and Cr layers are not soil
#' material for it.
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}}; \code{evidence$layer} holds the
#'   reading of each layer and \code{evidence$runs_yes} /
#'   \code{evidence$runs_may} the runs of 15 cm or more that pass or may pass.
#' @references Soil Survey Staff (2022), Keys to Soil Taxonomy, 13th ed.,
#'   Ch. 3, cambic horizon, pp. 22-23.
#' @noRd
cambic_horizon_usda <- function(pedon) {
  .memo(pedon, "cambic_horizon_usda", list(),
        .cambic_horizon_usda_uncached(pedon = pedon))
}

.cambic_horizon_usda_uncached <- function(pedon) {
  ref <- "Soil Survey Staff (2022), KST 13th ed., Ch. 3, cambic horizon (pp. 22-23)"
  h <- pedon$horizons
  n <- if (is.null(h)) 0L else nrow(h)
  if (n == 0L)
    return(DiagnosticResult$new(name = "cambic_horizon_usda", passed = FALSE,
             evidence = list(reason = "no horizons"), reference = ref))
  ord <- order(h$top_cm, na.last = TRUE)
  d <- .usda_desig(h$designation)
  master <- d$master; suffix <- d$suffix
  oc <- h$oc_pct
  # organic layers, rock and Cr are not soil material for the horizon
  org <- (!is.na(master) & master %in% c("O", "H")) | (!is.na(oc) & oc >= 12)
  rock <- !is.na(master) & (master == "R" | (master == "C" & grepl("r", suffix)))
  # 6. not part of an Ap horizon
  ap <- !is.na(master) & grepl("A", master) & grepl("p", suffix)
  # an altered B: E and C masters are not cambic (audit L14: a layer
  # designated C is not); transitions with a B are. Reading N11 (Hugo,
  # 2026-11-21): in a soil with no Ap horizon, an A may be part of the
  # epipedon and of the cambic horizon at once (KST p. 22), so it is read by
  # its properties too.
  plowed <- any(ap)
  elig <- is.na(master) | master == "" | grepl("B", master) |
          (!plowed & master == "A")
  elig <- elig & !org & !rock & !ap
  # 2. texture
  tx <- .usda_texture_grades(h)$vfs_or_finer
  # 3. soil structure or the absence of rock structure in more than half
  st <- tolower(as.character(.usda_col(h, "structure_type", NA_character_)))
  gr <- tolower(as.character(.usda_col(h, "structure_grade", NA_character_)))
  rsp <- .usda_col(h, "rock_structure_pct", NA_real_)
  s3 <- ifelse(!is.na(rsp), rsp < 50,
        ifelse(!is.na(st) & grepl("rock|stratif|lamina", st), FALSE,
        ifelse(!is.na(st) | !is.na(gr), TRUE, NA)))
  # 4a. aquic conditions within 50 cm (or artificial drainage) ...
  aq <- .usda_pv(.usda_try(aquic_conditions_usda(pedon, max_top_cm = 50)))
  if (isTRUE(pedon$site$artificially_drained)) aq <- TRUE
  chr <- h$munsell_chroma_moist; val <- h$munsell_value_moist
  hue <- as.character(h$munsell_hue_moist)
  rdx <- .usda_col(h, "redoximorphic_features_pct", NA_real_)
  redox <- !is.na(rdx) & rdx > 0          # a field observation: not recorded, absent
  neutral <- !is.na(hue) & grepl("^N", toupper(hue))
  col4a <- vapply(seq_len(n), function(i) {
    if (is.na(chr[i])) return(NA)
    a <- if (!neutral[i] || chr[i] != 0) FALSE else if (is.na(val[i])) NA else val[i] <= 3
    b <- if (chr[i] > 1) FALSE else if (is.na(val[i])) NA else val[i] >= 4
    cc <- chr[i] <= 2 && redox[i]
    .usda_any3(c(a, b, cc))
  }, NA)
  wet <- vapply(seq_len(n), function(i) .and3v(aq, col4a[i]), NA)
  # 4b(1). higher chroma, value, redder hue or more clay than the underlying
  # horizon or an overlying horizon
  hr <- .usda_hue_rank(hue); cl <- h$clay_pct
  pos <- integer(n); pos[ord] <- seq_len(n)
  cmp <- function(i, j) {
    if (org[j] || rock[j]) return(FALSE)
    .usda_any3(c(
      if (!is.na(chr[i]) && !is.na(chr[j])) chr[i] > chr[j] else NA,
      if (!is.na(val[i]) && !is.na(val[j])) val[i] > val[j] else NA,
      if (!is.na(hr[i]) && !is.na(hr[j])) hr[i] < hr[j] else NA,
      if (!is.na(cl[i]) && !is.na(cl[j])) cl[i] > cl[j] else NA))
  }
  b1 <- vapply(seq_len(n), function(i) {
    p <- pos[i]
    above <- if (p > 1L) ord[seq_len(p - 1L)] else integer(0)
    below <- if (p < n) ord[p + 1L] else integer(0)
    .usda_any3(vapply(c(above, below), function(j) cmp(i, j), NA))
  }, NA)
  # 4b(2). evidence of the removal of carbonates or gypsum
  ca <- .usda_col(h, "caco3_pct", NA_real_); gy <- .usda_col(h, "caso4_pct", NA_real_)
  removal <- function(x, i) {
    p <- pos[i]
    below <- if (p < n) ord[(p + 1L):n] else integer(0)
    mx <- suppressWarnings(max(x[below], na.rm = TRUE))
    if (!is.finite(mx) || mx < 1) return(FALSE)
    if (is.na(x[i])) NA else x[i] < mx / 2
  }
  b2 <- vapply(seq_len(n), function(i) .usda_any3(c(removal(ca, i), removal(gy, i))), NA)
  ev <- vapply(seq_len(n), function(i) {
    better <- .and3v(.usda_not3(wet[i]), .and3v(s3[i], .usda_any3(c(b1[i], b2[i]))))
    .usda_any3(c(wet[i], better))
  }, NA)
  # 5. layers of the diagnostic horizons and epipedons listed
  excl <- rep(FALSE, n)
  exr <- list(
    anthropic   = .usda_try(anthropic_epipedon_usda(pedon)),
    histic      = .usda_try(histic_epipedon_usda(pedon)),
    folistic    = .usda_try(folistic_epipedon_usda(pedon)),
    melanic     = .usda_try(melanic_epipedon_usda(pedon)),
    mollic      = .usda_try(mollic_epipedon_usda(pedon)),
    plaggen     = .usda_try(plaggen_epipedon_usda(pedon)),
    umbric      = .usda_try(umbric_epipedon_usda(pedon)),
    duripan     = .usda_try(duripan_usda(pedon, max_top_cm = Inf)),
    fragipan    = .usda_try(fragipan_usda(pedon, max_top_cm = Inf)),
    argillic    = .usda_try(argillic_usda(pedon)),
    calcic      = .usda_try(calcic_horizon_usda(pedon, max_top_cm = Inf)),
    gypsic      = .usda_try(gypsic_horizon_usda(pedon, max_top_cm = Inf)),
    natric      = .usda_try(natric_horizon_usda(pedon)),
    oxic        = .usda_try(oxic_usda(pedon)),
    petrocalcic = .usda_try(petrocalcic_subgroup_usda(pedon, max_top_cm = Inf)),
    petrogypsic = .usda_try(petrogypsic_horizon_usda(pedon, max_top_cm = Inf)),
    placic      = .usda_try(placic_horizon_usda(pedon)),
    salic       = .usda_try(salic_horizon_usda(pedon, max_top_cm = Inf)),
    spodic      = .usda_try(spodic_horizon_usda(pedon)),
    sulfuric    = .usda_try(sulfuric_horizon_usda(pedon, max_top_cm = Inf)))
  excluded_by <- character(0)
  for (nm in names(exr)) {
    r <- exr[[nm]]
    if (isTRUE(.usda_pv(r)) && length(r$layers)) {
      l <- intersect(r$layers, seq_len(n))
      if (length(l)) { excl[l] <- TRUE; excluded_by <- c(excluded_by, nm) }
    }
  }
  lay <- vapply(seq_len(n), function(i) {
    if (!elig[i] || excl[i]) return(FALSE)
    .usda_all3(c(tx[i], s3[i], ev[i]))
  }, NA)
  # 1. 15 cm or more
  yes <- Filter(function(r) .usda_run_thickness(h, r) >= 15, .usda_runs(lay %in% TRUE, ord))
  may <- Filter(function(r) .usda_run_thickness(h, r) >= 15, .usda_runs(!(lay %in% FALSE), ord))
  passed <- if (length(yes)) TRUE else if (length(may)) NA else FALSE
  miss <- character(0)
  if (is.na(passed)) {
    und <- unique(unlist(may))
    und <- und[is.na(lay[und])]
    if (any(is.na(tx[und]))) miss <- c(miss, "clay_pct", "sand_pct", "texture_class_usda")
    if (any(is.na(s3[und]))) miss <- c(miss, "structure_type")
    if (any(is.na(ev[und]))) miss <- c(miss, "munsell_chroma_moist", "munsell_value_moist",
                                       "munsell_hue_moist")
  }
  DiagnosticResult$new(
    name = "cambic_horizon_usda", passed = passed,
    layers = if (isTRUE(passed)) sort(unique(unlist(yes)))
             else if (is.na(passed)) sort(unique(unlist(may))) else integer(0),
    evidence = list(
      layer = data.frame(designation = h$designation, eligible = elig,
                         excluded = excl, texture = tx, structure = s3,
                         colour_4a = col4a, wet_4a = wet, compare_4b1 = b1,
                         removal_4b2 = b2, alteration = ev, cambic = lay,
                         stringsAsFactors = FALSE),
      aquic_50 = aq, excluded_by = unique(excluded_by),
      runs_yes = yes, runs_may = may),
    missing = unique(miss), reference = ref)
}


# The cambic horizon within `max_top_cm` of the mineral soil surface (or of
# the soil surface, mineral = FALSE) with its lower boundary at
# `min_bottom_cm` or deeper: item K.1.a of the Key to Soil Orders (100 and 25
# cm below the mineral soil surface) and G.1.c (100 and 25 cm below the soil
# surface). TRUE / FALSE / NA, read on the runs of cambic_horizon_usda().
.cambic_within_usda <- function(pedon, max_top_cm = 100, min_bottom_cm = 25,
                                mineral = TRUE) {
  cb <- cambic_horizon_usda(pedon)
  h <- pedon$horizons
  ms <- if (mineral) .mineral_soil_surface_cm(h) else 0
  if (is.na(ms)) ms <- 0
  ok <- function(runs) any(vapply(runs, function(r)
    isTRUE(min(h$top_cm[r], na.rm = TRUE) - ms < max_top_cm &&
           max(h$bottom_cm[r], na.rm = TRUE) - ms >= min_bottom_cm), NA))
  if (ok(cb$evidence$runs_yes)) TRUE
  else if (ok(cb$evidence$runs_may)) NA
  else FALSE
}


#' Item K of the Key to Soil Orders: Inceptisols (KST 13th ed., p. 70)
#'
#' Soils that do not have a positive water potential at the surface for more
#' than 21 hours of each day (the Wassent condition) and that have either
#' \describe{
#'   \item{K.1}{(a) a cambic horizon within 100 cm of the mineral soil
#'     surface with its lower boundary at 25 cm or more; (b) a calcic,
#'     petrocalcic, gypsic, petrogypsic or placic horizon or a duripan within
#'     100 cm; (c) a fragipan or an oxic, sombric or spodic horizon within
#'     200 cm; (d) a sulfuric horizon within 150 cm; or (e) a cryic or gelic
#'     soil temperature regime and a cambic horizon; or}
#'   \item{K.2}{no sulfidic materials within 50 cm, and (a) in a horizon
#'     between 20 and 50 cm an n value of 0.7 or less (or a nonfluid
#'     fluidity class) or less than 8 \% clay, and (b) a folistic, histic,
#'     mollic, plaggen or umbric epipedon, a salic horizon, or an ESP of 15
#'     or more in half or more of the layers within 50 cm, decreasing below
#'     50 cm, with ground water within 100 cm.}
#' }
#' Depths are from the mineral soil surface. Fluidity is a field observation:
#' an n value not measured and no fluidity class recorded read as nonfluid
#' (audit L16). Ground water is the recorded water table
#' (\code{site$groundwater_depth_cm}); not recorded, absent.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
inceptisol_qualifying_usda <- function(pedon) {
  .memo(pedon, "inceptisol_qualifying_usda", list(),
        .inceptisol_qualifying_usda_uncached(pedon = pedon))
}

.inceptisol_qualifying_usda_uncached <- function(pedon) {
  ref <- "Soil Survey Staff (2022), KST 13th ed., Ch. 4, Key to Soil Orders, item K (p. 70)"
  h <- pedon$horizons
  ms <- .mineral_soil_surface_cm(h); if (is.na(ms)) ms <- 0
  cb <- cambic_horizon_usda(pedon)
  # K.1.a; a cambic horizon the surveyor recorded (NASIS) settles an
  # undecided reading, as before
  k1a <- .cambic_within_usda(pedon, 100, 25)
  if (is.na(k1a)) {
    tb <- .apply_nasis_tiebreaker(cb, pedon, pattern = "^Cambic horizon$",
                                  feature_label = "Cambic horizon")
    if (isTRUE(tb$passed)) k1a <- TRUE
  }
  w <- function(r, d) .usda_within3(r, h, ms + d)
  # K.1.b
  r1b <- list(
    calcic      = .usda_try(calcic_horizon_usda(pedon, max_top_cm = ms + 100)),
    petrocalcic = .usda_try(petrocalcic_subgroup_usda(pedon, max_top_cm = ms + 100)),
    gypsic      = .usda_try(gypsic_horizon_usda(pedon, max_top_cm = ms + 100)),
    petrogypsic = .usda_try(petrogypsic_horizon_usda(pedon, max_top_cm = ms + 100)),
    placic      = .usda_try(placic_horizon_usda(pedon)),
    duripan     = .usda_try(duripan_usda(pedon, max_top_cm = ms + 100)))
  v1b <- vapply(r1b, function(r) w(r, 100), NA)
  # K.1.c
  r1c <- list(
    fragipan = .usda_try(fragipan_usda(pedon, max_top_cm = ms + 200)),
    oxic     = .usda_try(oxic_usda(pedon)),
    sombric  = .usda_try(sombric_horizon_usda(pedon, max_top_cm = ms + 200)),
    spodic   = .usda_try(spodic_horizon_usda(pedon)))
  v1c <- vapply(r1c, function(r) w(r, 200), NA)
  # K.1.d
  r1d <- .usda_try(sulfuric_horizon_usda(pedon, max_top_cm = ms + 150))
  k1d <- w(r1d, 150)
  # K.1.e
  cold <- .usda_any3(c(.usda_pv(.usda_try(str_cryic_usda(pedon))),
                       .usda_pv(.usda_try(str_gelic_usda(pedon)))))
  k1e <- .and3v(cold, .usda_pv(cb))
  k1 <- .usda_any3(c(k1a, v1b, v1c, k1d, k1e))
  # K.2
  sulf <- .usda_pv(.usda_try(sulfidic_materials_usda(pedon, max_top_cm = ms + 50)))
  dg <- .usda_desig(h$designation)
  band <- which(!is.na(h$top_cm) & h$bottom_cm > ms + 20 & h$top_cm < ms + 50 &
                  !(dg$master %in% c("R", "O", "H")) &
                  !(dg$master %in% "C" & grepl("r", dg$suffix)))
  nv <- .usda_col(h, "n_value", NA_real_)
  fl <- tolower(as.character(.usda_col(h, "fluidity_class", NA_character_)))
  k2a <- if (!length(band)) FALSE else .usda_any3(vapply(band, function(i) {
    nonfluid <- if (!is.na(nv[i])) nv[i] <= 0.7
                else if (!is.na(fl[i])) grepl("non", fl[i]) else TRUE
    .usda_any3(c(nonfluid, if (is.na(h$clay_pct[i])) NA else h$clay_pct[i] < 8))
  }, NA))
  r2b <- list(
    folistic = .usda_try(folistic_epipedon_usda(pedon)),
    histic   = .usda_try(histic_epipedon_usda(pedon)),
    mollic   = .usda_try(mollic_epipedon_usda(pedon)),
    plaggen  = .usda_try(plaggen_epipedon_usda(pedon)),
    umbric   = .usda_try(umbric_epipedon_usda(pedon)),
    salic    = .usda_try(salic_horizon_usda(pedon, max_top_cm = Inf)))
  v2b <- vapply(r2b, .usda_pv, NA)
  sodic <- .usda_sodic_groundwater(pedon, half_of = "layers")
  k2 <- .usda_all3(c(.usda_not3(sulf), k2a, .usda_any3(c(v2b, sodic))))
  wass <- .usda_pv(.usda_try(wassent_qualifying_usda(pedon)))
  passed <- .and3v(.usda_not3(wass), .usda_any3(c(k1, k2)))
  miss <- character(0)
  if (is.na(passed)) {
    if (is.na(k1a)) miss <- c(miss, cb$missing)
    for (r in c(r1b, r1c, list(r1d), r2b))
      if (!is.null(r) && is.na(.usda_pv(r))) miss <- c(miss, r$missing)
    if (is.na(k1e)) miss <- c(miss, "site$soil_temperature_regime")
  }
  DiagnosticResult$new(
    name = "inceptisol_qualifying_usda", passed = passed,
    layers = if (isTRUE(k1a)) cb$layers else integer(0),
    evidence = list(cambic = cb, k1a_cambic = k1a, k1b = v1b, k1c = v1c,
                    k1d_sulfuric = k1d, k1e_cold_cambic = k1e, k1 = k1,
                    k2_no_sulfidic = .usda_not3(sulf), k2a = k2a, k2b = v2b,
                    k2b_sodic = sodic, k2 = k2, wassent = wass),
    missing = unique(miss), reference = ref)
}


# Sodium with ground water (Key to Soil Orders K.2.b(3), p. 70; Aquepts KA.2,
# p. 207): an ESP of 15 or more (or SAR of 13 or more) in half or more of the
# layers (half_of = "layers") or of the soil volume (half_of = "volume")
# within 50 cm of the mineral soil surface, decreasing below 50 cm, and
# ground water within 100 cm at some time of the year. The ground water is
# site$groundwater_depth_cm (100 cm or less, with a groundwater_kind other
# than none); not recorded, absent (a field observation). TRUE / FALSE / NA.
.usda_sodic_groundwater <- function(pedon, half_of = c("layers", "volume")) {
  half_of <- match.arg(half_of)
  h <- pedon$horizons
  ms <- .mineral_soil_surface_cm(h); if (is.na(ms)) ms <- 0
  esp <- .usda_esp(h)
  sar <- .usda_col(h, "sar", NA_real_)
  sod <- ifelse(!is.na(esp) & esp >= 15, TRUE, ifelse(!is.na(sar) & sar >= 13, TRUE,
         ifelse(is.na(esp) & is.na(sar), NA, FALSE)))
  top <- which(!is.na(h$top_cm) & h$top_cm < ms + 50 & h$bottom_cm > ms)
  if (!length(top)) return(FALSE)
  wt <- if (half_of == "volume")
          pmin(h$bottom_cm[top], ms + 50) - pmax(h$top_cm[top], ms) else rep(1, length(top))
  yes <- sum(wt[sod[top] %in% TRUE]); may <- sum(wt[!(sod[top] %in% FALSE)])
  half <- if (yes >= sum(wt) / 2) TRUE else if (may >= sum(wt) / 2) NA else FALSE
  if (isFALSE(half)) return(FALSE)
  below <- which(!is.na(h$top_cm) & h$top_cm >= ms + 50)
  e_top <- suppressWarnings(max(esp[top], na.rm = TRUE))
  e_bel <- esp[below]
  decr <- if (!length(below) || all(is.na(e_bel)) || !is.finite(e_top)) NA
          else all(e_bel[!is.na(e_bel)] <= e_top)
  gw <- suppressWarnings(as.numeric(pedon$site$groundwater_depth_cm %||% NA_real_))[1]
  gk <- tolower(as.character(pedon$site$groundwater_kind %||% NA_character_))[1]
  ground <- !is.na(gw) && gw <= 100 && !(gk %in% c("none", "none observed", "not observed"))
  .usda_all3(c(half, decr, ground))
}


#' Aquept Suborder (KST 13th ed., item KA, p. 207)
#'
#' KA.1: aquic conditions in the layer at 40 to 50 cm (or above a densic,
#' lithic or paralithic contact), read by \code{aquic_conditions_usda()}
#' within 50 cm (or artificial drainage), and one or more of (a) a histic
#' epipedon, (b) a sulfuric horizon within 50 cm, (c) a layer directly under
#' the epipedon or within 50 cm with chroma of 2 or less and redox
#' concentrations, or chroma of 1 or less, on ped faces or in the matrix, (d)
#' a positive alpha,alpha-dipyridyl reaction within 50 cm
#' (\code{childs_test}); or KA.2: an ESP of 15 or more (or SAR of 13 or more)
#' in half or more of the soil volume within 50 cm, decreasing below, with
#' ground water within 100 cm. Up to v0.9.418 the aquic proxy alone. Redox
#' concentrations and the dipyridyl reaction are field observations: not
#' recorded, absent.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
aquept_qualifying_usda <- function(pedon) {
  h <- pedon$horizons
  ms <- .mineral_soil_surface_cm(h); if (is.na(ms)) ms <- 0
  aqr <- aquic_conditions_usda(pedon, max_top_cm = ms + 50)
  aq <- .usda_pv(aqr)
  if (isTRUE(pedon$site$artificially_drained)) aq <- TRUE
  hist <- .usda_pv(.usda_try(histic_epipedon_usda(pedon)))
  sulf <- .usda_pv(.usda_try(sulfuric_horizon_usda(pedon, max_top_cm = ms + 50)))
  d <- .usda_desig(h$designation)
  L <- which(!is.na(h$top_cm) & h$top_cm < ms + 50 &
               (is.na(d$master) | !d$master %in% c("A", "O", "H")))
  rdx <- .usda_col(h, "redoximorphic_features_pct", NA_real_)
  col <- if (!length(L)) FALSE else .usda_any3(vapply(L, function(i) {
    cm <- h$munsell_chroma_moist[i]
    if (is.na(cm)) NA else cm <= 1 || (cm <= 2 && !is.na(rdx[i]) && rdx[i] > 0)
  }, NA))
  ct <- tolower(as.character(.usda_col(h, "childs_test", NA_character_)))
  dip <- any(!is.na(h$top_cm) & h$top_cm < ms + 50 & grepl("^pos", ct))
  ka1 <- .and3v(aq, .usda_any3(c(hist, sulf, col, dip)))
  ka2 <- .usda_sodic_groundwater(pedon, half_of = "volume")
  passed <- .usda_any3(c(ka1, ka2))
  DiagnosticResult$new(
    name = "aquept_qualifying_usda", passed = passed, layers = aqr$layers,
    evidence = list(aquic = aq, histic = hist, sulfuric_50 = sulf,
                    colour = col, dipyridyl = dip, ka1 = ka1, ka2_sodic = ka2),
    missing = if (is.na(passed)) unique(c("munsell_chroma_moist", aqr$missing)) else character(0),
    reference = "Soil Survey Staff (2022), KST 13th ed., Ch. 11, item KA (p. 207)")
}


#' Halaquepts (KST 13th ed., item KAC, p. 207)
#'
#' A salic horizon; or, in horizons with a total thickness of 25 cm or more
#' within 50 cm of the mineral soil surface, an ESP of 15 or more (or SAR of
#' 13 or more), decreasing below 50 cm. Up to v0.9.418 an EC of 8 dS/m or
#' more within 100 cm, which the text does not give.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
halaquept_qualifying_usda <- function(pedon) {
  h <- pedon$horizons
  ms <- .mineral_soil_surface_cm(h); if (is.na(ms)) ms <- 0
  sal <- .usda_try(salic_horizon_usda(pedon, max_top_cm = Inf))
  salic <- .usda_pv(sal)
  esp <- .usda_esp(h); sar <- .usda_col(h, "sar", NA_real_)
  sod <- ifelse(!is.na(esp) & esp >= 15, TRUE, ifelse(!is.na(sar) & sar >= 13, TRUE,
         ifelse(is.na(esp) & is.na(sar), NA, FALSE)))
  top <- which(!is.na(h$top_cm) & h$top_cm < ms + 50 & h$bottom_cm > ms)
  thk <- pmin(h$bottom_cm[top], ms + 50) - pmax(h$top_cm[top], ms)
  t_yes <- sum(thk[sod[top] %in% TRUE]); t_may <- sum(thk[!(sod[top] %in% FALSE)])
  thick <- if (t_yes >= 25) TRUE else if (t_may >= 25) NA else FALSE
  below <- which(!is.na(h$top_cm) & h$top_cm >= ms + 50)
  e_top <- suppressWarnings(max(esp[top][sod[top] %in% TRUE], na.rm = TRUE))
  e_bel <- esp[below]
  decr <- if (isFALSE(thick)) FALSE
          else if (!length(below) || all(is.na(e_bel)) || !is.finite(e_top)) NA
          else all(e_bel[!is.na(e_bel)] <= e_top)
  sodic <- .usda_all3(c(thick, decr))
  passed <- .usda_any3(c(salic, sodic))
  miss <- character(0)
  if (is.na(passed)) {
    if (is.na(salic)) miss <- c(miss, sal$missing)
    if (is.na(sodic)) miss <- c(miss, "esp_pct")
  }
  DiagnosticResult$new(
    name = "halaquept_qualifying_usda", passed = passed,
    layers = if (isTRUE(salic)) sal$layers else top[sod[top] %in% TRUE],
    evidence = list(salic = salic, sodium_25cm = thick, decreasing = decr),
    missing = unique(miss),
    reference = "Soil Survey Staff (2022), KST 13th ed., Ch. 11, item KAC (p. 207)")
}


#' Densiaquept qualifying (densic contact within 100 cm)
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
densiaquept_qualifying_usda <- function(pedon) {
  h <- pedon$horizons
  cand <- which(!is.na(h$top_cm) & h$top_cm < 100 &
                  !is.na(h$rupture_resistance) &
                  tolower(h$rupture_resistance) %in%
                    c("very firm", "extremely firm"))
  passed <- length(cand) > 0L
  DiagnosticResult$new(
    name = "densiaquept_qualifying_usda", passed = passed, layers = cand,
    evidence = list(note = "v0.8 proxy: very firm/extremely firm rupture-resistance"),
    missing = character(0),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 3, p 39"
  )
}


#' Eutrudepts (KST 13th ed., item KFF, p. 223)
#'
#' Free carbonates throughout, or a base saturation (NH4OAc) of
#' \code{min_bs} (60) \% or more in one or more horizons between 25 and 75
#' cm from the mineral soil surface, or directly above a root-limiting
#' layer (a densic, lithic or paralithic contact) at a shallower depth.
#' Three-valued: up to v0.9.418 any horizon above 75 cm counted, and a base
#' saturation not measured made the soil a Dystrudept. Free carbonates are
#' read from caco3_pct (above 0), else from the recorded effervescence (a
#' field test: not recorded, none).
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_bs Base saturation threshold, 60.
#' @noRd
eutric_inceptisol_usda <- function(pedon, min_bs = 60) {
  h <- pedon$horizons
  ms <- .mineral_soil_surface_cm(h); if (is.na(ms)) ms <- 0
  rl <- .usda_contact_top_cm(h)
  lo <- if (!is.na(rl) && rl < ms + 75) rl else ms + 75
  band <- which(!is.na(h$top_cm) & h$bottom_cm > ms + 25 & h$top_cm < lo)
  if (!is.na(rl) && rl < ms + 75) {
    above <- which(!is.na(h$bottom_cm) & h$bottom_cm <= rl & h$top_cm < rl)
    if (length(above)) band <- union(band, above[which.max(h$bottom_cm[above])])
  }
  bs <- h$bs_pct[band]
  b <- if (any(bs >= min_bs, na.rm = TRUE)) TRUE
       else if (!length(band) || anyNA(bs)) NA else FALSE
  mineral <- which(!is.na(h$top_cm) & h$top_cm >= ms &
                     !(.usda_desig(h$designation)$master %in% c("R")))
  ca <- .usda_col(h, "caco3_pct", NA_real_)[mineral]
  ef <- tolower(as.character(.usda_col(h, "effervescence_class", NA_character_)))[mineral]
  carb_i <- ifelse(!is.na(ca), ca > 0,
            ifelse(!is.na(ef), !grepl("^none|^non|^no ", ef), FALSE))
  carb <- length(mineral) > 0L && all(carb_i)
  passed <- .usda_any3(c(carb, b))
  DiagnosticResult$new(
    name = "eutric_inceptisol_usda", passed = passed,
    layers = band[!is.na(bs) & bs >= min_bs],
    evidence = list(threshold = min_bs, band = band, bs = bs,
                    carbonates_throughout = carb),
    missing = if (is.na(passed)) "bs_pct" else character(0),
    reference = "Soil Survey Staff (2022), KST 13th ed., Ch. 11, item KFF (p. 223)"
  )
}


#' Humic Inceptisol Suborder helper (Hum*)
#' Pass when umbric or mollic epipedon present + thick (>= 25 cm).
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
humic_inceptisol_usda <- function(pedon) {
  mo <- mollic_epipedon_usda(pedon)
  um <- umbric_epipedon_usda(pedon)
  passed <- isTRUE(mo$passed) || isTRUE(um$passed)
  DiagnosticResult$new(
    name = "humic_inceptisol_usda", passed = passed,
    layers = unique(c(mo$layers, um$layers)),
    evidence = list(mollic = mo, umbric = um),
    missing = character(0),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 11"
  )
}


#' Humic colour-value intergrade (KST 13ed): Hum* Inceptisol subgroups
#'
#' The "Humic" subgroup differentia for Udept / Xerept / Ustept great groups
#' (verbatim, e.g. KST-13 Ch. 11 Humic Eutrudepts): a colour value, moist, of
#' \code{max_value_moist} (3) or less AND a colour value, dry, of
#' \code{max_value_dry} (5) or less (crushed and smoothed sample) throughout the
#' upper \code{depth_cm} (18) cm of the mineral soil. This is the dark-coloured
#' intergrade that does NOT reach an umbric / mollic epipedon (so the order has
#' already keyed without one), distinct from \code{humic_inceptisol_usda}
#' (the epipedon-based suborder helper). Conservative: requires BOTH the moist
#' and dry value recorded for every layer overlapping the window (a missing dry
#' value cannot confirm the criterion), so it never over-fires on a dark A alone.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_value_moist,max_value_dry Colour-value ceilings (default 3 / 5).
#' @param depth_cm Top-of-soil window in cm (default 18).
#' @return A \code{\link{DiagnosticResult}}.
#' @noRd
humic_colour_usda <- function(pedon, max_value_moist = 3, max_value_dry = 5,
                              depth_cm = 18) {
  h <- pedon$horizons
  cand <- which(!is.na(h$top_cm) & h$top_cm < depth_cm)
  if (length(cand) == 0L) {
    return(DiagnosticResult$new(
      name = "humic_colour_usda", passed = FALSE, layers = integer(0),
      evidence = list(reason = "no layer within the upper window"),
      missing = character(0),
      reference = "Soil Survey Staff (2022), KST 13ed, Ch. 11 (Humic)"
    ))
  }
  vm <- h$munsell_value_moist[cand]
  vd <- h$munsell_value_dry[cand]
  miss <- character(0)
  if (any(is.na(vm))) miss <- c(miss, "munsell_value_moist")
  if (any(is.na(vd))) miss <- c(miss, "munsell_value_dry")
  # "throughout the upper 18 cm": EVERY overlapping mineral layer must be dark
  # in BOTH moist and dry value. Missing -> not confirmed (conservative).
  vm_ok <- !is.na(vm) & vm <= max_value_moist
  vd_ok <- !is.na(vd) & vd <= max_value_dry
  passed <- all(vm_ok) && all(vd_ok)
  DiagnosticResult$new(
    name = "humic_colour_usda",
    passed = passed,
    layers = if (passed) cand else integer(0),
    evidence = list(window_cm = depth_cm, n_layers = length(cand),
                    value_moist = vm, value_dry = vd),
    missing = unique(miss),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 11 (Humic subgroups)"
  )
}
