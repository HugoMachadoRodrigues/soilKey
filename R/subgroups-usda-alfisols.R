# =============================================================
# USDA Soil Taxonomy 13th ed. -- Alfisol subgroups (Ch. 5, pp. 73-117)
# =============================================================
#
# v0.9.422 (audit items R3 to R6, G7, F16, F17): the tests of the subgroup
# keys of the Alfisols, read from the KST 13th edition text, for the lists
# of inst/rules/usda/subgroups/alfisols.yaml. Every test is three-valued:
# TRUE or FALSE where the recorded data decide it, NA (with the missing data
# listed) where they do not. Field observations not recorded (redox
# features, slickensides, cracks, worm holes, plinthite, lamellae, tongues
# of albic materials, visible gypsum or salts, cementation) are absent;
# measurements (base saturation, ESP, CEC, COLE, oxalate extracts), colour,
# structure and texture not recorded are undecided.
#
# The names of the KST differ from order to order: the Arenic, Grossarenic,
# Psammentic, Aeric, Mollic, Umbric, Ultic and Inceptic subgroups of the
# Alfisols are read here with the Alfisol text (audit item F16), so the
# helpers shared with other orders are left as they are.
# =============================================================


# ---- Shared readings -------------------------------------------------

.sga_ref <- function(what) paste0("Soil Survey Staff (2022), KST 13th ed., Ch. 5, ", what)

.sga_res <- function(name, passed, layers = integer(0), evidence = list(),
                     missing = character(0), ref = .sga_ref("Alfisol subgroups")) {
  passed <- if (length(passed) != 1L) NA else as.logical(passed)
  DiagnosticResult$new(name = name, passed = passed,
                       layers = if (isTRUE(passed)) as.integer(unique(layers)) else integer(0),
                       evidence = evidence,
                       missing = if (is.na(passed)) unique(as.character(missing)) else character(0),
                       reference = ref)
}

.sga_num <- function(h, col) suppressWarnings(as.numeric(.usda_col(h, col, NA_real_)))
.sga_chr <- function(h, col) as.character(.usda_col(h, col, NA_character_))

# The mineral soil surface (cm).
.sga_ms <- function(pedon) {
  ms <- .q_mineral_surface_cm(pedon$horizons)
  if (is.na(ms)) 0 else ms
}

# Layers overlapping the depth window [lo, hi) (absolute cm). A layer with no
# lower boundary recorded is taken to reach 1 cm below its top.
.sga_in <- function(h, lo, hi) {
  bot <- ifelse(is.na(h$bottom_cm), h$top_cm + 1, h$bottom_cm)
  which(!is.na(h$top_cm) & h$top_cm < hi & bot > lo)
}

# Thickness of each layer clipped to [lo, hi).
.sga_clip <- function(h, i, lo = -Inf, hi = Inf) {
  bot <- ifelse(is.na(h$bottom_cm[i]), h$top_cm[i] + 1, h$bottom_cm[i])
  pmax(pmin(bot, hi) - pmax(h$top_cm[i], lo), 0)
}

# Three-valued comparisons of a vector of numbers (NA stays NA).
.sga_ge <- function(x, t) ifelse(is.na(x), NA, x >= t)
.sga_lt <- function(x, t) ifelse(is.na(x), NA, x < t)

# A share of a thickness, three-valued: TRUE where the layers known to meet
# the test reach `share` of the total, FALSE where even the undecided ones
# cannot reach it.
.sga_share <- function(ok, w, share) {
  tot <- sum(w)
  if (tot <= 0) return(NA)
  yes <- sum(w[ok %in% TRUE]); unk <- sum(w[is.na(ok)])
  if (yes / tot >= share - 1e-9) TRUE else if ((yes + unk) / tot < share - 1e-9) FALSE else NA
}

# The diagnostic horizon of a subgroup test: "argillic", "kandic",
# "natric", "ak" (the argillic or, without one, the kandic horizon) or "akn"
# (argillic, kandic or natric). TRUE with its layers (sorted by depth), FALSE,
# or NA with the missing data of the undecided horizon.
.sga_hz <- function(pedon, kind = "argillic") {
  kinds <- switch(kind, argillic = "argillic", kandic = "kandic", natric = "natric",
                  ak = c("argillic", "kandic"), akn = c("argillic", "kandic", "natric"),
                  stop("unknown horizon kind: ", kind))
  h <- pedon$horizons
  ps <- logical(0); miss <- character(0)
  for (k in kinds) {
    r <- .usda_try(switch(k, argillic = argillic_usda(pedon),
                          kandic = kandic_horizon_usda(pedon),
                          natric = natric_horizon_usda(pedon)))
    if (is.null(r)) { ps <- c(ps, NA); next }
    L <- r$layers[!is.na(h$top_cm[r$layers])]
    if (isTRUE(r$passed) && length(L)) {
      L <- L[order(h$top_cm[L])]
      return(list(passed = TRUE, layers = L, top = min(h$top_cm[L]),
                  bottom = max(h$bottom_cm[L]), kind = k, missing = character(0)))
    }
    ps <- c(ps, r$passed)
    miss <- c(miss, r$missing %||% character(0))
  }
  list(passed = if (all(ps %in% FALSE)) FALSE else NA, layers = integer(0),
       top = NA_real_, bottom = NA_real_, kind = NA_character_, missing = unique(miss))
}

# Densic, lithic or paralithic contact (cm), NA when none is recorded.
.sga_contact <- function(pedon) .kst_contact_top_cm(pedon, petroferric = FALSE)

# Munsell chromas of the mottle colours recorded in each layer ("10YR 6/2;
# 7.5YR 5/8"): the lowest chroma, NA when none is recorded.
.sga_mottle_min_chroma <- function(h) {
  mc <- .sga_chr(h, "mottle_colors")
  vapply(mc, function(s) {
    if (is.na(s) || !nzchar(s)) return(NA_real_)
    m <- regmatches(s, gregexpr("[0-9.]+\\s*/\\s*[0-9.]+", s))[[1]]
    if (!length(m)) return(NA_real_)
    cr <- suppressWarnings(as.numeric(sub("^.*/\\s*", "", m)))
    if (all(is.na(cr))) NA_real_ else min(cr, na.rm = TRUE)
  }, 0, USE.NAMES = FALSE)
}

# Redox concentrations recorded in each layer (a field observation: not
# recorded, absent).
.sga_redox_conc <- function(h) {
  rdx <- .sga_num(h, "redoximorphic_features_pct")
  mp <- .usda_col(h, "mottles_present", NA)
  mc <- .sga_chr(h, "mottle_colors")
  nod <- .sga_num(h, "fe_mn_nodules_pct")
  (!is.na(rdx) & rdx > 0) | (as.logical(mp) %in% TRUE) |
    (!is.na(mc) & nzchar(mc)) | (!is.na(nod) & nod > 0)
}

# Redox depletions with chroma of 2 or less in a layer with aquic conditions
# (reading N10: aquic conditions shown by redox features, a g designation or
# a reduced matrix): a g in the designation, a reduced matrix recorded,
# mottles of chroma 2 or less, or redox features in a matrix of chroma 2 or
# less. Undecided where redox features are recorded but the matrix chroma is
# not.
.sga_depletion <- function(h) {
  d <- .usda_desig(h$designation)
  g <- !is.na(d$suffix) & grepl("g", d$suffix)
  red <- .sga_num(h, "reduced_matrix_pct")
  mot <- .sga_mottle_min_chroma(h)
  conc <- .sga_redox_conc(h)
  chr <- .sga_num(h, "munsell_chroma_moist")
  yes <- g | (!is.na(red) & red > 0) | (!is.na(mot) & mot <= 2) |
    (conc & !is.na(chr) & chr <= 2)
  ifelse(yes, TRUE, ifelse(conc & is.na(chr), NA, FALSE))
}


# ---- Aquic subgroups -----------------------------------------------------

#' Aquic subgroups of the Alfisols (KST 13th ed., Ch. 5)
#'
#' Redox depletions with chroma of 2 or less and aquic conditions for some
#' time in normal years, in a zone that each great group names (read with
#' \code{.sga_depletion()}, reading N10):
#' \describe{
#'   \item{\code{"surface"}}{in one or more horizons within \code{max_cm} of
#'     the mineral soil surface (75 cm in most great groups, e.g. Aquic
#'     Haplustalfs, p. 99; 100 cm Palecryalfs, p. 85; 60 cm Ferrudalfs,
#'     p. 87; 40 cm Fragiudalfs and Fragixeralfs, pp. 87, 110).}
#'   \item{\code{"upper25"}}{within the upper 25 cm of the \code{horizon}
#'     (Aquic Glossocryalfs, p. 82; Aquic Fraglossudalfs and Glossudalfs,
#'     pp. 87, 88).}
#'   \item{\code{"relative"}}{within the upper 25 cm of the \code{horizon} if
#'     its upper boundary is within 50 cm of the mineral soil surface, else
#'     within 75 cm (Aquic Hapludalfs, p. 92; Aquic Natrudalfs, p. 95).}
#'   \item{\code{"within"}}{in subhorizons of the \code{horizon} (Aquic
#'     Durixeralfs, p. 110).}
#' }
#' @param pedon A \code{\link{PedonRecord}}.
#' @param zone One of the zones above.
#' @param max_cm Depth of the \code{"surface"} zone (cm).
#' @param horizon The horizon the zone is read from (see \code{.sga_hz()}).
#' @return A \code{\link{DiagnosticResult}}.
#' @noRd
sg_alfisols_aquic_usda <- function(pedon, zone = "surface", max_cm = 75,
                                   horizon = "argillic") {
  zone <- match.arg(zone, c("surface", "upper25", "relative", "within"))
  h <- pedon$horizons
  nm <- "sg_alfisols_aquic_usda"
  ref <- .sga_ref("Aquic subgroups (e.g. pp. 85, 92, 99)")
  if (!nrow(h)) return(.sga_res(nm, FALSE, ref = ref))
  ms <- .sga_ms(pedon)
  dep <- .sga_depletion(h)
  miss <- c("munsell_chroma_moist")
  if (zone == "surface") {
    L <- .sga_in(h, ms, ms + max_cm)
    p <- .any3u(dep[L])
    return(.sga_res(nm, p, L[dep[L] %in% TRUE],
                    list(zone = zone, zone_cm = c(ms, ms + max_cm)), miss, ref))
  }
  hz <- .sga_hz(pedon, horizon)
  if (isTRUE(hz$passed)) {
    t <- hz$top
    # the upper 25 cm of the horizon are its own layers there (a layer below
    # the horizon, within 25 cm of its top, is not part of it)
    up25 <- intersect(hz$layers, .sga_in(h, t, t + 25))
    L <- switch(zone,
      upper25 = up25,
      relative = if (t < ms + 50) up25 else .sga_in(h, ms, ms + 75),
      within = hz$layers)
    p <- .any3u(dep[L])
    return(.sga_res(nm, p, L[dep[L] %in% TRUE],
                    list(zone = zone, horizon = hz$kind, horizon_top_cm = t), miss, ref))
  }
  if (isFALSE(hz$passed)) return(.sga_res(nm, FALSE, evidence = list(zone = zone,
                                   reason = paste("no", horizon, "horizon")), ref = ref))
  # the horizon is undecided: no depletion anywhere within 100 cm decides
  L <- .sga_in(h, ms, ms + 100)
  p <- if (all(dep[L] %in% FALSE)) FALSE else NA
  .sga_res(nm, p, evidence = list(zone = zone, horizon = "undecided"),
           missing = c(hz$missing, miss), ref = ref)
}


# ---- Oxyaquic above the fragipan ---------------------------------------

#' Oxyaquic Fragiudalfs and Fraglossudalfs (KST 13th ed., p. 87)
#'
#' Saturation in one or more layers above the fragipan for 20 consecutive or
#' 30 cumulative days in normal years, read as \code{oxyaquic_subgroup_usda()}
#' reads it (\code{water_saturation_days}; reading L17) with the depth set to
#' the top of the fragipan.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_alfisols_oxyaquic_usda <- function(pedon) {
  h <- pedon$horizons
  ms <- .sga_ms(pedon)
  fl <- .sga_fragic_layers(pedon)
  top <- if (length(fl)) min(h$top_cm[fl]) - ms else 100
  res <- oxyaquic_subgroup_usda(pedon, max_depth_cm = top)
  res$name <- "sg_alfisols_oxyaquic_usda"
  res$reference <- .sga_ref("Oxyaquic Fragiudalfs (p. 87)")
  res
}


# ---- Vertic subgroups ----------------------------------------------------

# Slickensides or wedge-shaped peds recorded in a layer (field observations).
.sga_slick <- function(h) {
  ss <- .field_term(.sga_chr(h, "slickensides"))
  st <- .field_term(.sga_chr(h, "structure_type"))
  (!is.na(ss) & grepl("few|common|many|continuous|present|yes|distinct|prominent", ss) &
     !grepl("^(none|absent|no)$", ss)) |
    (!is.na(st) & grepl("wedge", st))
}

# Thickness of the thickest run of adjoining layers among `L` (sorted).
.sga_run_thickness <- function(h, L) {
  if (!length(L)) return(0)
  L <- L[order(h$top_cm[L])]
  best <- 0; cur <- 0; prev_bot <- NA_real_
  for (i in L) {
    thk <- .sga_clip(h, i)
    if (!is.na(prev_bot) && abs(h$top_cm[i] - prev_bot) < 1e-6) cur <- cur + thk else cur <- thk
    best <- max(best, cur)
    prev_bot <- if (is.na(h$bottom_cm[i])) h$top_cm[i] + 1 else h$bottom_cm[i]
  }
  best
}

#' Vertic subgroups of the Alfisols (KST 13th ed., Ch. 5, e.g. p. 92)
#'
#' One or both of: (1) cracks within 125 cm of the mineral soil surface 5 mm
#' or more wide through a thickness of 30 cm or more, and slickensides or
#' wedge-shaped peds in a layer 15 cm or more thick with its upper boundary
#' within 125 cm; (2) a linear extensibility of 6.0 cm or more between the
#' mineral soil surface and 100 cm or a densic, lithic or paralithic contact.
#' Cracks, slickensides and wedge-shaped peds are field observations (absent
#' when not recorded); where no crack is recorded at all, the slickensides
#' stand for the cracks (reading L30, as in the Vertisols). The linear
#' extensibility is the sum of COLE x thickness, a measurement: where COLE is
#' not recorded and the cracks do not decide, the test is undecided.
#' With \code{above_duripan} (Vertic Durixeralfs, p. 110) both are read above
#' the duripan, with no other depth limit.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param above_duripan Read above the duripan (Durixeralfs).
#' @noRd
sg_alfisols_vertic_usda <- function(pedon, above_duripan = FALSE)
  .usda_vertic_field_reading(.sg_alfisols_vertic_usda_record(pedon, above_duripan), pedon)

# The test as the record gives it; the wrapper above reads a COLE not
# recorded from the field record (R/subgroups-usda-vertic-field.R).
.sg_alfisols_vertic_usda_record <- function(pedon, above_duripan = FALSE) {
  h <- pedon$horizons
  nm <- "sg_alfisols_vertic_usda"
  ref <- .sga_ref("Vertic subgroups (e.g. pp. 92, 110)")
  if (!nrow(h)) return(.sga_res(nm, FALSE, ref = ref))
  ms <- .sga_ms(pedon)
  lim_crack <- ms + 125; lim_le <- ms + 100
  ct <- .sga_contact(pedon)
  if (!is.na(ct)) lim_le <- min(lim_le, ct)
  if (isTRUE(above_duripan)) {
    du <- duripan_usda(pedon, max_top_cm = Inf)
    dt <- if (isTRUE(du$passed) && length(du$layers)) min(h$top_cm[du$layers]) else Inf
    lim_crack <- dt; lim_le <- min(dt, if (is.na(ct)) Inf else ct)
  }
  Z <- .sga_in(h, ms, lim_crack)
  cw <- .sga_num(h, "cracks_width_cm")
  cd <- .sga_num(h, "cracks_depth_cm")
  sl <- .sga_slick(h)
  slick <- .sga_run_thickness(h, Z[sl[Z]]) >= 15 - 1e-6
  crack_rec <- any(!is.na(cw[Z]) | !is.na(cd[Z]))
  cracks <- if (!crack_rec) slick else {
    wide <- Z[!is.na(cw[Z]) & cw[Z] >= 0.5]
    any(!is.na(cd[Z]) & cd[Z] >= 30) || .sga_run_thickness(h, wide) >= 30 - 1e-6
  }
  b1 <- isTRUE(cracks) && isTRUE(slick)
  # linear extensibility
  W <- .sga_in(h, ms, lim_le)
  cole <- .sga_num(h, "cole_value")
  w <- .sga_clip(h, W, ms, lim_le)
  known <- sum((cole[W] * w)[!is.na(cole[W])])
  le <- if (known >= 6 - 1e-9) TRUE
        else if (length(W) && !anyNA(cole[W])) FALSE else NA
  p <- .usda_or3(b1, le)
  .sga_res(nm, p, if (b1) Z[sl[Z]] else W,
           list(cracks = cracks, slickensides_15cm = slick,
                cracks_from_slickensides = !crack_rec,
                linear_extensibility_cm = if (is.na(le)) paste(">=", round(known, 2)) else known,
                above_duripan = isTRUE(above_duripan)),
           c("cole_value"), ref)
}


# ---- Chromic -------------------------------------------------------------

#' Chromic (Vertic) subgroups of the Alfisols (KST 13th ed., e.g. p. 74)
#'
#' An Ap horizon, or the materials between the mineral soil surface and 18
#' cm, that after mixing have a moist value of 4 or more, a dry value of 6 or
#' more, or a chroma of 4 or more. Mixing is a thickness-weighted mean; the
#' colours not recorded take their bounds, so the test is undecided only
#' where they could change it.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_alfisols_chromic_usda <- function(pedon) {
  h <- pedon$horizons
  nm <- "sg_alfisols_chromic_usda"
  ref <- .sga_ref("Chromic Vertic subgroups (e.g. p. 74)")
  if (!nrow(h)) return(.sga_res(nm, FALSE, ref = ref))
  ms <- .sga_ms(pedon)
  d <- .usda_desig(h$designation)
  ap <- which(!is.na(d$master) & d$master == "A" & grepl("p", d$suffix) &
                !is.na(h$top_cm) & h$top_cm >= ms - 1e-6)
  hi <- if (length(ap)) max(h$bottom_cm[ap], na.rm = TRUE) else ms + 18
  if (!is.finite(hi)) hi <- ms + 18
  L <- .sga_in(h, ms, hi)
  if (!length(L)) return(.sga_res(nm, NA, missing = "munsell_value_moist", ref = ref))
  w <- .sga_clip(h, L, ms, hi)
  vm <- .mo_wrange(.sga_num(h, "munsell_value_moist")[L], w, 0, 10)
  vd <- .mo_wrange(.sga_num(h, "munsell_value_dry")[L], w, 0, 10)
  cm <- .mo_wrange(.sga_num(h, "munsell_chroma_moist")[L], w, 0, 20)
  p <- .usda_or3(.mo_ge(vm, 4), .mo_ge(vd, 6), .mo_ge(cm, 4))
  .sga_res(nm, p, L, list(mixed_cm = c(ms, hi), value_moist = vm, value_dry = vd,
                          chroma_moist = cm),
           c("munsell_value_moist", "munsell_value_dry", "munsell_chroma_moist"), ref)
}


# ---- Aeric ---------------------------------------------------------------

# Layers between the lower boundary of the A or Ap horizon and `hi` (cm).
.sga_below_a <- function(pedon, hi) {
  h <- pedon$horizons
  ms <- .sga_ms(pedon)
  d <- .usda_desig(h$designation)
  isA <- !is.na(d$master) & grepl("^A", d$master) & !grepl("/", d$master)
  o <- order(h$top_cm)
  abot <- ms
  for (i in o) {
    if (is.na(h$top_cm[i]) || h$top_cm[i] < ms - 1e-6) next
    if (isA[i] && abs(h$top_cm[i] - abot) < 1e-6 && !is.na(h$bottom_cm[i])) abot <- h$bottom_cm[i]
    else if (!isA[i]) break
  }
  if (abot <= ms) abot <- ms
  L <- .sga_in(h, abot, hi)
  list(layers = L[!isA[L]], from = abot)
}

# The colours of item 2 of the Aeric subgroups of the Aqualfs (e.g. p. 76),
# one layer: hue 7.5YR or redder and chroma 2 or more; or hue 10YR or
# yellower and either value 3 or more and chroma 3 or more, or chroma 2 or
# more with no redox concentrations. `dry_red`, `dry_yellow`: the chroma is
# read moist and dry ("(both moist and dry)" in the text of the great group).
.sga_aeric_layer <- function(h, i, dry_red = FALSE, dry_yellow = FALSE) {
  red <- .hue_redder_than(h$munsell_hue_moist[i], "7.5YR")
  vm <- .sga_num(h, "munsell_value_moist")[i]
  cm <- .sga_num(h, "munsell_chroma_moist")[i]
  cd <- .sga_num(h, "munsell_chroma_dry")[i]
  conc <- .sga_redox_conc(h)[i]
  ch <- function(t, dry) {
    m <- if (is.na(cm)) NA else cm >= t
    if (!dry) return(m)
    .and3(m, if (is.na(cd)) NA else cd >= t)
  }
  rb <- .and3(red, ch(2, dry_red))
  yb <- .and3(if (is.na(red)) NA else !red,
              .usda_or3(.and3(if (is.na(vm)) NA else vm >= 3, ch(3, dry_yellow)),
                        .and3(ch(2, dry_yellow), !conc)))
  .usda_or3(rb, yb)
}

#' Aeric subgroups of the Aqualfs (KST 13th ed., pp. 74-81)
#'
#' \describe{
#'   \item{\code{"aqualf"}}{in one or more horizons between the A or Ap
#'     horizon and 75 cm, in 50 \% or more of the matrix, hue 7.5YR or redder
#'     with chroma 2 or more, or hue 10YR or yellower with value 3 or more
#'     and chroma 3 or more, or chroma 2 or more and no redox
#'     concentrations (Endo-, Epi-, Glossa-, Kandiaqualfs, pp. 76-80). The
#'     matrix colour stands for "50 percent or more of the matrix" and for
#'     the ped faces. \code{dry_red} and \code{dry_yellow} add the dry chroma
#'     where the text of the great group says "moist and dry".}
#'   \item{\code{"albaqualf"}}{chroma of 3 or more in 40 \% or more of the
#'     matrix between the lower boundary of the A or Ap horizon and 75 cm
#'     (Albaqualfs, p. 74), read as a share of the thickness.}
#'   \item{\code{"fragiaqualf"}}{between the A or Ap horizon and the
#'     fragipan, a horizon with 50 \% or more chroma of 3 or more (hue 10YR
#'     or redder) or of 4 or more (hue 2.5Y or yellower) (p. 79).}
#' }
#' @param pedon A \code{\link{PedonRecord}}.
#' @param variant One of the readings above.
#' @param dry_red,dry_yellow Read the chroma moist and dry.
#' @noRd
sg_alfisols_aeric_usda <- function(pedon, variant = "aqualf", dry_red = FALSE,
                                   dry_yellow = FALSE) {
  variant <- match.arg(variant, c("aqualf", "albaqualf", "fragiaqualf"))
  h <- pedon$horizons
  nm <- "sg_alfisols_aeric_usda"
  ref <- .sga_ref("Aeric subgroups of the Aqualfs (pp. 74-81)")
  if (!nrow(h)) return(.sga_res(nm, FALSE, ref = ref))
  ms <- .sga_ms(pedon)
  miss <- c("munsell_hue_moist", "munsell_value_moist", "munsell_chroma_moist",
            if (dry_red || dry_yellow) "munsell_chroma_dry")
  if (variant == "albaqualf") {
    z <- .sga_below_a(pedon, ms + 75)
    L <- z$layers
    if (!length(L)) return(.sga_res(nm, FALSE, evidence = list(reason = "no layer below the A horizon within 75 cm"), ref = ref))
    cm <- .sga_num(h, "munsell_chroma_moist")[L]
    p <- .sga_share(.sga_ge(cm, 3), .sga_clip(h, L, z$from, ms + 75), 0.4)
    return(.sga_res(nm, p, L, list(variant = variant, zone_cm = c(z$from, ms + 75)), miss, ref))
  }
  if (variant == "fragiaqualf") {
    fl <- .sga_fragic_layers(pedon)
    fp <- fragipan_usda(pedon, max_top_cm = Inf)
    fl <- unique(c(fl, if (isTRUE(fp$passed)) fp$layers))
    if (!length(fl)) return(.sga_res(nm, NA, evidence = list(reason = "fragipan not located"),
                                     missing = "designation", ref = ref))
    z <- .sga_below_a(pedon, min(h$top_cm[fl]))
    L <- setdiff(z$layers, fl)
    hue <- .mo_hue_rank(h$munsell_hue_moist[L])
    cm <- .sga_num(h, "munsell_chroma_moist")[L]
    # a chroma of 4 or more passes and one under 3 fails whatever the hue
    ok <- ifelse(!is.na(cm) & cm >= 4, TRUE, ifelse(!is.na(cm) & cm < 3, FALSE,
                 ifelse(is.na(hue) | is.na(cm), NA, hue <= 5)))
    p <-if (!length(L)) FALSE else .any3u(ok)
    return(.sga_res(nm, p, L[ok %in% TRUE], list(variant = variant), miss, ref))
  }
  z <- .sga_below_a(pedon, ms + 75)
  L <- z$layers
  ok <- vapply(L, function(i) .sga_aeric_layer(h, i, dry_red, dry_yellow), NA)
  p <- if (!length(L)) FALSE else .any3u(ok)
  .sga_res(nm, p, L[ok %in% TRUE], list(variant = variant, zone_cm = c(z$from, ms + 75)),
           miss, ref)
}


# ---- Andic, Vitrandic, Aquandic -------------------------------------------

#' Andic, Vitrandic and Aquandic subgroups of the Alfisols (KST 13th ed.)
#'
#' Throughout one or more horizons with a total thickness of 18 cm or more
#' within 75 cm of the mineral soil surface, the andic criterion (bulk
#' density 1.0 or less and Al + 1/2 Fe by oxalate over 1.0), the vitrandic
#' one (cinders and pumice, or volcanic glass), or either (\code{"either"},
#' the Aquandic subgroups), read with the helpers of
#' \code{andic_subgroup_usda()} and \code{vitrandic_subgroup_usda()}. With
#' \code{aquic}, the aquic conditions of the Aquandic subgroup of the great
#' group: \code{"surface75"} (within 75 cm, Haploxeralfs and Palexeralfs,
#' pp. 111, 113) or \code{"upper25"} (upper 25 cm of the argillic horizon,
#' Glossudalfs, p. 88). The Aquandic subgroups of the Aqualfs (pp. 74-77)
#' have no aquic item.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param which "andic", "vitrandic" or "either".
#' @param aquic "none", "surface75" or "upper25".
#' @noRd
sg_alfisols_andic_usda <- function(pedon, which = "andic", aquic = "none") {
  which <- match.arg(which, c("andic", "vitrandic", "either"))
  aquic <- match.arg(aquic, c("none", "surface75", "upper25"))
  h <- pedon$horizons
  nm <- "sg_alfisols_andic_usda"
  ref <- .sga_ref("Andic, Vitrandic and Aquandic subgroups (e.g. pp. 74, 88, 111)")
  if (!nrow(h)) return(.sga_res(nm, FALSE, ref = ref))
  L <- .andic_subgroup_layers_usda(h)
  sel <- switch(which, andic = L$andic, vitrandic = L$vitrandic, either = L$andic | L$vitrandic)
  an <- .andic_subgroup_18_usda(h, sel)
  miss <- switch(which, andic = L$missing_andic, vitrandic = L$missing_vitrandic,
                 either = c(L$missing_andic, L$missing_vitrandic))
  aq <- switch(aquic, none = NULL,
               surface75 = sg_alfisols_aquic_usda(pedon, "surface", 75),
               upper25 = sg_alfisols_aquic_usda(pedon, "upper25", horizon = "argillic"))
  p <- if (is.null(aq)) an$passed else .and3(an$passed, aq$passed)
  .sga_res(nm, p, c(an$layers, if (!is.null(aq)) aq$layers),
           list(which = which, aquic = aquic, thickness_cm = an$thickness_cm),
           c(if (is.na(an$passed)) miss, if (!is.null(aq) && is.na(aq$passed)) aq$missing), ref)
}


# ---- Mollic, Umbric and dark surface colours --------------------------------

#' Mollic and Umbric subgroups of the Alfisols (KST 13th ed.)
#'
#' Read from the mixed upper 18 cm of \code{mollic_epipedon_usda()}:
#' \describe{
#'   \item{\code{kind = "mollic", what = "all"}}{a mollic epipedon, or the
#'     upper 18 cm meet all the requirements of one except thickness after
#'     mixing (e.g. Mollic Albaqualfs, p. 74).}
#'   \item{\code{kind = "mollic", what = "colour"}}{a mollic epipedon, or the
#'     upper 18 cm meet its colour requirements after mixing (e.g. Mollic
#'     Natraqualfs, p. 81; Mollic Hapludalfs, p. 93).}
#'   \item{\code{kind = "umbric"}}{the same for the umbric epipedon: colour,
#'     organic carbon and structure of the mollic, base saturation (NH4OAc)
#'     below 50 \% (e.g. Umbric Albaqualfs, p. 75; "colour" in Umbric
#'     Kandiaqualfs, p. 81).}
#' }
#' @param pedon A \code{\link{PedonRecord}}.
#' @param kind "mollic" or "umbric".
#' @param what "all" or "colour".
#' @noRd
sg_alfisols_epipedon_usda <- function(pedon, kind = "mollic", what = "all") {
  kind <- match.arg(kind, c("mollic", "umbric"))
  what <- match.arg(what, c("all", "colour"))
  nm <- "sg_alfisols_epipedon_usda"
  ref <- .sga_ref("Mollic and Umbric subgroups (e.g. pp. 74, 81, 93)")
  mo <- mollic_epipedon_usda(pedon)
  ev <- mo$evidence
  if (is.null(ev$colour)) return(.sga_res(nm, FALSE, evidence = list(kind = kind, what = what,
                                      reason = ev$reason %||% "no mineral surface layer"), ref = ref))
  miss <- mo$missing %||% character(0)
  if (!length(miss)) miss <- c("munsell_value_moist", "munsell_value_dry", "oc_pct", "bs_pct")
  p <- if (kind == "mollic") {
    if (what == "all") .usda_or3(mo$passed, ev$upper_part) else .usda_or3(mo$passed, ev$colour)
  } else {
    if (what == "colour") ev$colour
    else {
      b <- ev$mixed$bs_pct
      low <- if (b[2] < 50) TRUE else if (b[1] >= 50) FALSE else NA
      .and3(ev$colour, ev$organic_carbon, ev$structure, low)
    }
  }
  .sga_res(nm, p, mo$layers, list(kind = kind, what = what, mixed_cm = ev$mix_depth_cm,
                                  colour = ev$colour, upper_part = ev$upper_part),
           miss, ref)
}

#' Dark surface colour of the Alfisol subgroups (KST 13th ed.)
#'
#' A moist value of 3 or less and a dry value of 5 or less (crushed and
#' smoothed) throughout the upper 18 cm of the mineral soil, unmixed, or
#' between the surface and 18 cm after mixing (Humic Fragiaqualfs, p. 79;
#' Mollic and Umbric Cryalfs, pp. 82-86; Aquollic Hapludalfs, p. 92; Mollic
#' Fragixeralfs, p. 111). Mixing passes whenever the unmixed layers do, so
#' the mixed means decide. A dry value not recorded is the moist value + 1
#' where the moist value is 2 or less (reading L27).
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_alfisols_dark_usda <- function(pedon) {
  h <- pedon$horizons
  nm <- "sg_alfisols_dark_usda"
  ref <- .sga_ref("dark surface subgroups (e.g. pp. 79, 83, 111)")
  if (!nrow(h)) return(.sga_res(nm, FALSE, ref = ref))
  ms <- .sga_ms(pedon)
  L <- .sga_in(h, ms, ms + 18)
  if (!length(L)) return(.sga_res(nm, NA, missing = "munsell_value_moist", ref = ref))
  w <- .sga_clip(h, L, ms, ms + 18)
  vd <- .mo_dry_value(h)$value
  vm <- .mo_wrange(.sga_num(h, "munsell_value_moist")[L], w, 0, 10)
  vdr <- .mo_wrange(vd[L], w, 0, 10)
  p <- .and3(.mo_le(vm, 3), .mo_le(vdr, 5))
  .sga_res(nm, p, L, list(value_moist = vm, value_dry = vdr),
           c("munsell_value_moist", "munsell_value_dry"), ref)
}

#' Mollic Haploxeralfs and Palexeralfs (KST 13th ed., pp. 112, 115)
#'
#' A moist value of 3 or less and 0.7 \% or more organic carbon, throughout
#' the upper 10 cm of the mineral soil (unmixed) or throughout the upper 18
#' cm after mixing. With \code{lithic = TRUE} (Lithic Mollic Haploxeralfs,
#' p. 111), throughout an Ap horizon or the upper 10 cm of an A horizon, and
#' a lithic contact within 50 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param lithic The Lithic Mollic reading.
#' @noRd
sg_alfisols_mollic_xeralf_usda <- function(pedon, lithic = FALSE) {
  h <- pedon$horizons
  nm <- "sg_alfisols_mollic_xeralf_usda"
  ref <- .sga_ref("Mollic Haploxeralfs and Palexeralfs (pp. 111-115)")
  if (!nrow(h)) return(.sga_res(nm, FALSE, ref = ref))
  ms <- .sga_ms(pedon)
  vmv <- .sga_num(h, "munsell_value_moist"); ocv <- .sga_num(h, "oc_pct")
  one <- function(i) .and3(if (is.na(vmv[i])) NA else vmv[i] <= 3,
                           if (is.na(ocv[i])) NA else .kst_round(ocv[i], 1) >= 0.7)
  d <- .usda_desig(h$designation)
  if (isTRUE(lithic)) {
    ap <- which(!is.na(d$master) & d$master == "A" & grepl("p", d$suffix))
    U <- if (length(ap)) ap else .sga_in(h, ms, ms + 10)
    p <- .and3(vapply(U, one, NA))
    li <- lithic_contact_usda(pedon, max_top_cm = 50)
    p <- .and3(li$passed, p)
    return(.sga_res(nm, p, U, list(lithic = li$passed), c("munsell_value_moist", "oc_pct"), ref))
  }
  U <- .sga_in(h, ms, ms + 10)
  unmixed <- if (length(U)) .and3(vapply(U, one, NA)) else NA
  M <- .sga_in(h, ms, ms + 18)
  w <- .sga_clip(h, M, ms, ms + 18)
  mixed <- if (length(M)) .and3(.mo_le(.mo_wrange(vmv[M], w, 0, 10), 3),
                               .mo_ge(.mo_wrange(ocv[M], w, 0, 100), 0.7)) else NA
  p <- .usda_or3(unmixed, mixed)
  .sga_res(nm, p, unique(c(U, M)), list(unmixed_10cm = unmixed, mixed_18cm = mixed),
           c("munsell_value_moist", "oc_pct"), ref)
}


# ---- Fragic soil properties ------------------------------------------------

# Layers with fragic soil properties as the record shows them: an x in the
# designation, a fragipan recorded as a pan, or the surveyor's fragipan
# (NASIS). A field observation: not recorded, absent.
.sga_fragic_layers <- function(pedon) {
  h <- pedon$horizons
  if (!nrow(h)) return(integer(0))
  d <- .usda_desig(h$designation)
  x <- !is.na(d$suffix) & grepl("x", d$suffix)
  pan <- .field_term(.sga_chr(h, "pan_type"))
  fp <- !is.na(pan) & grepl("fragipan", pan)
  which((x | fp) & !is.na(h$top_cm))
}

#' Fragic subgroups of the Alfisols (KST 13th ed., e.g. p. 77)
#'
#' Fragic soil properties in 30 \% or more of the volume of a layer 15 cm or
#' more thick with its upper boundary within 100 cm, or in 60 \% or more of a
#' layer 15 cm thick anywhere. The record does not give the share of the
#' volume: a layer designated x (or a recorded fragipan) has fragic soil
#' properties in most of its volume, so 15 cm of such adjoining layers pass.
#' A field observation: not recorded, absent. (Until v0.9.421 the Fragic
#' subgroups read \code{fragipan_usda()}, a firm rupture resistance.)
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_alfisols_fragic_usda <- function(pedon) {
  h <- pedon$horizons
  L <- .sga_fragic_layers(pedon)
  p <- .sga_run_thickness(h, L) >= 15 - 1e-6
  if (!p && .has_nasis_feature(pedon, "fragipan")) p <- TRUE
  .sga_res("sg_alfisols_fragic_usda", p, L, list(fragic_layers = L),
           ref = .sga_ref("Fragic subgroups (e.g. p. 77)"))
}


# ---- Arenic and Grossarenic ------------------------------------------------

#' Arenic and Grossarenic subgroups of the Alfisols (KST 13th ed., Ch. 5)
#'
#' A texture class (fine earth) of loamy fine sand or coarser throughout a
#' layer from the mineral soil surface to the top of the \code{horizon}, whose
#' upper boundary is at a depth from \code{min_cm} to \code{max_cm} (50 cm or
#' more; 50 to 100 cm; 100 cm or more) below the mineral soil surface.
#' Arenic subgroups outside the Spodosols read the argillic or kandic
#' horizon, not the spodic one (audit item F16). The laboratory texture is
#' read first, the field class where clay and sand are not measured
#' (\code{.usda_lfs_or_coarser()}).
#' @param pedon A \code{\link{PedonRecord}}.
#' @param horizon "argillic", "kandic" or "ak".
#' @param min_cm,max_cm Depth range of the upper boundary (cm).
#' @noRd
sg_alfisols_arenic_usda <- function(pedon, horizon = "argillic", min_cm = 50, max_cm = Inf) {
  h <- pedon$horizons
  nm <- "sg_alfisols_arenic_usda"
  ref <- .sga_ref("Arenic and Grossarenic subgroups (e.g. pp. 74, 93, 96)")
  if (!nrow(h)) return(.sga_res(nm, FALSE, ref = ref))
  ms <- .sga_ms(pedon)
  sandy <- .usda_lfs_or_coarser(h)
  hz <- .sga_hz(pedon, horizon)
  ev <- list(horizon = horizon, depth_cm = c(min_cm, max_cm))
  if (isTRUE(hz$passed)) {
    t <- hz$top
    depth_ok <- t >= ms + min_cm - 1e-6 && t <= ms + max_cm + 1e-6
    if (!depth_ok) return(.sga_res(nm, FALSE, evidence = c(ev, list(horizon_top_cm = t)), ref = ref))
    A <- .sga_in(h, ms, t)
    p <- .and3(sandy[A])
    return(.sga_res(nm, p, A, c(ev, list(horizon_top_cm = t)), c("clay_pct", "sand_pct"), ref))
  }
  if (isFALSE(hz$passed)) return(.sga_res(nm, FALSE, evidence = ev, ref = ref))
  A <- .sga_in(h, ms, ms + min_cm)
  p <- if (any(sandy[A] %in% FALSE)) FALSE else NA
  .sga_res(nm, p, evidence = c(ev, list(horizon_top_cm = NA)),
           missing = c(hz$missing, "clay_pct", "sand_pct"), ref = ref)
}


# ---- Plinthic, Vermic, Leptic, Anthraquic, Lithic Ruptic -----------------

#' Vermic Fragiaqualfs and Natraqualfs (KST 13th ed., pp. 79, 81)
#'
#' One or more layers at least 25 cm thick (cumulative) within 100 cm with
#' 25 \% or more (by volume) recognizable bioturbation (worm holes, filled
#' burrows, casts), read from \code{worm_holes_pct}. A field observation:
#' not recorded, absent.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_alfisols_vermic_usda <- function(pedon) {
  h <- pedon$horizons
  ms <- .sga_ms(pedon)
  wh <- .sga_num(h, "worm_holes_pct")
  L <- .sga_in(h, ms, ms + 100)
  L <- L[!is.na(wh[L]) & wh[L] >= 25]
  p <- sum(.sga_clip(h, L, ms, ms + 100)) >= 25 - 1e-6
  .sga_res("sg_alfisols_vermic_usda", p, L, list(threshold_pct = 25),
           ref = .sga_ref("Vermic subgroups (pp. 79, 81)"))
}

#' Leptic Natrustalfs (KST 13th ed., p. 105)
#'
#' Visible crystals of gypsum or of salts more soluble than gypsum, or both,
#' within 40 cm of the soil surface: \code{gypsum_visible_pct},
#' \code{halite_visible_pct} or a y or z in the designation. A field
#' observation: not recorded, absent.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_alfisols_leptic_usda <- function(pedon) {
  h <- pedon$horizons
  top0 <- suppressWarnings(min(h$top_cm, na.rm = TRUE)); if (!is.finite(top0)) top0 <- 0
  L <- .sga_in(h, top0, top0 + 40)
  d <- .usda_desig(h$designation)
  yz <- !is.na(d$suffix) & grepl("[yz]", d$suffix)
  gy <- .sga_num(h, "gypsum_visible_pct"); ha <- .sga_num(h, "halite_visible_pct")
  hit <- L[yz[L] | (!is.na(gy[L]) & gy[L] > 0) | (!is.na(ha[L]) & ha[L] > 0)]
  .sga_res("sg_alfisols_leptic_usda", length(hit) > 0L, hit,
           ref = .sga_ref("Leptic Natrustalfs (p. 105)"))
}

#' Anthraquic Hapludalfs and Paleudalfs (KST 13th ed., pp. 91, 95)
#'
#' Anthraquic conditions (anthric saturation of a puddled, flooded soil),
#' read from the record: a flood-irrigated site, or a puddled surface layer
#' (\code{anthraquic()}, the WRB anthraquic horizon). A field observation:
#' not recorded, absent.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_alfisols_anthraquic_usda <- function(pedon) {
  fi <- isTRUE(as.logical(pedon$site$flood_irrigated %||% NA)[1])
  an <- .usda_try(anthraquic(pedon))
  p <- fi || isTRUE(an$passed)
  .sga_res("sg_alfisols_anthraquic_usda", p, if (!is.null(an)) an$layers,
           list(flood_irrigated = fi), ref = .sga_ref("Anthraquic subgroups (pp. 91, 95)"))
}

# Horizontal discontinuity of the horizon recorded (boundary topography
# broken or discontinuous). A field observation.
.sga_discontinuous <- function(h, L) {
  bt <- .field_term(.sga_chr(h, "boundary_topography"))
  any(!is.na(bt[L]) & grepl("broken|discontinuous", bt[L]))
}


# ---- Sodium --------------------------------------------------------------

#' Sodium criteria of the Alfisol subgroups (KST 13th ed.)
#'
#' \describe{
#'   \item{\code{"albic"}}{ESP below 15 and Mg + Na below Ca + extractable
#'     acidity, throughout the upper 15 cm of the natric horizon or in all
#'     horizons within 40 cm, whichever is deeper (Albic Natraqualfs, p. 81).
#'     The extractable acidity at pH 8.2 is rarely recorded: where Mg + Na is
#'     already below Ca it is not needed.}
#'   \item{\code{"vermaqualf"}}{ESP 7 or more (or SAR 6 or more) throughout
#'     the upper 15 cm of the argillic horizon, or throughout all horizons
#'     within 40 cm (Natric Vermaqualfs, p. 82).}
#'   \item{\code{"argillic"}}{ESP 15 or more (or SAR 13 or more) in one or
#'     more subhorizons of the argillic or kandic horizon (Natric
#'     Haploxeralfs, p. 112).}
#'   \item{\code{"within100"}}{ESP 15 or more (SAR 13) in one or more
#'     horizons within 100 cm (Natric Palexeralfs, p. 114).}
#'   \item{\code{"haplargidic"}}{ESP below 15 (or SAR below 13) in 50 \% or
#'     more of the natric horizon (Haplargidic Natrustalfs, p. 105).}
#' }
#' ESP is \code{esp_pct}, else 100 Na / CEC.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param variant One of the readings above.
#' @noRd
sg_alfisols_sodium_usda <- function(pedon, variant = "within100") {
  variant <- match.arg(variant, c("albic", "vermaqualf", "argillic", "within100", "haplargidic"))
  h <- pedon$horizons
  nm <- "sg_alfisols_sodium_usda"
  ref <- .sga_ref("sodium in the subgroups (pp. 81, 82, 105, 112, 114)")
  if (!nrow(h)) return(.sga_res(nm, FALSE, ref = ref))
  ms <- .sga_ms(pedon)
  esp <- .usda_esp(h); sar <- .sga_num(h, "sar")
  hi_na <- function(i, e, s) .usda_or3(.sga_ge(.kst_round(esp[i]), e), .sga_ge(sar[i], s))
  miss <- c("esp_pct", "na_cmol", "cec_cmol")
  ev <- list(variant = variant)
  if (variant == "within100") {
    L <- .sga_in(h, ms, ms + 100)
    ok <- vapply(L, function(i) hi_na(i, 15, 13), NA)
    return(.sga_res(nm, .any3u(ok), L[ok %in% TRUE], ev, miss, ref))
  }
  if (variant == "argillic") {
    hz <- .sga_hz(pedon, "ak")
    if (!isTRUE(hz$passed)) return(.sga_res(nm, hz$passed, evidence = ev, missing = hz$missing, ref = ref))
    ok <- vapply(hz$layers, function(i) hi_na(i, 15, 13), NA)
    return(.sga_res(nm, .any3u(ok), hz$layers[ok %in% TRUE], ev, miss, ref))
  }
  if (variant == "haplargidic") {
    hz <- .sga_hz(pedon, "natric")
    if (!isTRUE(hz$passed)) return(.sga_res(nm, hz$passed, evidence = ev, missing = hz$missing, ref = ref))
    L <- hz$layers
    low <- vapply(L, function(i) { x <- hi_na(i, 15, 13); if (is.na(x)) NA else !x }, NA)
    return(.sga_res(nm, .sga_share(low, .sga_clip(h, L), 0.5), L, ev, miss, ref))
  }
  if (variant == "vermaqualf") {
    hz <- .sga_hz(pedon, "argillic")
    a <- if (isTRUE(hz$passed)) {
      U <- .sga_in(h, hz$top, hz$top + 15)
      .and3(vapply(U, function(i) hi_na(i, 7, 6), NA))
    } else if (isFALSE(hz$passed)) FALSE else NA
    B <- .sga_in(h, ms, ms + 40)
    b <- .and3(vapply(B, function(i) hi_na(i, 7, 6), NA))
    return(.sga_res(nm, .usda_or3(a, b), B, ev, c(miss, hz$missing), ref))
  }
  # albic
  hz <- .sga_hz(pedon, "natric")
  if (!isTRUE(hz$passed)) return(.sga_res(nm, hz$passed, evidence = ev, missing = hz$missing, ref = ref))
  Z <- if (hz$top + 15 > ms + 40) .sga_in(h, hz$top, hz$top + 15) else .sga_in(h, ms, ms + 40)
  ca <- .sga_num(h, "ca_cmol"); mg <- .sga_num(h, "mg_cmol"); na <- .sga_num(h, "na_cmol")
  acid <- .sga_num(h, "extractable_acidity_ph82_cmol")
  ok <- vapply(Z, function(i) {
    e <- if (is.na(esp[i])) NA else .kst_round(esp[i]) < 15
    mn <- mg[i] + na[i]
    cmp <- if (is.na(mn) || is.na(ca[i])) NA
           else if (mn < ca[i]) TRUE
           else if (!is.na(acid[i])) mn < ca[i] + acid[i] else NA
    .and3(e, cmp)
  }, NA)
  .sga_res(nm, .and3(ok), Z, c(ev, list(zone = "deeper of the upper 15 cm of the natric horizon and 0-40 cm")),
           c(miss, "ca_cmol", "mg_cmol", "extractable_acidity_ph82_cmol"), ref)
}


# ---- Glossic, interfingering, albic materials --------------------------------

# Layers recorded as the glossic horizon or as tongues of albic materials:
# designations E/B, B/E (with suffixes, e.g. E/Bt, Bt/E), and the albic
# tongues or glossic share where a column records them. A field observation.
.sga_glossic_layers <- function(h) {
  d <- .sga_chr(h, "designation")
  core <- sub("^\\^?[0-9]*", "", d)
  hit <- !is.na(core) & grepl("^(E[a-z0-9]*/B|B[a-z0-9]*/E)", core)
  tg <- .sga_num(h, "albic_tongues_pct")
  which((hit | (!is.na(tg) & tg > 0)) & !is.na(h$top_cm))
}

#' Glossic and albic-material criteria of the Alfisol subgroups
#'
#' Read from the designations (E/B, B/E: the glossic horizon and tongues of
#' albic materials into the argillic or natric horizon, KST 13th ed., p. 25)
#' and, for the Paleudalfs, skeletans (\code{skeletans_pct}) of chroma 2 or
#' less in the upper part of the argillic horizon. Field observations: not
#' recorded, absent.
#' \describe{
#'   \item{\code{"glossic_or_tongues"}}{a glossic horizon or interfingering
#'     of albic materials into the natric (or argillic) horizon (Glossic
#'     Natraqualfs, p. 81; Glossaquic Natrudalfs, p. 95).}
#'   \item{\code{"tongues"}}{interfingering of albic materials in one or
#'     more subhorizons (or the upper part) of the argillic horizon (Glossic
#'     and Glossaquic Hapludalfs, p. 92).}
#'   \item{\code{"haplic"}}{a glossic horizon less than 50 cm thick (Haplic
#'     Glossudalfs, p. 90).}
#'   \item{\code{"paleudalf"}}{a glossic horizon, or 5 \% or more skeletans
#'     (clay depletions) of chroma 2 or less in the upper part of the argillic
#'     horizon, or albic materials in some subhorizon of it (Glossic and
#'     Glossaquic Paleudalfs, pp. 95-96).}
#' }
#' @param pedon A \code{\link{PedonRecord}}.
#' @param variant One of the readings above.
#' @noRd
sg_alfisols_glossic_usda <- function(pedon, variant = "glossic_or_tongues") {
  variant <- match.arg(variant, c("glossic_or_tongues", "tongues", "haplic", "paleudalf"))
  h <- pedon$horizons
  nm <- "sg_alfisols_glossic_usda"
  ref <- .sga_ref("Glossic subgroups (pp. 81, 90, 92, 95-96)")
  if (!nrow(h)) return(.sga_res(nm, FALSE, ref = ref))
  G <- .sga_glossic_layers(h)
  thk <- .sga_run_thickness(h, G)
  if (variant == "haplic") {
    p <- thk >= 5 - 1e-6 && thk < 50
    return(.sga_res(nm, p, G, list(variant = variant, glossic_thickness_cm = thk), ref = ref))
  }
  if (variant == "paleudalf") {
    p <- length(G) > 0L
    if (!p) {
      hz <- .sga_hz(pedon, "argillic")
      if (isTRUE(hz$passed)) {
        sk <- .sga_num(h, "skeletans_pct")
        U <- .sga_in(h, hz$top, hz$top + 25)
        p <- any(!is.na(sk[U]) & sk[U] >= 5)
      }
    }
    return(.sga_res(nm, p, G, list(variant = variant), ref = ref))
  }
  .sga_res(nm, length(G) > 0L, G, list(variant = variant, glossic_thickness_cm = thk), ref = ref)
}


# ---- Abrupt textural change and clay increases ----------------------------

# The layer directly above the top of the horizon (the eluvial one) and the
# upper layer of the horizon.
.sga_boundary_pair <- function(h, hz) {
  top <- hz$layers[1]
  over <- which(!is.na(h$bottom_cm) & abs(h$bottom_cm - h$top_cm[top]) < 1e-6)
  list(over = if (length(over)) over[1] else NA_integer_, here = top)
}

# Distance class of a boundary: "le2.5" (abrupt or very abrupt, under 2 cm),
# "2-7.5" (clear, 2-5 cm), "gt7.5" (gradual, diffuse), NA.
.sga_bd_class <- function(bd) {
  bd <- .field_term(bd)
  if (is.na(bd) || !nzchar(bd)) return(NA_character_)
  if (grepl("abrupt|sharp", bd)) return("le2.5")
  if (grepl("clear", bd)) return("2-7.5")
  if (grepl("gradual|diffuse", bd)) return("gt7.5")
  NA_character_
}

#' Abrupt textural change and clay increases of the Alfisol subgroups
#'
#' At the upper boundary of the \code{horizon} (KST 13th ed.):
#' \describe{
#'   \item{\code{"atc"}}{an abrupt textural change (p. 31; Albaquultic and
#'     Albaquic Hapludalfs, p. 92), \code{.abrupt_textural_change_usda()}.}
#'   \item{\code{"inc15"}}{a clay increase of 15 \% (absolute) or more within
#'     2.5 cm (Albaquic Paleudalfs, p. 95): an abrupt boundary (under 2 cm).}
#'   \item{\code{"abruptic"}}{35 \% or more noncarbonate clay in
#'     subhorizons of the argillic horizon totaling 7.5 cm or more, and a clay
#'     increase of 20 \% within 7.5 cm or 15 \% within 2.5 cm, or an abrupt
#'     textural change (Abruptic Durixeralfs, p. 110).}
#'   \item{\code{"haplic"}}{less than 35 \% clay throughout the subhorizons
#'     within 15 cm of the upper boundary, or a clay increase there of less
#'     than 20 \% within 7.5 cm and less than 15 \% within 2.5 cm (Haplic
#'     Palexeralfs, p. 115).}
#' }
#' The boundary distinctness gives the distance (abrupt under 2 cm, clear 2
#' to 5 cm, gradual or diffuse more); not recorded, undecided.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param variant One of the readings above.
#' @param horizon "argillic" or "ak".
#' @noRd
sg_alfisols_abrupt_usda <- function(pedon, variant = "atc", horizon = "argillic") {
  variant <- match.arg(variant, c("atc", "inc15", "abruptic", "haplic"))
  h <- pedon$horizons
  nm <- "sg_alfisols_abrupt_usda"
  ref <- .sga_ref("abrupt clay increases (pp. 31, 92, 95, 110, 115)")
  hz <- .sga_hz(pedon, horizon)
  if (!isTRUE(hz$passed)) return(.sga_res(nm, hz$passed, evidence = list(variant = variant),
                                          missing = hz$missing, ref = ref))
  bp <- .sga_boundary_pair(h, hz)
  clay <- .sga_num(h, "clay_pct")
  bdv <- .sga_chr(h, "boundary_distinctness")
  over <- if (is.na(bp$over)) NA_real_ else clay[bp$over]
  here <- clay[bp$here]
  bd <- if (is.na(bp$over)) NA_character_ else .sga_bd_class(bdv[bp$over])
  d <- here - over
  inc_within <- function(t, cls) {
    if (is.na(d)) return(NA)
    if (.kst_round(d) < t) return(FALSE)
    if (is.na(bd)) return(NA)
    if (cls == "7.5") bd %in% c("le2.5", "2-7.5") else if (bd == "le2.5") TRUE else if (bd == "gt7.5") FALSE else NA
  }
  atc <- if (is.na(bp$over)) NA
         else .abrupt_textural_change_usda(over, here, if (is.na(bp$over)) NA else bdv[bp$over])
  miss <- c("clay_pct", "boundary_distinctness")
  p <- switch(variant,
    atc = atc,
    inc15 = inc_within(15, "2.5"),
    abruptic = {
      cl35 <- hz$layers[!is.na(clay[hz$layers]) & .kst_round(clay[hz$layers]) >= 35]
      c35 <- if (sum(.sga_clip(h, cl35)) >= 7.5 - 1e-6) TRUE
             else if (anyNA(clay[hz$layers])) NA else FALSE
      .and3(c35, .usda_or3(inc_within(20, "7.5"), inc_within(15, "2.5"), atc))
    },
    haplic = {
      U <- .sga_in(h, hz$top, hz$top + 15)
      c1 <- .and3(.sga_lt(.kst_round(clay[U]), 35))
      c2 <- if (is.na(d)) NA
            else if (.kst_round(d) < 15) TRUE
            else if (is.na(bd)) NA
            else if (.kst_round(d) < 20) bd != "le2.5"
            else bd == "gt7.5"
      .usda_or3(c1, c2)
    })
  .sga_res(nm, p, hz$layers, list(variant = variant, clay_over = over, clay_here = here,
                                  boundary = bd), miss, ref)
}


# ---- Duripan coherence -----------------------------------------------------

#' Haplic Durixeralfs (KST 13th ed., p. 110)
#'
#' A duripan that is strongly coherent or less coherent in all subhorizons:
#' no subhorizon of the duripan recorded as indurated (or very strongly
#' cemented). Cementation not recorded in a duripan: undecided.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_alfisols_duripan_coherence_usda <- function(pedon) {
  h <- pedon$horizons
  nm <- "sg_alfisols_duripan_coherence_usda"
  ref <- .sga_ref("Haplic Durixeralfs (p. 110)")
  du <- duripan_usda(pedon, max_top_cm = Inf)
  if (!isTRUE(du$passed)) return(.sga_res(nm, du$passed, missing = du$missing, ref = ref))
  cem <- .field_term(.sga_chr(h, "cementation_class"))[du$layers]
  pan <- .field_term(.sga_chr(h, "pan_type"))[du$layers]
  ind <- (!is.na(cem) & grepl("indurated|very strongly", cem))
  p <- if (any(ind)) FALSE else if (all(!is.na(cem))) TRUE else NA
  .sga_res(nm, p, du$layers, list(cementation = cem, pan = pan), "cementation_class", ref)
}


# ---- Psammentic, Inceptic, Lamellic ---------------------------------------

#' Psammentic subgroups of the Alfisols (KST 13th ed., e.g. p. 92)
#'
#' A sandy particle-size class (sandy or sandy-skeletal with
#' \code{skeletal = TRUE}, Psammentic Haplocryalfs, p. 84) throughout the
#' upper 75 cm of the \code{horizon}, or all of it if thinner. Sandy: loamy
#' fine sand or coarser (\code{.usda_lfs_or_coarser()}) with less than 35 \%
#' rock fragments (not recorded: none).
#' @param pedon A \code{\link{PedonRecord}}.
#' @param horizon "argillic" or "akn".
#' @param skeletal Accept the sandy-skeletal class.
#' @noRd
sg_alfisols_psammentic_usda <- function(pedon, horizon = "argillic", skeletal = FALSE) {
  h <- pedon$horizons
  nm <- "sg_alfisols_psammentic_usda"
  ref <- .sga_ref("Psammentic subgroups (e.g. pp. 84, 92)")
  hz <- .sga_hz(pedon, horizon)
  if (!isTRUE(hz$passed)) return(.sga_res(nm, hz$passed, missing = hz$missing, ref = ref))
  L <- intersect(hz$layers, .sga_in(h, hz$top, hz$top + 75))
  sandy <- .usda_lfs_or_coarser(h)[L]
  if (!skeletal) {
    rf <- .sga_num(h, "coarse_fragments_pct")[L]
    sandy <- sandy & (is.na(rf) | rf < 35)
  }
  .sga_res(nm, .and3(sandy), L, list(horizon = hz$kind), c("clay_pct", "sand_pct"), ref)
}

#' Inceptic subgroups of the Alfisols (KST 13th ed., e.g. p. 93)
#'
#' A \code{horizon} 35 cm or less thick (less than 35 cm, or discontinuous
#' horizontally, with \code{rhodoxeralf = TRUE}: Inceptic Rhodoxeralfs,
#' p. 117) and, except in the Rhodoxeralfs, no densic, lithic or paralithic
#' contact within 100 cm. The thickness is that of the run of layers that
#' \code{argillic_usda()} (or the kandic, natric horizon) finds; it is the
#' whole horizon where the layer directly below cannot be argillic (a C, R,
#' A or E horizon, or clay films recorded as none), and undecided where the
#' profile ends, a gap follows, or the layer below could go on with it.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param horizon "argillic", "ak" or "akn".
#' @param rhodoxeralf The Rhodoxeralf reading.
#' @noRd
sg_alfisols_inceptic_usda <- function(pedon, horizon = "argillic", rhodoxeralf = FALSE) {
  h <- pedon$horizons
  nm <- "sg_alfisols_inceptic_usda"
  ref <- .sga_ref("Inceptic subgroups (e.g. pp. 84, 93, 101, 112, 117)")
  hz <- .sga_hz(pedon, horizon)
  if (!isTRUE(hz$passed)) return(.sga_res(nm, hz$passed, missing = hz$missing, ref = ref))
  thk <- hz$bottom - hz$top
  thin <- if (is.na(thk)) NA else if (rhodoxeralf) thk < 35 else thk <= 35
  # a thin run is the whole horizon only where a layer directly below it
  # ends it; where the profile ends there, or a gap follows, the horizon may
  # go on (undecided)
  if (isTRUE(thin)) {
    nxt <- which(!is.na(h$top_cm) & h$top_cm >= hz$bottom - 1e-6)
    nxt <- nxt[order(h$top_cm[nxt])]
    if (!length(nxt) || h$top_cm[nxt[1]] > hz$bottom + 1e-6) thin <- NA
    else {
      # the layer below ends it when it cannot be argillic: a C, R, A or E
      # horizon, or one with clay films recorded as none and no t
      j <- nxt[1]
      d <- .usda_desig(h$designation[j])
      films <- .clay_films_class(.sga_chr(h, "clay_films_amount")[j])
      ends <- (!is.na(d$master) && grepl("^(C|R|A|E)", d$master)) ||
        (!is.na(films) && films == 0L && !grepl("t", d$suffix %||% ""))
      if (!ends) thin <- NA
    }
  }
  if (rhodoxeralf) {
    p <- .usda_or3(thin, .sga_discontinuous(h, hz$layers))
  } else {
    ct <- .sga_contact(pedon)
    ms <- .sga_ms(pedon)
    p <- .and3(thin, is.na(ct) || ct >= ms + 100)
  }
  .sga_res(nm, p, hz$layers, list(horizon = hz$kind, thickness_cm = thk), "bottom_cm", ref)
}

#' Lithic Ruptic-Inceptic Haploxeralfs (KST 13th ed., p. 111)
#'
#' A lithic contact within 50 cm and an argillic or kandic horizon that is
#' discontinuous horizontally (boundary topography recorded as broken or
#' discontinuous; a field observation, absent when not recorded).
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_alfisols_ruptic_usda <- function(pedon) {
  h <- pedon$horizons
  hz <- .sga_hz(pedon, "ak")
  li <- lithic_contact_usda(pedon, max_top_cm = 50)
  disc <- if (isTRUE(hz$passed)) .sga_discontinuous(h, hz$layers) else if (isFALSE(hz$passed)) FALSE else NA
  p <- .and3(li$passed, disc)
  .sga_res("sg_alfisols_ruptic_usda", p, hz$layers, list(lithic = li$passed), hz$missing,
           .sga_ref("Lithic Ruptic-Inceptic Haploxeralfs (p. 111)"))
}

#' Inceptic Fragixeralfs (KST 13th ed., p. 111)
#'
#' Above the fragipan, no argillic or kandic horizon with clay films on both
#' vertical and horizontal faces of peds. The faces are not recorded: clay
#' films recorded in the horizon above the fragipan (few or more) are read
#' as films on the faces; recorded as none, as their absence.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_alfisols_inceptic_fragixeralf_usda <- function(pedon) {
  h <- pedon$horizons
  nm <- "sg_alfisols_inceptic_fragixeralf_usda"
  ref <- .sga_ref("Inceptic Fragixeralfs (p. 111)")
  hz <- .sga_hz(pedon, "ak")
  if (isFALSE(hz$passed)) return(.sga_res(nm, TRUE, ref = ref))
  if (is.na(hz$passed)) return(.sga_res(nm, NA, missing = hz$missing, ref = ref))
  fl <- .sga_fragic_layers(pedon)
  ft <- if (length(fl)) min(h$top_cm[fl]) else Inf
  A <- hz$layers[h$top_cm[hz$layers] < ft]
  if (!length(A)) return(.sga_res(nm, TRUE, ref = ref))
  films <- .clay_films_class(.sga_chr(h, "clay_films_amount"))[A]
  p <- if (any(!is.na(films) & films >= 1L)) FALSE else if (all(!is.na(films))) TRUE else NA
  .sga_res(nm, p, A, list(clay_films = films), "clay_films_amount", ref)
}

#' Lamellic subgroups of the Alfisols (KST 13th ed., e.g. p. 84)
#'
#' An argillic horizon made of lamellae, or of lamellae and thin argillic
#' parts each under an eluvial horizon, or with two or more lamellae (5 cm
#' or more combined) above its thicker parts. Read from the record: two or
#' more layers designated with lamellae (an ampersand, e.g. E&Bt), or 5 cm
#' or more of lamellae (\code{lamellae_cumulative_cm}). A field observation:
#' not recorded, absent.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_alfisols_lamellic_usda <- function(pedon) {
  h <- pedon$horizons
  d <- .sga_chr(h, "designation")
  amp <- which(!is.na(d) & grepl("&", d, fixed = TRUE))
  lam <- .sga_num(h, "lamellae_cumulative_cm")
  p <- length(amp) >= 2L || sum(lam, na.rm = TRUE) >= 5
  .sga_res("sg_alfisols_lamellic_usda", p, c(amp, which(!is.na(lam) & lam > 0)),
           ref = .sga_ref("Lamellic subgroups (e.g. pp. 84, 92)"))
}


# ---- Base saturation ---------------------------------------------------------

#' Base saturation of the Alfisol subgroups (KST 13th ed.)
#'
#' \describe{
#'   \item{\code{"eutric"}}{NH4OAc base saturation of 50 \% or more in all
#'     parts from the mineral soil surface to 180 cm or a densic, lithic or
#'     paralithic contact (Eutric, Mollic, Xerollic, Ustollic Cryalfs,
#'     pp. 83-86). A profile sampled shallower is read to its deepest layer,
#'     as an inference (reading L19).}
#'   \item{\code{"udalf"}}{base saturation (sum of cations) below 60 \% at
#'     125 cm below the top of the argillic horizon, at 180 cm, or directly
#'     above a contact, whichever is shallowest (Ultic, Aquultic,
#'     Albaquultic Hapludalfs, p. 92), \code{.bs_at_depth_usda()}.}
#'   \item{\code{"throughout"}}{base saturation (sum of cations) below 75 \%
#'     throughout the argillic horizon (Ustalfs; the argillic or kandic
#'     horizon, Palexeralfs, p. 115), \code{ultic_subgroup_usda(alfisol =
#'     TRUE)}.}
#'   \item{\code{"upper75"}}{base saturation (sum of cations) below 75 \% in
#'     one or more subhorizons of the argillic or kandic horizon within its
#'     upper 75 cm or above a contact (Ultic, Aquultic Haploxeralfs, p. 112).}
#' }
#' The sum of cations is computed where recorded, else NH4OAc stands in
#' (reading L18).
#' @param pedon A \code{\link{PedonRecord}}.
#' @param variant One of the readings above.
#' @noRd
sg_alfisols_bs_usda <- function(pedon, variant = "eutric") {
  variant <- match.arg(variant, c("eutric", "udalf", "throughout", "upper75"))
  h <- pedon$horizons
  nm <- "sg_alfisols_bs_usda"
  ref <- .sga_ref("base saturation in the subgroups (pp. 83-86, 92, 101, 112, 115)")
  if (!nrow(h)) return(.sga_res(nm, FALSE, ref = ref))
  ms <- .sga_ms(pedon)
  ct <- .sga_contact(pedon)
  if (variant == "throughout") {
    r <- ultic_subgroup_usda(pedon, alfisol = TRUE)
    r$name <- nm
    return(r)
  }
  if (variant == "eutric") {
    lim <- min(ms + 180, if (is.na(ct)) Inf else ct)
    L <- .sga_in(h, ms, lim)
    bs <- .mo_bs(h)[L]
    deepest <- suppressWarnings(max(h$bottom_cm, na.rm = TRUE))
    short <- !is.finite(deepest) || deepest < lim - 1e-6
    p <- if (any(!is.na(bs) & .kst_round(bs) < 50)) FALSE
         else if (!length(L) || anyNA(bs)) NA else TRUE
    inf <- if (isTRUE(p) && short)
      sprintf("base saturation read to %s cm, the deepest horizon sampled, as a stand-in for %s cm",
              format(deepest), format(lim)) else character(0)
    return(.sga_res(nm, p, L, list(variant = variant, depth_cm = lim, inferences = inf),
                    "bs_pct", ref))
  }
  hz <- .sga_hz(pedon, if (variant == "udalf") "argillic" else "ak")
  if (!isTRUE(hz$passed)) return(.sga_res(nm, hz$passed, evidence = list(variant = variant),
                                          missing = hz$missing, ref = ref))
  if (variant == "udalf") {
    d <- min(hz$top + 125, ms + 180, if (is.na(ct)) Inf else ct)
    r <- .bs_at_depth_usda(pedon, d, at_contact = !is.na(ct) && abs(d - ct) < 1e-6)
    p <- if (is.na(r$bs)) NA else .kst_round(r$bs) < 60
    inf <- if (!is.na(p) && isTRUE(r$inferred))
      sprintf("base saturation read at the deepest horizon sampled as a stand-in for %s cm", format(d))
    else character(0)
    return(.sga_res(nm, p, hz$layers, list(variant = variant, depth_cm = d, bs = r$bs,
                                           source = r$source, inferences = inf),
                    "bs_pct", ref))
  }
  # upper75
  lim <- min(hz$top + 75, if (is.na(ct)) Inf else ct)
  L <- intersect(hz$layers, .sga_in(h, hz$top, lim))
  b <- .bs_sum_cations_usda(pedon)$bs[L]
  ok <- ifelse(is.na(b), NA, .kst_round(b) < 75)
  .sga_res(nm, .any3u(ok), L[ok %in% TRUE], list(variant = variant), "bs_pct", ref)
}

#' Kanhaplic Haplustalfs and Rhodustalfs, Kandic Paleustalfs (KST 13th ed.)
#'
#' A CEC of less than 24 cmol(+)/kg clay (NH4OAc, pH 7) in 50 \% or more of
#' the argillic horizon if it is less than 100 cm thick, or of its upper 100
#' cm (pp. 101, 108, 109), as a share of the thickness.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_alfisols_kanhaplic_usda <- function(pedon) {
  h <- pedon$horizons
  nm <- "sg_alfisols_kanhaplic_usda"
  ref <- .sga_ref("Kanhaplic and Kandic subgroups (pp. 101, 108, 109)")
  hz <- .sga_hz(pedon, "argillic")
  if (!isTRUE(hz$passed)) return(.sga_res(nm, hz$passed, missing = hz$missing, ref = ref))
  L <- intersect(hz$layers, .sga_in(h, hz$top, hz$top + 100))
  cec <- .sga_num(h, "cec_cmol")[L]; clay <- .sga_num(h, "clay_pct")[L]
  r <- ifelse(!is.na(cec) & !is.na(clay) & clay > 0, 100 * cec / clay, NA_real_)
  ok <- ifelse(is.na(r), NA, .kst_round(r) < 24)
  .sga_res(nm, .sga_share(ok, .sga_clip(h, L, hz$top, hz$top + 100), 0.5), L,
           list(cec_per_kg_clay = r), c("cec_cmol", "clay_pct"), ref)
}


# ---- Rhodic --------------------------------------------------------------

#' Rhodic subgroups of the Alfisols (KST 13th ed., pp. 94, 97, 103, 108)
#'
#' In all subhorizons of the upper \code{depth_cm} (75 or 50) of the
#' \code{horizon} (all of it if thinner), more than 50 \% colours with hue
#' 2.5YR or redder, moist value 3 or less and a dry value no more than 1 unit
#' higher than the moist value. The matrix colour stands for "more than 50
#' percent colors"; a dry colour not recorded is undecided (reading L26).
#' @param pedon A \code{\link{PedonRecord}}.
#' @param horizon "argillic" or "kandic".
#' @param depth_cm 75 or 50.
#' @noRd
sg_alfisols_rhodic_usda <- function(pedon, horizon = "argillic", depth_cm = 75) {
  h <- pedon$horizons
  nm <- "sg_alfisols_rhodic_usda"
  ref <- .sga_ref("Rhodic subgroups (pp. 94, 97, 103, 108)")
  hz <- .sga_hz(pedon, horizon)
  if (!isTRUE(hz$passed)) return(.sga_res(nm, hz$passed, missing = hz$missing, ref = ref))
  L <- intersect(hz$layers, .sga_in(h, hz$top, hz$top + depth_cm))
  red <- .hue_redder_than(h$munsell_hue_moist[L], "2.5YR")
  vm <- .sga_num(h, "munsell_value_moist")[L]; vd <- .sga_num(h, "munsell_value_dry")[L]
  ok <- ifelse(red %in% FALSE | (!is.na(vm) & vm > 3) | (!is.na(vm) & !is.na(vd) & vd - vm > 1),
               FALSE, ifelse(is.na(red) | is.na(vm) | is.na(vd), NA, TRUE))
  .sga_res(nm, .and3(ok), L, list(horizon = hz$kind, depth_cm = depth_cm),
           c("munsell_hue_moist", "munsell_value_moist", "munsell_value_dry"), ref)
}


# ---- Moisture regimes of the intergrades ------------------------------------

#' Moisture-regime intergrades of the Alfisol subgroups (KST 13th ed.)
#'
#' Read from the recorded soil moisture regime (\code{site$soil_moisture_regime}),
#' never inferred:
#' \describe{
#'   \item{\code{"xeric"}}{a xeric regime (Xeric, Xerollic Cryalfs, p. 83).}
#'   \item{\code{"dry45"}}{dry in some part of the moisture control section
#'     for 45 or more cumulative days in normal years (Ustic, Ustollic
#'     Cryalfs, p. 83): a recorded ustic, xeric or aridic regime has that;
#'     a perudic one has not; a udic or aquic one may have it (undecided).}
#'   \item{\code{"aridic"}, \code{"udic"}}{the aridic and udic intergrades of
#'     the Ustalfs (dry six-tenths of the time or more; dry four-tenths or
#'     less, pp. 98-109), which a plain "ustic" record does not decide: they
#'     pass when the recorded regime names the intergrade (e.g. "aridic
#'     ustic", "udic ustic", or an aridic or udic regime), fail when it names
#'     another one ("typic ustic", "xeric", ...), and are undecided on
#'     "ustic" alone.}
#' }
#' @param pedon A \code{\link{PedonRecord}}.
#' @param what One of the readings above.
#' @noRd
sg_alfisols_regime_usda <- function(pedon, what = "xeric") {
  what <- match.arg(what, c("xeric", "dry45", "aridic", "udic"))
  nm <- "sg_alfisols_regime_usda"
  ref <- .sga_ref("moisture intergrades (pp. 83, 98-109)")
  rec <- pedon$site$soil_moisture_regime %||% NA_character_
  if (.usda_regime_blank(rec)) return(.sga_res(nm, NA, evidence = list(what = what),
                                               missing = "site$soil_moisture_regime", ref = ref))
  s <- .usda_smr_value(as.character(rec)[1])
  tok <- strsplit(gsub("[^a-z]+", " ", s), " ")[[1]]
  tok <- tok[nzchar(tok)]
  tok[tok == "torric"] <- "aridic"
  if (what == "xeric") {
    r <- soil_moisture_regime_usda(pedon, "xeric")
    r$name <- nm
    return(r)
  }
  if (what == "dry45") {
    p <- if (any(tok %in% c("ustic", "xeric", "aridic"))) TRUE
         else if (any(tok == "perudic")) FALSE else NA
    return(.sga_res(nm, p, evidence = list(what = what, regime = s),
                    missing = "site$soil_moisture_regime (days dry)", ref = ref))
  }
  other <- setdiff(c("aridic", "udic", "typic", "xeric", "perudic", "aquic"), what)
  if (what == "udic") other <- setdiff(other, "perudic")
  p <- if ("ustic" %in% tok) {
    if (what %in% tok) TRUE else if (any(tok %in% other)) FALSE else NA
  } else {
    if (what %in% tok || (what == "udic" && "perudic" %in% tok)) TRUE else FALSE
  }
  .sga_res(nm, p, evidence = list(what = what, regime = s),
           missing = "site$soil_moisture_regime (ustic intergrade)", ref = ref)
}


# ---- Calcium carbonate --------------------------------------------------------

#' Calcidic Paleustalfs (KST 13th ed., p. 108)
#'
#' A calcic horizon within 100 cm if the weighted particle-size class of the
#' upper 50 cm of the argillic horizon is sandy, 60 cm if loamy, 50 cm if
#' clayey, and free carbonates in all horizons above it (\code{caco3_pct}
#' above 0, or effervescence recorded). The aridic item is a separate test.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_alfisols_calcidic_usda <- function(pedon) {
  h <- pedon$horizons
  nm <- "sg_alfisols_calcidic_usda"
  ref <- .sga_ref("Calcidic Paleustalfs (p. 108)")
  ms <- .sga_ms(pedon)
  ca <- calcic_horizon_usda(pedon, max_top_cm = Inf)
  if (!isTRUE(ca$passed)) return(.sga_res(nm, ca$passed, missing = ca$missing, ref = ref))
  ct <- min(h$top_cm[ca$layers])
  hz <- .sga_hz(pedon, "argillic")
  lim <- if (isTRUE(hz$passed)) {
    U <- intersect(hz$layers, .sga_in(h, hz$top, hz$top + 50))
    w <- .sga_clip(h, U, hz$top, hz$top + 50)
    cl <- .sga_num(h, "clay_pct")[U]
    sandy <- .and3(.usda_lfs_or_coarser(h)[U])
    if (isTRUE(sandy)) 100
    else if (anyNA(cl)) NA_real_
    else if (sum(cl * w) / sum(w) >= 35) 50 else 60
  } else 100
  depth_ok <- if (is.na(lim)) { if (ct < ms + 50) TRUE else if (ct >= ms + 100) FALSE else NA }
              else ct < ms + lim
  A <- .sga_in(h, ms, ct)
  cc <- .sga_num(h, "caco3_pct")
  ef <- .field_term(.sga_chr(h, "effervescence_class"))
  carb <- ifelse(!is.na(cc) & cc > 0, TRUE,
          ifelse(!is.na(ef) & !grepl("^(none|non|no)", ef), TRUE,
          ifelse((!is.na(ef) & grepl("^(none|non|no)", ef)) | (!is.na(cc) & cc == 0), FALSE, NA)))
  p <- .and3(depth_ok, .and3(carb[A]))
  .sga_res(nm, p, ca$layers, list(calcic_top_cm = ct, limit_cm = lim),
           c("caco3_pct", "effervescence_class", "clay_pct"), ref)
}
