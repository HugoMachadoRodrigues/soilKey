# =============================================================================
# Horizon-designation -> diagnostic-horizon inference (v0.9.187).
#
# Legacy soil archives frequently record a field pedologist's horizon
# DESIGNATION (Bt, Bw, Bi, Bh, Bn, ...) while omitting the quantitative lab
# attributes a diagnostic test needs (a clay gradient, CEC per clay, ...). The
# designation is itself diagnostic morphology: the subordinate letter encodes
# the process the surveyor judged in the field (t = clay illuviation, w/o = in-
# situ weathering, i = incipient alteration, h/s = illuvial humus/sesquioxides,
# n = sodium, g = gleying, f = plinthite, v = vertic). When the quantitative
# test cannot fire, these letters let the key still reach the right class, at a
# lower (morphological) evidence grade rather than defaulting to the catch-all.
#
# This is opt-in via options(soilKey.morphological_inference = TRUE); the strict
# lab-only default (and the aqp engine) are unchanged, so canonical fixtures
# stay byte-identical.
# =============================================================================

#' Is designation-driven morphological inference enabled?
#' @noRd
.morph_inference_enabled <- function() {
  isTRUE(getOption("soilKey.morphological_inference", FALSE))
}

#' Is a pedon in a tropical / deep-weathering regime?
#'
#' Region matters for a single ambiguous designation: a `Bw` denotes the
#' *cambic* horizon in temperate/USDA usage but the latossolic (ferralic/oxic)
#' B in Brazilian/tropical usage. This resolver decides which reading a `Bw`/`Bo`
#' designation should take when only morphology is available.
#'
#' Rule (conservative): TRUE if `site$country` is Brazil (whose soil cover is
#' dominated by deeply-weathered latossolic material) OR the site latitude is
#' within the tropics (|lat| <= 23.5). FALSE if the latitude is known and
#' outside the tropics. `NA` (unknown) when neither signal is present, in which
#' case callers must NOT assume ferralic -- the safe default is cambic.
#'
#' @param pedon A \code{\link{PedonRecord}} (or any object with a `$site` list).
#' @return `TRUE`, `FALSE`, or `NA`.
#' @noRd
.pedon_is_tropical <- function(pedon) {
  site <- tryCatch(pedon$site, error = function(e) NULL) %||% list()
  ctry <- toupper(trimws(as.character(site$country %||% "")))
  if (nzchar(ctry) && ctry %in% c("BR", "BRA", "BRAZIL", "BRASIL")) return(TRUE)
  lat <- suppressWarnings(as.numeric(site$lat %||% NA_real_))[1]
  if (!is.na(lat)) return(abs(lat) <= 23.5)
  NA
}

#' Map a horizon designation to the single diagnostic horizon its subordinate
#' letters indicate (or NA). Master must be a subsurface B (or E) horizon.
#'
#' Precedence is set so the ORDER-defining process wins for multi-letter
#' designations: sodium (n) and the spodic/vertic surface processes outrank the
#' clay processes; the clay-illuviation "t" (argic) then outranks a merely
#' modifying plinthite ("f", as in Btf = argic that is also plinthic) or gleying
#' ("g", as in Btg = argic that is also gleyed), so a bare Bf/Bg is needed to key
#' plinthic/gleyic. Slickenside "ss" (vertic) is distinguished from a single
#' sesquioxide "s" (spodic).
#' @param designation Character vector of horizon designations.
#' @param tropical Single logical (or `NA`) from \code{\link{.pedon_is_tropical}}.
#'   Governs the region-ambiguous `Bw`/`Bo` letter only: `TRUE` reads it as the
#'   latossolic *ferralic* B (Brazilian usage); `FALSE`/`NA` read it as the
#'   *cambic* B (temperate/USDA usage, and the safe default when region is
#'   unknown). Every other letter is region-invariant.
#' @return Character vector of indicated diagnostics (NA where none).
#' @noRd
.designation_indicates <- function(designation, tropical = NA) {
  d <- trimws(as.character(designation))
  master <- toupper(substr(d, 1, 1))
  sub    <- gsub("[^a-z]", "", d)          # lowercase subordinate letters only
  is_sub <- master %in% c("B", "E") & !is.na(d)
  has <- function(L) grepl(L, sub, fixed = TRUE)
  out <- rep(NA_character_, length(d))
  set <- function(cond, val) out[is_sub & is.na(out) & cond] <<- val
  # Region-aware reading of Bw/Bo: latossolic-ferralic in the tropics, cambic
  # in temperate usage (and when region is unknown -- ferralic is the strong
  # claim and must be positively supported).
  bw_diag <- if (isTRUE(tropical)) "ferralic" else "cambic"
  set(has("n"),                       "natric")    # Bn, Btn (sodium)
  set(has("h") | (has("s") & !has("ss")), "spodic")# Bh, Bs, Bhs (illuvial humus/R2O3)
  set(has("v") | has("ss"),           "vertic")    # Bv, Bss (slickensides)
  set(has("t"),                       "argic")     # Bt, Btf, Btg (clay illuviation)
  set(has("w") | has("o"),            bw_diag)     # Bw, Bo (in-situ weathering)
  set(has("f"),                       "plinthic")  # bare Bf (plinthite)
  set(has("g"),                       "gleyic")    # bare Bg (gleying)
  set(has("i"),                       "cambic")    # Bi (incipient)
  out
}

#' Layers whose designation indicates `diag`, at subsoil context (top >= min_top
#' or a B/E master). Returns integer layer indices.
#' @noRd
.morph_layers <- function(h, diag, min_top = 20, tropical = NA) {
  desg <- as.character(h$designation %||% rep(NA_character_, nrow(h)))
  ind  <- .designation_indicates(desg, tropical = tropical)
  top  <- suppressWarnings(as.numeric(h$top_cm %||% rep(NA_real_, nrow(h))))
  subsoil <- (is.na(top) | top >= min_top) | grepl("^[BE]", trimws(desg))
  which(!is.na(ind) & ind == diag & subsoil)
}

#' Attach a morphological-inference sub-test to a diagnostic result when its
#' quantitative test did not fire but the designation indicates it. Returns the
#' (possibly updated) list(passed, layers, evidence).
#' @noRd
.apply_morph_inference <- function(diag, h, passed, layers, evidence,
                                    min_top = 20, note = NULL, tropical = NA) {
  if (!.morph_inference_enabled() || isTRUE(passed)) {
    return(list(passed = passed, layers = layers, evidence = evidence))
  }
  ml <- .morph_layers(h, diag, min_top = min_top, tropical = tropical)
  if (length(ml) == 0L) {
    return(list(passed = passed, layers = layers, evidence = evidence))
  }
  evidence$morphological_inference <- list(
    passed = TRUE, layers = ml,
    diagnostic = diag,
    provenance = "morphological_designation",
    note = note %||% paste0("v0.9.187: quantitative ", diag,
                            " test did not fire; horizon designation indicates ",
                            diag, " (morphological evidence, downgraded grade)."))
  list(passed = TRUE, layers = union(layers, ml), evidence = evidence)
}
