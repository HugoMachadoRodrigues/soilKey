# ============================================================================
# WRB 2022 (4th ed.) -- Supplementary qualifier seed (v0.9.3.B)
#
# Adds the most commonly used SUPPLEMENTARY qualifiers per Ch 5 that
# v0.9.1 had not yet implemented as standalone functions:
#
#   Aric       homogenised plough layer (designation \\code{Ap*})
#   Cumulic    recent depositional cover (designation \\code{Cu/Au}
#              with low age proxy via fluvic / aeolic layer_origin)
#   Profondic  argic horizon that continues to >= 150 cm depth
#   Rubic      red Munsell hue >= 5YR + chroma >= 4 in upper 100 cm
#              (less strict than Rhodic, which needs <= 2.5YR + value < 4)
#   Lamellic   thin clay-enriched Bt lamellae (designation pattern
#              proxy: "lamell" / "E&Bt" / "&Bt")
#
# All five are dispatched the same way as principals through
# resolve_wrb_qualifiers; whether they appear as principal or
# supplementary depends on the YAML slot for the RSG.
# ============================================================================


#' Aric qualifier (ar): mineral surface horizon homogenised by
#' ploughing -- designation pattern \code{Ap}, \code{Apk},
#' \code{Apc}, etc., starting within the upper 30 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_aric <- function(pedon) {
  h <- pedon$horizons
  ly <- which(!is.na(h$top_cm) & h$top_cm <= 30)
  if (length(ly) == 0L)
    return(DiagnosticResult$new(name = "Aric", passed = NA,
            layers = integer(0), evidence = list(),
            missing = "designation",
            reference = "WRB (2022) Ch 5, Aric"))
  d <- h$designation[ly]
  ok <- !is.na(d) & grepl("^Ap", d, ignore.case = FALSE)
  passed <- any(ok)
  DiagnosticResult$new(
    name = "Aric", passed = passed,
    layers = ly[ok],
    evidence = list(designation = d),
    missing = if (all(is.na(d))) "designation" else character(0),
    reference = "WRB (2022) Ch 5, Aric"
  )
}


#' Cumulic qualifier (cu): a layer of recent depositional material
#' added on top of an existing soil. v0.9.3.B proxy: \code{layer_origin}
#' is fluvic / aeolic / solimovic at the top of the profile, OR the
#' uppermost mineral horizon's designation matches \code{^[AC]u?\\d?}
#' (cumulic-style suffix).
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_cumulic <- function(pedon) {
  h <- pedon$horizons
  if (nrow(h) == 0L)
    return(DiagnosticResult$new(name = "Cumulic", passed = FALSE,
            layers = integer(0), evidence = list(),
            missing = "top_cm",
            reference = "Not a WRB 2022 qualifier (out of the lists since v0.9.216)"))
  top_idx <- which(!is.na(h$top_cm) & h$top_cm <= 5)
  if (length(top_idx) == 0L)
    return(DiagnosticResult$new(name = "Cumulic", passed = FALSE,
            layers = integer(0), evidence = list(),
            missing = "top_cm",
            reference = "Not a WRB 2022 qualifier (out of the lists since v0.9.216)"))
  origin <- h$layer_origin[top_idx]
  d      <- h$designation [top_idx]
  ok_origin <- !is.na(origin) &
                 grepl("fluvic|aeolic|solimovic|cumul",
                       origin, ignore.case = TRUE)
  ok_dsg <- !is.na(d) &
             grepl("^Au\\d?|^Cu\\d?|^A[a-z]?u\\b|cumul",
                   d, ignore.case = FALSE)
  ok <- ok_origin | ok_dsg
  passed <- any(ok)
  DiagnosticResult$new(
    name = "Cumulic", passed = passed,
    layers = top_idx[ok],
    evidence = list(layer_origin = origin, designation = d),
    missing = character(0),
    reference = "Not a WRB 2022 qualifier (out of the lists since v0.9.216)",
    notes = "v0.9.3.B: proxy via layer_origin / cumulic-style designation"
  )
}


#' Profondic qualifier (pf): argic horizon that continues, with no
#' clay decrease, down to or below 150 cm.
#' v0.9.3.B: requires \code{argic} to pass AND at least one argic
#' layer with \code{bottom_cm >= 150}.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_profondic <- function(pedon) {
  arg <- argic(pedon)
  if (!isTRUE(arg$passed))
    return(DiagnosticResult$new(name = "Profondic", passed = FALSE,
            layers = integer(0), evidence = list(argic = arg),
            missing = arg$missing %||% character(0),
            reference = "WRB (2022) Ch 5, Profondic"))
  h <- pedon$horizons
  ly <- arg$layers
  ok <- !is.na(h$bottom_cm[ly]) & h$bottom_cm[ly] >= 150
  passed <- any(ok)
  DiagnosticResult$new(
    name = "Profondic", passed = passed,
    layers = ly[ok],
    evidence = list(argic = arg, bottom_cm = h$bottom_cm[ly]),
    missing = if (all(is.na(h$bottom_cm[ly]))) "bottom_cm" else character(0),
    reference = "WRB (2022) Ch 5, Profondic"
  )
}


#' Rubic qualifier (rb): red Munsell hue \eqn{\le} 5YR AND chroma
#' \eqn{\ge} 4 in some layer within the upper 100 cm. Less strict
#' than Rhodic (which requires \eqn{\le} 2.5YR + value < 4); useful
#' as a supplementary tag for tropical soils with reddish colours
#' that don't reach the Rhodic threshold.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_rubic <- function(pedon) {
  h <- pedon$horizons
  ly <- which(!is.na(h$top_cm) & h$top_cm <= 100)
  if (length(ly) == 0L)
    return(DiagnosticResult$new(name = "Rubic", passed = NA,
            layers = integer(0), evidence = list(),
            missing = "munsell_hue_moist",
            reference = "WRB (2022) Ch 5, Rubic"))
  hu <- h$munsell_hue_moist[ly]
  ch <- h$munsell_chroma_moist[ly]
  ok <- !is.na(hu) & !is.na(ch) &
          grepl("^(5YR|2\\.5YR|10R|7\\.5R|5R|2\\.5R)\\b",
                hu, ignore.case = TRUE) & ch >= 4
  passed <- any(ok)
  DiagnosticResult$new(
    name = "Rubic", passed = passed,
    layers = ly[ok],
    evidence = list(hues = hu, chromas = ch),
    missing = if (all(is.na(hu))) "munsell_hue_moist" else character(0),
    reference = "WRB (2022) Ch 5, Rubic"
  )
}


#' Lamellic qualifier (ll): thin (\eqn{<} 5 cm) clay-enriched
#' lamellae, typical of sandy Luvisols / Alisols / Acrisols.
#' v0.9.3.B proxy: designation pattern \code{lamell} / \code{E&Bt} /
#' \code{&Bt} / \code{Bt(t)?\\d?lam} in any subsurface layer.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_lamellic <- function(pedon) {
  # v0.9.220: WRB 2022 Ch 5, Lamellic: "having two or more lamellae, >= 0.5 and
  # < 7.5 cm thick, that have one or both of the following: higher clay contents
  # than the directly overlying and underlying layers as stated in the
  # diagnostic criteria 2.a of the argic horizon, or meet the diagnostic
  # criteria 2.b of the argic horizon ... and that have a combined thickness of
  # >= 5 cm within 50 cm; the uppermost lamella starting <= 100 cm from the
  # mineral soil surface". Read from the layers' thickness, clay and clay films
  # (2.b.ii, "common" or more). A lamella designation (E&Bt, "lamell") without
  # the data makes it NA; until v0.9.219 the designation alone decided.
  h <- pedon$horizons
  ref <- "WRB (2022) Ch 5, Lamellic"
  n <- nrow(h)
  ord <- order(h$top_cm)
  thk <- h$bottom_cm - h$top_cm
  clay <- h$clay_pct
  films <- .clay_films_class(h$clay_films_amount %||% rep(NA_character_, n))
  more_clay <- function(here, other) {
    if (is.na(here) || is.na(other)) return(NA)
    if (other < 15) here - other >= 6
    else if (other < 50) here / other >= 1.4
    else here - other >= 20
  }
  lam <- rep(FALSE, n)
  for (k in seq_along(ord)) {
    i <- ord[k]
    if (is.na(thk[i])) { lam[i] <- NA; next }
    if (thk[i] < 0.5 || thk[i] >= 7.5) next
    by_films <- if (is.na(films[i])) NA else films[i] >= 3L
    if (k == 1L || k == length(ord)) { lam[i] <- if (isTRUE(by_films)) TRUE else
                                                   if (is.na(by_films)) NA else FALSE; next }
    by_clay <- .and3(more_clay(clay[i], clay[ord[k - 1L]]),
                     more_clay(clay[i], clay[ord[k + 1L]]))
    lam[i] <- if (isTRUE(by_clay) || isTRUE(by_films)) TRUE
              else if (is.na(by_clay) || is.na(by_films)) NA else FALSE
  }
  ms <- .q_mineral_surface_cm(h)
  if (is.na(ms)) ms <- 0
  combined <- function(idx) {
    idx <- idx[!is.na(h$top_cm[idx]) & h$top_cm[idx] - ms <= 100]
    if (length(idx) < 2L) return(0)
    best <- 0
    for (i in idx) {
      w <- idx[h$top_cm[idx] >= h$top_cm[i] & h$top_cm[idx] < h$top_cm[i] + 50]
      if (length(w) >= 2L)
        best <- max(best, sum(pmin(h$bottom_cm[w], h$top_cm[i] + 50) - h$top_cm[w]))
    }
    best
  }
  yes <- which(lam %in% TRUE)
  may <- which(lam %in% TRUE | is.na(lam))
  passed <- if (combined(yes) >= 5) TRUE else if (combined(may) >= 5) NA else FALSE
  desg <- as.character(h$designation %||% rep(NA_character_, n))
  described <- which(!is.na(desg) &
                       grepl("lamell|E&Bt|&Bt|Btlam|Bt[0-9]?lam", desg, ignore.case = TRUE))
  if (isFALSE(passed) && length(described)) passed <- NA
  DiagnosticResult$new(
    name = "Lamellic", passed = passed,
    layers = if (isTRUE(passed)) yes else integer(0),
    evidence = list(lamella = lam, lamella_designations = described,
                    mineral_surface_cm = ms),
    missing = if (is.na(passed)) c("top_cm", "bottom_cm", "clay_pct",
                                   "clay_films_amount") else character(0),
    reference = ref
  )
}
