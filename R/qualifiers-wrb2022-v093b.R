# ============================================================================
# WRB 2022 (4th ed.) -- Supplementary qualifier seed (v0.9.3.B)
#
# Adds the most commonly used SUPPLEMENTARY qualifiers per Ch 6 that
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
#' @export
qual_aric <- function(pedon) {
  h <- pedon$horizons
  ly <- which(!is.na(h$top_cm) & h$top_cm <= 30)
  if (length(ly) == 0L)
    return(DiagnosticResult$new(name = "Aric", passed = NA,
            layers = integer(0), evidence = list(),
            missing = "designation",
            reference = "WRB (2022) Ch 6, Aric"))
  d <- h$designation[ly]
  ok <- !is.na(d) & grepl("^Ap", d, ignore.case = FALSE)
  passed <- any(ok)
  DiagnosticResult$new(
    name = "Aric", passed = passed,
    layers = ly[ok],
    evidence = list(designation = d),
    missing = if (all(is.na(d))) "designation" else character(0),
    reference = "WRB (2022) Ch 6, Aric"
  )
}


#' Cumulic qualifier (cu): a layer of recent depositional material
#' added on top of an existing soil. v0.9.3.B proxy: \code{layer_origin}
#' is fluvic / aeolic / solimovic at the top of the profile, OR the
#' uppermost mineral horizon's designation matches \code{^[AC]u?\\d?}
#' (cumulic-style suffix).
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_cumulic <- function(pedon) {
  h <- pedon$horizons
  if (nrow(h) == 0L)
    return(DiagnosticResult$new(name = "Cumulic", passed = FALSE,
            layers = integer(0), evidence = list(),
            missing = "top_cm",
            reference = "WRB (2022) Ch 6, Cumulic"))
  top_idx <- which(!is.na(h$top_cm) & h$top_cm <= 5)
  if (length(top_idx) == 0L)
    return(DiagnosticResult$new(name = "Cumulic", passed = FALSE,
            layers = integer(0), evidence = list(),
            missing = "top_cm",
            reference = "WRB (2022) Ch 6, Cumulic"))
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
    reference = "WRB (2022) Ch 6, Cumulic",
    notes = "v0.9.3.B: proxy via layer_origin / cumulic-style designation"
  )
}


#' Profondic qualifier (pf): argic horizon that continues, with no
#' clay decrease, down to or below 150 cm.
#' v0.9.3.B: requires \code{argic} to pass AND at least one argic
#' layer with \code{bottom_cm >= 150}.
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_profondic <- function(pedon) {
  arg <- argic(pedon)
  if (!isTRUE(arg$passed))
    return(DiagnosticResult$new(name = "Profondic", passed = FALSE,
            layers = integer(0), evidence = list(argic = arg),
            missing = arg$missing %||% character(0),
            reference = "WRB (2022) Ch 6, Profondic"))
  h <- pedon$horizons
  ly <- arg$layers
  ok <- !is.na(h$bottom_cm[ly]) & h$bottom_cm[ly] >= 150
  passed <- any(ok)
  DiagnosticResult$new(
    name = "Profondic", passed = passed,
    layers = ly[ok],
    evidence = list(argic = arg, bottom_cm = h$bottom_cm[ly]),
    missing = if (all(is.na(h$bottom_cm[ly]))) "bottom_cm" else character(0),
    reference = "WRB (2022) Ch 6, Profondic"
  )
}


#' Rubic qualifier (rb): red Munsell hue \eqn{\le} 5YR AND chroma
#' \eqn{\ge} 4 in some layer within the upper 100 cm. Less strict
#' than Rhodic (which requires \eqn{\le} 2.5YR + value < 4); useful
#' as a supplementary tag for tropical soils with reddish colours
#' that don't reach the Rhodic threshold.
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_rubic <- function(pedon) {
  h <- pedon$horizons
  ly <- which(!is.na(h$top_cm) & h$top_cm <= 100)
  if (length(ly) == 0L)
    return(DiagnosticResult$new(name = "Rubic", passed = NA,
            layers = integer(0), evidence = list(),
            missing = "munsell_hue_moist",
            reference = "WRB (2022) Ch 6, Rubic"))
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
    reference = "WRB (2022) Ch 6, Rubic"
  )
}


#' Lamellic qualifier (ll): thin (\eqn{<} 5 cm) clay-enriched
#' lamellae, typical of sandy Luvisols / Alisols / Acrisols.
#' v0.9.3.B proxy: designation pattern \code{lamell} / \code{E&Bt} /
#' \code{&Bt} / \code{Bt(t)?\\d?lam} in any subsurface layer.
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_lamellic <- function(pedon) {
  h <- pedon$horizons
  ly <- which(!is.na(h$top_cm) & h$top_cm >= 5 & h$top_cm <= 200)
  if (length(ly) == 0L)
    return(DiagnosticResult$new(name = "Lamellic", passed = FALSE,
            layers = integer(0), evidence = list(),
            missing = character(0),
            reference = "WRB (2022) Ch 6, Lamellic"))
  d <- h$designation[ly]
  ok <- !is.na(d) & grepl("lamell|E&Bt|&Bt|Btlam|Bt[0-9]?lam",
                              d, ignore.case = TRUE)
  passed <- any(ok)
  DiagnosticResult$new(
    name = "Lamellic", passed = passed,
    layers = ly[ok],
    evidence = list(designation = d),
    missing = character(0),
    reference = "WRB (2022) Ch 6, Lamellic",
    notes = "v0.9.3.B: designation-pattern proxy; dedicated lamellae_thickness_cm scheduled for v0.9.4"
  )
}
