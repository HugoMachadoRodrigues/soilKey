# =============================================================================
# soilKey v0.9.217 -- WRB 2022 qualifiers that Chapter 4 lists and soilKey had
# no function for.
# =============================================================================

#' Novic qualifier (nv)
#'
#' WRB 2022, Chapter 5: "having a layer, >= 5 and < 50 cm thick, overlying a
#' buried soil that is classified with preference according to the 'Rules for
#' naming soils' (Chapter 2.4)". Buried horizons are read from the designation
#' suffix \code{b} (\code{Ab}, \code{2Btb}), the convention soilKey already uses
#' for the Thapto- specifier: the soil is Novic when the shallowest buried
#' horizon starts at >= 5 and < 50 cm. Without designations it cannot be told,
#' and the result is \code{NA}.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_novic <- function(pedon) {
  h <- pedon$horizons
  d <- h$designation
  if (is.null(d) || all(is.na(d) | !nzchar(trimws(d)))) {
    return(DiagnosticResult$new(
      name = "Novic", passed = NA, layers = integer(0),
      evidence = list(reason = "no horizon designations to find a buried soil"),
      missing = "designation",
      reference = "WRB (2022) Ch 5, Novic"))
  }
  buried <- which(!is.na(d) & grepl("b$|/b$|^[A-Z][a-z]*b\\b", d))
  if (!length(buried)) {
    return(DiagnosticResult$new(
      name = "Novic", passed = FALSE, layers = integer(0),
      evidence = list(reason = "no buried horizon (designation suffix b)"),
      missing = character(0),
      reference = "WRB (2022) Ch 5, Novic"))
  }
  first <- buried[which.min(h$top_cm[buried])]
  depth <- h$top_cm[first]
  passed <- isTRUE(!is.na(depth) && depth >= 5 && depth < 50)
  DiagnosticResult$new(
    name = "Novic", passed = passed,
    layers = if (passed) which(h$top_cm < depth) else integer(0),
    evidence = list(buried_from_cm = depth, buried_horizon = d[first]),
    missing = if (is.na(depth)) "top_cm" else character(0),
    reference = "WRB (2022) Ch 5, Novic")
}


# ---- Epic, Endic, Dorsic: the RSG's own diagnostic horizon -----------------
#
# WRB 2022, Chapter 5. Epic: "in Cryosols, the cryic horizon starting <= 50 cm
# from the soil surface; in other soils, the uppermost respective diagnostic
# horizon of the RSG, not meeting the set of diagnostic criteria of the Petric
# qualifier, starting <= 50 cm from the mineral soil surface". Endic: the same,
# starting > 50 and <= 100 cm. Dorsic: "in Cryosols, the cryic horizon starting
# > 100 cm ...; in Ferralsols and Podzols, the ferralic/spodic horizon starting
# > 100 cm". They describe where the horizon that defines the RSG begins, so
# they need the RSG: the resolver passes it as `rsg_code`.
#
# Until v0.9.217 Epic passed for any profile with a horizon starting above
# 50 cm (i.e. almost every profile), Endic for any horizon starting at 50-100
# cm, and Dorsic read a "dorsal / ridge" microrelief field.

# The diagnostic horizon each RSG's Epic/Endic/Dorsic refers to (the RSGs whose
# Chapter 4 list carries them). Petric versions are left out, as the definition
# asks: they would make the soil Petric instead.
.WRB_RSG_HORIZON <- list(
  CR = "cryic_horizon", SN = "natric_horizon", VR = "vertic_horizon",
  PZ = "spodic", PT = c("plinthic", "pisoplinthic"), NT = "nitic_horizon",
  FR = "ferralic", DU = "duric_horizon", GY = "gypsic", CL = "calcic",
  RT = "argic", AC = "argic", LX = "argic", AL = "argic", LV = "argic"
)

# Top depth of the RSG's diagnostic horizon: list(top, layers, missing), top NA
# when the horizon is absent or cannot be evaluated.
.wrb_rsg_horizon_top <- function(pedon, rsg_code) {
  fns <- .WRB_RSG_HORIZON[[rsg_code %||% ""]]
  if (is.null(fns))
    return(list(top = NA_real_, layers = integer(0), missing = character(0),
                known = FALSE))
  layers <- integer(0); missing <- character(0)
  for (f in fns) {
    r <- tryCatch(get(f, envir = asNamespace("soilKey"))(pedon),
                  error = function(e) NULL)
    if (is.null(r)) next
    if (isTRUE(r$passed)) layers <- union(layers, r$layers %||% integer(0))
    missing <- c(missing, r$missing %||% character(0))
  }
  top <- if (length(layers)) min(pedon$horizons$top_cm[layers], na.rm = TRUE) else NA_real_
  list(top = if (is.finite(top)) top else NA_real_, layers = layers,
       missing = unique(missing), known = TRUE)
}

.wrb_rsg_horizon_depth_qualifier <- function(pedon, rsg_code, name, lo, hi,
                                             rsgs = names(.WRB_RSG_HORIZON)) {
  ref <- paste0("WRB (2022) Ch 5, ", name)
  if (is.null(rsg_code) || !rsg_code %in% rsgs)
    return(DiagnosticResult$new(
      name = name, passed = NA, layers = integer(0),
      evidence = list(reason = "defined by the RSG's diagnostic horizon; no RSG given"),
      missing = character(0), reference = ref))
  x <- .wrb_rsg_horizon_top(pedon, rsg_code)
  if (is.na(x$top))
    return(DiagnosticResult$new(
      name = name, passed = if (length(x$missing)) NA else FALSE,
      layers = integer(0),
      evidence = list(horizon = .WRB_RSG_HORIZON[[rsg_code]], top_cm = NA),
      missing = x$missing, reference = ref))
  passed <- x$top > lo && x$top <= hi
  DiagnosticResult$new(
    name = name, passed = passed,
    layers = if (passed) x$layers else integer(0),
    evidence = list(horizon = .WRB_RSG_HORIZON[[rsg_code]], top_cm = x$top),
    missing = character(0), reference = ref)
}

#' Epic qualifier (ep): the RSG's diagnostic horizon starts <= 50 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param rsg_code The RSG being named (the resolver passes it).
#' @noRd
qual_epic <- function(pedon, rsg_code = NULL)
  .wrb_rsg_horizon_depth_qualifier(pedon, rsg_code, "Epic", -Inf, 50)

#' Endic qualifier (ed): the RSG's diagnostic horizon starts > 50, <= 100 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param rsg_code The RSG being named (the resolver passes it).
#' @noRd
qual_endic <- function(pedon, rsg_code = NULL)
  .wrb_rsg_horizon_depth_qualifier(pedon, rsg_code, "Endic", 50, 100)

#' Dorsic qualifier (ds): the cryic, ferralic or spodic horizon starts > 100 cm
#' (in Cryosols, Ferralsols and Podzols only).
#' @param pedon A \code{\link{PedonRecord}}.
#' @param rsg_code The RSG being named (the resolver passes it).
#' @noRd
qual_dorsic <- function(pedon, rsg_code = NULL)
  .wrb_rsg_horizon_depth_qualifier(pedon, rsg_code, "Dorsic", 100, Inf,
                                   rsgs = c("CR", "FR", "PZ"))
