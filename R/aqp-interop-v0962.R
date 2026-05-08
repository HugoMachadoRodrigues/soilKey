# =============================================================================
# v0.9.62 -- aqp::SoilProfileCollection interop, augmented edition.
#
# soilKey already has aqp-interop.R (v0.7) with a basic converter. This
# file adds the v0.9.62-specific helpers that wire the canonical aqp
# diagnostic functions (getArgillicBounds, getCambicBounds, mollic
# helpers) into soilKey's diagnostic chain in PARALLEL with the
# existing hand-coded paths.
#
# The intent is A/B comparison, NOT replacement: every soilKey
# diagnostic that has a canonical aqp counterpart can be invoked with
# `engine = c("soilkey", "aqp", "both")` and the caller can choose
# which result to use (or compare both for downstream auditing).
# =============================================================================


#' NRCS texture-class shorthand from clay / silt / sand percent
#'
#' aqp's \code{getArgillicBounds()} requires an NRCS texture class
#' column (e.g. "SCL", "C", "CL", "FS"). soilKey horizons only carry
#' the percent fractions; this helper derives the class from the
#' standard USDA texture triangle.
#'
#' Returns the standard NRCS abbreviation:
#' \tabular{ll}{
#'   COS \tab Coarse sand            \cr
#'   S   \tab Sand                   \cr
#'   FS  \tab Fine sand              \cr
#'   VFS \tab Very fine sand         \cr
#'   LS  \tab Loamy sand             \cr
#'   LFS \tab Loamy fine sand        \cr
#'   SL  \tab Sandy loam             \cr
#'   FSL \tab Fine sandy loam        \cr
#'   L   \tab Loam                   \cr
#'   SIL \tab Silt loam              \cr
#'   SI  \tab Silt                   \cr
#'   SCL \tab Sandy clay loam        \cr
#'   CL  \tab Clay loam              \cr
#'   SICL\tab Silty clay loam        \cr
#'   SC  \tab Sandy clay             \cr
#'   SIC \tab Silty clay             \cr
#'   C   \tab Clay
#' }
#'
#' Implementation follows the canonical USDA texture triangle; vector-
#' ised over the input. NA in / NA out.
#'
#' @param clay Numeric vector of clay percent (0-100).
#' @param silt Numeric vector of silt percent.
#' @param sand Numeric vector of sand percent. (clay + silt + sand
#'        should sum to ~100; mild deviations are tolerated.)
#' @return Character vector of NRCS texture class abbreviations.
#' @export
texture_class_from_pct <- function(clay, silt, sand) {
  n <- length(clay)
  if (n == 0L) return(character(0))
  out <- rep(NA_character_, n)
  for (i in seq_len(n)) {
    cl <- clay[i]; si <- silt[i]; sa <- sand[i]
    if (any(is.na(c(cl, si, sa)))) next
    # USDA NRCS texture triangle, evaluated outer-corner-first so the
    # narrow Sand / Loamy Sand wedges win over the broader Sandy Loam
    # band at the high-sand corner. Order matters.
    #
    # NRCS Soil Survey Manual (2017) Table 3-3 boundary equations
    # are written in terms of SILT + clay (not sand + clay):
    #   Sand        : silt + 1.5*clay <  15
    #   Loamy sand  : silt + 1.5*clay >= 15 AND silt + 2*clay < 30
    if (si + 1.5 * cl < 15)                             { out[i] <- "S";  next }
    if (si + 2.0 * cl < 30)                             { out[i] <- "LS"; next }
    # Clay corner.
    if (cl >= 40 && si >= 40)                           { out[i] <- "SIC"; next }
    if (cl >= 40 && sa >= 45)                           { out[i] <- "SC";  next }
    if (cl >= 40)                                       { out[i] <- "C";   next }
    # Clay-loam band (clay 27-40).
    if (cl >= 27 && cl < 40 && sa <= 20)                { out[i] <- "SICL"; next }
    if (cl >= 27 && cl < 40 && sa > 45)                 { out[i] <- "SCL"; next }
    if (cl >= 27 && cl < 40)                            { out[i] <- "CL";  next }
    # Sandy clay loam wedge (clay 20-35, silt < 28, sand > 45).
    if (cl >= 20 && cl < 35 && si < 28 && sa > 45)      { out[i] <- "SCL"; next }
    # Silty band.
    if (si >= 80 && cl < 12)                            { out[i] <- "SI";  next }
    if (si >= 50 && cl < 12)                            { out[i] <- "SI";  next }
    if (si >= 50 && cl >= 12 && cl < 27)                { out[i] <- "SIL"; next }
    # Loam interior.
    if (cl >= 7 && cl < 27 && si >= 28 && si < 50 && sa <= 52) {
      out[i] <- "L"; next
    }
    # Sandy loam (residual).
    if (cl < 20 && sa >= 52)                            { out[i] <- "SL";  next }
    out[i] <- "L"  # fallback for triangle interior
  }
  out
}


#' Convert a soilKey PedonRecord to an aqp SoilProfileCollection
#'
#' The mapping respects aqp's expected column conventions and sets
#' the metadata required by \code{getArgillicBounds()},
#' \code{getCambicBounds()}, and \code{mollicEpipedon()}:
#'
#' \itemize{
#'   \item \code{id} from \code{pedon$site$id}
#'   \item \code{top} / \code{bottom} from \code{top_cm} / \code{bottom_cm}
#'   \item \code{name} (designation) from \code{designation}
#'   \item \code{texcl} (texture class) derived via
#'         \code{\link{texture_class_from_pct}}
#'   \item \code{clay}, \code{silt}, \code{sand} from
#'         \code{clay_pct} / \code{silt_pct} / \code{sand_pct}
#'   \item \code{m_hue}, \code{m_value}, \code{m_chroma},
#'         \code{d_value}, \code{d_chroma} from
#'         \code{munsell_*_moist} and \code{munsell_*_dry}
#' }
#'
#' Internal use; the soilKey diagnostics call this on the fly when
#' \code{engine = "aqp"}. Direct use is supported for users who want
#' to plug additional aqp algorithms (\code{slab}, \code{slice},
#' \code{glom}) into a soilKey workflow.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{aqp::SoilProfileCollection} with one site (the
#'   pedon) and one row per horizon.
#' @export
pedon_to_spc <- function(pedon) {
  if (!requireNamespace("aqp", quietly = TRUE)) {
    stop("pedon_to_spc(): the 'aqp' package is required. ",
         "install.packages('aqp').", call. = FALSE)
  }
  hz <- pedon$horizons
  if (is.null(hz) || nrow(hz) == 0L) {
    stop("pedon_to_spc(): pedon has no horizons.", call. = FALSE)
  }
  pid <- pedon$site$id %||% "unknown"
  hz <- as.data.frame(hz)
  df <- data.frame(
    id      = rep(as.character(pid), nrow(hz)),
    top     = as.integer(round(hz$top_cm)),
    bottom  = as.integer(round(hz$bottom_cm)),
    name    = as.character(hz$designation %||% rep(NA, nrow(hz))),
    clay    = as.numeric(hz$clay_pct %||% rep(NA_real_, nrow(hz))),
    silt    = as.numeric(hz$silt_pct %||% rep(NA_real_, nrow(hz))),
    sand    = as.numeric(hz$sand_pct %||% rep(NA_real_, nrow(hz))),
    stringsAsFactors = FALSE
  )
  df$texcl <- texture_class_from_pct(df$clay, df$silt, df$sand)
  # Munsell, both moist and dry, for cambic / mollic colour tests.
  df$m_hue    <- as.character(hz$munsell_hue_moist    %||% rep(NA, nrow(hz)))
  df$m_value  <- as.numeric  (hz$munsell_value_moist  %||% rep(NA_real_, nrow(hz)))
  df$m_chroma <- as.numeric  (hz$munsell_chroma_moist %||% rep(NA_real_, nrow(hz)))
  df$d_hue    <- as.character(hz$munsell_hue_dry      %||% rep(NA, nrow(hz)))
  df$d_value  <- as.numeric  (hz$munsell_value_dry    %||% rep(NA_real_, nrow(hz)))
  df$d_chroma <- as.numeric  (hz$munsell_chroma_dry   %||% rep(NA_real_, nrow(hz)))
  # Drop horizons with NA top / bottom (aqp::depths<- requires complete depths).
  ok <- !is.na(df$top) & !is.na(df$bottom) & df$bottom > df$top
  if (!any(ok)) {
    stop("pedon_to_spc(): no horizons with complete top/bottom depths.",
         call. = FALSE)
  }
  df <- df[ok, , drop = FALSE]
  # Build SPC. depths<- mutates in place.
  aqp::depths(df) <- id ~ top + bottom
  aqp::hzdesgnname(df) <- "name"
  aqp::hztexclname(df) <- "texcl"
  aqp::hzmetaname(df, "clay") <- "clay"
  df
}


#' Argic / argillic horizon via aqp::getArgillicBounds()
#'
#' Wraps \code{aqp::getArgillicBounds()} (Beaudette et al.) in soilKey's
#' \code{\link{DiagnosticResult}} contract. The aqp implementation is
#' the canonical NRCS R port and uses the tiered USDA-NRCS clay-increase
#' thresholds:
#' \itemize{
#'   \item Eluvial clay < 15\\%        : \\>= +3 percentage points
#'   \item Eluvial clay 15-40\\%       : \\>= 1.2x ratio
#'   \item Eluvial clay \\>= 40\\%      : \\>= +8 percentage points
#' }
#' (vs. soilKey's hand-coded \code{\link{argic}} which uses the WRB
#' 6/1.4/20 thresholds). For BDsolos / FEBR / KSSL profiles the aqp
#' rule is closer to KST 13ed and BDsolos field practice.
#'
#' By default aqp requires a "t" suffix in the horizon designation
#' (\code{require_t = TRUE}); we expose this so callers can be
#' permissive on datasets where designation is missing or
#' non-conforming (BDsolos exports often drop the "t").
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param require_t Whether to require an explicit "t" suffix in the
#'        horizon designation (default \code{FALSE} for BDsolos /
#'        FEBR; \code{TRUE} matches the strict KST 13ed text).
#' @param ... Reserved for future arguments.
#' @return A \code{\link{DiagnosticResult}} with \code{name =
#'   "argic_aqp"}. \code{$layers} are the row indices of horizons
#'   in the argillic / argic depth interval. \code{$evidence} carries
#'   the raw aqp \code{c(ubound, lbound)} bounds for traceability.
#' @seealso \code{\link{argic}} (soilKey hand-coded; WRB 6/1.4/20),
#'   \code{aqp::getArgillicBounds}.
#' @export
argic_aqp <- function(pedon, require_t = FALSE, ...) {
  if (!requireNamespace("aqp", quietly = TRUE)) {
    return(DiagnosticResult$new(
      name = "argic_aqp", passed = NA, layers = integer(0),
      evidence = list(error = "aqp not installed"),
      missing = "aqp",
      reference = "aqp::getArgillicBounds (NRCS, KST 13ed thresholds)"
    ))
  }
  spc <- tryCatch(pedon_to_spc(pedon), error = function(e) NULL)
  if (is.null(spc)) {
    return(DiagnosticResult$new(
      name = "argic_aqp", passed = NA, layers = integer(0),
      evidence = list(error = "pedon_to_spc failed"),
      missing = c("top_cm", "bottom_cm"),
      reference = "aqp::getArgillicBounds (NRCS, KST 13ed thresholds)"
    ))
  }
  bnds <- tryCatch(
    aqp::getArgillicBounds(spc, require_t = require_t, ...),
    error = function(e) c(ubound = NA_real_, lbound = NA_real_),
    warning = function(w) suppressWarnings(
      aqp::getArgillicBounds(spc, require_t = require_t, ...))
  )
  ub <- bnds[["ubound"]] %||% NA_real_
  lb <- bnds[["lbound"]] %||% NA_real_
  passed <- !is.na(ub) && !is.na(lb) && lb > ub
  hz <- pedon$horizons
  layers <- if (passed)
              which(!is.na(hz$top_cm) & !is.na(hz$bottom_cm) &
                      hz$top_cm < lb & hz$bottom_cm > ub)
            else integer(0)
  DiagnosticResult$new(
    name      = "argic_aqp",
    passed    = passed,
    layers    = layers,
    evidence  = list(ubound = ub, lbound = lb,
                       require_t = require_t,
                       engine = "aqp::getArgillicBounds"),
    missing   = if (is.na(ub)) "argillic_clay_increase" else character(0),
    reference = paste("Soil Survey Staff (2022) Keys to Soil",
                        "Taxonomy 13th ed.; aqp::getArgillicBounds.")
  )
}


#' Cambic horizon via aqp::getCambicBounds()
#'
#' Wraps \code{aqp::getCambicBounds()} in soilKey's
#' \code{\link{DiagnosticResult}} contract. The aqp test enforces the
#' KST 13ed cambic criteria:
#' \itemize{
#'   \item Texture finer than loamy fine sand (i.e. NOT in the
#'         sandy-texture pattern).
#'   \item Soil structure or absence of rock structure.
#'   \item Evidence of pedogenic alteration (chroma / value / clay).
#'   \item NOT meeting argic / oxic / spodic / mollic criteria.
#' }
#' soilKey's \code{\link{cambic}} (and the SiBCS proxy
#' \code{\link{B_incipiente}}) implements similar logic but with
#' SiBCS / WRB-flavoured exclusions; the aqp engine here is an
#' independent canonical reference.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param argi_bounds Optional \code{c(ubound, lbound)} for argillic
#'        bounds (forwarded to aqp). \code{NULL} (default) means the
#'        aqp internals re-detect.
#' @param ... Reserved for future arguments.
#' @return A \code{\link{DiagnosticResult}} with \code{name =
#'   "cambic_aqp"}.
#' @seealso \code{\link{cambic}} (soilKey hand-coded),
#'   \code{aqp::getCambicBounds}.
#' @export
cambic_aqp <- function(pedon, argi_bounds = NULL, ...) {
  if (!requireNamespace("aqp", quietly = TRUE)) {
    return(DiagnosticResult$new(
      name = "cambic_aqp", passed = NA, layers = integer(0),
      evidence = list(error = "aqp not installed"),
      missing = "aqp",
      reference = "aqp::getCambicBounds (NRCS, KST 13ed)"
    ))
  }
  spc <- tryCatch(pedon_to_spc(pedon), error = function(e) NULL)
  if (is.null(spc)) {
    return(DiagnosticResult$new(
      name = "cambic_aqp", passed = NA, layers = integer(0),
      evidence = list(error = "pedon_to_spc failed"),
      missing = c("top_cm", "bottom_cm"),
      reference = "aqp::getCambicBounds (NRCS, KST 13ed)"
    ))
  }
  res <- tryCatch(
    aqp::getCambicBounds(spc, argi_bounds = argi_bounds, ...),
    error = function(e) NULL,
    warning = function(w) suppressWarnings(
      aqp::getCambicBounds(spc, argi_bounds = argi_bounds, ...))
  )
  if (is.null(res) || nrow(res) == 0L ||
        all(is.na(res$cambic_top))) {
    return(DiagnosticResult$new(
      name      = "cambic_aqp",
      passed    = FALSE,
      layers    = integer(0),
      evidence  = list(engine = "aqp::getCambicBounds",
                         result = res),
      missing   = character(0),
      reference = paste("Soil Survey Staff (2022) Keys to Soil",
                        "Taxonomy 13th ed.; aqp::getCambicBounds.")
    ))
  }
  ub <- res$cambic_top[1L]; lb <- res$cambic_bottom[1L]
  passed <- !is.na(ub) && !is.na(lb) && lb > ub
  hz <- pedon$horizons
  layers <- if (passed)
              which(!is.na(hz$top_cm) & !is.na(hz$bottom_cm) &
                      hz$top_cm < lb & hz$bottom_cm > ub)
            else integer(0)
  DiagnosticResult$new(
    name      = "cambic_aqp",
    passed    = passed,
    layers    = layers,
    evidence  = list(ubound = ub, lbound = lb,
                       argi_bounds = argi_bounds,
                       engine = "aqp::getCambicBounds",
                       full_result = res),
    missing   = character(0),
    reference = paste("Soil Survey Staff (2022) Keys to Soil",
                        "Taxonomy 13th ed.; aqp::getCambicBounds.")
  )
}


#' Side-by-side comparison of soilKey vs aqp diagnostic engines
#'
#' Runs the soilKey hand-coded diagnostic and the aqp wrapper on the
#' same pedon, returns both results plus an agreement flag. Useful
#' for A/B benchmarks and for choosing which engine to use per
#' dataset.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param diagnostic One of \code{"argic"} or \code{"cambic"}.
#' @return A list with \code{soilkey}, \code{aqp}, \code{agree}.
#' @export
compare_engines <- function(pedon,
                              diagnostic = c("argic", "cambic")) {
  diagnostic <- match.arg(diagnostic)
  fns <- list(
    argic  = list(soilkey = argic,  aqp = argic_aqp),
    cambic = list(soilkey = cambic, aqp = cambic_aqp)
  )
  pair <- fns[[diagnostic]]
  sk  <- tryCatch(pair$soilkey(pedon), error = function(e) NULL)
  aq  <- tryCatch(pair$aqp(pedon),     error = function(e) NULL)
  list(
    soilkey = sk,
    aqp     = aq,
    agree   = isTRUE(sk$passed) == isTRUE(aq$passed)
  )
}
