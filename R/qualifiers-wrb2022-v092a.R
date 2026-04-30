# ============================================================================
# WRB 2022 (4th ed.) -- Sub-qualifiers v0.9.2.A
#
# Adds the parametric Hyper- / Hypo- / Proto- sub-qualifiers that
# refine the salinity (Salic), sodicity (Sodic), carbonate (Calcic),
# gypsum (Gypsic) and shrink-swell (Vertic) families. Each function
# returns a DiagnosticResult and follows the standard contract; depth
# gates default to 100 cm.
#
# WRB 2022 sub-qualifier conventions used here:
#   Hyper-  stronger expression than the base qualifier (e.g.
#           Hypersalic >= 30 dS/m vs. Salic >= 15 dS/m)
#   Hypo-   marginal / weaker expression, typically used when the soil
#           is below the Salic/Sodic/Calcic/Gypsic horizon threshold
#           but still carries enough of the property to deserve a tag
#   Proto-  incipient form of the property, with Munsell / chemistry
#           markers but not the full diagnostic horizon
# ============================================================================


# ---------- SALINITY -------------------------------------------------------

#' Hypersalic qualifier (yz): EC (1:5 H2O extract) >= 30 dS/m in some
#' layer within the upper 100 cm. Stronger than the Salic horizon
#' (default >= 15 dS/m).
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_hypersalic <- function(pedon) {
  h <- pedon$horizons
  ly <- .in_upper(pedon, 100)
  if (length(ly) == 0L)
    return(DiagnosticResult$new(name = "Hypersalic", passed = NA,
            layers = integer(0), evidence = list(),
            missing = "ec_dS_m",
            reference = "WRB (2022) Ch 5, Hypersalic"))
  ec <- h$ec_dS_m[ly]
  ok <- !is.na(ec) & ec >= 30
  passed <- any(ok)
  DiagnosticResult$new(
    name = "Hypersalic", passed = passed,
    layers = ly[ok],
    evidence = list(ec_dS_m = ec),
    missing = if (all(is.na(ec))) "ec_dS_m" else character(0),
    reference = "WRB (2022) Ch 5, Hypersalic"
  )
}

#' Hyposalic qualifier (jz): EC (1:5 H2O extract) >= 4 dS/m AND < 15
#' dS/m in some layer within the upper 100 cm. Used for soils too
#' weak to qualify as Solonchak but still carrying a salinity tag.
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_hyposalic <- function(pedon) {
  h <- pedon$horizons
  ly <- .in_upper(pedon, 100)
  if (length(ly) == 0L)
    return(DiagnosticResult$new(name = "Hyposalic", passed = NA,
            layers = integer(0), evidence = list(),
            missing = "ec_dS_m",
            reference = "WRB (2022) Ch 5, Hyposalic"))
  ec <- h$ec_dS_m[ly]
  ok <- !is.na(ec) & ec >= 4 & ec < 15
  passed <- any(ok)
  DiagnosticResult$new(
    name = "Hyposalic", passed = passed,
    layers = ly[ok],
    evidence = list(ec_dS_m = ec),
    missing = if (all(is.na(ec))) "ec_dS_m" else character(0),
    reference = "WRB (2022) Ch 5, Hyposalic"
  )
}


# ---------- SODICITY -------------------------------------------------------

# Helper: ESP (exchangeable sodium percentage) per layer.
.esp <- function(h, layers) {
  vapply(layers, function(i) {
    if (is.na(h$na_cmol[i]) || is.na(h$cec_cmol[i]) || h$cec_cmol[i] <= 0)
      NA_real_
    else h$na_cmol[i] / h$cec_cmol[i] * 100
  }, numeric(1))
}

#' Hypersodic qualifier (yo): ESP >= 50\% in some layer within 100 cm.
#' Stronger than Sodic (default ESP >= 6\%).
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_hypersodic <- function(pedon) {
  h <- pedon$horizons
  ly <- .in_upper(pedon, 100)
  if (length(ly) == 0L)
    return(DiagnosticResult$new(name = "Hypersodic", passed = NA,
            layers = integer(0), evidence = list(),
            missing = c("na_cmol", "cec_cmol"),
            reference = "WRB (2022) Ch 5, Hypersodic"))
  esp <- .esp(h, ly)
  ok <- !is.na(esp) & esp >= 50
  passed <- any(ok)
  DiagnosticResult$new(
    name = "Hypersodic", passed = passed,
    layers = ly[ok],
    evidence = list(esp = esp),
    missing = if (all(is.na(esp))) c("na_cmol", "cec_cmol") else character(0),
    reference = "WRB (2022) Ch 5, Hypersodic"
  )
}

#' Hyposodic qualifier (jo): ESP >= 6\% AND < 15\% in some layer within
#' 100 cm. Marginal sodicity tag.
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_hyposodic <- function(pedon) {
  h <- pedon$horizons
  ly <- .in_upper(pedon, 100)
  if (length(ly) == 0L)
    return(DiagnosticResult$new(name = "Hyposodic", passed = NA,
            layers = integer(0), evidence = list(),
            missing = c("na_cmol", "cec_cmol"),
            reference = "WRB (2022) Ch 5, Hyposodic"))
  esp <- .esp(h, ly)
  ok <- !is.na(esp) & esp >= 6 & esp < 15
  passed <- any(ok)
  DiagnosticResult$new(
    name = "Hyposodic", passed = passed,
    layers = ly[ok],
    evidence = list(esp = esp),
    missing = if (all(is.na(esp))) c("na_cmol", "cec_cmol") else character(0),
    reference = "WRB (2022) Ch 5, Hyposodic"
  )
}


# ---------- CARBONATES -----------------------------------------------------

#' Hypercalcic qualifier (yc): calcic horizon AND CaCO3 >= 50\% in some
#' calcic layer.
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_hypercalcic <- function(pedon) {
  cc <- calcic(pedon)
  if (!isTRUE(cc$passed))
    return(DiagnosticResult$new(name = "Hypercalcic", passed = FALSE,
            layers = integer(0), evidence = list(calcic = cc),
            missing = cc$missing %||% character(0),
            reference = "WRB (2022) Ch 5, Hypercalcic"))
  h <- pedon$horizons
  ly <- intersect(cc$layers, .in_upper(pedon, 100))
  ca <- h$caco3_pct[ly]
  ok <- !is.na(ca) & ca >= 50
  passed <- any(ok)
  DiagnosticResult$new(
    name = "Hypercalcic", passed = passed,
    layers = ly[ok],
    evidence = list(calcic = cc, caco3_pct = ca),
    missing = if (all(is.na(ca))) "caco3_pct" else character(0),
    reference = "WRB (2022) Ch 5, Hypercalcic"
  )
}

#' Hypocalcic qualifier (jc): CaCO3 >= 5\% AND < 15\% in some layer
#' within 100 cm (between protocalcic 0.5\% and the calcic-horizon
#' 15\% threshold). Marks the broad "carbonate-bearing" middle band
#' that doesn't meet the Calcic horizon.
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_hypocalcic <- function(pedon) {
  h <- pedon$horizons
  ly <- .in_upper(pedon, 100)
  if (length(ly) == 0L)
    return(DiagnosticResult$new(name = "Hypocalcic", passed = NA,
            layers = integer(0), evidence = list(),
            missing = "caco3_pct",
            reference = "WRB (2022) Ch 5, Hypocalcic"))
  ca <- h$caco3_pct[ly]
  ok <- !is.na(ca) & ca >= 5 & ca < 15
  passed <- any(ok)
  DiagnosticResult$new(
    name = "Hypocalcic", passed = passed,
    layers = ly[ok],
    evidence = list(caco3_pct = ca),
    missing = if (all(is.na(ca))) "caco3_pct" else character(0),
    reference = "WRB (2022) Ch 5, Hypocalcic"
  )
}

#' Protocalcic qualifier (qc): protocalcic properties (incipient
#' carbonate accumulation) within the upper 100 cm. Wraps
#' \code{\link{protocalcic_properties}}.
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_protocalcic <- function(pedon) .q_presence("Protocalcic",
  protocalcic_properties(pedon), 100, pedon)


# ---------- GYPSUM ---------------------------------------------------------

#' Hypergypsic qualifier (yg): gypsic horizon AND gypsum >= 60\% in
#' some gypsic layer.
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_hypergypsic <- function(pedon) {
  gy <- gypsic(pedon)
  if (!isTRUE(gy$passed))
    return(DiagnosticResult$new(name = "Hypergypsic", passed = FALSE,
            layers = integer(0), evidence = list(gypsic = gy),
            missing = gy$missing %||% character(0),
            reference = "WRB (2022) Ch 5, Hypergypsic"))
  h <- pedon$horizons
  ly <- intersect(gy$layers, .in_upper(pedon, 100))
  s <- h$caso4_pct[ly]
  ok <- !is.na(s) & s >= 60
  passed <- any(ok)
  DiagnosticResult$new(
    name = "Hypergypsic", passed = passed,
    layers = ly[ok],
    evidence = list(gypsic = gy, caso4_pct = s),
    missing = if (all(is.na(s))) "caso4_pct" else character(0),
    reference = "WRB (2022) Ch 5, Hypergypsic"
  )
}

#' Hypogypsic qualifier (jg): gypsum >= 1\% AND < 5\% in some layer
#' within 100 cm (below the gypsic-horizon threshold but above the
#' protogypsic-properties bare-detection bar).
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_hypogypsic <- function(pedon) {
  h <- pedon$horizons
  ly <- .in_upper(pedon, 100)
  if (length(ly) == 0L)
    return(DiagnosticResult$new(name = "Hypogypsic", passed = NA,
            layers = integer(0), evidence = list(),
            missing = "caso4_pct",
            reference = "WRB (2022) Ch 5, Hypogypsic"))
  s <- h$caso4_pct[ly]
  ok <- !is.na(s) & s >= 1 & s < 5
  passed <- any(ok)
  DiagnosticResult$new(
    name = "Hypogypsic", passed = passed,
    layers = ly[ok],
    evidence = list(caso4_pct = s),
    missing = if (all(is.na(s))) "caso4_pct" else character(0),
    reference = "WRB (2022) Ch 5, Hypogypsic"
  )
}

#' Protogypsic qualifier (qg): protogypsic properties (incipient
#' gypsum accumulation) within the upper 100 cm. Wraps
#' \code{\link{protogypsic_properties}}.
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_protogypsic <- function(pedon) .q_presence("Protogypsic",
  protogypsic_properties(pedon), 100, pedon)


# ---------- VERTIC --------------------------------------------------------

#' Protovertic qualifier (qv): protovertic horizon (vertic-spectrum
#' lower bound, no slickensides yet but the clay + structure /
#' shrink-swell signal is already present) within the upper 100 cm.
#' Wraps \code{\link{protovertic}} and is mutually exclusive with the
#' strict Vertic qualifier.
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_protovertic <- function(pedon) {
  pv <- protovertic(pedon)
  if (!isTRUE(pv$passed))
    return(DiagnosticResult$new(name = "Protovertic", passed = FALSE,
            layers = integer(0), evidence = list(protovertic = pv),
            missing = pv$missing %||% character(0),
            reference = "WRB (2022) Ch 5, Protovertic"))
  v  <- vertic_horizon(pedon)
  ly <- intersect(pv$layers, .in_upper(pedon, 100))
  # Strict vertic supersedes -- if any candidate layer also meets
  # vertic_horizon, drop it from the protovertic set.
  ly <- setdiff(ly, v$layers %||% integer(0))
  passed <- length(ly) > 0L
  DiagnosticResult$new(
    name = "Protovertic", passed = passed,
    layers = ly,
    evidence = list(protovertic = pv, vertic_horizon = v),
    missing = pv$missing %||% character(0),
    reference = "WRB (2022) Ch 5, Protovertic"
  )
}
