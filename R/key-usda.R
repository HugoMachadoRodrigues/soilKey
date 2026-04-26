# ================================================================
# USDA Soil Taxonomy key (v0.2 scaffold)
#
# v0.2 wires only one Order end-to-end (Oxisols, via the delegating
# oxic_usda() diagnostic). The remaining 11 Orders are stubbed in
# inst/rules/usda/key.yaml with not_implemented_v01 markers; they
# remain NA in the trace and the engine continues.
#
# The full USDA implementation -- mollic_usda / umbric / ochric /
# argillic / kandic / spodic_usda / oxic_usda / cambic_usda / calcic /
# gypsic / aridic moisture regime / xeric / udic / etc. plus suborders
# / great groups / subgroups -- is scheduled for v0.8.
# ================================================================


#' Run the USDA Soil Taxonomy key over a pedon
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param rules Optional pre-loaded rule set; if NULL, reads
#'        \code{inst/rules/usda/key.yaml}.
#' @return A list with \code{assigned} (the YAML entry of the assigned
#'         Order) and \code{trace}.
#' @export
run_usda_key <- function(pedon, rules = NULL) {
  rules <- rules %||% load_rules("usda")
  run_taxonomic_key(pedon, rules, level_key = "orders")
}


#' Classify a pedon under USDA Soil Taxonomy
#'
#' v0.2 scaffold: only the Oxisols path is wired, via the
#' \code{\link{oxic_usda}} diagnostic that delegates to WRB
#' \code{\link{ferralic}}. The other 11 Orders are stubbed and any
#' profile not meeting the oxic criteria falls through to the
#' Entisols catch-all.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param rules Optional pre-loaded rule set.
#' @param on_missing One of \code{"warn"} (default), \code{"silent"},
#'        \code{"error"}.
#' @return A \code{\link{ClassificationResult}}.
#' @export
classify_usda <- function(pedon,
                            rules      = NULL,
                            on_missing = c("warn", "silent", "error")) {
  on_missing <- match.arg(on_missing)
  rules      <- rules %||% load_rules("usda")

  key_result <- run_usda_key(pedon, rules)
  order      <- key_result$assigned

  order_codes <- vapply(rules$orders, function(o) o$code, character(1))
  is_default  <- identical(order$code, tail(order_codes, 1L))

  ambiguities  <- find_ambiguities(key_result$trace, current = order$code)
  grade        <- compute_evidence_grade(pedon, key_result$trace)
  missing_data <- collect_missing_attributes(key_result$trace)

  warnings <- character(0)
  if (is_default) {
    warnings <- c(warnings, paste0(
      "Profile keyed to USDA Entisols catch-all. v0.2 only wires the ",
      "Oxisols path (via oxic_usda); the remaining 11 Orders are ",
      "scheduled for v0.8."
    ))
  }

  if (length(missing_data) > 0L) {
    msg <- sprintf(
      "%d distinct attribute(s) missing across the key trace -- see $missing_data",
      length(missing_data)
    )
    if      (on_missing == "warn")  warnings <- c(warnings, msg)
    else if (on_missing == "error") rlang::abort(msg)
  }

  ClassificationResult$new(
    system         = "USDA Soil Taxonomy",
    name           = order$name,
    rsg_or_order   = order$name,
    qualifiers     = list(),
    trace          = key_result$trace,
    ambiguities    = ambiguities,
    missing_data   = missing_data,
    evidence_grade = grade,
    prior_check    = NULL,
    warnings       = warnings
  )
}
