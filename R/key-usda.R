# ================================================================
# USDA Soil Taxonomy key (v0.8 -- Path C: Suborders -> Great Groups
# -> Subgroups). The 12 Orders (Order key) are wired in
# inst/rules/usda/key.yaml; sub-levels are wired per-Order in:
#   inst/rules/usda/suborders/{order}.yaml
#   inst/rules/usda/great-groups/{order}.yaml
#   inst/rules/usda/subgroups/{order}.yaml
#
# Stage 1 (v0.8.x) implements diagnostic epipedons (Ch 3), diagnostic
# characteristics (Ch 3), and Cap 9 Gelisols end-to-end. Caps 5-16
# (the other 11 Orders) follow.
# ================================================================


#' Run the USDA Soil Taxonomy Order key over a pedon
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


#' Run the USDA Suborder key for a given Order
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param order_code The Order code (e.g. "GE" for Gelisols).
#' @param rules Optional pre-loaded rule set.
#' @return A list with \code{assigned} and \code{trace}; assigned is
#'         NULL if the Order has no suborders YAML.
#' @export
run_usda_suborder <- function(pedon, order_code, rules = NULL) {
  rules <- rules %||% load_rules("usda")
  run_taxa_list(pedon, rules$suborders[[order_code]])
}


#' Run the USDA Great Group key for a given Suborder
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param suborder_code The Suborder code (e.g. "AA" for Histels).
#' @param rules Optional pre-loaded rule set.
#' @return A list with \code{assigned} and \code{trace}; assigned is
#'         NULL if the Suborder has no great-groups YAML.
#' @export
run_usda_great_group <- function(pedon, suborder_code, rules = NULL) {
  rules <- rules %||% load_rules("usda")
  run_taxa_list(pedon, rules$great_groups[[suborder_code]])
}


#' Run the USDA Subgroup key for a given Great Group
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param great_group_code The Great Group code (e.g. "AAA" for
#'        Folistels).
#' @param rules Optional pre-loaded rule set.
#' @return A list with \code{assigned} and \code{trace}; assigned is
#'         NULL if the Great Group has no subgroups YAML.
#' @export
run_usda_subgroup <- function(pedon, great_group_code, rules = NULL) {
  rules <- rules %||% load_rules("usda")
  run_taxa_list(pedon, rules$subgroups[[great_group_code]])
}


#' Classify a pedon under USDA Soil Taxonomy (13th edition)
#'
#' Walks the canonical USDA key (Order -> Suborder -> Great Group ->
#' Subgroup) using YAML rule files at:
#' \itemize{
#'   \item \code{inst/rules/usda/key.yaml}: Order key (12 entries)
#'   \item \code{inst/rules/usda/suborders/<order>.yaml}
#'   \item \code{inst/rules/usda/great-groups/<order>.yaml}
#'   \item \code{inst/rules/usda/subgroups/<order>.yaml}
#' }
#'
#' With \code{include_family = TRUE} it additionally derives the 5th
#' category, the \strong{family} -- a set of class modifiers
#' (particle-size, mineralogy, CEC-activity, reaction, temperature
#' regime, depth) PREPENDED to the subgroup name, e.g. \emph{"fine,
#' kaolinitic, isohyperthermic Rhodic Hapludox"}. See
#' \code{\link{classify_usda_family}}.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param rules Optional pre-loaded rule set.
#' @param on_missing One of \code{"warn"} (default), \code{"silent"},
#'        \code{"error"}.
#' @param include_family If \code{TRUE}, derive and prepend the 5th-level
#'        family modifiers. Default \code{FALSE} (output byte-identical to
#'        earlier versions).
#' @param infer_temperature When deriving the family and
#'        \code{site$soil_temperature_regime} is not recorded, suggest a
#'        temperature class from latitude and elevation (a crude lapse-rate
#'        model, "iso" within 23 degrees of the equator). Default
#'        \code{FALSE} since v0.9.415: the regime is not inferred unless asked
#'        for, and the family then carries no temperature class. A suggested
#'        class lowers the evidence grade to "C" and adds a warning. The key
#'        itself (orders to subgroups) never infers a regime.
#' @param gapfill Opt-in within-pedon depth gap-fill, default \code{FALSE}
#'        (no-op, classification stays byte-identical). \code{TRUE} fills
#'        interior \code{NA} cells of the continuous depth-trending attributes
#'        by linear interpolation from the profile's own measured horizons; a
#'        character vector restricts it to those attributes; a named list is
#'        passed to \code{\link{gapfill_within_pedon}}. Filled cells carry
#'        \code{inferred_prior} provenance, so the evidence grade drops to
#'        \code{"C"}. Runs on a deep copy -- the caller's pedon is never mutated.
#' @section Undecided tests and regimes:
#' A test whose data are missing is undecided (NA), not failed. The key goes
#' on past it, as the WRB key does, and records it: every taxon tested before
#' the one assigned, at the order, suborder, great group and subgroup levels,
#' whose result is NA is listed in \code{$ambiguities} (with \code{level},
#' one of \code{"orders"}, \code{"suborders"}, \code{"great_groups"},
#' \code{"subgroups"}), and \code{$missing_data} collects what was missing
#' at every level. When the name depends on an undecided test, it holds only
#' if that test fails: it is inferred by elimination, so the evidence grade
#' falls from \code{"A"} or \code{"B"} to \code{"C"} (the grade of an
#' inferred result) and a warning says so. When no taxon of a level passes
#' and some were undecided, the level is not named and the name stops at the
#' level above.
#'
#' The soil moisture and temperature regimes are read from
#' \code{site$soil_moisture_regime} and \code{site$soil_temperature_regime}
#' and never inferred in the key: with no regime recorded every regime test
#' is undecided and lists the site field as missing (KST 13th ed., pp.
#' 47-55). Taxa keyed as "Other" in the KST (Udalfs, Udepts, Udolls, Udox,
#' ...) are still reached, past the undecided regime taxa, with grade C;
#' Udults need a udic or perudic regime (p. 321; "ud" covers both, p. 48), so
#' an Ultisol with no regime recorded has no suborder.
#'
#' @return A \code{\link{ClassificationResult}} with deepest-level
#'         taxon name. Each level's trace is in \code{$trace}; the family
#'         attributes are in \code{$trace$family}.
#' @references Soil Survey Staff (2022). Keys to Soil Taxonomy, 13th
#'   edition. USDA Natural Resources Conservation Service.
#' @examples
#' pedon <- make_ferralsol_canonical()
#' res <- classify_usda(pedon)
#' res$name
#' # include the 5th (family) level:
#' classify_usda(pedon, include_family = TRUE)$name
#' @export
classify_usda <- function(pedon,
                            rules      = NULL,
                            on_missing = c("warn", "silent", "error"),
                            include_family = FALSE,
                            infer_temperature = FALSE,
                            gapfill    = FALSE) {
  on_missing <- match.arg(on_missing)
  .classify_require_horizons(pedon, "classify_usda")
  # v0.9.400: one memo scope for the length of this call (R/memo.R)
  if (.memo_open()) on.exit(.memo_close(), add = TRUE)
  rules      <- rules %||% load_rules("usda")

  # Opt-in within-pedon gap-fill (default off => byte-identical). Deep copy,
  # so the caller's pedon is never mutated.
  pedon <- .classify_apply_gapfill(pedon, gapfill)

  # Level 1: Order
  key_result <- run_usda_key(pedon, rules)
  order      <- key_result$assigned

  order_codes <- vapply(rules$orders, function(o) o$code, character(1))
  is_default  <- identical(order$code, tail(order_codes, 1L))

  # Level 2: Suborder (Cap chapters 5-16)
  sub_result <- if (!is.null(order))
                  run_usda_suborder(pedon, order$code, rules)
                else list(assigned = NULL, trace = list())
  suborder <- sub_result$assigned
  if (!is.null(suborder) && !isTRUE(sub_result$trace[[suborder$code]]$passed)) {
    suborder <- NULL
  }

  # Level 3: Great Group
  gg_result <- if (!is.null(suborder))
                 run_usda_great_group(pedon, suborder$code, rules)
               else list(assigned = NULL, trace = list())
  gg <- gg_result$assigned
  if (!is.null(gg) && !isTRUE(gg_result$trace[[gg$code]]$passed)) {
    gg <- NULL
  }

  # Level 4: Subgroup
  sg_result <- if (!is.null(gg))
                 run_usda_subgroup(pedon, gg$code, rules)
               else list(assigned = NULL, trace = list())
  sg <- sg_result$assigned
  # v0.9.415: as at the levels above, a subgroup whose tests did not pass is
  # not named (a list that ends without a default may end undecided).
  if (!is.null(sg) && !isTRUE(sg_result$trace[[sg$code]]$passed)) {
    sg <- NULL
  }

  # Display name = deepest assigned level
  display_name <- if (!is.null(sg))            sg$name
                  else if (!is.null(gg))       gg$name
                  else if (!is.null(suborder)) suborder$name
                  else                         order$name

  # Level 5 (v0.9.104): family. Multi-label modifiers PREPENDED to the
  # subgroup name, e.g. "fine, kaolinitic, isohyperthermic Rhodic Hapludox".
  family_attrs <- NULL
  family_lbl   <- NULL
  if (isTRUE(include_family)) {
    family_attrs <- tryCatch(
      classify_usda_family(
        pedon,
        order_code        = order$code,
        subgroup_code     = if (!is.null(sg)) sg$code else NULL,
        infer_temperature = infer_temperature),
      error = function(e) list())
    family_lbl <- family_label_usda(family_attrs)
    if (!is.null(family_lbl) && nzchar(family_lbl))
      display_name <- paste(family_lbl, display_name)
  }

  trace_combined <- list(
    orders                = key_result$trace,
    suborders             = sub_result$trace,
    suborder_assigned     = suborder,
    great_groups          = gg_result$trace,
    great_group_assigned  = gg,
    subgroups             = sg_result$trace,
    subgroup_assigned     = sg,
    family                = family_attrs,
    family_label          = family_lbl
  )

  # v0.9.415 (audit decision P1 (c), Hugo, 2026-11-20): every level records
  # its undecided tests. A taxon tested before the one assigned, at the order,
  # suborder, great group or subgroup level, whose result is NA joins
  # $ambiguities with its level; $missing_data collects from every level; and
  # a name reached past an undecided test was reached by elimination, so the
  # evidence grade falls from A or B to C, the grade of an inferred result.
  # Up to v0.9.414 only the order level was read.
  level_traces <- list(orders       = key_result$trace,
                       suborders    = sub_result$trace,
                       great_groups = gg_result$trace,
                       subgroups    = sg_result$trace)
  level_assigned <- list(orders       = order$code,
                         suborders    = suborder$code,
                         great_groups = gg$code,
                         subgroups    = sg$code)
  und <- .usda_undecided_by_level(level_traces, level_assigned)
  ambiguities  <- und$ambiguities
  grade        <- compute_evidence_grade(pedon, key_result$trace)
  missing_data <- unique(unlist(lapply(level_traces, collect_missing_attributes),
                                use.names = FALSE)) %||% character(0)

  warnings <- character(0)
  if (is.na(grade)) warnings <- c(warnings, .NO_PROPERTIES_WARNING)
  if (length(und$before_assigned) > 0L) {
    if (!is.na(grade) && grade %in% c("A", "B")) grade <- "C"
    warnings <- c(warnings, sprintf(paste0(
      "The name was reached past %d undecided test(s) (%s): it holds only if ",
      "they fail, so it is inferred by elimination and the evidence grade ",
      "is at most C. See $ambiguities and $missing_data."),
      length(und$before_assigned), paste(und$before_assigned, collapse = ", ")))
  }
  # v0.9.420 (audit reading L19): a test on the path of the name whose result
  # rests on an inference (its evidence carries an `inference` note, e.g. the
  # base saturation of the Ultisols read in the deepest horizon of a profile
  # that ends above the depth of the KST) lowers the grade to C.
  inferences <- .usda_path_inferences(level_traces, level_assigned)
  if (length(inferences)) {
    if (!is.na(grade) && grade %in% c("A", "B")) grade <- "C"
    warnings <- c(warnings, paste(
      "Inferred, so the evidence grade is at most C:", inferences))
  }
  for (lv in und$undecided_levels) {
    warnings <- c(warnings, sprintf(paste0(
      "%s undecided: no taxon passed and %s could not be decided, so the ",
      "name stops at the level above. See $ambiguities."),
      .USDA_LEVEL_LABEL[[lv]], paste(und$by_level[[lv]], collapse = ", ")))
  }
  # v0.9.415 (audit decision P3 (a)): a family temperature class inferred
  # from latitude and elevation (infer_temperature = TRUE, opt-in) is a
  # suggestion, not a recorded regime.
  ft <- family_attrs$temperature_regime
  if (!is.null(ft) && isTRUE(ft$evidence$inferred) && !is.null(ft$value)) {
    if (!is.na(grade) && grade %in% c("A", "B")) grade <- "C"
    warnings <- c(warnings, sprintf(paste0(
      "Family temperature class '%s' is a suggestion inferred from latitude ",
      "and elevation (infer_temperature = TRUE), not a recorded regime; the ",
      "evidence grade is at most C. Record site$soil_temperature_regime."),
      ft$value))
  }
  if (is_default) {
    warnings <- c(warnings, paste0(
      "Profile keyed to USDA Entisols catch-all. ",
      "Verify whether profile is genuinely Entisol-like (no other ",
      "Order's diagnostic horizons or characteristics)."
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
    name           = display_name,
    rsg_or_order   = order$name,
    qualifiers     = list(),
    trace          = trace_combined,
    ambiguities    = ambiguities,
    missing_data   = missing_data,
    evidence_grade = grade,
    prior_check    = NULL,
    warnings       = warnings,
    system_id      = "usda"
  )
}


# v0.9.420: the inference notes of the tests that named each level (the
# `inference` element of their evidence), in key order.
.usda_path_inferences <- function(level_traces, level_assigned) {
  out <- character(0)
  for (lv in names(level_traces)) {
    code <- level_assigned[[lv]]
    if (is.null(code)) next
    entry <- level_traces[[lv]][[code]]
    if (is.null(entry) || !isTRUE(entry$passed)) next
    for (r in entry$evidence) {
      note <- tryCatch(r$evidence$inference, error = function(e) NULL)
      if (is.character(note) && length(note)) out <- c(out, note)
    }
  }
  unique(out)
}
