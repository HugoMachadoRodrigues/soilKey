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
#' @param infer_temperature When deriving the family, infer the soil
#'        temperature regime from latitude/elevation if
#'        \code{site$soil_temperature_regime} is absent (default
#'        \code{TRUE}). See \code{family_temperature_regime_usda}.
#' @param gapfill Opt-in within-pedon depth gap-fill, default \code{FALSE}
#'        (no-op, classification stays byte-identical). \code{TRUE} fills
#'        interior \code{NA} cells of the continuous depth-trending attributes
#'        by linear interpolation from the profile's own measured horizons; a
#'        character vector restricts it to those attributes; a named list is
#'        passed to \code{\link{gapfill_within_pedon}}. Filled cells carry
#'        \code{inferred_prior} provenance, so the evidence grade drops to
#'        \code{"C"}. Runs on a deep copy -- the caller's pedon is never mutated.
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
                            infer_temperature = TRUE,
                            gapfill    = FALSE) {
  on_missing <- match.arg(on_missing)
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

  ambiguities  <- find_ambiguities(key_result$trace, current = order$code)
  grade        <- compute_evidence_grade(pedon, key_result$trace)
  missing_data <- collect_missing_attributes(key_result$trace)

  warnings <- character(0)
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
    warnings       = warnings
  )
}
