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
#' Walks the canonical 4-level USDA key (Order -> Suborder ->
#' Great Group -> Subgroup) using YAML rule files at:
#' \itemize{
#'   \item \code{inst/rules/usda/key.yaml}: Order key (12 entries)
#'   \item \code{inst/rules/usda/suborders/<order>.yaml}
#'   \item \code{inst/rules/usda/great-groups/<order>.yaml}
#'   \item \code{inst/rules/usda/subgroups/<order>.yaml}
#' }
#'
#' Stops at the deepest level for which a YAML rule file is
#' available (e.g. v0.8.x: Gelisols full Path C; other 11 Orders at
#' Order level only).
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param rules Optional pre-loaded rule set.
#' @param on_missing One of \code{"warn"} (default), \code{"silent"},
#'        \code{"error"}.
#' @return A \code{\link{ClassificationResult}} with deepest-level
#'         taxon name. Each level's trace is in \code{$trace}.
#' @references Soil Survey Staff (2022). Keys to Soil Taxonomy, 13th
#'   edition. USDA Natural Resources Conservation Service.
#' @export
classify_usda <- function(pedon,
                            rules      = NULL,
                            on_missing = c("warn", "silent", "error")) {
  on_missing <- match.arg(on_missing)
  rules      <- rules %||% load_rules("usda")

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

  trace_combined <- list(
    orders                = key_result$trace,
    suborders             = sub_result$trace,
    suborder_assigned     = suborder,
    great_groups          = gg_result$trace,
    great_group_assigned  = gg,
    subgroups             = sg_result$trace,
    subgroup_assigned     = sg
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
