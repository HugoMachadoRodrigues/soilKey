# =============================================================================
# v0.9.411 -- WRB 2022 andic and humus qualifiers: helpers.
#
#   * .wrb_andic_group_specified(): Aluandic and Silandic at different depths,
#     each with its specifier (Ch 2.1, p. 24-25).
#   * qual_hyperhumic(): the Hyperhumic subqualifier of Humic (Ch 5, p. 137).
#   * .mulmic_structure_ok() and .mulmic_drained_origin(): criteria of mulmic
#     material (Ch 3.3.12, p. 90) read by mulmic_material().
# =============================================================================

# The slash group of the Andosols (Ch 4, p. 104): "Aluandic/ Silandic".
.WRB_ANDIC_GROUP <- c("Aluandic", "Silandic")

# WRB 2022 Ch 2.1 (p. 24-25): "Qualifiers that are mutually exclusive may apply
# to the same soil at different depths. In this case, they can be used both,
# each one with the respective specifier ... If no specifier is used, only the
# first applicable qualifier can be used." Aluandic (p. 128) and Silandic
# (p. 146) count their layers "within 100 cm of the soil surface", so the
# specifier of Ch 2.3.1 rule 2 (p. 27) is read from the soil surface. Returns
# member -> name with its specifier, or NULL when only the first member can be
# used: a member without its layers, layers shared by both (Si_ox undecided),
# a soil not described to 100 cm or to a limiting layer, a limiting layer
# < 60 cm, or no single specifier for a member.
.wrb_andic_group_specified <- function(pedon, passed, layers_map) {
  if (length(passed) < 2L) return(NULL)
  h <- pedon$horizons
  s0 <- suppressWarnings(min(h$top_cm, na.rm = TRUE))
  if (!is.finite(s0)) return(NULL)
  lim <- tryCatch(.barrier_top_cm(pedon), error = function(e) NA_real_)
  if (!is.na(lim) && lim - s0 < 60) return(NULL)
  to <- s0 + (if (is.na(lim)) 100 else min(100, lim - s0))
  if (!isTRUE(suppressWarnings(max(h$bottom_cm, na.rm = TRUE)) >= to - 1e-6))
    return(NULL)
  ly <- lapply(passed, function(q) layers_map[[q]] %||% integer(0))
  if (any(lengths(ly) == 0L) || anyDuplicated(unlist(ly))) return(NULL)
  sp <- vapply(ly, function(l) .wrb_layer_specifier(h, l, s0, lim), character(1))
  if (anyNA(sp)) return(NULL)
  stats::setNames(paste0(sp, tolower(substring(passed, 1, 1)), substring(passed, 2)),
                  passed)
}


#' Hyperhumic qualifier (jh): >= 5\% SOC as a weighted average to 50 cm
#'
#' WRB 2022 Ch 5 (p. 137), subqualifier of Humic: "having >= 5% soil organic
#' carbon as a weighted average to a depth of 50 cm from the mineral soil
#' surface". The window and the weighted average are those of Humic
#' (qual_humic(): the range below a limiting layer counts 0), as a subqualifier
#' "that fulfils all the criteria of the qualifier" (Ch 2.3, rule 1, p. 26). A
#' part of the 50 cm with no carbon recorded leaves it undecided unless the
#' recorded part alone reaches 5\%, or could not reach it with every
#' unrecorded mineral layer at < 20\% SOC.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_hyperhumic <- function(pedon) {
  hu <- qual_humic(pedon)
  ref <- "WRB (2022) Ch 5, Hyperhumic (p. 137)"
  e <- hu$evidence
  if (is.null(e$oc_weighted))
    return(DiagnosticResult$new(name = "Hyperhumic", passed = NA,
            layers = integer(0), evidence = list(humic = hu),
            missing = "oc_pct", reference = ref))
  known <- e$oc_weighted * 50
  upper <- known + (e$unrecorded_bound %||% (20 * e$unrecorded_cm))
  passed <- if (e$oc_weighted >= 5) TRUE
            else if (e$unrecorded_cm > 1e-6 && upper >= 250) NA
            else FALSE
  DiagnosticResult$new(name = "Hyperhumic", passed = passed,
    layers = if (isTRUE(passed)) hu$layers else integer(0),
    evidence = list(oc_weighted = e$oc_weighted, humic = hu),
    missing = if (is.na(passed)) "oc_pct" else character(0),
    reference = ref)
}


# Criterion 2 of mulmic material (WRB 2022 Ch 3.3.12, p. 90): "single or in
# combination: a. a single grain structure; or b. a subangular or angular
# blocky structure with an average aggregate size of <= 2 cm", per layer:
# TRUE / FALSE / NA. A structure not recorded counts as absent (a field
# observation, Hugo's rule), so the layer fails; a blocky structure with no
# size recorded is undecided. Blocky sizes very fine, fine and medium are
# < 20 mm (FAO 2006).
.mulmic_structure_ok <- function(h) {
  n <- nrow(h)
  ty <- tolower(as.character(h$structure_type %||% rep(NA_character_, n)))
  sz <- .structure_size_class(as.character(h$structure_size %||% rep(NA_character_, n)))
  single <- !is.na(ty) & grepl("single", ty)
  blocky <- !is.na(ty) & grepl("blocky", ty) & !grepl("wedge", ty)
  ifelse(single, TRUE,
         ifelse(blocky, ifelse(is.na(sz), NA, sz <= 3L), FALSE))
}

# Mulmic material is "mineral material that has developed from
# water-saturated organic material after drainage" (WRB 2022 Ch 3.3.12,
# p. 90). Read as recorded evidence of that origin, per layer: organic
# material (>= 20% SOC, or an O or H layer) below it, or a record of
# artificial drainage of the site (as qual_drainic() reads it, Ch 5, p. 131:
# "having been artificially drained"). No such record counts as absent.
.mulmic_drained_origin <- function(pedon) {
  h <- pedon$horizons
  n <- nrow(h)
  org <- (!is.na(h$oc_pct) & h$oc_pct >= 20) |
         (!is.na(h$designation) & grepl("^[0-9]*[OH]", h$designation))
  below <- vapply(seq_len(n), function(i)
    any(org & !is.na(h$top_cm) & !is.na(h$bottom_cm[i]) &
          h$top_cm >= h$bottom_cm[i] - 1e-6), logical(1))
  re <- "artific|reclaim|drained\\s+(organic|peat|mire|wetland|fen|bog)|\\bditch\\b|drained\\)"
  site <- c(pedon$site$drainage_class %||% NA_character_,
            pedon$site$land_use %||% NA_character_)
  drained <- any(!is.na(site) & grepl(re, site, ignore.case = TRUE))
  below | drained
}


# The layer "that corresponds to the parent material" of a mollic or umbric
# horizon (WRB 2022 Ch 3.1.20, p. 52; Ch 3.1.39, p. 70) with a moist value
# <= 4: the shallowest C layer so recorded, as for the chernic horizon
# (.chernic_parent_oc()), in the material of the surface layer: a C layer
# under a lithic discontinuity (a number before its designation that the
# surface layer does not carry, as 5Cr1 under A in ISRIC's PK006) is another
# material. NULL when there is none.
.wrb_mollic_parent_layer <- function(h) {
  d <- as.character(h$designation %||% rep(NA_character_, nrow(h)))
  disc <- suppressWarnings(as.integer(sub("^([0-9]*).*$", "\\1", d)))
  disc[is.na(disc)] <- 1L
  first <- order(h$top_cm)[1L]
  vm <- suppressWarnings(as.numeric(h$munsell_value_moist))
  c_layer <- which(!is.na(d) & grepl("^[0-9]*C", d) & !is.na(vm) & vm <= 4 &
                     disc == disc[first])
  if (!length(c_layer)) return(NULL)
  i <- c_layer[order(h$top_cm[c_layer])][1L]
  list(layer = i, oc_pct = h$oc_pct[i])
}
