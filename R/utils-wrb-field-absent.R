# =============================================================================
# soilKey -- field observations not recorded are absent, for the WRB 2022
# qualifiers (v0.9.413).
#
# Hugo's standing rule: a field observation that is not recorded counts as
# absent; a laboratory measurement that is not recorded is undecided. With
# Haplic given only when every principal qualifier of the reference soil group
# is decided (WRB 2022 Ch 5, p. 136; Hugo, L3), qualifiers left undecided by a
# field observation that nobody recorded kept Haplic out of every name.
#
# resolve_wrb_qualifiers() evaluates a qualifier that comes out undecided once
# more on a copy of the pedon in which the field observations of presence or
# absence that are not recorded read as absent, in the horizons that have a
# field description: mottles (Annex 1, 8.4.20) outside g, l and r horizons,
# coarse fragments (Annex 1, 8.4.7), Fe/Mn
# concretions and nodules (8.4.25), the desert pavement, varnish, ventifacts
# and vesicular pores of yermic properties (Ch 3.2.17, p. 84), animal pores and
# casts (8.4.18), cementation (8.4.24) and the reaction with 1 M HCl (8.4.21);
# and, at the site, a groundwater table (8.3.9). The diagnostics of the keys
# keep reading the pedon as it is. Left as they are: laboratory values;
# attributes that every horizon has, which a description that omits them
# does not make absent (colours, structure, texture, boundaries); and the
# observations for which the ISIS reader keeps an explicit undecided reading
# (mottles of a g, l or r horizon with no mottle field; slickensides and
# pressure faces where the cutans field holds another kind; cracks of a
# profile not described dry; plinthite, duripan and artefacts that a
# designation suggests; rows with laboratory data only). Pedons from other
# sources read slickensides, cracks, duripan, plinthite and artefacts not
# recorded as absent too.
# =============================================================================

# Column -> value read where it is not recorded.
.WRB_FIELD_ABSENT <- list(
  coarse_fragments_pct = 0, ferric_nodules_pct = 0,
  desert_pavement_pct = 0, varnish_pct = 0, ventifact_pct = 0,
  vesicular_pores = "absent", worm_holes_pct = 0,
  cementation_class = "none", effervescence_class = "none"
)

#' Rows with a field description: a designation, or any of the colour,
#' structure, boundary or mottle observations recorded. Rows with laboratory
#' data only have none.
#' @noRd
.wrb_described_rows <- function(h) {
  n <- nrow(h)
  rec <- function(col) { x <- h[[col]]; if (is.null(x)) rep(FALSE, n) else !is.na(x) & nzchar(trimws(as.character(x))) }
  Reduce(`|`, lapply(c("designation", "munsell_hue_moist", "munsell_value_moist",
                       "munsell_hue_dry", "structure_grade", "structure_type",
                       "boundary_distinctness", "redoximorphic_features_pct"), rec))
}

#' A copy of the pedon in which the field observations of .WRB_FIELD_ABSENT
#' that are not recorded in a described horizon (.wrb_described_rows()) are
#' absent, and so are mottles outside g, l and r horizons; the site records no
#' groundwater when it says nothing of it.
#' @noRd
.wrb_field_absent_pedon <- function(pedon) {
  p <- pedon$clone(deep = TRUE)
  h <- data.table::copy(p$horizons)
  n <- nrow(h)
  if (n) {
    described <- .wrb_described_rows(h)
    # mottles: absent where not recorded, except in a horizon whose
    # designation shows gleying or reduction (g, l, r: the ISIS reader keeps
    # those undecided, Hugo 2026-10-29)
    d <- .desg_master(as.character(h$designation %||% rep(NA_character_, n)), site = p$site)
    glr <- !is.na(d) & grepl("^[0-9]*[A-Z]+[a-z]*[glr]", d) & !grepl("^[0-9]*R", d)
    rx <- h$redoximorphic_features_pct %||% rep(NA_real_, n)
    rx[is.na(rx) & described & !glr] <- 0
    data.table::set(h, j = "redoximorphic_features_pct", value = as.numeric(rx))
    # a source whose reader has not applied the rule itself (the ISIS reader
    # has, with its own undecided readings): slickensides, cracks, duripan,
    # plinthite and artefacts not recorded are absent too, except where a
    # designation suggests them (qm, v, u)
    if (!isTRUE(pedon$site$absent_unless_recorded)) {
      sug <- function(rx) !is.na(d) & grepl(rx, d)
      more <- list(slickensides = list("absent", FALSE), cracks_width_cm = list(0, FALSE),
                   duripan_pct = list(0, sug("qm")),
                   plinthite_pct = list(0, sug("^[0-9]*[A-Z]+[a-z]*v")),
                   artefacts_pct = list(0, sug("^[0-9]*[A-Z]+[a-z]*u")))
      for (col in names(more)) {
        v <- more[[col]][[1]]
        x <- h[[col]]
        if (is.null(x)) x <- rep(if (is.character(v)) NA_character_ else NA_real_, n)
        x[is.na(x) & described & !more[[col]][[2]]] <- v
        data.table::set(h, j = col, value = x)
      }
    }
    for (col in names(.WRB_FIELD_ABSENT)) {
      v <- .WRB_FIELD_ABSENT[[col]]
      x <- h[[col]]
      if (is.null(x)) x <- rep(if (is.character(v)) NA_character_ else NA_real_, n)
      x[is.na(x) & described] <- v
      data.table::set(h, j = col, value = x)
    }
  }
  site <- p$site %||% list()
  if (is.na((site$groundwater_kind %||% NA)[1]) &&
      is.na(suppressWarnings(as.numeric(site$groundwater_depth_cm %||% NA))[1]))
    site$groundwater_kind <- "none observed"
  p$horizons <- h
  p$site <- site
  p
}

#' An undecided result made FALSE when `absent` is TRUE: the observation that
#' would decide it is a field observation not recorded (Hugo's rule). `why`
#' goes to the evidence.
#' @noRd
.wrb_field_decided <- function(res, absent, why) {
  if (!(length(res$passed) == 1L && is.na(res$passed)) || !isTRUE(absent)) return(res)
  DiagnosticResult$new(name = res$name, passed = FALSE, layers = integer(0),
                       evidence = list(undecided = res, field_absent = why),
                       missing = character(0), reference = res$reference)
}

#' Can a layer starting <= 100 cm from the mineral soil surface be cemented
#' (moderately or more)? Its cementation is recorded so, or its designation
#' has the suffix m (cementation, Annex 1, Ch 10, Table 10.2), or it is a row
#' with no designation and no cementation recorded (no field description).
#' @noRd
.wrb_cemented_possible <- function(pedon) {
  h <- pedon$horizons
  n <- nrow(h)
  if (!n) return(FALSE)
  ms <- .q_mineral_surface_cm(h)
  if (is.na(ms)) ms <- 0
  cem <- tolower(as.character(h$cementation_class %||% rep(NA_character_, n)))
  d <- as.character(h$designation %||% rep(NA_character_, n))
  up <- !is.na(h$top_cm) & h$top_cm - ms <= 100
  strong <- !is.na(cem) & grepl("moderat|strong|indurat", cem)
  m <- !is.na(d) & grepl("^[0-9]*[A-Z]+[a-z0-9]*m", d)
  open <- is.na(cem) & !.wrb_described_rows(h)
  any(up & (strong | m | open))
}
