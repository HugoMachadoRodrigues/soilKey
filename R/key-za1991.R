# =============================================================================
# v0.9.358 -- the South African soil classification of 1991, keyed to its 73
# soil forms. The forms are sequences of diagnostic horizons and materials
# (key.yaml), tried in the order of van Huyssteen (2020), which follows the
# book's; the first that holds is the form, an undecided one before it
# makes the result provisional (classify_hierarchical()). The families
# follow in the next part. Experimental and opt-in: classify_all() runs it
# only when asked.
# =============================================================================

#' Classify a pedon under the South African soil classification (1991)
#'
#' Keys a \code{\link{PedonRecord}} to the soil forms of the South African
#' soil classification of 1991 (Soil Classification Working Group, 1991;
#' the "Blue Book"). \strong{Experimental}: the book itself is not available
#' to soilKey; the key is read from its open pages (the AGIS key and form
#' pages, the general rules, the topsoil pages), SASRI (1999) and van
#' Huyssteen (2020). This release keys the 73 forms; the families follow.
#'
#' Each layer is read as one of the 30 diagnostic horizons and materials
#' (see \code{\link{identify_horizons}}): from the name written in
#' \code{diagnostic_za} (or in the designations of a profile declared with
#' \code{site$designation_system = "za"}), else by soilKey's tests on the
#' recorded data, three-valued, with WRB numbers standing in, flagged, where
#' no open source gives the book's (the podzol B, the placic pan, the
#' dorbank, the carbonate horizons and the colours of the subsoil). The
#' profile is first read by the general rules: only horizons whose top lies
#' above 1500 mm count; a recent deposit thinner than 500 mm over a buried
#' soil is set aside and the buried soil classified, a thicker one is
#' classified alone (\code{recent_deposit}, else its origin, flagged); a
#' profile with no topsoil whose first horizon is a red or yellow-brown
#' apedal B, a neocarbonate B, a dorbank or a hard plinthic B is read as
#' truncated, an orthic A supplied.
#'
#' A form holds when its horizons are the first of the profile's, in order
#' (layers of no diagnostic horizon skipped), and the horizon that follows
#' is not one that would make another form (Hutton over hard rock is
#' Hutton; Hutton over a soft plinthic B is Bainsvlei). A description that
#' stops above 1500 mm leaves the longer forms undecided, so the shorter
#' one is provisional. The 500 mm rules of the form pages are tested too
#' (Valsrivier, Sepane, Swartland, the podzol forms; Witbank, Dundee,
#' Namib over a buried soil). A sequence no form has gives no form, with
#' the nearest forms in the warnings; an orthic A under 100 mm thick over
#' nothing diagnostic is the land class "rock and lithosols" (general
#' rules). Lichtenburg (orthic A, red apedal B, hard plinthic B) is named
#' by van Huyssteen (2020) but not by the AGIS key of 1991: it is keyed,
#' always provisional. The readings soilKey makes where the open sources
#' are silent are in \code{system.file("rules", "za1991", "readings.yaml",
#' package = "soilKey")}.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param rules Optional pre-loaded rule set (\code{load_rules("za1991")}).
#' @param on_missing One of \code{"warn"} (default), \code{"silent"},
#'        \code{"error"}.
#' @param gapfill Opt-in within-pedon gap-fill, as in
#'        \code{\link{classify_usda}}.
#' @return A \code{\link{ClassificationResult}} with \code{system_id}
#'   \code{"za1991"}: the form in \code{name} and \code{rsg_or_order} (its
#'   code in \code{levels}, with \code{name_en}, soilKey's gloss: the
#'   sequence of its horizons), the sequence read in
#'   \code{qualifiers$za_sequence} and the identified layers in
#'   \code{qualifiers$za_horizons} (with \code{za_truncated},
#'   \code{za_land_class} and \code{za_nearest} when they apply),
#'   the general rules applied, the stand-ins used and the nearest forms in
#'   \code{warnings}, and \code{provisional_because}.
#' @references Soil Classification Working Group (1991). Soil
#'   classification: a taxonomic system for South Africa. Memoirs on the
#'   Agricultural Natural Resources of South Africa No. 15. Department of
#'   Agricultural Development, Pretoria.
#' @seealso \code{\link{make_za1991_example}}, \code{\link{identify_horizons}},
#'   \code{\link{soilkey_systems}}.
#' @examples
#' res <- classify_za1991(make_za1991_example("hutton"), on_missing = "silent")
#' res$name
#' res$qualifiers$za_sequence
#' @export
classify_za1991 <- function(pedon, rules = NULL, on_missing = c("warn", "silent", "error"),
                            gapfill = FALSE) {
  on_missing <- match.arg(on_missing)
  if (!inherits(pedon, "PedonRecord"))
    rlang::abort("classify_za1991(): 'pedon' must be a PedonRecord (see PedonRecord$new()).")
  if (!is.null(pedon$horizons) && !all(c("top_cm", "bottom_cm") %in% names(pedon$horizons))) {
    pedon <- pedon$clone(deep = TRUE)
    pedon$horizons <- ensure_horizon_schema(data.table::as.data.table(pedon$horizons))
  }
  pedon <- .classify_apply_gapfill(pedon, gapfill)
  .classify_require_horizons(pedon, "classify_za1991")
  rules <- rules %||% load_rules("za1991")
  view <- .za_view(pedon)
  prev <- list(view = .za_state$view, pedon = .za_state$pedon)
  .za_state$view <- view
  .za_state$pedon <- pedon
  on.exit({ .za_state$view <- prev$view; .za_state$pedon <- prev$pedon }, add = TRUE)
  res <- classify_hierarchical("za1991", pedon, rules = rules, on_missing = on_missing, gapfill = FALSE)
  .za_result_notes(res, view)
}

# The notes of a result: the sequence read, the general rules applied, the
# stand-ins, Lichtenburg, no form (Q2) and the land class of R6.
.za_result_notes <- function(res, view) {
  seq_txt <- .za_sequence_text(view)
  fr <- view$frame
  notes <- view$notes
  q <- res$qualifiers %||% list()
  q$za_sequence <- seq_txt
  q$za_truncated <- view$truncated
  lv <- res$levels
  form <- if (nrow(lv)) lv$code[1L] else NA_character_
  if (identical(form, "Lc")) {
    res$provisional_because <- unique(c(res$provisional_because, "forms:Lc"))
    notes <- c(notes, paste("Lichtenburg is named by van Huyssteen (2020) but is not in the AGIS key of 1991",
                            "(Hugo's decision Q1): provisional, with no family."))
  }
  if (is.na(form)) {
    und <- any(startsWith(res$provisional_because %||% character(0), "forms:"))
    el <- view$elements
    run <- .za_topsoil_layers(view$x)
    lithosol <- !und && length(el) == 1L && identical(el[[1L]]$codes, "OA") && length(run) &&
                isTRUE(sum(view$x$thick[run]) < .za_thv("lithosol_topsoil") / 10)
    if (lithosol) {
      res$name <- "Rock and lithosols (a land class, not a soil form)"
      q$za_land_class <- "rock and lithosols"
      notes <- c(notes, "An orthic A under 100 mm thick over nothing diagnostic: the land class rock and lithosols, not a form (general rules; Q2).")
    } else if (!und) {
      nr <- .za_nearest_forms(view)
      q$za_nearest <- nr
      notes <- c(notes, sprintf("No form of 1991 has the sequence %s (Q2); the nearest: %s.", seq_txt,
                                paste(sprintf("%s (%s, %s)", nr$name, nr$code, nr$sequence), collapse = ", ")))
    }
  }
  tg <- unique(c(view$tags, unlist(fr$tags)))
  si <- sub("^stand_in:", "", grep("^stand_in:", tg, value = TRUE))
  if (length(si)) notes <- c(notes, sprintf("WRB or soilKey stand-ins for numbers of the 1991 book: %s (thresholds.yaml).",
                                            paste(si, collapse = ", ")))
  if (any(fr$route %in% "data"))
    notes <- c(notes, "Some layers are not named in the South African notation: their horizons come from soilKey's tests on the data (see identify_horizons(pedon, \"za1991\")).")
  q$za_horizons <- list(open = view$open, truncated = view$truncated, implicit = view$implicit,
                        notes = view$notes, tags = view$tags,
                        frame = fr[, intersect(c("horizon_idx", "designation", "code", "candidates", "route",
                                                 "data_code", "tags", "evidence"), names(fr))])
  res$qualifiers <- q
  if (length(notes)) res$warnings <- c(res$warnings, notes)
  res
}
