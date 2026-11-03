# =============================================================================
# v0.9.337 -- the Russian soil classification (Field Guide of 2008, the field
# version of the classification of 2004), keyed to its trunks and divisions.
# The keys to trunks (2.2) and divisions (2.3) are branching trees; soilKey
# flattens them into first-match lists (Hugo, 2026-11-02), each gate the
# conjunction of the conditions of the book's path, and reads them on the
# diagnostic horizons of identify_horizons(pedon, "kidpr2008"). The types and
# subtypes follow. Experimental and opt-in: classify_all() runs it only when
# asked.
# =============================================================================

#' Classify a pedon under the Russian soil classification (Field Guide, 2008)
#'
#' Keys a \code{\link{PedonRecord}} to the trunks and divisions of the Russian
#' soil classification, in the version of its Field Guide (Polevoi
#' opredelitel' pochv Rossii, Dokuchaev Soil Science Institute, 2008).
#' \strong{Experimental}: this release keys the five trunks (key 2.2: the
#' organogenic, synlithogenic, postlithogenic and weakly developed soils, and
#' trunk E, not a soil: rock outcrops, technogenic surface formations, or a
#' deposit that is not humified over a buried soil) and the 27 divisions
#' (key 2.3), and the types of every division (the profile formulas of
#' chapter 4: CA to CJ in v0.9.338, CK to CT in v0.9.339, trunks A, B and D
#' in v0.9.342). The chemozems CU have no types: their name is built from
#' the pollutants and the soil they were (v0.9.342). The subtypes of every
#' division's list follow the type (v0.9.345).
#'
#' The keys read the diagnostic horizons of the profile: the Russian
#' notation where the profile is written in it (declared with
#' \code{site$designation_system = "kidpr"}, or recognised by an index only
#' it writes: AY, EL, BEL, BHF, BCA, PU, TUR ...), otherwise the key to
#' horizons 2.1 run on the recorded data (see \code{\link{identify_horizons}}).
#' From them the keys read whether a horizon is present, whether it starts
#' the profile (at the surface, or under a deposit of 40 cm or less),
#' whether it lies directly over another, whether the profile consists of
#' one horizon only, the depth of hard rock, a deposit at the surface (TPO
#' and the deposits the key to horizons finds on the data included), the
#' peat body of trunk A (more than 50 cm, the agro-peat horizons included)
#' and the thickness of a turbated horizon; of the site, a river floodplain
#' on modern alluvium (\code{landform}, \code{landform_element},
#' \code{layer_origin}, \code{rock_origin}, \code{parent_material}, the
#' alluvial mark of the notation; a terrace is no floodplain) and the
#' contaminated horizon X (\code{contamination_hazard}). The ochre horizon
#' BAN is formed in volcanic ash, so it makes the volcanic soils alone. With
#' several organic and humus horizons at the surface the thickest is the
#' diagnostic one (p. 37), for step 5 of the key to trunks as for the
#' divisions; FAO and WRB transitions (AB, BC, E/B) in which the key on the
#' data finds no diagnostic horizon are transitions, as those of the Russian
#' notation; a missing depth leaves the step that needs it undecided.
#'
#' The keys are branching trees; soilKey flattens them into ordered lists
#' in which each gate is the conjunction of the conditions of the book's path
#' to the class, and repairs only the gaps the book's own types prove; the
#' other combinations the key leaves without an exit are "not provided by
#' the Field Guide of 2008": the trunk is assigned and the division left
#' unassigned, with a note. Names: an English gloss in \code{name}, the
#' Russian of the book in \code{levels$name_native}. The readings where the
#' text is silent are listed in \code{system.file("rules", "kidpr2008",
#' "readings.yaml", package = "soilKey")}.
#'
#' The type is named by the formula of its profile: the profile is matched
#' against every formula of its division (see \code{\link{kidpr2008_types}}),
#' closed on the horizons the division's formulas write, most specific first
#' and agro before natural. A profile no formula matches keeps its division,
#' the type unassigned, with the nearest formulas in the warnings and in the
#' trace (phase \code{types_nearest}).
#'
#' A chemozem (CU, an X horizon: pollutants of the extremely hazardous
#' category within 30 cm) is named, as p. 134 asks, by the kind of
#' pollution and the original soil: the \code{pollutants} of the layers of
#' the X horizon, and the soil the profile keys to with the X masked (the X
#' overlays and the contamination records removed), down to its type. The
#' type level is soilKey's \code{"[CU:<code of the original soil>]"}, named
#' "Chemozems polluted with Cu, Ni, Co; original soil: Sod-podzolic"; the
#' original soil keyed is in the trace (phase \code{chemozem}), and an
#' original soil left undecided makes the name provisional.
#'
#' The subtypes (v0.9.345): each division has one list of subtypes in
#' chapter 4 (26 lists, 558 entries; the chemozems have none), each the
#' index of a genetic feature of section 3.2, read by the feature tests (the
#' recorded data first, the Russian notation where they say nothing). The
#' soil is named by its type and every subtype of the list that passes, in
#' the order of the list, comma-separated and in the plural as printed
#' ("Podzolistye gleevatye, yazykovatye"): the subtypes are the rows of
#' level \code{"subtype"} of \code{levels} (code: soilKey's key of the
#' entry; \code{name} the English gloss, \code{name_native} the Russian),
#' the full name in English in \code{name} and in Russian in
#' \code{qualifiers$name_native}. When every entry is FALSE the soil is of
#' the typical subtype, soilKey's "[tipichnye]" ("[typical]", code NA);
#' when some are undecided the name is provisional, the undecided entries
#' in \code{provisional_because} ("subtype:<key>") and in a warning. A
#' feature the formula of the type already writes (the g of the gley types,
#' the BF of CB05, the permafrost sign of the cryozems ...) is not named
#' again, and a sentence of 3.2 that names a single type restricts its
#' subtype to it. Each entry, with its evidence, is in the trace (phase
#' \code{subtype}). A soil whose type is unassigned gets no subtypes. The
#' lists are in \code{system.file("rules", "kidpr2008", "subtypes.yaml",
#' package = "soilKey")}.
#'
#' The species (v0.9.347): the twelve tables of chapter 5.1, the classes of
#' the thickness of the humus, peat and agro horizons, of the depth and place
#' of gleying, of the lower boundary of the bleached horizon, of the layer
#' over the solonetzic horizon, of the depth of the carbonates and of the
#' soluble salts and, in the organogenic trunk, of the decomposition, the
#' botanical composition (\code{peat_botanical_composition}) and the
#' thickness of the peat. Each table that applies to the type is read, and its
#' class follows the subtypes in the name, in the order of the tables
#' ("Dernovo-podzolistye [tipichnye], srednemoshchnye,
#' glubokoelyuvial'nye"; the bleached species in the prefix form of p. 136):
#' rows of level \code{"species"} of \code{levels} (code
#' \code{"<table>:<class>"}), \code{qualifiers$species} and
#' \code{species_native}. A depth not recorded, an undecided layer at the edge
#' of a horizon, or a horizon that reaches the bottom of the description when
#' a deeper class is possible, leaves the table undecided: it is not named,
#' it is listed in \code{qualifiers$species_undecided} and in a note, and the
#' name stays as it is. The tables are in \code{system.file("rules",
#' "kidpr2008", "species.yaml", package = "soilKey")}; the varieties and
#' ranks (5.2, 5.3) are not keyed.
#'
#' The 1977 analogue (v0.9.347): when the type is assigned, the rows of the
#' correlation table of chapter 6 that stand for it are added as a note (a
#' line in \code{warnings}) and as \code{qualifiers$analogue_1977} (the
#' 2004 name and formula, the page, whether the soils were distinguished in
#' 1977 and the units of 1977 with their level and extent), never changing
#' the classification; where the table says the soils were not distinguished
#' or has no row for the type, the note says so. The table is in
#' \code{system.file("rules", "kidpr2008", "correlation_1977.yaml", package =
#' "soilKey")}.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param rules Optional pre-loaded rule set (\code{load_rules("kidpr2008")}).
#' @param on_missing One of \code{"warn"} (default), \code{"silent"},
#'        \code{"error"}.
#' @param gapfill Opt-in within-pedon gap-fill, as in
#'        \code{\link{classify_usda}}.
#' @param subtypes Name the subtypes after the type (default \code{TRUE});
#'        \code{FALSE} stops at the type.
#' @param species Name the species of chapter 5.1 after the subtypes
#'        (default: as \code{subtypes}).
#' @return A \code{\link{ClassificationResult}} with \code{system_id}
#'   \code{"kidpr2008"}, the deepest class assigned in \code{name} (the
#'   type with its subtypes, else the division, else the trunk), the trunk in
#'   \code{rsg_or_order}, the trunk, division, type, subtypes and species in \code{levels} (with
#'   \code{name_native}, the Russian name), the key trace, the notes on a
#'   profile that is not a soil or whose combination of horizons the Field
#'   Guide does not provide for in \code{warnings}, and
#'   \code{provisional_because} (classes before the assigned one that could
#'   not be decided).
#' @references Polevoi opredelitel' pochv Rossii [Field Guide of Russian
#'   Soils] (2008). Dokuchaev Soil Science Institute, Moscow.
#' @seealso \code{\link{make_kidpr2008_example}}, \code{\link{identify_horizons}},
#'   \code{\link{soilkey_systems}}.
#' @examples
#' res <- classify_kidpr2008(make_kidpr2008_example("CA"), on_missing = "silent")
#' res$name
#' res$levels
#' @export
classify_kidpr2008 <- function(pedon, rules = NULL,
                               on_missing = c("warn", "silent", "error"),
                               gapfill = FALSE, subtypes = TRUE, species = subtypes) {
  on_missing <- match.arg(on_missing)
  if (!inherits(pedon, "PedonRecord"))
    rlang::abort("classify_kidpr2008(): 'pedon' must be a PedonRecord (see PedonRecord$new()).")
  # a horizon table without its depth columns is completed first
  if (!is.null(pedon$horizons) && !all(c("top_cm", "bottom_cm") %in% names(pedon$horizons))) {
    pedon <- pedon$clone(deep = TRUE)
    pedon$horizons <- ensure_horizon_schema(data.table::as.data.table(pedon$horizons))
  }
  pedon <- .classify_apply_gapfill(pedon, gapfill)
  .classify_require_horizons(pedon, "classify_kidpr2008")
  rules <- rules %||% load_rules("kidpr2008")
  # the profile read once: every predicate of the keys reads this view
  view <- .kidpr_dv_pedon(pedon)
  # v0.9.338: the type lists in the order of trial, kept on the view
  rules <- .kidpr_types_attach(rules, view)
  prev <- list(pedon = .kidpr_dv_state$pedon, view = .kidpr_dv_state$view)
  .kidpr_dv_state$pedon <- pedon
  .kidpr_dv_state$view <- view
  on.exit({
    .kidpr_dv_state$pedon <- prev$pedon
    .kidpr_dv_state$view <- prev$view
  }, add = TRUE)
  res <- classify_hierarchical("kidpr2008", pedon, rules = rules,
                               on_missing = on_missing, gapfill = FALSE)
  res <- .kidpr_types_notes(.kidpr_result_notes(res, view, pedon), view)
  # v0.9.342: a chemozem is named by its pollutants and the soil it was
  res <- .kidpr_chemozem_name(res, view, pedon, rules)
  # v0.9.345: the subtypes of the division's list, and the full name;
  # v0.9.347: the species of chapter 5.1 after them, and the 1977 analogue
  # of chapter 6 as a note (one feature context for both levels)
  ctx <- if (isTRUE(subtypes) || isTRUE(species)) .kidpr_level_context(res, pedon)
  if (isTRUE(subtypes)) res <- .kidpr_add_subtypes(res, pedon, rules, ctx = ctx)
  if (isTRUE(species)) res <- .kidpr_add_species(res, pedon, rules, ctx = ctx)
  res <- .kidpr_add_1977(res)
  res
}

# The feature context the subtypes and the species read, built once for a
# result whose type is assigned (NULL otherwise: the levels then add
# nothing).
.kidpr_level_context <- function(res, pedon) {
  lv <- res$levels
  if (is.null(lv) || nrow(lv) < 3L || isTRUE(.kidpr_dv_state$masking)) return(NULL)
  at <- which(lv$level == "types")
  if (!length(at) || is.na(lv$code[at[1L]])) return(NULL)
  div <- gsub("[][]", "", lv$code[lv$level == "divisions"][1L])
  if (identical(div, "CU")) return(NULL)
  .kidpr_ft_context(pedon, div)
}

# The name of a chemozem (CU, which has no types): the types are told "by
# the kind of pollution, keeping the name of the original soil" (p. 134), so
# the name is built from the pollutants of the X horizon (`pollutants` of the
# layers the X takes) and the soil the profile keys to with the X masked:
# the X overlays taken out of the designations and the contamination records
# (contamination_hazard of the extremely hazardous layers,
# contamination_type, site$contaminated) removed, the profile keyed again
# down to its type. The type level of the result is soilKey's
# "[CU:<code of the original soil>]"; in English "Chemozems polluted with
# Cu, Ni, Co; original soil: Sod-podzolic", in Russian "Khemozemy,
# zagryaznennye Cu, Ni, Co, po iskhodnoi pochve: Dernovo-podzolistye" (the
# original soil named in the nominative, after the book's "iskhodnaya
# pochva"; soilKey's form, accepted by Hugo, 2026-11-02). Pollutants not
# recorded are named so, and asked in $missing_data; an original soil that
# is undecided makes the name provisional ("original_soil:" entries in
# provisional_because), and the original soil keyed is in the trace (phase
# "chemozem").
# The designations of a profile with the X horizon taken out, from the
# overlays the notation reader parses (AY+X, AY+Xpb, Xpb+AY): the parts of a
# designation joined by "+" whose index is X are left out; a layer that is
# only X (X, Xpb) is dropped (`keep` FALSE).
.kidpr_cu_unmask <- function(d, site) {
  keep <- rep(TRUE, length(d))
  if (!length(d)) return(list(designation = d, keep = keep))
  p <- .kidpr_designations(d, site)
  isx <- function(q, i) identical(q$index[i], "X") ||
    (is.na(q$index[i]) && grepl("^\\s*[X\u0425]([a-z,]|$)", q$written[i] %||% ""))
  for (i in seq_along(d)) {
    if (is.na(d[i])) next
    if (!("X" %in% p$overlay[[i]]) && !isx(p, i)) next
    parts <- trimws(.kidpr_split_outside(d[i], "+"))
    if (length(parts) > 1L) {
      q <- .kidpr_designations(parts, site)
      parts <- parts[!vapply(seq_along(parts), function(j) isx(q, j) ||
                                grepl("^[X\u0425]([a-z,]|$)", parts[j]), NA)]
    } else parts <- character(0)
    if (length(parts)) d[i] <- paste(parts, collapse = "+") else keep[i] <- FALSE
  }
  list(designation = d, keep = keep)
}

.kidpr_chemozem_name <- function(res, view, pedon, rules) {
  lv <- res$levels
  if (is.null(lv) || nrow(lv) < 2L || !identical(lv$code[2L], "CU") || isTRUE(.kidpr_dv_state$masking))
    return(res)
  h <- pedon$horizons
  x <- contaminated_x_kidpr(view)
  hidx <- unique(stats::na.omit(view$f$horizon_idx[x$layers]))
  pol <- if (!is.null(h$pollutants) && length(hidx)) as.character(h$pollutants[hidx]) else character(0)
  pol <- trimws(unlist(strsplit(stats::na.omit(pol), "[;,]|\\s+(?:and|&|\u0438)\\s+", perl = TRUE)))
  pol <- unique(pol[nzchar(pol)])
  # the profile with the X masked
  masked <- pedon$clone(deep = TRUE)
  mh <- data.table::copy(data.table::as.data.table(masked$horizons))
  xrows <- integer(0)
  if (!is.null(mh$designation)) {
    xm <- .kidpr_cu_unmask(as.character(mh$designation), masked$site)
    mh$designation <- xm$designation
    xrows <- which(!xm$keep)
  }
  if (!is.null(mh$contamination_hazard)) {
    hz <- tolower(as.character(mh$contamination_hazard))
    mh$contamination_hazard[!is.na(hz) & grepl("^extrem", hz)] <- NA
  }
  if (!is.null(mh$contamination_type)) mh$contamination_type <- NA
  # a layer that is only the X horizon leaves the profile
  if (length(xrows) && length(xrows) < nrow(mh)) mh <- mh[setdiff(seq_len(nrow(mh)), xrows), ]
  masked$horizons <- mh
  s <- masked$site
  s$contaminated <- NULL
  masked$site <- s
  prev <- .kidpr_dv_state$masking
  .kidpr_dv_state$masking <- TRUE
  on.exit(.kidpr_dv_state$masking <- prev, add = TRUE)
  m <- classify_kidpr2008(masked, rules = rules, on_missing = "silent")
  ml <- m$levels
  k <- nrow(ml)
  if (!k) return(res)
  # the masked profile keyed to the chemozems again (an X the mask could not
  # take out): the original soil is undecided, not named
  again <- k >= 2L && identical(ml$code[2L], "CU")
  if (again) k <- 1L
  # the level the original soil stops at, undecided below it, or not
  # provided by the Field Guide
  pb <- m$provisional_because %||% character(0)
  und <- if (again) "soil"
         else if (k == 2L && any(startsWith(pb, "types:"))) "type"
         else if (k == 1L && any(startsWith(pb, "divisions:"))) "division"
         else if (k == 1L) "noprov" else ""
  orig_en <- if (again) "undecided"
             else paste0(ml$name[k], switch(und, type = ", type undecided", division = ", division undecided",
                                            noprov = ", division not provided by the Field Guide of 2008", ""))
  orig_ru <- if (again) "\u043d\u0435 \u0443\u0441\u0442\u0430\u043d\u043e\u0432\u043b\u0435\u043d\u0430"
             else paste0(ml$name_native[k] %||% ml$name[k],
                         switch(und, type = " (\u0442\u0438\u043f \u043d\u0435 \u0443\u0441\u0442\u0430\u043d\u043e\u0432\u043b\u0435\u043d)",
                                division = " (\u043e\u0442\u0434\u0435\u043b \u043d\u0435 \u0443\u0441\u0442\u0430\u043d\u043e\u0432\u043b\u0435\u043d)",
                                noprov = paste0(" (\u043e\u0442\u0434\u0435\u043b \u043d\u0435 \u043f\u0440\u0435\u0434\u0443\u0441\u043c\u043e\u0442\u0440\u0435\u043d ",
                                                "\u041f\u043e\u043b\u0435\u0432\u044b\u043c \u043e\u043f\u0440\u0435\u0434\u0435\u043b\u0438\u0442\u0435\u043b\u0435\u043c 2008 \u0433.)"),
                                ""))
  en <- if (length(pol)) sprintf("Chemozems polluted with %s; original soil: %s", paste(pol, collapse = ", "), orig_en)
        else sprintf("Chemozems, pollutants not recorded; original soil: %s", orig_en)
  ru_head <- "\u0425\u0435\u043c\u043e\u0437\u0435\u043c\u044b"
  ru_by <- "\u043f\u043e \u0438\u0441\u0445\u043e\u0434\u043d\u043e\u0439 \u043f\u043e\u0447\u0432\u0435"
  ru <- if (length(pol)) sprintf("%s, \u0437\u0430\u0433\u0440\u044f\u0437\u043d\u0435\u043d\u043d\u044b\u0435 %s, %s: %s",
                                 ru_head, paste(pol, collapse = ", "), ru_by, orig_ru)
        else sprintf("%s, \u0437\u0430\u0433\u0440\u044f\u0437\u043d\u0438\u0442\u0435\u043b\u0438 \u043d\u0435 \u0443\u043a\u0430\u0437\u0430\u043d\u044b, %s: %s",
                     ru_head, ru_by, orig_ru)
  code <- if (again) "[CU:?]" else sprintf("[CU:%s]", gsub("[][]", "", ml$code[k]))
  row <- lv[1L, , drop = FALSE]
  row$level <- "types"
  row$code <- code
  row$name <- en
  if (!is.null(row$name_native)) row$name_native <- ru
  if (!is.null(row$name_en)) row$name_en <- en
  res$levels <- rbind(lv[1:2, , drop = FALSE], row)
  rownames(res$levels) <- NULL
  res$name <- en
  if (length(m$provisional_because) || again)
    res$provisional_because <- unique(c(res$provisional_because,
                                        if (length(m$provisional_because)) paste0("original_soil:", m$provisional_because),
                                        if (again) "original_soil:divisions:CU"))
  res$missing_data <- unique(c(res$missing_data, m$missing_data, if (!length(pol)) "pollutants"))
  path <- paste(ml$code, collapse = "/")
  notes <- if (again) paste(
    "Chemozem (C.3\u0431; Hugo's decision Q23): its type is named by the pollutants and the original soil (p. 134);",
    "the profile keyed with the X horizon masked is a chemozem again, so the original soil is undecided.")
  else sprintf(paste(
    "Chemozem (C.3\u0431; Hugo's decision Q23): its type is named by the pollutants and the original soil (p. 134),",
    "the profile keyed with the X horizon masked: %s, %s%s."), path, ml$name[k],
    switch(und, type = ", its type undecided (see $missing_data)", division = ", its division undecided (see $missing_data)",
           noprov = ", its division not provided by the Field Guide of 2008",
           if (length(m$provisional_because)) ", provisional" else ""))
  if (!length(pol)) notes <- c(notes, "The pollutants of the X horizon are not recorded (`pollutants`).")
  carried <- grep("^The type is undecided|^Combination not provided|^Intergrade|not provided by the Field Guide",
                  m$warnings, value = TRUE)
  if (length(carried)) notes <- c(notes, paste("Original soil:", carried))
  res$warnings <- c(res$warnings, notes)
  res$trace[["chemozem"]] <- list(code = code, name = en, name_native = ru, passed = TRUE,
                                  missing = if (!length(pol)) "pollutants" else character(0),
                                  pollutants = pol, layers = hidx,
                                  original = list(levels = ml, name = m$name, provisional_because = m$provisional_because,
                                                  missing = m$missing_data, warnings = m$warnings))
  res
}

# The notes of a result: trunk E is not a soil; a profile of trunk C that no
# division takes is a thin layer of loose rock over hard rock (C.2b) or a
# combination the Field Guide does not provide for (C.21-C.23); horizons read
# from the data.
.kidpr_result_notes <- function(res, view, pedon) {
  lv <- res$levels
  trunk <- if (nrow(lv)) lv$code[1L] else NA_character_
  notes <- character(0)
  if (identical(trunk, "E")) {
    notes <- c(notes, paste(
      "Trunk E: not a soil (key 2.2): no soil horizon within about 1 m (a rock outcrop or a technogenic",
      "surface formation, TPO), or a deposit over 40 cm that is not humified over a buried soil. The",
      "groups of technogenic surface formations (chapter 8) are not keyed."))
  }
  undecided <- any(startsWith(res$provisional_because %||% character(0), "divisions:"))
  if (nrow(lv) == 1L && !identical(trunk, "E") && !undecided) {
    rock <- tryCatch(rock_within_kidpr(view, max_cm = 30)$passed, error = function(e) NA)
    notes <- c(notes, if (identical(trunk, "C") && isTRUE(rock)) paste(
      "Hard rock within 30 cm and none of the horizons of key step C.2: a thin layer of loose rock",
      "over hard rock (C.2b), not a division of the Field Guide of 2008; the division is unassigned.")
    else paste(
      "This combination of horizons is not provided by the Field Guide of 2008 (key 2.3, steps",
      "C.21-C.23): the division is unassigned."))
  }
  rt <- view$f$route
  if (any(rt %in% "data")) {
    notes <- c(notes, paste(
      "Some layers are not written in the Russian notation: their horizons come from key 2.1 run on",
      "the recorded data (see identify_horizons(pedon, \"kidpr2008\"))."))
  }
  if (length(notes)) res$warnings <- c(res$warnings, notes)
  res
}


#' Example pedons for the divisions of the Russian soil classification
#'
#' Small, plausible profiles written for the tests and examples, one per
#' division of the Russian soil classification (Field Guide of 2008), with
#' designations in the Russian notation (declared in
#' \code{site$designation_system}) and a few properties. They are not real
#' profiles.
#'
#' @param division The code of a division: \code{"AA"}, \code{"AB"}
#'   (trunk A), \code{"BA"}, \code{"BB"}, \code{"BC"} (trunk B),
#'   \code{"DA"} (trunk D, the division soilKey codes [DA]), \code{"CA"} to
#'   \code{"CU"} (trunk C).
#' @return A \code{\link{PedonRecord}}.
#' @examples
#' classify_kidpr2008(make_kidpr2008_example("CI"), on_missing = "silent")$name
#' @export
make_kidpr2008_example <- function(division = c("AA", "AB", "BA", "BB", "BC", "DA",
                                                "CA", "CB", "CC", "CD", "CE", "CF", "CG",
                                                "CH", "CI", "CJ", "CK", "CL", "CM", "CN",
                                                "CO", "CP", "CQ", "CR", "CS", "CT", "CU")) {
  division <- match.arg(division)
  mk <- function(h, site = list()) {
    n <- max(lengths(h))
    h <- lapply(h, function(v) if (length(v) == 1L) rep(v, n) else v)
    base <- list(id = paste0("kidpr2008-", division), country = "RU", designation_system = "kidpr")
    PedonRecord$new(site = utils::modifyList(base, site), horizons = ensure_horizon_schema(
      data.table::as.data.table(data.frame(h, stringsAsFactors = FALSE))))
  }
  switch(division,
    AA = mk(list(designation = c("TO", "TT1", "TT2"), top_cm = c(0, 40, 100), bottom_cm = c(40, 100, 180),
                 oc_pct = c(45, 48, 50), ph_h2o = 3.8, peat_moor_type = "high-moor"),
            list(landform = "raised bog", drainage_class = "very poorly drained")),
    AB = mk(list(designation = c("PT", "TT", "G"), top_cm = c(0, 25, 110), bottom_cm = c(25, 110, 140),
                 oc_pct = c(40, 46, 1.5), ph_h2o = c(5.6, 5.4, 6.2)),
            list(land_use = "arable", artificially_drained = TRUE)),
    BA = mk(list(designation = c("AU", "C~~1", "C~~2"), top_cm = c(0, 30, 70), bottom_cm = c(30, 70, 120),
                 oc_pct = c(3.5, 0.8, 0.4), clay_pct = c(28, 18, 22), silt_pct = 50, sand_pct = c(22, 32, 28),
                 ph_h2o = 6.8, layer_origin = "alluvium"),
            list(landform = "floodplain")),
    BB = mk(list(designation = c("AO", "BH", "BAN", "C\""), top_cm = c(0, 6, 15, 60), bottom_cm = c(6, 15, 60, 90),
                 oc_pct = c(12, 6, 3, 0.8), ph_h2o = 5.4, layer_origin = "volcanic ash"),
            list(parent_material = "volcanic ash")),
    BC = mk(list(designation = c("RU1", "RU2", "D"), top_cm = c(0, 30, 70), bottom_cm = c(30, 70, 110),
                 oc_pct = c(3, 2.2, 0.5), ph_h2o = 7.0, stratification_pattern = c("continuous", "continuous", NA))),
    DA = mk(list(designation = c("W", "R"), top_cm = c(0, 3), bottom_cm = c(3, 30),
                 oc_pct = c(1.2, NA), rock_origin = c(NA, "granite"))),
    CA = mk(list(designation = c("AY", "EL", "BEL", "BT1", "BT2", "C"), top_cm = c(0, 15, 30, 45, 80, 120),
                 bottom_cm = c(15, 30, 45, 80, 120, 150), oc_pct = c(2.5, 0.6, 0.4, 0.3, 0.2, 0.1),
                 clay_pct = c(14, 12, 18, 30, 32, 26), silt_pct = 55, sand_pct = c(31, 33, 27, 15, 13, 19),
                 ph_h2o = c(5.2, 5.0, 5.1, 5.3, 5.6, 6.0))),
    CB = mk(list(designation = c("O", "E", "BHF", "C"), top_cm = c(0, 5, 15, 40), bottom_cm = c(5, 15, 40, 100),
                 oc_pct = c(40, 0.4, 1.2, 0.1), clay_pct = c(NA, 3, 5, 3), sand_pct = c(NA, 90, 85, 92),
                 silt_pct = c(NA, 7, 10, 5), ph_h2o = c(3.9, 4.2, 4.6, 5.0))),
    CC = mk(list(designation = c("AO", "BFM", "C"), top_cm = c(0, 8, 50), bottom_cm = c(8, 50, 100),
                 oc_pct = c(10, 0.8, 0.2), ph_h2o = c(4.5, 5.0, 5.2))),
    CD = mk(list(designation = c("AY", "BM", "C"), top_cm = c(0, 15, 60), bottom_cm = c(15, 60, 100),
                 oc_pct = c(3, 0.9, 0.3), clay_pct = c(20, 24, 20), silt_pct = 40, sand_pct = c(40, 36, 40),
                 ph_h2o = c(5.4, 5.6, 5.8))),
    CE = mk(list(designation = c("O", "CRM", "C"), top_cm = c(0, 8, 40), bottom_cm = c(8, 40, 80),
                 oc_pct = c(35, 1, 0.4), ph_h2o = c(4.6, 5.4, 5.8)),
            list(frost_regime = "permafrost")),
    CF = mk(list(designation = c("AJ", "BPL", "BCA", "Cca"), top_cm = c(0, 20, 60, 100), bottom_cm = c(20, 60, 100, 140),
                 oc_pct = c(2.2, 0.8, 0.4, 0.2), caco3_pct = c(0, 0, 12, 10), ph_h2o = c(7.2, 7.6, 8.2, 8.3)),
            list(frost_regime = "long-seasonal")),
    CG = mk(list(designation = c("O", "CR", "C\u22a5"), top_cm = c(0, 5, 40), bottom_cm = c(5, 40, 60),
                 oc_pct = c(30, 2, 0.8), ph_h2o = c(5.0, 5.8, 6.0)),
            list(frost_regime = "permafrost")),
    CH = mk(list(designation = c("O", "G", "CG"), top_cm = c(0, 8, 50), bottom_cm = c(8, 50, 100),
                 oc_pct = c(30, 1.2, 0.5), ph_h2o = c(4.8, 5.6, 6.0)),
            list(drainage_class = "poorly drained")),
    CI = mk(list(designation = c("AU1", "AU2", "BCA", "Cca"), top_cm = c(0, 25, 50, 100), bottom_cm = c(25, 50, 100, 150),
                 oc_pct = c(4.5, 3, 1, 0.4), caco3_pct = c(0, 0, 10, 12), clay_pct = 35, silt_pct = 45, sand_pct = 20,
                 ph_h2o = c(6.8, 7.0, 8.0, 8.2))),
    CJ = mk(list(designation = c("AJ", "BM", "BCA", "Cca"), top_cm = c(0, 10, 30, 60), bottom_cm = c(10, 30, 60, 100),
                 oc_pct = c(1, 0.6, 0.3, 0.2), caco3_pct = c(0, 1, 15, 12), ph_h2o = c(7.8, 8.0, 8.4, 8.5))),
    CK = mk(list(designation = c("AU", "SEL", "ASN", "BCAs,cs", "Cca,s"), top_cm = c(0, 12, 20, 45, 80),
                 bottom_cm = c(12, 20, 45, 80, 120), oc_pct = c(3.5, 1.5, 1.8, 0.6, 0.3),
                 ph_h2o = c(7.0, 7.2, 8.6, 8.8, 8.6), clay_pct = c(25, 22, 38, 34, 30), silt_pct = 40,
                 sand_pct = c(35, 38, 22, 26, 30))),
    CL = mk(list(designation = c("S", "Gs", "CGs"), top_cm = c(0, 10, 40), bottom_cm = c(10, 40, 80),
                 oc_pct = c(1.2, 0.5, 0.3), ec_dS_m = c(40, 18, 12), ph_h2o = c(8.4, 8.2, 8.0)),
            list(drainage_class = "poorly drained", surface_condition = "salt crust")),
    CM = mk(list(designation = c("AU", "Q", "CQ"), top_cm = c(0, 30, 70), bottom_cm = c(30, 70, 100),
                 oc_pct = c(4, 1, 0.4), ph_h2o = c(6.6, 7.0, 7.4))),
    CN = mk(list(designation = c("AY", "C"), top_cm = c(0, 20), bottom_cm = c(20, 80),
                 oc_pct = c(2.8, 0.3), ph_h2o = c(5.8, 6.2))),
    CO = mk(list(designation = c("AY", "EL", "D"), top_cm = c(0, 10, 35), bottom_cm = c(10, 35, 80),
                 oc_pct = c(2.5, 0.4, 0.2), clay_pct = c(8, 5, 30), sand_pct = c(75, 82, 30), silt_pct = c(17, 13, 40),
                 ph_h2o = c(5.0, 5.2, 6.0))),
    CP = mk(list(designation = c("AU", "C", "R"), top_cm = c(0, 15, 25), bottom_cm = c(15, 25, 40),
                 oc_pct = c(5, 1, NA), ph_h2o = c(6.5, 6.8, NA), rock_origin = c(NA, NA, "granite"))),
    CQ = mk(list(designation = c("BM", "C"), top_cm = c(0, 30), bottom_cm = c(30, 80),
                 oc_pct = c(0.6, 0.2), ph_h2o = c(6.0, 6.4)),
            list(anthropic_truncation = TRUE)),
    CR = mk(list(designation = c("PB", "BCA", "Cca"), top_cm = c(0, 25, 60), bottom_cm = c(25, 60, 100),
                 oc_pct = c(0.8, 0.4, 0.2), caco3_pct = c(2, 12, 10), ph_h2o = c(7.8, 8.2, 8.3)),
            list(land_use = "arable")),
    CS = mk(list(designation = c("P", "BT", "C"), top_cm = c(0, 25, 80), bottom_cm = c(25, 80, 120),
                 oc_pct = c(1.6, 0.4, 0.2), clay_pct = c(14, 30, 24), silt_pct = 50, sand_pct = c(36, 20, 26),
                 ph_h2o = c(5.8, 5.6, 6.0)),
            list(land_use = "arable")),
    CT = mk(list(designation = c("P", "TUR", "C"), top_cm = c(0, 25, 75), bottom_cm = c(25, 75, 120),
                 oc_pct = c(1.8, 0.8, 0.2), ph_h2o = c(6.6, 7.2, 7.8)),
            list(land_use = "arable")),
    CU = mk(list(designation = c("AY+X", "EL", "BT", "C"), top_cm = c(0, 15, 30, 90), bottom_cm = c(15, 30, 90, 120),
                 oc_pct = c(2.2, 0.5, 0.3, 0.2), ph_h2o = c(5.6, 5.4, 5.6, 6.0),
                 contamination_hazard = c("extremely hazardous", NA, NA, NA),
                 pollutants = c("Cu, Ni, Co", NA, NA, NA)),
            list(contaminated = TRUE))
  )
}
