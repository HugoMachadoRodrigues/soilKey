# =============================================================================
# v0.9.364 -- the overlays of the German KA5 (Tab. 46 no. 18, p. 268; plan
# 4.4): two soils one over the other, the lower one buried, named by the
# thickness of the upper soil: under 3 dm "flach X ueber Y" (X\Y), 3 to < 7
# dm "X ueber Y" (X/Y), 7 dm or more X alone. The overlay splitter cuts the
# profile at the discontinuity and keys each part:
#
#   * a mineral cover on a Moor (Hugo's decision Q7, pp. 258-259, 283): the
#     Moor keeps the type; under 2 dm the cover is named as substrate only,
#     2 to < 3 dm "flach X ueber HN" (X\HN), 3 to < 4 dm "X ueber HN" (X/HN),
#     the cover keyed as the mineral soil it would be without the Moor gate;
#   * the fersiallitic and ferrallitic palaeosols (Q19, pp. 226, 283): the
#     soil above gives the type, the palaeosol (a relict or fossil Bj or Bu)
#     is named "... ueber Fersiallit / Ferrallit" by the steps of Tab. 46 no.
#     18; only a Cj surviving is ignored;
#   * a buried soil (the fossil rows of a layer II or deeper, or the fossil
#     parts of rows overprinted with the degree sign): the upper soil keeps
#     the type, the buried soil is keyed alone from its own top as GOF; the
#     Organomarsch over a Podsol takes its own steps (1.5 to < 3 dm and 3 to
#     < 7 dm, p. 254).
#
# The overlay is a variety (Tab. 46 no. 18): it does not change the levels,
# the name or provisional_because; it is reported in trace$overlay and in the
# warnings.
# =============================================================================

# The grammatical gender of the type names (der, die, das), for the
# adjective "flach" of the names of the shallow overlays.
.KA5_TYPE_GENDER <- c(
  GG = "m", GN = "m", GM = "m", GH = "m", "\u00dcA" = "m", RN = "m", RQ = "m", TT = "m", TC = "m", DD = "m",
  PP = "m", SS = "m", SH = "m", SG = "m", XX = "m", YK = "m", YE = "m", YO = "m", YY = "m", YU = "m", OO = "m",
  OL = "m", FF = "m", FS = "m", JD = "m", JS = "m", IA = "m",
  AO = "f", AQ = "f", AZ = "f", AT = "f", AB = "f", MR = "f", MC = "f", MO = "f", MK = "f", MD = "f", MH = "f",
  MN = "f", RR = "f", RZ = "f", BB = "f", LL = "f", LF = "f", CF = "f", CR = "f", JG = "f",
  HN = "n", HH = "n", KV = "n", KM = "n", KH = "n", IW = "n", JP = "n")

# The names of the lower soils that are no type of their own in an overlay.
.KA5_OVERLAY_LOWER <- list(VV = c("Fersiallit", "fersiallite"), VW = c("Ferrallit", "ferrallite"))

# The name of an overlay X\Y or X/Y in German and English.
.ka5_overlay_name <- function(upper, lower, sym) {
  te <- .ka5_type_entries()
  un <- te[[upper]]$name %||% upper; ue <- te[[upper]]$name_en %||% upper
  lw <- .KA5_OVERLAY_LOWER[[lower]]
  ln <- if (!is.null(lw)) lw[1L] else te[[lower]]$name %||% lower
  le <- if (!is.null(lw)) lw[2L] else tolower(te[[lower]]$name_en %||% lower)
  if (identical(sym, "\\")) {
    g <- unname(.KA5_TYPE_GENDER[upper])
    adj <- switch(if (is.na(g)) "f" else g, m = "flacher", f = "flache", n = "flaches")
    list(name = sprintf("%s %s \u00fcber %s", adj, un, ln), name_en = sprintf("shallow %s over %s", tolower(ue), le))
  } else {
    list(name = sprintf("%s \u00fcber %s", un, ln), name_en = sprintf("%s over %s", ue, le))
  }
}

# The step of a cover against limits (the last limit closes the overlay):
# "\\", "/" or NA (thinner than the first limit when `low` is given, or
# thicker than the last).
.ka5_overlay_step <- function(cover, lim, low = FALSE) {
  if (is.na(cover)) return(NA_character_)
  if (low) {
    if (cover < lim[1L]) return(NA_character_)
    lim <- lim[-1L]
  }
  if (cover < lim[1L]) "\\" else if (cover < lim[2L]) "/" else NA_character_
}

# One overlay record.
.ka5_overlay_rec <- function(kind, cover, upper = NA_character_, lower = NA_character_, sym = NA_character_,
                             provisional = FALSE, note = character(0), warn = TRUE) {
  nm <- if (!is.na(sym) && !is.na(upper) && !is.na(lower)) .ka5_overlay_name(upper, lower, sym) else list(name = NA_character_, name_en = NA_character_)
  code <- if (!is.na(nm$name)) paste0(upper, sym, lower) else NA_character_
  msg <- if (!is.na(code))
    sprintf("Overlay (Tab. 46 no. 18): %s (%s), the upper soil %s cm thick%s.", nm$name, code, format(cover),
            if (provisional) "; a part keyed only provisionally" else "")
  else paste(note, collapse = " ")
  list(kind = kind, code = code, name = nm$name, name_en = nm$name_en, cover_cm = cover, upper = upper,
       lower = lower, symbol = sym, provisional = isTRUE(provisional), notes = note, message = msg,
       warn = isTRUE(warn) && nzchar(msg))
}

# The type of a pedon (a part of a profile) by the key alone, no subtypes or
# overlays: list(code, provisional).
.ka5_key_type <- function(pedon, rules) {
  view <- .ka5_make_view(pedon)
  prev <- list(pedon = .ka5_state$pedon, view = .ka5_state$view, sub_memo = .ka5_state$sub_memo)
  .ka5_state$pedon <- pedon
  .ka5_state$view <- view
  .ka5_state$sub_memo <- new.env(parent = emptyenv())
  on.exit({
    .ka5_state$pedon <- prev$pedon
    .ka5_state$view <- prev$view
    .ka5_state$sub_memo <- prev$sub_memo
  }, add = TRUE)
  .ka5_result_type(.ka5_key_core(pedon, rules, "silent")$res)
}

.ka5_result_type <- function(res) {
  lv <- res$levels
  k <- if (is.data.frame(lv)) which(lv$level == "types") else integer(0)
  list(code = if (length(k)) lv$code[k[1L]] else NA_character_,
       provisional = length(res$provisional_because) > 0L)
}

# A mineral cover on a Moor (Q7).
.ka5_ov_moor_cover <- function(pedon, rules, view, ty) {
  fr <- view$frame
  m <- .ka5_rows(view, list(masters = "H"), buried = TRUE)
  first <- view$ord[m[view$ord] %in% TRUE][1L]
  if (is.na(first)) return(NULL)
  cover <- fr$top_gof[first]
  if (is.na(cover) || cover <= 0) return(NULL)
  lim <- .ka5_thr("ov_moor_cm")
  if (cover < lim[1L])
    return(.ka5_overlay_rec("moor_cover", cover, lower = ty, note = sprintf(
      "A mineral cover of %s cm on the Moor (under %s cm) is named as substrate only, not as an overlay (p. 258; Q7).",
      format(cover), format(lim[1L]))))
  sym <- .ka5_overlay_step(cover, lim, low = TRUE)
  if (is.na(sym)) return(NULL)
  # the cover keyed as the mineral soil it would be without the Moor gate
  r2 <- rules
  r2$divisions <- Filter(function(d) !identical(d$code, "[moore]"), rules$divisions)
  up <- .ka5_result_type(.ka5_key_core(pedon, r2, "silent")$res)
  if (is.na(up$code))
    return(.ka5_overlay_rec("moor_cover", cover, lower = ty, note = sprintf(
      "A mineral cover of %s cm on the Moor keeps the Moor (Q7); the cover keys to no mineral type, the overlay is not named.",
      format(cover))))
  .ka5_overlay_rec("moor_cover", cover, up$code, ty, sym, up$provisional,
                   note = sprintf("a mineral cover of %s cm keeps the Moor (Q7, pp. 258-259)", format(cover)))
}

# A fersiallitic or ferrallitic palaeosol under the soil (Q19).
.ka5_ov_palaeosol <- function(view, cls, ty) {
  if (identical(cls, "V") || is.na(ty)) return(NULL)
  fr <- view$frame
  m <- .ka5_rows(view, list(codes = c("Bj", "Bu", "Bku"), fossil = TRUE), buried = TRUE)
  i <- view$ord[m[view$ord] %in% TRUE][1L]
  if (is.na(i)) return(NULL)
  lower <- if (identical(fr$carrying[i], "Bj")) "VV" else "VW"
  same <- which(fr$layer == fr$layer[i] & !is.na(fr$top_gof) & fr$top_gof <= (fr$top_gof[i] %||% NA))
  cover <- if (isTRUE(fr$layer[i] >= 2L) && length(same)) min(fr$top_gof[same]) else fr$top_gof[i]
  sym <- .ka5_overlay_step(cover, .ka5_thr("ov_steps_cm"))
  if (is.na(sym))
    return(.ka5_overlay_rec("palaeosol", cover, ty, lower, warn = FALSE, note = sprintf(
      "A %s palaeosol under %s cm of the soil above: 7 dm or more, the soil above named alone (Tab. 46 no. 18; Q19).",
      if (lower == "VV") "fersiallitic" else "ferrallitic", format(cover))))
  .ka5_overlay_rec("palaeosol", cover, ty, lower, sym,
                   note = "the soil above gives the type, the palaeosol named as an overlay (Q19, pp. 226, 268)")
}

# The designation of a row of a buried soil keyed alone: the Roman numeral
# taken off (and the layers renumbered from the buried soil's own), the
# fossil part of a row overprinted with the degree sign, the prefix f taken
# off. NA for a row that is no part of the buried soil (a G of the present
# groundwater written without f).
.ka5_buried_designation <- function(d, layer, layer0) {
  if (is.na(d)) return(NA_character_)
  d <- sub("^(IV|III|II)", "", d)
  if (grepl("\u00b0", d, fixed = TRUE)) {
    pt <- strsplit(d, "\u00b0", fixed = TRUE)[[1L]]
    f <- pt[startsWith(pt, "f")]
    d <- if (length(f)) f[1L] else pt[length(pt)]
  }
  fossil <- startsWith(d, "f")
  d <- sub("^f", "", d)
  r <- tryCatch(.ka5_parse_one(d), error = function(e) NULL)
  if (!fossil && !is.null(r) && identical(r$master, "G")) return(NA_character_)
  rel <- (layer %||% 1L) - layer0 + 1L
  if (isTRUE(rel >= 2L)) d <- paste0(c("", "II", "III", "IV")[min(rel, 4L)], d)
  d
}

# A buried soil under the soil (the fossil rows).
.ka5_ov_buried <- function(pedon, rules, view, ty) {
  if (is.na(ty) || identical(ty, "MD")) return(NULL)
  fr <- view$frame
  fpart <- vapply(fr$with, function(w) any(grepl("^f[a-z]*[A-Z]", w)), NA)
  cand <- which((fr$buried > 0L) | (fr$layer >= 2L & (fr$fossil | fpart)))
  cand <- cand[!is.na(fr$top_gof[cand])]
  if (!length(cand)) return(NULL)
  first <- cand[which.min(fr$top_gof[cand])]
  cover <- fr$top_gof[first]
  sec <- view$ord[!is.na(fr$top_gof[view$ord]) & fr$top_gof[view$ord] >= cover]
  if (anyNA(fr$designation[sec])) return(NULL)
  des <- vapply(sec, function(i) .ka5_buried_designation(fr$designation[i], fr$layer[i], fr$layer[first]), "")
  keep <- !is.na(des)
  if (!any(keep)) return(NULL)
  ms <- vapply(des[keep], function(d) (.ka5_parse_one(d)$master %||% NA_character_), "")
  if (all(ms %in% c("F", "H", "C", NA))) return(NULL)
  lim <- if (identical(ty, "MO")) .ka5_thr("ov_mo_pp_cm") else .ka5_thr("ov_steps_cm")
  idx <- fr$horizon_idx[sec[keep]]
  h <- as.data.frame(pedon$horizons, stringsAsFactors = FALSE)[idx, , drop = FALSE]
  h$designation <- des[keep]
  low <- PedonRecord$new(site = pedon$site, horizons = ensure_horizon_schema(data.table::as.data.table(h)))
  lo <- .ka5_key_type(low, rules)
  if (is.na(lo$code))
    return(.ka5_overlay_rec("buried_soil", cover, ty, warn = FALSE, note = sprintf(
      "A buried soil under %s cm keys to no type alone: the overlay is not named.", format(cover))))
  mo_pp <- identical(ty, "MO") && identical(lo$code, "PP")
  if (!mo_pp) lim <- .ka5_thr("ov_steps_cm")
  sym <- .ka5_overlay_step(cover, lim, low = mo_pp)
  if (is.na(sym))
    return(.ka5_overlay_rec("buried_soil", cover, ty, lo$code, warn = FALSE, note = sprintf(
      "A buried %s under %s cm of the upper soil: %s, the upper soil named alone (Tab. 46 no. 18%s).", lo$code, format(cover),
      if (mo_pp && cover < lim[1L]) sprintf("under the %s cm of the Organomarsch over a Podsol", format(lim[1L])) else "7 dm or more",
      if (mo_pp) ", p. 254" else "")))
  .ka5_overlay_rec("buried_soil", cover, ty, lo$code, sym, lo$provisional,
                   note = sprintf("the buried soil keyed alone from its top at %s cm%s", format(cover),
                                  if (mo_pp) " (the Organomarsch over a Podsol, p. 254)" else ""))
}

# Peat under a mineral cover of 4 dm or more: the mineral type (p. 258),
# noted in the trace.
.ka5_ov_peat_below <- function(view, div) {
  if (identical(div, "[moore]")) return(NULL)
  fr <- view$frame
  m <- .ka5_rows(view, list(masters = "H"), buried = TRUE)
  first <- view$ord[m[view$ord] %in% TRUE][1L]
  if (is.na(first) || is.na(fr$top_gof[first]) || fr$top_gof[first] < .ka5_thr("ov_moor_cm")[3L]) return(NULL)
  .ka5_overlay_rec("peat_below", fr$top_gof[first], warn = FALSE, note = sprintf(
    "Peat under %s cm of mineral cover: 4 dm or more, the soil goes to the mineral type (p. 258; Q7).", format(fr$top_gof[first])))
}

#' The overlay of a KA5 result (Tab. 46 no. 18; Q7, Q19)
#'
#' In trace$overlay: kind (moor_cover, palaeosol, buried_soil, peat_below),
#' code (X\Y or X/Y), name (German), name_en, cover_cm, upper, lower,
#' provisional, notes; the named overlays and the thin covers of a Moor in the
#' warnings.
#' @noRd
.ka5_overlay_notes <- function(res, pedon, rules, view) {
  lv <- res$levels
  if (!is.data.frame(lv) || !nrow(lv)) return(res)
  code <- function(l) { k <- which(lv$level == l); if (length(k)) lv$code[k[1L]] else NA_character_ }
  div <- code("divisions"); cls <- code("classes"); ty <- code("types")
  ov <- NULL
  if (identical(div, "[moore]") && !is.na(ty)) ov <- .ka5_ov_moor_cover(pedon, rules, view, ty)
  if (is.null(ov)) ov <- .ka5_ov_palaeosol(view, cls, ty)
  if (is.null(ov)) ov <- .ka5_ov_buried(pedon, rules, view, ty)
  if (is.null(ov)) ov <- .ka5_ov_peat_below(view, div)
  if (is.null(ov)) return(res)
  res$trace$overlay <- ov
  if (isTRUE(ov$warn)) res$warnings <- c(res$warnings, ov$message)
  res
}
