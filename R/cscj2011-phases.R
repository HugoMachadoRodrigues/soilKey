# =============================================================================
# v0.9.329 -- the soil phases of the Comprehensive Soil Classification System
# of Japan, First Approximation (CSCJ 2011, chapter VIII, and the procedure
# of chapter IX): the soil temperature phase and the man-made phases. The
# CSCJ has no forest soil phases. Read with the SCSJ code of
# R/scsj2017-phases.R by the CSCJ edition; the CSCJ's own pieces are here.
# =============================================================================

# The soil temperature phase: the mean annual soil temperature at 50 cm, else
# the USDA regime, else the mean annual air temperature plus 2 C, which the
# CSCJ gives when the soil temperature is not measured (fig. 6; Hugo,
# 2026-11-01), the phase then marked as estimated.
.cscj_temperature_phase <- function(site) {
  r <- .scsj_temperature_phase(site)
  if (!is.null(r)) return(r)
  ta <- suppressWarnings(as.numeric(sub(",", ".", as.character(unlist(site$mean_annual_air_temp_C %||% NA)), fixed = TRUE)))
  ta <- ta[!is.na(ta)]
  if (!length(ta)) return(NULL)
  t <- ta[1L] + 2
  code <- if (t < 8) "frigid" else if (t < 15) "mesic" else if (t < 22) "thermic" else "hyperthermic"
  .scsj_phase_row("temperature", code,
                  basis = sprintf("estimated: mean annual air temperature %.1f C plus 2 C (CSCJ VIII)", ta[1L]))
}

# The name of a CSCJ class as a soil kind (a great group without 'great
# group', or a group): English and Japanese; NA when undecided.
.cscj_material_name <- function(code) {
  if (is.na(code)) return(c(NA_character_, NA_character_))
  rules <- load_rules("cscj2011")
  gg <- Filter(function(t) identical(t$code, code), rules$great_groups)
  if (length(gg)) return(c(gg[[1L]]$name, sub("\u5927\u7fa4$", "", gg[[1L]]$name_native)))
  for (g in rules$groups) for (t in g) if (identical(t$code, code)) return(c(t$name, t$name_native))
  c(code, code)
}

# The word for the texture of a layer, as the fill names use it (VIII):
# skeletal (a gravel layer), fine-, medium- or coarse-textured; NULL when not
# decided.
.cscj_texture_word <- function(x, i) {
  if (isTRUE(.cscj_gravel_rows(x)[i])) return(c("skeletal", "\u792b\u8cea"))
  switch(.cscj_texture_rows(x)[i] %||% "none",
         fine = c("fine-textured", "\u7d30\u7c92\u8cea"), medium = c("medium-textured", "\u4e2d\u7c92\u8cea"),
         coarse = c("coarse-textured", "\u7c97\u7c92\u8cea"), NULL)
}

# The thickest layer of a set of rows.
.cscj_thickest <- function(x, rows) {
  if (!length(rows)) return(NA_integer_)
  th <- x$thick[rows]
  if (all(is.na(th))) return(rows[1L])
  rows[which.max(th)]
}

.cscj_with_word <- function(word, kind) {
  if (is.null(word)) kind else c(paste(word[1L], kind[1L]), paste0(word[2L], kind[2L]))
}

# The word before a soil kind in a fill name (VIII): for a kuroboku fill its
# humus class (high-humic with 6 % carbon or more, humic with 3 %), else the
# texture word of its thickest layer; none for a peat fill (Organic soils);
# NULL when not decided.
.cscj_kind_word <- function(x, big, kind) {
  if (identical(kind, "B")) return(NULL)
  if (identical(kind, "D")) {
    oc <- x$oc[big]
    if (is.na(oc)) return(NULL)
    if (oc >= 6) return(c("high-humic", "\u591a\u8150\u690d\u8cea"))
    if (oc >= 3) return(c("humic", "\u8150\u690d\u8cea"))
  }
  .cscj_texture_word(x, big)
}

# The material of a fill outside the Man-made soils (VIII; Hugo,
# 2026-11-01): its artificial material (20 % or more artefacts), dredged
# sediment, or the soil kind its material features name in the order of the
# key (peat: Organic soils; the kuroboku feature: Andosols; the dark red
# colour, under 2 % carbon in an A horizon: Dark Red soils; alluvial
# deposits: Lowland soils; the red-yellow colour: Red-Yellow soils), after
# the word of .cscj_kind_word(). A fill with none of the five kinds is
# keyed on its own when 25 cm or more and named by its great group, else by
# the texture word alone (Hugo, 2026-11-01). NA when not decided.
.cscj_fill_material <- function(pedon, x, rows, open = FALSE) {
  na <- c(NA_character_, NA_character_)
  rows <- x$ord[x$ord %in% rows]
  if (!length(rows)) return(na)
  art <- .scsj_col(x$h, "artefacts_pct")[rows]
  if (any(!is.na(art) & art >= 20)) return(.scsj_artefact_name(x, rows))
  if (open) return(na)
  big <- .cscj_thickest(x, rows)
  txt <- tolower(paste(.scsj_txt(x$h, "layer_origin")[rows], collapse = " "))
  if (grepl("dredg|\u6d5a\u6e2b", txt))
    return(.cscj_with_word(.cscj_texture_word(x, big), c("dredged sediment", "\u6d5a\u6e2b\u571f\u7802")))
  # a dark red colour in a humus-rich A horizon is not that of dark red soil
  # material
  a_row <- ifelse(is.na(x$role) | x$role %in% "other", NA, grepl("^A", x$role))
  low_c <- ifelse(is.na(x$oc), NA, x$oc < 2)
  dark_red <- .scsj_and(x$colour$dark_red, .scsj_or(low_c, .scsj_not(a_row)))
  feats <- list(B = x$peat, D = x$kb, E = dark_red, F = x$orig$alluvial, G = .scsj_red_yellow_rows(x))
  for (k in names(feats)) {
    v <- .scsj_any(feats[[k]][rows])
    if (isTRUE(v)) return(.cscj_with_word(.cscj_kind_word(x, big, k), .cscj_material_name(k)))
    if (is.na(v)) return(na)
  }
  if (sum(x$thick[rows], na.rm = TRUE) >= 25) {
    gg <- .scsj_part_great_group(.scsj_part_pedon(pedon, rows, complete = TRUE))
    if (is.na(gg)) return(na)
    return(.cscj_with_word(.cscj_kind_word(x, big, gg), .cscj_material_name(gg)))
  }
  .cscj_texture_word(x, big) %||% na
}

# The fill of the Reformed soils (35 cm or more, keyed on its own): its
# artificial material or dredged sediment, or its great group after the word
# of .cscj_kind_word() (Hugo, 2026-11-01).
.cscj_reformed_fill_material <- function(pedon, x, rows) {
  rows <- x$ord[x$ord %in% rows]
  art <- .scsj_col(x$h, "artefacts_pct")[rows]
  if (any(!is.na(art) & art >= 20)) return(.scsj_artefact_name(x, rows))
  big <- .cscj_thickest(x, rows)
  txt <- tolower(paste(.scsj_txt(x$h, "layer_origin")[rows], collapse = " "))
  if (grepl("dredg|\u6d5a\u6e2b", txt))
    return(.cscj_with_word(.cscj_texture_word(x, big), c("dredged sediment", "\u6d5a\u6e2b\u571f\u7802")))
  gg <- .scsj_part_great_group(.scsj_part_pedon(pedon, rows, complete = TRUE))
  if (is.na(gg)) return(c(NA_character_, NA_character_))
  .cscj_with_word(.cscj_kind_word(x, big, gg), .cscj_material_name(gg))
}

# The original soil under the fill of the Reformed soils, by its group (VIII:
# texture and group name; the group alone is written): keyed on its own to
# the group level, outside the Man-made soils; NA when undecided. An error
# goes to the caller, which records it as a warning.
.cscj_original_group <- function(p) {
  if (is.null(p)) return(c(NA_character_, NA_character_))
  rules <- load_rules("cscj2011")
  rules$great_groups <- Filter(function(t) !identical(t$code, "A"), rules$great_groups)
  rules$subgroups <- NULL; rules$series_groups <- NULL
  r <- classify_hierarchical("cscj2011", .cscj_marked(p), rules = rules, on_missing = "silent")
  if (length(r$provisional_because) || nrow(r$levels) < 2L) return(c(NA_character_, NA_character_))
  c(r$levels$name[2L], r$levels$name_native[2L])
}

# All the phases of a pedon classified by the CSCJ (temperature, man-made).
.cscj_phases <- function(pedon, res) {
  if (is.null(res$levels) || !nrow(res$levels)) return(NULL)
  x <- .scsj_profile(.cscj_marked(pedon))
  rows <- list(.cscj_temperature_phase(x$site),
               tryCatch(.scsj_man_made_phases(.cscj_marked(pedon), x, res), error = function(e) {
                 res$warnings <- c(res$warnings, sprintf("CSCJ man-made phase not read: %s", conditionMessage(e)))
                 NULL
               }))
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(NULL)
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}
