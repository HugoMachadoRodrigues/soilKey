# =============================================================================
# v0.9.361 -- the readers of the German KA5 key to horizons on the data
# (Bodenkundliche Kartieranleitung, 5th ed., Ad-hoc-AG Boden 2005: 5.6.3,
# pp. 88-108; 5.6.5, pp. 110-112).
#
# Each reader turns one question of the horizon definitions into the recorded
# data, three-valued (TRUE, FALSE, or NA when the data leave it open), and
# says what it read: the attributes that would decide it (`missing`), a note
# in soilKey's words and a tag when a fallback or a soilKey WRB diagnostic
# stood in for a field descriptor. The numbers are in
# inst/rules/ka5/thresholds.yaml with their pages; Hugo's decisions and
# soilKey's readings in inst/rules/ka5/readings.yaml:
#   the depth datum GOF (default 9): the top of the first horizon that is not
#     L, O, Of, Oh or Ovh (p. 79); in peat the surface, so an H counts;
#   organic substance (Q10): the loss on ignition first, else organic carbon
#     with the factor of the candidate (2 for H and O, 1.72 for mineral
#     horizons); organic carbon 15 to < 17.4 % undecided between H and Aa
#     unless the designation or a peat descriptor decides;
#   the Ah humus minimum and the silt (Q11): 0.6, 0.9 or 1.2 % by texture;
#     silt not declared in KA5 fractions counts from 50 %, not under 45 %;
#   Munsell steps (Q12): one step of hue is one page of the chart;
#   the area classes f1-f9 and the field codes are read by their ranges;
#   carbonate of the rock (Q15): the prefix, the whole soil, then the fine
#     earth, flagged.
# The profile is an environment with the fields of the Russian readers
# (.kidpr_profile()), so their accessors serve.
# =============================================================================

.ka5_thr_cache <- new.env(parent = emptyenv())

# A number of inst/rules/ka5/thresholds.yaml.
.ka5_thr <- function(name) {
  if (is.null(.ka5_thr_cache$t)) {
    path <- system.file("rules", "ka5", "thresholds.yaml", package = "soilKey")
    if (!nzchar(path)) rlang::abort("The KA5 thresholds are not installed.")
    .ka5_thr_cache$t <- yaml::read_yaml(path)$thresholds
  }
  v <- .ka5_thr_cache$t[[name]]$value
  if (is.null(v)) rlang::abort(sprintf("No KA5 threshold '%s'.", name))
  if (is.list(v)) unlist(v) else v
}

# A reader's answer (the RP readers' shape: passed, missing, note, tag,
# code, value).
.ka5_r <- function(passed, missing = character(0), note = character(0), tag = character(0),
                   code = NULL, value = NULL) {
  .rp_r(passed, missing = missing, note = note, tag = tag, code = code, value = value)
}
.ka5_all <- function(...) .rp_combine(list(...), "and")
.ka5_any <- function(...) .rp_combine(list(...), "or")

# Accessors (the Russian readers' ones).
.ka5_num <- function(x, i, name) .kidpr_num(x, i, name)
.ka5_txt <- function(x, i, name) .kidpr_txt(x, i, name)
.ka5_lgl <- function(x, i, name) .rp_lgl(x, i, name)
.ka5_site_txt <- function(x, name) .kidpr_site_txt(x, name)
.ka5_site_num <- function(x, name) suppressWarnings(as.numeric(.kidpr_site(x, name)))
.ka5_site_lgl <- function(x, name) .rp_site_lgl(x, name)
.ka5_memo <- function(x, key, f) .kidpr_memo(x, key, f)
.ka5_prev <- function(x, i) .kidpr_prev(x, i)
.ka5_next <- function(x, i) .kidpr_next(x, i)
.ka5_colour <- function(x, i, state = "moist") .kidpr_colour(x, i, state)

# A range c(lo, hi) as text.
.ka5_range_txt <- function(r) {
  if (anyNA(r)) return("?")
  if (r[1L] == r[2L]) format(r[1L]) else sprintf("%s-%s", format(r[1L]), format(r[2L]))
}


# --- the profile ----------------------------------------------------------------------

#' The profile as the KA5 key to horizons reads it
#'
#' An environment with the fields of .kidpr_profile() (horizons, site, depth
#' order, caches), the designations read in the KA5 notation
#' (.ka5_designations()), whether the profile is in it, the layer of each row
#' (the KA5 Roman numerals, else the Arabic number of the FAO notation, else
#' 1) and the depth datum GOF.
#' @noRd
.ka5_profile <- function(pedon) {
  x <- new.env(parent = emptyenv())
  h <- pedon$horizons
  n <- if (is.null(h)) 0L else nrow(h)
  x$pedon <- pedon
  x$h <- h
  x$n <- n
  x$site <- pedon$site %||% list()
  x$top <- if (n) suppressWarnings(as.numeric(h$top_cm)) else numeric(0)
  x$bot <- if (n) suppressWarnings(as.numeric(h$bottom_cm)) else numeric(0)
  x$thick <- ifelse(is.na(x$top) | is.na(x$bot), NA_real_, pmax(0, x$bot - x$top))
  x$ord <- if (n) order(ifelse(is.na(x$top), Inf, x$top), seq_len(n)) else integer(0)
  x$rank <- match(seq_len(n), x$ord)
  x$cache <- new.env(parent = emptyenv())
  x$log <- replicate(n, list(), simplify = FALSE)
  x$busy <- logical(n)
  x$des <- .ka5_designations(if (n) h$designation else character(0), x$site)
  x$ka5 <- isTRUE(x$des$ka5)
  x$layer <- vapply(seq_len(n), function(i) {
    if (x$ka5 && isTRUE(x$des$known[i])) return(as.integer(x$des$layer[i] %||% 1L))
    d <- if (is.null(h$designation)) NA_character_ else as.character(h$designation[i])
    m <- regmatches(d, regexec("^\\s*([2-9])[A-Z]", d))[[1L]]
    if (length(m)) as.integer(m[2L]) else 1L
  }, 1L)
  .ka5_gof(x)
  x
}

# The KA5 notation of a row, when the profile is in it (NULL otherwise).
.ka5_row_notation <- function(x, i) {
  if (!x$ka5 || !isTRUE(x$des$known[i])) return(NULL)
  list(code = x$des$code[i], master = x$des$master[i], prefixes = x$des$prefixes[[i]],
       parts = x$des$parts[[i]])
}

# The designation of a row as recorded (NA when none).
.ka5_des <- function(x, i) {
  d <- if (is.null(x$h$designation)) NA else x$h$designation[i]
  d <- as.character(d)
  if (length(d) != 1L || is.na(d) || !nzchar(trimws(d))) NA_character_ else trimws(d)
}

# Are two rows in the same layer (relative tests are made only there,
# default 23)?
.ka5_same_layer <- function(x, i, j) identical(x$layer[i], x$layer[j])


# --- the depth datum GOF (default 9, p. 79) ---------------------------------------

# Is a row an organic surface layer (L, O, Of, Oh, Ovh) that lies above the
# GOF? From the KA5 notation; on the data, organic substance of 30 % or more
# with no sign of a peat. TRUE, FALSE or NA.
.ka5_is_o_layer <- function(x, i) {
  nt <- .ka5_row_notation(x, i)
  if (!is.null(nt)) return(isTRUE(nt$master %in% c("L", "O")))
  d <- .ka5_des(x, i)
  # the FAO Oi, Oe, Oa and the L, F, H of the Canadian and older notations
  if (!x$ka5 && isTRUE(grepl("^(O|Oi|Oe|Oa|L|F|LF|FH|LFH|Ol|Of|Oh)[0-9]*$", d))) return(TRUE)
  og <- .ka5_organic(x, i)
  if (!isTRUE(og$passed)) return(if (is.na(og$passed)) NA else FALSE)
  pt <- .ka5_peat(x, i)
  if (isTRUE(pt$passed)) FALSE else if (isFALSE(pt$passed)) TRUE else NA
}

# The GOF: the top of the first row, in depth order, that is not an organic
# surface layer. Stored as x$gof (cm, the pedon's own depths), x$gof_row and
# x$gof_note; the depths from it in x$rtop and x$rbot. Depths written with a
# "+" above the mineral surface (negative tops) need no conversion: the GOF
# is the top of the first mineral row whatever its value.
.ka5_gof <- function(x) {
  x$gof <- if (x$n && any(!is.na(x$top))) min(x$top, na.rm = TRUE) else 0
  x$gof_row <- NA_integer_
  x$gof_note <- character(0)
  for (i in x$ord) {
    if (is.na(x$top[i])) next
    o <- .ka5_is_o_layer(x, i)
    if (isTRUE(o)) next
    x$gof <- x$top[i]
    x$gof_row <- i
    if (is.na(o)) x$gof_note <- sprintf(
      "the GOF read at %s cm, the top of a layer that may be an organic surface layer (undecided)", format(x$top[i]))
    break
  }
  if (is.na(x$gof_row) && x$n) x$gof_note <- "every layer is an organic surface layer: the GOF is the base of the description"
  if (!is.na(x$gof_row) && x$gof != min(x$top, na.rm = TRUE))
    x$gof_note <- c(x$gof_note, sprintf("depths from the GOF at %s cm, the top of the first horizon that is not L, O, Of, Oh or Ovh (p. 79)",
                                        format(x$gof)))
  x$rtop <- x$top - x$gof
  x$rbot <- x$bot - x$gof
  invisible(x)
}


# --- organic substance (Q10, pp. 89, 92, 111) ---------------------------------------

#' The organic substance of a row, % by mass, as a range c(lo, hi): the loss
#' on ignition when recorded (read as the organic substance), else the
#' organic carbon times the factor (1.72 mineral, 2 peat and Oh), else the
#' humus class of field 29 (Tab. 15; the forest ranges under forest).
#' @noRd
.ka5_os <- function(x, i, factor = .ka5_thr("os_factor_mineral")) {
  loi <- .ka5_num(x, i, "loi_pct")
  if (!is.na(loi)) return(list(range = c(loi, loi), source = "loi_pct",
                               note = sprintf("loss on ignition %s %% read as the organic substance", format(loi))))
  oc <- .ka5_num(x, i, "oc_pct")
  if (!is.na(oc)) {
    v <- oc * factor
    return(list(range = c(v, v), source = "oc_pct",
                note = sprintf("organic carbon %s %% x %s = %s %% organic substance", format(oc), format(factor),
                               format(signif(v, 3)))))
  }
  hc <- .ka5_txt(x, i, "humus_class_ka5")
  if (!is.na(hc)) {
    lu <- .ka5_site_txt(x, "land_use")
    r <- .ka5_humus_class(hc, forest = isTRUE(grepl("forest|wald", lu)))
    if (!anyNA(r)) return(list(range = r, source = "humus_class_ka5",
                               note = sprintf("humus class %s: %s %% organic substance (Tab. 15)", hc, .ka5_range_txt(r))))
  }
  list(range = c(NA_real_, NA_real_), source = NA_character_, note = character(0))
}

# A peat: a peat descriptor recorded (peat_moor_type, fibre, von Post,
# decomposition). TRUE, else NA (FALSE when the fine substance of an O layer
# or a humus form of the forest floor is recorded and no peat descriptor).
.ka5_peat <- function(x, i) {
  .ka5_memo(x, sprintf("peat_%d", i), function() {
    pm <- .ka5_txt(x, i, "peat_moor_type")
    vals <- c(fib = .ka5_num(x, i, "fiber_content_rubbed_pct"), vp = .ka5_num(x, i, "von_post_index"),
              dec = .ka5_num(x, i, "peat_decomposition_pct"))
    if (!is.na(pm) || any(!is.na(vals)))
      return(.ka5_r(TRUE, note = "a peat descriptor recorded (moor type, fibre, von Post or decomposition)",
                    tag = "peat_descriptor"))
    fo <- .ka5_num(x, i, "fine_organic_matter_pct")
    hf <- .ka5_site_txt(x, "humus_form")
    if (!is.na(fo) || isTRUE(hf %in% c("mull", "moder", "mor", "l-mull", "f-mull", "tangel", "hydromull",
                                       "hydromoder", "hydromor")))
      return(.ka5_r(FALSE, note = "no peat descriptor; the fine substance or the humus form of a forest floor recorded"))
    # a wet site: the KA5 reads an organic layer formed under water surplus as
    # peat (soilKey's reading, Hugo 2026-11-06)
    dr <- .ka5_site_txt(x, "drainage_class")
    gw <- .ka5_site_num(x, "groundwater_depth_cm")
    if (isTRUE(grepl("very poorly|poorly drained", dr)) || isTRUE(gw <= 40))
      return(.ka5_r(TRUE, note = "no peat descriptor; the site is wet (drainage or groundwater within 40 cm): a peat",
                    tag = "wet_site_peat"))
    .ka5_r(NA, missing = c("peat_moor_type", "von_post_index", "fine_organic_matter_pct"),
           note = "an organic layer: peat (H) or forest floor (O) undecided")
  })
}

#' Organic substance of 30 % or more (Q10): the loss on ignition decides;
#' else organic carbon 17.4 % or more (x 1.72) is organic, under 15 % (x 2)
#' is not, and 15 to < 17.4 % is undecided between H and Aa unless the KA5
#' notation or a peat descriptor decides; else the humus class h7; else the
#' KA5 notation; a row whose particle size is recorded is mineral (soilKey's
#' reading, as the RP one).
#' @noRd
.ka5_organic <- function(x, i) {
  .ka5_memo(x, sprintf("organic_%d", i), function() {
    lim <- .ka5_thr("organic_min_pct")
    loi <- .ka5_num(x, i, "loi_pct")
    if (!is.na(loi)) return(.ka5_r(loi >= lim, note = sprintf("loss on ignition %s %%: %s", format(loi),
                                   if (loi >= lim) "organic (30 % or more, p. 89)" else "mineral (under 30 %)")))
    oc <- .ka5_num(x, i, "oc_pct")
    nt <- .ka5_row_notation(x, i)
    if (!is.na(oc)) {
      lo <- lim / .ka5_thr("os_factor_peat")
      hi <- lim / .ka5_thr("os_factor_mineral")
      if (oc >= hi) return(.ka5_r(TRUE, note = sprintf("organic carbon %s %%: 30 %% organic substance or more with either factor", format(oc))))
      if (oc < lo) return(.ka5_r(FALSE, note = sprintf("organic carbon %s %%: under 30 %% organic substance with either factor", format(oc))))
      band <- sprintf("organic carbon %s %% lies in the band 15 to < 17.4 %% where the factors 2 (H) and 1.72 (Aa) disagree (Q10)", format(oc))
      if (!is.null(nt) && isTRUE(nt$master %in% c("H", "L", "O")))
        return(.ka5_r(TRUE, note = c(band, sprintf("the notation's %s decides: organic", nt$code)), tag = "designation"))
      if (!is.null(nt) && isTRUE(nt$master %in% c("A", "G", "S")))
        return(.ka5_r(FALSE, note = c(band, sprintf("the notation's %s decides: mineral", nt$code)), tag = "designation"))
      if (isTRUE(.ka5_peat(x, i)$passed) && !"wet_site_peat" %in% .ka5_peat(x, i)$tag)
        return(.ka5_r(TRUE, note = c(band, "a peat descriptor decides: H, organic substance x 2"), tag = "peat_descriptor"))
      return(.ka5_r(NA, missing = c("loi_pct", "peat_moor_type", "von_post_index"), note = c(band, "undecided between H and Aa")))
    }
    hc <- .ka5_txt(x, i, "humus_class_ka5")
    if (!is.na(hc) && !anyNA(.ka5_humus_class(hc)))
      return(.ka5_r(identical(tolower(hc), "h7"), note = sprintf("humus class %s (Tab. 15)", hc)))
    if (!is.null(nt)) return(.ka5_r(nt$master %in% c("H", "L", "O"), tag = "designation",
                                    note = sprintf("no carbon recorded: the notation's %s", nt$code)))
    if (any(!is.na(c(.ka5_num(x, i, "clay_pct"), .ka5_num(x, i, "sand_pct"), .ka5_num(x, i, "silt_pct")))))
      return(.ka5_r(FALSE, note = "no carbon recorded; its particle size is recorded: mineral"))
    if (.ka5_rock_described(x, i)) return(.ka5_r(FALSE, note = "no carbon recorded; described as rock: mineral"))
    pt <- .ka5_peat(x, i)
    if (isTRUE(pt$passed) && !"wet_site_peat" %in% pt$tag) return(.ka5_r(TRUE, note = "no carbon recorded: a peat descriptor", tag = "peat_descriptor"))
    .ka5_r(NA, missing = c("oc_pct", "loi_pct"), note = "no carbon, loss on ignition or particle size recorded")
  })
}


# --- the texture classes of the Ah minimum and the Bt (Q11, pp. 93, 97) -----------------

# The silt of a row against 50 % (the KA5 silt is 2-63 um): TRUE (50 or
# more), FALSE (under 50) or NA, with the undecided band 45 to < 50 when the
# site does not declare KA5 fractions (Hugo's decision Q11).
.ka5_silt50 <- function(x, i) {
  si <- .ka5_num(x, i, "silt_pct")
  if (is.na(si)) {
    tx <- .ka5_txt(x, i, "texture_class_ka5")
    g <- if (!is.na(tx)) .ka5_texture_class(.ka5_v_raw(x, i, "texture_class_ka5")) else NA_character_
    if (identical(g, "U")) return(list(p = TRUE, note = sprintf("texture class %s: a silt", tx)))
    if (identical(g, "S")) return(list(p = FALSE, note = sprintf("texture class %s: a sand", tx)))
    return(list(p = NA, note = character(0)))
  }
  sch <- .ka5_site_txt(x, "particle_size_scheme")
  band <- .ka5_thr("silt_undecided_band")
  cut <- .ka5_thr("texture_silt_cut")
  if (identical(sch, "ka5")) return(list(p = si >= cut, note = sprintf("silt %s %% (KA5 fractions)", format(si))))
  if (si >= cut) return(list(p = TRUE, note = sprintf("silt %s %%", format(si))))
  if (si < band[1L]) return(list(p = FALSE, note = sprintf("silt %s %%", format(si))))
  list(p = NA, note = sprintf("silt %s %% not in the KA5 fractions (2-63 um): 45 to < 50 %% undecided (Q11)", format(si)))
}

# A raw value (with its case kept).
.ka5_v_raw <- function(x, i, name) {
  v <- .kidpr_v(x, i, name)
  if (length(v) != 1L || is.na(v)) NA_character_ else trimws(as.character(v))
}

# The texture class of a row for the Ah minimum and the Bt difference: 1
# (clay < 17, silt < 50), 2 (clay < 17 and silt >= 50, or clay 17 to < 45),
# 3 (clay >= 45); the classes still possible when the data leave it open.
.ka5_texture_classes <- function(x, i) {
  cl <- .ka5_num(x, i, "clay_pct")
  cut <- .ka5_thr("texture_clay_cut")
  if (is.na(cl)) {
    tx <- .ka5_v_raw(x, i, "texture_class_ka5")
    g <- if (!is.na(tx)) .ka5_texture_class(tx) else NA_character_
    # the main group of a Bodenart: T is clay 25 % or more, S and U under 17
    # with silt by the group (soilKey's reading, Hugo 2026-11-06)
    if (identical(g, "S")) return(list(cls = 1:2, note = sprintf("texture class %s", tx)))
    if (identical(g, "U")) return(list(cls = 2L, note = sprintf("texture class %s", tx)))
    if (identical(g, "L")) return(list(cls = 2L, note = sprintf("texture class %s", tx)))
    if (identical(g, "T")) return(list(cls = 2:3, note = sprintf("texture class %s", tx)))
    return(list(cls = 1:3, note = "clay not recorded"))
  }
  if (cl >= cut[2L]) return(list(cls = 3L, note = sprintf("clay %s %%", format(cl))))
  if (cl >= cut[1L]) return(list(cls = 2L, note = sprintf("clay %s %%", format(cl))))
  s <- .ka5_silt50(x, i)
  if (is.na(s$p)) return(list(cls = 1:2, note = c(sprintf("clay %s %%", format(cl)), s$note)))
  list(cls = if (s$p) 2L else 1L, note = c(sprintf("clay %s %%", format(cl)), s$note))
}

#' Organic substance at the Ah minimum (p. 93; Q11): 0.6, 0.9 or 1.2 % by the
#' texture class; a value above every possible minimum holds, below every one
#' fails, in between undecided.
#' @noRd
.ka5_ah_min <- function(x, i) {
  .ka5_memo(x, sprintf("ahmin_%d", i), function() {
    os <- .ka5_os(x, i)
    tc <- .ka5_texture_classes(x, i)
    mins <- .ka5_thr("ah_min_os_pct")[tc$cls]
    if (anyNA(os$range)) return(.ka5_r(NA, missing = c("oc_pct", "humus_class_ka5"),
                                       note = "organic substance not recorded: the Ah minimum undecided"))
    p <- .ka5_at_least(os$range, max(mins))
    if (!isTRUE(p)) p <- if (isTRUE(.ka5_below(os$range, min(mins)))) FALSE else NA
    .ka5_r(p, missing = c("clay_pct", "silt_pct"),
           note = c(os$note, tc$note, sprintf("the Ah minimum %s %% organic substance (p. 93)%s",
                                              paste(unique(mins), collapse = " or "), if (is.na(p)) ": undecided" else "")),
           value = os$range)
  })
}


# --- colours and Munsell steps (Q12) ---------------------------------------------------

# The page of the Munsell chart of a hue: 2.5 units of the hue circle; 10R,
# 2.5YR, 5YR, 7.5YR, 10YR, 2.5Y ... each one page (Hugo's decision Q12).
.ka5_hue_page <- function(col) {
  p <- .rp_hue_pos(col)
  if (is.na(p)) NA_real_ else p / 2.5
}

#' Is a row browner than a reference row (the Bv against the horizon below,
#' Q12): at least `pages` pages of hue redder or `chroma` steps of chroma
#' higher. TRUE, FALSE or NA (a colour missing). Neutral hues have no page:
#' only the chroma counts.
#' @noRd
.ka5_browner <- function(x, i, j, pages = .ka5_thr("bv_hue_pages"), chroma = .ka5_thr("bv_chroma_steps")) {
  a <- .ka5_colour(x, i); b <- .ka5_colour(x, j)
  pa <- .ka5_hue_page(a); pb <- .ka5_hue_page(b)
  hue <- if (is.na(pa) || is.na(pb)) NA else pb - pa >= pages - 1e-9
  chr <- if (is.na(a$chroma) || is.na(b$chroma)) NA else a$chroma - b$chroma >= chroma - 1e-9
  p <- .kidpr_or(hue, chr)
  .ka5_r(p, missing = a$missing, tag = unique(c(a$tag, b$tag)),
         note = sprintf("%s against %s below: %s", .kidpr_colour_text(a), .kidpr_colour_text(b),
                        if (is.na(p)) "browner undecided" else if (p) sprintf("%s page(s) redder or %s chroma steps higher", pages, chroma)
                        else "not browner"))
}

# The colours of the Gr (p. 105): N, any hue at chroma under 1.5, 5G under
# 2.5. TRUE, FALSE or NA.
.ka5_gr_colour <- function(col) {
  if (!is.na(col$family) && col$family == "N") return(TRUE)
  if (is.na(col$chroma)) return(NA)
  lim <- .ka5_thr("gr_chroma_max")
  if (col$chroma < lim) return(TRUE)
  if (isTRUE(col$family == "G") && isTRUE(col$number == 5) && col$chroma < 2.5) return(TRUE)
  FALSE
}

# The pale colour of the Ae and the Sew (p. 94): moist value 4 or more (dry 5
# or more) and value / chroma 2.5 or more (chroma 0 holds).
.ka5_pale <- function(x, i) {
  m <- .ka5_colour(x, i, "moist")
  vm <- .ka5_num(x, i, "munsell_value_moist"); vd <- .ka5_num(x, i, "munsell_value_dry")
  v_ok <- .kidpr_or(if (!is.na(vm)) vm >= .ka5_thr("ae_value_moist_min") else NA,
                    if (!is.na(vd)) vd >= .ka5_thr("ae_value_dry_min") else NA)
  ratio <- if (is.na(m$value) || is.na(m$chroma)) NA else if (m$chroma == 0) TRUE else m$value / m$chroma >= .ka5_thr("ae_value_chroma_min")
  p <- .kidpr_and(v_ok, ratio)
  .ka5_r(p, missing = m$missing, note = sprintf("%s: %s", .kidpr_colour_text(m),
                                               if (is.na(p)) "pale (Ae) undecided" else if (p) "pale as an Ae (p. 94)" else "not pale"))
}


# --- the area shares of the hydromorphic features (Tab. 4, Tab. 16) ----------------------

#' The rust, wet bleaching and reduction colours of a row, % of the area, as
#' ranges: the measured share (redoximorphic_features_pct for the oxidation
#' features, bleached_pct, reduction_colours_pct), else the field codes of
#' fields 30 and 31 by their area classes, else absent for rust and
#' bleaching (a field observation not recorded is absent); the reduction
#' colours from the matrix colour when not recorded (100 in the colours of
#' the Gr, 0 otherwise), undecided with no colour.
#' @noRd
.ka5_shares <- function(x, i) {
  .ka5_memo(x, sprintf("shares_%d", i), function() {
    code <- .ka5_v_raw(x, i, "hydromorphic_features_ka5")
    hf <- if (!is.na(code)) .ka5_hydromorphic_features(code) else NULL
    notes <- character(0); tags <- character(0)
    one <- function(attr, kind, what) {
      v <- .ka5_num(x, i, attr)
      if (!is.na(v)) return(c(v, v))
      if (!is.null(hf) && !anyNA(hf[[kind]])) {
        notes <<- c(notes, sprintf("%s %s %% from the field code %s (Tab. 4, Tab. 16)", what, .ka5_range_txt(hf[[kind]]), code))
        return(hf[[kind]])
      }
      NULL
    }
    rust <- one("redoximorphic_features_pct", "rust", "rust")
    if (is.null(rust)) { rust <- c(0, 0); notes <- c(notes, "no oxidation features recorded: absent") }
    bl <- one("bleached_pct", "bleached", "wet bleaching")
    if (is.null(bl)) { bl <- c(0, 0); notes <- c(notes, "no wet bleaching recorded: absent") }
    red <- one("reduction_colours_pct", "reduction", "reduction colours")
    if (is.null(red)) {
      col <- .ka5_colour(x, i, "moist")
      # only in the hues of the Gr set with a moist value of 4 or more: dark
      # humus colours are not read as reduction colours (soilKey's reading,
      # Hugo, 2026-11-06)
      g <- .ka5_gr_colour(col)
      if (isTRUE(g) && !isTRUE(.ka5_gr_set_hue(x, i))) g <- if (is.na(.ka5_gr_set_hue(x, i))) NA else FALSE
      red <- if (isTRUE(g)) c(100, 100) else if (isFALSE(g)) c(0, 0) else c(NA_real_, NA_real_)
      notes <- c(notes, if (isTRUE(g)) sprintf("no reduction colours recorded: the matrix %s is in the colours of the Gr", .kidpr_colour_text(col))
                 else if (isFALSE(g)) sprintf("no reduction colours recorded; the matrix %s is not reduction-coloured", .kidpr_colour_text(col))
                 else "no reduction colours or matrix colour recorded")
      if (isTRUE(g)) tags <- c(tags, "gley_matrix")
    }
    list(rust = rust, bleached = bl, reduction = red, s = c(min(100, rust[1L] + bl[1L]), min(100, rust[2L] + bl[2L])),
         marbled = isTRUE(hf$marbled), notes = notes, tags = tags,
         recorded = any(!is.na(c(.ka5_num(x, i, "redoximorphic_features_pct"), .ka5_num(x, i, "bleached_pct"),
                                 .ka5_num(x, i, "reduction_colours_pct")))) || !is.null(hf))
  })
}

# A range c(lo, hi) against a minimum v: TRUE when its lower end reaches v,
# FALSE when it lies below v (a measured value, lo = hi, under v; a class
# whose upper end, exclusive, is v or less), NA otherwise; and against a
# maximum (under v).
.ka5_at_least <- function(r, v) {
  if (anyNA(r)) return(NA)
  if (r[1L] >= v) return(TRUE)
  if (r[1L] == r[2L] || r[2L] <= v) FALSE else NA
}
.ka5_below <- function(r, v) {
  if (anyNA(r)) return(NA)
  if (r[1L] >= v) return(FALSE)
  if (r[1L] == r[2L] || r[2L] <= v) TRUE else NA
}


# --- the water of the profile (Q13) ------------------------------------------------------

#' Groundwater or perched water: the site's record. "ground" (groundwater
#' observed, apparent or flooded, a groundwater depth, a level of Tab. 59, a
#' groundwater step or source), "perched" (a perched water table), NA when
#' the site says neither.
#' @noRd
.ka5_water_regime <- function(x) {
  .ka5_memo(x, "water_regime", function() {
    k <- .ka5_site_txt(x, "groundwater_kind")
    if (isTRUE(k == "perched")) return("perched")
    if (isTRUE(k %in% c("observed", "apparent", "flooded"))) return("ground")
    lv <- c(.ka5_site_num(x, "groundwater_mhgw_cm"), .ka5_site_num(x, "groundwater_mngw_cm"),
            .ka5_site_num(x, "groundwater_depth_cm"))
    if (any(!is.na(lv)) || !is.na(.ka5_site_txt(x, "groundwater_step")) || !is.na(.ka5_site_txt(x, "groundwater_source")) ||
        isTRUE(.ka5_site_lgl(x, "groundwater_oxygen_rich")))
      return("ground")
    NA_character_
  })
}

#' Are the redox features of a row those of groundwater (G) rather than of
#' perched water (S)? The site's record (Q13); with none, soilKey's WRB
#' gleyic and stagnic properties stand in, flagged; else NA.
#' @noRd
.ka5_groundwater <- function(x, i) {
  rg <- .ka5_water_regime(x)
  if (identical(rg, "ground")) {
    # depth-aware (D12, Hugo 2026-11-06): a recorded level makes a row G only
    # at or below the MHGW, or within the fluctuation zone above the recorded
    # water table (the MNGW or the depth of the water table; soilKey's
    # convention of 1 m, thresholds.yaml); above it the features are of
    # perched water
    mh <- .ka5_site_num(x, "groundwater_mhgw_cm")
    zone <- .ka5_thr("gw_zone_above_cm")
    if (!is.na(mh))
      return(if (isTRUE(x$bot[i] <= mh)) .ka5_r(FALSE, note = sprintf("the layer lies above the mean high groundwater level (%s cm, Tab. 59)", format(mh)))
             else .ka5_r(TRUE, note = sprintf("at or below the mean high groundwater level (%s cm)", format(mh))))
    lv <- c(.ka5_site_num(x, "groundwater_mngw_cm"), .ka5_site_num(x, "groundwater_depth_cm"))
    lv <- lv[!is.na(lv)]
    if (length(lv)) {
      d <- min(lv)
      if (!is.na(x$bot[i]) && x$bot[i] <= d - zone)
        return(.ka5_r(FALSE, note = sprintf("groundwater recorded at %s cm: the layer lies above its fluctuation zone (%s cm above it), the features are of perched water", format(d), format(zone))))
      if (is.na(x$bot[i])) return(.ka5_r(NA, missing = "bottom_cm", note = "the depth of the layer not recorded"))
    }
    return(.ka5_r(TRUE, note = "groundwater recorded at the site: the redox features are read as formed by it"))
  }
  if (identical(rg, "perched")) return(.ka5_r(FALSE, note = "a perched water table recorded: the redox features are of perched water"))
  gl <- .kidpr_wrb_layer(x, "gleyic_properties", i)
  st <- .kidpr_wrb_layer(x, "stagnic_properties", i)
  if (isTRUE(gl) && !isTRUE(st))
    return(.ka5_r(TRUE, tag = "gleyic_properties", note = "the water not recorded: soilKey's WRB gleyic properties stand in, flagged"))
  if (isTRUE(st) && !isTRUE(gl))
    return(.ka5_r(FALSE, tag = "stagnic_properties", note = "the water not recorded: soilKey's WRB stagnic properties stand in, flagged"))
  .ka5_r(NA, missing = c("groundwater_kind", "groundwater_depth_cm", "groundwater_mhgw_cm"),
         note = "groundwater or perched water not recorded: G or S undecided")
}


# --- carbonate (Q15, pp. 84, 168-169) ------------------------------------------------------

#' The carbonate of a row (the rock prefixes i, e, c: < 2, 2 to < 75, 75 %
#' or more) as a range c(lo, hi): the whole soil (carbonate_whole_soil_pct),
#' else the fine earth (caco3_pct, flagged), else the carbonate class of
#' field 46, else the effervescence (none: under 2 %, flagged).
#' @noRd
.ka5_carbonate <- function(x, i) {
  .ka5_memo(x, sprintf("carbonate_%d", i), function() {
    w <- .ka5_num(x, i, "carbonate_whole_soil_pct")
    if (!is.na(w)) return(list(range = c(w, w), note = sprintf("carbonate of the whole soil %s %%", format(w)), tag = character(0)))
    f <- .ka5_num(x, i, "caco3_pct")
    if (!is.na(f)) return(list(range = c(f, f), tag = "fine_earth_carbonate",
                               note = sprintf("carbonate of the fine earth %s %% stands in for the whole soil (Q15), flagged", format(f))))
    cc <- .ka5_v_raw(x, i, "carbonate_class_ka5")
    if (!is.na(cc)) {
      r <- .ka5_carbonate_class(cc)
      if (!anyNA(r)) return(list(range = r, tag = "fine_earth_carbonate",
                                 note = sprintf("carbonate class %s: %s %% of the fine earth (Tab. 40), flagged", cc, .ka5_range_txt(r))))
    }
    ef <- .ka5_txt(x, i, "effervescence_class")
    if (isTRUE(ef == "none")) return(list(range = c(0, 0.5), tag = "effervescence",
                                          note = "no effervescence: under 0.5 % (c0, c1), flagged"))
    list(range = c(NA_real_, NA_real_), note = "carbonate not recorded", tag = character(0))
  })
}

# The rock prefix of the carbonate: "i", "e", "c" or NA (the possible ones in
# attr "options").
.ka5_carbonate_prefix <- function(r) {
  cut <- .ka5_thr("carbonate_rock_pct")
  if (anyNA(r)) return(structure(NA_character_, options = c("i", "e", "c")))
  k <- function(v, upper = FALSE) if (v < cut[1L] || (upper && v <= cut[1L])) "i" else if (v < cut[2L] || (upper && v <= cut[2L])) "e" else "c"
  a <- k(r[1L]); b <- if (r[2L] == r[1L]) a else k(r[2L], upper = TRUE)
  if (identical(a, b)) a else structure(NA_character_, options = unique(c(a, b)))
}

# Free of lithogenic carbonate in the fine earth (the B and the T, pp. 95,
# 101): caco3_lithogenic recorded; else caco3_pct 0 (or carbonate class c0,
# or no effervescence) holds, carbonate recorded is undecided (it may be
# secondary).
.ka5_no_lithogenic_carbonate <- function(x, i) {
  lg <- .ka5_lgl(x, i, "caco3_lithogenic")
  if (!is.na(lg)) return(.ka5_r(!lg, note = if (lg) "lithogenic carbonate in the fine earth" else "no lithogenic carbonate in the fine earth"))
  f <- .ka5_num(x, i, "caco3_pct")
  if (!is.na(f)) return(.ka5_r(if (f <= 0) TRUE else NA, missing = "caco3_lithogenic",
                               note = if (f <= 0) "no carbonate in the fine earth" else sprintf("carbonate %s %%: lithogenic or secondary undecided", format(f))))
  cc <- .ka5_v_raw(x, i, "carbonate_class_ka5")
  if (isTRUE(tolower(cc) == "c0")) return(.ka5_r(TRUE, note = "carbonate class c0"))
  if (isTRUE(.ka5_txt(x, i, "effervescence_class") == "none")) return(.ka5_r(TRUE, note = "no effervescence"))
  .ka5_r(NA, missing = c("caco3_pct", "caco3_lithogenic"), note = "carbonate of the fine earth not recorded")
}


# --- the clay differences (pp. 97, 219) ---------------------------------------------------

#' The clay difference between an eluvial row `upper` and an illuvial row
#' `lower`: the step its texture class asks (Bt: 3, 5, 8 points; Fahlerde 9
#' or 12), within one layer and over less than 30 cm (centre to centre).
#' `kind` "bt" or "lf". TRUE, FALSE or NA, with the difference in `value`.
#' @noRd
.ka5_clay_diff <- function(x, upper, lower, kind = c("bt", "lf"), between = integer(0)) {
  kind <- match.arg(kind)
  # rows of `between` (the Ael+Bt interfingering of the Fahlerde, p. 220)
  # may lie between the two: they are then read as adjacent (v0.9.363)
  nxt <- function(i) { j <- .ka5_next(x, i); while (!is.na(j) && j %in% between) j <- .ka5_next(x, j); j }
  if (is.na(upper) || is.na(lower)) return(.ka5_r(NA, note = "no eluvial horizon above"))
  cu <- .ka5_num(x, upper, "clay_pct"); cl <- .ka5_num(x, lower, "clay_pct")
  if (is.na(cu) || is.na(cl)) return(.ka5_r(NA, missing = "clay_pct", note = "the clay of the two horizons not recorded"))
  if (!.ka5_same_layer(x, upper, lower))
    return(.ka5_r(NA, note = "the two horizons lie in different layers: the clay difference is not binding (p. 95, default 23)"))
  mid <- function(k) (x$top[k] + x$bot[k]) / 2
  dist <- mid(lower) - mid(upper)
  d <- cl - cu
  tc <- .ka5_texture_classes(x, lower)
  steps <- if (kind == "bt") .ka5_thr("bt_clay_diff")[tc$cls] else .ka5_thr("lf_clay_diff")[pmin(tc$cls, 2L)]
  p <- if (d >= max(steps)) TRUE else if (d < min(steps)) FALSE else NA
  # two adjacent horizons of one layer: the difference is reached at their
  # boundary, within 30 cm (soilKey's reading, Hugo,
  # 2026-11-06); horizons further apart are not compared
  if (!isTRUE(nxt(upper) == lower)) p <- if (isTRUE(p) && isTRUE(dist < .ka5_thr("bt_distance_max_cm"))) TRUE else if (isFALSE(p)) FALSE else NA
  .ka5_r(p, missing = if (is.na(p)) c("clay_pct", "silt_pct") else character(0), value = d,
         note = c(tc$note, sprintf("clay %s %% over %s %%: a difference of %s points against %s (%s)",
                                   format(cl), format(cu), format(d), paste(unique(steps), collapse = " or "),
                                   if (kind == "bt") "p. 97" else "the Fahlerde, p. 219"),
                  if (!isTRUE(nxt(upper) == lower)) "the two horizons are not adjacent"))
}
