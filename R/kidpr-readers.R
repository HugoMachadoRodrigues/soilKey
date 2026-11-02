# =============================================================================
# v0.9.336 -- the readers of the Russian horizon key (Field Guide of 2008,
# key 2.1, pp. 12-27, and the horizon descriptions of chapter 3).
#
# The key asks its questions in field words: grey or dark, whitish, bluish,
# olive; lighter or heavier by one gradation; acid or neutral; more than 1 %
# salts; continuous effervescence; frost; ploughed; a deposited layer. Each
# reader here turns one of them into the recorded data, three-valued (TRUE,
# FALSE, or NA when the data leave it open), and says what it read: the
# attributes that would decide it when undecided (`missing`), a note in
# soilKey's words (`note`), and a tag when it used a fallback or a soilKey WRB
# diagnostic in place of a field descriptor (`tag`), so that the evidence of
# every horizon shows it.
#
# Hugo's decisions of 2026-11-02:
#   colours (Q5): dry Munsell, one cut-point per binary choice of the key;
#     dry value = moist value + 1 when only the moist colour is recorded; the
#     gley, olive, bog-iron, peregnoi and solonetz colours (state not given in
#     the text) read moist; the gley set is the whole gley chart;
#   organic matter (Q6): humus = 1.724 x OC; LOI = loi_pct, else 1.724 x OC;
#     decomposition from peat_decomposition_pct, else von Post, else the
#     rubbed fibre, the middle undecided;
#   texture (Q7): the Kachinsky gradations from phys_clay_pct or
#     texture_class_ru, else the USDA classes in order of fineness, one step
#     one gradation, flagged; clay ratio < 2 um for the Russian < 1 um;
#   salts and pH (Q8): soluble_salts_pct, else the EC of chapter 9, else the
#     field signs of key position 2; pH(H2O) acid under 6.5, pH(KCl) alone
#     acid under 5.0 and not acid from 6.2;
#   ploughing (Q18), the deposit (Q21), carbonates, frost and the field
#     observations not recorded (the defaults of the questions).
# =============================================================================

.KIDPR_HUMUS_OC <- 1.724   # humus = 1.724 x organic carbon (Tyurin)
.KIDPR_LOI_OC <- 1.724     # loss on ignition from organic carbon (Q6)

# A reader's answer.
.kidpr_r <- function(passed, missing = character(0), note = character(0),
                     tag = character(0), value = NULL) {
  list(passed = if (length(passed) == 1L) as.logical(passed) else NA,
       missing = unique(as.character(missing)), note = unique(as.character(note)),
       tag = unique(as.character(tag)), value = value)
}

# Three-valued AND / OR / NOT of plain logicals.
.kidpr_and <- function(...) {
  v <- c(...)
  if (any(v %in% FALSE)) FALSE else if (length(v) && all(v %in% TRUE)) TRUE else NA
}
.kidpr_or <- function(...) {
  v <- c(...)
  if (any(v %in% TRUE)) TRUE else if (length(v) && all(v %in% FALSE)) FALSE else NA
}
.kidpr_not <- function(a) if (length(a) != 1L || is.na(a)) NA else !a

# AND / OR of readers' answers: the verdict, and what the answers that
# count for it carried (all of them when undecided).
.kidpr_rand <- function(...) .kidpr_rcombine(list(...), "and")
.kidpr_ror <- function(...) .kidpr_rcombine(list(...), "or")
.kidpr_rcombine <- function(rs, how) {
  rs <- Filter(Negate(is.null), rs)
  v <- vapply(rs, function(r) r$passed, NA)
  p <- if (how == "and") .kidpr_and(v) else .kidpr_or(v)
  # what decided it: the FALSE answers of an AND, the TRUE ones of an OR;
  # all of them when it holds (AND) or fails (OR), or is undecided
  key <- if (how == "and") FALSE else TRUE
  use <- if (!is.na(p) && p == key) which(v %in% key) else seq_along(rs)
  pick <- function(f) unique(unlist(lapply(rs[use], `[[`, f)))
  .kidpr_r(p, missing = if (is.na(p)) unique(unlist(lapply(rs[is.na(v)], `[[`, "missing"))) else character(0),
           note = pick("note"), tag = pick("tag"))
}

# A logical as a reader's answer.
.kidpr_rl <- function(p, missing = character(0), note = character(0), tag = character(0)) {
  .kidpr_r(p, missing = if (is.na(p)) missing else character(0), note = note, tag = tag)
}


# --- the profile ---------------------------------------------------------------

#' The profile as the Russian horizon key reads it
#'
#' An environment, so that the readers cache what they compute once per
#' profile: the horizons, the site, the depth order, the designations read in
#' the Russian notation (.kidpr_designations()), the USDA texture classes,
#' and caches for the readers, the WRB stand-ins and the identification of
#' each layer.
#' @noRd
.kidpr_profile <- function(pedon) {
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
  # depth order; layers with no top come last, in the input order
  x$ord <- if (n) order(ifelse(is.na(x$top), Inf, x$top), seq_len(n)) else integer(0)
  x$rank <- match(seq_len(n), x$ord)
  x$des <- .kidpr_designations(if (n) h$designation else character(0), x$site)
  x$kidpr <- isTRUE(x$des$kidpr)
  x$tex <- if (n) tryCatch(.hscs_texture_class(h), error = function(e) rep(NA_character_, n))
           else character(0)
  x$cache <- new.env(parent = emptyenv())
  x$log <- replicate(n, list(), simplify = FALSE)
  x$ident <- vector("list", n)
  x$busy <- logical(n)
  x
}

# A value computed once per profile (or per layer, by its key).
.kidpr_memo <- function(x, key, f) {
  v <- x$cache[[key]]
  if (!is.null(v)) return(v)
  v <- f()
  assign(key, v, envir = x$cache)
  v
}

# A horizon attribute of one layer (NA when the column is absent).
.kidpr_v <- function(x, i, name) {
  v <- x$h[[name]]
  if (is.null(v) || i < 1L || i > length(v)) return(NA)
  v[[i]]
}
.kidpr_num <- function(x, i, name) suppressWarnings(as.numeric(.kidpr_v(x, i, name)))
.kidpr_txt <- function(x, i, name) {
  v <- .kidpr_v(x, i, name)
  if (length(v) != 1L || is.na(v)) return(NA_character_)
  v <- .field_term(as.character(v))
  if (!nzchar(v)) NA_character_ else v
}
.kidpr_site <- function(x, name) {
  v <- tryCatch(x$site[[name]], error = function(e) NULL)
  if (is.null(v) || !length(v)) return(NA)
  v[[1L]]
}
.kidpr_site_txt <- function(x, name) {
  v <- .kidpr_site(x, name)
  if (length(v) != 1L || is.na(v)) return(NA_character_)
  v <- .field_term(as.character(v))
  if (!nzchar(v)) NA_character_ else v
}

# The layers above and below one layer, in depth order (nearest first for
# `below`, top-down for `above`).
.kidpr_above <- function(x, i) x$ord[seq_len(x$rank[i] - 1L)]
.kidpr_below <- function(x, i) {
  r <- x$rank[i]
  if (r >= x$n) integer(0) else x$ord[seq.int(r + 1L, x$n)]
}
.kidpr_prev <- function(x, i) { a <- .kidpr_above(x, i); if (length(a)) a[length(a)] else NA_integer_ }
.kidpr_next <- function(x, i) { b <- .kidpr_below(x, i); if (length(b)) b[1L] else NA_integer_ }


# --- colours (Q5) ----------------------------------------------------------------

#' The colour of a layer in one state: dry (the key's colour words) or moist
#' (the horizons whose state the text does not give). Missing in that state,
#' it is inferred from the other one, value +/- 1 with hue and chroma as
#' recorded, and the inference is noted.
#' @noRd
.kidpr_colour <- function(x, i, state = c("dry", "moist")) {
  state <- match.arg(state)
  .kidpr_memo(x, sprintf("colour_%s_%d", state, i), function() {
    hd <- .kidpr_v(x, i, "munsell_hue_dry"); hm <- .kidpr_v(x, i, "munsell_hue_moist")
    vd <- .kidpr_num(x, i, "munsell_value_dry"); vm <- .kidpr_num(x, i, "munsell_value_moist")
    cd <- .kidpr_num(x, i, "munsell_chroma_dry"); cm <- .kidpr_num(x, i, "munsell_chroma_moist")
    pick <- function(a, b) if (length(a) == 1L && !is.na(a) && nzchar(as.character(a))) a else b
    note <- character(0); tag <- character(0)
    if (state == "dry") {
      value <- vd; hue <- pick(hd, hm); chroma <- pick(cd, cm)
      if (is.na(vd) && !is.na(vm)) {
        value <- min(10, vm + 1)
        note <- "dry colour inferred from the moist one (value + 1)"
        tag <- "dry_from_moist"
      }
    } else {
      value <- vm; hue <- pick(hm, hd); chroma <- pick(cm, cd)
      if (is.na(vm) && !is.na(vd)) {
        value <- max(0, vd - 1)
        note <- "moist colour inferred from the dry one (value - 1)"
        tag <- "moist_from_dry"
      }
    }
    hp <- .munsell_hue_parts(if (is.null(hue)) NA else hue)
    list(hue = if (is.null(hue) || is.na(hue)) NA_character_ else toupper(gsub("[[:space:]]", "", as.character(hue))),
         number = hp$number, family = hp$family, value = value,
         chroma = if (is.na(hp$family) || hp$family != "N") as.numeric(chroma) else 0,
         state = state, note = note, tag = tag,
         missing = if (state == "dry") c("munsell_value_dry", "munsell_chroma_dry", "munsell_hue_dry")
                   else c("munsell_value_moist", "munsell_chroma_moist", "munsell_hue_moist"))
  })
}

# A colour as a short text ("10YR 5/2 dry").
.kidpr_colour_text <- function(col) {
  if (is.na(col$value) && is.na(col$hue)) return(sprintf("no %s colour", col$state))
  sprintf("%s %s/%s %s", ifelse(is.na(col$hue), "?", col$hue),
          ifelse(is.na(col$value), "?", format(col$value)),
          ifelse(is.na(col$chroma), "?", format(col$chroma)), col$state)
}

# A colour word as a reader's answer: the test on the colour, with the
# colour and its inference in the note.
.kidpr_colour_word <- function(col, p, word) {
  .kidpr_r(p, missing = if (is.na(p)) col$missing else character(0),
           note = c(sprintf("%s: %s", .kidpr_colour_text(col), if (is.na(p)) paste("undecided", word)
                            else if (p) word else paste("not", word)), col$note),
           tag = col$tag)
}

# Grey tones (key 12, 20, 34): chroma 3 or less with hue 10YR, 2.5Y, 5Y or
# N, 7.5YR only at chroma 2 or less (Q5, D29).
.kidpr_is_grey <- function(col) {
  f <- col$family; num <- col$number; c <- col$chroma
  if (!is.na(f) && f == "N") return(TRUE)
  if (is.na(c) && is.na(f)) return(NA)
  if (!is.na(c) && c > 3) return(FALSE)
  if (is.na(f)) return(NA)
  ok_hue <- (f == "YR" && isTRUE(num == 10)) || (f == "Y" && num %in% c(2.5, 5))
  if (f == "YR" && isTRUE(num == 7.5)) return(if (is.na(c)) NA else c <= 2)
  if (!ok_hue) return(FALSE)
  if (is.na(c)) NA else c <= 3
}
.kidpr_grey <- function(x, i) {
  col <- .kidpr_colour(x, i, "dry")
  .kidpr_colour_word(col, .kidpr_is_grey(col), "grey")
}

# Dark grey to black (key 15, 19, 36, 46): dry value 4 or less, chroma 2 or
# less.
.kidpr_dark <- function(x, i) {
  col <- .kidpr_colour(x, i, "dry")
  p <- if (!is.na(col$value) && col$value > 4) FALSE
       else if (!is.na(col$chroma) && col$chroma > 2) FALSE
       else if (is.na(col$value) || is.na(col$chroma)) NA else TRUE
  .kidpr_colour_word(col, p, "dark grey to black")
}

# Whitish: dry value 7 or more, chroma 2 or less.
.kidpr_is_whitish <- function(value, chroma) {
  if (!is.na(value) && value < 7) return(FALSE)
  if (!is.na(chroma) && chroma > 2) return(FALSE)
  if (is.na(value) || is.na(chroma)) NA else TRUE
}

# The gley colours (key 24, moist): the whole gley chart (hues GY, G, BG, B,
# PB and N at value 4-7), with the text's own set (2.5Y value 5-6 chroma 3 or
# less; 7.5Y and 10Y value 5-6 chroma 1).
.kidpr_is_gley <- function(col) {
  f <- col$family; num <- col$number; v <- col$value; c <- col$chroma
  if (is.na(f) || is.na(v)) return(NA)
  if (f %in% c("GY", "G", "BG", "B", "PB", "N")) return(v >= 4 && v <= 7)
  if (f == "Y" && isTRUE(num == 2.5)) return(if (is.na(c)) NA else v >= 5 && v <= 6 && c <= 3)
  if (f == "Y" && num %in% c(7.5, 10)) return(if (is.na(c)) NA else v >= 5 && v <= 6 && c <= 1)
  FALSE
}
.kidpr_gley <- function(x, i) {
  col <- .kidpr_colour(x, i, "moist")
  .kidpr_colour_word(col, .kidpr_is_gley(col), "a gley colour of the matrix")
}

# The olive colour (key 25, moist): 5Y value 5-6, chroma 3-4.
.kidpr_olive <- function(x, i) {
  col <- .kidpr_colour(x, i, "moist")
  f <- col$family; v <- col$value; c <- col$chroma
  p <- if (is.na(f)) NA else if (!(f == "Y" && isTRUE(col$number == 5))) FALSE
       else if (is.na(v) || is.na(c)) NA else v >= 5 && v <= 6 && c >= 3 && c <= 4
  .kidpr_colour_word(col, p, "olive")
}

# The colours of the mottles and of a variegated horizon, and the leached
# (bleached) spots: the morphons of key 22, 48 and 53. Mottles not recorded
# are absent (a field observation).
.kidpr_morphons <- function(x, i) {
  .kidpr_memo(x, sprintf("morphons_%d", i), function() {
    mot <- .munsell_notations(.kidpr_v(x, i, "mottle_colors"))
    var <- .munsell_notations(.kidpr_v(x, i, "variegated_colors"))
    ls <- .kidpr_v(x, i, "leached_spots")
    list(mottles = mot, variegated = var, leached = isTRUE(as.logical(ls)))
  })
}

# Is a moist Munsell colour of a list whitish when dry (moist value + 1)?
.kidpr_morphon_whitish <- function(m) {
  if (!nrow(m)) return(logical(0))
  vapply(seq_len(nrow(m)), function(k) isTRUE(.kidpr_is_whitish(m$value[k] + 1, m$chroma[k])), NA)
}
# Brown: chroma 3 or more in a red or yellow hue.
.kidpr_morphon_brown <- function(m) {
  if (!nrow(m)) return(logical(0))
  vapply(seq_len(nrow(m)), function(k) isTRUE(m$family[k] %in% c("R", "YR", "Y") && m$chroma[k] >= 3), NA)
}


# --- texture (Q7) ------------------------------------------------------------------

# The Kachinsky gradations of the podzolic scale (physical clay < 0.01 mm).
.KIDPR_KACHINSKY <- c("sand" = 1, "loamy sand" = 2, "light loam" = 3, "medium loam" = 4,
                      "heavy loam" = 5, "light clay" = 6, "medium clay" = 7, "heavy clay" = 8)
# The USDA classes in order of fineness (Hugo, Q7): one step, one gradation.
.KIDPR_USDA_STEP <- c("sand" = 1, "loamy sand" = 2, "sandy loam" = 3, "loam" = 4,
                      "silt loam" = 4, "silt" = 4, "sandy clay loam" = 5, "clay loam" = 5,
                      "silty clay loam" = 5, "sandy clay" = 6, "silty clay" = 6, "clay" = 6)

# The Kachinsky gradation of a texture word, English or Russian.
.kidpr_texture_word <- function(w) {
  if (length(w) != 1L || is.na(w)) return(NA_real_)
  w <- tolower(trimws(gsub("\u0451", "\u0435", as.character(w))))
  if (w %in% names(.KIDPR_KACHINSKY)) return(.KIDPR_KACHINSKY[[w]])
  ru <- list(
    c("^\u0441\u0443\u043f\u0435\u0441", 2),                                  # supes'
    c("^\u043f\u0435\u0441\u043e\u043a|^\u043f\u0435\u0441\u0447\u0430\u043d", 1),  # pesok
    c("\u043b\u0435\u0433\u043a.*\u0441\u0443\u0433\u043b\u0438\u043d|^\u043b\u0435\u0433\u043a\u043e\u0441\u0443\u0433\u043b", 3),
    c("\u0441\u0440\u0435\u0434\u043d.*\u0441\u0443\u0433\u043b\u0438\u043d|^\u0441\u0440\u0435\u0434\u043d\u0435\u0441\u0443\u0433\u043b", 4),
    c("\u0442\u044f\u0436\u0435\u043b.*\u0441\u0443\u0433\u043b\u0438\u043d|^\u0442\u044f\u0436\u0435\u043b\u043e\u0441\u0443\u0433\u043b", 5),
    c("\u043b\u0435\u0433\u043a.*\u0433\u043b\u0438\u043d|^\u043b\u0435\u0433\u043a\u043e\u0433\u043b\u0438\u043d", 6),
    c("\u0441\u0440\u0435\u0434\u043d.*\u0433\u043b\u0438\u043d|^\u0441\u0440\u0435\u0434\u043d\u0435\u0433\u043b\u0438\u043d", 7),
    c("\u0442\u044f\u0436\u0435\u043b.*\u0433\u043b\u0438\u043d|^\u0442\u044f\u0436\u0435\u043b\u043e\u0433\u043b\u0438\u043d", 8))
  for (e in ru) if (grepl(e[1], w, perl = TRUE)) return(as.numeric(e[2]))
  NA_real_
}

#' The texture of a layer as a gradation: the Kachinsky gradation (1 sand to
#' 8 heavy clay) from phys_clay_pct, else texture_class_ru; and the USDA step
#' (1 sand to 6 clay) from the lab fractions or the field texture.
#' @noRd
.kidpr_texture <- function(x, i) {
  .kidpr_memo(x, sprintf("texture_%d", i), function() {
    pc <- .kidpr_num(x, i, "phys_clay_pct")
    k <- if (!is.na(pc)) as.numeric(findInterval(pc, c(10, 20, 30, 40, 50, 65)) + 1L + (pc > 80))
         else .kidpr_texture_word(.kidpr_v(x, i, "texture_class_ru"))
    u <- x$tex[i]
    list(k = k, u = if (is.na(u) || !u %in% names(.KIDPR_USDA_STEP)) NA_real_ else .KIDPR_USDA_STEP[[u]],
         usda = u)
  })
}

# The texture of a layer as a reader's value: the gradation and its scale
# (Kachinsky, else the USDA step, flagged).
.kidpr_grad <- function(x, i) {
  t <- .kidpr_texture(x, i)
  if (!is.na(t$k)) return(list(step = t$k, scale = "kachinsky", note = character(0), tag = character(0)))
  if (!is.na(t$u)) return(list(step = t$u, scale = "usda",
                               note = sprintf("texture from the USDA class (%s) for the Kachinsky gradation", t$usda),
                               tag = "usda_texture"))
  list(step = NA_real_, scale = NA_character_, note = character(0), tag = character(0))
}
.KIDPR_TEXTURE_MISSING <- c("phys_clay_pct", "texture_class_ru", "clay_pct", "silt_pct", "sand_pct")

# The texture of a layer against a bound on the gradation scale: `test` a
# function of the gradation (the USDA step reads alike up to heavy loam).
.kidpr_grad_test <- function(x, i, test, word) {
  g <- .kidpr_grad(x, i)
  p <- if (is.na(g$step)) NA else test(g$step)
  .kidpr_r(p, missing = if (is.na(p)) .KIDPR_TEXTURE_MISSING else character(0),
           note = c(if (!is.na(p)) sprintf("texture %s%s", if (p) "" else "not ", word), g$note),
           tag = g$tag)
}

# Gradations of layer i minus those of layer j, on one scale: Kachinsky when
# both have it, else the USDA steps, flagged.
.kidpr_grad_diff <- function(x, i, j) {
  ti <- .kidpr_texture(x, i); tj <- .kidpr_texture(x, j)
  if (!is.na(ti$k) && !is.na(tj$k)) return(list(diff = ti$k - tj$k, note = character(0), tag = character(0)))
  if (!is.na(ti$u) && !is.na(tj$u))
    return(list(diff = ti$u - tj$u, tag = "usda_texture",
                note = sprintf("texture compared by the USDA classes (%s, %s) for the Kachinsky gradations",
                               ti$usda, tj$usda)))
  list(diff = NA_real_, note = character(0), tag = character(0))
}


# --- reaction (Q8) ------------------------------------------------------------------

#' Acid or slightly acid (key 17, 37): pH(H2O) under 6.5; pH(KCl) alone acid
#' under 5.0 and not acid from 6.2, undecided between; without pH, a layer
#' with continuous effervescence is not acid.
#' @noRd
.kidpr_acid <- function(x, i) {
  ph <- .kidpr_num(x, i, "ph_h2o")
  if (!is.na(ph)) return(.kidpr_r(ph < 6.5, note = sprintf("pH(H2O) %s: %s", format(ph),
                                                           if (ph < 6.5) "acid" else "neutral or alkaline")))
  kcl <- .kidpr_num(x, i, "ph_kcl")
  if (!is.na(kcl)) {
    p <- if (kcl < 5.0) TRUE else if (kcl >= 6.2) FALSE else NA
    return(.kidpr_r(p, missing = if (is.na(p)) "ph_h2o" else character(0), tag = "ph_kcl",
                    note = sprintf("pH(KCl) %s only: %s", format(kcl),
                                   if (is.na(p)) "undecided between acid and neutral"
                                   else if (p) "acid" else "neutral or alkaline")))
  }
  e <- .kidpr_eff_cont(x, i)
  if (isTRUE(e$passed)) return(.kidpr_r(FALSE, note = "no pH; continuous effervescence: not acid", tag = e$tag))
  .kidpr_r(NA, missing = c("ph_h2o", "ph_kcl"))
}


# --- salts and pollutants (Q8, Q23) -----------------------------------------------

# The field signs of salts at the surface (key position 2): a salt crust or
# efflorescences recorded at the site or on the top layer.
.kidpr_salt_signs <- function(x) {
  sc <- .kidpr_site_txt(x, "surface_condition")
  sal <- .kidpr_site(x, "salinised")
  top <- x$ord[1L]
  crust <- .kidpr_txt(x, top, "surface_crust_type")
  pat <- .kidpr_txt(x, top, "salt_crust_pattern")
  hal <- .kidpr_num(x, top, "halite_visible_pct")
  isTRUE(grepl("salt|saline|efflores|solonchak|\u0441\u043e\u043b\u0435\u0432|\u0432\u044b\u0446\u0432\u0435\u0442", sc)) ||
    isTRUE(as.logical(sal)) || isTRUE(grepl("evapor|salt", crust)) || !is.na(pat) || isTRUE(hal > 0)
}

#' More than 1 % toxic soluble salts in the 0-10 cm of a surface layer (key
#' positions 1-2; the key's inequality): soluble_salts_pct, else the EC of
#' chapter 9 (ECe 16 dS/m or more, EC 1:5 over 2 dS/m), else the field signs
#' (a salt crust or efflorescences) give it; with neither measurement nor
#' sign, no salts.
#' @noRd
.kidpr_salts_s <- function(x, i) {
  g <- .kidpr_ground(x)
  if (is.na(x$top[i]) || is.na(g) || x$top[i] - g >= 10)
    return(.kidpr_r(FALSE, note = "not in the top 10 cm"))
  s <- .kidpr_num(x, i, "soluble_salts_pct")
  if (!is.na(s)) return(.kidpr_r(s > 1, note = sprintf("soluble salts %s %%", format(s))))
  ece <- .kidpr_num(x, i, "ec_dS_m")
  e15 <- .kidpr_num(x, i, "ec_1to5_dS_m")
  if (!is.na(ece) || !is.na(e15)) {
    p <- .kidpr_or(if (!is.na(ece)) ece >= 16, if (!is.na(e15)) e15 > 2)
    return(.kidpr_r(p, tag = "ec_salts",
                    note = sprintf("salts from the EC (%s): %s", paste(c(if (!is.na(ece)) sprintf("ECe %s dS/m", format(ece)),
                                                                          if (!is.na(e15)) sprintf("EC 1:5 %s dS/m", format(e15))),
                                                                        collapse = ", "),
                                   if (p) "over 1 %" else "under 1 %")))
  }
  if (.kidpr_salt_signs(x)) return(.kidpr_r(TRUE, tag = "salt_signs",
                                            note = "no salt measurement; a salt crust or efflorescences recorded at the surface"))
  .kidpr_r(FALSE, note = "no salt measurement and no salt crust: no salts")
}

#' Pollutants at the extremely hazardous level in a layer starting within 30
#' cm (key positions 1 and 4; Q23): contamination_hazard; the site's
#' contaminated or the layer's contamination_type without a category leave
#' it undecided; nothing recorded, no pollutants.
#' @noRd
.kidpr_contaminated <- function(x, i) {
  g <- .kidpr_ground(x)
  if (is.na(x$top[i]) || is.na(g) || x$top[i] - g >= 30)
    return(.kidpr_r(FALSE, note = "not within 30 cm"))
  hz <- .kidpr_txt(x, i, "contamination_hazard")
  if (!is.na(hz)) return(.kidpr_r(grepl("^extrem", hz), note = sprintf("contamination hazard: %s", hz)))
  ct <- .kidpr_txt(x, i, "contamination_type")
  if (isTRUE(as.logical(.kidpr_site(x, "contaminated"))) || !is.na(ct))
    return(.kidpr_r(NA, missing = "contamination_hazard",
                    note = "contamination recorded without its hazard category"))
  .kidpr_r(FALSE)
}


# --- organic matter (Q6) ------------------------------------------------------------

#' The loss on ignition of a layer: loi_pct, else 1.724 x OC, flagged.
#' @noRd
.kidpr_loi <- function(x, i) {
  l <- .kidpr_num(x, i, "loi_pct")
  if (!is.na(l)) return(list(value = l, note = sprintf("LOI %s %%", format(l)), tag = character(0)))
  oc <- .kidpr_num(x, i, "oc_pct")
  if (!is.na(oc)) {
    v <- round(oc * .KIDPR_LOI_OC, 2)
    return(list(value = v, tag = "loi_from_oc",
                note = sprintf("LOI read as 1.724 x OC (%s %%) = %s %%", format(oc), format(v))))
  }
  list(value = NA_real_, note = character(0), tag = character(0))
}

#' Key position 10: plant residues make up more than 2/3 of the volume (live
#' roots aside): plant_residues_pct; not recorded, the organic matter stands
#' in: LOI over 35 % yes, 25 % or less no, undecided between (Q6, Q24).
#' @noRd
.kidpr_org23 <- function(x, i) {
  .kidpr_memo(x, sprintf("org23_%d", i), function() {
    pr <- .kidpr_num(x, i, "plant_residues_pct")
    if (!is.na(pr)) return(.kidpr_r(pr > 200 / 3, note = sprintf("plant residues %s %% of the volume", format(pr))))
    l <- .kidpr_loi(x, i)
    if (is.na(l$value)) return(.kidpr_r(NA, missing = c("plant_residues_pct", "loi_pct", "oc_pct")))
    p <- if (l$value > 35) TRUE else if (l$value <= 25) FALSE else NA
    .kidpr_r(p, missing = if (is.na(p)) "plant_residues_pct" else character(0),
             note = c(l$note, sprintf("plant residues not recorded: organic matter %s", if (is.na(p)) "between 25 and 35 % LOI, undecided"
                                       else if (p) "over 35 % LOI, an organic layer" else "25 % LOI or less, a mineral layer")),
             tag = c("residues_from_om", l$tag))
  })
}

#' Key position 11: coarse plant residues under 1/3 of the volume (a few
#' fragments do not count): plant_residues_pct over 0 and under 1/3; not
#' recorded, LOI 15 % or more yes (the range of AO, AH and PTR), under 10 %
#' no, undecided between.
#' @noRd
.kidpr_res13 <- function(x, i) {
  pr <- .kidpr_num(x, i, "plant_residues_pct")
  if (!is.na(pr)) return(.kidpr_r(pr > 0 && pr < 100 / 3, note = sprintf("plant residues %s %% of the volume", format(pr))))
  l <- .kidpr_loi(x, i)
  if (is.na(l$value)) return(.kidpr_r(NA, missing = c("plant_residues_pct", "loi_pct", "oc_pct")))
  p <- if (l$value >= 15) TRUE else if (l$value < 10) FALSE else NA
  .kidpr_r(p, missing = if (is.na(p)) "plant_residues_pct" else character(0),
           note = c(l$note, sprintf("plant residues not recorded: organic matter %s", if (is.na(p)) "10-15 % LOI, undecided"
                                     else if (p) "15 % LOI or more, coarse residues" else "under 10 % LOI, no coarse residues")),
           tag = c("residues_from_om", l$tag))
}

#' Degree of decomposition 50 % or more (key position 39): the Russian degree
#' (peat_decomposition_pct), else von Post H7 or more yes and H4 or less no,
#' else the rubbed fibre under 17 % yes and 40 % or more no; between them
#' undecided (Q6).
#' @noRd
.kidpr_decomp50 <- function(x, i) {
  d <- .kidpr_num(x, i, "peat_decomposition_pct")
  if (!is.na(d)) return(.kidpr_r(d >= 50, note = sprintf("decomposition %s %%", format(d))))
  vp <- .kidpr_v(x, i, "von_post_index")
  vpn <- suppressWarnings(as.numeric(sub("^[Hh]", "", as.character(vp))))
  if (!is.na(vpn) && (vpn >= 7 || vpn <= 4))
    return(.kidpr_r(vpn >= 7, tag = "von_post", note = sprintf("decomposition from von Post H%s", format(vpn))))
  rf <- .kidpr_num(x, i, "fiber_content_rubbed_pct")
  if (!is.na(rf) && (rf < 17 || rf >= 40))
    return(.kidpr_r(rf < 17, tag = "rubbed_fibre", note = sprintf("decomposition from the rubbed fibre (%s %%)", format(rf))))
  .kidpr_r(NA, missing = c("peat_decomposition_pct", "von_post_index", "fiber_content_rubbed_pct"),
           note = if (!is.na(vpn) || !is.na(rf)) "von Post H5-H6 or rubbed fibre 17-40 %: undecided")
}


# --- carbonates (default 19) ---------------------------------------------------------

#' Continuous effervescence (key positions 32, 60, 64): effervescence_extent
#' when recorded; else caco3_pct 1 % or more yes, under 0.5 % no; else
#' effervescence_class strong or very strong yes, none no; else an acid
#' layer (pH(H2O) under 6.5) holds no free carbonates, flagged; else
#' undecided.
#' @noRd
.kidpr_eff_cont <- function(x, i) {
  .kidpr_memo(x, sprintf("effc_%d", i), function() {
    ex <- .kidpr_txt(x, i, "effervescence_extent")
    if (!is.na(ex)) return(.kidpr_r(grepl("^contin", ex), note = sprintf("effervescence %s", ex)))
    cc <- .kidpr_num(x, i, "caco3_pct")
    if (!is.na(cc) && (cc >= 1 || cc < 0.5))
      return(.kidpr_r(cc >= 1, tag = "effervescence_from_caco3",
                      note = sprintf("CaCO3 %s %%: %s", format(cc), if (cc >= 1) "continuous effervescence" else "no effervescence")))
    ec <- .kidpr_txt(x, i, "effervescence_class")
    if (!is.na(ec) && grepl("^(strong|very strong|none|no)", ec))
      return(.kidpr_r(grepl("strong", ec), tag = "effervescence_class",
                      note = sprintf("effervescence class %s", ec)))
    ac <- .kidpr_acid_no_carbonates(x, i)
    if (!is.null(ac)) return(ac)
    .kidpr_r(NA, missing = c("effervescence_extent", "caco3_pct", "effervescence_class"))
  })
}

# No effervescence where the layer is acid (pH(H2O) under 6.5): free
# carbonates do not stay there. NULL when the pH does not say it.
.kidpr_acid_no_carbonates <- function(x, i) {
  ph <- .kidpr_num(x, i, "ph_h2o")
  if (is.na(ph) || ph >= 6.5) return(NULL)
  .kidpr_r(FALSE, tag = "effervescence_from_ph",
           note = sprintf("no effervescence recorded; pH(H2O) %s: no free carbonates", format(ph)))
}

# Any effervescence (none recorded is none): the extent, the class or CaCO3.
.kidpr_eff_any <- function(x, i) {
  ex <- .kidpr_txt(x, i, "effervescence_extent")
  if (!is.na(ex)) return(.kidpr_r(!grepl("^none", ex), note = sprintf("effervescence %s", ex)))
  ec <- .kidpr_txt(x, i, "effervescence_class")
  if (!is.na(ec)) return(.kidpr_r(!grepl("^(none|no)$", ec), note = sprintf("effervescence class %s", ec)))
  cc <- .kidpr_num(x, i, "caco3_pct")
  if (!is.na(cc)) return(.kidpr_r(cc >= 0.5, tag = "effervescence_from_caco3", note = sprintf("CaCO3 %s %%", format(cc))))
  ac <- .kidpr_acid_no_carbonates(x, i)
  if (!is.null(ac)) return(ac)
  .kidpr_r(NA, missing = c("effervescence_extent", "effervescence_class", "caco3_pct"))
}

# The forms of the secondary carbonates recorded (";"-lists read).
.kidpr_carb_forms <- function(x, i) {
  f <- .kidpr_txt(x, i, "secondary_carbonates_form")
  if (is.na(f)) return(character(0))
  trimws(strsplit(f, "[;,]")[[1L]])
}

#' Distinct carbonate neoformations (key position 62): pseudomycelium, white-
#' eye (soft concretions) or impregnation spots (masses, powdery), or
#' secondary carbonates recorded with no form; not recorded, soilKey's calcic
#' horizon stands in (Hugo's decision Q4), flagged.
#' @noRd
.kidpr_carb_neo <- function(x, i) {
  fm <- .kidpr_carb_forms(x, i)
  sp <- .kidpr_num(x, i, "secondary_carbonates_pct")
  soft <- .kidpr_num(x, i, "carbonate_soft_pct")
  if (length(fm) && all(grepl("^(none|no|absent)$", fm)))
    return(.kidpr_r(FALSE, note = "no carbonate neoformations recorded"))
  if (length(fm)) {
    p <- any(grepl("pseudomyc|soft concr|mass|powder", fm))
    return(.kidpr_r(p, note = sprintf("carbonate forms: %s", paste(fm, collapse = ", "))))
  }
  if (isTRUE(sp > 0) || isTRUE(soft > 0))
    return(.kidpr_r(TRUE, note = "secondary carbonates recorded (no form)"))
  w <- .kidpr_wrb_layer(x, "calcic", i)
  .kidpr_r(w, missing = if (is.na(w)) c("secondary_carbonates_form", "secondary_carbonates_pct") else character(0),
           tag = "calcic", note = sprintf("carbonate neoformations not recorded: soilKey's calcic horizon %s",
                                          if (is.na(w)) "undecided" else if (w) "stands in (passes)" else "stands in (fails)"))
}

# White-eye (beloglazka): rounded soft carbonate segregations, the FAO soft
# concretions (a field observation: not recorded, absent).
.kidpr_belo <- function(x, i) {
  fm <- .kidpr_carb_forms(x, i)
  p <- any(grepl("soft concr", fm))
  .kidpr_r(p, note = if (p) "white-eye (soft carbonate concretions)" else "no white-eye recorded")
}


# --- frost (default 20) --------------------------------------------------------------

# A climate that excludes long frost: a soil temperature regime of mesic or
# warmer, a mean annual soil or air temperature over 5 C, a Koppen A or C
# climate.
.kidpr_warm <- function(x) {
  str <- .kidpr_site_txt(x, "soil_temperature_regime")
  ms <- suppressWarnings(as.numeric(.kidpr_site(x, "mean_annual_soil_temp_C")))
  ma <- suppressWarnings(as.numeric(.kidpr_site(x, "mean_annual_air_temp_C")))
  kp <- .kidpr_site(x, "koppen_climate")
  isTRUE(grepl("mesic|thermic", str)) || isTRUE(ms > 5) || isTRUE(ma > 5) ||
    isTRUE(grepl("^[AaCc]", as.character(kp)))
}

#' Permafrost in or under the profile: frost_regime permafrost; ice or a
#' frozen layer in the horizon data (permafrost_temp_C 0 or less, ice_pct,
#' ice_form, the permafrost sign, the FAO frozen suffix); a gelic or pergelic
#' soil temperature regime.
#' @noRd
.kidpr_permafrost <- function(x) {
  .kidpr_memo(x, "permafrost", function() {
    fr <- .kidpr_site_txt(x, "frost_regime")
    if (!is.na(fr) && fr == "permafrost") return(.kidpr_r(TRUE, note = "frost regime: permafrost"))
    h <- x$h
    pt <- suppressWarnings(as.numeric(h$permafrost_temp_C))
    ice <- suppressWarnings(as.numeric(h$ice_pct))
    ifm <- .field_term(h$ice_form)
    des <- as.character(h$designation)
    pat <- tryCatch(.frozen_designation_pattern(x$site), error = function(e) "perma")
    if (x$kidpr) pat <- "\u22a5|\uf05e|perma"
    if (any(pt <= 0, na.rm = TRUE) || any(ice > 0, na.rm = TRUE) ||
        any(!is.na(ifm) & nzchar(ifm) & !grepl("^(none|no|absent)", ifm)) ||
        any(grepl(pat, des, perl = TRUE)))
      return(.kidpr_r(TRUE, note = "frozen ground in the horizon data"))
    str <- .kidpr_site_txt(x, "soil_temperature_regime")
    if (isTRUE(grepl("gelic", str))) return(.kidpr_r(TRUE, note = sprintf("soil temperature regime %s", str)))
    if (!is.na(fr)) return(.kidpr_r(FALSE, note = sprintf("frost regime: %s", fr)))
    if (!is.na(str) || .kidpr_warm(x)) return(.kidpr_r(FALSE, note = "no permafrost (soil temperature regime or climate)"))
    .kidpr_r(NA, missing = c("frost_regime", "permafrost_temp_C", "ice_pct", "soil_temperature_regime"))
  })
}

#' Long-seasonal or perennial frost in or directly under the profile (key
#' position 26): frost_regime long-seasonal or permafrost; permafrost by the
#' data; no frost by frost_regime none or seasonal, or by a climate that
#' excludes it; else undecided (default 20).
#' @noRd
.kidpr_frost <- function(x) {
  .kidpr_memo(x, "frost", function() {
    fr <- .kidpr_site_txt(x, "frost_regime")
    if (!is.na(fr) && fr %in% c("long-seasonal", "permafrost"))
      return(.kidpr_r(TRUE, note = sprintf("frost regime: %s", fr)))
    pf <- .kidpr_permafrost(x)
    if (isTRUE(pf$passed)) return(pf)
    if (!is.na(fr)) return(.kidpr_r(FALSE, note = sprintf("frost regime: %s", fr)))
    if (.kidpr_warm(x)) return(.kidpr_r(FALSE, tag = "frost_from_climate",
                                        note = "no long frost: a climate record excludes it"))
    .kidpr_r(NA, missing = c("frost_regime", "soil_temperature_regime"),
             note = "long-seasonal frost not recorded")
  })
}


# --- ploughing (Q18) -------------------------------------------------------------------

.KIDPR_ARABLE <- paste0("arable|crop|cultivat|plough|plow|tillage|tilled|cereal|wheat|maize|corn|",
                        "barley|potato|vegetable|horticult|fallow|abandoned (arable|field|crop)|",
                        "\u043f\u0430\u0448\u043d|\u043f\u0430\u0445\u043e\u0442|\u0437\u0430\u043b\u0435\u0436")

# The top layer that is not litter: where the soil starts.
.kidpr_ground <- function(x) {
  .kidpr_memo(x, "ground", function() {
    s <- .kidpr_start_row(x)
    if (is.na(s)) NA_real_ else x$top[s]
  })
}
.kidpr_start_row <- function(x) {
  .kidpr_memo(x, "start_row", function() {
    o <- x$ord[!vapply(x$ord, function(i) .kidpr_litter(x, i), NA)]
    if (length(o)) o[1L] else NA_integer_
  })
}

# Fresh litter, moss or felt (not diagnostic, p. 36): the FAO Oi or L, the
# Soviet A00, read from the hints of a designation that is not Russian.
.kidpr_litter <- function(x, i) {
  if (x$kidpr && isTRUE(x$des$known[i])) return(FALSE)
  nt <- x$des$notes[[i]] %||% character(0)
  any(grepl("litter, not diagnostic", nt))
}

# A designation that is not Russian with the plough suffix p (Ap, Op, Hp).
.kidpr_fao_p <- function(x, i) {
  d <- as.character(.kidpr_v(x, i, "designation"))
  if (is.na(d) || (x$kidpr && isTRUE(x$des$known[i]))) return(NA)
  grepl("^[0-9IVX]*[A-Z][A-Z]?[a-z]*p", d) && !grepl("^[0-9IVX]*[A-Z][A-Z]?[a-z]*b", d)
}

#' An agro (ploughed) horizon (key positions 14, 21, 40, 46; Hugo's decision
#' Q18): the Russian agro index; else the plough suffix p (Ap, Op, Hp) in the
#' run of layers from the surface; else a land use of tillage with the top
#' layers ending at an abrupt (smooth) boundary within 40 cm. A designated
#' profile with no plough mark and no tillage is natural; a profile with no
#' designation and no land use is undecided.
#' @noRd
.kidpr_agro <- function(x, i) {
  .kidpr_memo(x, sprintf("agro_%d", i), function() {
    if (x$kidpr && isTRUE(x$des$known[i])) {
      a <- isTRUE(x$des$agro[i])
      return(.kidpr_r(a, note = sprintf("the Russian index %s", if (a) "is an agro horizon" else "is not an agro horizon")))
    }
    start <- .kidpr_start_row(x)
    if (is.na(start)) return(.kidpr_r(FALSE))
    from_top <- x$ord[x$rank >= x$rank[start]]
    from_top <- from_top[order(x$rank[from_top])]
    p <- vapply(from_top, function(j) isTRUE(.kidpr_fao_p(x, j)), NA)
    run <- if (length(p) && p[1L]) from_top[seq_len(if (all(p)) length(p) else which(!p)[1L] - 1L)] else integer(0)
    if (i %in% run) return(.kidpr_r(TRUE, note = "the plough suffix p of the designation"))
    if (length(run)) return(.kidpr_r(FALSE, note = "below the ploughed layers"))
    g <- .kidpr_ground(x)
    deep <- !is.na(x$top[i]) && !is.na(g) && x$top[i] - g >= 40
    if (deep) return(.kidpr_r(FALSE, note = "deeper than 40 cm"))
    lu <- .kidpr_site_txt(x, "land_use")
    if (!is.na(lu) && grepl(.KIDPR_ARABLE, lu, perl = TRUE)) {
      # the top layers down to an abrupt lower boundary within 40 cm
      for (j in from_top) {
        if (is.na(x$bot[j]) || is.na(g) || x$bot[j] - g > 40)
          return(.kidpr_r(FALSE, note = sprintf("land use '%s', no abrupt boundary within 40 cm", lu)))
        bd <- .kidpr_txt(x, j, "boundary_distinctness")
        bt <- .kidpr_txt(x, j, "boundary_topography")
        if (is.na(bd)) return(.kidpr_r(NA, missing = "boundary_distinctness",
                                       note = sprintf("land use '%s'; the lower boundary not recorded", lu)))
        if (grepl("abrupt|sharp", bd) && (is.na(bt) || grepl("smooth|even|plan", bt))) {
          ok <- x$rank[i] <= x$rank[j]
          return(.kidpr_r(ok, tag = if (ok) "agro_from_land_use",
                          note = sprintf("land use '%s'%s", lu, if (ok) sprintf(" and an abrupt smooth boundary at %s cm", format(x$bot[j]))
                                                               else "; below the ploughed layer")))
        }
      }
      return(.kidpr_r(FALSE, note = sprintf("land use '%s', no abrupt boundary within 40 cm", lu)))
    }
    if (!is.na(lu)) return(.kidpr_r(FALSE, note = sprintf("land use '%s': not tilled", lu)))
    if (any(!is.na(x$h$designation) & nzchar(trimws(as.character(x$h$designation)))))
      return(.kidpr_r(FALSE, note = "a designated profile with no plough mark: natural"))
    .kidpr_r(NA, missing = c("designation", "land_use"), note = "no designation and no land use: ploughing undecided")
  })
}


# --- the surface deposit (Q21, key positions 8-9) -----------------------------------

# Origins that are a deposit (layer_origin, rock_origin).
.KIDPR_DEPOSIT_ORIGIN <- "fluv|alluv|aeol|eol|wind|colluv|solimov|tephr|pyroclast|volcan|ash|fill|dump|deposit|\u043d\u0430\u043d\u043e\u0441|\u0430\u043b\u043b\u044e\u0432"

# A buried horizon by its designation: brackets of the Russian notation, the
# FAO suffix b.
.kidpr_buried_des <- function(x, i) {
  if (isTRUE(x$des$buried[i])) return(TRUE)
  d <- as.character(.kidpr_v(x, i, "designation"))
  if (is.na(d) || (x$kidpr && isTRUE(x$des$known[i]))) return(FALSE)
  grepl("^[0-9IVX]*[A-Z]{1,2}[a-z]*b[a-z]*[0-9]*$", d)
}

#' The deposit at the surface (key positions 8-9, Hugo's decision Q21): the
#' mineral layers above a buried soil (a buried horizon by its designation,
#' or an organic layer under mineral ones, key criterion 4); else the layers
#' from the surface whose origin is a deposit and that show layering
#' (stratification recorded, criterion 1). A humus horizon formed in the
#' deposit, with no buried soil and no layering, is not a deposit (an AY at
#' the top of a floodplain). Rows in depth order, the total thickness and the
#' base.
#' @noRd
.kidpr_deposit <- function(x) {
  .kidpr_memo(x, "deposit", function() {
    start <- .kidpr_start_row(x)
    none <- list(rows = integer(0), thick = 0, bottom = NA_real_, note = character(0), passed = FALSE)
    if (is.na(start)) return(none)
    o <- x$ord[x$rank >= x$rank[start]]
    o <- o[order(x$rank[o])]
    # a buried soil: the first buried horizon, or an organic layer that lies
    # under mineral layers
    mineral_seen <- FALSE
    marker <- NA_integer_
    for (j in o) {
      if (.kidpr_buried_des(x, j)) { marker <- j; break }
      org <- isTRUE(.kidpr_org23(x, j)$passed)
      if (org && mineral_seen) { marker <- j; break }
      if (!org) mineral_seen <- TRUE
    }
    if (!is.na(marker)) {
      above <- o[x$rank[o] < x$rank[marker]]
      # the mineral layers directly above the buried soil
      rows <- integer(0)
      for (j in rev(above)) {
        if (isTRUE(.kidpr_org23(x, j)$passed)) break
        rows <- c(j, rows)
      }
      if (length(rows)) {
        th <- sum(x$thick[rows])
        return(list(rows = rows, thick = th, bottom = x$bot[rows[length(rows)]], passed = TRUE,
                    note = sprintf("%s cm of deposited material over a buried soil (%s)",
                                   format(th), as.character(.kidpr_v(x, marker, "designation")))))
      }
    }
    orig <- function(j) paste(.kidpr_txt(x, j, "layer_origin"), .kidpr_txt(x, j, "rock_origin"))
    strat <- function(j) { s <- .kidpr_txt(x, j, "stratification_pattern"); !is.na(s) && !grepl("^(none|no|absent)", s) }
    rows <- integer(0)
    for (j in o) {
      if (isTRUE(.kidpr_org23(x, j)$passed) && !length(rows)) next
      if (grepl(.KIDPR_DEPOSIT_ORIGIN, orig(j), perl = TRUE) && strat(j)) rows <- c(rows, j) else break
    }
    if (length(rows)) {
      th <- sum(x$thick[rows])
      return(list(rows = rows, thick = th, bottom = x$bot[rows[length(rows)]], passed = TRUE,
                  note = sprintf("%s cm of layered deposited material at the surface", format(th))))
    }
    none
  })
}


# --- structure and clay films -------------------------------------------------------

# The structure of a layer in field terms (lower case, no accents).
.kidpr_structure <- function(x, i) {
  list(type = .kidpr_txt(x, i, "structure_type"), grade = .kidpr_txt(x, i, "structure_grade"),
       size = .kidpr_txt(x, i, "structure_size"), secondary = .kidpr_txt(x, i, "structure_type_secondary"))
}
# A structure no soil formation made: massive, single grain, structureless,
# or the layering of the rock.
.KIDPR_RAW_STRUCTURE <- "massive|single|structureless|apedal|layer|stratif|rock|\u0431\u0435\u0441\u0441\u0442\u0440\u0443\u043a\u0442"
# A pedogenic structure.
.KIDPR_PED_STRUCTURE <- "granul|crumb|block|nut|polyhed|prism|column|angular|subangular|wedge|lentic|cuneif|platy|lamin|roe"

#' Continuous clay or humus-clay cutans (key position 31): clay films common
#' or many; few or none, no. The thin cutans of CAT (position 63, `thin`):
#' few or more. Not recorded, soilKey's argic horizon stands in (Hugo's
#' decision Q4), flagged.
#' @noRd
.kidpr_cutans <- function(x, i, thin = FALSE) {
  cf <- .clay_films_class(.kidpr_v(x, i, "clay_films_amount"))
  if (!is.na(cf)) return(.kidpr_r(cf >= if (thin) 2L else 3L, note = sprintf("clay films %s", .kidpr_txt(x, i, "clay_films_amount"))))
  w <- .kidpr_wrb_layer(x, "argic", i)
  .kidpr_r(w, missing = if (is.na(w)) "clay_films_amount" else character(0), tag = "argic",
           note = sprintf("clay films not recorded: soilKey's argic horizon %s",
                          if (is.na(w)) "undecided" else if (w) "stands in (the layer is argic)" else "stands in (the layer is not argic)"))
}


# --- soilKey's WRB diagnostics as stand-ins (Q4) --------------------------------------

# A WRB diagnostic of the profile, computed once (NULL when it fails).
.kidpr_wrb <- function(x, name) {
  .kidpr_memo(x, paste0("wrb_", name), function() {
    f <- get0(name, envir = asNamespace("soilKey"), mode = "function")
    if (is.null(f)) return(list(failed = TRUE))
    r <- tryCatch(suppressWarnings(suppressMessages(f(x$pedon))), error = function(e) NULL)
    if (is.null(r)) list(failed = TRUE) else r
  })
}

# Does a WRB diagnostic hold in one layer: TRUE when the layer is among its
# layers, FALSE when it was decided without the layer, NA otherwise.
.kidpr_wrb_layer <- function(x, name, i) {
  r <- .kidpr_wrb(x, name)
  if (is.list(r) && isTRUE(r$failed)) return(NA)
  if (i %in% r$layers) return(TRUE)
  if (isTRUE(r$passed) || isFALSE(r$passed)) return(FALSE)
  NA
}
