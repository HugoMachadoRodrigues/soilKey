# =============================================================================
# v0.9.356 -- the South African soil classification of 1991 (Soil
# Classification Working Group, 1991; the "Blue Book"), part 2: the
# diagnostic horizons on the recorded data, first part. The five topsoil
# horizons (organic O, humic A, vertic A, melanic A, orthic A) and the eight
# apedal and structured B horizons (red apedal, yellow-brown apedal, red
# structured, neocutanic, neocarbonate, pedocutanic, prismacutanic and
# lithocutanic B), with the colour readers of Hugo's decisions Q9-Q12
# (2026-11-04) and their amendments of 2026-11-05.
#
# The book is not available: the criteria are read from the open topsoil
# pages (T), SASRI 1999 (S) and van Huyssteen 2020 (V), paraphrased. Every
# number is in inst/rules/za1991/thresholds.yaml; a number that is not the
# book's (a WRB stand-in or soilKey's own) is marked stand_in there, and every
# answer that reads it carries the tag "stand_in:<name>", so that the flags
# drop when the book's numbers replace them, without a change here.
#
# Each test is three-valued (TRUE, FALSE, NA) on one layer, and says what it
# read: the attributes that would decide it (missing), a note, and tags for
# the stand-ins and fallbacks it used. A field observation not recorded is
# absent (slickensides, mottles, cutans, variegated colours, effervescence,
# stratification); a measurement not recorded leaves its test undecided; the
# structure, which every layer has in some grade, is not "absent" when not
# recorded. The order of the tests over a whole layer (.za_identify(), the
# position pass, the other seventeen horizons and materials) is the next
# part; here each horizon has its own test, and the topsoil horizons their
# fixed order (O, vertic, melanic, humic, orthic; default 7). v0.9.357 adds
# that part at the end of this file.
# =============================================================================

.ZA_REFERENCE <- paste(
  "Soil Classification Working Group (1991). Soil classification: a taxonomic system for",
  "South Africa. Memoirs on the Agricultural Natural Resources of South Africa No. 15.",
  "Read without the book from its open topsoil pages, SASRI (1999) and van Huyssteen (2020);",
  "numbers and stand-ins in inst/rules/za1991/thresholds.yaml.")


# --- thresholds ------------------------------------------------------------------

# The thresholds of inst/rules/za1991/thresholds.yaml.
.za_thresholds <- function() {
  if (is.null(.za_cache$thresholds)) {
    path <- system.file("rules", "za1991", "thresholds.yaml", package = "soilKey")
    if (!nzchar(path)) rlang::abort("The thresholds of the South African horizons are not installed.")
    .za_cache$thresholds <- yaml::read_yaml(path)$thresholds
  }
  .za_cache$thresholds
}
.za_th <- function(name) {
  t <- .za_thresholds()[[name]]
  if (is.null(t)) rlang::abort(sprintf("No South African threshold '%s'.", name))
  t
}
.za_thv <- function(name) .za_th(name)$value

# The tags and notes of thresholds that stand in for the book's numbers.
.za_flags <- function(names) {
  names <- unique(names[!is.na(names)])
  sin <- names[vapply(names, function(n) isTRUE(.za_th(n)$stand_in), NA)]
  list(tag = if (length(sin)) paste0("stand_in:", sin) else character(0),
       note = if (length(sin)) sprintf("%s: soilKey's stand-in (%s) for a number of the book, flagged",
                                       sin, vapply(sin, function(n) as.character(.za_th(n)$source), ""))
              else character(0))
}

# An answer (the RP readers' answer), with the flags of the thresholds read.
.za_r <- function(passed, missing = character(0), note = character(0), tag = character(0),
                  th = character(0), value = NULL) {
  f <- .za_flags(th)
  .rp_r(passed, missing = missing, note = c(note, f$note), tag = c(tag, f$tag), value = value)
}
# The negation of an answer; `note` is added when the negation holds (or
# always, with always = TRUE).
.za_not <- function(r, note = character(0), always = FALSE) {
  p <- if (is.na(r$passed)) NA else !r$passed
  .rp_r(p, missing = r$missing, note = c(r$note, if (always || isTRUE(p)) note), tag = r$tag, value = r$value)
}


# --- the profile ---------------------------------------------------------------------

#' The profile as the South African tests read it: an environment with the
#' fields the Russian accessors use (.kidpr_num(), .kidpr_txt(), the memo,
#' the layers above and below), and the South African names of the layers
#' (.za_pedon_names()).
#' @noRd
.za_x <- function(pedon) {
  if (is.environment(pedon) && !inherits(pedon, "R6") && !is.null(pedon$cache)) return(pedon)
  if (!inherits(pedon, "PedonRecord")) rlang::abort("'pedon' must be a PedonRecord.")
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
  x$names <- if (n) tryCatch(.za_pedon_names(pedon), error = function(e) NULL) else NULL
  x
}

.za_num <- function(x, i, name) .kidpr_num(x, i, name)
.za_txt <- function(x, i, name) .kidpr_txt(x, i, name)
.za_des <- function(x, i) {
  d <- .kidpr_v(x, i, "designation")
  if (length(d) != 1L || is.na(d) || !nzchar(trimws(as.character(d)))) NA_character_ else trimws(as.character(d))
}
.za_lgl <- function(x, i, name) .rp_lgl(x, i, name)
# The master letter of a designation (after a discontinuity number), NA.
.za_master <- function(x, i) {
  d <- .za_des(x, i)
  if (is.na(d)) return(NA_character_)
  m <- regmatches(d, regexpr("^[0-9]*[A-Z]", d))
  if (!length(m)) NA_character_ else sub("^[0-9]*", "", m)
}

#' A layer of rock: designated R (or 2R, R/Cr) or bedrock_pct of 100. No B
#' horizon is tested on it (the review's D6).
#' @noRd
.za_is_rock <- function(x, i) {
  isTRUE(grepl(.WRB_ROCK_DESIGNATION, .za_des(x, i))) || isTRUE(.za_num(x, i, "bedrock_pct") >= 100)
}
.za_rock_no <- function() .za_r(FALSE, note = "a layer of rock: no B horizon")


# --- colours (Q9-Q12) ------------------------------------------------------------------

# The hue families written without a number (GY alone).
.ZA_HUE_FAMILIES <- c("YR", "GY", "BG", "PB", "RP", "R", "Y", "G", "B", "P")

#' The matrix colour of a layer in one state, as recorded (no inference from
#' the other state here: each test says what it does without it). A hue
#' family written alone (GY) is read as the family, with no number.
#' @noRd
.za_colour <- function(x, i, state = c("moist", "dry")) {
  state <- match.arg(state)
  .kidpr_memo(x, sprintf("za_col_%s_%d", state, i), function() {
    hue <- .kidpr_v(x, i, paste0("munsell_hue_", state))
    hue <- if (length(hue) != 1L || is.na(hue) || !nzchar(trimws(as.character(hue)))) NA_character_
           else toupper(gsub("[[:space:]]", "", as.character(hue)))
    hp <- if (!is.na(hue) && hue %in% .ZA_HUE_FAMILIES) list(number = NA_real_, family = hue) else .munsell_hue_parts(hue)
    v <- .za_num(x, i, paste0("munsell_value_", state))
    ch <- .za_num(x, i, paste0("munsell_chroma_", state))
    if (identical(hp$family, "N")) ch <- 0
    list(hue = hue, number = hp$number, family = hp$family, value = v, chroma = ch, state = state,
         missing = paste0(c("munsell_hue_", "munsell_value_", "munsell_chroma_"), state))
  })
}
.za_col_text <- function(col) {
  sprintf("%s %s/%s %s", ifelse(is.na(col$hue), "?", col$hue),
          ifelse(is.na(col$value), "?", format(col$value)),
          ifelse(is.na(col$chroma), "?", format(col$chroma)), col$state)
}

# Is a hue red (Q11)? TRUE, FALSE or NA (not a hue); `redder` TRUE when the
# hue is redder than the listed ones (read as red by red_redder_hues).
.za_hue_red <- function(col) {
  f <- col$family
  if (is.null(f) || is.na(f)) return(list(red = NA, redder = FALSE))
  if (f == "N") return(list(red = FALSE, redder = FALSE))
  if (is.na(col$number)) return(list(red = if (f %in% c("R", "YR", "RP")) NA else FALSE, redder = FALSE))
  p <- .rp_hue_pos(col)
  if (is.na(p)) return(list(red = NA, redder = FALSE))
  pos <- vapply(.za_thv("red_hues"), function(h) .rp_hue_pos(.munsell_hue_parts(h)), 0)
  if (p >= min(pos) - 1e-9 && p <= max(pos) + 1e-9) return(list(red = TRUE, redder = FALSE))
  if (p < min(pos) && f %in% c("R", "RP")) return(list(red = isTRUE(.za_thv("red_redder_hues")), redder = TRUE))
  list(red = FALSE, redder = FALSE)
}

# Is a colour (family, number, value, chroma) a gley hue or of low chroma
# (Q12 amended, D4): a gley hue (N, GY, G, BG, B, PB, 10Y), else a moist
# chroma of 2 or less at a moist value of 4 or more (the dark colours of FC 5
# are not grey). TRUE, FALSE or NA.
.za_is_low <- function(family, number = NA_real_, value = NA_real_, chroma = NA_real_) {
  gh <- as.character(unlist(.za_thv("gley_hues")))
  if (!is.na(family)) {
    for (g in gh) {
      if (g %in% c("N", .ZA_HUE_FAMILIES)) { if (family == g) return(TRUE) }
      else {
        hp <- .munsell_hue_parts(g)
        if (identical(hp$family, family) && isTRUE(abs(hp$number - number) < 1e-9)) return(TRUE)
      }
    }
  }
  vmin <- .za_thv("low_chroma_value")
  if (!is.na(value) && value < vmin) return(FALSE)
  if (!is.na(chroma) && chroma > .za_thv("low_chroma")) return(FALSE)
  if (is.na(chroma) || is.na(value)) return(NA)
  TRUE
}

#' A grey low-chroma matrix (Q12 amended): a gley hue, else moist chroma 2 or
#' less at a moist value of 4 or more; flagged.
#' @noRd
.za_low_chroma <- function(x, i) {
  m <- .za_colour(x, i, "moist")
  p <- .za_is_low(m$family, m$number, m$value, m$chroma)
  th <- c("low_chroma", "low_chroma_value", "gley_hues")
  if (is.na(p)) return(.za_r(NA, missing = c("munsell_chroma_moist", "munsell_value_moist", "munsell_hue_moist"),
                             note = "moist matrix colour not recorded: low chroma undecided", th = th))
  .za_r(p, note = sprintf("%s: %s", .za_col_text(m), if (p) "a grey low-chroma matrix" else "not low chroma"), th = th)
}

#' Red (Q11): the moist hue of the matrix one of 5YR, 2.5YR, 10R, 7.5R (or
#' redder), and not a grey low-chroma colour (5YR 6/1 is not red; the
#' review's D5); with no moist hue, the dry hue, flagged. A colour yellow dry
#' and red moist is red (S 32). The greater part of a horizon of several
#' layers is read by the part that merges them.
#' @noRd
.za_red <- function(x, i) {
  .kidpr_memo(x, sprintf("za_red_%d", i), function() {
    m <- .za_colour(x, i, "moist")
    use <- m; tag <- character(0); note <- character(0)
    if (is.na(m$hue)) {
      d <- .za_colour(x, i, "dry")
      if (is.na(d$hue)) return(.za_r(NA, missing = "munsell_hue_moist", note = "no hue recorded: red undecided"))
      use <- d; tag <- "dry_hue_fallback"
      note <- "no moist hue recorded: the dry hue read, flagged (Q11)"
    }
    h <- .za_hue_red(use)
    if (is.na(h$red)) return(.za_r(NA, missing = "munsell_hue_moist", note = sprintf("hue '%s' not read", use$hue)))
    hue <- .za_r(h$red, note = c(sprintf("%s hue %s: %s", use$state, use$hue, if (h$red) "a red hue" else "not red"), note),
                 tag = tag, th = c("red_hues", if (h$redder) "red_redder_hues"))
    if (!h$red) return(hue)
    .rp_all(hue, .za_not(.za_low_chroma(x, i), always = TRUE))
  })
}

#' Grey when dry (Q9): the dry part of the WRB albic material as soilKey
#' tests it (value 7 or more and chroma 3 or less, or value 5 or more and
#' chroma 2 or less), the moist colour not required; flagged. With the dry
#' colour not recorded, a moist chroma of 4 or more is not grey, flagged
#' (Hugo, 2026-11-05); else undecided.
#' @noRd
.za_grey_dry <- function(x, i) {
  .kidpr_memo(x, sprintf("za_grey_%d", i), function() {
    d <- .za_colour(x, i, "dry")
    pairs <- .za_thv("grey_dry")
    vmin <- vapply(pairs, function(p) as.numeric(p$value_min), 0)
    cmax <- vapply(pairs, function(p) as.numeric(p$chroma_max), 0)
    v <- d$value; ch <- d$chroma
    if (is.na(v) || is.na(ch)) {
      if ((!is.na(v) && v < min(vmin)) || (!is.na(ch) && ch > max(cmax)))
        return(.za_r(FALSE, note = sprintf("%s: not grey dry", .za_col_text(d)), th = "grey_dry"))
      m <- .za_colour(x, i, "moist")
      if (!is.na(m$chroma) && m$chroma >= .za_thv("grey_dry_moist_chroma"))
        return(.za_r(FALSE, tag = "grey_dry_from_moist", th = c("grey_dry", "grey_dry_moist_chroma"),
                     note = sprintf("dry colour not recorded; %s: not grey dry, flagged", .za_col_text(m))))
      return(.za_r(NA, missing = c("munsell_value_dry", "munsell_chroma_dry"),
                   note = "dry colour not recorded: grey undecided (Q9)", th = "grey_dry"))
    }
    p <- any(v >= vmin & ch <= cmax)
    .za_r(p, note = sprintf("%s: %s", .za_col_text(d), if (p) "grey dry" else "not grey dry"), th = "grey_dry")
  })
}

#' Yellow-brown (Q10): the moist hue 7.5YR or yellower (a YR or Y hue), the
#' moist chroma above the low-chroma limit (Q12) and the dry colour not grey
#' (Q9); flagged. Moist colour not recorded: undecided.
#' @noRd
.za_yellow_brown <- function(x, i) {
  m <- .za_colour(x, i, "moist")
  if (is.na(m$family)) return(.za_r(NA, missing = "munsell_hue_moist", note = "no moist hue recorded: yellow-brown undecided",
                                    th = "yellow_brown_hue"))
  lim <- .rp_hue_pos(.munsell_hue_parts(.za_thv("yellow_brown_hue")))
  p <- .rp_hue_pos(m)
  hue_ok <- m$family %in% c("YR", "Y") && !is.na(p) && p >= lim - 1e-9
  hue <- .za_r(hue_ok, note = sprintf("moist hue %s: %s", m$hue, if (hue_ok) "7.5YR or yellower" else "not 7.5YR or yellower"),
               th = "yellow_brown_hue")
  if (!hue_ok) return(hue)
  ch <- if (is.na(m$chroma)) .za_r(NA, missing = "munsell_chroma_moist", note = "moist chroma not recorded")
        else .za_r(m$chroma > .za_thv("low_chroma"), th = "low_chroma",
                   note = sprintf("moist chroma %s: %s", format(m$chroma), if (m$chroma > .za_thv("low_chroma")) "above the low chroma" else "low"))
  .rp_all(hue, ch, .za_not(.za_low_chroma(x, i), always = TRUE), .za_not(.za_grey_dry(x, i), always = TRUE))
}

#' Signs of wetness (Q12; FC 15): a grey low-chroma matrix, or low-chroma
#' mottles on 5 % or more of the layer (redoximorphic_features_pct); flagged.
#' Redoximorphic features recorded without their colours leave it undecided.
#' @noRd
.za_wetness <- function(x, i) {
  .kidpr_memo(x, sprintf("za_wet_%d", i), function() {
    lc <- .za_low_chroma(x, i)
    if (isTRUE(lc$passed)) return(.za_r(TRUE, note = c(lc$note, "signs of wetness"), tag = lc$tag))
    mot <- .munsell_notations(as.character(.kidpr_v(x, i, "mottle_colors")))
    rf <- .za_num(x, i, "redoximorphic_features_pct")
    thr <- .za_thv("wetness_signs")
    low_m <- if (nrow(mot)) any(vapply(seq_len(nrow(mot)), function(k)
      isTRUE(.za_is_low(mot$family[k], mot$number[k], mot$value[k], mot$chroma[k])), NA)) else FALSE
    mo <- if (low_m) {
      if (is.na(rf)) .za_r(NA, missing = "redoximorphic_features_pct", note = "low-chroma mottles, their extent not recorded",
                           th = "wetness_signs")
      else .za_r(rf >= thr, note = sprintf("low-chroma mottles on %s %% of the layer", format(rf)), th = "wetness_signs")
    } else if (!nrow(mot) && isTRUE(rf >= thr)) {
      .za_r(NA, missing = "mottle_colors", note = sprintf("redoximorphic features on %s %%, their colours not recorded", format(rf)),
            th = "wetness_signs")
    } else .za_r(FALSE, note = "no low-chroma mottles recorded")
    .rp_any(lc, mo)
  })
}


# --- structure and the other B properties ---------------------------------------------

# The structure of a layer: its type, grade (0-3), and what it is.
.za_struct <- function(x, i) {
  st <- .kidpr_structure(x, i)
  g <- .field_grade_class(st$grade)
  massive <- isTRUE(grepl("massive|single|structureless|apedal", st$type)) || identical(g, 0L)
  if (massive && is.na(g)) g <- 0L
  blocky <- isTRUE(grepl("block|angular|nut|polyhed|cub", st$type))
  prism <- isTRUE(grepl("prism|column", st$type))
  list(type = st$type, grade_text = st$grade, grade = g, massive = massive, blocky = blocky, prism = prism,
       borderline = isTRUE(grepl("weak", st$grade) && grepl("moderate", st$grade)),
       other = !is.na(st$type) && !massive && !blocky && !prism)
}
.za_struct_text <- function(s) {
  trimws(paste(ifelse(is.na(s$grade_text), "", s$grade_text), ifelse(is.na(s$type), "", s$type)))
}
# The highest grade an apedal B may have (apedal_max_grade, V 21).
.za_apedal_grade <- function() .field_grade_class(.za_thv("apedal_max_grade"))

#' The CEC under 11 cmol(+)/kg soil of a borderline apedal B (V 21; default
#' 16): by ammonium acetate at pH 7; another method stands in, flagged.
#' @noRd
.za_cec_low <- function(x, i) {
  cec <- .za_num(x, i, "cec_cmol")
  thr <- .za_thv("apedal_cec")
  if (is.na(cec)) return(.za_r(NA, missing = "cec_cmol", note = "borderline structure (weak to moderate), CEC not recorded"))
  meth <- .za_txt(x, i, "cec_method")
  tag <- "borderline_cec"; note <- character(0)
  if (is.na(meth)) note <- "cec_method not recorded: the CEC read as by ammonium acetate at pH 7"
  else if (meth != "acetate_ph7") {
    tag <- c(tag, "cec_method_substitute")
    note <- sprintf("CEC by %s stands in for the ammonium acetate CEC, flagged (default 16)", meth)
  }
  .za_r(cec < thr, note = c(sprintf("borderline structure: CEC %s cmol(+)/kg soil (%s, V 21)", format(cec),
                                    if (cec < thr) sprintf("under %s: apedal", format(thr)) else sprintf("%s or more: read as moderate", format(thr))),
                            note), tag = tag, th = "apedal_cec")
}

#' Apedal (V 21; S 32): a structure weaker than moderate blocky or prismatic
#' when moist; borderline (weak to moderate), a CEC under 11 cmol(+)/kg soil.
#' Structure not recorded: undecided (Hugo, 2026-11-05).
#' @noRd
.za_apedal <- function(x, i) {
  s <- .za_struct(x, i)
  txt <- .za_struct_text(s)
  if (s$massive) return(.za_r(TRUE, note = sprintf("structure %s: no visible structure", txt)))
  if (s$borderline) return(.za_cec_low(x, i))
  if (!is.na(s$grade)) {
    if (s$grade <= .za_apedal_grade()) return(.za_r(TRUE, note = sprintf("structure %s: weaker than moderate (V 21)", txt)))
    if (s$blocky || s$prism) return(.za_r(FALSE, note = sprintf("structure %s: moderate or strong blocky or prismatic", txt)))
    if (is.na(s$type)) return(.za_r(NA, missing = "structure_type", note = sprintf("structure %s, type not recorded", txt)))
    return(.za_r(TRUE, note = sprintf("structure %s: not blocky or prismatic", txt)))
  }
  if (s$blocky || s$prism) return(.za_r(NA, missing = "structure_grade", note = sprintf("structure %s, grade not recorded", txt)))
  if (s$other) return(.za_r(TRUE, note = sprintf("structure %s: not blocky or prismatic", txt)))
  .za_r(NA, missing = c("structure_grade", "structure_type"), note = "structure not recorded: apedal undecided")
}

#' A grade of moderate or more: a borderline grade (weak to moderate) with a
#' CEC of 11 cmol(+)/kg soil or more counts as moderate, flagged; without the
#' CEC, undecided (the review's D7).
#' @noRd
.za_moderate <- function(x, i) {
  s <- .za_struct(x, i)
  if (s$massive) return(.za_r(FALSE, note = sprintf("structure %s", .za_struct_text(s))))
  if (s$borderline) return(.za_not(.za_cec_low(x, i), always = TRUE))
  if (is.na(s$grade)) return(.za_r(NA, missing = "structure_grade", note = "structure grade not recorded"))
  .za_r(s$grade > .za_apedal_grade(), note = sprintf("structure %s%s", .za_struct_text(s),
                                                      if (s$grade > .za_apedal_grade()) "" else ": weaker than moderate"))
}

# A moderate or strong blocky structure (red structured and pedocutanic B).
.za_blocky <- function(x, i) {
  s <- .za_struct(x, i)
  txt <- .za_struct_text(s)
  if (s$massive) return(.za_r(FALSE, note = sprintf("structure %s", txt)))
  if (!s$blocky) {
    if (is.na(s$type)) return(.za_r(NA, missing = c("structure_type", if (is.na(s$grade)) "structure_grade"),
                                     note = "structure type not recorded"))
    return(.za_r(FALSE, note = sprintf("structure %s: not blocky", txt)))
  }
  .rp_all(.za_moderate(x, i), .za_r(TRUE, note = "blocky"))
}

#' Calcareous (Q22; FC 16): effervescence recorded (read as with cold 10 %
#' HCl): none, nil, 0 or '-' (and non-effervescent) not calcareous, a
#' degree of effervescence calcareous, any other word undecided (the review's
#' D9); not recorded, CaCO3 1 % or more, flagged, unless the carbonates may
#' be relict nodules (FC 16: undecided); neither recorded, not calcareous (an
#' observation not recorded is absent).
#' @noRd
.za_calcareous <- function(x, i) {
  neg <- "^(none|no|nil|0|-|absent|non|not|noneffervescent|non effervescent)( |$|-)"
  pos <- "^(very slight|slight|weak|moderate|strong|very strong|violent|effervescent|calcareous|yes|present|\\+)"
  for (a in c("effervescence_class", "effervescence_extent")) {
    ef <- .za_txt(x, i, a)
    if (is.na(ef)) next
    if (grepl(neg, ef) || ef %in% c("0", "-")) return(.za_r(FALSE, note = sprintf("effervescence %s", ef)))
    if (grepl(pos, ef) || grepl("^(local|continuous)", ef)) return(.za_r(TRUE, note = sprintf("effervescence %s", ef)))
    return(.za_r(NA, missing = a, note = sprintf("effervescence '%s' not read", ef)))
  }
  cc <- .za_num(x, i, "caco3_pct")
  if (!is.na(cc)) {
    p <- cc >= .za_thv("calcareous_caco3")
    hn <- .za_num(x, i, "carbonate_hard_pct")
    if (p && isTRUE(hn > 0))
      return(.za_r(NA, missing = "effervescence_class", th = "calcareous_caco3",
                   note = sprintf("CaCO3 %s %% with hard carbonate nodules: they may be relict nodules in a non-calcareous matrix (FC 16)", format(cc))))
    return(.za_r(p, note = sprintf("no effervescence recorded: CaCO3 %s %% (Q22)", format(cc)), th = "calcareous_caco3"))
  }
  .za_r(FALSE, note = "no effervescence or CaCO3 recorded: not calcareous")
}

#' Stratified (S 32-33): cross bedding, or a stratification recorded (not a
#' lithological break alone); not recorded, absent.
#' @noRd
.za_stratified <- function(x, i) {
  if (isTRUE(.za_lgl(x, i, "cross_bedding"))) return(.za_r(TRUE, note = "cross bedding recorded"))
  sp <- .za_txt(x, i, "stratification_pattern")
  if (!is.na(sp) && !grepl("^(none|absent|no|lithologic)", sp))
    return(.za_r(TRUE, note = sprintf("stratification %s recorded", sp)))
  .za_r(FALSE, note = "no stratification recorded")
}

# The mottles and concretions of a layer, % (the largest of the
# redoximorphic features and plinthite plus ferric nodules), NA when none is
# recorded.
.za_mottles <- function(x, i) {
  pn <- c(.za_num(x, i, "plinthite_pct"), .za_num(x, i, "ferric_nodules_pct"))
  .za_max(.za_num(x, i, "redoximorphic_features_pct"), if (any(!is.na(pn))) sum(pn, na.rm = TRUE))
}

#' Mottles and concretions of 10 % or less (S 32-33): more makes the soft
#' plinthic B. Not recorded, absent.
#' @noRd
.za_few_mottles <- function(x, i) {
  rf <- .za_num(x, i, "redoximorphic_features_pct")
  pn <- c(.za_num(x, i, "plinthite_pct"), .za_num(x, i, "ferric_nodules_pct"))
  vals <- c(rf, if (any(!is.na(pn))) sum(pn, na.rm = TRUE))
  if (all(is.na(vals))) return(.za_r(TRUE, note = "no mottles or concretions recorded"))
  m <- max(vals, na.rm = TRUE)
  thr <- .za_thv("plinthic_mottles")
  .za_r(m <= thr, note = sprintf("mottles and concretions %s %% (plinthic over %s %%)", format(m), format(thr)),
        th = "plinthic_mottles")
}

#' The mottles of an apedal B (S 32): allowed when not enough for a soft
#' plinthic B, read as 10 % or less, or more when the layer is not a soft
#' plinthic B by R3 (Hugo, 2026-11-06).
#' @noRd
.za_apedal_mottles <- function(x, i) {
  f <- .za_few_mottles(x, i)
  if (!isFALSE(f$passed)) return(f)
  sp <- .za_t_SPL(x, i)
  if (isFALSE(sp$passed)) return(.za_r(TRUE, note = c(f$note, "more than 10 % mottles, but not a soft plinthic B (R3)")))
  .za_not(sp, "mottles of a soft plinthic B", always = TRUE)
}

#' A uniform colour: no variegated colours recorded; for a red horizon,
#' variegated colours that are all red are allowed (red cutans, S 31).
#' @noRd
.za_uniform <- function(x, i, red = FALSE) {
  vc <- .munsell_notations(as.character(.kidpr_v(x, i, "variegated_colors")))
  if (!nrow(vc)) return(.za_r(TRUE, note = "no variegated colours recorded"))
  txt <- as.character(.kidpr_v(x, i, "variegated_colors"))
  if (red) {
    r <- vapply(seq_len(nrow(vc)), function(k) .za_hue_red(list(family = vc$family[k], number = vc$number[k]))$red, NA)
    if (all(r %in% TRUE)) return(.za_r(TRUE, note = sprintf("variegated colours %s, all red", txt), th = "red_hues"))
    if (any(r %in% FALSE)) return(.za_r(FALSE, note = sprintf("variegated colours %s: not uniformly red", txt), th = "red_hues"))
    return(.za_r(NA, missing = "variegated_colors", note = sprintf("variegated colours %s not read", txt)))
  }
  .za_r(FALSE, note = sprintf("variegated colours %s: not uniform", txt))
}

# The nearest layer above a B that is not part of it (`same(j)` TRUE for the
# layers of the same B): the material above, its lower boundary the
# transition into the B; and the uppermost layer of the B (the B1, Q20).
.za_above_b <- function(x, i, same) {
  a <- rev(.kidpr_above(x, i))
  b1 <- i
  for (j in a) {
    if (isTRUE(same(j))) b1 <- j else return(list(above = j, b1 = b1))
  }
  list(above = NA_integer_, b1 = b1)
}
#' Is the lower boundary of a layer abrupt? Abrupt, very abrupt, sharp, the
#' abbreviations a and va, or a word with abrupt in it (smooth abrupt):
#' TRUE; clear, gradual, diffuse (c, g, d): FALSE; not recorded or another
#' word: NA.
#' @noRd
.za_abrupt <- function(x, j) {
  if (is.na(j)) return(NA)
  b <- .za_txt(x, j, "boundary_distinctness")
  if (is.na(b)) return(NA)
  if (grepl("abrupt|sharp", b) || b %in% c("a", "va")) return(TRUE)
  if (grepl("clear|gradual|diffuse", b) || b %in% c("c", "g", "d")) return(FALSE)
  NA
}
.za_abrupt_text <- function(x, j) if (is.na(j)) "no layer above" else sprintf("transition '%s'", .za_txt(x, j, "boundary_distinctness"))


# --- the topsoil horizons (T) ------------------------------------------------------------

#' The surface run: the first layer by depth and the layers that continue it
#' as topsoil (a topsoil name read in diagnostic_za or the designation, else
#' an A, O or H designation, AP and AH in capitals too; not a transition such
#' as AB). A first layer named or designated a subsoil starts no run (a
#' truncated profile). An A2 under an A1 (the older notation) is a candidate
#' E: it continues the run only when it is not grey dry (Hugo, 2026-11-05).
#' @noRd
.za_surface_run <- function(x) {
  .kidpr_memo(x, "za_surface", function() {
    if (!x$n) return(integer(0))
    voc <- .za_vocabulary()
    tok <- if (!is.null(x$names)) x$names$token else rep(NA_character_, x$n)
    run <- integer(0)
    x$za_a2 <- integer(0)
    for (k in seq_along(x$ord)) {
      i <- x$ord[k]
      t <- tok[i]
      d <- .za_des(x, i)
      top_like <- if (!is.na(t) && nzchar(t)) voc$info[t, "group"] == "topsoil"
                  else if (!is.na(d)) grepl("^[0-9]*(A([a-z0-9]|[PH][0-9]*$|$)|O([a-z0-9]|[IEAP][0-9]*$|$)|H([a-z0-9]|$))", d)
                  else NA
      if (k > 1L && isTRUE(top_like) && (is.na(t) || !nzchar(t)) && isTRUE(grepl("^[0-9]*A2", d)) &&
          any(grepl("^[0-9]*A1", vapply(run, function(j) .za_des(x, j) %||% "", "")))) {
        g <- .za_grey_dry(x, i)$passed
        x$za_a2 <- c(x$za_a2, i)
        if (!isFALSE(g)) break
      }
      if (k == 1L) {
        # v0.9.357: any surface horizon is a topsoil, so a surface layer
        # designated C starts the run too (Hugo, 2026-11-06)
        if (isFALSE(top_like) && (is.na(t) || !nzchar(t)) && identical(.za_master(x, i), "C") &&
            !isTRUE(grepl("^[0-9]*C[a-z]*r", d))) top_like <- TRUE
        if (isFALSE(top_like)) break
        run <- i
      } else if (isTRUE(top_like)) run <- c(run, i) else break
    }
    run
  })
}

# The mineral A layers of the surface run (no O or H designation, no
# organic O name).
.za_mineral_run <- function(x) {
  run <- .za_surface_run(x)
  tok <- if (!is.null(x$names)) x$names$token else rep(NA_character_, x$n)
  run[vapply(run, function(j) !isTRUE(.za_master(x, j) %in% c("O", "H")) && !identical(tok[j], "OO"), NA)]
}

#' Organic O (i): an average of 10 % organic carbon or more over 200 mm from
#' the top of the layer (thickness-weighted over the layers it spans). A layer
#' of rock counts 0 %. A layer whose carbon is not recorded is bounded by the
#' nearest recorded carbon above it when it is designated B, C, E or G or lies
#' under a layer with under 10 % (flagged; the review's D3), else by 58 %.
#' @noRd
.za_oo_oc <- function(x, i) {
  depth <- .za_thv("organic_o_depth") / 10
  thr <- .za_thv("organic_o_oc")
  t0 <- x$top[i]
  if (is.na(t0)) return(.za_r(NA, missing = "top_cm"))
  end <- t0 + depth
  known <- 0; unknown <- 0; covered <- 0; bound <- 0; last_oc <- NA_real_; tag <- character(0)
  for (j in rev(.kidpr_above(x, i))) {
    oc <- .za_num(x, j, "oc_pct")
    if (!is.na(oc)) { last_oc <- oc; break }
  }
  for (j in x$ord[x$rank >= x$rank[i]]) {
    a <- max(x$top[j], t0); b <- min(x$bot[j], end)
    if (is.na(a) || is.na(b) || b <= a) next
    th <- b - a
    oc <- .za_num(x, j, "oc_pct")
    if (is.na(oc) && isTRUE(grepl(.WRB_ROCK_DESIGNATION, .za_des(x, j)))) oc <- 0
    if (is.na(oc)) {
      unknown <- unknown + th
      ub <- if (!is.na(last_oc) && (isTRUE(.za_master(x, j) %in% c("B", "C", "E", "G")) || last_oc < thr)) {
              tag <- "oc_bound_from_above"; last_oc
            } else 58
      bound <- bound + ub * th
    } else { known <- known + oc * th; last_oc <- oc }
    covered <- covered + th
  }
  short <- max(0, depth - covered)
  lo <- known / depth
  hi <- (known + bound + 58 * short) / depth
  note <- sprintf("organic carbon over the %s mm from %s cm: %s", format(depth * 10), format(t0),
                  if (unknown + short > 1e-9) sprintf("at least %s %%", format(round(lo, 2))) else sprintf("%s %%", format(round(lo, 2))))
  if (length(tag)) note <- c(note, "a layer with no carbon recorded bounded by the carbon above it, flagged")
  if (lo >= thr) return(.za_r(TRUE, note = note))
  if (unknown + short <= 1e-9 || hi < thr) return(.za_r(FALSE, note = note, tag = tag))
  .za_r(NA, missing = c(if (unknown > 0) "oc_pct", if (short > 0) "bottom_cm"), note = note, tag = tag)
}

#' Organic O (ii), saturated for long periods unless drained (Q18):
#' water_saturation_days when recorded (30 days or more, the WRB histic
#' horizon standing in); else its evidence, flagged: the site artificially
#' drained, a water table within the layer or the next one, signs of wetness
#' in a layer that starts within 50 cm below the O (organic_o_wetness_cm);
#' none of it, undecided.
#' @noRd
.za_oo_saturation <- function(x, i) {
  ws <- .za_num(x, i, "water_saturation_days")
  if (!is.na(ws)) return(.za_r(ws >= .za_thv("organic_o_saturation_days"),
                               note = sprintf("saturated %s days a year", format(ws)), th = "organic_o_saturation_days"))
  ev <- "saturation_by_evidence"
  if (isTRUE(.rp_site_lgl(x, "artificially_drained")))
    return(.za_r(TRUE, tag = ev, note = "the site is artificially drained ('unless drained'), flagged (Q18)"))
  gw <- suppressWarnings(as.numeric(.kidpr_site(x, "groundwater_depth_cm")))
  nx <- .kidpr_next(x, i)
  lim <- if (!is.na(nx)) x$bot[nx] else x$bot[i]
  if (!is.na(gw) && !is.na(lim) && gw <= lim)
    return(.za_r(TRUE, tag = ev, note = sprintf("a water table at %s cm, in the layer or the next one, flagged (Q18)", format(gw))))
  win <- .za_thv("organic_o_wetness_cm")
  for (j in .kidpr_below(x, i)) {
    if (is.na(x$top[j]) || is.na(x$bot[i]) || x$top[j] >= x$bot[i] + win) next
    w <- .za_wetness(x, j)
    if (isTRUE(w$passed))
      return(.za_r(TRUE, tag = c(ev, w$tag), th = "organic_o_wetness_cm",
                   note = sprintf("signs of wetness in the subsoil within %s cm (layer %d; T-organic (ii)), flagged (Q18)", format(win), j)))
  }
  .za_r(NA, missing = "water_saturation_days", note = "no saturation and no evidence of it recorded: undecided (Q18)")
}

#' Organic O horizon (T-organic): (i) and (ii).
#' @noRd
.za_t_OO <- function(x, i) .rp_all(.za_oo_oc(x, i), .za_oo_saturation(x, i))

# The slickensides of a layer, or of a transition to the layer below: at or
# over vertic_slickensides (common) TRUE, less or none FALSE (not recorded:
# absent), recorded with no amount NA.
.za_slickensides <- function(x, i) {
  lev <- c(.za_thv("vertic_slickensides"), "continuous")
  one <- function(j) {
    v <- .kidpr_v(x, j, "slickensides")
    if (length(v) != 1L || is.na(v)) return(list(p = FALSE, recorded = FALSE, text = "not recorded"))
    if (is.logical(v)) return(list(p = if (v) NA else FALSE, recorded = TRUE, text = if (v) "present" else "absent"))
    t <- .field_term(as.character(v))
    if (.amount_at_least(t, lev)) return(list(p = TRUE, recorded = TRUE, text = t))
    if (!is.na(.clay_films_class(t)) || grepl("^(false|no)$", t)) return(list(p = FALSE, recorded = TRUE, text = t))
    list(p = NA, recorded = TRUE, text = t)
  }
  r <- one(i)
  nx <- .kidpr_next(x, i)
  if (!isTRUE(r$p) && !is.na(nx) && isTRUE(grepl("^[0-9]*(A[BC]|[BC]A|A/[BC]|[BC]/A)", .za_des(x, nx)))) {
    r2 <- one(nx)
    if (isTRUE(r2$p)) return(list(p = TRUE, recorded = TRUE, text = sprintf("%s in the transition below", r2$text)))
  }
  r
}

# The plasticity index and its limit: 32 by the Casagrande cup, 36 by the
# British Standard cone; the method not recorded, the cup, flagged.
.za_pi <- function(x, i) {
  pi <- .za_num(x, i, "plasticity_index")
  m <- .za_txt(x, i, "liquid_limit_method")
  cone <- isTRUE(grepl("cone", m))
  list(value = pi, limit = .za_thv(if (cone) "plasticity_index_cone" else "plasticity_index_cup"),
       tag = if (is.na(m) && !is.na(pi)) "liquid_limit_default" else character(0),
       note = if (is.na(m) && !is.na(pi)) "liquid_limit_method not recorded: the Casagrande cup, flagged (default 18)" else character(0))
}

#' Vertic A (ii): slickensides regularly occurring (common or more, Q17), or
#' a plasticity index over 32 (cup) or 36 (cone). Without the plasticity
#' index the slickensides decide, flagged. Slickensides recorded with no
#' amount leave it undecided unless the plasticity index passes (the review's
#' D1).
#' @noRd
.za_vertic_ii <- function(x, i) {
  sl <- .za_slickensides(x, i)
  pi <- .za_pi(x, i)
  if (isTRUE(sl$p)) return(.za_r(TRUE, note = sprintf("slickensides %s", sl$text), th = "vertic_slickensides"))
  if (!is.na(pi$value)) {
    pp <- pi$value > pi$limit
    pnote <- c(sprintf("plasticity index %s (vertic over %s)", format(pi$value), format(pi$limit)), pi$note)
    if (!pp && is.na(sl$p)) return(.za_r(NA, missing = "slickensides", tag = pi$tag,
                                         note = c(pnote, sprintf("slickensides %s, amount not recorded", sl$text))))
    return(.za_r(pp, note = pnote, tag = pi$tag))
  }
  if (is.na(sl$p)) return(.za_r(NA, missing = c("slickensides", "plasticity_index"),
                                note = sprintf("slickensides %s, amount not recorded, no plasticity index", sl$text)))
  .za_r(FALSE, tag = "slickensides_decide", th = "vertic_slickensides",
        note = sprintf("no plasticity index: the slickensides (%s) decide, not vertic, flagged (Q17)", sl$text))
}

#' Vertic A horizon (T-vertic): (i) strong structure, (ii).
#' @noRd
.za_t_VA <- function(x, i) {
  s <- .za_struct(x, i)
  st <- if (is.na(s$grade)) .za_r(NA, missing = "structure_grade", note = "structure grade not recorded")
        else .za_r(s$grade >= .field_grade_class(.za_thv("vertic_structure_grade")),
                   note = sprintf("structure %s", .za_struct_text(s)))
  .rp_all(st, .za_vertic_ii(x, i))
}

#' The bases per kg clay per 1 % organic carbon (T-humic (ii), T-melanic
#' (iv)): the exchangeable Ca + Mg + K + Na (cmol(+)/kg soil) x 100 / clay %
#' / organic carbon %. A base not recorded leaves a lower bound; no base at
#' all, the base saturation times the CEC, flagged. A high EC is flagged
#' (soluble cations; default 10).
#' @noRd
.za_base_ratio <- function(x, i) {
  b <- vapply(c("ca_cmol", "mg_cmol", "k_cmol", "na_cmol"), function(a) .za_num(x, i, a), 0)
  tag <- character(0); note <- character(0); th <- "base_ratio"
  if (all(is.na(b))) {
    bs <- .za_num(x, i, "bs_pct"); cec <- .za_num(x, i, "cec_cmol")
    if (is.na(bs) || is.na(cec)) return(list(low = NA_real_, exact = FALSE, missing = c("ca_cmol", "mg_cmol", "k_cmol", "na_cmol"),
                                             tag = tag, note = "exchangeable bases not recorded", th = th))
    s <- bs * cec / 100; exact <- TRUE
    tag <- "bases_from_bs"; note <- "the bases from the base saturation and the CEC, flagged"
  } else {
    s <- sum(b, na.rm = TRUE); exact <- all(!is.na(b))
    if (!exact) note <- sprintf("%s not recorded: a lower bound", paste(names(b)[is.na(b)], collapse = ", "))
  }
  clay <- .za_num(x, i, "clay_pct"); oc <- .za_num(x, i, "oc_pct")
  miss <- c(if (is.na(clay) || isTRUE(clay <= 0)) "clay_pct", if (is.na(oc)) "oc_pct")
  if (length(miss)) return(list(low = NA_real_, exact = FALSE, missing = miss, tag = tag, note = note, th = th))
  ec <- .za_num(x, i, "ec_dS_m")
  if (!is.na(ec) && ec >= .za_thv("ec_high")) {
    tag <- c(tag, "ec_high_soluble_salts")
    note <- c(note, sprintf("EC %s dS/m: soluble cations may count among the bases, flagged (default 10)", format(ec)))
    th <- c(th, "ec_high")
  }
  low <- if (oc > 0) s * 100 / clay / oc else Inf
  list(low = low, exact = exact, missing = if (exact) character(0) else names(b)[is.na(b)], tag = tag,
       note = c(sprintf("%s%s cmol(+)/kg clay per 1 %% organic carbon", if (exact) "" else "at least ", format(round(low, 2))), note),
       th = th)
}
.za_ratio_test <- function(x, i, high) {
  r <- .za_base_ratio(x, i)
  thr <- .za_thv("base_ratio")
  p <- if (is.na(r$low)) NA
       else if (r$low >= thr) high
       else if (r$exact) !high
       else NA
  .za_r(p, missing = r$missing, note = r$note, tag = r$tag, th = r$th)
}

#' Humic A (i): more than 1.8 % organic carbon in some part of the mineral A
#' (the A layers of the surface run that holds the layer; a litter O on top
#' does not count).
#' @noRd
.za_ha_oc <- function(x, i) {
  run <- .za_mineral_run(x)
  ls <- if (i %in% run) run else i
  oc <- vapply(ls, function(j) .za_num(x, j, "oc_pct"), 0)
  thr <- .za_thv("humic_oc")
  p <- if (any(oc > thr, na.rm = TRUE)) TRUE else if (all(!is.na(oc))) FALSE else NA
  .za_r(p, missing = "oc_pct", note = sprintf("organic carbon of the mineral A up to %s %% (humic over %s %%)",
                                               if (all(is.na(oc))) "?" else format(max(oc, na.rm = TRUE)), format(thr)))
}

# The first layer below the topsoil (the surface run), NA when none.
.za_first_subsoil <- function(x) {
  run <- .za_surface_run(x)
  if (!length(run)) return(NA_integer_)
  .kidpr_next(x, run[length(run)])
}

#' Does a layer hold one of the horizons a humic A may not directly overlie
#' (T-humic (iv): G, E, placic pan, podzol B, soft or hard plinthic B,
#' prismacutanic B, red structured B, any material with signs of wetness)?
#' A name written for the layer decides; else the data: the red structured
#' and prismacutanic B and the signs of wetness by the tests of this part,
#' the E by its grey dry colour unless the designation says otherwise, the
#' podzol B, the placic pan and the hard plinthic B by their marks (the
#' designation, soilKey's spodic() standing in for the podzol B), the soft
#' plinthic B by more than 10 % mottles. The next part tests these fully.
#' @noRd
.za_excluded_below <- function(x, j) {
  excl <- c("G", "E", "PLP", "PZB", "SPL", "HPL", "PRC", "RSB", "UMW", "USW")
  nm <- x$names
  if (!is.null(nm) && identical(nm$route[j], "notation")) {
    t <- nm$token[j]; cand <- nm$candidates[[j]]
    if (!is.na(t)) return(.za_r(t %in% excl, note = sprintf("the layer below is written %s", .za_label(t))))
    if (length(cand)) {
      p <- if (all(cand %in% excl)) TRUE else if (any(cand %in% excl)) NA else FALSE
      return(.za_r(p, missing = "diagnostic_za", note = sprintf("the layer below is written as %s", paste(cand, collapse = " or "))))
    }
  }
  if (.za_is_rock(x, j)) return(.za_r(FALSE, note = "rock below the topsoil"))
  hint <- if (!is.null(nm)) nm$hint[[j]] else character(0)
  has_hint <- length(hint) > 0L
  e <- if (has_hint && !"E" %in% hint) .za_r(FALSE, note = sprintf("'%s' is no E", .za_des(x, j)))
       else .za_grey_dry(x, j)
  pz <- if ("PZB" %in% hint) .za_r(TRUE, tag = "designation_hint", note = sprintf("'%s': a podzol B", .za_des(x, j)))
        else if (isTRUE(.kidpr_wrb_layer(x, "spodic", j))) .za_r(TRUE, tag = "spodic", note = "soilKey's spodic() stands in for the podzol B, flagged")
        else .za_r(FALSE, note = "no podzol B mark (Bh, Bs) and spodic() not passing")
  cem <- .za_txt(x, j, "cementation_class")
  hp <- if ("HPL" %in% hint || "PLP" %in% hint) .za_r(TRUE, tag = "designation_hint", note = sprintf("'%s': a cemented iron pan", .za_des(x, j)))
        else if (isTRUE(grepl("indurat|strong", cem)) && isTRUE((.za_num(x, j, "plinthite_pct") %||% 0) > 0 || (.za_num(x, j, "ferric_nodules_pct") %||% 0) > 0))
          .za_r(TRUE, note = "an indurated layer with plinthite or ferric nodules: a hard plinthic B")
        else .za_r(FALSE, note = "no iron pan recorded")
  sp <- .za_not(.za_few_mottles(x, j), "more than 10 % mottles: a soft plinthic B")
  .rp_any(.za_t_RSB(x, j), .za_t_PRC(x, j), .za_wetness(x, j), e, pz, hp, sp)
}

#' Humic A (iv), the position: the first subsoil is none of those of
#' .za_excluded_below(). Nothing below the topsoil: TRUE.
#' @noRd
.za_ha_position <- function(x) {
  .kidpr_memo(x, "za_ha_iv", function() {
    j <- .za_first_subsoil(x)
    if (is.na(j)) return(.za_r(TRUE, note = "nothing recorded below the topsoil"))
    r <- .za_excluded_below(x, j)
    .za_not(r, sprintf("T-humic (iv) on the layer below the topsoil (layer %d)", j), always = TRUE)
  })
}

#' Humic A horizon (T-humic): (i) over 1.8 % organic carbon in some part; (ii)
#' under 4 cmol(+) bases per kg clay per 1 % organic carbon; (iii) no stripped
#' grains (uncoated_grains_pct 25 % or more; not recorded, absent; default
#' 8); with pass 2, (iv) the position (default 7, the two passes).
#' @noRd
.za_t_HA <- function(x, i, pass = 2L) {
  ug <- .za_num(x, i, "uncoated_grains_pct")
  iii <- if (is.na(ug)) .za_r(TRUE, note = "no stripped grains recorded")
         else .za_r(ug < .za_thv("uncoated_grains"), note = sprintf("uncoated grains %s %%", format(ug)), th = "uncoated_grains")
  .rp_all(.za_ha_oc(x, i), .za_ratio_test(x, i, high = FALSE), iii, if (pass >= 2L) .za_ha_position(x))
}

#' Melanic A (i): dry value and chroma 3 or less, not 10YR 3/3 (the hue
#' compared by number: 10.0YR is 10YR), the hue not 5YR or redder. With no
#' dry hue, the moist hue stands in, flagged. Dry value or chroma not
#' recorded: undecided, the moist colour + 1 value given as evidence only
#' (default 9); but a moist value over 3 with no dry value refuses the
#' colour, flagged, since the dry value is never darker (M1, Hugo,
#' 2026-11-10).
#' @noRd
.za_ma_colour <- function(x, i) {
  d <- .za_colour(x, i, "dry")
  tag <- character(0); note <- character(0)
  if (is.na(d$family)) {
    m <- .za_colour(x, i, "moist")
    if (!is.na(m$family)) {
      d$hue <- m$hue; d$family <- m$family; d$number <- m$number
      tag <- "dry_hue_from_moist"; note <- sprintf("no dry hue recorded: the moist hue %s stands in, flagged", m$hue)
    }
  }
  vmax <- .za_thv("melanic_dry_value"); cmax <- .za_thv("melanic_dry_chroma")
  red_lim <- .rp_hue_pos(.munsell_hue_parts(.za_thv("melanic_excluded_hue")))
  redish <- function(col) {
    if (is.na(col$family)) return(NA)
    if (col$family == "N") return(FALSE)
    if (is.na(col$number)) return(if (col$family %in% c("R", "RP")) TRUE else if (col$family == "YR") NA else FALSE)
    p <- .rp_hue_pos(col)
    if (is.na(p)) NA else col$family %in% c("R", "RP", "YR") && p <= red_lim + 1e-9
  }
  fails <- c(if (!is.na(d$value) && d$value > vmax) "value", if (!is.na(d$chroma) && d$chroma > cmax) "chroma",
             if (isTRUE(redish(d))) "hue")
  if (length(fails)) return(.za_r(FALSE, tag = tag, note = c(sprintf("%s: not melanic (%s)", .za_col_text(d), paste(fails, collapse = ", ")), note)))
  if (is.na(d$value) || is.na(d$chroma) || is.na(redish(d))) {
    m <- .za_colour(x, i, "moist")
    # M1 (Hugo, 2026-11-10): no dry value and a moist value over the dry
    # limit: the dry value is never darker than the moist, so the melanic
    # colour is refused, flagged; a moist value of 3 or less stays undecided
    # (default 9)
    if (is.na(d$value) && !is.na(m$value) && m$value > vmax)
      return(.za_r(FALSE, tag = c(tag, "dry_value_from_moist"),
                   note = c(sprintf("no dry value recorded; moist %s: the dry value is never darker than the moist, so over %s: not melanic, flagged (M1)",
                                    .za_col_text(m), format(vmax)), note)))
    ev <- if (!is.na(m$value)) sprintf("evidence only: the moist %s gives a dry value of about %s (default 9)",
                                       .za_col_text(m), format(min(10, m$value + 1))) else character(0)
    return(.za_r(NA, missing = c("munsell_value_dry", "munsell_chroma_dry", "munsell_hue_dry"),
                 tag = c(tag, if (length(ev)) "dry_from_moist_evidence"),
                 note = c("dry colour not recorded: melanic colour undecided", ev, note)))
  }
  ex <- strsplit(.za_thv("melanic_excluded_colour"), "[ /]")[[1]]
  exh <- .munsell_hue_parts(ex[1])
  if (identical(d$family, exh$family) && isTRUE(abs(d$number - exh$number) < 1e-9) &&
      isTRUE(d$value == as.numeric(ex[2])) && isTRUE(d$chroma == as.numeric(ex[3])))
    return(.za_r(FALSE, tag = tag, note = c(sprintf("%s: the excluded 10YR 3/3", .za_col_text(d)), note)))
  .za_r(TRUE, tag = tag, note = c(sprintf("%s: dark", .za_col_text(d)), note))
}

#' Melanic A (ii) and (iii): no slickensides diagnostic of vertic and a
#' plasticity index of 32 or less (cup) or 36 or less (cone). Without the
#' plasticity index the slickensides decide, flagged (Q17); slickensides
#' recorded with no amount leave it undecided (the review's D1).
#' @noRd
.za_ma_not_vertic <- function(x, i) {
  sl <- .za_slickensides(x, i)
  pi <- .za_pi(x, i)
  if (isTRUE(sl$p)) return(.za_r(FALSE, note = sprintf("slickensides %s: vertic", sl$text), th = "vertic_slickensides"))
  if (!is.na(pi$value)) {
    pp <- pi$value <= pi$limit
    pnote <- c(sprintf("plasticity index %s (melanic %s or less)", format(pi$value), format(pi$limit)), pi$note)
    if (pp && is.na(sl$p)) return(.za_r(NA, missing = "slickensides", tag = pi$tag,
                                        note = c(pnote, sprintf("slickensides %s, amount not recorded", sl$text))))
    return(.za_r(pp, note = pnote, tag = pi$tag))
  }
  if (is.na(sl$p)) return(.za_r(NA, missing = c("slickensides", "plasticity_index"),
                                note = sprintf("slickensides %s, amount not recorded, no plasticity index", sl$text)))
  .za_r(TRUE, tag = "slickensides_decide", th = "vertic_slickensides",
        note = sprintf("no plasticity index: the slickensides (%s) decide, not vertic, flagged (Q17)", sl$text))
}

#' Melanic A horizon (T-melanic): (i) colour, (ii)-(iii) not vertic, (iv) 4
#' cmol(+) or more bases per kg clay per 1 % organic carbon, (v) less carbon
#' than an organic O, (vi) the greater part not both massive and hard or very
#' hard dry (the rupture resistance not recorded: not hard).
#' @noRd
.za_t_MA <- function(x, i) {
  oo <- .za_oo_oc(x, i)
  v <- if (!is.na(oo$passed)) .za_not(oo, "less carbon than an organic O")
       else {
         oc <- .za_num(x, i, "oc_pct")
         if (!is.na(oc) && oc < .za_thv("organic_o_oc")) .za_r(TRUE, note = sprintf("organic carbon %s %%: less than an organic O", format(oc)))
         else .za_r(NA, missing = oo$missing)
       }
  s <- .za_struct(x, i)
  rr <- .za_txt(x, i, "rupture_resistance")
  hard <- !is.na(rr) && grepl("^(hard|very hard|extremely hard|rigid|indurat)", rr)
  vi <- if (s$massive && hard) .za_r(FALSE, note = sprintf("massive and %s dry", rr))
        else .za_r(TRUE, note = if (s$massive) sprintf("massive, rupture resistance %s", if (is.na(rr)) "not recorded (not hard)" else rr)
                                else "not massive")
  .rp_all(.za_ma_colour(x, i), .za_ma_not_vertic(x, i), .za_ratio_test(x, i, high = TRUE), v, vi)
}

#' Orthic A horizon (T-orthic): a topsoil that is none of the other four.
#' Undecided when one of them is, with them as candidates.
#' @noRd
.za_t_OA <- function(x, i) {
  rs <- list(OO = .za_t_OO(x, i), VA = .za_t_VA(x, i), MA = .za_t_MA(x, i), HA = .za_t_HA(x, i, 2L))
  v <- vapply(rs, function(r) r$passed, NA)
  if (any(v %in% TRUE)) {
    k <- names(v)[which(v %in% TRUE)[1L]]
    return(.rp_r(FALSE, note = sprintf("the layer is the %s", .za_label(k))))
  }
  if (all(v %in% FALSE)) {
    h1 <- .za_t_HA(x, i, 1L)
    note <- "none of organic O, vertic, melanic or humic A (T-orthic)"
    if (isTRUE(h1$passed)) note <- c(note, "a humic A that directly overlies a horizon T-humic (iv) excludes falls to orthic (default 7)")
    return(.rp_r(TRUE, note = note))
  }
  na <- names(v)[is.na(v)]
  .rp_r(NA, missing = unique(unlist(lapply(rs[na], `[[`, "missing"))),
        tag = unique(unlist(lapply(rs[na], `[[`, "tag"))),
        note = sprintf("undecided: %s may hold", paste(na, collapse = ", ")), value = list(candidates = na))
}

#' The topsoil of a layer of the surface run, by the fixed order organic O,
#' vertic A, melanic A, humic A, orthic A (default 7): the first that holds;
#' an undecided one before it leaves the layer undecided with both as
#' candidates.
#' @noRd
.za_topsoil <- function(x, i) {
  x <- .za_x(x)
  tests <- list(OO = function() .za_t_OO(x, i), VA = function() .za_t_VA(x, i), MA = function() .za_t_MA(x, i),
                HA = function() .za_t_HA(x, i, 2L), OA = function() .za_t_OA(x, i))
  und <- character(0); answers <- list()
  for (k in names(tests)) {
    r <- tests[[k]]()
    answers[[k]] <- r
    if (isTRUE(r$passed)) {
      if (length(und)) return(list(code = NA_character_, candidates = c(und, k), answers = answers))
      return(list(code = k, candidates = character(0), answers = answers))
    }
    if (is.na(r$passed) && k != "OA") und <- c(und, k)
  }
  list(code = NA_character_, candidates = unique(c(und, "OA")), answers = answers)
}


# --- the apedal and structured B horizons (S 31-32; V 21-29) ----------------------------

#' Red apedal B (S 32; V 21): red (Q11), apedal, a uniform colour, mottles of
#' 10 % or less, not calcareous, not stratified.
#' @noRd
.za_t_RAB <- function(x, i) {
  if (.za_is_rock(x, i)) return(.za_rock_no())
  .rp_all(.za_red(x, i), .za_apedal(x, i), .za_uniform(x, i), .za_apedal_mottles(x, i),
          .za_not(.za_calcareous(x, i), always = TRUE), .za_not(.za_stratified(x, i), always = TRUE))
}

#' Yellow-brown apedal B (S 32; V 21): yellow-brown (Q10: not red, not low
#' chroma, not grey dry), apedal, a uniform colour, mottles of 10 % or less,
#' not calcareous, not stratified.
#' @noRd
.za_t_YAB <- function(x, i) {
  if (.za_is_rock(x, i)) return(.za_rock_no())
  red <- .za_red(x, i)
  if (isTRUE(red$passed)) return(.za_not(red, "red, not yellow-brown", always = TRUE))
  .rp_all(.za_yellow_brown(x, i), .za_apedal(x, i), .za_uniform(x, i), .za_apedal_mottles(x, i),
          .za_not(.za_calcareous(x, i), always = TRUE), .za_not(.za_stratified(x, i), always = TRUE))
}

# Is a layer of the same moderate or strong blocky B?
.za_same_blocky <- function(x) function(j) isTRUE(.za_blocky(x, j)$passed)

#' The red B with cutans of another colour (S 31; the review's D2): TRUE
#' when the cutans differ in colour from the red matrix (variegated colours
#' not all red, or coatings of another hue or value) or the transition from
#' the material above is abrupt (the red pedocutanic B); FALSE when the
#' colour is uniform and the transition recorded not abrupt (the red
#' structured B); NA when that transition is not recorded. Shared by the two
#' tests, so that both stay candidates when it is undecided.
#' @noRd
.za_red_cutanic <- function(x, i) {
  .kidpr_memo(x, sprintf("za_redcut_%d", i), function() {
    var <- .za_uniform(x, i, red = TRUE)
    m <- .za_colour(x, i, "moist")
    cv <- .za_num(x, i, "coatings_value_moist")
    ch <- .za_txt(x, i, "coatings_hue_moist")
    chp <- if (is.na(ch)) list(family = NA, number = NA) else .munsell_hue_parts(ch)
    contrast <- (!is.na(cv) && !is.na(m$value) && abs(cv - m$value) >= 1) ||
                (!is.na(chp$family) && !is.na(m$family) && !(identical(chp$family, m$family) && isTRUE(abs(chp$number - m$number) < 1e-9)))
    if (isFALSE(var$passed) || contrast) return(.za_r(TRUE, note = "cutans of another colour than the red matrix (S 31)"))
    ab <- .za_above_b(x, i, .za_same_blocky(x))
    a <- .za_abrupt(x, ab$above)
    if (isTRUE(a)) return(.za_r(TRUE, note = sprintf("%s: abrupt into a red B (S 31)", .za_abrupt_text(x, ab$above))))
    if (isFALSE(a)) return(.za_r(FALSE, note = sprintf("uniformly red, %s: not abrupt", .za_abrupt_text(x, ab$above))))
    .za_r(NA, missing = "boundary_distinctness",
          note = "uniformly red, the transition from above not recorded: red pedocutanic or red structured B")
  })
}

#' Red structured B (S 31): red, moderate or strong blocky, uniformly red
#' (red cutans allowed), the transition from the material above recorded
#' not abrupt (.za_red_cutanic() FALSE; not recorded, undecided). More than
#' 35 % clay is usual (evidence only); under orthic A only (the position,
#' next part).
#' @noRd
.za_t_RSB <- function(x, i) {
  if (.za_is_rock(x, i)) return(.za_rock_no())
  red <- .za_red(x, i)
  if (isFALSE(red$passed)) return(red)
  bl <- .za_blocky(x, i)
  if (isFALSE(bl$passed)) return(bl)
  cl <- .za_num(x, i, "clay_pct")
  ev <- if (!is.na(cl)) sprintf("clay %s %% (usually over %s %%, S 31: evidence only)", format(cl), format(.za_thv("red_structured_clay")))
  r <- .rp_all(red, bl, .za_not(.za_red_cutanic(x, i), always = TRUE))
  r$note <- unique(c(r$note, ev))
  r
}

# Is a layer of the same prismacutanic B: prismatic or columnar, or a layer
# designated B (a blocky B1 over a columnar B2 is one B).
.za_same_prc <- function(x) function(j) isTRUE(.za_struct(x, j)$prism) || identical(.za_master(x, j), "B")

# The change of clay into a prismatic B: the B1 (Q20) against the material
# directly above the B (Q21), and the transition between them.
.za_prc_change <- function(x, i) {
  ab <- .za_above_b(x, i, .za_same_prc(x))
  if (is.na(ab$above)) return(.za_r(FALSE, note = "no material above the B"))
  ca <- .za_num(x, ab$above, "clay_pct"); cb <- .za_num(x, ab$b1, "clay_pct")
  rule <- .za_thv("prismacutanic_clay")
  cl <- if (is.na(ca) || is.na(cb)) .za_r(NA, missing = "clay_pct", note = "clay of the B or of the material above not recorded")
        else {
          p <- if (ca < rule$low$above_lt) cb >= rule$low$factor * ca else cb >= ca + rule$high$plus
          .za_r(p, th = "prismacutanic_clay",
                note = sprintf("clay %s %% above, %s %% in the B1: %s", format(ca), format(cb),
                               if (p) "the textural change of a prismacutanic B" else "not enough increase (S 32)"))
        }
  a <- .za_abrupt(x, ab$above)
  tr <- if (isFALSE(a)) .za_r(FALSE, note = sprintf("%s: not abrupt", .za_abrupt_text(x, ab$above)))
        else .za_r(TRUE, note = if (is.na(a)) "transition not recorded: the change of clay read as the abrupt change" else "abrupt transition")
  .rp_all(cl, tr)
}

#' Prismacutanic B (S 32): prismatic or columnar; the very abrupt change of
#' texture from the A or E (under 20 % clay above, twice as much; 20 % or
#' more, 20 points more; Q21); no signs of wetness, or with them uniformly
#' dark cutans (coatings value 3 or less, flagged). The colour contrast of
#' cutans and ped interiors is noted, not tested.
#' @noRd
.za_t_PRC <- function(x, i) {
  if (.za_is_rock(x, i)) return(.za_rock_no())
  s <- .za_struct(x, i)
  st <- if (s$prism) .za_r(TRUE, note = sprintf("structure %s", .za_struct_text(s)))
        else if (is.na(s$type)) .za_r(NA, missing = "structure_type", note = "structure type not recorded")
        else .za_r(FALSE, note = sprintf("structure %s: not prismatic or columnar", .za_struct_text(s)))
  if (isFALSE(st$passed)) return(st)
  wet <- .za_wetness(x, i)
  cv <- .za_num(x, i, "coatings_value_moist")
  dark <- if (isFALSE(wet$passed)) .za_r(TRUE, note = "no signs of wetness")
          else {
            cd <- if (is.na(cv)) .za_r(NA, missing = "coatings_value_moist", note = "signs of wetness: the colour of the cutans not recorded")
                  else .za_r(cv <= .za_thv("prismacutanic_dark_cutans"), th = "prismacutanic_dark_cutans",
                             note = sprintf("signs of wetness; cutans of moist value %s", format(cv)))
            if (isTRUE(wet$passed)) cd else .rp_any(.za_not(wet, always = TRUE), cd)
          }
  mv <- .za_colour(x, i, "moist")$value
  con <- if (!is.na(cv) && !is.na(mv)) sprintf("colour contrast of cutans (value %s) and ped interiors (value %s), S 32: noted, not tested",
                                                format(cv), format(mv))
         else "colour contrast of cutans and ped interiors not recorded (S 32: not tested)"
  r <- .rp_all(st, .za_prc_change(x, i), dark)
  r$note <- unique(c(r$note, con))
  r
}

#' Pedocutanic B (S 31; K 53-55): moderate or strong blocky (prismatic when
#' the transition from above is not the abrupt change of a prismacutanic B);
#' prominent cutans on most ped faces (clay films common or more; not
#' recorded, soilKey's argic() stands in, flagged, but only one way: argic
#' not holding leaves the cutans undecided, as the pedocutanic B needs no
#' clay increase; C1, Hugo, 2026-11-10); for a red B, the cutans
#' giving variegated colours or an abrupt transition from above (S 31,
#' .za_red_cutanic()), else it is the red structured B.
#' @noRd
.za_t_PCU <- function(x, i) {
  if (.za_is_rock(x, i)) return(.za_rock_no())
  s <- .za_struct(x, i)
  st <- if (s$prism && !s$blocky) {
          ch <- .za_prc_change(x, i)
          .rp_all(.za_moderate(x, i), .za_not(ch, "prismatic: allowed when the transition is not that of a prismacutanic B (K 53-55)"))
        } else .za_blocky(x, i)
  if (isFALSE(st$passed)) return(st)
  cut <- .kidpr_cutans(x, i)
  # C1 (Hugo, 2026-11-10): clay films not recorded and soilKey's argic
  # horizon not holding is no evidence against cutans (S 31 asks no clay
  # increase): undecided. Only the decision by argic is read so (its tag,
  # and the layer not argic); the other refusals of .kidpr_cutans() (no
  # peds, carbonate neoformations, outside the zone of the clay maximum)
  # stand. Only here; .kidpr_cutans() is shared with other systems and stays
  # as it is.
  if (is.na(.clay_films_class(.kidpr_v(x, i, "clay_films_amount"))) && isFALSE(cut$passed) &&
      "argic" %in% cut$tag && isFALSE(.kidpr_wrb_layer(x, "argic", i)))
    cut <- .za_r(NA, missing = "clay_films_amount", tag = cut$tag,
                 note = c(cut$note, "clay films not recorded: the pedocutanic B needs no clay increase, so argic not holding leaves the cutans undecided (C1)"))
  red <- .za_red(x, i)
  col <- if (isFALSE(red$passed)) .za_r(TRUE, note = "not red: the cutans give the variegation")
         else {
           rc <- .za_red_cutanic(x, i)
           if (is.na(red$passed) && isFALSE(rc$passed)) .za_r(NA, missing = "munsell_hue_moist", note = rc$note) else rc
         }
  .rp_all(st, cut, col)
}

#' Lithocutanic B (S 31; K 31, 72): soil merging into weathering rock: rock
#' in place in the layer (bedrock_pct, else saprolite_pct, 10 % or more and
#' under 100 %, flagged), or a designation of a B with weathering rock (BCr,
#' B/R); a B-C transition with the rock not recorded is undecided. Not
#' dominated by carbonate (soilKey's calcic() or a calcrete pan: a carbonate
#' horizon, K 72). The cutans on the rock and the tongues of soil are noted,
#' not required (K 31). Lithocutanic against saprolite by position (default
#' 17, next part).
#' @noRd
.za_t_LCU <- function(x, i) {
  if (.za_is_rock(x, i)) return(.za_rock_no())
  d <- .za_des(x, i)
  br <- .za_num(x, i, "bedrock_pct")
  sp <- .za_num(x, i, "saprolite_pct")
  rock <- if (!is.na(br)) br else sp
  thr <- .za_thv("lithocutanic_rock")
  rk <- if (!is.na(rock)) {
          what <- if (!is.na(br)) "rock in place" else "saprolite"
          if (rock >= 100) .za_r(FALSE, note = sprintf("%s %s %%: rock, no soil", what, format(rock)))
          else .za_r(rock >= thr, th = "lithocutanic_rock", note = sprintf("%s %s %%", what, format(rock)))
        } else if (isTRUE(grepl("B", d) && grepl("Cr|/R|R/|BR", d)))
          .za_r(TRUE, tag = "designation_hint", note = sprintf("'%s': a B with weathering rock", d))
        else if (isTRUE(grepl("^[0-9]*(BC|CB|B/C|C/B)", d)))
          .za_r(NA, missing = "bedrock_pct", note = sprintf("'%s': the rock in it not recorded", d))
        else .za_r(FALSE, note = "no weathering rock recorded in the layer")
  if (isFALSE(rk$passed)) return(rk)
  pan <- .za_txt(x, i, "pan_type")
  carb <- if (isTRUE(grepl("calcrete|calcic|petrocalc", pan))) .za_r(FALSE, note = "a calcrete pan: a carbonate horizon (K 72)")
          else if (isTRUE(.kidpr_wrb_layer(x, "calcic", i))) .za_r(FALSE, tag = "calcic", note = "soilKey's calcic(): dominated by carbonate, a carbonate horizon (K 72)")
          else .za_r(TRUE, note = "not dominated by carbonate")
  cf <- .clay_films_class(.za_txt(x, i, "clay_films_amount"))
  ev <- if (isTRUE(cf >= 2L) || nrow(.munsell_notations(as.character(.kidpr_v(x, i, "variegated_colors")))))
          "cutans or variegated colours recorded" else "no cutans recorded (cutanic character may be minimal, K 31)"
  r <- .rp_all(rk, carb)
  r$note <- unique(c(r$note, ev))
  r
}

#' Neocarbonate B (S 32; P; Q6, Q16, Q22): calcareous, an apedal or
#' neocutanic structure (weaker than moderate), not stratified (a calcareous
#' stratified alluvium, regic sand or man-made deposit stays the material:
#' the neocarbonate B is an unstratified B, Q6). A soft carbonate horizon
#' comes first in the order (next part).
#' @noRd
.za_t_NCA <- function(x, i) {
  if (.za_is_rock(x, i)) return(.za_rock_no())
  .rp_all(.za_calcareous(x, i), .za_apedal(x, i), .za_not(.za_stratified(x, i), always = TRUE))
}

#' Neocutanic B (S 32; V 21): a structure weaker than moderate (borderline:
#' the CEC) with variegated colours or cutans (clay films few or more; not
#' recorded, absent), or a uniform weak structure in a colour that is
#' neither red nor yellow-brown nor of low chroma (the uniform dark brown of
#' S 32; the review's D8); not calcareous, no stratification left. A uniform
#' weak structure that is red or yellow-brown is an apedal B.
#' @noRd
.za_t_NCU <- function(x, i) {
  if (.za_is_rock(x, i)) return(.za_rock_no())
  var <- .za_not(.za_uniform(x, i), "variegated colours")
  cf <- .clay_films_class(.za_txt(x, i, "clay_films_amount"))
  cut <- if (is.na(cf)) .za_r(FALSE, note = "no clay films recorded") else .za_r(cf >= 2L, note = sprintf("clay films %s", .za_txt(x, i, "clay_films_amount")))
  s <- .za_struct(x, i)
  weak <- if (s$massive) .za_r(FALSE, note = "no visible structure")
          else if (s$borderline) .za_cec_low(x, i)
          else if (is.na(s$grade)) .za_r(NA, missing = "structure_grade", note = "structure grade not recorded")
          else .za_r(s$grade >= 1L && s$grade <= .za_apedal_grade(), note = sprintf("structure %s", .za_struct_text(s)))
  brown <- if (isFALSE(weak$passed)) weak
           else .rp_all(weak, .za_uniform(x, i),
                        .za_not(.za_red(x, i), always = TRUE),
                        .za_not(.za_yellow_brown(x, i), always = TRUE),
                        .za_not(.za_low_chroma(x, i), always = TRUE))
  if (isTRUE(brown$passed)) brown <- .za_r(TRUE, tag = c(brown$tag, "uniform_dark_brown"),
                                           note = c(brown$note, "uniform, weak, neither red nor yellow-brown: the uniform dark brown of S 32"))
  .rp_all(.za_apedal(x, i), .rp_any(var, cut, brown), .za_not(.za_calcareous(x, i), always = TRUE),
          .za_not(.za_stratified(x, i), always = TRUE))
}


# --- the predicates over a pedon ----------------------------------------------------------

# A test over the layers of a pedon as a DiagnosticResult: the topsoil tests
# over the surface run, the B tests over the layers below it, unless the
# layers are given.
.za_predicate <- function(pedon, name, test, layers = NULL, topsoil = FALSE) {
  x <- .za_x(pedon)
  if (is.null(layers)) {
    run <- .za_surface_run(x)
    layers <- if (topsoil) run else setdiff(x$ord, run)
  }
  ev <- list(); v <- logical(0); miss <- character(0); pass <- integer(0)
  for (i in layers) {
    r <- test(x, i)
    ev[[sprintf("layer_%d", i)]] <- list(passed = r$passed, layers = if (isTRUE(r$passed)) i else integer(0),
                                         missing = r$missing, note = r$note, tag = r$tag)
    v <- c(v, r$passed)
    if (isTRUE(r$passed)) pass <- c(pass, i)
    if (is.na(r$passed)) miss <- c(miss, r$missing)
  }
  passed <- if (any(v %in% TRUE)) TRUE else if (any(is.na(v))) NA else FALSE
  use <- if (isTRUE(passed)) names(ev)[v %in% TRUE] else names(ev)
  tags <- unique(unlist(lapply(ev[use], `[[`, "tag")))
  if (topsoil) .za_surface_run(x)
  if (topsoil && length(x$za_a2)) tags <- c(tags, "a2_old_notation")
  DiagnosticResult$new(name = name, passed = passed, layers = pass, evidence = ev,
                       missing = if (is.na(passed)) unique(miss) else character(0), reference = .ZA_REFERENCE,
                       notes = if (length(tags)) sprintf("flagged: %s", paste(tags, collapse = ", ")) else NA_character_)
}

#' The diagnostic horizons of the South African classification of 1991 on
#' the data (first part)
#'
#' Three-valued tests, one per horizon, over the layers of a pedon: the
#' topsoil horizons over the surface run, the B horizons over the layers
#' below it (or the `layers` given). Each returns a DiagnosticResult whose
#' evidence holds, per layer, the verdict, the missing attributes, notes and
#' the tags of the stand-ins used ("stand_in:<threshold>", "argic", ...).
#' @param pedon A PedonRecord.
#' @param layers Optional layer indices to test.
#' @param pass For the humic A: 1 tests (i)-(iii), 2 also the position (iv).
#' @noRd
organic_o_za <- function(pedon, layers = NULL) .za_predicate(pedon, "organic_o_za", .za_t_OO, layers, TRUE)
#' @noRd
humic_a_za <- function(pedon, layers = NULL, pass = 2L)
  .za_predicate(pedon, "humic_a_za", function(x, i) .za_t_HA(x, i, pass), layers, TRUE)
#' @noRd
vertic_a_za <- function(pedon, layers = NULL) .za_predicate(pedon, "vertic_a_za", .za_t_VA, layers, TRUE)
#' @noRd
melanic_a_za <- function(pedon, layers = NULL) .za_predicate(pedon, "melanic_a_za", .za_t_MA, layers, TRUE)
#' @noRd
orthic_a_za <- function(pedon, layers = NULL) .za_predicate(pedon, "orthic_a_za", .za_t_OA, layers, TRUE)
#' @noRd
red_apedal_b_za <- function(pedon, layers = NULL) .za_predicate(pedon, "red_apedal_b_za", .za_t_RAB, layers)
#' @noRd
yellow_brown_apedal_b_za <- function(pedon, layers = NULL) .za_predicate(pedon, "yellow_brown_apedal_b_za", .za_t_YAB, layers)
#' @noRd
red_structured_b_za <- function(pedon, layers = NULL) .za_predicate(pedon, "red_structured_b_za", .za_t_RSB, layers)
#' @noRd
neocutanic_b_za <- function(pedon, layers = NULL) .za_predicate(pedon, "neocutanic_b_za", .za_t_NCU, layers)
#' @noRd
neocarbonate_b_za <- function(pedon, layers = NULL) .za_predicate(pedon, "neocarbonate_b_za", .za_t_NCA, layers)
#' @noRd
pedocutanic_b_za <- function(pedon, layers = NULL) .za_predicate(pedon, "pedocutanic_b_za", .za_t_PCU, layers)
#' @noRd
prismacutanic_b_za <- function(pedon, layers = NULL) .za_predicate(pedon, "prismacutanic_b_za", .za_t_PRC, layers)
#' @noRd
lithocutanic_b_za <- function(pedon, layers = NULL) .za_predicate(pedon, "lithocutanic_b_za", .za_t_LCU, layers)


# =============================================================================
# v0.9.357 -- part 3: the other seventeen horizons and materials on the data
# (the E and G horizons and the wet materials, the podzol B and the placic
# pan, the soft and hard plinthic B, the soft and hardpan carbonate
# horizons, the dorbank, saprolite, hard rock, stratified alluvium, regic
# sand, the man-made soil deposit and the unconsolidated material), the
# position pass and the identification of every layer, .za_identify(), the
# registry namespace "za" of identify_horizons().
#
# Where no open source gives a number (podzol B, placic pan, dorbank, the
# carbonate horizons; Q13-Q16) the WRB numbers stand in, in thresholds.yaml,
# flagged. The fixed order of the tests over one layer and the readings of
# the position pass are in readings.yaml.
# =============================================================================

# --- shared readings ---------------------------------------------------------------

#' The cementation of a layer, 0 (none) to 4 (indurated), from
#' cementation_class; NA when not recorded. A rupture resistance of
#' indurated or rigid stands in for it, with a note.
#' @noRd
.za_cement <- function(x, i) {
  cc <- .za_txt(x, i, "cementation_class")
  lev <- if (is.na(cc)) NA_integer_
         else if (grepl("^(none|non|not|no|un)", cc)) 0L
         else if (grepl("^(weak|slight)", cc)) 1L
         else if (grepl("^moder", cc)) 2L
         else if (grepl("^strong", cc)) 3L
         else if (grepl("indurat|extrem", cc)) 4L
         else NA_integer_
  note <- character(0)
  if (is.na(cc)) {
    rr <- .za_txt(x, i, "rupture_resistance")
    if (!is.na(rr) && grepl("indurat|rigid", rr)) {
      lev <- 4L
      note <- sprintf("rupture resistance %s read as indurated", rr)
    }
  }
  list(level = lev, text = cc, note = note)
}
# The largest of some values, NA when none is recorded.
.za_max <- function(...) { v <- c(...); v <- v[!is.na(v)]; if (length(v)) max(v) else NA_real_ }
.za_cement_word <- function(lev) c("not cemented", "weakly cemented", "moderately cemented",
                                   "strongly cemented", "indurated")[lev + 1L]
# The level of a cementation word of thresholds.yaml.
.za_cement_level <- function(w) match(w, c("none", "weakly", "moderately", "strongly", "indurated")) - 1L

#' The cementing agent of a layer (Q14-Q16): from pan_type when recorded,
#' else from what the layer holds (duripan_pct, plinthite or ferric nodules,
#' CaCO3 of 15 % or more, the hints of the FAO letters Bkm, Bqm, Bsm, Bv);
#' one of rock (silcrete, P), carbonate, soft_carbonate (nodular or powder
#' calcrete, reading 4), silica, iron or other, or NA (several or none).
#' @noRd
.za_agent <- function(x, i) {
  pan <- .za_txt(x, i, "pan_type")
  if (!is.na(pan) && !grepl("^(none|no|absent)$", pan)) {
    a <- if (grepl("silcrete", pan)) "rock"
         else if (grepl("nodul|powder", pan) && grepl("calcrete|carbonate", pan)) "soft_carbonate"
         else if (grepl("calcrete|petrocalc|calcic|caliche|carbonate", pan)) "carbonate"
         else if (grepl("duripan|dorbank|silic|duric", pan)) "silica"
         else if (grepl("ferricrete|iron|laterit|ouklip|plinth|placic|ortstein|bog ore", pan)) "iron"
         else "other"
    return(list(agent = a, note = sprintf("pan type %s", pan)))
  }
  hint <- if (!is.null(x$names)) x$names$hint[[i]] else character(0)
  ev <- character(0)
  if (isTRUE(.za_num(x, i, "duripan_pct") >= .za_thv("dorbank_duripan"))) ev <- c(ev, "silica")
  if (isTRUE(.za_num(x, i, "plinthite_pct") > 0) || isTRUE(.za_num(x, i, "ferric_nodules_pct") > 0)) ev <- c(ev, "iron")
  if (isTRUE(.za_num(x, i, "caco3_pct") >= .za_thv("carbonate_caco3"))) ev <- c(ev, "carbonate")
  if (!length(ev) && length(hint)) {
    if (identical(sort(hint), "HCH")) ev <- "carbonate"
    else if (identical(hint, "DOR")) ev <- "silica"
    else if (all(hint %in% c("SPL", "HPL", "PLP"))) ev <- "iron"
  }
  ev <- unique(ev)
  if (length(ev) == 1L) return(list(agent = ev, note = sprintf("cementing agent read as %s from what the layer holds", ev)))
  list(agent = NA_character_, note = if (length(ev)) sprintf("cementing agent undecided (%s)", paste(ev, collapse = ", "))
                                     else "cementing agent not recorded")
}

#' A cemented layer of one agent, as the hardpan carbonate horizon, the
#' dorbank and the hard plinthic B test it: cemented at least `min_level`,
#' the agent `agent`, at least `min_cm` thick.
#' @noRd
.za_pan_test <- function(x, i, agent, min_level, min_cm = NA_real_, th = character(0), what) {
  cem <- .za_cement(x, i)
  ag <- .za_agent(x, i)
  pan_says <- identical(ag$agent, agent) && !is.na(.za_txt(x, i, "pan_type"))
  if (is.na(cem$level)) {
    if (pan_says) {
      lev <- .za_r(TRUE, tag = "cementation_from_pan_type",
                   note = c(sprintf("%s: a cemented pan, its cementation not recorded: the pan type counts as evidence, flagged (R4)", ag$note), cem$note))
    } else return(.za_r(FALSE, note = sprintf("no cementation recorded: not a %s", what)))
  } else {
    lev <- .za_r(cem$level >= min_level, th = th,
                 note = c(sprintf("%s (%s needs %s or more)", .za_cement_word(cem$level), what,
                                  .za_cement_word(min_level)), cem$note))
    if (isFALSE(lev$passed)) return(lev)
  }
  agr <- if (is.na(ag$agent)) .za_r(NA, missing = c("pan_type", "caco3_pct", "duripan_pct"), note = ag$note)
         else .za_r(ag$agent == agent, note = sprintf("%s: %s", ag$note, if (ag$agent == agent) sprintf("the agent of a %s", what)
                                                     else sprintf("not the agent of a %s", what)))
  if (isFALSE(agr$passed)) return(agr)
  thk <- if (is.na(min_cm)) NULL
         else if (is.na(x$thick[i])) .za_r(NA, missing = c("top_cm", "bottom_cm"), note = "thickness not recorded")
         else .za_r(x$thick[i] >= min_cm, th = th,
                    note = sprintf("%s cm thick (%s cm or more)", format(x$thick[i]), format(min_cm)))
  .rp_all(lev, agr, thk)
}

# Is the hint of the FAO letters of a layer this code (and only codes of `set`)?
.za_hint_has <- function(x, i, code) {
  !is.null(x$names) && code %in% (x$names$hint[[i]] %||% character(0))
}
.za_origin <- function(x, i) .za_txt(x, i, "layer_origin")
.ZA_AEOLIAN <- "aeol|eolian|eolic|aeolic|wind|dune"
.ZA_FLUVIAL <- "fluv|alluv|colluv"
.ZA_MADE <- "anthrop|man.?made|made ground|fill|spoil|technogen|dump|mine"


# --- the cemented and rock layers (P; S 31; Q14-Q16, Q19) --------------------------------

#' Man-made soil deposit (S 33; K 38): soil material deposited by people,
#' with or without rock fragments or artefacts: anthropic_material recorded,
#' a layer_origin that says so, artefacts of 20 % or more (the WRB
#' Technosols standing in, flagged) or the u and ^ of the FAO letters
#' (flagged). Not recorded, absent.
#' @noRd
.za_t_MMD <- function(x, i) {
  am <- .za_txt(x, i, "anthropic_material")
  if (!is.na(am) && !grepl("^(none|no|absent|natural)", am))
    return(.za_r(TRUE, note = sprintf("anthropic material %s", am)))
  lo <- .za_origin(x, i)
  if (!is.na(lo) && grepl(.ZA_MADE, lo)) return(.za_r(TRUE, note = sprintf("layer origin %s", lo)))
  af <- .za_max(.za_num(x, i, "artefacts_pct"), .za_num(x, i, "technic_hardmaterial_pct"))
  if (!is.na(af) && af >= .za_thv("man_made_artefacts"))
    return(.za_r(TRUE, tag = "artefacts", th = "man_made_artefacts",
                 note = sprintf("artefacts %s %%: a man-made deposit, flagged", format(af))))
  if (.za_hint_has(x, i, "MMD"))
    return(.za_r(TRUE, tag = "designation_hint", note = sprintf("'%s': a human-made layer by its letters, flagged", .za_des(x, i))))
  .za_r(FALSE, note = "no man-made material recorded")
}

#' Hard rock (S 31; K 73; P): continuous hard rock, a layer designated R (or
#' bedrock_pct of 100), or silcrete. Not recorded, absent.
#' @noRd
.za_t_HRK <- function(x, i) {
  if (.za_is_rock(x, i)) return(.za_r(TRUE, note = sprintf("'%s': rock", .za_des(x, i) %||% "bedrock 100 %")))
  ag <- .za_agent(x, i)
  if (identical(ag$agent, "rock")) return(.za_r(TRUE, note = sprintf("%s: hard rock (P; K 73)", ag$note)))
  .za_r(FALSE, note = "no hard rock recorded in the layer")
}

#' Hardpan carbonate horizon (P; K 72; Q16): cemented at least moderately by
#' carbonate (calcrete recorded, or 15 % CaCO3 or more), 10 cm or more: the
#' WRB petrocalcic horizon standing in, flagged.
#' @noRd
.za_t_HCH <- function(x, i) {
  if (.za_is_rock(x, i)) return(.za_r(FALSE, note = "a layer of rock"))
  .za_pan_test(x, i, "carbonate", .za_cement_level(.za_thv("hardpan_carbonate_cementation")),
               .za_thv("hardpan_carbonate_thickness"),
               th = c("hardpan_carbonate_cementation", "hardpan_carbonate_thickness"), what = "hardpan carbonate horizon")
}

#' Dorbank (V 32; K 45, 60, 66, 70; Q15): cemented at least moderately by
#' silica (duripan or dorbank recorded, or 10 % duripan material or more),
#' 10 cm or more: the WRB petroduric horizon standing in, flagged. Silcrete is
#' hard rock (P).
#' @noRd
.za_t_DOR <- function(x, i) {
  if (.za_is_rock(x, i)) return(.za_r(FALSE, note = "a layer of rock"))
  .za_pan_test(x, i, "silica", .za_cement_level(.za_thv("dorbank_cementation")), .za_thv("dorbank_thickness"),
               th = c("dorbank_cementation", "dorbank_thickness", "dorbank_duripan"), what = "dorbank")
}

#' Hard plinthic B (S 31; V 26): an indurated layer of iron and manganese
#' oxides that cannot be cut with a spade when wet, read as strongly
#' cemented or indurated by iron (ferricrete, ouklip, plinthite or ferric
#' nodules recorded; flagged), thicker than a placic pan.
#' @noRd
.za_t_HPL <- function(x, i) {
  if (.za_is_rock(x, i)) return(.za_r(FALSE, note = "a layer of rock"))
  r <- .za_pan_test(x, i, "iron", .za_cement_level(.za_thv("hard_plinthic_cementation")),
                    th = "hard_plinthic_cementation", what = "hard plinthic B")
  if (isFALSE(r$passed)) return(r)
  thk <- if (is.na(x$thick[i])) .za_r(NA, missing = c("top_cm", "bottom_cm"), note = "thickness not recorded")
         else .za_r(x$thick[i] >= .za_thv("placic_max_thickness"), th = "placic_max_thickness",
                    note = sprintf("%s cm thick: %s", format(x$thick[i]),
                                   if (x$thick[i] >= .za_thv("placic_max_thickness")) "thicker than a placic pan" else "a placic pan, not a hard plinthic B"))
  .rp_all(r, thk)
}

#' Placic pan (P; V 30-32; Q14): a layer 0.1 cm to under 2.5 cm thick,
#' cemented at least weakly by iron (with manganese or organic matter): the
#' WRB placic horizon standing in, flagged. Iron not recorded: a designation
#' with s and m (Bsm) says it, flagged; else undecided. Only under a podzol B
#' (the position pass).
#' @noRd
.za_t_PLP <- function(x, i) {
  if (.za_is_rock(x, i)) return(.za_r(FALSE, note = "a layer of rock"))
  cem <- .za_cement(x, i)
  if (is.na(cem$level) || cem$level < 1L)
    return(.za_r(FALSE, note = if (is.na(cem$level)) "no cementation recorded: not a placic pan" else "not cemented"))
  th <- c("placic_min_thickness", "placic_max_thickness")
  thk <- if (is.na(x$thick[i])) .za_r(NA, missing = c("top_cm", "bottom_cm"), note = "thickness not recorded", th = th)
         else .za_r(x$thick[i] >= .za_thv("placic_min_thickness") && x$thick[i] < .za_thv("placic_max_thickness"), th = th,
                    note = sprintf("%s cm thick (a placic pan: %s to under %s cm)", format(x$thick[i]),
                                   format(.za_thv("placic_min_thickness")), format(.za_thv("placic_max_thickness"))))
  if (isFALSE(thk$passed)) return(thk)
  ag <- .za_agent(x, i)
  agr <- if (identical(ag$agent, "iron")) .za_r(TRUE, note = ag$note, th = "placic_min_thickness")
         else if (!is.na(ag$agent)) .za_r(FALSE, note = sprintf("%s: not iron", ag$note))
         else if (.za_hint_has(x, i, "PLP")) .za_r(TRUE, tag = "designation_hint",
                                                   note = sprintf("'%s': a thin iron pan by its letters, flagged", .za_des(x, i)))
         else .za_r(NA, missing = "pan_type", note = ag$note)
  .rp_all(thk, agr)
}

#' Saprolite (S 31; K 26, 51, 55): weathering rock that keeps the
#' organisation of the parent rock: weathering_stage saprolite or
#' moderately (partly, highly) weathered, saprolite_pct or bedrock_pct of 50
#' % or more (soilKey's reading, flagged), or a C designated r (Cr), flagged.
#' Not a layer of rock.
#' @noRd
.za_t_SAP <- function(x, i) {
  if (.za_is_rock(x, i)) return(.za_r(FALSE, note = "hard rock, not saprolite"))
  ws <- .za_txt(x, i, "weathering_stage")
  if (!is.na(ws)) {
    if (grepl("saprolit|moderat|partly|partial|highly|strongly", ws))
      return(.za_r(TRUE, note = sprintf("weathering stage %s", ws)))
    if (grepl("fresh|slight|unweath|complete", ws))
      return(.za_r(FALSE, note = sprintf("weathering stage %s: not saprolite", ws)))
  }
  sp <- .za_max(.za_num(x, i, "saprolite_pct"), .za_num(x, i, "bedrock_pct"))
  if (!is.na(sp)) {
    p <- sp >= .za_thv("saprolite_rock")
    if (p || !.za_hint_has(x, i, "SAP"))
      return(.za_r(p, th = "saprolite_rock", note = sprintf("weathering rock in place %s %%", format(sp))))
  }
  d <- .za_des(x, i)
  if (isTRUE(grepl("^[0-9]*C[a-z]*r", d)))
    return(.za_r(TRUE, tag = "designation_hint", note = sprintf("'%s': weathered rock (Cr), flagged", d)))
  .za_r(FALSE, note = "no weathering rock recorded")
}

#' Soft carbonate horizon (P; K 72; Q16): not cemented (or weakly), 15 % CaCO3
#' or more with secondary carbonates of 5 % or more (soft masses, nodules;
#' secondary_carbonates_pct, carbonate_soft_pct, carbonate_hard_pct), 15 cm
#' or more: the WRB calcic horizon standing in, flagged; nodular or powder
#' calcrete recorded. Secondary carbonates not recorded: absent (an
#' observation), flagged in the note.
#' @noRd
.za_t_SCH <- function(x, i) {
  if (.za_is_rock(x, i)) return(.za_r(FALSE, note = "a layer of rock"))
  cem <- .za_cement(x, i)
  if (isTRUE(cem$level >= .za_cement_level(.za_thv("hardpan_carbonate_cementation"))))
    return(.za_r(FALSE, note = sprintf("%s: a hardpan, not soft", .za_cement_word(cem$level))))
  ag <- .za_agent(x, i)
  th <- c("carbonate_caco3", "soft_carbonate_secondary", "soft_carbonate_thickness")
  thk <- if (is.na(x$thick[i])) .za_r(NA, missing = c("top_cm", "bottom_cm"), note = "thickness not recorded")
         else .za_r(x$thick[i] >= .za_thv("soft_carbonate_thickness"), th = "soft_carbonate_thickness",
                    note = sprintf("%s cm thick", format(x$thick[i])))
  if (identical(ag$agent, "soft_carbonate")) return(.rp_all(.za_r(TRUE, note = ag$note), thk))
  cc <- .za_num(x, i, "caco3_pct")
  if (is.na(cc)) {
    ca <- .za_calcareous(x, i)
    if (isFALSE(ca$passed)) return(.za_r(FALSE, note = "not calcareous"))
    return(.za_r(NA, missing = "caco3_pct", note = "calcareous, CaCO3 not recorded", th = th))
  }
  c1 <- .za_r(cc >= .za_thv("carbonate_caco3"), th = "carbonate_caco3", note = sprintf("CaCO3 %s %%", format(cc)))
  if (isFALSE(c1$passed)) return(c1)
  sec <- .za_max(.za_num(x, i, "secondary_carbonates_pct"), .za_num(x, i, "carbonate_soft_pct"),
                 .za_num(x, i, "carbonate_hard_pct"))
  c2 <- if (is.na(sec)) .za_r(FALSE, th = th, tag = "secondary_carbonates_not_recorded",
                                   note = "no secondary carbonates recorded: not an accumulation (an observation not recorded is absent)")
        else .za_r(sec >= .za_thv("soft_carbonate_secondary"), th = "soft_carbonate_secondary",
                   note = sprintf("secondary carbonates %s %%", format(sec)))
  .rp_all(c1, c2, thk)
}


# --- the E, podzol B, plinthic and wet horizons (S 33; V 19-26; Q9, Q12, Q13) ---------

#' E horizon (S 33; V 19; Q9): grey when dry; not more clay than the layer
#' above it (the loss of colloidal matter; soilKey's reading); no cross
#' bedding or stratification (S 33); not cemented at least moderately; no
#' moderate or strong blocky or prismatic structure (a B). Directly under a
#' topsoil horizon or another E (the position pass).
#' @noRd
.za_t_E <- function(x, i) {
  if (.za_is_rock(x, i)) return(.za_r(FALSE, note = "a layer of rock"))
  g <- .za_grey_dry(x, i)
  if (isFALSE(g$passed) && !isTRUE(.za_low_chroma(x, i)$passed)) return(g)
  st <- .za_stratified(x, i)
  if (isTRUE(st$passed)) return(.za_r(FALSE, note = c(st$note, "stratified or cross-bedded: not an E (S 33)")))
  cem <- .za_cement(x, i)
  if (isTRUE(cem$level >= 2L)) return(.za_r(FALSE, note = sprintf("%s: not an E", .za_cement_word(cem$level))))
  s <- .za_struct(x, i)
  if ((s$blocky || s$prism) && isTRUE(s$grade > .za_apedal_grade()))
    return(.za_r(FALSE, note = sprintf("structure %s: a B, not an E", .za_struct_text(s))))
  dep <- .za_e_clay(x, i)
  if (isFALSE(g$passed) && !isFALSE(dep$passed)) {
    # D3: grey only by its moist colour (the WRB dry stand-in failing) and
    # eluvial: an E, flagged (Hugo, 2026-11-06)
    lc <- .za_low_chroma(x, i)
    if (isTRUE(lc$passed))
      g <- .za_r(TRUE, tag = "e_by_moist_colour", th = c("grey_dry", "low_chroma"),
                 note = c(g$note, "not grey dry by the WRB stand-in, but grey and of low chroma moist and eluvial: an E, flagged"))
  }
  .rp_all(g, dep)
}

#' The clay of an E (Hugo, 2026-11-06, reading R2): less clay than the layer
#' below, or at most 2 points more than the layer above.
#' @noRd
.za_e_clay <- function(x, i) {
  a <- .kidpr_prev(x, i); b <- .kidpr_next(x, i)
  if (is.na(a)) return(.za_r(FALSE, note = "nothing above: not an E"))
  ci <- .za_num(x, i, "clay_pct"); ca <- .za_num(x, a, "clay_pct")
  cb <- if (is.na(b)) NA_real_ else .za_num(x, b, "clay_pct")
  # nothing described below: no layer to have lost clay to
  if (is.na(ci)) return(.za_r(NA, missing = "clay_pct", note = "clay of the layer not recorded"))
  tol <- .za_thv("e_clay_above")
  up <- if (is.na(ca)) NA else ci <= ca + tol
  dn <- if (is.na(b)) FALSE else if (is.na(cb)) NA else ci < cb
  p <- .kidpr_or(up, dn)
  .za_r(p, missing = if (is.na(p)) "clay_pct" else character(0), th = "e_clay_above",
        note = sprintf("clay %s %% (above %s %%, below %s %%): %s", format(ci), if (is.na(ca)) "?" else format(ca),
                       if (is.na(cb)) "?" else format(cb),
                       if (isTRUE(p)) "eluvial" else if (isFALSE(p)) "not eluvial" else "undecided"))
}

# Illuvial colours of a podzol B, moist (the WRB spodic colours standing in,
# flagged): hue 7.5YR or redder with value and chroma within the first
# limits, or any hue dark within the second.
.za_podzol_colour <- function(x, i) {
  m <- .za_colour(x, i, "moist")
  lim <- .za_thv("podzol_colour")
  if (is.na(m$value) || is.na(m$chroma))
    return(.za_r(NA, missing = c("munsell_value_moist", "munsell_chroma_moist"), note = "moist colour not recorded", th = "podzol_colour"))
  p <- .rp_hue_pos(m)
  red <- !is.na(p) && m$family %in% c("R", "YR", "RP") && p <= .rp_hue_pos(.munsell_hue_parts(lim$hue_max)) + 1e-9
  ok <- (red && m$value <= lim$value_max && m$chroma <= lim$chroma_max) ||
        (m$value <= lim$dark_value_max && m$chroma <= lim$dark_chroma_max)
  if (!ok && is.na(m$family)) return(.za_r(NA, missing = "munsell_hue_moist", note = "moist hue not recorded", th = "podzol_colour"))
  .za_r(ok, th = "podzol_colour", note = sprintf("%s: %s", .za_col_text(m), if (ok) "illuvial colours" else "not the colours of a podzol B"))
}

#' Podzol B (P; V 30; Q13): the WRB spodic horizon as soilKey tests it,
#' standing in, flagged: an illuvial mark (a Bh, Bs or Bhs designation, or a
#' grey E directly above), 2.5 cm or more, pH(H2O) 5.9 or less, and Alox +
#' 1/2 Feox of 0.5 % or more. Without oxalate data the morphological route:
#' a Bh, Bs or Bhs designation with illuvial colours (pH over 5.9 or organic
#' carbon under 0.5 % recorded fail it), flagged; a grey E above with
#' illuvial colours and no oxalate data leaves it undecided.
#' @noRd
.za_t_PZB <- function(x, i, assume_mark = FALSE) {
  if (.za_is_rock(x, i)) return(.za_r(FALSE, note = "a layer of rock"))
  th <- c("podzol_ph", "podzol_alfe", "podzol_oc", "podzol_thickness")
  des <- .za_hint_has(x, i, "PZB")
  a <- .kidpr_prev(x, i)
  ga <- if (is.na(a) || assume_mark) FALSE else .za_grey_dry(x, a)$passed
  # v0.9.359 (FC 17): a layer read as if it lay directly beneath an A or E
  mark <- if (des) .za_r(TRUE, tag = "designation_hint", note = sprintf("'%s': an illuvial B by its letters", .za_des(x, i)))
          else if (assume_mark) .za_r(TRUE, note = "read as if directly beneath an A or E (FC 17)")
          else if (isTRUE(ga)) .za_r(TRUE, note = "a grey E-like layer directly above")
          else if (is.na(ga)) .za_r(NA, missing = c("munsell_value_dry", "munsell_chroma_dry"), note = "the colour of the layer above not recorded")
          else .za_r(FALSE, note = "no Bh or Bs designation and no grey layer directly above")
  if (isFALSE(mark$passed)) return(mark)
  ph <- .za_num(x, i, "ph_h2o"); oc <- .za_num(x, i, "oc_pct")
  al <- .za_num(x, i, "al_ox_pct"); fe <- .za_num(x, i, "fe_ox_pct")
  thk <- if (is.na(x$thick[i])) .za_r(NA, missing = c("top_cm", "bottom_cm"), note = "thickness not recorded")
         else .za_r(x$thick[i] >= .za_thv("podzol_thickness"), th = "podzol_thickness", note = sprintf("%s cm thick", format(x$thick[i])))
  phr <- if (is.na(ph)) NULL else .za_r(ph <= .za_thv("podzol_ph"), th = "podzol_ph", note = sprintf("pH(H2O) %s", format(ph)))
  if (!is.na(al) || !is.na(fe)) {
    v <- sum(c(al, 0.5 * fe), na.rm = TRUE)
    exact <- !is.na(al) && !is.na(fe)
    ch <- if (v >= .za_thv("podzol_alfe")) .za_r(TRUE, th = "podzol_alfe", note = sprintf("Alox + 1/2 Feox %s %%", format(round(v, 2))))
          else if (exact) .za_r(FALSE, th = "podzol_alfe", note = sprintf("Alox + 1/2 Feox %s %%: not spodic", format(round(v, 2))))
          else .za_r(NA, missing = c("al_ox_pct", "fe_ox_pct"), note = "Alox or Feox not recorded")
    if (is.null(phr)) phr <- .za_r(NA, missing = "ph_h2o", note = "pH(H2O) not recorded")
    ocx <- if (is.na(oc)) .za_r(NA, missing = "oc_pct", note = "organic carbon not recorded")
           else .za_r(oc >= .za_thv("podzol_oc"), th = "podzol_oc", note = sprintf("organic carbon %s %%", format(oc)))
    return(.rp_all(mark, ch, phr, ocx, thk))
  }
  # no oxalate data: the morphological route
  col <- .za_podzol_colour(x, i)
  if (isFALSE(col$passed)) return(col)
  if (isFALSE(phr$passed)) return(phr)
  ocr <- if (!is.na(oc)) .za_r(oc >= .za_thv("podzol_oc"), th = "podzol_oc", note = sprintf("organic carbon %s %%", format(oc)))
  if (isFALSE(ocr$passed)) return(ocr)
  if (!des) return(.za_r(NA, missing = c("al_ox_pct", "fe_ox_pct"), th = th,
                         note = c(mark$note, col$note, "no oxalate data and no Bh or Bs designation: podzol B undecided")))
  .rp_all(mark, col, phr, ocr, thk, .za_r(TRUE, tag = "podzol_b_morphology", th = th,
                                         note = "no oxalate data: the morphological route (designation and colours), flagged (Q13)"))
}

#' Soft plinthic B (S 33; V 25): more than 10 % mottles or concretions of iron
#' and manganese; grey colours of wetness in the horizon or directly below
#' (the signs of wetness, Q12); can be cut with a spade when wet (not
#' strongly cemented). The layer below not described leaves the grey colours
#' undecided when the layer shows none.
#' @noRd
.za_t_SPL <- function(x, i) {
  if (.za_is_rock(x, i)) return(.za_r(FALSE, note = "a layer of rock"))
  cem <- .za_cement(x, i)
  if (isTRUE(cem$level >= .za_cement_level(.za_thv("hard_plinthic_cementation"))))
    return(.za_r(FALSE, note = sprintf("%s: too hard for a soft plinthic B", .za_cement_word(cem$level))))
  mo <- .za_not(.za_few_mottles(x, i), "more than 10 % mottles and concretions")
  if (!isTRUE(mo$passed)) return(mo)
  w <- .za_wetness(x, i)
  if (!isTRUE(w$passed)) {
    nx <- .kidpr_next(x, i)
    w2 <- if (is.na(nx)) .za_r(NA, missing = "mottle_colors", note = "no grey colours recorded in the layer and nothing described below it")
          else {
            r <- .za_wetness(x, nx); r$note <- c(r$note, "the layer directly below")
            # R3 (Hugo, 2026-11-06): with no grey of its own, the B1 is soft
            # plinthic only with mottles at least those of the layer below
            mine <- .za_mottles(x, i); below <- .za_mottles(x, nx)
            if (isTRUE(r$passed) && !is.na(below) && !is.na(mine) && mine < below)
              r <- .za_r(FALSE, note = sprintf("no grey of its own and fewer mottles (%s %%) than the layer below (%s %%): not the soft plinthic B (K 33)",
                                               format(mine), format(below)))
            r
          }
    w <- .rp_any(w, w2)
  }
  .rp_all(mo, w)
}

#' A grey low-chroma matrix (the G horizon and the materials with signs of
#' wetness; Q12): the matrix of .za_low_chroma(). Which of the G horizon, the
#' unspecified or the unconsolidated material with signs of wetness it is
#' depends on what lies above (the position pass).
#' @noRd
.za_t_WET <- function(x, i) {
  if (.za_is_rock(x, i)) return(.za_r(FALSE, note = "a layer of rock"))
  .za_low_chroma(x, i)
}


# --- the transported and unconsolidated materials (S 32-33; P) -------------------------

#' Stratified alluvium (S 33; K 74): stratification recorded and a fluvial or
#' colluvial origin, or not an aeolian one (a stratified layer of unrecorded
#' origin, flagged); a fluvial or colluvial origin with no stratification
#' recorded, soilKey's fluvic_material() standing in, flagged. Cross bedding
#' alone in a layer not of fluvial origin is left to the regic sand.
#' @noRd
.za_t_SAL <- function(x, i) {
  if (.za_is_rock(x, i)) return(.za_r(FALSE, note = "a layer of rock"))
  lo <- .za_origin(x, i)
  fl <- !is.na(lo) && grepl(.ZA_FLUVIAL, lo)
  ae <- !is.na(lo) && grepl(.ZA_AEOLIAN, lo)
  sp <- .za_txt(x, i, "stratification_pattern")
  strat <- !is.na(sp) && !grepl("^(none|absent|no|lithologic)", sp)
  if (strat) {
    if (ae) return(.za_r(FALSE, note = sprintf("stratified, of aeolian origin (%s): not alluvium", lo)))
    if (fl) return(.za_r(TRUE, note = sprintf("stratification %s, origin %s", sp, lo)))
    return(.za_r(TRUE, tag = "origin_not_recorded", note = sprintf("stratification %s, origin not recorded: read as alluvium, flagged", sp)))
  }
  if (fl) {
    w <- .kidpr_wrb_layer(x, "fluvic_material", i)
    if (isTRUE(w)) return(.za_r(TRUE, tag = "fluvic_material", note = sprintf("origin %s; soilKey's fluvic_material() stands in for the stratification, flagged", lo)))
    return(.za_r(FALSE, note = sprintf("origin %s, no stratification recorded", lo)))
  }
  .za_r(FALSE, note = "no stratification or alluvial origin recorded")
}

#' Regic sand (S 33; P): a recent deposit, usually aeolian: an aeolian
#' origin, cross bedding or recent_deposit recorded; loamy sand or coarser
#' (the WRB Arenosol texture standing in, flagged); virtually structureless.
#' None of the marks recorded: absent (a sandy apedal B stays a B).
#' @noRd
.za_t_RSA <- function(x, i) {
  if (.za_is_rock(x, i)) return(.za_r(FALSE, note = "a layer of rock"))
  lo <- .za_origin(x, i)
  ae <- (!is.na(lo) && grepl(.ZA_AEOLIAN, lo)) || isTRUE(.za_lgl(x, i, "cross_bedding")) ||
        (isTRUE(.za_lgl(x, i, "recent_deposit")) && !(!is.na(lo) && grepl(.ZA_FLUVIAL, lo)))
  if (!ae) return(.za_r(FALSE, note = "no aeolian origin, cross bedding or recent deposit recorded"))
  sa <- .za_num(x, i, "sand_pct"); si <- .za_num(x, i, "silt_pct"); cl <- .za_num(x, i, "clay_pct")
  lim <- .za_thv("regic_sand_texture")
  tex <- if (is.na(cl) || (is.na(si) && is.na(sa))) .za_r(NA, missing = c("clay_pct", "silt_pct", "sand_pct"), note = "particle size not recorded")
         else {
           si2 <- if (!is.na(si)) si else 100 - sa - cl
           p <- si2 + 2 * cl < lim
           .za_r(p, th = "regic_sand_texture", note = sprintf("silt %s %%, clay %s %%: %s", format(si2), format(cl),
                                                             if (p) "loamy sand or coarser" else "finer than loamy sand"))
         }
  s <- .za_struct(x, i)
  st <- if (isTRUE(s$grade > .za_apedal_grade())) .za_r(FALSE, note = sprintf("structure %s: not virtually structureless", .za_struct_text(s)))
        else .za_r(TRUE, note = "virtually structureless")
  .rp_all(.za_r(TRUE, note = "an aeolian or recent deposit"), tex, st)
}

#' Unconsolidated material (S 32-33; P): a C layer (or the hint of its
#' letters) that is neither rock nor saprolite. With or without signs of
#' wetness, and whether it is the material of the key at all, by the
#' position pass (under a podzol or pedocutanic B). No designation:
#' undecided.
#' @noRd
.za_t_UMN <- function(x, i) {
  if (.za_is_rock(x, i)) return(.za_r(FALSE, note = "a layer of rock"))
  m <- .za_master(x, i)
  if (is.na(m) && !.za_hint_has(x, i, "UMN")) return(.za_r(NA, missing = "designation", note = "no designation: unconsolidated material undecided"))
  if (!identical(m, "C") && !.za_hint_has(x, i, "UMN")) return(.za_r(FALSE, note = sprintf("'%s': not a C layer", .za_des(x, i))))
  sap <- .za_t_SAP(x, i)
  if (isTRUE(sap$passed)) return(.za_r(FALSE, note = c(sap$note, "saprolite, not unconsolidated material")))
  .za_r(TRUE, note = sprintf("'%s': unconsolidated material", .za_des(x, i)))
}


# --- the identification of a layer and the position pass --------------------------------

# The fixed order of the tests over a layer below the topsoil (readings.yaml;
# soilKey's): the materials and cemented layers first, then the E, podzol B,
# plinthic and prismacutanic B, the lithocutanic B, a grey low-chroma matrix,
# the alluvium and regic sand, then the structured and apedal B horizons, and
# the unconsolidated material last. WET is a grey low-chroma matrix, the
# position pass makes it G, USW or UMW.
.ZA_ORDER <- c("MMD", "HRK", "HCH", "DOR", "HPL", "PLP", "SAP", "SCH", "E", "PZB", "SPL", "PRC", "LCU",
               "WET", "SAL", "RSA", "PCU", "RSB", "NCA", "NCU", "RAB", "YAB", "UMN")
.ZA_TESTS <- list(MMD = .za_t_MMD, HRK = .za_t_HRK, HCH = .za_t_HCH, DOR = .za_t_DOR, HPL = .za_t_HPL,
                  PLP = .za_t_PLP, SAP = .za_t_SAP, SCH = .za_t_SCH, E = .za_t_E, PZB = .za_t_PZB,
                  SPL = .za_t_SPL, PRC = function(x, i) .za_t_PRC(x, i), LCU = function(x, i) .za_t_LCU(x, i),
                  WET = .za_t_WET, SAL = .za_t_SAL, RSA = .za_t_RSA, PCU = function(x, i) .za_t_PCU(x, i),
                  RSB = function(x, i) .za_t_RSB(x, i), NCA = function(x, i) .za_t_NCA(x, i),
                  NCU = function(x, i) .za_t_NCU(x, i), RAB = function(x, i) .za_t_RAB(x, i),
                  YAB = function(x, i) .za_t_YAB(x, i), UMN = .za_t_UMN)
.ZA_TOPSOILS <- c("OO", "VA", "MA", "HA", "OA")
# The apedal and structured B horizons, which a layer designated C is not.
.ZA_B_CODES <- c("PRC", "PCU", "RSB", "NCA", "NCU", "RAB", "YAB", "LCU", "PZB")
# The horizons (besides a topsoil, an E and the same B) under which each
# apedal or structured B may lie (D2; S 32).
.ZA_B_ABOVE <- list(RAB = "YAB", YAB = character(0), NCU = character(0), NCA = character(0),
                    PCU = character(0), PRC = character(0))

# One more sign of wetness than a grey matrix (R1): low-chroma mottles on
# 5 % or more, a gley hue, or a moist value of 5 or more.
.za_wet_sign <- function(x, i) {
  m <- .za_colour(x, i, "moist")
  gh <- as.character(unlist(.za_thv("gley_hues")))
  if (!is.na(m$family) && (m$family %in% gh || isTRUE(m$hue %in% gh))) return(TRUE)
  if (isTRUE(m$value >= .za_thv("g_sign_value"))) return(TRUE)
  mot <- .munsell_notations(as.character(.kidpr_v(x, i, "mottle_colors")))
  rf <- .za_num(x, i, "redoximorphic_features_pct")
  low <- nrow(mot) && any(vapply(seq_len(nrow(mot)), function(k)
    isTRUE(.za_is_low(mot$family[k], mot$number[k], mot$value[k], mot$chroma[k])), NA))
  isTRUE(low && rf >= .za_thv("wetness_signs"))
}

# The first code of `v` (named TRUE / FALSE / NA, in the order of trial)
# that passes, the codes in `drop` left out: the code, or NA with the
# candidates (the undecided before the first that passes, and that one; ""
# when none passes, no diagnostic horizon).
.za_pick <- function(v, drop = character(0)) {
  v <- v[!names(v) %in% drop]
  und <- character(0)
  for (k in names(v)) {
    if (isTRUE(v[[k]])) {
      if (length(und)) return(list(code = NA_character_, candidates = c(und, k)))
      return(list(code = k, candidates = character(0)))
    }
    if (is.na(v[[k]])) und <- c(und, k)
  }
  if (length(und)) list(code = NA_character_, candidates = c(und, "")) else list(code = "", candidates = character(0))
}

# The layers tested as topsoil: the surface run, less a layer of it at 350
# mm or deeper that is grey when dry (S 33: a light grey topsoil deeper than
# 350 mm has an E; soilKey's reading, flagged).
.za_topsoil_layers <- function(x) {
  .kidpr_memo(x, "za_top_layers", function() {
    run <- .za_surface_run(x)
    if (length(run) < 2L) return(run)
    lim <- .za_thv("e_light_topsoil") / 10
    keep <- run[1L]
    for (j in run[-1L]) {
      if (!is.na(x$top[j]) && x$top[j] >= lim && isTRUE(.za_grey_dry(x, j)$passed)) break
      keep <- c(keep, j)
    }
    keep
  })
}

#' Pass 1 on one layer: every test of .ZA_ORDER (or the topsoil order on a
#' layer of the surface run), memoised.
#' @noRd
.za_pass1 <- function(x, i) {
  .kidpr_memo(x, sprintf("za_p1_%d", i), function() {
    if (i %in% .za_topsoil_layers(x)) {
      t <- .za_topsoil(x, i)
      v <- vapply(t$answers, function(r) r$passed, NA)
      return(list(topsoil = TRUE, v = v, answers = t$answers))
    }
    ans <- lapply(.ZA_ORDER, function(k) .ZA_TESTS[[k]](x, i))
    names(ans) <- .ZA_ORDER
    # a layer designated C is no apedal or structured B (soilKey's reading)
    if (identical(.za_master(x, i), "C")) {
      for (k in intersect(.ZA_B_CODES, names(ans))) if (!isFALSE(ans[[k]]$passed))
        ans[[k]] <- .za_r(FALSE, note = sprintf("'%s': a C layer, not a B horizon", .za_des(x, i)))
    }
    list(topsoil = FALSE, v = vapply(ans, function(r) r$passed, NA), answers = ans)
  })
}

# Is `set` (the code, or the candidates, of the layer above) within
# `allowed`? TRUE, FALSE or NA.
.za_within <- function(set, allowed) {
  set <- set[nzchar(set)]
  if (!length(set)) return(FALSE)
  if (all(set %in% allowed)) TRUE else if (any(set %in% allowed)) NA else FALSE
}

# The wet material a layer with a grey low-chroma matrix is by what lies
# above it (S 33; P; Q12): the G horizon under a topsoil or an E, the
# unspecified material with signs of wetness under a red or yellow-brown
# apedal, neocutanic or neocarbonate B, the unconsolidated material with
# signs of wetness under a podzol or pedocutanic B; the same as the layer
# above when that one is a wet material; else none ("").
.za_wet_by <- function(above) {
  if (!length(above) || all(!nzchar(above))) return("")
  vapply(above, function(a)
    if (a %in% c(.ZA_TOPSOILS, "E")) "G"
    else if (a %in% c("RAB", "YAB", "NCU", "NCA")) "USW"
    else if (a %in% c("PZB", "PCU")) "UMW"
    else if (a %in% c("G", "USW", "UMW")) a
    else "", "")
}

#' The position pass on one layer below the topsoil: the codes of pass 1
#' read with what lies above (`above`: the code, or the candidates, of the
#' nearest layer above that holds a diagnostic horizon). Returns the code or
#' the candidates, and the notes and tags of the position.
#' @noRd
.za_position <- function(x, i, p1, above, above_top = NA_real_) {
  v <- p1$v
  notes <- character(0); tags <- character(0)
  if (!length(above)) above <- ""
  # the E only directly under an orthic A or another E (S 33; R2, Hugo,
  # 2026-11-06)
  if (!isFALSE(v[["E"]])) {
    ok <- .za_within(above, c("OA", "E"))
    if (isFALSE(ok)) { if (isTRUE(v[["E"]])) notes <- c(notes, "grey and eluvial, but not under an orthic A or an E: not an E")
                       v[["E"]] <- FALSE }
    else if (is.na(ok)) v[["E"]] <- NA
  }
  # the placic pan only under a podzol B (the key); the red structured B
  # only under an orthic A (the key); v0.9.359: either also under a layer of
  # itself (a horizon of several layers)
  for (rule in list(c("PLP", "PZB"), c("RSB", "OA"))) {
    k <- rule[1L]
    if (isFALSE(v[[k]])) next
    ok <- .za_within(above, c(rule[2L], k))
    if (isFALSE(ok)) { if (isTRUE(v[[k]])) notes <- c(notes, sprintf("a %s passes on the data but not under a %s: read on",
                                                                        .za_label(k), .za_label(rule[2L])))
                       v[[k]] <- FALSE }
    else if (is.na(ok)) v[[k]] <- NA
  }
  # lithocutanic B directly under a topsoil horizon or an E; saprolite under
  # a B (default 17)
  under_top <- .za_within(above, c(.ZA_TOPSOILS, "E"))
  # v0.9.359: a layer under a lithocutanic B may be more of it (a horizon of
  # several layers); it is saprolite only by its own test (saprolite_rock)
  under_lcu <- .za_within(above, "LCU")
  if (isTRUE(under_top) && isTRUE(v[["LCU"]]) && !isFALSE(v[["SAP"]])) {
    v[["SAP"]] <- FALSE
    notes <- c(notes, "weathering rock directly under the topsoil or E: the lithocutanic B, not saprolite (default 17)")
  }
  if (isFALSE(under_top) && !isTRUE(under_lcu) && !isFALSE(v[["LCU"]])) {
    if (isTRUE(v[["LCU"]]) && !isTRUE(v[["SAP"]])) {
      v[["SAP"]] <- TRUE
      tags <- c(tags, "lithocutanic_under_b_as_saprolite")
      notes <- c(notes, "weathering rock in place under a B: saprolite, not a lithocutanic B (default 17), flagged")
    }
    v[["LCU"]] <- FALSE
  }
  # D6 (Hugo, 2026-11-06): saprolite directly under the topsoil or an E is
  # the lithocutanic B, flagged
  if (isTRUE(under_top) && isTRUE(v[["SAP"]]) && !isTRUE(v[["LCU"]])) {
    v[["LCU"]] <- TRUE; v[["SAP"]] <- FALSE
    tags <- c(tags, "saprolite_under_topsoil_as_lithocutanic")
    notes <- c(notes, "weathering rock directly under the topsoil or an E: the lithocutanic B, not saprolite (default 17), flagged")
  }
  # D2 (S 32; K 25, 50; Hugo, 2026-11-06): the apedal and structured B
  # horizons only under a topsoil, an E or the same B (the red apedal B also
  # under a yellow-brown apedal B); under a podzol or pedocutanic B a layer
  # that loses its B name so is unconsolidated material
  lost <- FALSE
  for (k in names(.ZA_B_ABOVE)) {
    # nothing above (a truncated profile): no position to read
    if (isFALSE(v[[k]]) || !any(nzchar(above))) next
    ok <- .za_within(above, c(.ZA_TOPSOILS, "E", k, .ZA_B_ABOVE[[k]]))
    if (isFALSE(ok)) {
      if (isTRUE(v[[k]])) { lost <- TRUE; notes <- c(notes, sprintf("a %s on the data, but not under a topsoil, an E or the same B: read on (S 32)", .za_label(k))) }
      v[[k]] <- FALSE
    } else if (is.na(ok)) v[[k]] <- NA
  }
  if (lost && isTRUE(.za_within(above, c("PZB", "PCU", "UMN", "UMW"))) && !isTRUE(v[["SAP"]]) && !isTRUE(v[["HRK"]]) &&
      !isTRUE(v[["UMN"]])) {
    v[["UMN"]] <- TRUE
    tags <- c(tags, "b_under_podzol_or_pedocutanic_as_material")
    notes <- c(notes, "under a podzol or pedocutanic B: unconsolidated material (K 25, 50), flagged")
  }
  # R1 (Hugo, 2026-11-06): a grey matrix that is also a pedocutanic B is the
  # G (or a wet material) only with one more sign of wetness: mottles of
  # wetness, a gley hue or a moist value of 5 or more
  if (isTRUE(v[["WET"]]) && isTRUE(v[["PCU"]]) && !isTRUE(.za_wet_sign(x, i))) {
    v[["WET"]] <- FALSE
    notes <- c(notes, "a grey matrix with no further sign of wetness in a pedocutanic B: the pedocutanic B (R1)")
  }
  # D3 (Hugo, 2026-11-06): under an orthic A (where an E may lie) the G has
  # no less clay than the layer above (an eluvial grey layer is no G)
  if (!isFALSE(v[["WET"]]) && isTRUE(.za_within(above, "OA"))) {
    a <- .kidpr_prev(x, i)
    ca <- if (is.na(a)) NA_real_ else .za_num(x, a, "clay_pct"); ci <- .za_num(x, i, "clay_pct")
    if (!is.na(ca) && !is.na(ci) && ci < ca) {
      v[["WET"]] <- FALSE
      notes <- c(notes, sprintf("a grey layer with less clay (%s %%) than the topsoil above (%s %%): not a G (D3)", format(ci), format(ca)))
    } else if (is.na(ca) || is.na(ci)) { if (isTRUE(v[["WET"]])) v[["WET"]] <- NA }
  }
  # E over a clay horizon that is both G and prismacutanic (K 19, 27): G
  # when the coarser material above is more than 500 mm thick, prismacutanic
  # when less; 500 mm exactly undecided
  if (identical(above, "E") && isTRUE(v[["PRC"]]) && isTRUE(v[["WET"]]) && !is.na(x$top[i])) {
    lim <- .za_thv("g_or_prismacutanic") / 10
    if (x$top[i] > lim) { v[["PRC"]] <- FALSE; notes <- c(notes, sprintf("under an E, %s cm of coarser material above: the G horizon (K 19, 27)", format(x$top[i]))) }
    else if (x$top[i] < lim) { v[["WET"]] <- FALSE; notes <- c(notes, sprintf("under an E, %s cm of coarser material above: the prismacutanic B (K 19, 27)", format(x$top[i]))) }
    else { notes <- c(notes, "under an E at 500 mm exactly: G or prismacutanic undecided (K 19, 27)")
           return(list(code = NA_character_, candidates = c("PRC", "G"), v = v, notes = notes, tags = tags)) }
  }
  # the wet materials and the unconsolidated material by what lies above
  wet_to <- unique(.za_wet_by(above))
  wet_ok <- wet_to[nzchar(wet_to)]
  w <- if (isTRUE(v[["UMN"]]) || is.na(v[["UMN"]])) .za_wetness(x, i) else NULL
  um_to <- if (!isFALSE(v[["UMN"]])) vapply(above, function(a)
             if (a %in% c("PZB", "PCU", "UMN", "UMW")) "UM" else "", "") else ""
  out_v <- v
  # expand WET and UMN into the 30 codes
  expand <- function(v) {
    res <- logical(0)
    for (k in names(v)) {
      if (k == "WET") {
        if (isFALSE(v[[k]])) next
        if (!length(wet_ok)) next
        for (c in wet_ok) res[[c]] <- if (length(wet_to) > 1L && isTRUE(v[[k]])) NA else v[[k]]
      } else if (k == "UMN") {
        if (isFALSE(v[[k]]) || !any(um_to == "UM")) next
        amb <- length(unique(um_to)) > 1L
        wv <- w$passed
        if (isTRUE(wv)) res[["UMW"]] <- if (amb) NA else v[[k]]
        else if (isFALSE(wv)) res[["UMN"]] <- if (amb) NA else v[[k]]
        else { res[["UMW"]] <- NA; res[["UMN"]] <- NA }
      } else res[[k]] <- v[[k]]
    }
    res
  }
  ev <- expand(v)
  if (!isFALSE(v[["WET"]]) && !length(wet_ok) && isTRUE(v[["WET"]]))
    notes <- c(notes, "a grey low-chroma matrix not under a topsoil, E or B that names a wet material: signs of wetness (FC 15), read on")
  pk <- .za_pick(ev)
  # a grey matrix or an unconsolidated material that holds, split between
  # two names only by what lies above or by its wetness: one of them, not none
  sure <- (isTRUE(v[["WET"]]) && length(wet_to) > 1L && all(nzchar(wet_to))) ||
          (isTRUE(v[["UMN"]]) && all(um_to == "UM"))
  if (sure && is.na(pk$code)) pk$candidates <- setdiff(pk$candidates, "")
  list(code = pk$code, candidates = pk$candidates, v = ev, notes = notes, tags = tags)
}

# The excluded horizons of T-humic (iv) as codes.
.ZA_HUMIC_EXCLUDED <- c("G", "E", "PLP", "PZB", "SPL", "HPL", "PRC", "RSB", "UMW", "USW")

# The topsoil of a layer of the surface run after the position pass: T-humic
# (iv) read on the code (or candidates) identified for the first subsoil
# (default 7, the second pass).
.za_topsoil_final <- function(x, i, p1, first) {
  a <- p1$answers
  v <- p1$v
  if (any(v[c("OO", "VA", "MA")] %in% TRUE) || is.null(first)) {
    pk <- .za_pick(v[.ZA_TOPSOILS])
    return(list(code = pk$code, candidates = setdiff(pk$candidates, ""), notes = character(0)))
  }
  h1 <- .za_t_HA(x, i, 1L)$passed
  ex <- .za_within(first, .ZA_HUMIC_EXCLUDED)
  ha <- .kidpr_and(h1, .kidpr_not(ex))
  notes <- character(0)
  if (isTRUE(h1) && isTRUE(ex)) notes <- "a humic A directly over a horizon T-humic (iv) excludes: orthic (default 7)"
  vv <- c(v[c("OO", "VA", "MA")], HA = ha)
  oa <- if (any(vv %in% TRUE)) FALSE else if (all(vv %in% FALSE)) TRUE else NA
  pk <- .za_pick(c(vv, OA = oa))
  list(code = pk$code, candidates = setdiff(pk$candidates, ""), notes = notes)
}

#' The diagnostic horizons of a pedon by the South African classification of
#' 1991
#'
#' The horizon identification of the registry namespace za
#' (identify_horizons()): every layer read as one of the 30 diagnostic
#' horizons and materials. Pass 1 runs the tests of each layer in soilKey's
#' fixed order (the topsoil order on the surface run); the position pass
#' reads them with what lies above (the E and the placic pan, the red
#' structured B, lithocutanic against saprolite, the wet materials, the 500
#' mm rule of K 19 and 27, T-humic (iv)). A layer named in the notation
#' (diagnostic_za, or the names in the designations) keeps that name (route
#' "notation", "both" when the data agree); the others are read on the data
#' (route "data").
#'
#' @param pedon A PedonRecord.
#' @param ... Not used.
#' @param .profile The profile of the readers (.za_x()) when the caller has it.
#' @return The frame of identify_horizons() with the columns `data_code` (the
#'   reading on the data), `za_label` (the printed name with the token),
#'   `tags` (the stand-ins and fallbacks used, a list column) and `notes` (a
#'   list column).
#' @noRd
.za_identify <- function(pedon, ..., .profile = NULL) {
  if (!inherits(pedon, "PedonRecord")) rlang::abort("'pedon' must be a PedonRecord.")
  fr <- .horizon_frame(pedon)
  n <- nrow(fr)
  fr$data_code <- rep(NA_character_, n)
  fr$za_label <- rep(NA_character_, n)
  fr$tags <- rep(list(character(0)), n)
  fr$notes <- rep(list(character(0)), n)
  if (!n) return(fr)
  x <- .profile %||% .za_x(pedon)
  nm <- x$names
  run <- .za_topsoil_layers(x)
  res <- vector("list", n)
  # the code (or candidates) a layer gives the layers below it
  given <- function(r) if (!is.na(r$code)) r$code else r$candidates
  last <- character(0)
  for (i in x$ord) {
    p1 <- .za_pass1(x, i)
    d <- if (p1$topsoil) {
      pk <- .za_pick(p1$v[.ZA_TOPSOILS])
      list(code = pk$code, candidates = setdiff(pk$candidates, ""), v = p1$v, notes = character(0), tags = character(0))
    } else .za_position(x, i, p1, last)
    ans <- p1$answers
    used <- if (!is.na(d$code) && nzchar(d$code)) d$code else d$candidates
    src <- c(if ("WET" %in% names(ans) && any(used %in% c("G", "USW", "UMW"))) "WET",
             if ("UMN" %in% names(ans) && any(used %in% c("UMN", "UMW"))) "UMN", intersect(used, names(ans)))
    tg <- unique(c(d$tags, unlist(lapply(ans[src], `[[`, "tag"))))
    nt <- unique(c(d$notes, unlist(lapply(ans[src], `[[`, "note"))))
    ms <- unique(unlist(lapply(ans[names(ans) %in% c(src, if (is.na(d$code)) names(ans))], function(r) if (is.na(r$passed)) r$missing)))
    r <- list(code = d$code, candidates = d$candidates, tags = tg, notes = nt, missing = ms, data_code = d$code, route = "data")
    # the name written for the layer
    if (!is.null(nm) && identical(nm$route[i], "notation")) {
      t <- nm$token[i]
      cand <- nm$candidates[[i]]
      dc <- d$code
      if (!is.na(t)) {
        r$route <- if (identical(dc, t)) "both" else "notation"
        r$notes <- c(sprintf("written %s%s", .za_label(t),
                             if (identical(dc, t)) "; the data agree"
                             else if (!is.na(dc)) sprintf("; the data read %s, the name is kept", if (nzchar(dc)) .za_label(dc) else "no diagnostic horizon")
                             else sprintf("; the data leave it undecided (%s), the name is kept",
                                          paste(ifelse(nzchar(d$candidates), d$candidates, "none"), collapse = ", "))),
                     nm$notes[[i]])
        r$code <- t; r$candidates <- character(0); r$missing <- character(0)
      } else if (length(cand)) {
        r$route <- "notation"
        r$code <- NA_character_; r$candidates <- cand
        r$notes <- c(sprintf("written as %s, with doubt", paste(cand, collapse = " or ")), nm$notes[[i]])
      }
    }
    res[[i]] <- r
    g <- given(r)
    # a layer of no diagnostic horizon is skipped; one that may be none
    # leaves the layer above it possible too
    if (!(length(g) == 1L && identical(g, "")))
      last <- unique(c(g[nzchar(g)], if ("" %in% g) last))
  }
  # T-humic (iv) on the code identified for the first subsoil (default 7)
  if (length(run)) {
    below <- setdiff(x$ord[x$rank > max(x$rank[run])], integer(0))
    first <- NULL
    for (j in below) { g <- given(res[[j]]); if (!(length(g) == 1L && identical(g, ""))) { first <- g; break } }
    for (i in run) {
      if (!identical(res[[i]]$route, "data")) next
      tf <- .za_topsoil_final(x, i, .za_pass1(x, i), first)
      res[[i]]$code <- tf$code; res[[i]]$candidates <- tf$candidates; res[[i]]$data_code <- tf$code
      res[[i]]$notes <- unique(c(res[[i]]$notes, tf$notes))
      ans <- .za_pass1(x, i)$answers
      use <- if (!is.na(tf$code)) tf$code else tf$candidates
      res[[i]]$tags <- unique(unlist(lapply(ans[intersect(use, names(ans))], `[[`, "tag")))
      res[[i]]$notes <- unique(c(res[[i]]$notes, unlist(lapply(ans[intersect(use, names(ans))], `[[`, "note"))))
      res[[i]]$missing <- if (is.na(tf$code)) unique(unlist(lapply(ans[intersect(tf$candidates, names(ans))], `[[`, "missing"))) else character(0)
    }
  }
  # D7 (K 70 i, K 72 iv, K 73 iii; Hugo, 2026-11-06): a surface layer with
  # under 0.3 % organic carbon that is a red or yellow-brown apedal or a
  # neocarbonate B, directly over hard rock, a lithocutanic B or a dorbank,
  # is that B at the surface (the profile truncated), flagged. B1 (Hugo,
  # 2026-11-10): only when the layer is more than 100 mm thick
  # (surface_b_thick); a thinner one is not read as a B at the surface; the
  # thickness not recorded, undecided (D)
  if (length(run) == 1L && identical(res[[run]]$route, "data")) {
    i <- run
    oc <- .za_num(x, i, "oc_pct")
    nx <- .kidpr_next(x, i)
    below <- if (is.na(nx)) NA_character_ else res[[nx]]$code
    thick_ok <- x$thick[i] * 10 > .za_thv("surface_b_thick")
    if (!is.na(oc) && oc < .za_thv("surface_b_oc") && isTRUE(below %in% c("HRK", "LCU", "DOR")) && !isFALSE(thick_ok)) {
      for (k in c("RAB", "YAB", "NCA")) {
        b <- .ZA_TESTS[[k]](x, i)
        # D (Hugo, 2026-11-10): the thickness not recorded leaves the
        # layer undecided between the orthic A and the B at the surface
        if (isTRUE(b$passed) && is.na(thick_ok)) {
          res[[i]]$code <- NA_character_; res[[i]]$candidates <- c("OA", k); res[[i]]$data_code <- NA_character_
          res[[i]]$tags <- unique(c(b$tag, "surface_b_low_oc", "stand_in:surface_b_oc", "stand_in:surface_b_thick"))
          res[[i]]$notes <- unique(c(sprintf("organic carbon %s %% over %s, thickness not recorded: the orthic A or a %s at the surface (B1), undecided",
                                             format(oc), .za_label(below), .za_label(k)), b$note))
          res[[i]]$missing <- c("top_cm", "bottom_cm")
          break
        }
        if (isTRUE(b$passed)) {
          res[[i]]$code <- k; res[[i]]$candidates <- character(0); res[[i]]$data_code <- k
          res[[i]]$tags <- unique(c(b$tag, "surface_b_low_oc", "stand_in:surface_b_oc", "stand_in:surface_b_thick"))
          res[[i]]$notes <- unique(c(sprintf("organic carbon %s %% over %s, %s mm thick: a %s at the surface, the profile truncated (K 70, 72, 73), flagged",
                                             format(oc), .za_label(below), format(x$thick[i] * 10), .za_label(k)), b$note))
          res[[i]]$missing <- character(0)
          break
        }
      }
    }
  }
  for (i in seq_len(n)) {
    r <- res[[i]]
    fr$code[i] <- if (!is.na(r$code) && nzchar(r$code)) r$code else NA_character_
    fr$passed[i] <- if (is.na(r$code)) NA else nzchar(r$code)
    fr$candidates[[i]] <- r$candidates
    fr$missing[[i]] <- if (is.na(r$code)) r$missing %||% character(0) else character(0)
    fr$route[i] <- r$route
    fr$data_code[i] <- r$data_code
    fr$za_label[i] <- if (!is.na(r$code)) .za_label(r$code) else NA_character_
    fr$tags[[i]] <- r$tags
    fr$notes[[i]] <- r$notes
    fr$evidence[i] <- .kidpr_evidence(c(r$notes, if (length(r$tags)) sprintf("flagged: %s", paste(r$tags, collapse = ", "))))
  }
  fr
}


# --- the predicates of part 3 over a pedon ----------------------------------------------

# A test over the identified layers of a pedon: TRUE on the layers the
# identification gives the code, NA where it is a candidate.
.za_ident_predicate <- function(pedon, name, code) {
  fr <- .za_identify(pedon)
  hit <- which(fr$code %in% code)
  cand <- which(is.na(fr$code) & vapply(fr$candidates, function(cc) any(cc %in% code), NA))
  passed <- if (length(hit)) TRUE else if (length(cand)) NA else FALSE
  ev <- lapply(seq_len(nrow(fr)), function(i) list(code = fr$code[i], candidates = fr$candidates[[i]],
                                                   route = fr$route[i], notes = fr$notes[[i]], tags = fr$tags[[i]]))
  names(ev) <- sprintf("layer_%d", seq_len(nrow(fr)))
  tags <- unique(unlist(fr$tags[if (length(hit)) hit else cand]))
  DiagnosticResult$new(name = name, passed = passed, layers = hit, evidence = ev,
                       missing = if (is.na(passed)) unique(unlist(fr$missing[cand])) else character(0),
                       reference = .ZA_REFERENCE,
                       notes = if (length(tags)) sprintf("flagged: %s", paste(tags, collapse = ", ")) else NA_character_)
}

#' The diagnostic horizons of the South African classification of 1991
#' on the data (second part)
#'
#' The layer tests of the other seventeen horizons and materials over the
#' layers below the topsoil (or the `layers` given), and, for the G horizon
#' and the three wet and unconsolidated materials, which depend on what lies
#' above, the identification of the whole profile (.za_identify()).
#' @param pedon A PedonRecord.
#' @param layers Optional layer indices to test.
#' @noRd
e_horizon_za <- function(pedon, layers = NULL) .za_predicate(pedon, "e_horizon_za", .za_t_E, layers)
#' @noRd
podzol_b_za <- function(pedon, layers = NULL) .za_predicate(pedon, "podzol_b_za", .za_t_PZB, layers)
#' @noRd
placic_pan_za <- function(pedon, layers = NULL) .za_predicate(pedon, "placic_pan_za", .za_t_PLP, layers)
#' @noRd
soft_plinthic_b_za <- function(pedon, layers = NULL) .za_predicate(pedon, "soft_plinthic_b_za", .za_t_SPL, layers)
#' @noRd
hard_plinthic_b_za <- function(pedon, layers = NULL) .za_predicate(pedon, "hard_plinthic_b_za", .za_t_HPL, layers)
#' @noRd
soft_carbonate_za <- function(pedon, layers = NULL) .za_predicate(pedon, "soft_carbonate_za", .za_t_SCH, layers)
#' @noRd
hardpan_carbonate_za <- function(pedon, layers = NULL) .za_predicate(pedon, "hardpan_carbonate_za", .za_t_HCH, layers)
#' @noRd
dorbank_za <- function(pedon, layers = NULL) .za_predicate(pedon, "dorbank_za", .za_t_DOR, layers)
#' @noRd
saprolite_za <- function(pedon, layers = NULL) .za_predicate(pedon, "saprolite_za", .za_t_SAP, layers)
#' @noRd
hard_rock_za <- function(pedon, layers = NULL) .za_predicate(pedon, "hard_rock_za", .za_t_HRK, layers)
#' @noRd
stratified_alluvium_za <- function(pedon, layers = NULL) .za_predicate(pedon, "stratified_alluvium_za", .za_t_SAL, layers)
#' @noRd
regic_sand_za <- function(pedon, layers = NULL) .za_predicate(pedon, "regic_sand_za", .za_t_RSA, layers)
#' @noRd
man_made_deposit_za <- function(pedon, layers = NULL) .za_predicate(pedon, "man_made_deposit_za", .za_t_MMD, layers)
#' @noRd
wet_material_za <- function(pedon, layers = NULL) .za_predicate(pedon, "wet_material_za", .za_t_WET, layers)
#' @noRd
unconsolidated_material_za <- function(pedon, layers = NULL) .za_predicate(pedon, "unconsolidated_material_za", .za_t_UMN, layers)
#' @noRd
g_horizon_za <- function(pedon) .za_ident_predicate(pedon, "g_horizon_za", "G")
#' @noRd
unspecified_material_wet_za <- function(pedon) .za_ident_predicate(pedon, "unspecified_material_wet_za", "USW")
#' @noRd
unconsolidated_material_wet_za <- function(pedon) .za_ident_predicate(pedon, "unconsolidated_material_wet_za", "UMW")
#' @noRd
unconsolidated_material_dry_za <- function(pedon) .za_ident_predicate(pedon, "unconsolidated_material_dry_za", "UMN")
