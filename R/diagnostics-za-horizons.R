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
# fixed order (O, vertic, melanic, humic, orthic; default 7).
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
#' (default 9).
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
  .rp_all(.za_red(x, i), .za_apedal(x, i), .za_uniform(x, i), .za_few_mottles(x, i),
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
  .rp_all(.za_yellow_brown(x, i), .za_apedal(x, i), .za_uniform(x, i), .za_few_mottles(x, i),
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
#' recorded, soilKey's argic() stands in, flagged); for a red B, the cutans
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
