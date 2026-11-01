# =============================================================================
# v0.9.321 -- horizon designations for the Soil Classification System of
# Japan (SCSJ 2017, Japanese Society of Pedology), in the Japanese notation of
# the Soil Survey Handbook (JSP 1997), which the SCSJ names for its master
# horizons: H (peat or muck formed under water), O (forest floor: Oi, Oe,
# Oa), A, E, B, C, G (a gley layer, with Go for an oxidised gley layer with
# iron mottles and Gr for a strongly reduced one) and R (hard bedrock). A
# lithologic discontinuity is an Arabic (2A) or Roman (IIA) prefix; a buried
# horizon has a trailing b (Ab, 2Ab); two masters make a transition (AB, BC,
# A/B); trailing digits split a horizon (B1, B2).
#
# The suffixes the key reads: p ploughed (Ap, the plough layer), g gleyed or
# redox-mottled, t illuvial clay, w weathered, s and h sesquioxide and humus
# accumulation (Bs, Bh, Bhs), k carbonates, ir iron and mn manganese
# accumulation (Bir, Bmn: the iron accumulation of a paddy), and the
# decomposition of O (i, e, a). The forest-floor notation of forest soil
# surveys (L, F and H above the mineral soil) is read as O horizons: there
# an H is the humus layer of the floor, not peat. A leading ^ (USDA:
# human-transported material) marks fill. The FAO/USDA notation is read by
# the same letters; there a Cr is weathered rock, as in the USDA notation,
# unless the site declares the FAO notation (a strongly reduced C, the G of
# Japan).
# =============================================================================

# The parse of one designation: fill mark, lithologic number, master
# horizon(s), suffix tokens, trailing digits. NULL when it does not fit.
.parse_designation_scsj1 <- function(d) {
  d <- trimws(as.character(d))
  if (is.na(d) || !nzchar(d)) return(NULL)
  d <- sub("[[:space:]].*$", "", d)          # a note after the designation
  d <- sub("\\?+$", "", d)
  m <- regmatches(d, regexec(
    "^(\\^?)([0-9]*|I{1,3}|IV|VI{0,3}|IX|X)(H|O|A|E|B|C|G|R|L|F)(/?)(H|O|A|E|B|C|G|R)?(.*)$", d))[[1]]
  if (!length(m)) return(NULL)
  lith <- m[3L]
  num <- if (!nzchar(lith)) 1L else if (grepl("^[0-9]+$", lith)) as.integer(lith) else
    c(I = 1L, II = 2L, III = 3L, IV = 4L, V = 5L, VI = 6L, VII = 7L, VIII = 8L, IX = 9L, X = 10L)[[lith]]
  rest <- m[7L]
  digits <- sub("^.*?([0-9]*)$", "\\1", rest)
  rest <- sub("[0-9]*$", "", rest)
  rest <- gsub("[0-9]", "", rest)             # Ap1g, Bg2: digits inside
  if (grepl("[^a-z]", rest)) return(NULL)
  # the two-letter tokens first (ir, mn, ss), then single letters
  tok <- character(0)
  r <- rest
  while (nzchar(r)) {
    two <- substr(r, 1L, 2L)
    if (two %in% c("ir", "mn", "ss")) { tok <- c(tok, two); r <- substring(r, 3L) }
    else { tok <- c(tok, substr(r, 1L, 1L)); r <- substring(r, 2L) }
  }
  list(fill = nzchar(m[2L]), lith = num, master = m[4L], intermingled = nzchar(m[5L]),
       master2 = if (nzchar(m[6L])) m[6L] else "", tokens = tok,
       digits = digits, raw = rest)
}

# Is the notation one this parser reads? Declared in site$designation_system;
# the Australian, Canadian and New Zealand notations are not read. `fao` when
# the FAO notation is declared (a Cr is then a reduced C).
.SCSJ_OTHER_NOTATIONS <- c("asc", "australian", "fieldhandbook", "nchs", "australianfieldhandbook",
                           "cssc", "cssc3", "canadian", "canada", "nzsc", "nzsc4", "nz", "newzealand")

.scsj_notation <- function(site = list()) {
  declared <- tolower(as.character(site$designation_system %||% NA)[1L])
  key <- gsub("[^a-z0-9]", "", declared)
  if (!is.na(declared) && key %in% .SCSJ_OTHER_NOTATIONS)
    return(list(ok = FALSE, fao = FALSE, why = sprintf("declared: %s", declared)))
  list(ok = TRUE, fao = !is.na(declared) && .cssc_fao_declared(declared),
       why = if (is.na(declared)) "Japanese or FAO/USDA notation" else sprintf("declared: %s", declared))
}

# The role and flags of each horizon. role: H, O, A, E, B, C, G, R, a
# transition as written (AB, BA, BC, CB, AC, A/B ...), "other" (not readable)
# or NA (no designation). L and F (and an H among them, above the mineral
# soil) are O.
.scsj_designations <- function(designation, site = list(), top = NULL) {
  d <- as.character(designation %||% character(0))
  n <- length(d)
  nota <- .scsj_notation(site)
  role <- rep(NA_character_, n); tok <- vector("list", n); lith <- rep(NA_integer_, n)
  master <- rep(NA_character_, n); fill <- rep(FALSE, n); lf <- rep(FALSE, n)
  for (i in seq_len(n)) {
    tok[[i]] <- character(0)
    if (is.na(d[i]) || !nzchar(trimws(d[i]))) next
    p <- if (nota$ok) .parse_designation_scsj1(d[i]) else NULL
    if (is.null(p)) { role[i] <- "other"; next }
    m1 <- p$master
    if (m1 %in% c("L", "F")) { lf[i] <- TRUE; m1 <- "O" }
    role[i] <- if (nzchar(p$master2) && p$master2 != m1)
                 paste0(m1, if (p$intermingled) "/" else "", p$master2)
               else m1
    master[i] <- m1
    tok[[i]] <- p$tokens
    lith[i] <- p$lith
    fill[i] <- p$fill
  }
  # the forest-floor notation: with L or F horizons recorded, an H above the
  # first mineral horizon is the humus layer of the floor (O), not peat
  # (Hugo, 2026-10-31)
  if (any(lf)) {
    ord <- if (is.null(top)) seq_len(n) else order(top, na.last = TRUE)
    for (i in ord) {
      if (role[i] %in% c("O")) next
      if (role[i] %in% "H") { role[i] <- "O"; master[i] <- "O"; next }
      break
    }
  }
  has <- function(l) vapply(tok, function(s) l %in% s, NA)
  known <- !is.na(role) & role != "other"
  org <- role %in% c("H", "O")
  cr <- role %in% "C" & has("r")
  list(notation = nota, role = role, master = master, known = known, tokens = tok,
       lith = lith, fill = fill, forest_floor_notation = lf,
       buried = has("b") & !org,
       p = has("p") & !org, g = has("g") & !org, t = has("t") & !org,
       w = has("w") & !org, s = has("s") & !org, h = has("h") & !org,
       k = has("k"), ir = has("ir"), mn = has("mn"),
       # G horizons: Go oxidised with iron mottles, Gr strongly reduced
       gley_master = role %in% "G" | (cr & nota$fao),
       weathered_rock = cr & !nota$fao,
       decomp = ifelse(role %in% "O" & has("i"), "slightly",
                ifelse(role %in% "O" & has("e"), "moderately",
                ifelse(role %in% "O" & has("a"), "highly", NA_character_))))
}
