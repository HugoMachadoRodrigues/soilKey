# =============================================================================
# v0.9.330 -- the horizon notation of the French soil classification (CPCS
# 1967, pp. 11-13): Aoo and Ao (with F and H), A1, A2, A3, AB, B, (B), B1 to
# B3, C, R, a Roman numeral for a change of parent material, and the symbols
# Ca, Can, Cs, Cn, g (pseudogley), G (gley), Sa, p, h, fe, t, x, m; (A) for
# the embryonic A of raw soils and G (Go, Gr) or g as a master horizon. The
# FAO and USDA letters (E, Bw, Bt, Bs, Bh, Ck, Cr, Oi ...), which most later
# French descriptions use, are read beside them.
# =============================================================================

.CPCS_DECLARED <- c("cpcs", "cpcs1967", "french", "francaise", "fr", "cpcs 1967")

# Is a profile written in the CPCS notation? Declared in
# site$designation_system, otherwise judged from its own tokens (Aoo or Ao,
# (A), (B), Bfe, Ca or Can, Cs, Cn, Sa, a G master), never from the country.
# Where the two notations clash (Cs, Cn, n), the CPCS reading needs this.
.cpcs_notation <- function(desig, site) {
  d <- tolower(trimws(as.character((site %||% list())$designation_system %||% NA)[1L]))
  if (!is.na(d) && nzchar(d) && d != "na") return(d %in% .CPCS_DECLARED)
  v <- trimws(as.character(desig)); v <- v[!is.na(v) & nzchar(v)]
  if (!length(v)) return(FALSE)
  any(grepl("^(A00|Aoo|AOO|A0|Ao|AO)$|\\((A|B)\\)|fe|[ABC][Cc]an?$|^[IVX0-9]*C(s|n|Sa|sa)$|^[IVX0-9]*G[or0]?$", v))
}

# One designation: its master (role), and the symbols.
#   role: "O" (Aoo, Ao, L, F, H, O), "A" (A, A1, Ap, Ah, (A)), "E" (A2, E, or
#   a g master), "AB" (A3, AB, A/B, AE), "AC", "B" (B, (B), B1 to B3, BA,
#   BC), "C" (C, CB, Cr), "R", "G" (a gley master), "other" (unreadable).
#   bstruct: a structural or colour B ((B), Bw, or a B whose only symbols
#   are accumulations of carbonate, gypsum or salts, or slickensides);
#   benriched: an enriched B (Bt, Bh, Bs, Bfe, Bo, Bn, and a plain B, B2 or
#   Bg in the CPCS notation, where B alone means enriched); NA where the
#   FAO notation leaves it open (a plain B or Bg).
.cpcs_parse_one <- function(d, cpcs) {
  out <- list(role = NA_character_, known = FALSE, embryo = FALSE, bstruct = FALSE, benriched = FALSE,
              t = FALSE, h = FALSE, fe = FALSE, g = FALSE, G = FALSE, ca = FALSE, cs = FALSE, cn = FALSE,
              sa = FALSE, p = FALSE, x = FALSE, m = FALSE, frozen = FALSE, buried = FALSE, natric = FALSE,
              ss = FALSE, wrock = FALSE, trans = FALSE, lith = 1L)
  if (is.na(d) || !nzchar(trimws(d))) return(out)
  # the symbol only: a word written after a space or a slash ("Btg/glossic")
  # is a note, not symbols
  s <- strsplit(trimws(as.character(d)), "[[:space:]]+")[[1L]][1L]
  s <- sub("/[a-z]{3,}.*$", "", s)
  # a Roman (or Arabic) numeral before the master: a change of parent material
  lm <- regmatches(s, regexpr("^([IVX]+|[0-9]+)(?=[A-Z(])", s, perl = TRUE))
  if (length(lm) && nzchar(lm)) {
    out$lith <- if (grepl("^[0-9]+$", lm)) as.integer(lm) else
      c(I = 1L, II = 2L, III = 3L, IV = 4L, V = 5L, VI = 6L, VII = 7L, VIII = 8L, IX = 9L, X = 10L)[lm] %||% 2L
    s <- substring(s, nchar(lm) + 1L)
  }
  out$known <- TRUE
  rest <- ""
  pick <- function(pat, role) {
    if (grepl(pat, s, perl = TRUE)) { rest <<- sub(pat, "", s, perl = TRUE); out$role <<- role; TRUE } else FALSE
  }
  if (pick("^\\(A\\)", "A")) out$embryo <- TRUE
  else if (pick("^\\(B\\)", "B")) out$bstruct <- TRUE
  else if (pick("^(A00|Aoo|AOO|A0|Ao|AO)(?![0-9])", "O")) NULL
  else if (pick("^(L|F|H|O)", "O")) NULL
  else if (pick("^(A2|E)", "E")) NULL
  else if (pick("^(A3|AB|A/B|AE)", "AB")) NULL
  else if (pick("^(AC|A/C)", "AC")) NULL
  else if (pick("^A", "A")) NULL
  else if (pick("^(BA|BE|B/A|BC|B/C)", "B")) out$trans <- TRUE
  else if (pick("^B", "B")) { if (grepl("^[13]($|[^0-9])", rest)) out$trans <- TRUE }
  else if (pick("^(CB|C/B|C)", "C")) NULL
  else if (pick("^R", "R")) NULL
  else if (pick("^G", "G")) NULL
  else if (pick("^g", "E")) out$g <- TRUE
  else { out$role <- "other"; return(out) }

  r0 <- sub("^[0-9]+", "", rest)
  take <- function(pat) {
    hit <- grepl(pat, r0, perl = TRUE)
    if (hit) r0 <<- gsub(pat, "", r0, perl = TRUE)
    hit
  }
  # the CPCS two-letter symbols first, then the single letters
  can <- take("[Cc]an")
  ca <- take("[Cc]a") || can
  cs <- if (cpcs) take("[Cc]s") else FALSE
  cn <- if (cpcs) take("[Cc]n") else FALSE
  sa <- take("[Ss]a")
  fe <- take("fe")
  ss <- take("ss")
  ch <- strsplit(gsub("[^A-Za-z]", "", r0), "")[[1L]]
  out$t <- "t" %in% ch
  out$h <- "h" %in% ch
  out$fe <- fe || "s" %in% ch
  out$g <- out$g || "g" %in% ch
  # r: reduced (Gr, Br); a Cr is weathered rock (the USDA letter), as in
  # the other national keys
  out$G <- "G" %in% ch || out$role %in% "G" || ("r" %in% ch && out$role %in% "B")
  out$wrock <- out$role %in% "C" && "r" %in% ch
  out$ca <- ca || "k" %in% ch
  out$cs <- cs || "y" %in% ch
  out$cn <- cn || "c" %in% ch
  out$sa <- sa || "z" %in% ch
  out$p <- "p" %in% ch
  out$x <- "x" %in% ch
  out$m <- "m" %in% ch
  out$frozen <- "f" %in% ch
  out$buried <- "b" %in% ch
  out$natric <- !cpcs && "n" %in% ch
  out$ss <- ss
  if (out$role %in% "B" && !out$bstruct) {
    if (out$t || out$h || out$fe || "o" %in% ch || out$natric || "v" %in% ch) out$benriched <- TRUE
    else if ("w" %in% ch || out$ca || out$cs || out$sa || ss) out$bstruct <- TRUE
    else if (cpcs) out$benriched <- TRUE
    else { out$benriched <- NA; out$bstruct <- NA }
  }
  out
}

# All the designations of a profile, as vectors.
.cpcs_designations <- function(desig, site) {
  desig <- as.character(desig)
  cpcs <- .cpcs_notation(desig, site)
  rows <- lapply(desig, .cpcs_parse_one, cpcs = cpcs)
  fields <- names(.cpcs_parse_one(NA, cpcs))
  out <- lapply(fields, function(f) {
    v <- lapply(rows, `[[`, f)
    if (f == "role") vapply(v, function(z) z %||% NA_character_, "") else
    if (f == "lith") vapply(v, function(z) as.integer(z), 1L) else
      vapply(v, function(z) if (is.na(z)) NA else isTRUE(z), NA)
  })
  names(out) <- fields
  out$cpcs <- cpcs
  out$any_known <- any(out$known)
  out
}
