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
  v <- vapply(as.character(desig), .cpcs_symbol, "")
  v <- v[!is.na(v) & nzchar(v)]
  if (!length(v)) return(FALSE)
  any(grepl(paste0("^(A00|Aoo|AOO|A0|Ao|AO)$|^\\((A|B)\\)|fe|",
                   "^[IVX0-9]*[ABC][0-9]*(C(an?|s|n)|Sa)[0-9]*$|^[IVX0-9]*C[0-9]*c(an?|s|n)$|",
                   "^[IVX0-9]*G[or0]?$|^[IVX0-9]*B[123]([0-9]|$)"), v))
}

# The symbol of a designation: a Roman or Arabic numeral written apart is
# joined to the master ("II B3", p. 12); a word after a space or a slash
# ("Btg/glossic") and a bracketed letter after the master (the New Zealand
# "Bw(h)") are notes; a designation written all in lower case takes a
# capital master.
.cpcs_symbol <- function(d) {
  if (is.na(d) || !nzchar(trimws(d))) return(NA_character_)
  s <- sub("^([IVX]+|[0-9]+)[[:space:]]+(?=[A-Z(])", "\\1", trimws(as.character(d)), perl = TRUE)
  s <- strsplit(s, "[[:space:]]+")[[1L]][1L]
  s <- sub("/[a-z]{3,}.*$", "", s)
  s <- gsub("(?<=[A-Za-z0-9])\\([a-z]{1,3}\\)", "", s, perl = TRUE)
  if (nchar(s) > 1L && grepl("^[abcer][a-z0-9]*$", s)) s <- paste0(toupper(substr(s, 1L, 1L)), substring(s, 2L))
  s
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
  s <- .cpcs_symbol(d)
  # a Roman (or Arabic) numeral before the master: a change of parent material
  lm <- regmatches(s, regexpr("^([IVX]+|[0-9]+)(?=[A-Z(])", s, perl = TRUE))
  if (length(lm) && nzchar(lm)) {
    rom <- c("I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X", "XI", "XII", "XIII", "XIV", "XV")
    out$lith <- if (grepl("^[0-9]+$", lm)) as.integer(lm) else { k <- match(lm, rom); if (is.na(k)) 2L else k }
    s <- substring(s, nchar(lm) + 1L)
  }
  out$known <- TRUE
  rest <- ""
  pick <- function(pat, role) {
    if (grepl(pat, s, perl = TRUE)) { rest <<- sub(pat, "", s, perl = TRUE); out$role <<- role; TRUE } else FALSE
  }
  if (cpcs && grepl("^[AB][0-9]*C(an?|s|n)([0-9]|$)", s)) {
    out$role <- substr(s, 1L, 1L); rest <- substring(s, 2L)
  }
  else if (pick("^\\(A\\)", "A")) out$embryo <- TRUE
  else if (pick("^\\(B\\)", "B")) out$bstruct <- TRUE
  else if (pick("^(A00|Aoo|AOO|A0|Ao|AO)(?![0-9])", "O")) NULL
  else if (pick("^(L|F|H|O)", "O")) NULL
  else if (cpcs && pick("^A2", "E")) NULL
  else if (pick("^E", "E")) NULL
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
  # in the CPCS notation a C master followed by s or n is Cs (gypsum) or Cn
  # (concretions)
  if (cpcs && out$role %in% "C" && grepl("^(s|n)$", r0)) r0 <- paste0("C", r0)
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
  # the Canadian, New Zealand and Australian notations (declared, or the
  # country when none is declared): f on a B is iron (the podzolic Bf), not
  # frozen
  ds <- tolower(as.character((site %||% list())$designation_system %||% NA)[1L])
  ct <- toupper(trimws(as.character((site %||% list())$country %||% NA)[1L]))
  other <- (!is.na(ds) && grepl("cssc|canad|nzsc|new zealand|^nz$|asc|austral|field handbook|nchs", ds)) ||
    ((is.na(ds) || !nzchar(ds) || ds == "na") && !is.na(ct) &&
       ct %in% c("CA", "CAN", "CANADA", "NZ", "NZL", "NEW ZEALAND", "AU", "AUS", "AUSTRALIA"))
  if (other && !cpcs) {
    out$fe <- out$fe | (out$role %in% "B" & out$frozen %in% TRUE)
    out$frozen <- rep(FALSE, length(out$frozen))
  }
  out$cpcs <- cpcs
  out$any_known <- any(out$known)
  out
}
