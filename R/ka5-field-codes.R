# =============================================================================
# v0.9.360 -- the field codes of the German KA5 description sheet (Ad-hoc-AG
# Boden 2005), read into the attributes they record (Hugo's decision Q24):
# the humus classes h0-h7 (field 29, Tab. 15, p. 112), the carbonate classes
# c0-c7 (field 46, Tab. 40, pp. 168-169), the hydromorphic features (fields
# 30 and 31, Tab. 16, p. 113, with the area classes f1-f9 of Tab. 4, p. 53),
# the texture classes (Bodenart, Tab. 30), the colour words of Liste 18
# (p. 110) and the German humus-form names. A class gives a range: minima
# are read at its lower end, maxima at its upper end (the owner's range
# rule). Nothing stops with an error: an unread code gives NA, with a note.
# =============================================================================

# Tab. 15, % organic substance: lower and upper end (upper exclusive); under
# forest the footnote shifts h3 to h5 (2-5, 5-10, 10-15 %).
.KA5_HUMUS_CLASSES <- list(h0 = c(0, 0), h1 = c(0, 1), h2 = c(1, 2), h3 = c(2, 4), h4 = c(4, 8),
                           h5 = c(8, 15), h6 = c(15, 30), h7 = c(30, 100))
.KA5_HUMUS_CLASSES_FOREST <- list(h3 = c(2, 5), h4 = c(5, 10), h5 = c(10, 15))

# Tab. 40, % CaCO3 equivalent of the fine earth.
.KA5_CARBONATE_CLASSES <- list(c0 = c(0, 0), c1 = c(0, 0.5), c2 = c(0.5, 2), c3 = c(2, 10),
                               c3.2 = c(2, 4), c3.3 = c(4, 7), c3.4 = c(7, 10), c4 = c(10, 25),
                               c5 = c(25, 50), c6 = c(50, 75), c7 = c(75, 100))

# Tab. 4, % of the area.
.KA5_AREA_CLASSES <- list(f1 = c(0, 1), f2 = c(1, 2), f3 = c(2, 5), f4 = c(5, 10), f5 = c(10, 30),
                          f6 = c(30, 50), f7 = c(50, 70), f8 = c(70, 90), f9 = c(90, 100))

# Tab. 30: the texture classes (Feinbodenarten) and their main group
# (Bodenartenhauptgruppe) as Abb. 17 (p. 142) draws them: Sl4, Slu and St3
# are loams, Ts3 and Ts4 loams, Lu a silt, Lt3 a clay.
.KA5_TEXTURE_CLASSES <- c(
  Ss = "S", Sl2 = "S", Sl3 = "S", Sl4 = "L", Slu = "L", St2 = "S", St3 = "L", Su2 = "S", Su3 = "S", Su4 = "S",
  Ls2 = "L", Ls3 = "L", Ls4 = "L", Lt2 = "L", Lt3 = "T", Lts = "L", Lu = "U",
  Uu = "U", Uls = "U", Us = "U", Ut2 = "U", Ut3 = "U", Ut4 = "U",
  Tt = "T", Tl = "T", Tu2 = "T", Tu3 = "T", Tu4 = "T", Ts2 = "T", Ts3 = "L", Ts4 = "L")

# Liste 18: the colour words, the words before them and the one after them.
.KA5_COLOURS <- c("bl", "bn", "ge", "gn", "ro", "gr", "sw", "we", "vi", "or", "ol", "tk", "oc")
.KA5_COLOUR_BEFORE <- c("hh", "dd", "le", "fa", "sm", "h", "d")

# Tab. 16: the kinds of hydromorphic features and the attribute they record.
.KA5_HYDRO_KINDS <- c(e = "rust", ed = "rust", eh = "rust", eo = "rust", es = "rust",
                      r = "reduction", rb = "bleached", rg = "reduction", rt = "reduction", rs = "reduction")

# A class code read as its range; NA when the code is not one.
.ka5_class_range <- function(x, table) {
  k <- tolower(trimws(as.character(x)[1L]))
  if (is.na(k) || !(k %in% names(table))) return(c(NA_real_, NA_real_))
  table[[k]]
}

#' The humus class of field 29 (h0-h7) as % organic substance
#' @param forest TRUE under forest (the footnote of Tab. 15).
#' @return c(lower, upper), the upper end exclusive (h7: 30 or more); NA when
#'   the code is not a class.
#' @noRd
.ka5_humus_class <- function(x, forest = FALSE) {
  tab <- .KA5_HUMUS_CLASSES
  if (isTRUE(forest)) tab[names(.KA5_HUMUS_CLASSES_FOREST)] <- .KA5_HUMUS_CLASSES_FOREST
  .ka5_class_range(x, tab)
}

#' The carbonate class of field 46 (c0-c7, c3.2-c3.4) as % CaCO3 equivalent
#' @noRd
.ka5_carbonate_class <- function(x) .ka5_class_range(x, .KA5_CARBONATE_CLASSES)

#' The area class f1-f9 of Tab. 4 as % of the area
#' @noRd
.ka5_area_class <- function(x) .ka5_class_range(x, .KA5_AREA_CLASSES)

#' The hydromorphic features of fields 30 and 31 read into area shares
#'
#' Features separated by ";" (or "/"), each a comma list in the order of
#' 5.3 (kind, form, degree g1-g5, area class f1-f9, size); kinds joined by "+"
#' (ed+eh) share the area. Oxidative kinds (e, ed, eh, eo, es) give the rust
#' share, rb the wet bleaching, r (reductive), rg, rt and rs the reduction colours. Several
#' features of one kind add up (lower ends summed, upper ends summed and
#' capped at 100).
#' @return A list of c(lower, upper) ranges, % of the area: \code{rust},
#'   \code{bleached}, \code{reduction} (NA when the kind is not recorded),
#'   \code{marbled} (the form mr written), and \code{notes}.
#' @noRd
.ka5_hydromorphic_features <- function(x) {
  out <- list(rust = c(NA_real_, NA_real_), bleached = c(NA_real_, NA_real_),
              reduction = c(NA_real_, NA_real_), marbled = FALSE, notes = character(0))
  s <- trimws(as.character(x)[1L])
  if (is.na(s) || !nzchar(s)) return(out)
  add <- function(cur, r) if (anyNA(cur)) r else c(cur[1L] + r[1L], min(100, cur[2L] + r[2L]))
  for (f in strsplit(s, "[;/]")[[1L]]) {
    tok <- trimws(strsplit(f, ",", fixed = TRUE)[[1L]])
    tok <- tok[nzchar(tok)]
    if (!length(tok)) next
    kinds <- strsplit(tok[1L], "+", fixed = TRUE)[[1L]]
    if ("mr" %in% unlist(strsplit(tok, "+", fixed = TRUE))) out$marbled <- TRUE
    att <- unique(.KA5_HYDRO_KINDS[kinds[kinds %in% names(.KA5_HYDRO_KINDS)]])
    if (!length(att)) {
      out$notes <- c(out$notes, sprintf("'%s': no hydromorphic kind of Tab. 16", f))
      next
    }
    ac <- tok[grepl("^f[1-9]$", tok)]
    r <- if (length(ac)) .ka5_area_class(ac[1L]) else c(NA_real_, NA_real_)
    if (anyNA(r)) {
      out$notes <- c(out$notes, sprintf("'%s': no area class, the share undecided", f))
      r <- c(0, 100)
    }
    for (a in att) out[[a]] <- add(out[[a]], r)
  }
  out
}

#' A texture class of Tab. 30 (Ss, Sl2, Lu, Tu4 ...): its main group (S, L,
#' U, T), NA when the code is not one.
#' @noRd
.ka5_texture_class <- function(x) {
  k <- trimws(as.character(x)[1L])
  if (is.na(k) || !(k %in% names(.KA5_TEXTURE_CLASSES))) return(NA_character_)
  .KA5_TEXTURE_CLASSES[[k]]
}

#' The colour words of Liste 18 (roligrbn = reddish grey-brown): the words in
#' order, the main colour last, NULL when the code does not read. Never
#' converted to Munsell (p. 110).
#' @noRd
.ka5_colour_words <- function(x) {
  s <- tolower(trimws(as.character(x)[1L]))
  if (is.na(s) || !nzchar(s)) return(NULL)
  out <- character(0)
  p <- 1L
  n <- nchar(s)
  while (p <= n) {
    hit <- NA_character_
    for (w in c(paste0(.KA5_COLOURS, "li"), .KA5_COLOUR_BEFORE, .KA5_COLOURS)) {
      if (substr(s, p, p + nchar(w) - 1L) == w) { hit <- w; break }
    }
    if (is.na(hit)) return(NULL)
    out <- c(out, hit)
    p <- p + nchar(hit)
  }
  main <- out[out %in% .KA5_COLOURS]
  if (!length(main) || length(main) > 3L || !(out[length(out)] %in% .KA5_COLOURS)) return(NULL)
  list(words = out, main = main[length(main)])
}

#' A humus form recorded with its German name, as the value of site
#' humus_form (Rohhumus mor, Feuchtmull hydromull, Feuchtmoder hydromoder,
#' Feuchtrohhumus hydromor, L-Mull l-mull, F-Mull f-mull); the value itself
#' when it is one already, NA otherwise.
#' @noRd
.ka5_humus_form <- function(x) {
  k <- gsub("[^a-z]", "", .ka5_fold_umlauts(tolower(trimws(as.character(x)[1L]))))
  if (is.na(k) || !nzchar(k)) return(NA_character_)
  map <- c(rohhumus = "mor", feuchtmull = "hydromull", feuchtmoder = "hydromoder",
           feuchtrohhumus = "hydromor", lmull = "l-mull", fmull = "f-mull", tangel = "tangel",
           moder = "moder", mull = "mull", anmoor = "anmoor", torf = "tourbe", mor = "mor",
           hydromull = "hydromull", hydromoder = "hydromoder", hydromor = "hydromor")
  if (k %in% names(map)) unname(map[[k]]) else NA_character_
}
