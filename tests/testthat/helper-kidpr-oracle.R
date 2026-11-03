# v0.9.337: a literal walker of the keys to trunks (2.2) and divisions (2.3)
# of the Russian Field Guide of 2008, the test oracle of the flattened key
# (inst/rules/kidpr2008/key.yaml and divisions/).
#
# A profile is a vector of tokens, top-down, "CODE:thickness": a horizon
# ("AY:15"), an overlay after a "+" ("AY+S:15"), a buried horizon in brackets
# ("[AU]:20"), a rock mark after the code ("C~~:40", "C\":30"), a small
# horizon in lower case ("r:10", "e:2"), a transition of the notation
# ("AB:15", "BC:20"), the technogenic formation TPO ("TPO:60"); and two
# layers of the data route: one in which the key to horizons finds no
# diagnostic horizon ("_none:10", position 66), the same with an FAO or WRB
# designation after a tilde ("~AB:25", "~E/B:15": a transition when it names
# two master horizons) and the deposit of its position 38b ("_dep:60"); and a
# site flag, a recorded floodplain. The
# walker reads the profile with its own simple helpers (every layer decided;
# the codes from its own reading of the tokens, not the package's notation
# reader) and follows the book's branching steps in their printed order,
# each step a question with its answers; an answer with no exit is "not
# provided" (C.21). With `repaired = TRUE` it applies, at the step where each
# acts, the repairs and readings of Hugo's decisions of 2026-11-02 (Q9, Q10,
# Q20, Q21, and those taken after the review: every ploughed soil of CC or
# CJ is an agrozem; BAN is formed in volcanic ash) and the readings he
# accepted, and records the ones that changed the walk. The readings shared
# by both walks (the thickest-horizon rule of the surface, Q11, read by step
# 5 as by C.19; the deposit, Q21, TPO and the deposit of 38b in it; the peat
# body, Q22; the guard of a truncated soil, Q19; transitions and up to 15 cm
# with no diagnostic horizon between AU and BCA passed over, Q14; T read with
# TO and TE; C.14 read "directly under", as C.13 and the gley division text,
# p. 92, say) are not differences.

.KOR_ROCKS <- c("C", "D", "R", "CLM", "TT")
.KOR_AGRO <- c("P", "PU", "PT", "PTR", "PB")
.KOR_NATURAL <- c("AJ", "AY", "AU", "AO", "AK", "AH", "W", "O", "T", "TO", "TE", "TJ", "H")
.KOR_HUM4 <- c("AJ", "AY", "AU", "AO", "AK", "AH")
.KOR_ORG4 <- c("O", "T", "TO", "TE", "TJ", "H")
.KOR_ELUV <- c("E", "EL", "AEL", "BEL", "SEL")
.KOR_RSTAR <- c("RJ", "RY", "RU")
.KOR_TRANS <- c("AB", "BA", "BC", "CB", "AC")
# Codes that are no soil horizon of the profile: rock, TPO, and the data
# route's layers with no horizon and deposits.
.KOR_NOSOIL <- c(.KOR_ROCKS, "TPO", "_none", "_dep")

# One token as a layer.
.kor_layer <- function(tok) {
  p <- strsplit(tok, ":", fixed = TRUE)[[1L]]
  s <- p[1L]
  buried <- grepl("^\\[.*\\]$", s)
  if (buried) s <- sub("^\\[(.*)\\]$", "\\1", s)
  ov <- character(0)
  if (grepl("+", s, fixed = TRUE)) {
    q <- strsplit(s, "+", fixed = TRUE)[[1L]]
    s <- q[1L]; ov <- q[-1L]
  }
  m <- regmatches(s, regexpr("(~~|\"\"|\"|\\^\\^|=|\\.\\.)$", s))
  mark <- if (length(m)) m else NA_character_
  if (length(m)) s <- substr(s, 1L, nchar(s) - nchar(m))
  small <- grepl("^[a-z]", s)
  trans <- s %in% .KOR_TRANS
  # a layer of the data route with an FAO designation and no horizon
  fao <- if (startsWith(s, "~")) substring(s, 2L) else NA_character_
  if (!is.na(fao)) {
    m2 <- regmatches(fao, regexec("^([ABEC])[a-z0-9]*/?([ABEC])", fao))[[1L]]
    trans <- length(m2) == 3L && m2[2L] != m2[3L]
    # an FAO C is loose rock
    s <- if (grepl("^[0-9]*C[a-z0-9]*$", fao)) "C" else "_none"
  }
  list(code = if (small || trans) NA_character_ else s, feature = if (small) s else NA_character_,
       small = small, trans = trans, buried = buried, overlay = ov, mark = mark, thick = as.numeric(p[2L]),
       fao = fao)
}

# The profile as a list of layers with tops, and its readings.
.kor_profile <- function(tokens, floodplain = FALSE) {
  L <- lapply(tokens, .kor_layer)
  top <- 0
  for (i in seq_along(L)) { L[[i]]$top <- top; top <- top + L[[i]]$thick; L[[i]]$bottom <- top }
  pr <- list(L = L, tokens = tokens, floodplain = floodplain)
  code <- vapply(L, function(l) l$code %||% NA_character_, "")
  small <- vapply(L, `[[`, NA, "small")
  trans <- vapply(L, `[[`, NA, "trans")
  bur <- vapply(L, `[[`, NA, "buried")
  feat <- vapply(L, function(l) l$feature %||% NA_character_, "")
  # the deposit at the surface (Q21): TPO and the deposit of 38b always,
  # loose rock only over a buried soil or an organic or humus horizon
  run <- integer(0); i <- 1L
  while (i <= length(L) && !bur[i] &&
         ((!small[i] && code[i] %in% c(.KOR_RSTAR, "TPO", "_dep", "C", "D")) ||
          (small[i] && feat[i] %in% c("r", "rh", "ur", "rr", "rx")))) { run <- c(run, i); i <- i + 1L }
  if (length(run)) {
    ok <- i <= length(L) && (bur[i] || (!small[i] && code[i] %in% c(.KOR_ORG4, .KOR_HUM4, "W", .KOR_RSTAR)))
    if (!ok) {
      keep <- which(!code[run] %in% c("C", "D") | small[run])
      run <- if (length(keep)) run[seq_len(max(keep))] else integer(0)
    }
  }
  pr$dep <- run
  pr$dep_thick <- sum(vapply(L[run], `[[`, 0, "thick"))
  pr$dep_humified <- any(code[run] %in% .KOR_RSTAR)
  # the profile: under a deposit of 40 cm or less (a buried soil directly
  # under it promoted), else the layers that are not buried
  rows <- seq_along(L)
  b2 <- bur
  if (length(run) && pr$dep_thick <= 40) {
    rows <- setdiff(rows, run)
    if (length(rows) && b2[rows[1L]]) { k <- 1L; while (k <= length(rows) && b2[rows[k]]) { b2[rows[k]] <- FALSE; k <- k + 1L } }
  }
  pr$prof <- rows[!b2[rows]]
  # the horizons in order, sub-horizons merged, small ones and transitions
  # out (a layer with no horizon is an entry of its own)
  s <- pr$prof[!small[pr$prof] & !trans[pr$prof]]
  hz <- list()
  for (j in s) {
    if (length(hz) && identical(hz[[length(hz)]]$code, code[j]) && !code[j] %in% c("_none", "_dep")) {
      hz[[length(hz)]]$thick <- hz[[length(hz)]]$thick + L[[j]]$thick
      hz[[length(hz)]]$overlay <- unique(c(hz[[length(hz)]]$overlay, L[[j]]$overlay))
    } else hz[[length(hz) + 1L]] <- list(code = code[j], thick = L[[j]]$thick, top = L[[j]]$top, overlay = L[[j]]$overlay)
  }
  # the surface by the thickest-horizon rule (Q11)
  hc <- vapply(hz, `[[`, "", "code")
  run <- 0L
  while (run < length(hz) && hc[run + 1L] %in% c(.KOR_AGRO, .KOR_NATURAL)) run <- run + 1L
  drop <- integer(0)
  if (run) {
    th <- vapply(hz[seq_len(run)], `[[`, 0, "thick")
    nat <- which(hc[seq_len(run)] %in% .KOR_NATURAL)
    if (length(nat) > 1L) drop <- setdiff(nat, nat[which.max(th[nat])])
    if (length(nat) == 1L && hc[nat] == "W" && !any(hc[seq_len(run)] %in% .KOR_AGRO) && th[nat] <= 5 &&
        run < length(hz) && !hc[run + 1L] %in% .KOR_NOSOIL) drop <- nat
  }
  pr$hz_raw <- hz
  pr$hz <- hz[setdiff(seq_along(hz), drop)]
  pr$hc <- vapply(pr$hz, `[[`, "", "code")
  pr$run <- run - length(drop)
  rc <- pr$hc[seq_len(pr$run)]
  pr$surf <- if (any(rc %in% .KOR_AGRO)) rc[rc %in% .KOR_AGRO][1L] else if (length(rc)) rc[1L] else NA_character_
  pr$all_code <- code; pr$small <- small; pr$trans <- trans; pr$bur <- bur
  pr
}

# The readings of the walk.
# a V laid over another horizon is a feature, not a horizon of the key (Hugo, 2026-11-02)
.kor_ov <- function(o) setdiff(o, "V")
.kor_is <- function(pr, i, K) pr$hc[i] %in% K || any(.kor_ov(pr$hz[[i]]$overlay) %in% K)
.kor_has <- function(pr, K) any(vapply(seq_along(pr$hz), function(i) .kor_is(pr, i, K), NA))
.kor_has_within <- function(pr, K, cm) any(vapply(seq_along(pr$hz), function(i) .kor_is(pr, i, K) && pr$hz[[i]]$top < cm, NA))
.kor_first <- function(pr, K) length(pr$hz) > 0L && .kor_is(pr, 1L, K)
# directly over: transitions are out of the list; layers with no horizon
# passed over up to `gap` cm
.kor_over <- function(pr, U, Lw, gap = 0) {
  n <- length(pr$hz)
  for (i in seq_len(n)) {
    if (!.kor_is(pr, i, U)) next
    j <- i + 1L; g <- 0
    while (j <= n && pr$hc[j] == "_none" && g + pr$hz[[j]]$thick <= gap) { g <- g + pr$hz[[j]]$thick; j <- j + 1L }
    if (j <= n && .kor_is(pr, j, Lw)) return(TRUE)
  }
  FALSE
}
.kor_below <- function(pr, U, Lw) {
  n <- length(pr$hz)
  if (n < 2L) return(FALSE)
  for (i in seq_len(n - 1L)) for (j in seq.int(i + 1L, n)) if (.kor_is(pr, i, U) && .kor_is(pr, j, Lw)) return(TRUE)
  FALSE
}
.kor_set <- function(q) sort(unique(c(q$hc[!q$hc %in% .KOR_NOSOIL], .kor_ov(unlist(lapply(q$hz, `[[`, "overlay"))))))
.kor_only <- function(pr, sets, raw = FALSE) {
  q <- if (raw) list(hc = vapply(pr$hz_raw, `[[`, "", "code"), hz = pr$hz_raw) else pr
  any(vapply(sets, function(s) identical(.kor_set(q), sort(unique(s))), NA))
}
.kor_rock30 <- function(pr) any(vapply(pr$L, function(l) identical(l$code, "R") && !l$buried && l$top <= 30, NA))
.kor_tur40 <- function(pr) any(vapply(pr$hz, function(e) identical(e$code, "TUR") && e$thick > 40, NA))
# step 6: more than 50 cm of peat (51 cm or more, as the book says)
.kor_peat50 <- function(pr) {
  tot <- 0
  for (j in pr$prof) {
    if (pr$small[j]) next
    if (pr$all_code[j] %in% c(.KOR_ORG4, "TT", "PT", "PTR")) tot <- tot + pr$L[[j]]$thick else break
  }
  tot > 50
}
.kor_strat40 <- function(pr) {
  tot <- 0; rs <- FALSE
  for (j in seq_along(pr$L)) {
    if (pr$bur[j]) break
    if (pr$small[j]) next
    if (pr$all_code[j] %in% c(.KOR_RSTAR, "P", "PU")) { tot <- tot + pr$L[[j]]$thick; rs <- rs || pr$all_code[j] %in% .KOR_RSTAR } else break
  }
  rs && tot >= 40
}
# step 1: a soil horizon (a transition is one) within about 1 m
.kor_soil100 <- function(pr) any(vapply(pr$L, function(l) !l$small && (l$trans || !l$code %in% .KOR_NOSOIL) && l$top < 100, NA))
.kor_marks <- function(pr, m) any(vapply(pr$L, function(l) identical(l$mark, m), NA))
.kor_floodplain <- function(pr) isTRUE(pr$floodplain) && any(vapply(pr$L[pr$prof], function(l) identical(l$mark, "~~"), NA))
.kor_ash <- function(pr) .kor_marks(pr, "\"")
.kor_remnant <- function(pr) {
  # an agro horizon at the surface with no natural horizon under it
  agro <- !is.na(pr$surf) && pr$surf %in% .KOR_AGRO
  !agro || .kor_over(pr, .KOR_AGRO, c(.KOR_HUM4, .KOR_ORG4, .KOR_RSTAR))
}

# The profile under the agro horizon, as a natural soil: the agro horizons
# taken out, the natural horizon under them (if any) the surface, else the
# natural counterpart of the agro horizon (P: AY, PU: AU, PT and PTR: T;
# none for PB).
.kor_natural <- function(pr) {
  q <- pr
  ag <- which(pr$hc %in% .KOR_AGRO)
  q$hz <- pr$hz[setdiff(seq_along(pr$hz), ag)]
  q$hc <- pr$hc[setdiff(seq_along(pr$hc), ag)]
  rem <- if (length(q$hc) && q$hc[1L] %in% .KOR_NATURAL) q$hc[1L] else NA_character_
  q$surf <- if (!is.na(rem)) rem else c(P = "AY", PU = "AU", PT = "T", PTR = "T", PB = NA_character_)[[pr$surf]]
  q
}

#' The literal walk of keys 2.2 and 2.3 (`repaired = FALSE`), or the walk
#' with the repairs and readings (`repaired = TRUE`). Returns the trunk, the
#' division (NA: none; the reason in `why`) and the repairs used.
.kor_walk <- function(pr, repaired = FALSE) {
  used <- character(0)
  fix <- function(what) used <<- c(used, what)
  out <- function(trunk, division = NA_character_, why = NA_character_)
    list(trunk = trunk, division = division, why = why, used = unique(used))
  # --- key 2.2 ---
  if (!.kor_soil100(pr)) return(out("E", why = "1b"))
  b4 <- FALSE
  if (length(pr$dep) && pr$dep_thick > 40) {
    if (pr$dep_humified) b4 <- TRUE else return(out("E", why = "4b"))
  }
  if (!b4) {
    # step 5: only O or only W, read after the thickest-horizon rule
    if (.kor_only(pr, list("O", "W"))) return(out("D", "[DA]"))
    if (repaired && .kor_only(pr, list("S")) && .kor_over(pr, "S", c("C", "D"))) {
      fix("Q10: trunk D takes S alone over C"); return(out("D", "[DA]"))
    }
    # step 6: the peat body
    if (.kor_peat50(pr)) return(out("A", if (.kor_has(pr, c("PT", "PTR"))) "AB" else "AA"))
    # step 7
    lit7 <- isTRUE(pr$floodplain) || .kor_marks(pr, "~~") || .kor_marks(pr, "\"") || .kor_marks(pr, "^^") ||
      .kor_marks(pr, "\"\"") || any(pr$all_code %in% .KOR_RSTAR)
    ba <- .kor_floodplain(pr) && .kor_remnant(pr)
    bb <- .kor_has(pr, "BAN")
    bc <- .kor_strat40(pr)
    if (repaired) {
      b7 <- ba || bb || bc
      if (bb && !lit7) fix("decision: BAN is formed in volcanic ash")
      if (lit7 && !b7) fix(if (.kor_floodplain(pr) && !.kor_remnant(pr))
                             "Q9: an agro horizon with no natural remnant on alluvium is an agrozem"
                           else "Q21: step 7 only for what a division of trunk B takes")
    } else b7 <- lit7
    if (!b7) return(.kor_walk_c(pr, repaired, used))
  }
  # --- trunk B (B.1, B.2) ---
  if (.kor_floodplain(pr)) {
    if (!repaired || .kor_remnant(pr)) return(out("B", "BA"))
    fix("Q9: an agro horizon with no natural remnant on alluvium is an agrozem")
    return(.kor_walk_c(pr, repaired, used))
  }
  if (.kor_has(pr, "BAN") && .kor_ash(pr)) return(out("B", "BB"))
  if (repaired && .kor_has(pr, "BAN")) { fix("decision: BAN is formed in volcanic ash"); return(out("B", "BB")) }
  if (!repaired) return(if (any(pr$all_code %in% .KOR_RSTAR)) out("B", "BC") else out("B", NA_character_, "B.2: no exit"))
  if (b4 || .kor_strat40(pr)) return(out("B", "BC"))
  out("B", NA_character_, "B.2: no exit")
}

# The walk of trunk C (key 2.3, C.1-C.23).
.kor_walk_c <- function(pr, repaired, used) {
  fix <- function(what) used <<- c(used, what)
  out <- function(division = NA_character_, why = NA_character_)
    list(trunk = "C", division = division, why = why, used = unique(used))
  np <- function(step) out(NA_character_, step)
  # C.1, C.2
  if (.kor_rock30(pr)) {
    c2 <- c("T", "TO", "TE", "TJ", "H", "AO", "AJ", "AU", "AH", "P", "PU")
    if (.kor_has(pr, c2)) return(out("CP"))
    if (repaired && .kor_has(pr, c("AY", "AK"))) { fix("Q10: C.2 takes AY and AK"); return(out("CP")) }
    return(np("C.2b"))
  }
  # C.3
  if (.kor_has_within(pr, c("S", "SS"), 10)) return(out("CL"))
  if (.kor_has_within(pr, "X", 30)) return(out("CU"))
  # C.4
  c4 <- .kor_first(pr, c(.KOR_HUM4, .KOR_ORG4, .KOR_AGRO)) || .kor_has(pr, .KOR_ELUV)
  if (!c4 && repaired && .kor_first(pr, "TUR")) { fix("Q10: a TUR at the surface counts at C.4"); c4 <- TRUE }
  if (!c4) {
    # C.20
    if (.kor_first(pr, c("BI", "BHF", "BFM", "BM", "BPL", "CRM", "BCA", "CAT", "ASN", "BSN", "V"))) return(out("CQ"))
    if (repaired && .kor_first(pr, "BT")) { fix("Q15: an exposed BT counts as BI at C.20"); return(out("CQ")) }
    return(np("C.20b"))
  }
  # C.5
  mid5 <- c("BT", "BI", "BHF", "BFM", "BM", "BPL", "CRM", "CR", "BCA", "CAT", "ASN", "BSN", "G", "Q", "TUR")
  c5 <- .kor_has(pr, mid5)
  if (!c5 && repaired && .kor_has(pr, c("V", "CRH"))) { fix("reading: V and CRH are middle horizons at C.5"); c5 <- TRUE }
  if (!c5) {
    # C.18
    if (.kor_below(pr, c(.KOR_HUM4, .KOR_ORG4, .KOR_AGRO), c("E", "EL"))) return(out("CO"))
    # C.19
    a <- .kor_has(pr, c("P", "PU"))
    a2 <- .kor_has(pr, c("PT", "PTR"))
    b <- .kor_only(pr, list("PB"))
    v1 <- .kor_only(pr, list("AJ", "AY", "AU", "AO", "AH", "H", "TJ", c("P", "AY"), c("PU", "AU")))
    v2 <- .kor_only(pr, list("AK"))
    if (!repaired) {
      if (a) return(out("CS"))
      if (b) return(out("CR"))
      if (v1) return(out("CN"))
      return(np("C.19"))
    }
    if (v1 || v2) {
      if (!v1) fix("Q10: C.19v takes AK")
      if (a) fix("Q9: C.19v before C.19a")
      return(out("CN"))
    }
    if (a || a2) { if (!a) fix("Q10: C.19a takes PT and PTR"); return(out("CS")) }
    if (b) return(out("CR"))
    return(np("C.19"))
  }
  # C.6
  over6 <- .kor_over(pr, c(.KOR_AGRO, "TUR"), c("BT", "BI", "BHF", "BM", "BCA", "CAT", "G", "Q"))
  list6 <- .kor_has(pr, c("BFM", "BPL", "CRM", "CR", "CRH", "BMK"))
  b6 <- .kor_below(pr, c("P", "PU", "PT", "PTR"), c(.KOR_HUM4, .KOR_ORG4, .KOR_ELUV)) || list6 || !.kor_has(pr, .KOR_AGRO)
  to <- if (!repaired) {
    if (over6) "C.15" else if (b6) "C.7" else "none"
  } else {
    ploughed <- pr$surf %in% .KOR_AGRO &&
      .kor_walk_c7(.kor_natural(pr), repaired, character(0))$division %in% c("CC", "CJ")
    if (.kor_tur40(pr)) { if (!over6) fix("Q10: a TUR over 40 cm is tested at C.6"); "CT" }
    else if (ploughed) {
      fix("decision: a ploughed CC or CJ soil is an agrozem")
      if (identical(pr$surf, "PB")) "CR" else "CS"
    }
    else if (list6) { if (over6) fix("Q9: C.6b over C.6a with BFM, BPL, CRM, CR, CRH or BMK"); "C.7" }
    else if (over6) "C.15"
    else { if (!b6) fix("reading: C.6 with no exit goes on to C.7"); "C.7" }
  }
  if (to == "none") return(np("C.6"))
  if (to %in% c("CT", "CS", "CR")) return(out(to))
  if (to == "C.15") {
    if (.kor_has(pr, "TUR")) {
      if (!repaired) return(out("CT"))
      fix("Q20: a TUR of 40 cm or less goes on to C.16")
    }
    if (identical(pr$surf, "PB")) return(out("CR"))
    if (pr$surf %in% c("P", "PU", "PT", "PTR")) return(out("CS"))
    return(np("C.16"))
  }
  r <- .kor_walk_c7(pr, repaired, used)
  r$used <- unique(c(used, r$used))
  r
}

# The walk from C.7 on.
.kor_walk_c7 <- function(pr, repaired, used) {
  fix <- function(what) used <<- c(used, what)
  out <- function(division = NA_character_, why = NA_character_)
    list(trunk = "C", division = division, why = why, used = unique(used))
  np <- function(step) out(NA_character_, step)
  # C.7
  if (.kor_has(pr, "BFM")) return(out("CC"))
  if (.kor_has(pr, "BPL")) return(out("CF"))
  if (.kor_has(pr, "CR")) return(out("CG"))
  if (.kor_has(pr, "CRM")) return(out(if (.kor_below(pr, "G", "CRM")) "CH" else "CE"))
  if (.kor_has(pr, "BMK")) return(out("CJ"))
  if (.kor_has(pr, c("ASN", "BSN"))) return(out("CK"))
  # C.8
  if (.kor_has(pr, "BHF")) return(out("CB"))
  if (.kor_has(pr, "BT")) return(if (.kor_below(pr, c("EL", "AEL", "BEL"), "BT")) out("CA") else np("C.9b"))
  if (.kor_has(pr, "BM")) {
    if (identical(pr$surf, "AJ")) return(out("CJ"))
    if (pr$surf %in% c("O", "AO", "AY", "AU", "P", "PU")) return(out("CD"))
    return(np("C.10v"))
  }
  if (!.kor_has(pr, c("BI", "BCA", "CAT", "V", "CRH", "G", "Q"))) return(np("C.8"))
  # C.11
  s <- pr$surf
  if (identical(s, "AJ")) return(if (.kor_has(pr, c("BCA", "CAT"))) out("CJ") else np("C.12b"))
  if (identical(s, "AU") || (identical(s, "PU") && .kor_over(pr, "PU", "AU"))) {
    # C.13: directly under AU; up to 15 cm with no diagnostic horizon over BCA
    if (.kor_over(pr, "AU", c("BI", "CAT", "V", "CRH")) || .kor_over(pr, "AU", "BCA", gap = 15)) return(out("CI"))
    if (.kor_over(pr, "AU", "G")) return(out("CH"))
    if (.kor_over(pr, "AU", "Q")) return(out("CM"))
    return(np("C.13"))
  }
  if (s %in% c("O", "T", "TO", "TE", "H", "PT", "PU")) {
    # C.14: directly under the organic or agro horizon (the key prints
    # "under"; the gley division text, p. 92, says directly under and that
    # another mineral horizon leaves the soil out of the gley soils)
    if (.kor_over(pr, c("O", "T", "TO", "TE", "H", "PT", "PU"), "G")) return(out("CH"))
    if (.kor_over(pr, c("O", "T", "TO", "TE", "H", "PT", "PU"), "Q")) return(out("CM"))
    return(np("C.14"))
  }
  if (repaired && s %in% c("AY", "AO") && .kor_over(pr, c("AY", "AO"), "G")) {
    fix("Q10: AY or AO directly over G is a gley soil")
    return(out("CH"))
  }
  np("C.11")
}

# The profile as the frame identify_horizons() gives, the site, and the
# view the flattened key reads. The data route's layers carry the outcome of
# the key to horizons in key_path.
.kor_view <- function(pr) {
  L <- pr$L
  n <- length(L)
  code <- vapply(L, function(l) l$code %||% NA_character_, "")
  fao <- vapply(L, function(l) l$fao %||% NA_character_, "")
  data <- code %in% c("_none", "_dep") | !is.na(fao)
  # the data route's frame marks no transition: the view reads the FAO
  # designation
  fr <- data.frame(horizon_idx = seq_len(n), top_cm = vapply(L, `[[`, 0, "top"),
                   bottom_cm = vapply(L, `[[`, 0, "bottom"), designation = ifelse(data, fao, pr$tokens),
                   code = ifelse(data, NA_character_, code),
                   small = vapply(L, `[[`, NA, "small"), transitional = vapply(L, `[[`, NA, "trans") & !data,
                   buried = as.integer(vapply(L, `[[`, NA, "buried")),
                   mark = vapply(L, function(l) l$mark %||% NA_character_, ""),
                   route = ifelse(data, "data", "notation"), stringsAsFactors = FALSE)
  fr$passed <- !fr$small & !fr$transitional & !data
  fr$key_path <- ifelse(code %in% "_none" | !is.na(fao), "66\u0430: _none",
                        ifelse(code %in% "_dep", "38\u0431: _deposit", NA_character_))
  fr$overlay <- lapply(L, `[[`, "overlay")
  fr$features <- lapply(L, function(l) if (l$small) l$feature else character(0))
  site <- if (isTRUE(pr$floodplain)) list(landform = "floodplain") else list()
  soilKey:::.kidpr_dv(fr, site, NULL)
}

# The flattened key on a token profile: the trunk and division the engine
# assigns (NA: none), and whether any class was left undecided.
.kor_flat <- function(pr, rules = .kor_rules()) {
  v <- .kor_view(pr)
  t <- soilKey:::.run_level(v, rules$trunks, "trunks")
  if (is.null(t$assigned)) return(list(trunk = NA_character_, division = NA_character_, undecided = TRUE))
  tc <- t$assigned$code
  d <- soilKey:::.run_level(v, rules$divisions[[tc]] %||% list(), "divisions")
  list(trunk = tc, division = if (is.null(d$assigned)) NA_character_ else d$assigned$code,
       undecided = length(t$undecided) + length(d$undecided) > 0L)
}

# The same profile end to end: a PedonRecord in the Russian notation through
# classify_kidpr2008() (the notation reader, identify_horizons() and the
# view of the pedon). Not for the data route's tokens.
.kor_pedon <- function(tokens, floodplain = FALSE) {
  d <- sub(":[0-9.]+$", "", tokens)
  th <- as.numeric(sub("^.*:", "", tokens))
  bot <- cumsum(th)
  h <- data.frame(designation = d, top_cm = c(0, bot[-length(bot)]), bottom_cm = bot, stringsAsFactors = FALSE)
  site <- list(id = "kor", designation_system = "kidpr")
  if (isTRUE(floodplain)) site$landform <- "floodplain"
  PedonRecord$new(site = site, horizons = ensure_horizon_schema(data.table::as.data.table(h)))
}
.kor_e2e <- function(tokens, floodplain = FALSE) {
  r <- classify_kidpr2008(.kor_pedon(tokens, floodplain), on_missing = "silent")
  lv <- r$levels
  list(trunk = if (nrow(lv)) lv$code[1L] else NA_character_,
       division = if (nrow(lv) > 1L) lv$code[2L] else NA_character_,
       # the trunk and the division only: an undecided type, or the original
       # soil of a chemozem (v0.9.342), is no answer of the keys 2.2 and 2.3
       undecided = any(grepl("^(trunks|divisions):", r$provisional_because)))
}

.kor_rules_cache <- new.env(parent = emptyenv())
.kor_rules <- function() {
  if (is.null(.kor_rules_cache$r)) .kor_rules_cache$r <- load_rules("kidpr2008")
  .kor_rules_cache$r
}

# A label "trunk/division" of an outcome.
.kor_label <- function(o) paste0(o$trunk, "/", ifelse(is.na(o$division), "-", o$division))

# The index of an item of a profile formula, by the walker's own reading: the
# capitals that start it (BCAs,cs: BCA; BTg: BT), a rock flag G or Q after C
# or D left out (CG, CQca: C; DG: D), the modifications BH and BF read as
# BHF and AV as V, the turbated composition in brackets and the optional
# features in parentheses dropped; the mark of the rock kept.
.KOR_INDICES <- c("O", "T", "TO", "TE", "TJ", "TT", "H", "AY", "AU", "AJ", "AO", "AK", "AH", "W", "P", "PU",
                  "PT", "PTR", "PB", "RJ", "RY", "RU", "E", "EL", "AEL", "BEL", "SEL", "BT", "BI", "BHF", "BFM",
                  "BM", "BMK", "BPL", "CRM", "CR", "CRH", "BCA", "CAT", "ASN", "BSN", "V", "BAN", "F", "G", "Q",
                  "S", "SS", "X", "TUR", "ML", "C", "D", "R", "CLM", "TPO")
.kor_index <- function(item) {
  s <- sub("\\[.*\\]$", "", item)
  s <- gsub("\\([^)]*\\)", "", s)
  m <- regmatches(s, regexpr("(~~|\"\"|\"|\\^\\^|=|\\.\\.|\u22a5)$", s))
  mark <- if (length(m)) m else NA_character_
  if (length(m)) s <- substr(s, 1L, nchar(s) - nchar(m))
  cap <- regmatches(s, regexpr("^[A-Z]+", s))
  code <- if (!length(cap)) NA_character_
          else if (cap %in% c("BH", "BF")) "BHF" else if (cap == "AV") "V"
          else if (cap %in% .KOR_INDICES) cap
          else if (substr(cap, 1L, 1L) %in% c("C", "D") && substring(cap, 2L) %in% c("G", "Q")) substr(cap, 1L, 1L)
          else NA_character_
  list(code = code, mark = if (identical(mark, "\u22a5")) NA_character_ else mark)
}

# A profile formula of chapter 4 ("AU-EL-(BEL)-BT-C") as a token profile of
# plausible thicknesses for its division: the optional horizon in
# parentheses written or left out, a buried soil ([ABC], {ABC}) as two buried
# horizons, the TUR of the turbozems over 40 cm and that of the solonetzic
# agrozems thinner, the peat under an agro-peat horizon and the horizons over
# the rock of a lithozem thin.
.kor_formula_tokens <- function(formula, division, optional = TRUE) {
  f <- sub(" \\(.*\\)$", "", formula)
  items <- strsplit(f, "-(?![^\\[]*\\])", perl = TRUE)[[1L]]
  th <- c(O = 5, W = 3, T = 30, TO = 30, TE = 30, TJ = 30, H = 20, AY = 15, AU = 30, AJ = 15, AO = 8,
          AK = 10, AH = 20, P = 25, PU = 30, PT = 25, PTR = 25, PB = 25, RJ = 50, RY = 50, RU = 50,
          E = 10, EL = 15, AEL = 10, BEL = 15, SEL = 8, G = 30, Q = 30, F = 15, ML = 20, S = 10, SS = 10,
          C = 30, D = 30, CLM = 30, R = 30, TT = 60)
  toks <- character(0); prev <- ""
  for (it in items) {
    if (grepl("^\\(.*\\)$", it)) {
      if (!optional) next
      it <- sub("^\\((.*)\\)$", "\\1", it)
    }
    if (it %in% c("[ABC]", "{ABC}")) { toks <- c(toks, "[AU]:20", "[BM]:30"); next }
    d <- .kor_index(it)
    code <- d$code
    if (is.na(code)) stop(sprintf("the walker cannot read the formula item '%s'", it))
    mark <- d$mark
    t <- if (code %in% names(th)) th[[code]] else 30
    if (code == "TUR") t <- if (division == "CT") 50 else 20
    if (code == "T" && prev %in% c("PT", "PTR")) t <- 15
    if (division == "CP" && code != "R") t <- if (code == "C") 10 else 15
    toks <- c(toks, sprintf("%s%s:%s", code, if (is.na(mark)) "" else mark, t))
    prev <- code
  }
  toks
}
