# =============================================================================
# v0.9.344 -- the genetic features of the Russian soil classification (Field
# Guide of 2008, section 3.2, pp. 59-74, and the subtype lists of chapter
# 4): one test per feature, shared by every division that lists it.
#
# A feature is written in lower case to the right of a horizon index (BTg,
# BCAs,cs), or alone as a small horizon (O-e-BHF-C); the subtypes of chapter
# 4 name the soils by them. Each test here reads one feature on one layer
# (or on the whole profile, for u, ro, the permafrost sign, the deposited
# layers r, rh, ur, rr, rx and the contaminated x), three-valued, and says
# what it read and what would decide it. The order of the evidence is the
# one Hugo set on 2026-11-02 (the defaults of the questions):
#
#   1. the recorded data, where an attribute records the same thing the
#      feature describes (the proxies of Q17: g, q, v, s, cs, nn, i, the
#      columnar sn, BF and BH by the carbon, an by the oxalates, the sapric
#      h, lc, mc, nc and dc by the form of the carbonates, ml, the
#      permafrost sign, @, tr, y, [hh], ro, r, rh, ur, rr; the key to
#      horizons 2.1 on the data for the small horizons and the features it
#      ends in: ao, e, el, r, rh, tr, u, ca; soilKey's WRB diagnostics only
#      where Q17 lists a proxy, flagged);
#   2. the Russian notation where the data say nothing (a feature not
#      written on a layer of a profile written in it is absent), and the
#      hints of another notation's letters (the FAO g, y, z, k, @, f), which
#      can only add a feature;
#   3. a weak reading of the data (the key to horizons did not reach the
#      feature, with nothing recorded to the contrary);
#   4. the defaults: a field observation not recorded is absent; a
#      measurement not recorded leaves the feature undecided.
#
# Some features have no attribute that records them: hi, pl, crm, f, el (on
# a horizon), ic, agr, ad, pr, ff, fn, ocr and yu are read from the notation
# only (Q17), with mr, [e-hf], [akl], cf and ct (soilKey's reading). x is an
# X horizon in a soil the key did not send to the chemozems; rx a toxic
# layer of 5-40 cm over a buried humus horizon; ur the same layer with 20 %
# or more of artefacts (Q17).
#
# A feature holds on the horizons the text puts it on (its hosts, with the
# analogies of Q17: pl in CRM, cf at the edge of CR); a list can name the
# host (ELf: f on EL; AUel: el in AU) and the place (the double el of CI and
# CM: in the AU, solodised; under the AU, eluviated). The metadata of every
# test (hosts, place, route, kind) are in
# inst/rules/kidpr2008/feature_tests.yaml; the dispatcher
# .kidpr_features() runs a list of (feature, host) entries on a pedon and
# returns one row per entry. The subtypes and their names follow (part 9).
# =============================================================================

# Horizon groups the host lists of feature_tests.yaml may name.
.KIDPR_FT_GROUPS <- list(
  humus = c("AY", "AJ", "AU", "AH", "AK", "AO", "W", "RU", "RY", "RJ"),
  organic = c("O", "T", "TO", "TE", "TJ", "H"),
  peat = c("T", "TO", "TE", "TJ", "TT"),
  agro = c("P", "PU", "PB", "PT", "PTR"),
  eluvial = c("E", "EL", "AEL", "BEL", "SEL"),
  middle = c("BHF", "BFM", "BT", "BI", "BM", "BMK", "CRM", "CR", "CRH", "BPL", "BAN", "BCA",
             "CAT", "ASN", "BSN", "V", "TUR"),
  hydrogenic = c("G", "Q", "F", "ML"),
  halomorphic = c("S", "SS"),
  parent = c("C", "D", "CLM")
)
.KIDPR_FT_GROUPS$mineral <- unique(c(.KIDPR_FT_GROUPS$humus, "P", "PU", "PB", .KIDPR_FT_GROUPS$eluvial,
                                     .KIDPR_FT_GROUPS$middle, .KIDPR_FT_GROUPS$hydrogenic,
                                     .KIDPR_FT_GROUPS$halomorphic, .KIDPR_FT_GROUPS$parent))

# Humus horizon = 1.724 x OC (Tyurin): BH over 3 % humus is C over 1.74 %;
# pb over 1.5 % humus is C over 0.87 %.
.KIDPR_FT_BH_OC <- 1.74
.KIDPR_FT_PB_OC <- 0.87
# CaCO3 = 2.274 x CO2: the CO2 limit of 30 % of the feature ml.
.KIDPR_FT_ML_CACO3 <- 68.2


# --- the metadata -------------------------------------------------------------------

#' The metadata of the feature tests (inst/rules/kidpr2008/feature_tests.yaml):
#' one entry per feature, by its canonical code.
#' @noRd
.kidpr_ft_meta <- function() {
  if (is.null(.kidpr_cache$ft_meta)) {
    f <- system.file("rules", "kidpr2008", "feature_tests.yaml", package = "soilKey")
    if (!nzchar(f)) rlang::abort("The feature tests of the Russian classification are not installed.")
    m <- yaml::read_yaml(f)$features
    names(m) <- vapply(m, function(e) as.character(e$code), "")
    .kidpr_cache$ft_meta <- m
  }
  .kidpr_cache$ft_meta
}


# --- a test's answer ------------------------------------------------------------------

# The answer of a test on the data: the verdict, and how firmly the data say
# it (`said`): TRUE when an attribute records it, FALSE when nothing
# recorded decides it (a weak reading the notation overrides, or nothing at
# all). `layers` for the tests of the whole profile; `kind` overrides the
# default of the feature (observed: not recorded is absent; measured: not
# recorded is undecided).
.kidpr_fd <- function(passed, said = TRUE, missing = character(0), note = character(0),
                      tag = character(0), layers = integer(0), kind = NULL) {
  r <- .kidpr_r(passed, missing = missing, note = note, tag = tag)
  r$said <- isTRUE(said)
  r$layers <- as.integer(layers)
  r$kind <- kind
  r
}

# A feature no recorded attribute says: read from the notation only.
.kidpr_ft_nota <- function(f) {
  .kidpr_fd(NA, said = FALSE,
            note = sprintf("%s: no recorded attribute says it; read from the Russian notation only", f))
}


# --- the profile ---------------------------------------------------------------------

#' The profile as the feature tests read it: the profile of the readers
#' (.kidpr_profile()), the identified horizons (.kidpr_identify()), the code
#' of every layer (a layer of another notation designated C in which the key
#' finds no horizon is loose rock C) and the division the soil keyed to.
#' @noRd
.kidpr_ft_context <- function(pedon, division = NA_character_) {
  if (inherits(pedon, "kidpr_ft_context")) return(pedon)
  x <- .kidpr_profile(pedon)
  fr <- .kidpr_identify(pedon, .profile = x)
  ctx <- new.env(parent = emptyenv())
  class(ctx) <- "kidpr_ft_context"
  ctx$x <- x
  ctx$fr <- fr
  ctx$division <- as.character(division %||% NA_character_)[1L]
  code <- fr$code
  for (i in seq_len(x$n)) {
    if (is.na(code[i]) && !isTRUE(fr$small[i]) && identical(fr$route[i], "data") &&
        .kidpr_dv_fao_c(fr$designation[i])) code[i] <- "C"
  }
  ctx$code <- code
  ctx$cand <- lapply(fr$candidates, as.character)
  ctx$small <- as.logical(fr$small) %in% TRUE
  # the layers in depth order, fresh litter left out
  ctx$ord <- x$ord[!vapply(x$ord, function(i) .kidpr_litter(x, i), NA)]
  ctx$main <- ctx$ord[!ctx$small[ctx$ord]]
  ctx$memo <- new.env(parent = emptyenv())
  ctx
}

# The layer as a text: its designation and depth.
.kidpr_ft_lab <- function(ctx, i) {
  d <- ctx$fr$designation[i]
  dep <- sprintf("%s-%s cm", format(ctx$x$top[i]), format(ctx$x$bot[i]))
  if (!is.na(d) && nzchar(d)) sprintf("%s (%s)", d, dep) else dep
}

# The layer above or below one layer that is not a small horizon.
.kidpr_ft_prev <- function(ctx, i) {
  k <- match(i, ctx$ord)
  if (is.na(k) || k == 1L) return(NA_integer_)
  up <- ctx$ord[seq_len(k - 1L)]
  up <- up[!ctx$small[up]]
  if (length(up)) up[length(up)] else NA_integer_
}
.kidpr_ft_next <- function(ctx, i) {
  k <- match(i, ctx$ord)
  if (is.na(k) || k == length(ctx$ord)) return(NA_integer_)
  dn <- ctx$ord[seq.int(k + 1L, length(ctx$ord))]
  dn <- dn[!ctx$small[dn]]
  if (length(dn)) dn[1L] else NA_integer_
}

# The host codes of a list (groups expanded), less `not_on`; "any" for any
# layer.
.kidpr_ft_hosts <- function(hosts, not_on = character(0)) {
  hosts <- as.character(unlist(hosts))
  if (!length(hosts) || "any" %in% hosts) return("any")
  out <- unlist(lapply(hosts, function(h) .KIDPR_FT_GROUPS[[h]] %||% h))
  setdiff(unique(out), as.character(unlist(not_on)))
}

# Is a layer (not a small horizon) one of the hosts: TRUE, FALSE, or NA when
# the data leave it undecided between a host and another horizon.
.kidpr_ft_is_host <- function(ctx, j, H) {
  if (is.na(j) || ctx$small[j]) return(FALSE)
  cd <- ctx$code[j]
  if (identical(H, "any")) return(TRUE)
  if (!is.na(cd)) return(cd %in% H)
  cand <- ctx$cand[[j]]
  cand <- cand[nzchar(cand)]
  if (!length(cand) || isFALSE(ctx$fr$passed[j])) return(FALSE)
  if (all(cand %in% H)) TRUE else if (any(cand %in% H)) NA else FALSE
}

# The layers a feature is read on, with how sure their host is: `place` on
# (the host layers, and the small horizons beside them when the feature may
# form one), under or over (the layer directly under or over a host, and
# the small horizons between), near (on, under or over), in (the host
# layers, and the small horizons inside the host), surface (the top of the
# soil).
.kidpr_ft_candidates <- function(ctx, H, place = "on", small_ok = FALSE) {
  rows <- integer(0); host <- logical(0)
  add <- function(r, h) {
    if (is.na(r) || isFALSE(h)) return(invisible())
    k <- match(r, rows)
    if (is.na(k)) { rows <<- c(rows, r); host <<- c(host, h) }
    else if (is.na(host[k]) && isTRUE(h)) host[k] <<- TRUE
  }
  smalls_between <- function(a, b) {
    ka <- if (is.na(a)) 0L else match(a, ctx$ord)
    kb <- if (is.na(b)) length(ctx$ord) + 1L else match(b, ctx$ord)
    if (kb - ka < 2L) return(integer(0))
    s <- ctx$ord[seq.int(ka + 1L, kb - 1L)]
    s[ctx$small[s]]
  }
  if (place == "surface") {
    if (length(ctx$ord)) {
      add(ctx$ord[1L], TRUE)
      if (length(ctx$main)) add(ctx$main[1L], TRUE)
    }
    return(list(rows = rows, host = host))
  }
  for (j in ctx$main) {
    h <- .kidpr_ft_is_host(ctx, j, H)
    if (isFALSE(h)) next
    p <- .kidpr_ft_prev(ctx, j); q <- .kidpr_ft_next(ctx, j)
    if (place %in% c("on", "near", "in")) add(j, h)
    # under and over read the boundary of the run of host layers: a host
    # layer under or over a host, and the small horizons between them, are
    # inside it
    if (place %in% c("under", "near") && !isTRUE(.kidpr_ft_is_host(ctx, q, H))) {
      add(q, h)
      if (small_ok || place == "under") for (s in smalls_between(j, q)) add(s, h)
    }
    if (place %in% c("over", "near") && !isTRUE(.kidpr_ft_is_host(ctx, p, H))) {
      add(p, h)
      if (small_ok || place == "over") for (s in smalls_between(p, j)) add(s, h)
    }
    if (place == "on" && small_ok) {
      for (s in c(smalls_between(p, j), smalls_between(j, q))) add(s, h)
    }
  }
  if (place == "in") {
    for (s in ctx$ord[ctx$small[ctx$ord]]) {
      p <- .kidpr_ft_prev(ctx, s); q <- .kidpr_ft_next(ctx, s)
      hp <- .kidpr_ft_is_host(ctx, p, H); hq <- .kidpr_ft_is_host(ctx, q, H)
      add(s, .kidpr_and(hp, hq))
    }
  }
  o <- order(match(rows, ctx$ord))
  list(rows = rows[o], host = host[o])
}

# The lithological layer of a row: the number the notation reads, else a
# leading Arabic or Roman numeral of the designation.
.kidpr_ft_lith <- function(ctx, i) {
  d <- ctx$x$des
  if (isTRUE(d$known[i])) return(as.integer(d$lith[i] %||% 1L))
  s <- as.character(ctx$fr$designation[i])
  if (is.na(s)) return(1L)
  m <- regmatches(s, regexpr("^([2-9]|VIII|VII|III|IX|VI|IV|II|V)(?=[A-Z])", s, perl = TRUE))
  if (!length(m) || !nzchar(m)) return(1L)
  if (grepl("^[0-9]$", m)) as.integer(m) else match(m, c("I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX"))
}

# Is a layer buried (brackets of the notation, the FAO suffix b) or a humus,
# organic or agro horizon: what lies under a surface deposit.
.kidpr_ft_buried_humus <- function(ctx, j) {
  if (is.na(j)) return(FALSE)
  if (.kidpr_buried_des(ctx$x, j)) return(TRUE)
  .kidpr_ft_is_host(ctx, j, c(.KIDPR_FT_GROUPS$humus, .KIDPR_FT_GROUPS$organic, .KIDPR_FT_GROUPS$agro))
}

# Is a layer peat or another organic layer: its horizon, else the organic
# matter of key position 10.
.kidpr_ft_is_peat <- function(ctx, j) {
  cd <- ctx$code[j]
  if (!is.na(cd)) return(cd %in% c(.KIDPR_FT_GROUPS$organic, "TT", "PT", "PTR"))
  isTRUE(.kidpr_org23(ctx$x, j)$passed)
}

# The feature as the key to horizons 2.1 reads it on the data of a layer:
# the outcome gives it (TRUE), some outcomes give it (NA), none does (a weak
# FALSE: the notation may still write it).
.kidpr_ft_key <- function(ctx, i, f) {
  k <- .kidpr_data_layer(ctx$x, i)
  if (f %in% k$features)
    return(.kidpr_fd(TRUE, note = sprintf("key 2.1 on the data ends in %s (%s)", f, k$path %||% "")))
  if (f %in% c(k$features_na, k$candidates))
    return(.kidpr_fd(NA, missing = k$missing, note = sprintf("key 2.1 on the data leaves %s undecided", f)))
  .kidpr_fd(FALSE, said = FALSE, note = sprintf("key 2.1 on the data does not end in %s", f))
}

# Text of an attribute of a layer, and a value of a site attribute that
# reads TRUE (TRUE, yes, 1 ...).
.kidpr_ft_true <- function(v) {
  if (is.null(v) || !length(v) || is.na(v[[1L]])) return(NA)
  v <- tolower(trimws(as.character(v[[1L]])))
  if (v %in% c("true", "t", "yes", "y", "1")) TRUE else if (v %in% c("false", "f", "no", "n", "0")) FALSE else NA
}


# --- 3.2.1 transitional features ---------------------------------------------------

# ao: coarse-humus material under O, too thin for AO (key 47a).
.kidpr_ft_ao <- function(ctx, i) .kidpr_ft_key(ctx, i, "ao")

# mr: mineral admixture in a peat or peregnoi horizon; no attribute records
# it (soilKey's reading).
.kidpr_ft_mr <- function(ctx, i) .kidpr_ft_nota("mr")

# te: within 50 cm of the top of the peat, an oligotrophic (high-moor) layer
# over a thinner eutrophic (low-moor) one: peat_moor_type.
.kidpr_ft_te <- function(ctx, i) {
  x <- ctx$x
  k <- match(i, ctx$main)
  peat <- vapply(ctx$main, function(j) .kidpr_ft_is_peat(ctx, j), NA)
  a <- k; while (a > 1L && peat[a - 1L]) a <- a - 1L
  b <- k; while (b < length(peat) && peat[b + 1L]) b <- b + 1L
  run <- ctx$main[seq.int(a, b)]
  top <- x$top[run[1L]]
  if (is.na(top)) return(.kidpr_fd(NA, said = FALSE, missing = c("top_cm", "bottom_cm")))
  w <- run[!is.na(x$top[run]) & x$top[run] < top + 50]
  mt <- vapply(w, function(j) .kidpr_txt(x, j, "peat_moor_type"), "")
  if (all(is.na(mt))) return(.kidpr_fd(NA, said = FALSE, missing = "peat_moor_type",
                                       note = "the botanical composition of the peat not recorded"))
  if (anyNA(mt)) return(.kidpr_fd(NA, missing = "peat_moor_type",
                                  note = "the botanical composition of some peat layers within 50 cm not recorded"))
  th <- pmax(0, pmin(x$bot[w], top + 50) - x$top[w])
  hi <- grepl("^high", mt); lo <- grepl("^low", mt)
  k <- which(!hi)[1L]
  p <- !is.na(k) && k > 1L && any(lo[seq.int(k, length(w))]) && sum(th[seq_len(k - 1L)]) > sum(th[seq.int(k, length(w))])
  .kidpr_fd(p, note = sprintf("peat within 50 cm of its top: %s", paste(mt, collapse = " over ")))
}

# h: smeary highly decomposed (sapric) material in a peat or humus horizon,
# or a band of it under 5 cm at the base of O, T or PT or on top of AU: the
# degree of decomposition 50 % or more (key 39), or an H thinner than 5 cm
# beside the host.
.kidpr_ft_h <- function(ctx, i) {
  x <- ctx$x; cd <- ctx$code[i]
  org <- c("O", "T", "TO", "TE", "TJ", "PT", "PTR")
  j <- if (isTRUE(cd %in% org)) .kidpr_ft_next(ctx, i) else .kidpr_ft_prev(ctx, i)
  if (!is.na(j) && identical(ctx$code[j], "H") && isTRUE(x$thick[j] < 5))
    return(.kidpr_fd(TRUE, note = sprintf("a muck band of %s cm (%s) %s the host", format(x$thick[j]),
                                          .kidpr_ft_lab(ctx, j), if (isTRUE(cd %in% org)) "at the base of" else "on top of")))
  d <- .kidpr_decomp50(x, i)
  if (isTRUE(cd %in% org)) {
    if (isTRUE(d$passed)) return(.kidpr_fd(TRUE, note = c("sapric peat in the horizon", d$note), tag = d$tag))
    if (isFALSE(d$passed)) return(.kidpr_fd(FALSE, note = c("the peat is not sapric", d$note), tag = d$tag))
    return(.kidpr_fd(NA, said = FALSE, missing = d$missing, note = "the decomposition of the peat not recorded"))
  }
  # a humus horizon: the muck band above it is a layer of the description
  if (isTRUE(d$passed)) return(.kidpr_fd(TRUE, note = c("sapric material in the humus horizon", d$note), tag = d$tag))
  .kidpr_fd(NA, said = FALSE, kind = "observed", note = "no muck band on top of the humus horizon")
}

# e: a whitish band under 2 cm (key 50a), or diffuse bleached spots in a
# humus or middle horizon (leached_spots).
.kidpr_ft_e <- function(ctx, i) {
  k <- .kidpr_ft_key(ctx, i, "e")
  if (!isFALSE(k$passed) || ctx$small[i]) return(k)
  ls <- .kidpr_ft_true(.kidpr_v(ctx$x, i, "leached_spots"))
  if (!is.na(ls) && .kidpr_ft_is_host(ctx, i, c(.KIDPR_FT_GROUPS$humus, .KIDPR_FT_GROUPS$middle)) %in% TRUE)
    return(.kidpr_fd(ls, tag = "leached_spots", note = if (ls) "bleached spots recorded (leached_spots)" else "no bleached spots (leached_spots)"))
  .kidpr_fd(NA, said = FALSE, note = "no whitish band and no bleached spots recorded")
}

# el: the band of eluvial material of 5 cm or less of key 52a; on a horizon
# (bleached grains, skeletans) no attribute records it (Q17: the notation).
.kidpr_ft_el <- function(ctx, i) {
  k <- .kidpr_ft_key(ctx, i, "el")
  if (!isFALSE(k$passed)) return(k)
  .kidpr_ft_nota("el (on a horizon)")
}

# BH, BF: the modifications of BHF by the humus, over 3 % (C over 1.74 %)
# or not.
.kidpr_ft_bhf_mod <- function(ctx, i, which) {
  oc <- .kidpr_num(ctx$x, i, "oc_pct")
  if (is.na(oc)) return(.kidpr_fd(NA, said = FALSE, missing = "oc_pct", note = "carbon of the BHF not recorded"))
  bh <- oc > .KIDPR_FT_BH_OC
  .kidpr_fd(if (which == "BH") bh else !bh,
            note = sprintf("OC %s %% (humus %s %%): %s", format(oc), format(round(oc * .KIDPR_HUMUS_OC, 2)),
                           if (bh) "over 3 % humus, the coffee-brown BH" else "3 % humus or less, the ochre BF"))
}
.kidpr_ft_bh <- function(ctx, i) .kidpr_ft_bhf_mod(ctx, i, "BH")
.kidpr_ft_bf <- function(ctx, i) .kidpr_ft_bhf_mod(ctx, i, "BF")

# f, [e-hf], [akl]: from the notation.
.kidpr_ft_f <- function(ctx, i) .kidpr_ft_nota("f")
.kidpr_ft_ehf <- function(ctx, i) .kidpr_ft_nota("[e-hf]")
.kidpr_ft_akl <- function(ctx, i) .kidpr_ft_nota("[akl]")

# i: thin fragmentary clay cutans on BM, CRM, the parent rock (C, D) or G:
# clay films recorded (few or more); none, no.
.kidpr_ft_i <- function(ctx, i) {
  a <- .kidpr_v(ctx$x, i, "clay_films_amount")
  cf <- .clay_films_class(a)
  if (is.na(cf)) return(.kidpr_fd(NA, said = FALSE, missing = "clay_films_amount", note = "clay films not recorded"))
  .kidpr_fd(cf >= 1L, note = sprintf("clay films %s", as.character(a)))
}

# m: a weak pedogenic crumb or nutty structure in the top of a loamy or
# clayey parent rock.
.kidpr_ft_m <- function(ctx, i) {
  x <- ctx$x
  p <- .kidpr_ft_prev(ctx, i)
  if (!is.na(p) && identical(ctx$code[p], ctx$code[i]))
    return(.kidpr_fd(FALSE, note = "not the top of the parent rock"))
  g <- .kidpr_grad(x, i)
  if (!is.na(g$step) && g$step <= 2) return(.kidpr_fd(FALSE, note = c("sand or loamy sand: not a loamy or clayey rock", g$note), tag = g$tag))
  st <- .kidpr_structure(x, i)
  if (is.na(st$type) && is.na(st$grade))
    return(.kidpr_fd(NA, said = FALSE, missing = c("structure_type", "structure_grade"), note = "structure not recorded"))
  ped <- isTRUE(grepl("granul|crumb|block|nut|polyhed|subang", st$type))
  weak <- isTRUE(.field_grade_class(st$grade) == 1L)
  .kidpr_fd(ped && weak, note = sprintf("structure %s", paste(stats::na.omit(c(st$grade, st$type)), collapse = " ")))
}

# crm: from the notation.
.kidpr_ft_crm <- function(ctx, i) .kidpr_ft_nota("crm")

# an: ochre Fe-humus films in a BHF on volcanic glass: oxalate Fe2O3 1.5-3 %
# (1.430 x Fe) and Al2O3 2-6 % (1.889 x Al).
.kidpr_ft_an <- function(ctx, i) {
  fe <- .kidpr_num(ctx$x, i, "fe_ox_pct"); al <- .kidpr_num(ctx$x, i, "al_ox_pct")
  f2 <- fe * 1.430; a2 <- al * 1.889
  pf <- if (is.na(fe)) NA else f2 >= 1.5 && f2 <= 3
  pa <- if (is.na(al)) NA else a2 >= 2 && a2 <= 6
  p <- .kidpr_and(pf, pa)
  .kidpr_fd(p, said = !is.na(p), missing = c(if (is.na(fe)) "fe_ox_pct", if (is.na(al)) "al_ox_pct"),
            note = sprintf("oxalate Fe2O3 %s %%, Al2O3 %s %%", if (is.na(fe)) "?" else format(round(f2, 2)),
                           if (is.na(al)) "?" else format(round(a2, 2))))
}

# pl: from the notation.
.kidpr_ft_pl <- function(ctx, i) .kidpr_ft_nota("pl")

# sn: a columnar structure in a humus or middle horizon that is not a
# solonetz horizon (Q17: the columnar structure only).
.kidpr_ft_sn <- function(ctx, i) {
  st <- .kidpr_structure(ctx$x, i)
  s <- paste(stats::na.omit(c(st$type, st$secondary)), collapse = " ")
  if (is.na(st$type) && is.na(st$secondary))
    return(.kidpr_fd(NA, said = FALSE, missing = "structure_type", note = "structure not recorded"))
  p <- grepl(.KIDPR_COLUMNAR, s)
  .kidpr_fd(p, note = sprintf("structure %s", trimws(s)))
}

# v: one or two of the three vertic signs (slickensides, wedge-shaped peds,
# clay), or all three below 80 cm (key 16b, 23; default 18).
.kidpr_ft_v <- function(ctx, i) {
  x <- ctx$x
  vt <- .kidpr_vertic3(x, i)
  if (isTRUE(vt$partial)) return(.kidpr_fd(TRUE, note = c("vertic signs short of the vertic horizon", vt$note)))
  sl <- .kidpr_txt(x, i, "slickensides"); st <- .kidpr_structure(x, i)$type
  if (!is.na(sl) || !is.na(st)) return(.kidpr_fd(FALSE, note = c("no vertic sign recorded", vt$note)))
  .kidpr_fd(NA, said = FALSE, missing = c("slickensides", "structure_type"), note = "slickensides not recorded")
}

# The olive mottles of q: hue 5Y, chroma 3-4.
.kidpr_ft_olive_mottle <- function(m) {
  if (!nrow(m)) return(logical(0))
  m$family %in% "Y" & m$number %in% 5 & !is.na(m$chroma) & m$chroma >= 3 & m$chroma <= 4
}

# g: bluish or greenish tones, ochre-rusty mottles, concretions and Fe-Mn
# stains in a horizon that is not G: a redox sign recorded (mottles, redox
# features, segregations, a positive dipyridyl test), mottles that are all
# olive excepted (q).
.kidpr_ft_g <- function(ctx, i) {
  x <- ctx$x
  mc <- .kidpr_txt(x, i, "mottle_colors")
  rf <- .kidpr_num(x, i, "redoximorphic_features_pct")
  rl <- .kidpr_txt(x, i, "redox_segregations_location")
  m <- .kidpr_morphons(x, i)$mottles
  only_olive <- nrow(m) > 0L && all(.kidpr_ft_olive_mottle(m)) && !isTRUE(rf > 0)
  if (.kidpr_redox_sign(x, i) && !only_olive)
    return(.kidpr_fd(TRUE, note = sprintf("redox signs: %s", paste(c(if (!is.na(mc)) sprintf("mottles %s", mc),
                                                                       if (isTRUE(rf > 0)) sprintf("redox features %s %%", format(rf)),
                                                                       if (!is.na(rl)) sprintf("segregations %s", rl)), collapse = ", "))))
  if (!is.na(mc) || !is.na(rf) || !is.na(rl))
    return(.kidpr_fd(FALSE, note = if (only_olive) "only olive mottles (q, not g)" else "no redox sign"))
  .kidpr_fd(NA, said = FALSE, missing = c("mottle_colors", "redoximorphic_features_pct"), note = "mottles not recorded")
}

# q: olive mottles (5Y, chroma 3-4) in a matrix that is not olive, or a
# continuous olive layer deeper than 120 cm.
.kidpr_ft_q <- function(ctx, i) {
  x <- ctx$x
  m <- .kidpr_morphons(x, i)$mottles
  if (any(.kidpr_ft_olive_mottle(m))) return(.kidpr_fd(TRUE, note = "olive mottles (5Y, chroma 3-4)"))
  g <- .kidpr_ground(x)
  deep <- !is.na(x$top[i]) && !is.na(g) && x$top[i] - g > 120
  if (deep) {
    o <- .kidpr_olive(x, i)
    if (isTRUE(o$passed)) return(.kidpr_fd(TRUE, note = c("a continuous olive layer deeper than 120 cm", o$note)))
  }
  if (nrow(m)) return(.kidpr_fd(FALSE, note = "no olive mottles"))
  .kidpr_fd(NA, said = FALSE, missing = "mottle_colors", note = "mottles not recorded")
}

# ml: marly impregnation or large carbonate concretions in the middle or
# lower profile, CO2 under 30 % (CaCO3 under 68.2 %): hard concretions.
.kidpr_ft_ml <- function(ctx, i) {
  x <- ctx$x
  cc <- .kidpr_num(x, i, "caco3_pct")
  if (isTRUE(cc >= .KIDPR_FT_ML_CACO3)) return(.kidpr_fd(FALSE, note = sprintf("CaCO3 %s %%: the marl horizon ML, not ml", format(cc))))
  fm <- .kidpr_carb_forms(x, i)
  hard <- .kidpr_num(x, i, "carbonate_hard_pct")
  if (any(grepl("hard concr|nodul|marl", fm)) || isTRUE(hard > 0))
    return(.kidpr_fd(TRUE, note = "hard carbonate concretions"))
  if (length(fm) || !is.na(hard)) return(.kidpr_fd(FALSE, note = "no hard carbonate concretions"))
  .kidpr_fd(NA, said = FALSE, missing = "secondary_carbonates_form", note = "carbonate forms not recorded")
}

# s: toxic soluble salts over 0.1 % in a horizon, not over 1 % in the top
# 20 cm (there it is the salt horizon S): soluble_salts_pct, else the EC of
# chapter 9 (ECe 4 dS/m or more, EC 1:5 0.2 dS/m or more).
.kidpr_ft_s <- function(ctx, i) {
  x <- ctx$x
  g <- .kidpr_ground(x)
  top20 <- !is.na(x$top[i]) && !is.na(g) && x$top[i] - g < 20
  s <- .kidpr_num(x, i, "soluble_salts_pct")
  if (!is.na(s)) {
    if (top20 && s > 1) return(.kidpr_fd(FALSE, note = sprintf("soluble salts %s %% within 20 cm: the salt horizon S, not s", format(s))))
    return(.kidpr_fd(s > 0.1, note = sprintf("soluble salts %s %%", format(s))))
  }
  ece <- .kidpr_num(x, i, "ec_dS_m"); e15 <- .kidpr_num(x, i, "ec_1to5_dS_m")
  if (!is.na(ece) || !is.na(e15)) {
    strong <- isTRUE(ece >= 16) || isTRUE(e15 > 2)
    if (top20 && strong) return(.kidpr_fd(FALSE, tag = "ec_salts", note = "salts over 1 % by the EC within 20 cm: the salt horizon S, not s"))
    p <- .kidpr_or(if (!is.na(ece)) ece >= 4, if (!is.na(e15)) e15 >= 0.2)
    return(.kidpr_fd(p, tag = "ec_salts", note = sprintf("salts by the EC of chapter 9 (%s)", paste(c(
      if (!is.na(ece)) sprintf("ECe %s dS/m", format(ece)), if (!is.na(e15)) sprintf("EC 1:5 %s dS/m", format(e15))), collapse = ", "))))
  }
  .kidpr_fd(NA, said = FALSE, missing = c("soluble_salts_pct", "ec_dS_m", "ec_1to5_dS_m"), note = "salts not measured")
}

# tk: a smooth clay crust of 0.5 cm or less cracked into polygons on a bare
# surface: a clay crust with polygonal cracks recorded on the top layer.
.kidpr_ft_tk <- function(ctx, i) {
  x <- ctx$x
  if (!identical(i, ctx$ord[1L])) return(.kidpr_fd(FALSE, note = "not the surface layer"))
  cr <- .kidpr_txt(x, i, "surface_crust_type")
  pc <- .kidpr_num(x, i, "polygonal_cracks_spacing_cm")
  if (is.na(cr)) return(.kidpr_fd(NA, said = FALSE, missing = "surface_crust_type", note = "no surface crust recorded"))
  p <- grepl("clay|pelocrust|takyr", cr) && !is.na(pc)
  .kidpr_fd(p, note = sprintf("surface crust %s%s", cr, if (!is.na(pc)) sprintf(", polygonal cracks every %s cm", format(pc)) else ", no polygonal cracks"))
}


# --- 3.2.2 process features -----------------------------------------------------------

.kidpr_ft_ff <- function(ctx, i) .kidpr_ft_nota("ff")
.kidpr_ft_fn <- function(ctx, i) .kidpr_ft_nota("fn")

# nn: abundant (10-20 %) Fe concretions in the eluvial horizon.
.kidpr_ft_nn <- function(ctx, i) {
  fn <- .kidpr_num(ctx$x, i, "ferric_nodules_pct")
  if (is.na(fn)) return(.kidpr_fd(NA, said = FALSE, missing = "ferric_nodules_pct", note = "iron concretions not recorded"))
  .kidpr_fd(fn >= 10, note = sprintf("iron concretions %s %%", format(fn)))
}

# cf: from the notation (soilKey's reading).
.kidpr_ft_cf <- function(ctx, i) .kidpr_ft_nota("cf")

# ox: in G, the ochre-rusty mottles over half the section:
# redoximorphic_features_pct 50 or more.
.kidpr_ft_ox <- function(ctx, i) {
  if (!identical(ctx$code[i], "G")) return(.kidpr_fd(NA, said = FALSE, note = "read on the G horizon"))
  rf <- .kidpr_num(ctx$x, i, "redoximorphic_features_pct")
  if (is.na(rf)) return(.kidpr_fd(NA, said = FALSE, missing = "redoximorphic_features_pct", note = "redox features not recorded"))
  .kidpr_fd(rf >= 50, note = sprintf("redox features %s %% of the gley horizon", format(rf)))
}

# ek (ko): a bleached band at the base of a light deposit directly over a
# heavier one (another lithological layer, or the underlying rock D): a
# whitish band of 5 cm or less (a small e or el, or a thin whitish layer);
# an eluvial horizon over the rock is that horizon, not the band.
.kidpr_ft_ek <- function(ctx, i) {
  x <- ctx$x
  j <- .kidpr_ft_next(ctx, i)
  if (is.na(j)) return(.kidpr_fd(FALSE, note = "nothing below"))
  disc <- identical(ctx$code[j], "D") || .kidpr_ft_lith(ctx, j) > .kidpr_ft_lith(ctx, i)
  if (!disc) return(.kidpr_fd(FALSE, said = FALSE, note = "no lithological discontinuity under the layer"))
  if (!ctx$small[i] && !isTRUE(x$thick[i] <= 5))
    return(.kidpr_fd(FALSE, said = FALSE, note = "not a band of 5 cm or less over the discontinuity"))
  light <- any(c("e", "el") %in% c(ctx$fr$features[[i]], .kidpr_data_layer(x, i)$features))
  if (!light) {
    col <- .kidpr_colour(x, i, "dry")
    light <- .kidpr_is_whitish(col$value, col$chroma)
  }
  d <- .kidpr_grad_diff(x, j, i)
  heavier <- if (is.na(d$diff)) NA else d$diff >= 1
  p <- .kidpr_and(light, heavier)
  .kidpr_fd(p, said = !is.na(p), tag = d$tag,
            missing = c(if (is.na(light)) "munsell_value_dry", if (is.na(heavier)) .KIDPR_TEXTURE_MISSING),
            note = c(sprintf("over %s, another lithological layer", .kidpr_ft_lab(ctx, j)),
                     if (isTRUE(light)) "bleached" else if (isFALSE(light)) "not bleached",
                     if (isTRUE(heavier)) "heavier below" else if (isFALSE(heavier)) "not heavier below", d$note))
}

# gr: a granular structure in BFM.
.kidpr_ft_gr <- function(ctx, i) {
  st <- .kidpr_structure(ctx$x, i)$type
  if (is.na(st)) return(.kidpr_fd(NA, said = FALSE, missing = "structure_type", note = "structure not recorded"))
  .kidpr_fd(grepl("granul|roe|crumb", st), note = sprintf("structure %s", st))
}

.kidpr_ft_hi <- function(ctx, i) .kidpr_ft_nota("hi")

# The carbonate forms of lc, mc, nc and dc (default 19): pseudomycelium
# (mc; lc in the humus horizon with the coatings), white-eye or soft
# concretions (nc), powdery or masses alone (dc).
.kidpr_ft_carb <- function(ctx, i, which) {
  fm <- .kidpr_carb_forms(ctx$x, i)
  if (!length(fm)) return(.kidpr_fd(NA, said = FALSE, missing = "secondary_carbonates_form", note = "carbonate forms not recorded"))
  none <- all(grepl("^(none|no|absent)$", fm))
  myc <- any(grepl("pseudomyc", fm)); coat <- any(grepl("coating", fm))
  soft <- any(grepl("soft concr", fm)); hard <- any(grepl("hard concr|nodul", fm))
  disp <- any(grepl("powder|mass", fm))
  p <- !none && switch(which, mc = myc, lc = myc || coat, nc = soft, dc = disp && !myc && !soft && !hard)
  .kidpr_fd(p, note = sprintf("carbonate forms: %s", paste(fm, collapse = ", ")))
}
.kidpr_ft_mc <- function(ctx, i) .kidpr_ft_carb(ctx, i, "mc")
.kidpr_ft_lc <- function(ctx, i) .kidpr_ft_carb(ctx, i, "lc")
.kidpr_ft_nc <- function(ctx, i) .kidpr_ft_carb(ctx, i, "nc")
.kidpr_ft_dc <- function(ctx, i) .kidpr_ft_carb(ctx, i, "dc")

.kidpr_ft_ic <- function(ctx, i) .kidpr_ft_nota("ic")

# cs: gypsum crystals, clusters or druses: visible gypsum; no gypsum at all
# (CaSO4 0) is none.
.kidpr_ft_cs <- function(ctx, i) {
  gv <- .kidpr_num(ctx$x, i, "gypsum_visible_pct")
  if (!is.na(gv)) return(.kidpr_fd(gv > 0, note = sprintf("visible gypsum %s %%", format(gv))))
  g4 <- .kidpr_num(ctx$x, i, "caso4_pct")
  if (isTRUE(g4 == 0)) return(.kidpr_fd(FALSE, note = "no gypsum (CaSO4 0 %)"))
  .kidpr_fd(NA, said = FALSE, missing = "gypsum_visible_pct", note = "visible gypsum not recorded")
}


# --- 3.2.3 turbation features ---------------------------------------------------------

# tr: the upper horizons mixed, with the base above 40 cm (key 7b): the
# fragments of other horizons or the site's mixing depth recorded.
.kidpr_ft_tr <- function(ctx, i) {
  k <- .kidpr_ft_key(ctx, i, "tr")
  if (!isFALSE(k$passed)) return(k)
  ff <- .kidpr_num(ctx$x, i, "foreign_fragments_pct")
  if (isTRUE(ff == 0)) return(.kidpr_fd(FALSE, note = "no fragments of other horizons"))
  k
}

# @: cryoturbation recorded (Q24), else a broken or involuted boundary with
# long frost (the reading of cryoturbation_usda()).
.kidpr_ft_cryo <- function(ctx, i) {
  x <- ctx$x
  cr <- .kidpr_txt(x, i, "cryoturbation")
  if (!is.na(cr)) return(.kidpr_fd(grepl("^present", cr), note = sprintf("cryoturbation %s", cr)))
  bt <- .kidpr_txt(x, i, "boundary_topography")
  if (isTRUE(grepl("involut|broken|cryoturb|convolut", bt))) {
    fr <- .kidpr_frost(x)
    if (isTRUE(fr$passed)) return(.kidpr_fd(TRUE, tag = "boundary_and_frost",
                                            note = sprintf("a %s boundary under long frost", bt)))
    if (is.na(fr$passed)) return(.kidpr_fd(NA, missing = c("cryoturbation", fr$missing),
                                           note = sprintf("a %s boundary; long frost undecided", bt)))
  }
  .kidpr_fd(NA, said = FALSE, missing = "cryoturbation", note = "cryoturbation not recorded")
}

.kidpr_ft_ocr <- function(ctx, i) .kidpr_ft_nota("ocr")
.kidpr_ft_agr <- function(ctx, i) .kidpr_ft_nota("agr")

# pb: fresh fragments of the underlying horizon in an agro horizon with over
# 1.5 % humus (C over 0.87 %) (Q19): foreign_fragments_pct, else recorded
# erosion; in CJ ("deflated") a natural soil with deflation or truncation
# recorded.
.kidpr_ft_erosion <- function(ctx) {
  x <- ctx$x
  tr <- .kidpr_ft_true(.kidpr_site(x, "anthropic_truncation"))
  wd <- .kidpr_ft_true(.kidpr_site(x, "wind_deflation"))
  sc <- .kidpr_site_txt(x, "surface_condition")
  isTRUE(tr) || isTRUE(wd) || isTRUE(grepl("erod|erosion|deflat|\u044d\u0440\u043e\u0434|\u0434\u0435\u0444\u043b\u044f\u0446", sc))
}
.kidpr_ft_pb <- function(ctx, i) {
  x <- ctx$x
  er <- .kidpr_ft_erosion(ctx)
  if (identical(ctx$division, "CJ")) {
    return(.kidpr_fd(er, said = er, note = if (er) "deflation or truncation recorded (CJ: deflated)" else "no deflation or truncation recorded"))
  }
  oc <- .kidpr_num(x, i, "oc_pct")
  if (isTRUE(oc <= .KIDPR_FT_PB_OC)) return(.kidpr_fd(FALSE, note = sprintf("OC %s %%: humus 1.5 %% or less", format(oc))))
  hum <- if (is.na(oc)) NA else TRUE
  ff <- .kidpr_num(x, i, "foreign_fragments_pct")
  frag <- if (!is.na(ff)) ff > 0 else if (er) TRUE else NA
  if (is.na(frag)) return(.kidpr_fd(NA, said = FALSE, missing = "foreign_fragments_pct", note = "fragments in the agro horizon not recorded"))
  p <- .kidpr_and(hum, frag)
  .kidpr_fd(p, missing = if (is.na(p)) "oc_pct" else character(0), tag = if (is.na(ff) && er) "erosion_recorded",
            note = c(if (!is.na(ff)) sprintf("fragments of other horizons %s %%", format(ff)) else "erosion recorded at the site",
                     if (!is.na(oc)) sprintf("OC %s %%", format(oc))))
}


# --- 3.2.4 evolutionary features ------------------------------------------------------

# [hh]: a second humus horizon in EL, BEL or the top of BT: darker (moist
# value one lower) and richer in carbon than the layer above it; or an FAO
# buried A there.
.kidpr_ft_hh <- function(ctx, i) {
  x <- ctx$x
  a <- .kidpr_ft_prev(ctx, i)
  if (identical(ctx$code[i], "BT") && !is.na(a) && identical(ctx$code[a], "BT"))
    return(.kidpr_fd(FALSE, note = "not the top of BT"))
  d <- as.character(ctx$fr$designation[i])
  if (!isTRUE(x$des$known[i]) && !is.na(d) && grepl("^[0-9IVX]*A[a-z]*b", d))
    return(.kidpr_fd(TRUE, tag = "fao_buried_a", note = sprintf("a buried A (%s) in the eluvial zone", d)))
  if (is.na(a)) return(.kidpr_fd(FALSE, note = "nothing above"))
  vi <- .kidpr_colour(x, i, "moist")$value; va <- .kidpr_colour(x, a, "moist")$value
  oi <- .kidpr_num(x, i, "oc_pct"); oa <- .kidpr_num(x, a, "oc_pct")
  dark <- if (is.na(vi) || is.na(va)) NA else vi <= va - 1
  rich <- if (is.na(oi) || is.na(oa)) NA else oi > oa
  p <- .kidpr_and(dark, rich)
  .kidpr_fd(p, said = !is.na(p), missing = c(if (is.na(dark)) "munsell_value_moist", if (is.na(rich)) "oc_pct"),
            note = sprintf("against %s: moist value %s vs %s, OC %s vs %s %%", .kidpr_ft_lab(ctx, a),
                           format(vi), format(va), format(oi), format(oa)))
}

.kidpr_ft_ct <- function(ctx, i) .kidpr_ft_nota("ct")

# y: whitish tongues of E or EL deep into BT or BHF: a tonguing lower
# boundary of the eluvial layer above, tongues recorded in the host, else
# soilKey's retic properties or albeluvic glossae (flagged).
.kidpr_ft_y <- function(ctx, i) {
  x <- ctx$x
  a <- .kidpr_ft_prev(ctx, i)
  tv <- .kidpr_num(x, i, "tongue_vol_pct")
  if (isTRUE(tv > 0)) return(.kidpr_fd(TRUE, note = sprintf("tongues %s %% of the volume", format(tv))))
  bt <- if (is.na(a)) NA_character_ else .kidpr_txt(x, a, "boundary_topography")
  if (!is.na(bt)) return(.kidpr_fd(grepl("tongu|glossic|\u044f\u0437\u044b\u043a", bt),
                                   note = sprintf("lower boundary of %s: %s", .kidpr_ft_lab(ctx, a), bt)))
  for (nm in c("retic_properties", "albeluvic_glossae")) {
    w <- .kidpr_wrb_layer(x, nm, i)
    if (isTRUE(w)) return(.kidpr_fd(TRUE, tag = nm, note = sprintf("tongues not recorded: soilKey's %s() stands in (passes)", nm)))
  }
  .kidpr_fd(NA, said = FALSE, missing = "boundary_topography", note = "tongues not recorded")
}

.kidpr_ft_yu <- function(ctx, i) .kidpr_ft_nota("yu")

# d: a coarse platy compaction of the surface layer of a natural soil.
.kidpr_ft_d <- function(ctx, i) {
  x <- ctx$x
  if (isTRUE(.kidpr_agro(x, i)$passed)) return(.kidpr_fd(FALSE, note = "an agro horizon (ad, not d)"))
  if (isTRUE(.kidpr_org23(x, i)$passed)) return(.kidpr_fd(FALSE, said = FALSE, note = "an organic layer"))
  st <- .kidpr_structure(x, i)$type
  if (is.na(st)) return(.kidpr_fd(NA, said = FALSE, missing = "structure_type", note = "structure not recorded"))
  .kidpr_fd(grepl("platy|lamin", st), note = sprintf("structure %s at the surface", st))
}

.kidpr_ft_ad <- function(ctx, i) .kidpr_ft_nota("ad")

# pa: the traces of a former agro horizon after ploughing stopped: a land
# use of abandoned or fallow land, on a ploughed horizon (the agro index, or
# the plough suffix p).
.KIDPR_FT_ABANDONED <- paste0("abandon|fallow|post-?agr|former (arable|field|crop)|old field|set-?aside|",
                              "\u0437\u0430\u043b\u0435\u0436|\u043f\u043e\u0441\u0442\u0430\u0433\u0440")
.kidpr_ft_pa <- function(ctx, i) {
  x <- ctx$x
  lu <- .kidpr_site_txt(x, "land_use")
  if (is.na(lu)) return(.kidpr_fd(NA, said = FALSE, missing = "land_use", note = "land use not recorded"))
  ab <- grepl(.KIDPR_FT_ABANDONED, lu, perl = TRUE)
  if (!ab) return(.kidpr_fd(FALSE, note = sprintf("land use '%s': not abandoned arable land", lu)))
  ag <- isTRUE(ctx$code[i] %in% .KIDPR_FT_GROUPS$agro) || isTRUE(.kidpr_fao_p(x, i))
  if (ag) return(.kidpr_fd(TRUE, note = sprintf("land use '%s' on a ploughed horizon", lu)))
  .kidpr_fd(NA, said = FALSE, note = sprintf("land use '%s'; no plough mark on the horizon", lu))
}

.kidpr_ft_pr <- function(ctx, i) .kidpr_ft_nota("pr")

# md: the upper peat more decomposed than the peat below it: a sapric layer
# at the surface over the host peat that is not.
.kidpr_ft_md <- function(ctx, i) {
  x <- ctx$x
  a <- .kidpr_ft_prev(ctx, i)
  if (is.na(a) || !identical(a, ctx$main[1L])) return(.kidpr_fd(FALSE, said = FALSE, note = "no surface layer over the peat"))
  da <- .kidpr_decomp50(x, a); di <- .kidpr_decomp50(x, i)
  p <- .kidpr_and(da$passed, .kidpr_not(di$passed))
  .kidpr_fd(p, said = !is.na(p), missing = unique(c(da$missing, di$missing)),
            note = sprintf("%s over the peat: %s", .kidpr_ft_lab(ctx, a),
                           if (isTRUE(p)) "more decomposed" else if (isFALSE(p)) "not more decomposed" else "decomposition not recorded"))
}

# pir: over 2 cm of ash from a peat fire on the peat: a layer of pyrogenic
# origin (layer_origin) over the host.
.KIDPR_FT_PYRO <- "pyrog|burn|fire|charcoal|\u043f\u0438\u0440\u043e\u0433|\u0433\u0430\u0440\u044c|\u043f\u043e\u0436\u0430\u0440"
.kidpr_ft_pir <- function(ctx, i) {
  x <- ctx$x
  o <- paste(.kidpr_txt(x, i, "layer_origin"), .kidpr_txt(x, i, "rock_origin"))
  if (grepl("^NA NA$", o)) return(.kidpr_fd(NA, said = FALSE, missing = "layer_origin", note = "origin not recorded"))
  py <- grepl(.KIDPR_FT_PYRO, o, perl = TRUE) || (grepl("\\bash\\b|\u0437\u043e\u043b", o, perl = TRUE) && !grepl("tephr|volcan|pyroclast", o))
  if (!py) return(.kidpr_fd(FALSE, note = sprintf("origin %s", trimws(gsub("NA", "", o)))))
  th <- x$thick[i]
  .kidpr_fd(if (is.na(th)) NA else th > 2, missing = "top_cm",
            note = sprintf("pyrogenic ash %s cm thick", format(th)))
}


# --- 3.2.5 parent-rock features -------------------------------------------------------

# ca: residual carbonates with no pedogenic neoformations: key 64b; else
# effervescence (continuous or local) where no carbonate neoformation is
# recorded or read.
.kidpr_ft_ca <- function(ctx, i) {
  x <- ctx$x
  k <- .kidpr_ft_key(ctx, i, "ca")
  if (isTRUE(k$passed)) return(k)
  if (isTRUE(ctx$code[i] %in% c("BCA", "CAT", "ML")))
    return(.kidpr_fd(FALSE, note = "a horizon of pedogenic carbonates"))
  e <- .kidpr_eff_any(x, i)
  if (isFALSE(e$passed)) return(.kidpr_fd(FALSE, note = c("no effervescence", e$note), tag = e$tag))
  if (is.na(e$passed)) return(.kidpr_fd(NA, said = FALSE, missing = e$missing, note = "effervescence not recorded"))
  n <- .kidpr_carb_neo(x, i)
  p <- .kidpr_not(n$passed)
  .kidpr_fd(p, missing = n$missing, tag = c(e$tag, n$tag),
            note = c("effervescent", if (isTRUE(p)) "no carbonate neoformations: residual carbonates"
                     else if (isFALSE(p)) "carbonate neoformations: not residual", n$note))
}

# u: every horizon under the humus horizon dark from a dark rock (key 13).
.kidpr_ft_u <- function(ctx, i = NA_integer_) {
  d <- .kidpr_dark_rock(ctx$x)
  .kidpr_fd(d$passed, said = !is.na(d$passed), missing = d$missing, note = d$note,
            layers = if (isTRUE(d$passed)) ctx$main else integer(0))
}

# ro: every mineral horizon red (hue 5YR or redder) from the parent rock.
.kidpr_ft_ro <- function(ctx, i = NA_integer_) {
  x <- ctx$x
  rows <- ctx$main[!vapply(ctx$main, function(j) .kidpr_hard_rock(x, j) || isTRUE(.kidpr_org23(x, j)$passed), NA)]
  if (!length(rows)) return(.kidpr_fd(FALSE, note = "no mineral horizon"))
  hue <- vapply(rows, function(j) {
    h <- .kidpr_colour(x, j, "moist")$hue
    if (is.na(h)) .kidpr_colour(x, j, "dry")$hue else h
  }, "")
  u <- suppressWarnings(.munsell_hue_units(hue))
  red <- ifelse(is.na(u), NA, u <= 12.5)
  if (any(red %in% FALSE))
    return(.kidpr_fd(FALSE, note = sprintf("%s: hue %s, yellower than 5YR", .kidpr_ft_lab(ctx, rows[which(red %in% FALSE)[1L]]),
                                           hue[which(red %in% FALSE)[1L]])))
  if (anyNA(red)) return(.kidpr_fd(NA, said = FALSE, missing = "munsell_hue_moist", note = "the hue of some mineral horizons not recorded"))
  .kidpr_fd(TRUE, layers = rows, note = sprintf("every mineral horizon of hue 5YR or redder (%s)", paste(unique(hue), collapse = ", ")))
}

# The permafrost sign: dense icy permafrost within the top metre (a frozen
# layer recorded within 100 cm: temperature 0 C or less, ice); none in a
# profile with no long frost.
.kidpr_ft_perm <- function(ctx, i = NA_integer_) {
  x <- ctx$x
  g <- .kidpr_ground(x)
  fz <- vapply(ctx$ord, function(j) {
    pt <- .kidpr_num(x, j, "permafrost_temp_C"); ic <- .kidpr_num(x, j, "ice_pct")
    ifm <- .kidpr_txt(x, j, "ice_form")
    isTRUE(pt <= 0) || isTRUE(ic > 0) || (!is.na(ifm) && !grepl("^(none|no|absent)", ifm))
  }, NA)
  within <- vapply(ctx$ord, function(j) !is.na(x$top[j]) && !is.na(g) && x$top[j] - g <= 100, NA)
  if (any(fz & within))
    return(.kidpr_fd(TRUE, layers = ctx$ord[fz & within], note = "frozen ground recorded within the top metre"))
  if (any(fz)) return(.kidpr_fd(FALSE, note = "frozen ground recorded deeper than the top metre"))
  fr <- .kidpr_site_txt(x, "frost_regime")
  if (!is.na(fr)) {
    if (fr == "permafrost") return(.kidpr_fd(NA, missing = c("permafrost_temp_C", "ice_pct"),
                                             note = "permafrost recorded at the site, its depth not"))
    return(.kidpr_fd(FALSE, note = sprintf("frost regime %s", fr)))
  }
  f <- .kidpr_frost(x)
  if (isFALSE(f$passed)) return(.kidpr_fd(FALSE, note = c("no long frost:", f$note), tag = f$tag))
  .kidpr_fd(NA, said = FALSE, missing = c("frost_regime", "permafrost_temp_C", "ice_pct"), note = "permafrost not recorded")
}


# --- 3.2.6 accumulative-substrate features ---------------------------------------------

# The deposited layer at the surface of r, rh, ur, rr and rx: 5-40 cm over a
# buried humus (organic) horizon. The deposit of key positions 8-9
# (.kidpr_deposit()), its thickness and what lies under it.
.kidpr_ft_surface_deposit <- function(ctx) {
  .kidpr_memo(ctx$x, "ft_deposit", function() {
    d <- .kidpr_deposit(ctx$x)
    if (!isTRUE(d$passed) || !length(d$rows)) return(list(ok = FALSE, rows = integer(0), note = "no deposit at the surface"))
    th <- d$thick
    nx <- .kidpr_ft_next(ctx, d$rows[length(d$rows)])
    under <- .kidpr_ft_buried_humus(ctx, nx)
    ok <- .kidpr_and(!is.na(th) && th >= 5 && th <= 40, under)
    if (is.na(th)) ok <- NA
    list(ok = ok, rows = d$rows, thick = th,
         note = c(d$note, if (!is.na(th) && (th < 5 || th > 40)) sprintf("%s cm: not 5-40 cm", format(th)),
                  if (isFALSE(under)) "not over a buried humus or organic horizon"))
  })
}

# r, rh: the deposit not humified (r) or grey and humified (rh): the key
# 2.1 outcomes r (38a) and rh (35a), 5-40 cm over a buried humus horizon.
.kidpr_ft_rkey <- function(ctx, f) {
  rows <- ctx$ord
  ks <- lapply(rows, function(j) .kidpr_ft_key(ctx, j, f))
  p <- vapply(ks, function(k) k$passed, NA)
  if (!any(p %in% TRUE)) {
    if (any(is.na(p))) return(.kidpr_fd(NA, missing = unique(unlist(lapply(ks[is.na(p)], `[[`, "missing"))),
                                        note = sprintf("key 2.1 leaves %s undecided", f)))
    return(.kidpr_fd(FALSE, said = FALSE, note = sprintf("key 2.1 on the data reads no %s", f)))
  }
  sd <- .kidpr_ft_surface_deposit(ctx)
  .kidpr_fd(sd$ok, layers = rows[p %in% TRUE], note = c(sprintf("key 2.1 on the data ends in %s", f), sd$note))
}
.kidpr_ft_r <- function(ctx, i = NA_integer_) .kidpr_ft_rkey(ctx, "r")
.kidpr_ft_rh <- function(ctx, i = NA_integer_) .kidpr_ft_rkey(ctx, "rh")

# aq, ae: the stratified horizon deposited by water or by wind
# (layer_origin, rock_origin).
.kidpr_ft_origin <- function(ctx, i, which) {
  x <- ctx$x
  o <- paste(.kidpr_txt(x, i, "layer_origin"), .kidpr_txt(x, i, "rock_origin"))
  if (grepl("^NA NA$", o)) return(.kidpr_fd(NA, said = FALSE, missing = "layer_origin", note = "origin not recorded"))
  pat <- if (which == "aq") "fluv|alluv|lacustr|water|\u0430\u043b\u043b\u044e\u0432|\u0432\u043e\u0434\u043d"
         else "aeol|eol|wind|\u044d\u043e\u043b"
  .kidpr_fd(grepl(pat, o), note = sprintf("origin %s", trimws(gsub("NA", "", o))))
}
.kidpr_ft_aq <- function(ctx, i) .kidpr_ft_origin(ctx, i, "aq")
.kidpr_ft_ae <- function(ctx, i) .kidpr_ft_origin(ctx, i, "ae")

# The artefacts of a layer: artefacts_pct, else its urbic and industrial
# parts.
.kidpr_ft_artefacts <- function(ctx, j) {
  a <- .kidpr_num(ctx$x, j, "artefacts_pct")
  if (!is.na(a)) return(a)
  p <- c(.kidpr_num(ctx$x, j, "artefacts_urbic_pct"), .kidpr_num(ctx$x, j, "artefacts_industrial_pct"))
  if (all(is.na(p))) NA_real_ else sum(p, na.rm = TRUE)
}

# The toxicity of the deposit (Q23's hazard categories): toxic above the
# permissible category; contamination recorded with no category undecided;
# nothing recorded, not toxic.
.kidpr_ft_toxic <- function(ctx, rows) {
  hz <- vapply(rows, function(j) .kidpr_txt(ctx$x, j, "contamination_hazard"), "")
  if (any(grepl("^(moderat|highly|extrem)", hz)))
    return(list(p = TRUE, said = TRUE, note = sprintf("hazard %s", paste(stats::na.omit(hz), collapse = ", "))))
  if (any(!is.na(hz))) return(list(p = FALSE, said = TRUE, note = sprintf("hazard %s", paste(stats::na.omit(hz), collapse = ", "))))
  ct <- vapply(rows, function(j) .kidpr_txt(ctx$x, j, "contamination_type"), "")
  if (any(!is.na(ct)) || isTRUE(.kidpr_ft_true(.kidpr_site(ctx$x, "contaminated"))))
    return(list(p = NA, said = TRUE, note = "contamination recorded without its hazard category"))
  list(p = FALSE, said = FALSE, note = "no contamination recorded")
}

# ur, rr, rx: the deposited layer at the surface (5-40 cm over a buried
# humus horizon) with 20 % or more of artefacts and under half of them
# (ur), made of artificial material (half or more of artefacts) that is not
# toxic (rr), or toxic (rx).
.kidpr_ft_artificial <- function(ctx, which) {
  sd <- .kidpr_ft_surface_deposit(ctx)
  if (!length(sd$rows)) return(.kidpr_fd(FALSE, said = FALSE, note = sd$note))
  if (isFALSE(sd$ok)) return(.kidpr_fd(FALSE, note = sd$note))
  a <- vapply(sd$rows, function(j) .kidpr_ft_artefacts(ctx, j), 0)
  am <- if (all(is.na(a))) NA_real_ else max(a, na.rm = TRUE)
  tx <- .kidpr_ft_toxic(ctx, sd$rows)
  p <- switch(which,
              ur = if (is.na(am)) NA else am >= 20 && am < 50,
              rr = .kidpr_and(if (is.na(am)) NA else am >= 50, .kidpr_not(tx$p)),
              rx = tx$p)
  said <- switch(which, ur = !is.na(am), rr = !is.na(am) || isTRUE(tx$said), rx = tx$said)
  p <- .kidpr_and(sd$ok, p)
  .kidpr_fd(p, said = said, layers = if (isTRUE(p)) sd$rows else integer(0),
            missing = c(if (is.na(am) && which != "rx") "artefacts_pct", if (is.na(tx$p)) "contamination_hazard"),
            note = c(sd$note, if (!is.na(am)) sprintf("artefacts %s %%", format(am)), tx$note))
}
.kidpr_ft_ur <- function(ctx, i = NA_integer_) .kidpr_ft_artificial(ctx, "ur")
.kidpr_ft_rr <- function(ctx, i = NA_integer_) .kidpr_ft_artificial(ctx, "rr")
.kidpr_ft_rx <- function(ctx, i = NA_integer_) .kidpr_ft_artificial(ctx, "rx")

# TT~~, TT": thin bands of alluvium or of volcanic ash in a peat soil: a
# mineral layer between peat layers, of fluvial or of tephric origin.
.kidpr_ft_bands <- function(ctx, which) {
  x <- ctx$x
  m <- ctx$main
  peat <- vapply(m, function(j) .kidpr_ft_is_peat(ctx, j), NA)
  k <- which(!peat)
  band <- k[k > 1L & k < length(m)]
  band <- band[peat[band - 1L] & peat[band + 1L]]
  if (!length(band)) return(.kidpr_fd(FALSE, said = FALSE, note = "no mineral band inside the peat"))
  pat <- if (which == "al") "fluv|alluv|lacustr|\u0430\u043b\u043b\u044e\u0432" else "tephr|volcan|pyroclast|ash|\u043f\u0435\u043f\u0435?\u043b"
  o <- vapply(m[band], function(j) paste(.kidpr_txt(x, j, "layer_origin"), .kidpr_txt(x, j, "rock_origin")), "")
  hit <- grepl(pat, o)
  if (any(hit)) return(.kidpr_fd(TRUE, layers = m[band][hit], note = sprintf("bands of %s in the peat", if (which == "al") "alluvium" else "volcanic ash")))
  if (all(grepl("^NA NA$", o))) return(.kidpr_fd(NA, said = FALSE, missing = "layer_origin", note = "mineral bands in the peat, their origin not recorded"))
  .kidpr_fd(FALSE, note = "the mineral bands in the peat are of another origin")
}
.kidpr_ft_ttal <- function(ctx, i = NA_integer_) .kidpr_ft_bands(ctx, "al")
.kidpr_ft_ttash <- function(ctx, i = NA_integer_) .kidpr_ft_bands(ctx, "ash")


# --- the indices of the chapter 4 lists ----------------------------------------------

# w: a W on an abandoned agro horizon or on the middle horizon of a
# truncated soil: the key on the data reads W directly over the host.
.kidpr_ft_w <- function(ctx, i) {
  a <- .kidpr_ft_prev(ctx, i)
  if (is.na(a)) return(.kidpr_fd(FALSE, said = FALSE, note = "no layer above the host"))
  k <- .kidpr_data_layer(ctx$x, a)
  if (identical(k$code, "W")) return(.kidpr_fd(TRUE, note = sprintf("key 2.1 reads W on %s, over the host", .kidpr_ft_lab(ctx, a))))
  if ("W" %in% k$candidates) return(.kidpr_fd(NA, missing = k$missing, note = sprintf("%s may be W", .kidpr_ft_lab(ctx, a))))
  .kidpr_fd(FALSE, said = FALSE, note = "no W over the host")
}

# x: the contaminated horizon X (pollutants at the extremely hazardous level
# in a layer starting within 30 cm), in a soil the key did not send to the
# chemozems CU.
.kidpr_ft_x <- function(ctx, i = NA_integer_) {
  if (identical(ctx$division, "CU")) return(.kidpr_fd(FALSE, note = "the chemozems: the X names the division"))
  rs <- lapply(ctx$ord, function(j) .kidpr_contaminated(ctx$x, j))
  p <- vapply(rs, function(r) r$passed, NA)
  if (any(p %in% TRUE)) return(.kidpr_fd(TRUE, layers = ctx$ord[p %in% TRUE], note = "pollutants at the extremely hazardous level within 30 cm"))
  if (any(is.na(p))) return(.kidpr_fd(NA, missing = "contamination_hazard", note = "contamination recorded without its hazard category"))
  .kidpr_fd(FALSE, said = FALSE, note = "no contamination recorded within 30 cm")
}


# --- the notation -------------------------------------------------------------------

# Is a feature written on a layer: TRUE or FALSE on a layer written in the
# Russian notation (a feature not written is absent), TRUE where the
# designation of another notation suggests it (the FAO g, y, z, k, @, f),
# else NA. BH and BF are the modifications of BHF; TT~~ and TT" the marks
# on TT; w also a W written over the host; x an X laid over a layer.
.kidpr_ft_written <- function(ctx, i, f) {
  x <- ctx$x; d <- x$des
  if (is.na(i)) return(list(passed = NA, note = character(0)))
  lab <- .kidpr_ft_lab(ctx, i)
  if (x$kidpr && isTRUE(d$known[i])) {
    fe <- d$features[[i]] %||% character(0)
    w <- f %in% fe
    if (f %in% c("BH", "BF")) w <- identical(d$modification[i], f)
    if (f == "[e-hf]") w <- w || all(c("e", "hf") %in% fe)
    if (f == "TT~~") w <- identical(d$index[i], "TT") && isTRUE(grepl("~", d$mark[i]))
    if (f == "TT\"") w <- identical(d$index[i], "TT") && isTRUE(grepl("[\"`']", d$mark[i]))
    if (f == "w") {
      a <- .kidpr_ft_prev(ctx, i)
      w <- w || (!is.na(a) && isTRUE(d$known[a]) && identical(d$index[a], "W"))
    }
    if (f == "x") w <- "X" %in% c(d$overlay[[i]], d$index[i])
    return(list(passed = w, note = sprintf("the Russian notation %s %s on %s", if (w) "writes" else "does not write", f, lab)))
  }
  hf <- d$hint_features[[i]] %||% character(0)
  if (f %in% hf) return(list(passed = TRUE, hint = TRUE,
                             note = sprintf("the designation of %s suggests %s (a letter of another notation)", lab, f)))
  list(passed = NA, note = character(0))
}

# The notation of the whole profile: written on some layer (TRUE), on none
# of a profile written in it (FALSE), else NA.
.kidpr_ft_written_profile <- function(ctx, f) {
  ws <- lapply(ctx$ord, function(j) .kidpr_ft_written(ctx, j, f))
  p <- vapply(ws, function(w) w$passed, NA)
  if (any(p %in% TRUE)) return(list(passed = TRUE, layers = ctx$ord[p %in% TRUE],
                                    note = unlist(lapply(ws[p %in% TRUE], `[[`, "note"))))
  if (ctx$x$kidpr && any(p %in% FALSE)) return(list(passed = FALSE, layers = integer(0),
                                                     note = sprintf("the Russian notation writes %s on no layer", f)))
  list(passed = NA, layers = integer(0), note = character(0))
}


# --- one feature ---------------------------------------------------------------------

# The verdict of one layer: the data when they say it, else the notation,
# else the weak reading of the data, else the default of the feature (a
# field observation not recorded is absent, a measurement not recorded is
# undecided).
.kidpr_ft_combine <- function(dres, n, kind) {
  kind <- dres$kind %||% kind
  dn <- n$passed
  if (dres$said && !is.na(dres$passed)) {
    agree <- !is.na(dn) && identical(dn, dres$passed)
    dis <- if (!is.na(dn) && !agree) sprintf("the notation reads otherwise (%s); the data decide", paste(n$note, collapse = "; "))
    return(list(passed = dres$passed, route = if (agree) "both" else "data",
                notes = c(dres$note, if (agree) n$note, dis), tag = dres$tag, missing = character(0)))
  }
  if (!is.na(dn)) return(list(passed = dn, route = "notation", notes = c(n$note, dres$note), tag = character(0),
                              missing = character(0)))
  if (!is.na(dres$passed)) return(list(passed = dres$passed, route = "data", notes = dres$note, tag = dres$tag,
                                       missing = character(0)))
  if (identical(kind, "observed") && !dres$said)
    return(list(passed = FALSE, route = "data", tag = dres$tag, missing = character(0),
                notes = c(dres$note, "not recorded: absent (a field observation)")))
  list(passed = NA, route = "data", notes = dres$note, tag = dres$tag, missing = dres$missing)
}

# The route of a verdict read on several layers: both where data and
# notation agree on every layer; the data where the data decided every
# layer (the notation agreeing on some); the notation where it decided
# every layer; both where some layers are decided by each.
.kidpr_ft_route <- function(routes) {
  routes <- routes[!is.na(routes)]
  if (!length(routes)) return(NA_character_)
  if (all(routes == "both")) "both" else if (all(routes %in% c("data", "both"))) "data"
  else if (all(routes == "notation")) "notation" else "both"
}

# One feature on one host: the layers it is read on, the verdict of each,
# and the verdict of the feature (TRUE when it holds on some host layer;
# undecided when it may; FALSE otherwise, and when no host is in the
# profile).
.kidpr_ft_one <- function(ctx, code, host = NULL, place = NULL) {
  meta <- .kidpr_ft_meta()[[code]]
  if (is.null(meta)) rlang::abort(sprintf("'%s' is no feature of the Russian classification.", code))
  fn <- get(paste0(".kidpr_ft_", meta$fn), mode = "function")
  kind <- meta$kind %||% "observed"
  place <- place %||% meta$place %||% "on"
  hosts <- if (is.null(host)) .kidpr_ft_hosts(meta$hosts, meta$not_on) else .kidpr_ft_hosts(host)
  if (code == "pb" && identical(ctx$division, "CJ") && is.null(host)) { hosts <- "any"; place <- "surface" }
  out <- list(passed = FALSE, layers = integer(0), route = NA_character_, evidence = character(0),
              missing = character(0), tags = character(0))
  if (place == "profile") {
    dres <- tryCatch(fn(ctx), error = function(e) .kidpr_fd(NA, note = sprintf("error: %s", conditionMessage(e))))
    n <- .kidpr_ft_written_profile(ctx, code)
    v <- .kidpr_ft_combine(dres, n, kind)
    lay <- if (identical(v$route, "notation")) n$layers else dres$layers
    out$passed <- v$passed
    out$layers <- if (isTRUE(v$passed)) lay else integer(0)
    out$route <- v$route
    out$evidence <- unique(v$notes)
    out$missing <- v$missing
    out$tags <- v$tag
    return(out)
  }
  cands <- .kidpr_ft_candidates(ctx, hosts, place, small_ok = isTRUE(meta$small))
  if (!length(cands$rows)) {
    out$evidence <- sprintf("no host horizon (%s) in the profile", paste(hosts, collapse = ", "))
    return(out)
  }
  per <- lapply(seq_along(cands$rows), function(k) {
    i <- cands$rows[k]
    dres <- tryCatch(fn(ctx, i), error = function(e) .kidpr_fd(NA, note = sprintf("error: %s", conditionMessage(e))))
    n <- .kidpr_ft_written(ctx, i, code)
    v <- .kidpr_ft_combine(dres, n, kind)
    if (is.na(cands$host[k]) && !isFALSE(v$passed)) {
      v$passed <- NA
      v$notes <- c(v$notes, "the horizon of the layer undecided")
      v$missing <- unique(c(v$missing, ctx$fr$missing[[i]]))
    }
    v$row <- i
    v
  })
  p <- vapply(per, function(v) v$passed, NA)
  verdict <- .kidpr_or(p)
  use <- if (isTRUE(verdict)) which(p %in% TRUE) else if (is.na(verdict)) which(is.na(p)) else seq_along(per)
  routes <- vapply(per[use], function(v) v$route, "")
  out$passed <- verdict
  out$layers <- if (isTRUE(verdict)) vapply(per[use], function(v) v$row, 0L) else if (is.na(verdict)) vapply(per[use], function(v) v$row, 0L) else integer(0)
  out$route <- .kidpr_ft_route(routes)
  out$evidence <- unlist(lapply(per[use], function(v) {
    nt <- unique(v$notes[nzchar(v$notes)])
    if (length(nt)) sprintf("%s: %s", .kidpr_ft_lab(ctx, v$row), paste(nt, collapse = "; ")) else character(0)
  }))
  out$missing <- if (is.na(verdict)) unique(unlist(lapply(per[use], `[[`, "missing"))) else character(0)
  out$tags <- unique(unlist(lapply(per[use], `[[`, "tag")))
  out
}


# --- the entries of a list ------------------------------------------------------------

# An entry read: the feature (canonical code, the aliases of the book read),
# or a host form of the chapter 4 lists (ELf, AUel, Th, Di, AUlc-BCAmc ...)
# as its parts, each with its host and place.
.kidpr_ft_parts <- function(feature, host = NULL, place = NULL) {
  voc <- .kidpr_vocabulary()
  meta <- .kidpr_ft_meta()
  f <- as.character(feature)[1L]
  if (f %in% names(voc$feature_aliases)) f <- unname(voc$feature_aliases[[f]])
  if (f %in% names(meta)) return(list(list(code = f, host = host, place = place)))
  hf <- Filter(function(e) identical(as.character(e$written), f), .kidpr_catalogue()$features$host_forms)
  if (!length(hf)) rlang::abort(sprintf("'%s' is no feature or host form of the Russian classification.", f))
  e <- hf[[1L]]
  hs <- as.character(unlist(e$host)); fs <- as.character(unlist(e$features))
  lapply(seq_along(fs), function(k) {
    code <- if (fs[k] %in% names(voc$feature_aliases)) unname(voc$feature_aliases[[fs[k]]]) else fs[k]
    # el written on AU is the el of the AU (solodised); on G or D, on them
    pl <- place %||% if (code == "el" && hs[k] == "AU") "in" else "on"
    list(code = code, host = host %||% hs[k], place = pl)
  })
}

#' The genetic features of a pedon by the Russian Field Guide of 2008
#'
#' Runs the feature tests on a pedon for a list of entries: each a feature
#' (its index, the aliases of the book read: tu, ko, ag, as, ael, hh, akl),
#' or a host form of the chapter 4 lists (ELf, ELg, Th, Oh, AUh, AUel, Gel,
#' Pw, PUw, TEmr, Tote, Di, Del, AJ[akl], and the pairs AUlc-BCAmc,
#' AUlc-BCAnc, AJlc-BCAnc, which need both parts), optionally with the host
#' horizons and the place (on, in, under, over, near, surface). The data
#' are read first, the Russian notation where they say nothing, then the
#' defaults (a field observation not recorded is absent, a measurement not
#' recorded undecided).
#' @param pedon A PedonRecord (or a context of .kidpr_ft_context()).
#' @param entries NULL for every feature on its own hosts; a character
#'   vector of features or host forms; or a list of lists with
#'   \code{feature}, and optionally \code{host} and \code{place}.
#' @param division The division the soil keyed to (x is no feature of the
#'   chemozems CU; pb reads the natural deflated soils of CJ).
#' @return A data frame, one row per entry: \code{feature} (as given),
#'   \code{code} (the canonical index of each part), \code{host},
#'   \code{place}, \code{layers} (the rows of the pedon's horizons, a list
#'   column), \code{passed} (TRUE, FALSE, NA), \code{route} (data, notation
#'   or both), \code{evidence} and \code{missing} (a list column).
#' @noRd
.kidpr_features <- function(pedon, entries = NULL, division = NA_character_) {
  ctx <- .kidpr_ft_context(pedon, division)
  if (is.null(entries)) entries <- names(.kidpr_ft_meta())
  if (is.character(entries)) entries <- lapply(entries, function(e) list(feature = e))
  rows <- lapply(entries, function(e) {
    parts <- .kidpr_ft_parts(e$feature, e$host, e$place)
    res <- lapply(parts, function(pt) .kidpr_ft_one(ctx, pt$code, pt$host, pt$place))
    p <- vapply(res, function(r) r$passed, NA)
    routes <- unique(stats::na.omit(vapply(res, function(r) r$route, "")))
    hosts <- vapply(parts, function(pt) {
      h <- if (is.null(pt$host)) .kidpr_ft_hosts(.kidpr_ft_meta()[[pt$code]]$hosts, .kidpr_ft_meta()[[pt$code]]$not_on)
           else as.character(unlist(pt$host))
      paste(h, collapse = ",")
    }, "")
    places <- vapply(parts, function(pt) pt$place %||% .kidpr_ft_meta()[[pt$code]]$place %||% "on", "")
    data.frame(feature = as.character(e$feature), code = paste(vapply(parts, `[[`, "", "code"), collapse = "+"),
               host = paste(hosts, collapse = "+"), place = paste(places, collapse = "+"),
               layers = I(list(sort(unique(unlist(lapply(res, `[[`, "layers")))))),
               passed = .kidpr_and(p),
               route = if (!length(routes)) NA_character_ else if (length(routes) == 1L) routes else "both",
               evidence = .kidpr_evidence(unlist(lapply(res, `[[`, "evidence"))),
               missing = I(list(unique(unlist(lapply(res, `[[`, "missing"))))),
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}
