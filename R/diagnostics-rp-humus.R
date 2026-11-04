# =============================================================================
# v0.9.353 -- the humus forms of the French Referentiel pedologique 2008
# (AFES; Baize and Girard coord.; Quae, 2009), Annex 1 ("Typologie des formes
# d'humus forestieres", pp. 327-355; B. Jabiol, A. Brethes, J.-J. Brun,
# J.-F. Ponge).
#
# The annex is a typology of its own: the O and A horizons of the epipedon
# ("episolum humifere") attach it to a humus form, whose name enters the name
# of the solum as "a <form>" (pp. 56, 106). soilKey reads it in a fixed order
# (Hugo's decision Q22, 2026-11-02; inst/rules/rp2008/humus_forms.yaml):
#   1 an epipedon with H horizons is the histosols' (p. 351): no form;
#   2 an OHta makes a tangel (p. 349), an An an anmoor (p. 351): the codes
#     the references read too (organosols a tangel, reductisols a anmoor);
#   3 O horizons over an X, R or D: the forms without fine mineral earth
#     (peyromull, lithomoder, peyromoder, lithomor, peyromor,
#     peyro-amphimus; pp. 348-349);
#   4 open land by site$land_use (or a ploughed L): agri-mull, mull de
#     pelouse ... (p. 337);
#   5 the key of Figure 2 (p. 354) on the O and A horizons, walked by
#     .walk_key(): 11 outcomes;
#   6 a wet epipedon makes the outcome hydromorphic (hydromull, hydromoder,
#     hydromor; p. 350).
# Every test is three-valued: zoogenic or not (zo, noz) not recorded leaves
# the form undecided among its candidates; a recorded site$humus_form decides
# where the horizons leave it undecided, and a conflict goes to the trace.
# The qualifiers of the humus forms (pH, S/CEC, Ca/Mg, carbonatee, sodisee,
# lignique, xylique; pp. 338-340) are read on the first A horizon.
# =============================================================================

.rp_humus_cache <- new.env(parent = emptyenv())

# The rules of the humus forms, read once.
.rp_humus_rules <- function() {
  if (!is.null(.rp_humus_cache$rules)) return(.rp_humus_cache$rules)
  f <- system.file("rules", "rp2008", "humus_forms.yaml", package = "soilKey")
  r <- yaml::read_yaml(f)
  r$form_by_code <- stats::setNames(r$humus_forms, vapply(r$humus_forms, `[[`, "", "code"))
  assign("rules", r, envir = .rp_humus_cache)
  r
}

# The name, type and group of a form (or a type) by its code.
.rp_humus_name <- function(code) {
  if (is.na(code)) return(NA_character_)
  f <- .rp_humus_rules()$form_by_code[[code]]
  if (is.null(f)) code else f$name
}
.rp_humus_type <- function(code) {
  if (is.na(code)) return(NA_character_)
  if (code %in% c("mull", "moder", "mor", "amphimus")) return(code)
  f <- .rp_humus_rules()$form_by_code[[code]]
  if (is.null(f) || is.null(f$type)) NA_character_ else f$type
}

# A recorded humus form read as a code of the annex: a form, a type, or NA
# when it names none (a peat, "tourbe", is the histosols').
.rp_humus_recorded <- function(v) {
  if (length(v) != 1L || is.na(v)) return(list(code = NA_character_, written = NA_character_))
  w <- as.character(v)
  s <- .field_term(w)
  s <- gsub("[^a-z]+", "_", s)
  s <- gsub("^_+|_+$", "", s)
  alias <- c(moder_ou_eumoder = "eumoder", humus_brut = "mor", mor_humus_brut = "mor", amphimull = "euamphimus",
             amphimus_typique = "euamphimus", mull_typique = "eumull",
             litho_amphimus = "tangel", peyroamphimus = "peyro_amphimus", agrimull = "agri_mull",
             agrimoder = "agri_moder", agrimor = "agri_mor", dysmull_actif = "dysmull")
  if (s %in% names(alias)) s <- alias[[s]]
  ok <- s %in% c("mull", "moder", "mor", "amphimus", names(.rp_humus_rules()$form_by_code))
  list(code = if (ok) s else NA_character_, written = w)
}


# --- the epipedon ---------------------------------------------------------------------------

# The epipedon read on the frame: the holorganic layers at the top (O and
# H, in depth order), the first mineral layer under them, and what the
# tests need of each. Frame rows are indexed k; the horizons of the view i.
.rp_humus_context <- function(pedon) {
  fr <- .rp_frame(pedon)
  x <- .rp_view(pedon)
  n <- nrow(fr)
  ord <- if (n) order(ifelse(is.na(fr$top_cm), Inf, fr$top_cm), seq_len(n)) else integer(0)
  o_fam <- c(.rp_family_members("O"), "O")
  is_h <- function(k) isTRUE(fr$code[k] %in% c(.rp_family_members("H"), "H"))
  # a row is holorganic when its code is an O or an H, or when it is
  # undecided only between O and H codes
  holo <- function(k) {
    cd <- fr$code[k]
    if (!is.na(cd)) return(cd %in% c(o_fam, .rp_family_members("H"), "H"))
    cc <- as.character(fr$candidates[[k]])
    length(cc) > 0L && all(cc %in% c(o_fam, .rp_family_members("H"), "H"))
  }
  top_rows <- integer(0)
  for (k in ord) if (holo(k)) top_rows <- c(top_rows, k) else break
  rest <- setdiff(ord, top_rows)
  m <- if (length(rest)) rest[1L] else NA_integer_
  hidx <- function(k) fr$horizon_idx[k]
  # an undecided top layer that may be an H: the histosols possible (p. 351)
  h_maybe <- any(vapply(setdiff(top_rows, top_rows[vapply(top_rows, is_h, NA)]), function(k)
    is.na(fr$code[k]) && any(as.character(fr$candidates[[k]]) %in% c(.rp_family_members("H"), "H")), NA))
  list(fr = fr, x = x, ord = ord, holo = top_rows, h_maybe = h_maybe,
       o_rows = top_rows[!vapply(top_rows, is_h, NA)],
       h_rows = top_rows[vapply(top_rows, is_h, NA)],
       m = m, m2 = if (length(rest) >= 2L) rest[2L] else NA_integer_, hidx = hidx,
       rp = isTRUE(x$rp))
}

# The membership of the rows `rows` in a set of codes, three-valued.
.rp_humus_in <- function(ctx, rows, codes) {
  if (!length(rows)) return(logical(0))
  .rp_members(ctx$fr, codes)[rows]
}

# Zoogenic or not of an OF or OH row: the suffix zo or noz of the notation,
# else o_zoogenic; NA when neither is recorded.
.rp_humus_zo <- function(ctx, k) {
  f <- ctx$fr$features[[k]]
  if ("zo" %in% f) return("zo")
  if ("noz" %in% f) return("noz")
  v <- .rp_txt(ctx$x, ctx$hidx(k), "o_zoogenic")
  if (!is.na(v) && v %in% c("zo", "noz")) return(v)
  NA_character_
}

# The lateral cover of an O row (% of the ground): lateral_cover_pct; else
# in the RP notation, written in parentheses discontinuous and without them
# continuous; else NA.
.rp_humus_cover <- function(ctx, k) {
  i <- ctx$hidx(k)
  cv <- .rp_num(ctx$x, i, "lateral_cover_pct")
  if (!is.na(cv)) return(list(continuous = cv >= .RP_HUMUS_CONTINUOUS, value = cv, how = "lateral_cover_pct"))
  if (ctx$rp && isTRUE(ctx$x$des$known[i]))
    return(list(continuous = !isTRUE(ctx$x$des$discontinuous[i]), value = NA_real_,
                how = if (isTRUE(ctx$x$des$discontinuous[i])) "written in parentheses" else "written without parentheses"))
  list(continuous = NA, value = NA_real_, how = "not recorded")
}

# A lateral cover at or above this share of the ground makes an O horizon
# continuous (Hugo's decision, 2026-11-04).
.RP_HUMUS_CONTINUOUS <- 90
# An OH on less than this share of the ground is "very sporadic": the
# hemimoder (p. 344).
.RP_HUMUS_SPORADIC <- 10

# The O rows that count as an OH (an OH on under 10 % of the ground does not).
.rp_humus_oh <- function(ctx) {
  r <- ctx$o_rows
  v <- .rp_humus_in(ctx, r, "OH")
  sporadic <- vapply(r, function(k) isTRUE(.rp_humus_cover(ctx, k)$value < .RP_HUMUS_SPORADIC), NA)
  v[sporadic] <- FALSE
  list(rows = r, member = v)
}

# The A of the epipedon: the first mineral layer under the O horizons when it
# is an A (TRUE), may be (NA) or is not (FALSE: an E, an S ...).
.rp_humus_a <- function(ctx) {
  if (is.na(ctx$m)) return(FALSE)
  .rp_members(ctx$fr, "A")[ctx$m]
}

# A test answer for .walk_key(): list(passed, missing).
.rp_hk <- function(passed, missing = character(0)) list(passed = passed, missing = missing)


# --- the tests of the key of Figure 2 ------------------------------------------------------

.rp_humus_tests <- list(
  oh_present = function(ctx, ...) {
    oh <- .rp_humus_oh(ctx)
    .rp_hk(.rp_or3(c(FALSE, oh$member)), "fine_organic_matter_pct")
  },
  of_present = function(ctx, ...) {
    .rp_hk(.rp_or3(c(FALSE, .rp_humus_in(ctx, ctx$o_rows, "OF"))), "fine_organic_matter_pct")
  },
  # an OFnoz: an OF row that is noz; NA when an OF (or a row that may be one)
  # has its zoogenic character unrecorded
  ofnoz_present = function(ctx, ...) {
    r <- ctx$o_rows; v <- .rp_humus_in(ctx, r, "OF")
    z <- vapply(r, function(k) .rp_humus_zo(ctx, k), "")
    out <- .rp_or3(c(FALSE, ifelse(v %in% FALSE, FALSE, ifelse(z %in% "noz", v, ifelse(is.na(z), NA, FALSE)))))
    .rp_hk(out, "o_zoogenic")
  },
  # an OFzo or OHzo
  zo_present = function(ctx, ...) {
    r <- ctx$o_rows; v <- .rp_or3v(.rp_humus_in(ctx, r, "OF"), .rp_humus_oh(ctx)$member)
    z <- vapply(r, function(k) .rp_humus_zo(ctx, k), "")
    out <- .rp_or3(c(FALSE, ifelse(v %in% FALSE, FALSE, ifelse(z %in% "zo", v, ifelse(is.na(z), NA, FALSE)))))
    .rp_hk(out, "o_zoogenic")
  },
  # the continuous OH horizons 1 cm thick or more in all: a discontinuous
  # OH alone makes an eumoder (p. 344; Hugo's decision, 2026-11-04)
  oh_1cm = function(ctx, ...) {
    oh <- .rp_humus_oh(ctx)
    th <- ctx$fr$bottom_cm[oh$rows] - ctx$fr$top_cm[oh$rows]
    cont <- vapply(oh$rows, function(k) .rp_humus_cover(ctx, k)$continuous, NA)
    m <- ifelse(cont %in% FALSE, FALSE, ifelse(oh$member %in% TRUE & cont %in% TRUE, TRUE, ifelse(oh$member %in% FALSE, FALSE, NA)))
    sure <- sum(th[m %in% TRUE], na.rm = TRUE)
    if (sure >= 1) return(.rp_hk(TRUE))
    if (any(m %in% TRUE & is.na(th))) return(.rp_hk(NA, c("top_cm", "bottom_cm")))
    poss <- sure + sum(th[is.na(m)], na.rm = TRUE)
    if (any(is.na(m)) && (poss >= 1 || any(is.na(m) & is.na(th))))
      return(.rp_hk(NA, c("fine_organic_matter_pct", "lateral_cover_pct")))
    .rp_hk(FALSE)
  },
  # the A clearly crumbly: biomacro- or biomesostructured (pp. 334, 347);
  # no A under the O horizons, not crumbly. The recorded humus form does
  # not stand in for the biological structure here: it decides only where
  # the horizons leave the form undecided (Q22)
  a_crumbly = function(ctx, ...) {
    a <- .rp_humus_a(ctx)
    if (isFALSE(a)) return(.rp_hk(FALSE))
    b <- .rp_biomacro(ctx$x, ctx$hidx(ctx$m), meso = TRUE, humus = FALSE)
    .rp_hk(.rp_and3(c(a, b$passed)), b$missing)
  },
  of_continuous = function(ctx, ...) .rp_humus_continuous(ctx, "OF"),
  olv_present = function(ctx, ...) {
    r <- ctx$o_rows
    # a litter OL of unknown kind may be an OLv
    v <- vapply(r, function(k) {
      cd <- ctx$fr$code[k]
      if (identical(cd, "OLv")) TRUE else if (identical(cd, "OL")) NA
      else if (is.na(cd) || identical(cd, "O")) { m <- .rp_humus_in(ctx, k, "OL"); if (isFALSE(m)) FALSE else NA }
      else FALSE
    }, NA)
    .rp_hk(.rp_or3(c(FALSE, v)), "designation")
  },
  olv_continuous = function(ctx, ...) .rp_humus_continuous(ctx, "OLv"),
  oln_continuous = function(ctx, ...) .rp_humus_continuous(ctx, "OLn")
)

# Three-valued OR of two logical vectors, element by element.
.rp_or3v <- function(a, b) {
  vapply(seq_along(a), function(j) .rp_or3(c(a[j], b[j])), NA)
}

# Is one horizon of `code` (OF, OLv, OLn) continuous? The rows that are
# surely of that code; an OL of unknown kind may be an OLn or OLv.
.rp_humus_continuous <- function(ctx, code) {
  r <- ctx$o_rows
  v <- if (code == "OF") .rp_humus_in(ctx, r, "OF")
       else vapply(r, function(k) { cd <- ctx$fr$code[k]
         if (identical(cd, code)) TRUE else if (identical(cd, "OL")) NA
         else if (is.na(cd) || identical(cd, "O")) { m <- .rp_humus_in(ctx, k, "OL"); if (isFALSE(m)) FALSE else NA }
         else FALSE }, NA)
  cont <- vapply(r, function(k) .rp_humus_cover(ctx, k)$continuous, NA)
  each <- ifelse(v %in% FALSE, FALSE, ifelse(cont %in% FALSE, FALSE, ifelse(v %in% TRUE & cont %in% TRUE, TRUE, NA)))
  .rp_hk(.rp_or3(c(FALSE, each)), c("lateral_cover_pct", "designation"))
}

# The key of Figure 2 as a tree for .walk_key().
.rp_humus_tree <- function() {
  key <- .rp_humus_rules()$humus_key
  tree <- lapply(key, function(p) {
    f <- .rp_humus_tests[[p$test]]
    list(test = function(layer, ctx) f(ctx), yes = p$pass, no = p$fail)
  })
  names(tree) <- vapply(key, `[[`, "", "id")
  tree
}


# --- the gates ------------------------------------------------------------------------------

# The codes under the O horizons that make a form without fine mineral
# earth: X (any kind), R or D.
.RP_HUMUS_NO_FINE <- c("X", "R", "D")

# The forms of the analogous type without fine earth: over X (peyro-) or
# over R or D (litho-); amphimus only over X (the tangel is its own gate).
.rp_humus_no_fine_forms <- function(type, over) {
  if (over == "X") switch(type, mull = "peyromull", moder = "peyromoder", mor = "peyromor",
                          amphimus = "peyro_amphimus", NA_character_)
  else switch(type, moder = "lithomoder", mor = "lithomor", NA_character_)
}

# The type of the O horizons alone, for the forms without fine earth: no OF
# and no OH, mull; an OF or OH non-zoogenic, mor; zoogenic, moder, and
# amphimus over an X whose fine earth is biomesostructured (pp. 348-349).
.rp_humus_o_types <- function(ctx, over) {
  t <- .rp_humus_tests
  of <- t$of_present(ctx)$passed; oh <- t$oh_present(ctx)$passed
  any_o <- .rp_or3(c(of, oh))
  noz <- .rp_or3(c(t$ofnoz_present(ctx)$passed, .rp_humus_ohnoz(ctx)))
  out <- character(0)
  if (!isTRUE(any_o)) out <- c(out, "mull")
  if (!isFALSE(any_o)) {
    if (!isFALSE(noz)) out <- c(out, "mor")
    if (!isTRUE(noz)) {
      amph <- if (over == "X" && !isFALSE(oh))
        .rp_and3(c(oh, .rp_biomacro(ctx$x, ctx$hidx(ctx$m), meso = TRUE, humus = FALSE)$passed))
      else FALSE
      if (!isTRUE(amph)) out <- c(out, "moder")
      if (!isFALSE(amph)) out <- c(out, "amphimus")
    }
  }
  out
}

# An OHnoz (or an OHzo) among the O rows (TRUE, FALSE, NA).
.rp_humus_ohz <- function(ctx, kind = "noz") {
  oh <- .rp_humus_oh(ctx)
  z <- vapply(oh$rows, function(k) .rp_humus_zo(ctx, k), "")
  .rp_or3(c(FALSE, ifelse(oh$member %in% FALSE, FALSE, ifelse(z %in% kind, oh$member, ifelse(is.na(z), NA, FALSE)))))
}
.rp_humus_ohnoz <- function(ctx) .rp_humus_ohz(ctx, "noz")

# The words of the land uses (whole words, folded to ASCII lower case).
.RP_HUMUS_CROP <- "\\b(crops?|cropland|croplands|arable|cultivated|cultivation|cultive|cultures?|tilled|tillage|ploughed|plowed|ploughing|plowing|vineyards?|vignes?|vignoble|orchards?|vergers?|market gardens?|market gardening|maraichage|maraicher|cereals?|maize|wheat|agricultural|agriculture|agricole)\\b"
.RP_HUMUS_GRASS <- "\\b(grass|grassland|grasslands|pastures?|meadows?|prairies?|lawns?|pelouses?|heaths?|heathland|landes?|steppes?|moors?|moorland|alpages?|rangelands?|parks?|hay|hayfields?|paturages?)\\b"
.RP_HUMUS_FOREST <- "\\b(forests?|forested|woods?|woodlands?|wooded|forets?|bois|taiga|silviculture|sylviculture|plantation forest|tree plantation)\\b"

# The land use as the open-land forms read it (p. 337): "crop" (cultivated,
# or a ploughed L at the surface), "grass" (grassland, heath, lawn,
# pasture), "forest", or NA when not recorded.
.rp_humus_land <- function(ctx) {
  lu <- .rp_site_txt(ctx$x, "land_use")
  veg <- .rp_site_txt(ctx$x, "vegetation")
  s <- paste(stats::na.omit(c(lu, veg)), collapse = " ")
  # whole words; a crop before a grassland before a forest, so that a
  # cleared forest now cropped or grazed reads by its present use
  if (grepl(.RP_HUMUS_CROP, s, perl = TRUE)) return("crop")
  if (grepl(.RP_HUMUS_GRASS, s, perl = TRUE)) return("grass")
  if (grepl(.RP_HUMUS_FOREST, s, perl = TRUE)) return("forest")
  if (!is.na(ctx$m) && "L" %in% ctx$fr$overlay[[ctx$m]]) return("crop")
  if (nzchar(s)) return("other")
  NA_character_
}

# The open-land forms (p. 337) by the A: case 1 biomacro- or
# mesostructured, case 4 the same with an OHzo; case 2 not crumbly
# (juxtaposition), case 3 massive.
.rp_humus_open <- function(ctx, land) {
  a <- .rp_humus_a(ctx)
  if (isFALSE(a)) return(list(forms = character(0), missing = character(0), note = "no A under the O horizons: no open-land form"))
  b <- .rp_biomacro(ctx$x, ctx$hidx(ctx$m), meso = TRUE, humus = FALSE)
  st <- .kidpr_structure(ctx$x, ctx$hidx(ctx$m))$type
  massive <- if (is.na(st)) NA else grepl("massive|structureless|apedal", st)
  ohzo <- .rp_humus_ohz(ctx, "zo")
  pick <- function(case) {
    fs <- Filter(function(f) identical(f$group, "open_land") && identical(as.integer(f$case), as.integer(case)) &&
                   f$land %in% c(land, "any"), .rp_humus_rules()$humus_forms)
    vapply(fs, `[[`, "", "code")
  }
  out <- character(0)
  if (!isFALSE(b$passed)) {
    if (!isTRUE(ohzo)) out <- c(out, pick(1))
    if (!isFALSE(ohzo)) out <- c(out, pick(4))
  }
  if (!isTRUE(b$passed)) {
    if (!isTRUE(massive)) out <- c(out, pick(2))
    if (!isFALSE(massive)) out <- c(out, pick(3))
  }
  list(forms = unique(out), missing = unique(c(b$missing, if (is.na(massive)) "structure_type")),
       note = character(0))
}

# Is the epipedon wet (pp. 350-351)? The first mineral layer under the O
# horizons is a G or an AG, or bears g; for the mor type the layer under
# the organo-mineral one counts too (its lower boundary with Ea, Ga, Go or
# Gr, p. 350).
.rp_humus_wet <- function(ctx, mor = FALSE) {
  one <- function(k) {
    if (is.na(k)) return(FALSE)
    .rp_members(ctx$fr, c("G", "AG", "-g"))[k]
  }
  .rp_or3(c(one(ctx$m), if (mor) one(ctx$m2)))
}

# The hydromorphic form of an aerated outcome: mull to hydromull, moder to
# hydromoder, mor to hydromor; an amphimus keeps its form (the annex
# describes no wet amphimus; Hugo's decision, 2026-11-04).
.rp_humus_hydro <- function(code) {
  switch(.rp_humus_type(code) %||% "", mull = "hydromull", moder = "hydromoder", mor = "hydromor", code)
}


# --- the qualifiers (pp. 338-340) ------------------------------------------------------------

# Classes of a value: the class it falls in (degree "pass") and a neighbour
# it reaches within the tolerance ("imperfect"). Classes half-open, the
# lower edge inclusive unless `lo_open`, the upper exclusive unless
# `hi_closed`.
.rp_humus_classes <- function(value, classes, tol = 0) {
  if (is.na(value)) return(NULL)
  rows <- list()
  for (cl in classes) {
    lo <- cl$lo %||% -Inf; hi <- cl$hi %||% Inf
    inside <- (if (isTRUE(cl$lo_open)) value > lo else value >= lo) &&
              (if (isTRUE(cl$hi_closed)) value <= hi else value < hi)
    near <- !inside && tol > 0 && value >= lo - tol && value <= hi + tol
    if (inside || near)
      rows[[length(rows) + 1L]] <- data.frame(qualifier = cl$name, degree = if (inside) "pass" else "imperfect",
                                              stringsAsFactors = FALSE)
  }
  if (length(rows)) do.call(rbind, rows) else NULL
}

# The qualifiers of the humus form read on the first A (and the woody
# debris of the O horizons): a data frame of qualifier, group, value,
# degree (pass, imperfect, undecided), page.
.rp_humus_qualifiers <- function(ctx) {
  q <- .rp_humus_rules()$humus_qualifiers
  x <- ctx$x
  out <- list()
  add <- function(df, group, value, page, note = NA_character_) {
    if (is.null(df) || !nrow(df)) return(invisible())
    df$group <- group; df$value <- value; df$page <- page; df$note <- note
    out[[length(out) + 1L]] <<- df[, c("qualifier", "group", "value", "degree", "page", "note")]
  }
  # a class whose condition is undecided: its degree "undecided"
  hedge <- function(df, cond) {
    if (is.null(df) || isTRUE(cond)) return(df)
    if (isFALSE(cond)) return(NULL)
    df$degree <- "undecided"; df
  }
  a <- .rp_humus_a(ctx)
  if (!isFALSE(a)) {
    i <- ctx$hidx(ctx$m)
    ph <- .rp_num(x, i, "ph_h2o")
    add(.rp_humus_classes(ph, q$ph$classes, q$ph$tol), "pH", ph, q$ph$page)
    carb <- .rp_carbonates(x, i)$carbonated
    if (isTRUE(carb)) add(data.frame(qualifier = q$carbonatee$name, degree = "pass"), "carbonates", NA_real_, q$carbonatee$page)
    else if (is.na(carb)) add(data.frame(qualifier = q$carbonatee$name, degree = "undecided"), "carbonates", NA_real_, q$carbonatee$page)
    bs <- .rp_bs(x, i)$value
    # saturee and subsaturee only on a non-carbonated A: undecided when the
    # carbonates are
    bd <- .rp_humus_classes(bs, q$bs$classes, q$bs$tol)
    if (!is.null(bd)) {
      nc <- bd$qualifier %in% vapply(Filter(function(cl) isTRUE(cl$not_carbonated), q$bs$classes), `[[`, "", "name")
      if (isTRUE(carb)) bd <- bd[!nc, , drop = FALSE]
      else if (is.na(carb)) bd$degree[nc] <- "undecided"
    }
    add(bd, "S/CEC", bs, q$bs$page)
    # Ca/Mg on a non-carbonated A, saturated or subsaturated (S/CEC 80 or
    # more); undecided when either is
    cm <- .rp_camg(x, i)
    add(hedge(.rp_humus_classes(cm, q$ca_mg$classes), .rp_and3(c(!carb, if (is.na(bs)) NA else bs >= 80))),
        "Ca/Mg", cm, q$ca_mg$page)
    na <- .rp_num(x, i, "na_cmol"); cec <- .rp_num(x, i, "cec_cmol")
    esp <- if (!is.na(na) && isTRUE(cec > 0)) 100 * na / cec else .rp_num(x, i, "esp_pct")
    if (!is.na(esp) && esp >= q$sodisee$lo && esp <= q$sodisee$hi)
      add(data.frame(qualifier = q$sodisee$name, degree = "pass"), "Na/CEC", esp, q$sodisee$page)
  }
  wood <- suppressWarnings(max(vapply(ctx$o_rows, function(k) .rp_num(x, ctx$hidx(k), "woody_fragments_pct"), 0), na.rm = TRUE))
  # the woody fragments of the SiBCS (branches of 2 cm or more) stand in for
  # the recognisable woody debris, flagged (Hugo's decision, 2026-11-04)
  if (is.finite(wood)) add(.rp_humus_classes(wood, q$wood$classes), "woody debris in O", wood, q$wood$page,
                           "woody_fragments_pct (branches of 2 cm or more) stands in for the woody debris, flagged")
  if (length(out)) { d <- do.call(rbind, out); rownames(d) <- NULL; d }
  else data.frame(qualifier = character(0), group = character(0), value = numeric(0),
                  degree = character(0), page = character(0), note = character(0), stringsAsFactors = FALSE)
}


# --- the reading ---------------------------------------------------------------------------

#' The humus form of the Referentiel pedologique 2008
#'
#' Attaches the epipedon ("episolum humifere") of a pedon to a humus form of
#' Annex 1 of the French Referentiel pedologique 2008 (pp. 327-355): the
#' four functional types (mull, moder, mor, amphimus) and their forms. The
#' horizons are read as \code{\link{classify_rp2008}} reads them (the RP
#' notation where written, else the data), then, in order: H horizons at the
#' top leave the form to the histosols (p. 351); an OHta makes a tangel, an
#' An an anmoor; O horizons directly over an X, R or D give the forms without
#' fine mineral earth (pp. 348-349); \code{site$land_use} or a ploughed L
#' gives the open-land forms (p. 337); otherwise the key of Figure 2 (p. 354)
#' is walked on the O and A horizons, and its outcome is made hydromorphic
#' when the first mineral layer is a G or bears g (p. 350).
#'
#' The key reads the kind of the O horizons (OL, OLn, OLv, OF, OH: the
#' notation, or \code{fine_organic_matter_pct}), their zoogenic character
#' (the suffixes zo and noz, or \code{o_zoogenic}), their lateral cover
#' (\code{lateral_cover_pct}, or the parentheses of the RP notation), the
#' thickness of the OH and the biological structure of the A
#' (\code{biogenic_aggregates_pct}, else the structure). Every test is
#' three-valued: what is not recorded leaves the form undecided among its
#' candidates. A recorded \code{site$humus_form} decides when the horizons
#' leave the form undecided (even outside the forms they leave possible, the
#' conflict reported); when the horizons decide another form, they prevail
#' and the conflict is reported.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A list: \code{form} (the code of the form, NA when undecided),
#'   \code{name} (its name as printed), \code{type} (mull, moder, mor or
#'   amphimus, NA when undecided or for the anmoor), \code{label} (the name of
#'   the form when decided, else of the type when decided, else NA: what
#'   follows "a" in the name of the solum), \code{group} (aerated,
#'   hydromorphic, no_fine_earth, open_land, histic or NA), \code{decided},
#'   \code{route} ("horizons", "recorded" or "none"), \code{candidates} (the
#'   codes of the forms still possible), \code{no_form_possible} (TRUE when
#'   the horizons may also fit no form), \code{recorded} (the recorded
#'   \code{site$humus_form}), \code{conflict} (the recorded form disagrees
#'   with the horizons), \code{qualifiers} (a data frame: \code{qualifier},
#'   \code{group}, \code{value}, \code{degree} (pass, imperfect,
#'   undecided), \code{page}, \code{note}), \code{path} and
#'   \code{answers} (the walk of the key), \code{missing} (the attributes
#'   that would decide it) and \code{notes}.
#' @references Baize D., Girard M.-C. (coord.) (2009). Referentiel
#'   pedologique 2008. AFES, Editions Quae, Versailles. Annex 1, pp. 327-355.
#' @seealso \code{\link{classify_rp2008}}.
#' @examples
#' humus_form_rp(make_rp2008_example("organosols_holorganiques"))$label
#' @export
humus_form_rp <- function(pedon) {
  if (!inherits(pedon, "PedonRecord"))
    rlang::abort("humus_form_rp(): 'pedon' must be a PedonRecord (see PedonRecord$new()).")
  .rp_cached(pedon, "humus_form", function() .rp_humus_read(pedon))
}

.rp_humus_read <- function(pedon) {
  ctx <- .rp_humus_context(pedon)
  res <- .rp_humus_gates(ctx)
  forms <- res$forms
  decided <- length(forms) == 1L && !isTRUE(res$none)
  form <- if (decided) forms else NA_character_
  types <- unique(vapply(forms, .rp_humus_type, ""))
  type <- if (length(types) == 1L && !is.na(types)) types else NA_character_
  route <- if (decided) "horizons" else "none"
  # the recorded humus form (Q22)
  rec <- .rp_humus_recorded(.rp_site(ctx$x, "humus_form"))
  conflict <- FALSE
  notes <- res$notes
  if (!is.na(rec$written) && is.na(rec$code))
    notes <- c(notes, sprintf("the recorded humus form '%s' is no form of Annex 1", rec$written))
  if (!is.na(rec$code)) {
    rt <- .rp_humus_type(rec$code)
    is_type <- rec$code %in% c("mull", "moder", "mor", "amphimus")
    fits <- if (is_type) (length(forms) == 0L || rec$code %in% types)
            else (length(forms) == 0L || rec$code %in% forms)
    if (decided) {
      if (!(rec$code == form || (is_type && identical(rt, type)))) {
        conflict <- TRUE
        notes <- c(notes, sprintf("the recorded humus form '%s' disagrees with the horizons (%s), which prevail (Q22)",
                                  rec$written, .rp_humus_name(form)))
      }
    } else {
      if (!fits && length(forms)) {
        conflict <- TRUE
        notes <- c(notes, sprintf("the recorded humus form '%s' is not among the forms the horizons leave possible (%s)",
                                  rec$written, paste(vapply(forms, .rp_humus_name, ""), collapse = ", ")))
      }
      notes <- c(notes, sprintf("the horizons leave the form undecided: the recorded '%s' decides (Q22)", rec$written))
      route <- "recorded"
      if (is_type) {
        type <- rec$code
        keep <- forms[vapply(forms, .rp_humus_type, "") %in% rec$code]
        if (length(keep) == 1L) { form <- keep; decided <- TRUE } else forms <- keep
      } else {
        form <- rec$code; type <- rt; decided <- TRUE
      }
      if (decided) forms <- form
    }
  }
  grp <- if (!is.na(form)) .rp_humus_rules()$form_by_code[[form]]$group %||% res$group else res$group
  label <- if (!is.na(form)) .rp_humus_name(form) else type
  q <- .rp_humus_qualifiers(ctx)
  list(form = form, name = .rp_humus_name(form), type = type, label = label, group = grp,
       decided = !is.na(form), route = route, candidates = if (is.na(form)) forms else form,
       no_form_possible = is.na(form) && isTRUE(res$none),
       recorded = rec$written, conflict = conflict, qualifiers = q,
       path = res$path, answers = res$answers,
       missing = if (is.na(form)) unique(res$missing) else character(0),
       notes = unique(notes), page = "327-355")
}

# The gates and the key, in order: the forms the horizons leave possible,
# the group, the walk of the key, the notes and the missing attributes.
.rp_humus_gates <- function(ctx) {
  notes <- character(0); missing <- character(0)
  path <- character(0); answers <- logical(0)
  cands <- character(0)          # forms of the gates left undecided
  none_pre <- FALSE              # a gate left undecided may give no form
  # the end of the reading: the forms possible, and whether no form is
  # possible too (a dead end of the key, a gate with no form, or an earlier
  # gate left undecided that gives none)
  done <- function(forms, grp, extra_notes = character(0), extra_missing = character(0), none = FALSE) {
    list(forms = unique(c(cands, forms)), none = none || none_pre || !length(forms),
         group = if (length(cands) || none_pre) NA_character_ else grp, notes = c(notes, extra_notes),
         missing = unique(c(missing, extra_missing)), path = path, answers = answers)
  }
  # 1 H horizons at the top: the histosols (p. 351); an undecided top
  # layer that may be an H leaves it possible
  if (length(ctx$h_rows))
    return(done(character(0), "histic", "H horizons at the top of the solum: an epipedon of the histosols (p. 351), no humus form of Annex 1"))
  if (isTRUE(ctx$h_maybe)) {
    none_pre <- TRUE
    notes <- c(notes, "a top layer may be an H: an epipedon of the histosols possible, with no form (p. 351)")
    missing <- c(missing, "water_saturation_days", "drainage_class")
  }
  # 2 the tangel and the anmoor, the codes the references read
  ta <- .rp_or3(c(FALSE, .rp_humus_in(ctx, ctx$o_rows, "OHta")))
  if (isTRUE(ta)) return(done("tangel", "no_fine_earth", "an OHta: a tangel (p. 349)"))
  if (is.na(ta)) {
    cands <- c(cands, "tangel")
    notes <- c(notes, "an O horizon may be an OHta: a tangel possible")
    missing <- c(missing, "fine_organic_matter_pct")
  }
  an <- if (is.na(ctx$m)) FALSE else .rp_members(ctx$fr, "An")[ctx$m]
  if (isTRUE(an)) return(done("anmoor", "hydromorphic", "an An: an anmoor (p. 351)"))
  if (is.na(an)) {
    cands <- c(cands, "anmoor")
    notes <- c(notes, "the first mineral horizon may be an An: an anmoor possible")
    missing <- c(missing, unique(unlist(ctx$fr$missing[ctx$m])))
  }
  # 3 no fine mineral earth: O horizons directly over an X, R or D
  if (length(ctx$o_rows) && !is.na(ctx$m)) {
    nf <- .rp_members(ctx$fr, .RP_HUMUS_NO_FINE)[ctx$m]
    if (!isFALSE(nf)) {
      # over an X (peyro-), over an R or D (litho-), or both still possible
      overs <- c(if (!isFALSE(.rp_members(ctx$fr, "X")[ctx$m])) "X",
                 if (!isFALSE(.rp_members(ctx$fr, c("R", "D"))[ctx$m])) "R")
      fs_by <- lapply(overs, function(o) {
        ty <- .rp_humus_o_types(ctx, o)
        unique(as.character(stats::na.omit(vapply(ty, .rp_humus_no_fine_forms, "", over = o, USE.NAMES = FALSE))))
      })
      fs <- unique(unlist(fs_by))
      # a litter alone over the rock has no form (`lithomull`, p. 337, is
      # defined nowhere), nor a reading of the O that gives only a mull
      # over an R or D
      none_nf <- !length(fs) || any(vapply(seq_along(overs), function(j) {
        ty <- .rp_humus_o_types(ctx, overs[j])
        any(is.na(vapply(ty, .rp_humus_no_fine_forms, "", over = overs[j])))
      }, NA))
      msg <- sprintf("O horizons directly over %s: a form without fine mineral earth (pp. 348-349)",
                     paste(c(X = "an X", R = "an R or D")[overs], collapse = " or "))
      if (none_nf) msg <- c(msg, "a litter alone over the rock has no form of the annex ('lithomull', p. 337, is defined nowhere)")
      if (isTRUE(nf))
        return(done(fs, "no_fine_earth", msg, if (length(fs) > 1L || none_nf) c("o_zoogenic", "fine_organic_matter_pct", "biogenic_aggregates_pct"),
                    none = none_nf))
      cands <- c(cands, fs)
      if (none_nf) none_pre <- TRUE
      notes <- c(notes, sprintf("the layer under the O horizons may be an X, R or D: %s possible",
                                if (length(fs)) paste(fs, collapse = ", ") else "no form"))
      missing <- c(missing, unique(unlist(ctx$fr$missing[ctx$m])))
    }
  }
  # 4 open land (p. 337)
  land <- .rp_humus_land(ctx)
  if (land %in% c("crop", "grass")) {
    op <- .rp_humus_open(ctx, land)
    return(done(op$forms, "open_land",
                c(sprintf("open land (%s): the forms of p. 337", if (land == "crop") "cultivated" else "grassland or heath"), op$note),
                op$missing))
  }
  # 5 no O horizon recorded: the key reads the O horizons (Hugo's
  # decision, 2026-11-04)
  if (!length(ctx$o_rows))
    return(done(character(0), NA_character_,
                "no O horizon recorded: the key of Figure 2 reads the O horizons, the humus form is undecided", "humus_form"))
  # 6 the key of Figure 2 (p. 354)
  w <- .walk_key(.rp_humus_tree(), NULL, ctx)
  path <- if (length(w$path)) w$path else unique(unlist(w$paths))
  answers <- w$answers
  out <- as.character(stats::na.omit(w$candidates))
  kn <- character(0)
  if (w$dead_end) kn <- sprintf("%s of Figure 2 fits %s (an OLn only, discontinuous, over an A not crumbly)",
                                if (length(out)) "possibly no outcome" else "no outcome",
                                if (length(out)) "one reading of the horizons" else "the horizons")
  # 7 a wet epipedon (pp. 350-351), for each outcome: for a mor the layer
  # under the organo-mineral one counts too
  wets <- vapply(out, function(o) .rp_humus_wet(ctx, mor = identical(.rp_humus_type(o), "mor")), NA)
  forms <- unique(unlist(lapply(seq_along(out), function(j) {
    h <- .rp_humus_hydro(out[j])
    if (isTRUE(wets[j])) h else if (is.na(wets[j])) c(out[j], h) else out[j]
  })))
  if (any(wets %in% TRUE)) kn <- c(kn, "a G or g under the O horizons: a hydromorphic form (p. 350)")
  if (any(!(wets %in% FALSE) & vapply(out, .rp_humus_type, "") %in% "amphimus"))
    kn <- c(kn, "the annex describes no hydromorphic amphimus: the form is kept")
  # an early-winter eumull may show a continuous OLn of the year (p. 342):
  # the key gives a mesomull, as Figure 2 does (Hugo's decision, 2026-11-04)
  if (identical(out, "mesomull") && "K9b" %in% names(w$answers) && isTRUE(w$answers[["K9b"]]) && isTRUE(.rp_humus_tests$a_crumbly(ctx)$passed))
    kn <- c(kn, "a continuous OLn over a clearly crumbly A: a mesomull by the key; an eumull early in winter shows such a litter too (p. 342)")
  grp <- if (all(wets %in% TRUE)) "hydromorphic" else if (all(wets %in% FALSE)) "aerated" else NA_character_
  if (!length(out)) grp <- "aerated"
  done(forms, grp, kn, c(w$missing, if (anyNA(wets)) "redoximorphic_features_pct"), none = w$dead_end)
}

# The humus form as one row of the result of classify_rp2008().
.rp_humus_row <- function(h) {
  qp <- h$qualifiers[h$qualifiers$degree == "pass", , drop = FALSE]
  data.frame(form = h$form, name = h$name, type = h$type, label = h$label, group = h$group,
             decided = h$decided, route = h$route,
             candidates = paste(vapply(h$candidates, .rp_humus_name, ""), collapse = "; "),
             recorded = h$recorded, conflict = h$conflict,
             qualifiers = paste(qp$qualifier, collapse = ", "),
             stringsAsFactors = FALSE)
}
