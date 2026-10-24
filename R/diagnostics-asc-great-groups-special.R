# =============================================================================
# v0.9.284 -- great groups of the Australian Soil Classification (3rd ed.)
# for the Vertosols (the soil surface), Calcarosols (what the soil directly
# overlies, the B), Podosols (the kinds of Podosol B horizon) and Organosols
# (Folic, acid sulfate materials, carbonate and acidity of the organic
# materials). Three-valued; the readings are in inst/rules/asc3/readings.yaml.
# =============================================================================

# ---- Vertosols ------------------------------------------------------------------

# The surface of a soil: `top` the first row from the top of the profile (an
# O1 litter left out) below any thin organic layer that is not a peaty
# horizon; `organic` whether the surface is a peaty horizon (organic
# materials, read as the order reads them, at least 0.2 m: `peaty`); `a` the
# A horizon, the rows with an A role (A1, A2, A3, AB, A/B, AC) from there
# down to the first that is not (without designations, that row when it is
# mineral); `no_a` TRUE when the designations put another horizon there. A
# gap in the record at the surface leaves it all undecided.
.asc_surface <- function(x, om, peaty) {
  h <- x$h; top0 <- .asc_profile_top(x)
  none <- list(top = integer(0), organic = NA, a = integer(0), no_a = NA)
  if (is.na(top0)) return(none)
  ord <- order(h$top_cm, na.last = NA)
  ord <- ord[h$top_cm[ord] >= top0 - 0.5]
  if (!length(ord)) return(none)
  # an organic layer at the top: organic material (measured organic C
  # decides, the designation where it is not measured), or a P or O2 short of
  # it, which is passed over like any thin organic layer
  is_org <- ifelse(om %in% TRUE | (x$des$known & x$role %in% c("P", "O2")), TRUE, om)
  if (!(is_org[ord[1L]] %in% FALSE)) {
    if (isTRUE(peaty)) return(list(top = ord[1L], organic = TRUE, a = integer(0), no_a = TRUE))
    if (is.na(peaty) || is.na(is_org[ord[1L]])) return(utils::modifyList(none, list(top = ord[1L])))
    # a thin organic layer over the mineral soil: the surface is below it
    prev <- top0; k <- 1L
    while (k <= length(ord) && is_org[ord[k]] %in% TRUE && abs(h$top_cm[ord[k]] - prev) <= 0.5) {
      prev <- h$bottom_cm[ord[k]]; k <- k + 1L
      if (is.na(prev)) return(none)
    }
    if (k > length(ord) || is.na(is_org[ord[k]])) return(none)
    top0 <- prev; ord <- ord[k:length(ord)]
  }
  first <- ord[1L]
  if (h$top_cm[first] > top0 + 0.5) return(none)
  if (!x$any_known) return(list(top = first, organic = FALSE, a = first, no_a = FALSE))
  if (x$unknown_role[first] || is.na(x$role[first])) return(utils::modifyList(none, list(top = first)))
  out <- integer(0); prev <- top0
  for (i in ord) {
    if (!(x$role[i] %in% c("A1", "A2", "A3", "AB", "A/B", "AC")) || x$des$buried[i] ||
        abs(h$top_cm[i] - prev) > 0.5) break
    out <- c(out, i); prev <- h$bottom_cm[i]
    if (is.na(prev)) break
  }
  list(top = first, organic = FALSE, a = out, no_a = !length(out))
}

# The items of a site record ("cracking; surface crust" -> two items); a
# qualifier standing alone after a comma ("self-mulching, weak") stays with
# its item.
.asc_site_items <- function(txt) {
  if (!length(txt) || is.na(txt) || !nzchar(txt)) return(character(0))
  v <- trimws(strsplit(tolower(txt), "[;,]")[[1]])
  v <- v[nzchar(v)]
  words <- "very|weakly|weak|slightly|slight|poorly|moderately|moderate|strongly|strong|well|absent|nil|none|present|not|developed|expressed"
  qual <- paste0("^[-()[:space:]]*((", words, "|to)[-()[:space:]]*)+$")
  out <- character(0)
  for (it in v) {
    if (length(out) && grepl(qual, it)) out[length(out)] <- paste(out[length(out)], it) else out <- c(out, it)
  }
  out
}

#' Vertosol great groups: the surface
#'
#' \code{kind}: "self_mulching" (a moderately or strongly self-mulching
#' surface recorded: site$surface_condition), "epipedal" (a pedal A,
#' moderate or strong, blocky or polyhedral, not self-mulching, no surface
#' crusty horizon), "crusty" (a surface crusty horizon over pedal clay,
#' blocky or polyhedral, not self-mulching), "massive" (a massive or weak
#' blocky A, no surface crusty horizon), "peaty" (a surface peaty horizon).
#' The text asks that the self-mulching great group be used only where the
#' condition is not in doubt, the pedal surface being recorded otherwise: a
#' self-mulching surface the record does not show (or shows as weak) is read
#' as absent. A surface crusty horizon is a surface crust in the site record,
#' or a surface horizon 30 mm thick or less that is massive or weakly
#' structured; the pedal clay of the Crusty great group is read under it. A
#' peat at the surface, or no A horizon at the top, has no pedal or massive
#' surface A.
#' @noRd
vertosol_surface_asc <- function(pedon, kind = c("self_mulching", "epipedal", "crusty", "massive", "peaty")) {
  kind <- match.arg(kind)
  x <- .asc_horizons(pedon); h <- x$h
  surf <- .asc_site_text(pedon$site$surface_condition)
  items <- .asc_site_items(surf)
  neg <- "\\b(no|not|non|without|absent|nil|none)\\b|\\bun-?crust"
  mulch <- any(grepl("self[- ]?mulch", items) &
               !grepl(paste0(neg, "|\\b(weak|weakly|slight|slightly|poorly)"), items))
  crust_site <- any(grepl("crust", items) & !grepl(paste0(neg, "|salt|saline"), items))
  peaty <- .asc_chernic_parts(pedon)$peaty
  s <- .asc_surface(x, .asc_organic_material(x, pedon), peaty)
  a <- s$a; top_i <- s$top
  t <- tolower(trimws(as.character(h$structure_type)))
  g <- tolower(trimws(as.character(h$structure_grade)))
  pedal <- .asc_pedal(h)
  bp <- ifelse(is.na(t) | !nzchar(t), NA, grepl("blocky|polyhedral", t))
  pedal_bp <- .and3v(pedal, bp)
  weak_or_massive <- ifelse(pedal %in% TRUE, FALSE, ifelse(pedal %in% FALSE, TRUE, NA))
  # "massive or weak blocky": massive (structureless, not single grain), or
  # weak with blocky peds; peds of another type are neither, whatever their
  # grade
  t_known <- !is.na(t) & nzchar(t)
  massive_wb <- ifelse(grepl("massive|structureless|apedal", t), TRUE,
                ifelse(grepl("single", t), FALSE,
                ifelse(pedal %in% TRUE, FALSE,
                ifelse(t_known & !grepl("blocky", t), FALSE,
                ifelse(pedal %in% FALSE, ifelse(grepl("structureless|none|apedal", g) | t_known, TRUE, NA), NA)))))
  # a surface crusty horizon: recorded at the site, or a thin surface
  # horizon (30 mm or less) that is massive or weakly structured
  mineral_top <- length(top_i) > 0L && s$organic %in% FALSE
  thin_top <- mineral_top && !is.na(x$thick[top_i]) && x$thick[top_i] <= 3
  crust_h <- if (!length(top_i) || s$organic %in% TRUE) FALSE
             else if (is.na(s$organic) || is.na(x$thick[top_i])) NA
             else if (!thin_top) FALSE else weak_or_massive[top_i]
  crust <- .any3(c(crust_site, crust_h))
  # the pedal clay of a Crusty Vertosol lies under a thin surface horizon:
  # the rest of the A, or the horizon directly below when the A is no
  # thicker; under a surface crust of the site record, the A itself (or the
  # horizon at the top when there is no A)
  under <- if (!mineral_top) integer(0) else if (!thin_top) { if (length(a)) a else top_i } else {
    rest <- setdiff(a, top_i)
    if (length(rest)) rest else {
      bot <- h$bottom_cm[top_i]
      which(!is.na(h$top_cm) & abs(h$top_cm - bot) <= 0.5 & !x$des$buried)[1L]
    }
  }
  under <- under[!is.na(under)]
  major <- function(rows, ok) if (!length(rows)) NA else .asc_rows_major(x, rows, ok)
  surface_a <- .and3(!s$organic, !s$no_a)
  passed <- switch(kind,
    self_mulching = mulch,
    epipedal = .and3(!mulch, surface_a, major(a, pedal_bp), !crust),
    crusty = .and3(!s$organic, crust, !mulch, major(under, pedal_bp)),
    massive = .and3(surface_a, major(a, massive_wb), !crust),
    peaty = peaty)
  .asc_result(paste0("gg_vertosol_", kind), passed, a, list(surface = surf, crust = crust),
              if (is.na(passed)) c("site$surface_condition", "structure_grade", "structure_type", "oc_pct") else character(0),
              "Vertosols, great groups and Comment")
}

# ---- Calcarosols ----------------------------------------------------------------

# What the soil directly overlies: the rows whose top is the bottom of the
# deepest of the rows `sol` that is not a pan (integer(0) when the
# description ends there or goes on after a gap); `after_gap` the first rows
# below that bottom when none starts at it.
.asc_under_solum <- function(x, sol = which(x$solum %in% TRUE)) {
  h <- x$h
  none <- list(rows = integer(0), after_gap = integer(0))
  sol <- sol[!x$des$m[sol]]
  if (!length(sol)) return(none)
  bot <- suppressWarnings(max(h$bottom_cm[sol], na.rm = TRUE))
  if (!is.finite(bot)) return(none)
  rows <- which(!is.na(h$top_cm) & abs(h$top_cm - bot) <= 0.5 & !(seq_len(x$n) %in% sol))
  deeper <- which(!is.na(h$top_cm) & h$top_cm > bot + 0.5)
  list(rows = rows,
       after_gap = if (length(rows) || !length(deeper)) integer(0)
                   else deeper[abs(h$top_cm[deeper] - min(h$top_cm[deeper])) <= 0.5])
}

# The kind of substrate of a row: "rock" (an R horizon), "pan" (a cemented
# pan: an m horizon or a recorded pan of a cemented kind), "hard" (another
# layer moderately cemented or more: not unconsolidated, perhaps hard rock),
# "marl", "saprolite" (a layer or rock origin naming saprolite or weathered
# rock), "unconsolidated" (a sedimentary origin, or a buried soil), or NA (a
# C or D of unrecorded origin, or a pan of unrecorded kind). Fragipans,
# densipans and cultivation pans are not cemented.
.asc_substrate_kind <- function(x, i) {
  h <- x$h
  origin <- tolower(paste(h$layer_origin[i] %||% NA, h$rock_origin[i] %||% NA))
  pt <- tolower(trimws(as.character(h$pan_type[i] %||% NA)))
  if (x$rock[i] || x$role[i] %in% "R") return("rock")
  uncemented <- !is.na(pt) && grepl("fragipan|densipan|cultivation|plough|plow|tillage|traffic", pt)
  if (x$des$m[i] || (!is.na(pt) && nzchar(pt) && pt != "none" && !uncemented && !grepl("other pan", pt)))
    return("pan")
  if (x$hard[i]) return("hard")
  if (!is.na(pt) && grepl("other pan", pt)) return(NA_character_)
  if (grepl("\\bmarl", origin)) return("marl")
  if (grepl("saprolit|saprock|weathered rock|decomposed rock|residual|in situ", origin)) return("saprolite")
  if (x$des$buried[i] ||
      grepl("alluv|fluvi|aeolian|aeolic|eolian|colluv|solimov|marine|lacustrine|sediment|dune|transported", origin))
    return("unconsolidated")
  NA_character_
}

#' Calcarosol great groups
#'
#' \code{kind}: "duric" and "petrocalcic" (directly over a red-brown hardpan
#' or a calcrete pan), "rendic" (the A directly over a Bk of more than 80 %
#' soft carbonate), "argic" (an argic horizon, clay lamellae, within the B),
#' "pedal" (the major part of the B of a grade stronger than weak),
#' "lithic", "paralithic", "marly", "regolithic" (directly over hard rock,
#' partially weathered rock or saprolite, marl, or other unconsolidated
#' mineral materials). Rows of unknown role that may be part of the solum
#' (or of the B) are tried in and out: the answer holds only when both agree.
#' @noRd
calcarosol_gg_asc <- function(pedon, kind = c("duric", "petrocalcic", "rendic", "argic", "pedal",
                                              "lithic", "paralithic", "marly", "regolithic")) {
  kind <- match.arg(kind)
  x <- .asc_horizons(pedon); h <- x$h
  if (!x$any_known && kind != "pedal")
    return(.asc_result(paste0("gg_calcarosol_", kind), NA, integer(0), list(), "designation",
                       "Calcarosols, great groups"))
  # a lamellae horizon above the first C, D or R is B material in bands
  lam <- x$role %in% "lamellae" & !x$des$buried & !is.na(h$top_cm) & h$top_cm < x$c_top
  sol <- which(x$solum %in% TRUE | lam)
  maybe <- which(x$maybe_soil)
  # what the soil directly overlies, with the rows of unknown role out of the
  # solum and then in it
  unders <- list(.asc_under_solum(x, sol), .asc_under_solum(x, union(sol, maybe)))
  on <- function(f) { v <- lapply(unders, f); if (identical(v[[1L]], v[[2L]])) v[[1L]] else NA }
  over <- function(k) on(function(u) {
    if (!length(u$rows)) return(NA)
    sk <- vapply(u$rows, function(i) .asc_substrate_kind(x, i), "")
    # a cemented layer is not unconsolidated, nor marl, but may be hard rock
    # or a cemented saprolite
    sk[sk %in% "hard"] <- if (k %in% c("unconsolidated", "marl")) "other" else NA
    if (any(sk %in% k)) TRUE else if (anyNA(sk)) NA else FALSE
  })
  under <- unders[[1L]]$rows
  passed <- switch(kind,
    duric = , petrocalcic = {
      v <- .asc_pan_rows(x, if (kind == "duric") "red-brown hardpan" else "calcrete")
      on(function(u) {
        # a pan directly under the solum, or a pan designated in it
        cand <- c(u$rows, which(x$solum %in% TRUE & x$des$m))
        if (length(cand) && any(v[cand] %in% TRUE)) return(TRUE)
        # after a gap in the record, a pan first below may still lie directly
        # under the soil
        if (anyNA(v[cand]) || any(!(v[u$after_gap] %in% FALSE))) NA else FALSE
      })
    },
    rendic = {
      a <- which(x$role %in% c("A1", "A2", "A3", "AB", "A/B") & !x$des$buried)
      if (!length(a)) FALSE else {
        abot <- suppressWarnings(max(h$bottom_cm[a], na.rm = TRUE))
        nxt <- which(!is.na(h$top_cm) & abs(h$top_cm - abot) <= 0.5 & !(seq_len(x$n) %in% a))
        if (!length(nxt)) NA else {
          i <- nxt[1L]
          soft <- .asc_carbonate(h)$soft[i]
          soft_ok <- if (is.na(soft)) NA else soft > 80
          # a Bk; a row with no designation, or one of unknown role, may be one
          bk <- if (is.na(x$role[i]) || x$unknown_role[i]) NA else grepl("^B", x$role[i]) && x$des$k[i]
          .and3(bk, soft_ok)
        }
      }
    },
    # clay lamellae in the B: a B designation naming them, or a lamellae
    # horizon (B material in bands); lamellae are conspicuous
    argic = any(x$des$lamellae & !x$des$buried & grepl("^B", x$role)) || any(lam),
    pedal = {
      b <- .asc_b_rows(x); pd <- .asc_pedal(h)
      if (length(b)) .asc_rows_major_open(x, b, maybe, pd)
      else if (!length(maybe)) { if (x$any_known) FALSE else NA }
      else if (all(pd[maybe] %in% FALSE)) FALSE else NA
    },
    lithic = over("rock"), paralithic = over("saprolite"), marly = over("marl"),
    regolithic = over("unconsolidated"))
  .asc_result(paste0("gg_calcarosol_", kind), passed, under, list(),
              if (is.na(passed)) c("designation", "layer_origin", "pan_type", "carbonate_soft_pct", "caco3_pct",
                                   "structure_grade") else character(0),
              "Calcarosols, great groups; glossary 'Argic horizon', 'Marl', 'Unconsolidated mineral materials'")
}

# ---- Podosols --------------------------------------------------------------------

#' Podosol great groups: the kinds of Podosol B horizon
#'
#' Read from the designations of the Podosol B horizons (as the order reads
#' them): Bs (s without h), Bhs (h and s), Bh (h without s), and a Basi (a
#' cemented B, not h or s, directly under a Bh). \code{kind}: "pipey" (pipes
#' of bleached A2 more than 0.5 m into the B: a tongued lower boundary of the
#' A2 may be one; with none recorded the B is not pipey), "sesquic" (only
#' Bs), "humosesquic" (only Bhs), "humic" (only Bh), "humosesquic_sesquic"
#' (Bhs and Bs), "humic_sesquic" (Bh and Bs), "humic_humosesquic" (Bh and
#' Bhs), "humic_alsilic" (Bh over a Basi). Rows of unknown role that may be a
#' Podosol B may add any kind: a set is decided only when they cannot change
#' it.
#' @noRd
podosol_gg_asc <- function(pedon, kind = c("pipey", "sesquic", "humosesquic", "humic", "humosesquic_sesquic",
                                           "humic_sesquic", "humic_humosesquic", "humic_alsilic")) {
  kind <- match.arg(kind)
  x <- .asc_horizons(pedon); h <- x$h; des <- x$des
  bpod <- x$role %in% c("B1", "B2", "B3") & !des$buried & !des$t & (des$s | des$h) & !(des$s & des$m & !des$h)
  k <- ifelse(!bpod, NA, ifelse(des$h & des$s, "Bhs", ifelse(des$h, "Bh", "Bs")))
  kinds <- unique(stats::na.omit(k))
  # a Basi: a cemented B (not h or s) directly under a Bh; a missing depth
  # leaves it undecided
  bh <- which(k %in% "Bh")
  basi <- vapply(seq_len(x$n), function(i) {
    if (!(x$role[i] %in% c("B1", "B2", "B3") && !des$buried[i] && !des$h[i] && !des$s[i])) return(FALSE)
    if (!(des$m[i] || x$hard[i]) || !length(bh)) return(FALSE)
    d <- abs(h$bottom_cm[bh] - h$top_cm[i])
    if (any(d <= 0.5, na.rm = TRUE)) TRUE else if (anyNA(d)) NA else FALSE
  }, NA)
  if (any(basi %in% TRUE)) kinds <- c(kinds, "Basi")
  # kinds the rows of unknown role (or an undecided Basi) may add: a row of
  # unknown role may be a cemented B itself, or a Bh over a cemented B
  cem_b <- which(x$role %in% c("B1", "B2", "B3") & !des$buried & !des$h & !des$s & (des$m | x$hard))
  md <- which(x$maybe_desc)
  under_maybe <- length(md) && length(cem_b) &&
    any(outer(h$bottom_cm[md], h$top_cm[cem_b], function(b, t) !is.na(b) & !is.na(t) & abs(b - t) <= 0.5))
  open <- c(if (length(md)) c("Bs", "Bhs", "Bh"),
            if (any(x$maybe_desc & x$hard) || under_maybe || (anyNA(basi) && !any(basi %in% TRUE))) "Basi")
  only <- function(set) {
    if (!all(kinds %in% set)) return(FALSE)
    lacking <- setdiff(set, kinds)
    if (!all(lacking %in% open)) return(FALSE)
    if (!length(lacking) && !length(open)) TRUE else NA
  }
  passed <- switch(kind,
    pipey = {
      topo <- tolower(as.character(h$boundary_topography %||% rep(NA_character_, x$n)))
      a2 <- x$role %in% c("A2", "A3") & !des$buried
      if (any(a2 & grepl("tongue", topo))) NA else FALSE
    },
    sesquic = only("Bs"), humosesquic = only("Bhs"), humic = only("Bh"),
    humosesquic_sesquic = only(c("Bhs", "Bs")), humic_sesquic = only(c("Bh", "Bs")),
    humic_humosesquic = only(c("Bh", "Bhs")), humic_alsilic = only(c("Bh", "Basi")))
  .asc_result(paste0("gg_podosol_", kind), passed, which(bpod), list(kinds = kinds),
              if (is.na(passed)) c("designation", "boundary_topography") else character(0),
              "Podosols, great groups; glossary 'Podosol diagnostic horizons'")
}

# ---- Organosols ------------------------------------------------------------------

#' Organosol great groups
#'
#' \code{kind}: "folic" (more or less freely drained, never saturated more
#' than several days, organic materials as in Definition (ii) of the order);
#' "sulfuric" (sulfuric material at least 15 cm thick starting within the
#' upper 1.5 m); "hypersulfidic" and "hyposulfidic" (that material within
#' the upper 1.5 m, where the incubation test shows it: replacements of
#' Sulfidic used only with that evidence); "monosulfidic" ones (monosulfidic
#' material with sulfuric, hypersulfidic or hyposulfidic: soilKey records no
#' monosulfidic test, so they are not used); "sulfidic" (sulfidic material,
#' 0.01 % sulfidic S, within the upper 1.5 m: a test not done is undecided);
#' "calcareous", "basic", "acidic" (the major part of the organic materials
#' calcareous; not calcareous and not strongly acid; strongly acid). The
#' upper 1.5 m includes buried horizons; a part of it below the description
#' (not on rock or a hard layer) may hold sulfuric or sulfidic material.
#' @noRd
organosol_gg_asc <- function(pedon, kind = c("folic", "monosulfidic_sulfuric", "sulfuric", "monohypersulfidic",
                                             "hypersulfidic", "monohyposulfidic", "hyposulfidic", "sulfidic",
                                             "calcareous", "basic", "acidic")) {
  kind <- match.arg(kind)
  x <- .asc_horizons(pedon); h <- x$h
  top0 <- .asc_profile_top(x) %|NA|% 0
  # rock is not soil material: R horizons are not tested
  in150 <- !is.na(h$top_cm) & h$top_cm < top0 + 150 & !x$rock
  # the upper 1.5 m not all described (the description stops above it, a
  # depth is missing, or the record has a gap); a description that ends on
  # rock or a hard layer is complete below that layer, not above it
  zb <- top0 + 150
  if (.asc_ends_hard(x)) {
    last <- which(!is.na(h$top_cm)); zb <- min(zb, max(h$top_cm[last]))
  }
  win <- which(!is.na(h$top_cm) & h$top_cm < zb & (is.na(h$bottom_cm) | h$bottom_cm > top0))
  cover <- 0; end <- top0
  for (i in win[order(h$top_cm[win])]) {
    if (is.na(h$bottom_cm[i])) { cover <- NA; break }
    a <- max(h$top_cm[i], end); b <- min(h$bottom_cm[i], zb)
    if (b > a) { cover <- cover + b - a; end <- b }
  }
  unseen <- is.na(cover) || cover < zb - top0 - 0.5
  open_below <- function(v) if (identical(v, FALSE) && unseen) NA else v
  om <- .asc_organic_material(x, pedon)
  s_pct <- suppressWarnings(as.numeric(h$sulfidic_s_pct %||% rep(NA_real_, x$n)))
  inc <- suppressWarnings(as.numeric(h$incubation_ph %||% rep(NA_real_, x$n)))
  ph <- suppressWarnings(as.numeric(h$ph_h2o %||% rep(NA_real_, x$n)))
  hyper <- .asc_hypersulfidic_rows(x)
  # hyposulfidic: sulfidic (S 0.01 % or more), a field pH of 4 or more, and
  # no substantial drop (0.5 unit or more) to below 4 on incubation
  hypo <- ifelse(is.na(inc), FALSE,
          ifelse(!is.na(s_pct) & s_pct < 0.01, FALSE,
          ifelse(!is.na(ph) & ph < 4, FALSE,
          ifelse(!is.na(ph) & inc < 4 & round(ph - inc, 6) >= 0.5, FALSE,
          ifelse(is.na(ph) | is.na(s_pct), NA, TRUE)))))
  passed <- switch(kind,
    folic = {
      d <- pedon$site$drainage_class %||% NA
      wet_end <- .drainage_class(d, "wetter"); dry_end <- .drainage_class(d, "drier")
      free <- if (length(d) == 1L && !is.na(d) && grepl("^\\s*rapid", tolower(as.character(d)))) TRUE
              else if (!is.na(wet_end) && wet_end <= 4) TRUE
              else if (!is.na(dry_end) && dry_end >= 6) FALSE else NA
      .and3(free, .asc_organic_branch2(x, om))
    },
    monosulfidic_sulfuric = , monohypersulfidic = , monohyposulfidic = FALSE,
    sulfuric = {
      # a run starting within the upper 1.5 m may go on below it
      x2 <- x; x2$top_ms <- h$top_cm - top0
      v <- .asc_sulfuric_rows(x); v[x$rock] <- FALSE
      open_below(.cssc_run_test(x2, v, 15, max_top_cm = 150 - 1e-6)$passed)
    },
    # the replacements need the incubation: untested rows are not evidence
    hypersulfidic = .asc_any_rows(ifelse(is.na(inc), FALSE, hyper)[in150]),
    hyposulfidic = .asc_any_rows(hypo[in150]),
    sulfidic = {
      sul <- ifelse(!is.na(s_pct), s_pct >= 0.01, ifelse(hyper %in% TRUE, TRUE, .asc_untested_material()))
      open_below(.asc_any_rows(sul[in150]))
    },
    calcareous = , basic = , acidic = {
      live <- !x$des$buried
      rows <- which(om %in% TRUE & live); maybe <- which(is.na(om) & live)
      calc <- .asc_calcareous(x); acid <- .asc_strongly_acid(h)
      ok <- switch(kind, calcareous = calc, basic = .and3v(!calc, !acid), acidic = acid)
      if (!length(rows) && !length(maybe)) NA else .asc_rows_major_open(x, rows, maybe, ok)
    })
  .asc_result(paste0("gg_organosol_", kind), passed, integer(0), list(kind = kind),
              if (is.na(passed)) c("site$drainage_class", "sulfidic_s_pct", "incubation_ph", "ph_h2o",
                                   "effervescence_class") else character(0),
              "Organosols, great groups; glossary 'Sulfidic materials', 'Strongly acid'")
}
