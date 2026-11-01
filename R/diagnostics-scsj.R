# =============================================================================
# v0.9.321 -- diagnostics of the Soil Classification System of Japan (SCSJ
# 2017, Japanese Society of Pedology, Fifth Committee for Soil Classification
# and Nomenclature): the profile as the system reads it (the soil surface,
# the subsurface control section), the materials of chapter IV.3, the
# features of IV.2, the diagnostic horizons of IV.1 and the colour words of
# IV.4. Three-valued: TRUE, FALSE or NA (undecided).
#
# Each diagnostic reads the measured property when recorded and the
# surveyor's notation otherwise (an H horizon is peat material, a G horizon a
# gley layer, a Bs or Bh a podzolic accumulation horizon, a Bt clay
# illuviation, a B with no other mark a weathered horizon). Depths are from
# the soil surface of IV.4. Materials that come from the description (fill,
# volcanic ejecta, alluvial deposits) are read when recorded: a profile that
# records none of them has none. Choices where the text is silent are in
# inst/rules/scsj2017/readings.yaml. Names carry the _scsj suffix.
# =============================================================================

.SCSJ_REF <- "JSP (2017). Soil Classification System of Japan."

.scsj_result <- function(name, passed, layers = integer(0), evidence = list(),
                         missing = character(0), where = "", notes = NA_character_) {
  DiagnosticResult$new(name = name, passed = passed, layers = as.integer(layers),
                       evidence = evidence, missing = unique(missing),
                       reference = sprintf("%s %s", .SCSJ_REF, where), notes = notes)
}

# Three-valued helpers on vectors.
.scsj_or <- function(a, b) ifelse(a %in% TRUE | b %in% TRUE, TRUE, ifelse(a %in% FALSE & b %in% FALSE, FALSE, NA))
.scsj_and <- function(a, b) ifelse(a %in% FALSE | b %in% FALSE, FALSE, ifelse(a %in% TRUE & b %in% TRUE, TRUE, NA))
.scsj_not <- function(a) ifelse(is.na(a), NA, !a)
.scsj_col <- function(h, name, type = NA_real_) {
  v <- h[[name]]
  if (is.null(v)) rep(type, nrow(h)) else v
}
.scsj_txt <- function(h, name) {
  v <- tolower(trimws(as.character(.scsj_col(h, name, NA_character_))))
  ifelse(is.na(v) | !nzchar(v), NA_character_, v)
}

# ---- colours (IV.4, soil colour) ------------------------------------------------

# Hue as a number on the Munsell circle, larger is yellower (R 0-10, YR
# 10-20, Y 20-30, GY 30-40, G 40-50, BG 50-60, B 60-70, PB 70-80); RP and P
# below R (redder). NA for a neutral or unreadable hue.
.scsj_hue_num <- function(hue) {
  hn <- .nzsc_hue_num(hue)
  fam <- sub("^[0-9.]+", "", toupper(gsub("\\s+", "", as.character(hue))))
  ifelse(!is.na(hn) & fam %in% "P", hn - 100, hn)
}

.scsj_neutral <- function(hue) grepl("^N", toupper(trimws(as.character(hue))))

# The colour words of IV.4 for each row, from the moist matrix colour: red,
# yellow, brown, grey, blue-grey, black. A list of logical vectors, NA where
# the colour is not recorded.
.scsj_colour_words <- function(h) {
  hue <- as.character(.scsj_col(h, "munsell_hue_moist", NA_character_))
  v <- .scsj_col(h, "munsell_value_moist"); c <- .scsj_col(h, "munsell_chroma_moist")
  hn <- .scsj_hue_num(hue); neu <- .scsj_neutral(hue)
  known <- !is.na(v) & !is.na(c) & (!is.na(hn) | neu)
  # 10Y or bluer: 10Y, GY, G, BG, B, PB
  bluer10y <- !is.na(hn) & hn >= 30 & hn < 80
  redder5 <- !is.na(hn) & hn <= 15                  # 5YR or redder
  yellower5 <- !is.na(hn) & hn > 15 & !bluer10y     # yellower than 5YR
  yel_eq5 <- !is.na(hn) & hn >= 15 & !bluer10y      # 5YR or yellower
  red <- redder5 & v > 3 & c >= 3 & !(v == 4 & c %in% c(3, 4))
  yellow <- yellower5 & v >= 3 & c >= 6 & !(v %in% c(3, 4) & c == 6)
  brown <- yel_eq5 & v >= 3 & ((c >= 3 & c < 6) | (v %in% c(3, 4) & c == 6))
  grey <- ((!is.na(hn) & !bluer10y & v >= 3 & c < 3) | (neu & v >= 3))
  # a partial record that already rules a word out (value or chroma alone)
  no_red <- (!is.na(v) & v <= 3) | (!is.na(c) & c < 3)
  no_yellow <- (!is.na(v) & v < 3) | (!is.na(c) & c < 6)
  no_brown <- (!is.na(v) & v < 3) | (!is.na(c) & (c < 3 | c > 6))
  no_grey <- (!is.na(v) & v < 3) | (!is.na(c) & c >= 3)
  na_out <- function(x, no) ifelse(known, x, ifelse(no, FALSE, NA))
  list(red = na_out(red, no_red), yellow = na_out(yellow, no_yellow), brown = na_out(brown, no_brown),
       grey = na_out(grey, no_grey), bluegrey = ifelse(!is.na(hn) | neu, bluer10y, NA),
       black = ifelse(is.na(v), NA, v < 3), hue_num = hn, value = v, chroma = c)
}

# ---- texture ------------------------------------------------------------------

# Sandy loam or finer (the weathered horizon, IV.1): not sand or loamy sand.
# From a field texture word, else lab clay and silt (silt + 2 clay >= 30, the
# USDA sand/loamy sand line); NA when neither is recorded.
.scsj_sl_or_finer <- function(h) {
  tx <- .nzsc_texture(h)
  ifelse(!is.na(tx$class), !(tx$class %in% c("sand", "loamy sand", "peat")),
         ifelse(is.na(tx$group), NA, tx$group != "sandy"))
}

# ---- materials described by the surveyor (IV.3) -------------------------------

# volcanic ejecta: pyroclastic materials (ash, lapilli, pumice, scoria,
# pyroclastic-flow deposits), not volcanic rock, nor ash redeposited by wind
# as dunes
.SCSJ_EJECTA_RE <- paste0("tephr|volcanic ash|ash ?fall|pumice|scoria|lapilli|pyroclast|cinders?\\b|",
                          "volcanic ejecta|volcanic sand|",
                          "\u706b\u5c71\u7070|\u706b\u5c71\u792b|\u8efd\u77f3|\u30b9\u30b3\u30ea\u30a2|",
                          "\u30c6\u30d5\u30e9|\u706b\u7815|\u706b\u5c71\u653e\u51fa\u7269")
.SCSJ_NOT_EJECTA_RE <- paste0("non-?volcanic|\\brock\\b|lava|andesit|basalt|rhyolit|dacit|tuff|",
                              "\u5ca9|\u6eb6\u5ca9|\u7802\u4e18|dune")
# alluvial deposits: Holocene fluvial, marine or lacustrine unconsolidated
# deposits; not rock, terrace, Pleistocene or older, glacial
.SCSJ_ALLUVIAL_RE <- paste0("alluvi|fluvial|fluviatile|fluvic|river|marine|lacustrine|estuarine|delta|",
                            "lagoon|backswamp|floodplain|holocene|",
                            "\u6c96\u7a4d|\u6cb3\u6210|\u6d77\u6210|\u6e56\u6cbc\u6210|\u6e56\u6210|\u5b8c\u65b0\u4e16")
.SCSJ_NOT_ALLUVIAL_RE <- paste0("pleistocene|terrace|tertiary|miocene|pliocene|glacio|glacial|\\brock\\b|",
                                "mudstone|sandstone|siltstone|shale|conglomerate|",
                                "\u53f0\u5730|\u6d2a\u7a4d|\u6bb5\u4e18|\u66f4\u65b0\u4e16|\u7b2c\u4e09\u7d00|\u5ca9")
.SCSJ_FILL_RE <- paste0("\\bfill\\b|backfill|made ground|dredg|soil dressing|spoil|landfill|",
                        "\u76db\u571f|\u5ba2\u571f|\u6d5a\u6e2b|\u57cb\u3081\u7acb\u3066|\u9020\u6210")

# Recorded origin words of each row: fill, volcanic ejecta, alluvial
# deposits. Fill is read from layer_origin (or a ^ of the notation); ejecta
# also from rock_origin (pumice or scoria fragments); alluvial deposits from
# layer_origin only (rock_origin names the rock the material came from). A
# row without its own origin takes the site parent material (except fill). A
# profile that records no origin at all has none of them (recorded when
# present). A fill row keeps the material it records ("fill (river
# alluvium)"): a fill of the same material is part of an ordinary profile
# (chapter VI, the fill phase), and the 35 cm rules of the key name a fill of
# other material (0.9.322).
.scsj_origins <- function(h, site, des) {
  lo <- .scsj_txt(h, "layer_origin"); ro <- .scsj_txt(h, "rock_origin")
  pm <- tolower(paste(as.character(unlist(site$parent_material %||% character(0))), collapse = " "))
  pm <- if (nzchar(trimws(pm)) && !identical(trimws(pm), "na")) pm else NA_character_
  fill <- (!is.na(lo) & grepl(.SCSJ_FILL_RE, lo, perl = TRUE)) | des$fill
  ej_txt <- ifelse(!is.na(lo) | !is.na(ro), paste(ifelse(is.na(lo), "", lo), ifelse(is.na(ro), "", ro)), pm)
  ejecta <- !is.na(ej_txt) & grepl(.SCSJ_EJECTA_RE, ej_txt, perl = TRUE) &
            !grepl(.SCSJ_NOT_EJECTA_RE, ej_txt, perl = TRUE)
  al_txt <- ifelse(!is.na(lo), lo, pm)
  alluvial <- !is.na(al_txt) & grepl(.SCSJ_ALLUVIAL_RE, al_txt, perl = TRUE) &
              !grepl(.SCSJ_NOT_ALLUVIAL_RE, al_txt, perl = TRUE)
  list(fill = fill, ejecta = ejecta, alluvial = alluvial)
}

# ---- the site -------------------------------------------------------------------

.scsj_drainage <- function(site) {
  for (k in c("drainage_class", "drainage")) {
    v <- tolower(trimws(as.character(site[[k]] %||% NA)[1L]))
    if (!is.na(v) && nzchar(v) && v != "na") return(v)
  }
  NA_character_
}

# Is the site saturated with water for most of the year, or artificially
# drained? (IV.4, soil surface.) TRUE for an artificial drainage record or a
# very poorly drained site; FALSE for any other recorded drainage class; NA
# when neither is recorded.
.scsj_site_wet <- function(site) {
  ad <- site$artificially_drained %||% NA
  if (isTRUE(as.logical(ad[1L]))) return(TRUE)
  dr <- .scsj_drainage(site)
  if (is.na(dr)) return(NA)
  grepl("^very poor", dr)
}

# Does organic matter accumulate under water at the site (peat rather than a
# forest floor)? TRUE poorly or very poorly drained (or artificially
# drained), FALSE any other recorded class, NA none recorded.
.scsj_site_peaty <- function(site) {
  ad <- site$artificially_drained %||% NA
  if (isTRUE(as.logical(ad[1L]))) return(TRUE)
  dr <- .scsj_drainage(site)
  if (is.na(dr)) return(NA)
  grepl("^(very )?poor", dr)
}

# Paddy land (irrigated rice): site$land_use (rice, paddy, the Japanese
# word) or site$flood_irrigated; upland rice is not paddy. TRUE / FALSE
# (another land use recorded) / NA (nothing recorded).
.scsj_paddy_site <- function(site) {
  fi <- site$flood_irrigated %||% NA
  if (isTRUE(as.logical(fi[1L]))) return(TRUE)
  lu <- tolower(paste(as.character(unlist(site$land_use %||% character(0))), collapse = " "))
  if (!nzchar(trimws(lu)) || identical(trimws(lu), "na")) return(if (isFALSE(as.logical(fi[1L]))) FALSE else NA)
  lu2 <- gsub("upland rice|\u9678\u7a32", "", lu)
  grepl("rice|paddy|\u6c34\u7530|\u7a32", lu2)
}

# ---- the profile as the SCSJ reads it -----------------------------------------

# Shared reading of a pedon: designations, row order, organic and mineral
# rows, the soil surface (IV.4), depths from it, the materials and the
# features of each row. Recomputed by each gate.
.scsj_profile <- function(pedon) {
  h <- pedon$horizons
  n <- nrow(h)
  site <- pedon$site %||% list()
  top <- h$top_cm; bot <- h$bottom_cm
  des <- .scsj_designations(.scsj_col(h, "designation", NA_character_), site, top)
  role <- des$role
  oc <- .scsj_col(h, "oc_pct")
  thick <- ifelse(is.na(top) | is.na(bot), NA_real_, pmax(0, bot - top))
  ord <- order(top, na.last = NA)
  orig <- .scsj_origins(h, site, des)

  # kuroboku and immature kuroboku features (IV.2), needed for peat material
  kb <- .scsj_kuroboku_rows(h, des, oc)
  # immature kuroboku: an ejecta layer without the kuroboku feature (the
  # field note of IV.2 splits ejecta layers between the two)
  ikb_raw <- .scsj_immature_kuroboku_rows(h, des, oc, orig)
  ikb <- .scsj_and(ikb_raw, .scsj_not(kb))

  # peat material (IV.3): 12 % organic carbon or more, without the kuroboku
  # feature unless a sixth or more of the volume is fibre after rubbing; an H
  # horizon when carbon is not measured; never an O horizon (forest floor).
  # An undesignated layer of 20 % carbon or more is peat on a poorly or very
  # poorly drained site and organic material (forest floor) on any other
  # (Hugo, 2026-10-31); undecided when the drainage is not recorded.
  # Organic material (IV.3): an O horizon (unless its carbon is measured
  # under 20 %); otherwise 20 % carbon or more, without the kuroboku feature,
  # when not peat.
  fib <- .scsj_col(h, "fiber_content_rubbed_pct")
  fib6 <- !is.na(fib) & fib >= 100 / 6
  peaty <- .scsj_site_peaty(site)
  not_kb <- ifelse(fib6, TRUE, .scsj_not(kb))
  peat <- rep(NA, n); orgmat <- rep(NA, n)
  for (i in seq_len(n)) {
    r <- role[i]
    if (r %in% "O") {
      peat[i] <- FALSE
      orgmat[i] <- if (is.na(oc[i])) TRUE else oc[i] >= 20
      next
    }
    if (is.na(oc[i])) {
      peat[i] <- if (r %in% "H") TRUE else if (des$known[i]) FALSE else NA
      orgmat[i] <- if (des$known[i]) FALSE else NA
      next
    }
    if (oc[i] < 12) { peat[i] <- FALSE; orgmat[i] <- FALSE; next }
    if (is.na(r) && oc[i] >= 20) {
      # undesignated: peat or forest floor by the site
      p <- .scsj_and(peaty, not_kb[i])
      peat[i] <- p
      orgmat[i] <- .scsj_and(.scsj_not(peaty), .scsj_not(kb[i]))
      next
    }
    # a designated horizon (or 12 to 20 % carbon): peat unless kuroboku
    peat[i] <- not_kb[i]
    orgmat[i] <- FALSE
  }
  organic <- .scsj_or(peat, orgmat)

  # the soil surface (IV.4): the ground surface when the first layer is peat
  # material, or the site is artificially drained or saturated most of the
  # year; otherwise below a forest floor (an O horizon, or a first layer of
  # organic material): the top of the first layer with the kuroboku feature
  # or under 20 % carbon. Undecided when the first layers cannot be read.
  wet <- .scsj_site_wet(site)
  first <- if (length(ord)) ord[1L] else NA_integer_
  ground <- if (is.na(first)) NA_real_ else top[first]
  surface <- ground
  surface_note <- "ground surface"
  # a forest floor: organic material (an O horizon, unless its carbon is
  # measured under 20 %)
  floor_row <- function(i) orgmat[i]
  if (!is.na(first) && !(peat[first] %in% TRUE) && !(wet %in% TRUE)) {
    f1 <- floor_row(first)
    if (is.na(f1)) {
      surface <- NA_real_; surface_note <- "undecided (the first layer may be a forest floor)"
    } else if (isTRUE(f1)) {
      surface <- NA_real_
      prev_bot <- NA_real_
      for (i in ord) {
        fi <- floor_row(i)
        if (isTRUE(fi) && !(kb[i] %in% TRUE)) { prev_bot <- bot[i]; next }
        if (is.na(fi)) break
        # a gap under the floor: the soil surface may lie in it
        if (!is.na(prev_bot) && top[i] - prev_bot > 0.5) break
        surface <- top[i]; break
      }
      surface_note <- if (is.na(surface)) "undecided (below the forest floor)" else
        if (is.na(wet)) "below the forest floor (wetness not recorded)" else "below the forest floor"
    }
  }
  dtop <- top - surface; dbot <- bot - surface

  out <- list(h = h, n = n, site = site, des = des, role = role, top = top, bot = bot,
              thick = thick, ord = ord, oc = oc, orig = orig, kb = kb, ikb = ikb, ikb_raw = ikb_raw,
              peat = peat, orgmat = orgmat, organic = organic,
              mineral = .scsj_not(organic), surface = surface, ground = ground,
              surface_note = surface_note, dtop = dtop, dbot = dbot, wet = wet)
  out$colour <- .scsj_colour_words(h)
  out$paddy <- .scsj_paddy_site(site)
  rk <- .scsj_rock_rows(out)
  out$rock <- rk$rock; out$rock_maybe <- rk$maybe
  out$rock_top <- .scsj_rock_top(out)
  # a part of a profile keyed on its own (the fill of Man-made soils, or the
  # soil under it) is complete: nothing lies below its last horizon
  out$complete <- isTRUE(site$scsj_part) && isTRUE(site$scsj_part_complete %||% TRUE)
  out
}

# Bedrock rows (IV.4: unweathered and semi-weathered bedrock): an R horizon,
# or a Cr outside the FAO notation. `maybe`: a row that may be rock (an
# unreadable designation, or no designation and no fine-earth data in a
# designated profile).
.scsj_rock_rows <- function(x) {
  h <- x$h
  rock <- x$role %in% "R" | x$des$weathered_rock
  fine <- !is.na(.scsj_col(h, "clay_pct")) | !is.na(x$oc) | !is.na(.scsj_txt(h, "field_texture"))
  maybe <- !rock & (x$role %in% "other" | (is.na(x$role) & any(x$des$known) & !fine))
  list(rock = rock, maybe = maybe)
}

# Depth (from the soil surface) of the top of the bedrock. Inf when the
# profile has none (some depth recorded); NA when nothing is known.
.scsj_rock_top <- function(x) {
  if (any(x$rock & !is.na(x$dtop))) return(min(x$dtop[x$rock & !is.na(x$dtop)]))
  if (all(is.na(x$dbot)) && all(is.na(x$dtop))) return(NA_real_)
  Inf
}

# The deepest closed depth (from the soil surface) of the description; Inf
# when the deepest row is open (no bottom).
.scsj_depth <- function(x) {
  ok <- !is.na(x$dtop)
  if (!any(ok)) return(NA_real_)
  deepest <- which(ok)[which.max(x$dtop[ok])]
  if (is.na(x$dbot[deepest])) return(Inf)
  suppressWarnings(max(x$dbot, na.rm = TRUE))
}

# "No bedrock within d cm": FALSE when rock starts within d, NA when a row
# that may be rock starts within d or the description (its closed bottoms;
# a horizon without a bottom may end at once) stops short of d, TRUE
# otherwise.
.scsj_no_rock_within <- function(x, d) {
  if (is.na(x$surface)) return(NA)
  rt <- x$rock_top
  if (is.na(rt)) return(NA)
  if (is.finite(rt) && rt <= d) return(FALSE)
  if (any(x$rock_maybe & !is.na(x$dtop) & x$dtop <= d)) return(NA)
  if (is.finite(rt)) return(TRUE)
  dep <- suppressWarnings(max(x$dbot, na.rm = TRUE))
  if (isTRUE(x$complete)) return(TRUE)
  if (!is.finite(dep)) return(NA)
  if (dep > d) TRUE else NA
}

# ---- thickness tests ----------------------------------------------------------

# Cumulative thickness of rows (logical vector, three-valued) within the
# window [from, to] cm of the soil surface: the certain thickness (TRUE
# rows) and the possible one (TRUE or NA rows). A row with no bottom counts
# as possible from its top down; a part of the window no row covers (a gap,
# or below the description) is possible, unless it lies below the bedrock.
.scsj_cum <- function(x, rows, from = 0, to = Inf) {
  if (is.na(x$surface)) return(list(sure = 0, poss = Inf))
  ok <- which(!is.na(x$dtop))
  if (!length(ok)) return(list(sure = 0, poss = if (is.finite(to)) to - from else Inf))
  top <- x$dtop[ok]; open <- is.na(x$dbot[ok]); bot <- ifelse(open, Inf, x$dbot[ok]); o <- rows[ok]
  last <- suppressWarnings(max(bot))
  rock <- x$rock_top
  hi <- if (is.finite(to)) to else if (is.finite(last)) max(last, from) else max(top, from) + 1
  if (hi <= from) return(list(sure = 0, poss = 0))
  cuts <- sort(unique(c(from, hi, top[top > from & top < hi], bot[is.finite(bot) & bot > from & bot < hi])))
  sure <- 0; poss <- 0
  for (k in seq_len(length(cuts) - 1L)) {
    a <- cuts[k]; b <- cuts[k + 1L]; mid <- (a + b) / 2
    cov <- top <= mid & bot > mid
    closed <- cov & !open
    if (!any(cov)) {
      if (!(is.finite(rock %||% NA) && mid > rock) && !(mid > last && (!is.finite(to) || isTRUE(x$complete))))
        poss <- poss + (b - a)
      next
    }
    if (any(closed)) {
      v <- o[closed]
      if (any(v %in% FALSE)) next
      if (all(v %in% TRUE)) sure <- sure + (b - a)
      poss <- poss + (b - a)
    } else {
      # only rows without a bottom cover it: they may end above it
      poss <- poss + (b - a)
    }
  }
  if (!is.finite(to) && any(open & !(o %in% FALSE))) poss <- Inf
  list(sure = sure, poss = poss)
}

.scsj_at_least <- function(cum, thr) {
  if (cum$sure >= thr) TRUE else if (cum$poss < thr) FALSE else NA
}

# Runs of adjacent rows (in depth order) where `rows` may hold: for each run
# the rows, its top and bottom (from the soil surface; NA bottom when its
# last row has none), the certain thickness (closed TRUE rows), the possible
# one, and whether it is open. A gap between rows breaks a run, unless
# `bridge` (then the gap is counted as possible thickness); with
# `split_lith`, so does a change of lithologic number.
.scsj_runs <- function(x, rows, bridge = FALSE, split_lith = FALSE) {
  if (is.na(x$surface)) return(list())
  ord <- x$ord[!is.na(x$dtop[x$ord])]
  runs <- list(); cur <- integer(0); gap <- 0
  flush <- function() {
    if (length(cur)) {
      last <- cur[length(cur)]
      open <- is.na(x$dbot[last])
      # the deepest certain depth of the run (an open last row reaches at
      # least its top), and whether the description stops at its end (not
      # on rock or a complete part): it may then go on
      deep <- if (open) x$dtop[last] else x$dbot[last]
      e <- .scsj_ends(x)
      at_end <- open || (!is.na(e$depth) && abs(x$dbot[last] - e$depth) <= 0.5 && !e$on_rock)
      runs[[length(runs) + 1L]] <<- list(
        rows = cur, top = x$dtop[cur[1L]], bottom = x$dbot[last], open = open,
        deep = deep, at_end = at_end,
        sure = sum(x$thick[cur][rows[cur] %in% TRUE], na.rm = TRUE),
        poss = if (at_end) Inf else sum(x$thick[cur], na.rm = TRUE) + gap,
        all_true = all(rows[cur] %in% TRUE))
    }
    cur <<- integer(0); gap <<- 0
  }
  for (i in ord) {
    if (rows[i] %in% FALSE) { flush(); next }
    if (length(cur)) {
      pb <- x$dbot[cur[length(cur)]]
      if (is.na(pb)) { flush() }
      else if (split_lith && !identical(x$des$lith[i], x$des$lith[cur[length(cur)]])) { flush() }
      else if (x$dtop[i] - pb > 0.5) { if (bridge) gap <- gap + (x$dtop[i] - pb) else flush() }
    }
    cur <- c(cur, i)
  }
  flush()
  runs
}

# The deepest closed bottom of the description and whether it ends on rock.
.scsj_ends <- function(x) {
  dep <- .scsj_depth(x)
  list(depth = dep, on_rock = is.finite(x$rock_top %||% NA) || isTRUE(x$complete))
}

# "A horizon of rows, at least `thick` cm thick, whose top is within
# `within` cm of the soil surface": three-valued. TRUE when TRUE rows alone
# make an adjacent run of the thickness starting within the depth; NA when
# rows that may hold could (gaps and open bottoms counted as possible), or
# when the description stops above `within` (not on rock).
.scsj_horizon_within <- function(x, rows, thick, within = Inf) {
  if (is.na(x$surface)) return(list(passed = NA, rows = integer(0)))
  for (r in .scsj_runs(x, ifelse(rows %in% TRUE, TRUE, FALSE))) {
    if (r$top <= within && r$sure >= thick) return(list(passed = TRUE, rows = r$rows))
  }
  for (r in .scsj_runs(x, rows, bridge = TRUE)) {
    if (r$top <= within && r$poss >= thick) return(list(passed = NA, rows = r$rows))
  }
  e <- .scsj_ends(x)
  if (is.finite(within) && !is.na(e$depth) && e$depth < within && !e$on_rock)
    return(list(passed = NA, rows = integer(0)))
  list(passed = FALSE, rows = integer(0))
}

# Row flags of the horizons that need a thickness: TRUE for rows of an
# adjacent run of TRUE rows of `thick` cm or more; FALSE for rows that are
# FALSE or whose possible run is thinner; NA otherwise.
.scsj_horizon_rows <- function(x, rows, thick) {
  out <- ifelse(rows %in% FALSE, FALSE, NA)
  if (is.na(x$surface)) return(out)
  for (r in .scsj_runs(x, rows, bridge = TRUE)) if (r$poss < thick) out[r$rows] <- FALSE
  for (r in .scsj_runs(x, ifelse(rows %in% TRUE, TRUE, FALSE))) if (r$sure >= thick) out[r$rows] <- TRUE
  out
}

# ---- features (IV.2) ------------------------------------------------------------

# The kuroboku feature of each row: a phosphate absorption coefficient of
# 1500 mg P2O5/100 g or more (15.00 mg/g in the text), which a P retention of
# 85 % or more, or Alo + 1/2 Feo of 2.0 % or more, may replace; and less than
# a sixth of the volume fibre after rubbing. The coefficient decides when
# measured (Hugo, 2026-10-31: the substitutes stand in when it is missing);
# otherwise a measured substitute that passes, or all measured substitutes
# failing; otherwise the field active-aluminium test (vivid red within a
# minute, 2+ or more: yes; none or +-: no). Fibre not recorded is read as
# under a sixth except in H and O horizons.
.scsj_kuroboku_rows <- function(h, des, oc) {
  pac <- .scsj_col(h, "phosphate_absorption_coef")
  pr <- .scsj_col(h, "phosphate_retention_pct")
  alo <- .scsj_col(h, "al_ox_pct"); feo <- .scsj_col(h, "fe_ox_pct")
  af <- ifelse(is.na(alo) | is.na(feo), NA_real_, alo + feo / 2)
  sub_pass <- (!is.na(pr) & pr >= 85) | (!is.na(af) & af >= 2.0)
  sub_known <- !is.na(pr) | !is.na(af)
  field <- .scsj_active_al(.scsj_col(h, "active_al_test", NA_character_))
  p_crit <- ifelse(!is.na(pac), pac >= 1500,
            ifelse(sub_pass, TRUE, ifelse(sub_known, FALSE,
            ifelse(field >= 2, TRUE, ifelse(field <= 0.5, FALSE, NA)))))
  fib <- .scsj_col(h, "fiber_content_rubbed_pct")
  fib_ok <- ifelse(!is.na(fib), fib < 100 / 6,
            ifelse(des$role %in% c("H", "O"), NA, TRUE))
  .scsj_and(p_crit, fib_ok)
}

# The Japanese field active-aluminium test (JSP 1997: NaF with
# phenolphthalein, graded by how fast and how strongly the paper turns red):
# -, +-, +, 2+ (++), 3+ (+++) as 0, 0.5, 1, 2, 3; NA otherwise.
.scsj_active_al <- function(v) {
  v <- gsub("\\s+", "", tolower(as.character(v)))
  v <- gsub("\u00b1", "+-", v, fixed = TRUE)
  v <- gsub("\uff0b", "+", v, fixed = TRUE)
  v <- gsub("[\u2212\uff0d]", "-", v)
  tab <- c("-" = 0, "none" = 0, "+-" = 0.5, "-+" = 0.5, "weak" = 0.5, "+" = 1,
           "++" = 2, "2+" = 2, "strong" = 2, "+++" = 3, "3+" = 3, "verystrong" = 3)
  out <- unname(tab[v])
  out[is.na(v)] <- NA
  out
}

# The immature kuroboku feature of each row: less than half the area
# unweathered gravel-size ejecta; 60 % or more (by weight) volcanic ejecta;
# and a phosphate absorption coefficient of 300 to under 1500 mg/100 g (P
# retention 25 to under 85 %, or Alo + 1/2 Feo 0.4 to under 2.0 %), or 3 %
# organic carbon or more. Ejecta are read from the recorded origin (none
# recorded: no ejecta); coarse fragments not recorded read as under half.
.scsj_immature_kuroboku_rows <- function(h, des, oc, orig) {
  pac <- .scsj_col(h, "phosphate_absorption_coef")
  pr <- .scsj_col(h, "phosphate_retention_pct")
  alo <- .scsj_col(h, "al_ox_pct"); feo <- .scsj_col(h, "fe_ox_pct")
  af <- ifelse(is.na(alo) | is.na(feo), NA_real_, alo + feo / 2)
  mid <- ifelse(!is.na(pac), pac >= 300 & pac < 1500,
         ifelse((!is.na(pr) & pr >= 25 & pr < 85) | (!is.na(af) & af >= 0.4 & af < 2.0), TRUE,
         ifelse(!is.na(pr) | !is.na(af), FALSE, NA)))
  c3 <- ifelse(is.na(oc), NA, oc >= 3)
  crit3 <- .scsj_or(mid, c3)
  cf <- .scsj_col(h, "coarse_fragments_pct")
  crit1 <- ifelse(is.na(cf), TRUE, cf < 50)
  .scsj_and(.scsj_and(crit1, orig$ejecta), crit3)
}

# Physical immaturity (IV.2): the material squeezes between the fingers
# (slightly, moderately or very fluid), or an n value of 1 or more.
.scsj_immature_rows <- function(h) {
  fl <- .nzsc_class_index(.scsj_col(h, "fluidity_class", NA_character_), "fluidity")
  nv <- .scsj_col(h, "n_value")
  .scsj_or(ifelse(is.na(fl), NA, fl >= 1), ifelse(is.na(nv), NA, nv >= 1))
}

# The dipyridyl reaction (childs_test): TRUE an immediate vivid reaction,
# FALSE none or a weak one (+-), NA otherwise.
.scsj_dipyridyl <- function(h) {
  ct <- .scsj_txt(h, "childs_test")
  ct <- gsub("\u00b1", "+-", ct, fixed = TRUE)
  pos <- c("positive", "pos", "+", "++", "2+", "+++", "3+", "yes", "true", "strong", "immediate")
  neg <- c("negative", "neg", "-", "+-", "-+", "no", "none", "false", "weak")
  ifelse(is.na(ct), NA, ifelse(ct %in% pos, TRUE, ifelse(ct %in% neg, FALSE, NA)))
}

# The gley feature (IV.2) of each row: an immediate vivid dipyridyl reaction,
# or a moist hue of 10Y or bluer in physically immature material. Measured
# tests decide; a G horizon of the notation has it when they do not (the SCSJ
# defines G by the dipyridyl reaction, or a soft blue-grey layer); another
# master of a designated profile has not.
.scsj_gley_feature_rows <- function(x) {
  h <- x$h
  dip <- .scsj_dipyridyl(h)
  blue <- .scsj_and(x$colour$bluegrey, .scsj_immature_rows(h))
  by_test <- .scsj_or(dip, blue)
  ifelse(by_test %in% TRUE, TRUE,
  ifelse(by_test %in% FALSE, FALSE,
  ifelse(x$des$gley_master, TRUE,
  ifelse(x$des$known, FALSE, NA))))
}

# Wetness features (IV.2) of each row: saturated at some time of the year,
# without the gley feature, with
#   * groundwater: iron-oxide coatings on ped faces or pore walls;
#   * surface water: grey ped faces or pore walls and ped interiors redder
#     and of higher chroma.
# Read from the redox segregations (mottles, 2 % or more) and where they are
# (redox_segregations_location: ped faces or voids = groundwater; within
# peds or the matrix = surface water); a g suffix is one of the two, not said
# which. `any` = either feature. No mottles recorded (0 %) and no g: neither;
# a designated horizon without g and without a mottle record: neither.
.scsj_wetness_rows <- function(x) {
  h <- x$h
  rf <- .scsj_col(h, "redoximorphic_features_pct")
  loc <- .scsj_txt(h, "redox_segregations_location")
  gley <- .scsj_gley_feature_rows(x)
  mottled <- ifelse(is.na(rf), NA, rf >= 2)
  gw_loc <- !is.na(loc) & grepl("ped face|void|pore|skeletan|root", loc)
  sw_loc <- !is.na(loc) & grepl("within ped|matrix|interior", loc)
  g <- x$des$g
  any <- ifelse(g | mottled %in% TRUE, TRUE,
         ifelse(mottled %in% FALSE | (x$des$known & !g & is.na(rf)), FALSE, NA))
  any <- .scsj_and(any, .scsj_not(gley))
  gw <- ifelse(any %in% TRUE & gw_loc, TRUE, ifelse(any %in% FALSE | (any %in% TRUE & sw_loc), FALSE, NA))
  sw <- ifelse(any %in% TRUE & sw_loc, TRUE, ifelse(any %in% FALSE | (any %in% TRUE & gw_loc), FALSE, NA))
  list(any = any, groundwater = gw, surface = sw, gley = gley)
}

# The red-yellow feature (IV.2) of each row: under 2.0 % organic carbon and a
# red or yellow moist colour. Carbon not measured: a B, C or E horizon (not a
# Bh or Bs) is read as under 2 %.
.scsj_red_yellow_rows <- function(x) {
  ry <- .scsj_or(x$colour$red, x$colour$yellow)
  low_role <- !is.na(x$role) & grepl("^(B|C|E)", x$role) & !x$des$h & !x$des$s
  lowc <- ifelse(is.na(x$oc), ifelse(low_role, TRUE, NA), x$oc < 2.0)
  out <- .scsj_and(ry, lowc)
  ifelse(x$mineral %in% FALSE, FALSE, out)
}

# ---- horizons (IV.1) ------------------------------------------------------------

# Rows that may be a subsurface horizon (E or B position): an E or B master
# (or a transition starting with E or B) in a designated profile; an
# undesignated mineral row below the first; never the first row.
.scsj_subsurface_pos <- function(x) {
  r <- x$role
  pos <- ifelse(r %in% "other", NA, ifelse(is.na(r), TRUE, grepl("^(E|B)", r)))
  first <- if (length(x$ord)) x$ord[1L] else 0L
  pos[is.na(r) & seq_len(x$n) == first] <- FALSE
  ifelse(x$mineral %in% FALSE, FALSE, ifelse(is.na(x$mineral) & pos %in% TRUE, NA, pos))
}

# The plough layer (Ap) of each row.
.scsj_plough_rows <- function(x) x$des$p & x$role %in% c("A", "AB", "AC")

# The mineral row directly above each row (in depth order), NA when none.
.scsj_row_above <- function(x) {
  out <- rep(NA_integer_, x$n)
  ord <- x$ord
  for (k in seq_along(ord)) {
    if (k == 1L) next
    out[ord[k]] <- ord[k - 1L]
  }
  out
}

# The argic (clay accumulation) horizon rows: a mineral subsurface layer in
# the same material as the horizon directly above (no lithologic
# discontinuity), 8 % clay or more, with clay coatings (or a t of the
# notation) or a clay increase within 30 cm over a mineral horizon above
# (under 10 % clay above: 4 % more; 10 to 50 %: 1.4 times; 50 % or more:
# 20 % more). Row flags; the 7.5 cm is tested by .scsj_argic_horizon_rows().
.scsj_argic_rows <- function(x) {
  h <- x$h
  clay <- .scsj_col(h, "clay_pct")
  cf <- .scsj_txt(h, "clay_films_amount")
  # clay coatings: a t of the notation, or recorded clay films; a designated
  # horizon without t and without a record has none (the surveyor writes t)
  films <- ifelse(x$des$t, TRUE,
           ifelse(!is.na(cf), !grepl("^(none|no|0|absent|nil)", cf),
           ifelse(x$des$known, FALSE, NA)))
  above1 <- .scsj_row_above(x)
  same <- ifelse(is.na(above1), TRUE,
                 ifelse(is.na(x$des$lith) | is.na(x$des$lith[above1]), TRUE, x$des$lith == x$des$lith[above1]))
  inc <- rep(NA, x$n)
  for (i in x$ord) {
    if (is.na(clay[i]) || is.na(x$dtop[i])) next
    k <- match(i, x$ord)
    above <- x$ord[seq_len(k - 1L)]
    above <- above[!is.na(x$dbot[above]) & x$dbot[above] <= x$dtop[i] + 0.5 &
                   x$dbot[above] >= x$dtop[i] - 30 & !(x$mineral[above] %in% FALSE) &
                   (x$des$lith[above] %in% x$des$lith[i] | is.na(x$des$lith[above]) | is.na(x$des$lith[i]))]
    # no layer above in the same material: no increase to measure
    if (!length(above)) { inc[i] <- FALSE; next }
    ok <- vapply(above, function(j) {
      u <- clay[j]
      if (is.na(u) || is.na(x$mineral[j])) return(NA)
      if (u < 10) clay[i] >= u + 4 else if (u < 50) clay[i] >= 1.4 * u else clay[i] >= u + 20
    }, NA)
    inc[i] <- if (any(ok %in% TRUE)) TRUE else if (all(ok %in% FALSE)) FALSE else NA
  }
  c8 <- ifelse(is.na(clay), ifelse(x$des$t, TRUE, NA), clay >= 8)
  ev <- .scsj_or(films, inc)
  # in a designated profile, a horizon without t whose clay increase cannot
  # be measured is not one (properties decide when recorded, the notation
  # otherwise)
  ev <- ifelse(is.na(ev) & x$des$known & !x$des$t, FALSE, ev)
  out <- .scsj_and(.scsj_and(c8, ev), .scsj_subsurface_pos(x))
  ifelse(same, out, FALSE)
}

# The argic horizon rows (7.5 cm or more).
.scsj_argic_horizon_rows <- function(x, ag = .scsj_argic_rows(x)) .scsj_horizon_rows(x, ag, 7.5)

# The weathered (cambic) horizon rows, before criterion 5: a mineral
# subsurface layer, sandy loam or finer, not the plough layer. A B horizon of
# the notation with no mark of another horizon (not t, s, h, G) has the
# weathering of criteria 1 and 2; an undesignated row by a recorded structure
# grade other than structureless. The 15 cm and criterion 5 are tested by
# .scsj_weathered_only().
.scsj_weathered_rows <- function(x) {
  h <- x$h
  r <- x$role
  des <- x$des
  b_plain <- !is.na(r) & grepl("^B", r) & !des$t & !des$s & !des$h & !des$gley_master
  grade <- .scsj_txt(h, "structure_grade")
  developed <- ifelse(is.na(grade), NA, !grepl("structureless|massive|single", grade))
  weath <- ifelse(b_plain, TRUE, ifelse(des$known, FALSE, ifelse(is.na(r), developed, NA)))
  tex <- .scsj_sl_or_finer(h)
  tex <- ifelse(is.na(tex), ifelse(b_plain, TRUE, NA), tex)
  out <- .scsj_and(.scsj_and(weath, tex), .scsj_subsurface_pos(x))
  out[.scsj_plough_rows(x)] <- FALSE
  out
}

# The bleached horizon (IV.1) rows, before thickness: a subsurface horizon of
# moist value 6 to 8 with chroma 4 or less, value 5 with chroma 3 or less, or
# value 4 with chroma 2 or less. An E horizon of the notation when the colour
# is not recorded; never a Bs, Bh or Bhs.
.scsj_bleached_rows <- function(x) {
  v <- x$colour$value; c <- x$colour$chroma
  col <- ifelse(is.na(v) | is.na(c), NA, (v >= 6 & c <= 4) | (v == 5 & c <= 3) | (v == 4 & c <= 2))
  col <- ifelse(is.na(col), ifelse(x$role %in% "E", TRUE, ifelse(x$des$known, FALSE, NA)), col)
  out <- .scsj_and(col, .scsj_subsurface_pos(x))
  ifelse(x$des$s | x$des$h, FALSE, out)
}

# The podzolic accumulation horizon (IV.1) rows, before thickness; never an
# E horizon.
#   morphology: a crushed moist colour of hue 7.5YR or redder with value 5 or
#     less and chroma 4 or less, or 10YR with value 3 or less and chroma 2 or
#     less, or a visible accumulation of free oxides with the reddest hue of
#     the profile (a Bs, Bh or Bhs of the notation); in a soil without a
#     layer with the red-yellow feature and without an argic horizon
#     (criterion 2 read for the soil, as its verb says: Hugo, 2026-10-31).
#   analysis: 0.6 % carbon or more, pH(H2O) 5.9 or less, Alo + 1/2 Feo 0.5 %
#     or more and twice that of the A or the bleached horizon.
.scsj_podzolic_rows <- function(x, red_yellow = NULL, argic = NULL) {
  h <- x$h
  hn <- x$colour$hue_num; v <- x$colour$value; c <- x$colour$chroma
  col_a <- ifelse(is.na(hn) | is.na(v) | is.na(c), NA, hn <= 17.5 & v <= 5 & c <= 4)
  col_b <- ifelse(is.na(hn) | is.na(v) | is.na(c), NA, hn == 20 & v <= 3 & c <= 2)
  hn_min <- suppressWarnings(min(hn[x$mineral %in% TRUE], na.rm = TRUE))
  oxide <- (x$des$s | x$des$h) & !is.na(x$role) & grepl("^B", x$role)
  # c: a visible free-oxide accumulation (a Bs, Bh or Bhs of the notation)
  # with the reddest hue of the profile (not needed over a buried soil); a
  # designated horizon without s or h has none
  col_c <- ifelse(oxide, ifelse(is.na(hn), TRUE, hn <= hn_min | any(x$des$buried)),
                  ifelse(x$des$known, FALSE, NA))
  col <- .scsj_or(.scsj_or(col_a, col_b), col_c)
  ry <- red_yellow %||% .scsj_red_yellow_rows(x)
  ag <- .scsj_argic_horizon_rows(x, argic %||% .scsj_argic_rows(x))
  none_ry_ag <- .scsj_not(.scsj_any(.scsj_or(ry, ag)))
  sub <- .scsj_subsurface_pos(x)
  not_e <- !(!is.na(x$role) & grepl("^E", x$role))
  morph <- ifelse(not_e, .scsj_and(.scsj_and(col, rep(none_ry_ag, x$n)), sub), FALSE)
  # analysis
  oc <- x$oc; ph <- .scsj_col(h, "ph_h2o")
  alo <- .scsj_col(h, "al_ox_pct"); feo <- .scsj_col(h, "fe_ox_pct")
  af <- ifelse(is.na(alo) | is.na(feo), NA_real_, alo + feo / 2)
  bl <- .scsj_bleached_rows(x)
  ref_rows <- which((x$role %in% c("A", "E") | bl %in% TRUE) & !is.na(af))
  ref <- if (length(ref_rows)) min(af[ref_rows]) else NA_real_
  twice <- if (is.na(ref)) rep(NA, x$n) else ifelse(is.na(af), NA, af >= 2 * ref)
  ana <- .scsj_and(.scsj_and(ifelse(is.na(oc), NA, oc >= 0.6), ifelse(is.na(ph), NA, ph <= 5.9)),
                   ifelse(is.na(af), NA, af >= 0.5))
  ana <- .scsj_and(.scsj_and(ana, twice), sub)
  # in a designated profile, a horizon without s or h and without Alo and
  # Feo is not one by analysis
  ana <- ifelse(is.na(ana) & is.na(af) & x$des$known & !oxide, FALSE, ana)
  ana <- ifelse(not_e, ana, FALSE)
  list(morph = morph, analysis = ana)
}

# The red-yellow weathered horizon rows: weathered, red or yellow, under
# 1.0 % carbon, structure other than single grain or massive or 15 % clay or
# more.
.scsj_red_yellow_weathered_rows <- function(x, weathered) {
  h <- x$h
  ry <- .scsj_or(x$colour$red, x$colour$yellow)
  lowc <- ifelse(is.na(x$oc), NA, x$oc < 1.0)
  st <- .scsj_txt(h, "structure_type"); gr <- .scsj_txt(h, "structure_grade")
  structured <- ifelse(is.na(st) & is.na(gr), NA,
                       !grepl("single|massive|structureless", paste(st, gr)))
  clay <- .scsj_col(h, "clay_pct")
  c15 <- ifelse(is.na(clay), NA, clay >= 15)
  .scsj_and(.scsj_and(.scsj_and(weathered, ry), lowc), .scsj_or(structured, c15))
}

# The paddy surface horizons (IV.1): the plough layer of paddy land, 7.5 cm
# or more; unbleached with yellowish or reddish brown mottles after drainage
# of 2 % or more and Fe(d) 0.4 % or more, bleached with none or rare mottles
# and Fe(d) under 0.4 %. Not paddy land: none. Land use not recorded: an Ap
# may be one. `surface` = either kind.
.scsj_paddy_surface_rows <- function(x) {
  ap <- .scsj_plough_rows(x)
  pad <- x$paddy
  any <- if (isFALSE(pad)) rep(FALSE, x$n) else ifelse(ap, if (isTRUE(pad)) TRUE else NA, FALSE)
  # 7.5 cm or more of plough layer
  ap_th <- .scsj_horizon_rows(x, ifelse(ap, TRUE, FALSE), 7.5)
  any <- .scsj_and(any, ifelse(ap, ap_th, FALSE))
  h <- x$h
  rf <- .scsj_col(h, "redoximorphic_features_pct"); fed <- .scsj_col(h, "fe_dcb_pct")
  unbleached <- .scsj_and(any, .scsj_and(ifelse(is.na(rf), NA, rf >= 2), ifelse(is.na(fed), NA, fed >= 0.4)))
  bleached <- .scsj_and(any, .scsj_and(ifelse(is.na(rf), NA, rf < 2), ifelse(is.na(fed), NA, fed < 0.4)))
  list(any = any, unbleached = unbleached, bleached = bleached,
       surface = .scsj_or(unbleached, bleached))
}

# ---- impermeable artificial layers (II A) -------------------------------------------

# Rows of a continuous impermeable artificial layer: a geomembrane recorded
# in the horizon (geomembrane_present), or technic hard material of 90 % or
# more.
.scsj_impermeable_rows <- function(x) {
  h <- x$h
  gm <- .scsj_col(h, "geomembrane_present", NA)
  thm <- .scsj_col(h, "technic_hardmaterial_pct")
  (as.logical(gm) %in% TRUE) | (!is.na(thm) & thm >= 90)
}

# "The top of a continuous impermeable artificial layer within d cm":
# technic hard material from its horizon's top; a geomembrane at the site's
# geomembrane_depth_cm (from the ground), else somewhere in its horizon
# (undecided when the horizon spans the depth).
.scsj_impermeable_within <- function(x, d) {
  h <- x$h
  gm <- as.logical(.scsj_col(h, "geomembrane_present", NA)) %in% TRUE
  thm <- .scsj_col(h, "technic_hardmaterial_pct")
  hard <- !is.na(thm) & thm >= 90
  sgd <- suppressWarnings(as.numeric(x$site$geomembrane_depth_cm %||% NA)[1L])
  site_gm <- isTRUE(as.logical(x$site$geomembrane %||% NA)[1L]) || any(gm)
  v <- any(hard & !is.na(x$dtop) & x$dtop <= d)
  if (site_gm && !is.na(sgd)) return(v || (sgd - (x$surface - x$ground)) <= d)
  if (any(gm)) {
    g <- .scsj_any(ifelse(gm, ifelse(!is.na(x$dbot) & x$dbot <= d, TRUE,
                                     ifelse(!is.na(x$dtop) & x$dtop > d, FALSE, NA)), FALSE))
    return(.scsj_or3(v, g))
  }
  v
}

# ---- "the top of a layer within / between depths" ------------------------------------

# "The top of a layer with the feature(s) within [from, to] cm of the soil
# surface", the layer `thick` cm or more. `rows` are row flags. TRUE: a run of
# TRUE rows of the thickness with its top in the window; NA: rows that may
# hold could make one (open bottoms and the end of the description counted
# as possible), or a part of the window no horizon describes (a gap, or below
# the description) could hold one; FALSE otherwise.
.scsj_top_in <- function(x, rows, from = -Inf, to = Inf, thick = 0) {
  if (is.na(x$surface)) return(NA)
  for (r in .scsj_runs(x, ifelse(rows %in% TRUE, TRUE, FALSE)))
    if (r$top >= from - 0.5 && r$top <= to && r$sure >= max(thick, 1e-9)) return(TRUE)
  for (r in .scsj_runs(x, rows, bridge = TRUE)) {
    bot <- if (r$at_end) Inf else r$bottom
    starts_above <- r$all_true && r$top < from - 0.5
    if (!starts_above && r$top <= to && bot >= from && r$poss >= thick) return(NA)
  }
  lo <- if (is.finite(from)) max(from, 0) else 0
  if (is.finite(to) && to > lo && .scsj_cum(x, rep(FALSE, x$n), lo, to)$poss > 0) return(NA)
  FALSE
}
