# =============================================================================
# v0.9.338 -- the types of the Russian soil classification (Field Guide of
# 2008, chapter 4): the profile formula of each type, matched on the
# diagnostic horizons of the profile.
#
# The Field Guide names a type by the formula of its profile, drawn as a
# column of horizons ("O-EL-BEL-BT-C"); it has no key to types. soilKey
# matches the profile against the formulas of the division it keyed to, with
# match_horizon_sequence() (R/sequence-engine.R), three-valued. The profile
# is the one the divisions read (R/diagnostics-kidpr.R): fresh litter left
# out, under a deposit of 40 cm or less, the surface horizons by the
# thickest-horizon rule of p. 37 (the thinner natural ones dropped, a W of
# 5 cm or less over another horizon dropped); the matcher merges the
# sub-horizons (BT1, BT2) and leaves out the transitions and the small
# horizons; an overlay (S, X or V laid over a host) is a feature of its
# layer, not a horizon of the sequence.
#
# Hugo's decisions of 2026-11-02:
#   Q13 the formulas are closed on the division: a horizon that some formula
#       of the division writes must be in the type's formula when it is in
#       the profile (in CA: EL, AEL, BEL, BT, BCA, G, Q); a horizon foreign
#       to the division is ignored (it may name a subtype). The surface and
#       agro horizons a formula does not write stay out of that closure, so
#       the natural remnant under the plough does not block an agro type.
#       The types are tried most specific first (more required horizons,
#       then more required marks), agro before natural; when two match, the
#       more specific names the type and the other is an intergrade, kept in
#       the trace. BEL is the exception (Hugo, 2026-11-02, from p. 44): it
#       closes only the formulas where it is diagnostic (CA07, CA08, CA11,
#       CA20, CA21, CD09); elsewhere a BEL is read as part of the passage from
#       the eluvial to the middle horizon, neither blocking a formula nor
#       required by it (the type files list it under `ignore`). So is BCA
#       in the formulas of CD that do not write it (p. 85), and in CB and
#       CE an EL counts with E (p. 43): it fills the E of a formula and
#       blocks the formulas without E.
#   Q14 a horizon in parentheses is optional; BEL is required only where
#       p. 44 calls it diagnostic (CA07, CA08, CA11, CA20, CA21, CD09) and
#       optional elsewhere (the type files say where); the g and q marks on
#       BEL, BT, BHF or BCA are expected, not required (the G or the Q makes
#       the gley type); the parent material (C, Cca, CG, CQ, C with
#       permafrost, CLM, D) is not tested; "directly under" passes over
#       transitions and sub-horizons, with up to 15 cm with no diagnostic
#       horizon between AU and BCA (p. 94).
#   Q15 a profile no formula matches keeps its division, the type
#       unassigned, with the nearest formulas in the trace and a note that
#       the Field Guide of 2008 does not provide for the combination. Before
#       that only near-identity readings, written on the types they concern
#       (`near_identity`): the BF of CB05, CE06 and CE07 is a BHF of either
#       kind; CJ01 takes BCA or CAT; CH02 takes O, T or H over G-CRM.
#
# An undecided layer that could change the match leaves the type undecided:
# a type ranked before the one assigned that is undecided makes it
# provisional; with none matched and some undecided, the type is left
# unassigned and the candidates are named.
#
# The formulas are in inst/rules/kidpr2008/types/<division>.yaml, one entry
# per type, the code in brackets (soilKey's: [CA01] ...) in the order of
# chapter 4, natural types first and then the agro types; the entry's test
# is type_formula_kidpr(). classify_kidpr2008() reorders each list into the
# order of trial before the generic engine walks it.
#
# v0.9.339 adds the types of CK to CT, under the same decisions: the BCA
# under the solonetz horizon is optional (Q14); the rock R of the lithozems
# and the gleyed substrate DG of the gley eluvozems are tested where they
# name the type (`test_terminals`, with the carbonate mark of Rca); the
# fragments of a TUR are tested, read by .kidpr_ty_fragments() (Q20); S over
# Q is a gley solonchak, a BT exposed or under PB is BI, the BH of CR07 and
# CS10 a BHF (Q15). Hugo's decisions of 2026-11-02 on them: in the types of
# CL an S laid over another horizon is the S of the formula
# (.kidpr_ty_salt_rows()); an exposed BT is BI at C.20 too (CQ01).
# After the review (Hugo, 2026-11-02): a type file may close a formula on
# more horizons (`close`: the middle horizons of C.5 in CT), reject a TUR of
# some fragments (`reject_fragments`: ASN, BSN in CT01 to CT13) and ask for a
# horizon directly under the salt horizon (`directly_under`: CL01, CL02),
# the last two applied to the matcher's results (.kidpr_ty_checks()); on
# the data route the gley flag of the substrate and the carbonate mark of
# the rock are read from the data (.kidpr_ty_data_flags()); the
# post-solonetzic turbozems, TUR on top, are tried with the agro types.
#
# v0.9.342 adds the types of trunks A, B and D (AA, AB, BA, BB, BC and the
# division [DA] of trunk D) and the name of the chemozems (CU), which have no
# types. The buried soil [ABC] of the stratozems is tested within 100 cm
# (p. 128); the rock marks of trunk D (C=, C.., C~~, C", C^^) and the rocks R,
# Rca, Rcs name its types and are tested (Q14); the rock under the dry peat
# (AA05) is R or loose material C (p. 43). Hugo's decisions (2026-11-02) on
# soilKey's readings: in trunk A a peat horizon that runs across the
# line 50 cm under the top of the peat body is TO, TE or TJ above it and TT
# below (Q22), and a T of the notation in AA is TO or TE by the botany of its
# layer (.kidpr_ty_peat_rows()); in [DA] a layer with no horizon under the O
# or W is the loose rock, the mark of a C that the notation does not write
# comes from the data (an alluvial, ash or aeolian origin, else the texture:
# sand and loamy sand "..", finer "="), the legend's aeolian mark "" is read
# as the ^^ of the diagrams, and on the data route the carbonate and gypsum
# marks of the rock R come from the rock named or the effervescence
# (.kidpr_ty_substrate()); the BH of BB01 and BB03 is a BHF of either kind,
# as the notation reads it. A chemozem is named by its pollutants and the
# soil it was: the profile keyed again with the X horizon masked
# (.kidpr_chemozem_name()).
# =============================================================================

.KIDPR_TY_REF <- "Polevoi opredelitel' pochv Rossii (2008), chapter 4 (the profile formulas of the types)."

# Near-identity readings every formula takes (soilKey's reading): the peat
# T of a formula includes the oligotrophic and eutrophic peats TO and TE, as
# in the key to divisions.
.KIDPR_TY_ALTERNATIVES <- list(T = c("TO", "TE"))

# "Directly under": a carbonate-free part of up to 15 cm between AU and BCA
# is not counted (p. 94; Q14).
.KIDPR_TY_GAPS <- c("AU>BCA" = 15)

# A buried soil names the stratozems on a buried soil when it starts within
# 100 cm (p. 128; Q14); no other formula tests one.
.KIDPR_TY_BURIED_CM <- 100

# The code of a division without soilKey's brackets: "[DA]" -> "DA".
.kidpr_ty_bare <- function(div) gsub("[][]", "", as.character(div)[1L])

# Parsed formulas of the type lists, by the content of the list.
.kidpr_ty_cache <- new.env(parent = emptyenv())


# The division of a type code: "[CA01]" -> "CA".
.kidpr_ty_division <- function(type) {
  x <- gsub("[][]", "", as.character(type)[1L])
  sub("[0-9]+$", "", x)
}

# The type lists of the installed rules (a predicate called outside a
# classification).
.kidpr_ty_installed <- function() {
  if (is.null(.kidpr_ty_cache$lists)) .kidpr_ty_cache$lists <- load_rules("kidpr2008")$types %||% list()
  .kidpr_ty_cache$lists
}

# The type entries of a division in a set of lists (keyed by the division
# code, or by its bracketed code, [DA]).
.kidpr_ty_list <- function(lists, div) {
  lists[[div]] %||% lists[[paste0("[", div, "]")]] %||% list()
}

# The formulas of a list of type entries, parsed: the near-identity readings
# of each type (with the T of every formula), its optional horizons and the
# parent slots it tests; the order of trial (the agro types, then the
# natural ones, each in the order of the list; then most specific first:
# the order .pick_scheme() reads); the vocabulary each formula is closed on:
# the division's (Q13), less the horizons the type ignores (BEL where it is
# not diagnostic, Hugo, 2026-11-02).
.kidpr_ty_parse <- function(entries) {
  key <- paste(vapply(entries, function(e) paste(
    e$code, e$formula, paste(unlist(e$optional), collapse = ","),
    paste(names(e$near_identity), vapply(e$near_identity %||% list(), paste, "", collapse = "|"), collapse = ";"),
    paste(unlist(e$test_terminals), collapse = ","), paste(unlist(e$ignore), collapse = ","),
    paste(unlist(e$close), collapse = ","), paste(unlist(e$reject_fragments), collapse = ","),
    paste(unlist(e$directly_under), collapse = ","),
    sep = "\t"), ""), collapse = "\r")
  hit <- .kidpr_ty_cache[[key]]
  if (!is.null(hit)) return(hit)
  voc <- .kidpr_vocabulary()
  codes <- vapply(entries, function(e) as.character(e$code), "")
  specs <- lapply(entries, function(e) {
    alt <- .KIDPR_TY_ALTERNATIVES
    ni <- e$near_identity %||% list()
    for (k in names(ni)) alt[[k]] <- unique(c(alt[[k]], as.character(unlist(ni[[k]]))))
    .parse_profile_formula(e$formula, masters = voc$masters, features = voc$features,
                           test_terminals = as.character(unlist(e$test_terminals)),
                           optional = as.character(unlist(e$optional)), alternatives = alt)
  })
  names(specs) <- codes
  agro <- vapply(specs, function(s) {
    z <- Filter(function(z) identical(z$role, "surface"), s$slots)
    length(z) > 0L && any(z[[1L]]$codes %in% c(.KIDPR_DV_AGRO, "TUR"))
  }, NA)
  base <- c(which(agro), which(!agro))
  sp <- vapply(specs, function(s) as.numeric(s$specificity[1:2]), c(0, 0))
  trial <- base[order(-sp[1L, base], -sp[2L, base], seq_along(base))]
  voc <- .formula_vocabulary(specs)
  vocabularies <- stats::setNames(lapply(entries, function(e)
    setdiff(union(voc, as.character(unlist(e$close))), as.character(unlist(e$ignore)))), codes)
  field <- function(k) stats::setNames(lapply(entries, function(e) as.character(unlist(e[[k]]))), codes)
  out <- list(specs = specs[base], codes = codes, trial = codes[trial], agro = agro,
              vocabulary = voc, vocabularies = vocabularies,
              reject_fragments = field("reject_fragments"), directly_under = field("directly_under"))
  assign(key, out, envir = .kidpr_ty_cache)
  out
}

# The type lists of a rule set in the order of trial, kept on the view of the
# classification for type_formula_kidpr(). classify_hierarchical() walks a
# list in its order and makes the class assigned provisional when one before
# it is undecided, so the lists must be in the order the matcher picks in.
.kidpr_types_attach <- function(rules, view = NULL) {
  ty <- rules$types %||% list()
  for (d in names(ty)) {
    e <- ty[[d]]
    if (length(e) < 2L) next
    sch <- .kidpr_ty_parse(e)
    ty[[d]] <- e[match(sch$trial, sch$codes)]
  }
  if (length(ty)) rules$types <- ty
  if (!is.null(view)) view$type_lists <- ty
  rules
}

# The profile as the types read it: the rows of the view under a deposit of
# 40 cm or less, fresh litter out, buried rows marked; an overlay is a feature
# of its layer. The surface horizons all stay, but for a W of 5 cm or less
# over another horizon (p. 39): the matcher applies the thickest-horizon rule
# of p. 37 itself (agro_remnant "thickest": under the agro horizon only the
# thickest natural horizon stays, its remnant), so that a tie or a missing
# thickness, or an undecided layer that could thicken a horizon of the
# surface run, leaves the type undecided instead of following one reading.
# Rows the view reads as transitions or loose rock (the FAO transitions BC,
# B/C and the FAO C of the data route) are transitions and parent material
# here too. A data-route layer with no horizon may be a transition or loose
# rock (Hugo, 2026-11-02): it is undecided between no horizon and loose
# rock, so between two horizons of a formula it leaves the type undecided.
.kidpr_ty_table <- function(v, div = NULL) {
  f <- v$f
  rows <- if (v$start <= v$n) seq.int(v$start, v$n) else integer(0)
  if (length(v$dep$rows) && isTRUE(v$dep$thick <= 40)) rows <- setdiff(rows, v$dep$rows)
  # the W of 5 cm or less the view drops in every reading
  wdrop <- integer(0)
  for (e in v$hz_raw) if (identical(e$code, "W") && e$kind == "horizon" && isTRUE(e$thick <= 5) &&
                          all(vapply(v$var, function(w) !any(vapply(w$hz, function(h) identical(h$rows, e$rows), NA)), NA)))
    wdrop <- c(wdrop, e$rows)
  rows <- setdiff(rows, wdrop)
  cols <- c("horizon_idx", "top_cm", "bottom_cm", "designation", "code", "passed", "small",
            "transitional", "rockflag", "mark", "route", "evidence", "features", "features_na",
            "composition", "missing", "candidates")
  t <- f[rows, intersect(cols, names(f)), drop = FALSE]
  kind <- v$kind[rows]
  t$transitional <- t$transitional | kind == "trans"
  rk <- kind == "rock" & is.na(t$code)
  t$code[rk] <- "C"
  t$passed[rk] <- TRUE
  nh <- kind == "none" & !(t$route %in% "notation")
  t$passed[nh] <- NA
  t$candidates[nh] <- rep(list(c("", "C")), sum(nh))
  t$missing[nh] <- lapply(t$missing[nh], function(m) if (length(m)) m else "designation")
  t$no_horizon <- nh
  t$buried <- v$bur[rows]
  ov <- f$overlay[rows]
  ovna <- if (is.null(f$overlay_na)) rep(list(character(0)), length(rows)) else f$overlay_na[rows]
  t$features <- Map(function(a, b) unique(c(as.character(a), as.character(b))), t$features, ov)
  t$features_na <- Map(function(a, b) unique(c(as.character(a), as.character(b))), t$features_na, ovna)
  t$overlay <- rep(list(character(0)), length(rows))
  t$composition <- .kidpr_ty_fragments(v, t)
  t$flag_missing <- rep(list(character(0)), nrow(t))
  if (!is.null(div) && div %in% c("CO", "CP")) t <- .kidpr_ty_data_flags(v, t, div)
  if (identical(div, "CL")) t <- .kidpr_ty_salt_rows(t)
  dv <- if (is.null(div)) "" else .kidpr_ty_bare(div)
  if (dv %in% c("AA", "AB")) t <- .kidpr_ty_peat_rows(v, t, dv)
  if (dv == "DA") t <- .kidpr_ty_substrate(v, t)
  rownames(t) <- NULL
  t
}

# The marks the gley eluvozems and the carbolithozems test (Q14), on the data
# route, where the notation does not write them: the gley flag of the loose
# substrate under the eluvial horizon (read as the D of the formulas), from
# a gley colour of the matrix (key 24) or a redox sign recorded (mottles,
# redoximorphic features, redox segregations, a positive dipyridyl test);
# the carbonate mark of the hard rock R, from a carbonate rock named
# (rock_origin, parent_material, of the layer or the site) or effervescence
# (or CaCO3).
# Evidence that says neither leaves the flag undecided, so the type is not
# the non-gley or non-carbonate one by default, and so does a designation
# that marks the substrate gleyed (Cg, 2Cg) against a colour that is not
# (Hugo, 2026-11-02).
.KIDPR_TY_CARB_ROCK <- paste0("limestone|dolomit|marl|chalk|calcar|carbonat|travertin|calcite|",
                              "\u0438\u0437\u0432\u0435\u0441\u0442\u043d|\u0434\u043e\u043b\u043e\u043c\u0438\u0442|",
                              "\u043c\u0435\u0440\u0433\u0435\u043b|\u043a\u0430\u0440\u0431\u043e\u043d\u0430\u0442")
.kidpr_ty_data_flags <- function(v, t, div) {
  if (is.null(t$rockflag)) t$rockflag <- rep(NA_character_, nrow(t))
  data <- t$route %in% "data" & t$buried == 0L
  rows <- which(data & if (div == "CO") t$code %in% c("C", "D") & is.na(t$rockflag) else t$code %in% "R")
  if (!length(rows) || is.null(v$pedon)) return(t)
  x <- v$kidpr_x %||% (v$kidpr_x <- .kidpr_profile(v$pedon))
  for (j in rows) {
    i <- t$horizon_idx[j]
    if (is.na(i)) next
    if (div == "CO") {
      # the substrate of the eluvozems, D in their formulas
      t$code[j] <- "D"
      g <- .kidpr_gley(x, i)$passed
      p <- if (isTRUE(g) || isTRUE(.kidpr_redox_sign(x, i))) TRUE else if (isFALSE(g)) FALSE else NA
      # a gleyed substrate by its designation (Cg, 2Cg) with neither a gley
      # colour nor a redox sign recorded: undecided, not the non-gley type
      d <- as.character(.kidpr_v(x, i, "designation"))
      if (isFALSE(p) && !is.na(d) && grepl("^[0-9IVX]*[CD][a-z]*g", d)) p <- NA
      if (isTRUE(p)) t$rockflag[j] <- "G"
      else if (is.na(p)) {
        t$features_na[[j]] <- unique(c(as.character(t$features_na[[j]]), "G"))
        t$flag_missing[[j]] <- c("munsell_hue_moist", "munsell_value_moist", "munsell_chroma_moist", "mottle_colors")
      }
    } else {
      txt <- tolower(paste(c(.kidpr_txt(x, i, "rock_origin"), .kidpr_txt(x, i, "parent_material"),
                             as.character(v$site$parent_material %||% NA)), collapse = " "))
      named <- grepl(.KIDPR_TY_CARB_ROCK, txt)
      eff <- .kidpr_eff_any(x, i)$passed
      p <- if (named || isTRUE(eff)) TRUE else if (isFALSE(eff)) FALSE else NA
      if (isTRUE(p)) t$features[[j]] <- unique(c(as.character(t$features[[j]]), "ca"))
      else if (is.na(p)) {
        t$features_na[[j]] <- unique(c(as.character(t$features_na[[j]]), "ca"))
        t$flag_missing[[j]] <- c("rock_origin", "effervescence_class", "caco3_pct")
      }
    }
  }
  t
}

# In the types of the halomorphic soils an S or SS laid over another horizon
# is the S of the formula (Hugo, 2026-11-02: the secondary solonchaks form
# over any profile, pp. 56, 105): the layer is read as the salt horizon over
# its host, two rows at the same depths; an overlay the data leave undecided
# makes the salt row undecided (S or no horizon). Elsewhere the overlay stays
# a feature of its layer.
.kidpr_ty_salt_rows <- function(t) {
  salt <- c("S", "SS")
  if (!nrow(t)) return(t)
  dec <- lapply(t$features, function(x) intersect(salt, as.character(x)))
  und <- lapply(t$features_na, function(x) intersect(salt, as.character(x)))
  host <- !(t$code %in% salt)
  add <- host & (lengths(dec) > 0L | lengths(und) > 0L)
  if (!any(add)) return(t)
  idx <- unlist(lapply(seq_len(nrow(t)), function(i) if (add[i]) c(i, i) else i))
  first <- c(TRUE, idx[-1L] != idx[-length(idx)])
  out <- t[idx, , drop = FALSE]
  srow <- which(add[idx] & first)
  for (j in srow) {
    i <- idx[j]
    d <- length(dec[[i]]) > 0L
    out$code[j] <- if (d) dec[[i]][1L] else NA_character_
    out$passed[j] <- if (d) TRUE else NA
    out$candidates[[j]] <- if (d) character(0) else c(und[[i]], "")
    out$missing[[j]] <- if (d) character(0) else out$missing[[j]]
    for (k in c("features", "features_na", "composition")) out[[k]][[j]] <- character(0)
    out$small[j] <- FALSE
    out$transitional[j] <- FALSE
    if (!is.null(out$rockflag)) out$rockflag[j] <- NA_character_
    if (!is.null(out$mark)) out$mark[j] <- NA_character_
    if (!is.null(out$no_horizon)) out$no_horizon[j] <- FALSE
  }
  hostrow <- which(add[idx] & !first)
  for (j in hostrow) {
    out$features[[j]] <- setdiff(as.character(out$features[[j]]), salt)
    out$features_na[[j]] <- setdiff(as.character(out$features_na[[j]]), salt)
  }
  out
}

# The peat of trunk A (Hugo's decision Q22: TO, TE and TJ are the peat
# horizons of the upper 50 cm of the peat body, TT the peat below), as the
# types read it (Hugo, 2026-11-02, on soilKey's readings): a
# peat horizon that runs across the line 50 cm under the top of the body is
# split there, TT below; in AA a T of the notation is TO or TE by the botany
# of its layer (keys 43a, 43b: the moor type, Sphagnum fibres, a pH of 6.5
# or more that rules TO out), undecided between them when the layer records
# neither. A peat horizon that starts under the line keeps its index.
.KIDPR_TY_PEAT <- c("T", "TO", "TE", "TJ")
.kidpr_ty_peat_rows <- function(v, t, div) {
  if (!nrow(t)) return(t)
  live <- t$buried == 0L
  allin <- function(set) vapply(t$candidates, function(c) length(c) > 0L && all(c %in% set), NA)
  if (div == "AA" && !is.null(v$pedon)) {
    x <- v$kidpr_x %||% (v$kidpr_x <- .kidpr_profile(v$pedon))
    for (j in which(live & t$code %in% "T")) {
      i <- t$horizon_idx[j]
      if (is.na(i)) next
      a <- .kidpr_q43a(x, i)$passed
      b <- .kidpr_q43b(x, i)$passed
      cd <- if (isTRUE(a)) "TO" else if (isFALSE(a) || isTRUE(b)) "TE" else NA_character_
      t$code[j] <- cd
      if (is.na(cd)) {
        t$passed[j] <- NA
        t$candidates[[j]] <- c("TO", "TE")
        t$missing[[j]] <- unique(c(as.character(t$missing[[j]]), "peat_moor_type", "sphagnum_fibres_pct"))
      }
    }
  }
  body <- which(live & (t$code %in% .KIDPR_DV_PEAT | allin(.KIDPR_DV_PEAT)))
  if (!length(body) || is.na(t$top_cm[body[1L]])) return(t)
  line <- t$top_cm[body[1L]] + 50
  peat <- t$code %in% .KIDPR_TY_PEAT | (is.na(t$code) & allin(.KIDPR_TY_PEAT))
  cut <- which(live & peat & !is.na(t$top_cm) & !is.na(t$bottom_cm) & t$top_cm < line & t$bottom_cm > line)
  if (!length(cut)) return(t)
  idx <- unlist(lapply(seq_len(nrow(t)), function(i) if (i %in% cut) c(i, i) else i))
  first <- c(TRUE, idx[-1L] != idx[-length(idx)])
  out <- t[idx, , drop = FALSE]
  up <- which(idx %in% cut & first)
  lo <- which(idx %in% cut & !first)
  out$bottom_cm[up] <- line
  out$top_cm[lo] <- line
  out$code[lo] <- "TT"
  out$passed[lo] <- TRUE
  out$candidates[lo] <- rep(list(character(0)), length(lo))
  out$missing[lo] <- rep(list(character(0)), length(lo))
  out
}

# The substrate of the weakly developed soils, whose types it names (Q14), as
# the types read it (Hugo, 2026-11-02, on soilKey's readings): a
# layer with no horizon under the O or W is the loose rock C (the trunk has no
# other horizon); the legend's aeolian mark "" (p. 75) is the ^^ of the
# diagrams; a C whose mark is not written (the notation or the data route)
# takes it from the data: an alluvial origin "~~", volcanic ash (or 30 % of
# volcanic glass) the ash mark, an aeolian one "^^" (loess is no stratified
# aeolian body), else the texture (sand and loamy sand, the Russian pesok and
# supes', ".."; finer "="), undecided between them when no texture is
# recorded; a proluvial body has no type. On the data route the rock R takes
# its carbonate mark as in the lithozems (a carbonate rock named, or
# effervescence) and its gypsum mark from a gypsum rock named; a rock not
# named leaves the gypsum mark undecided.
.KIDPR_TY_AEOLIAN <- paste0("aeolian|eolian|\\bdunes?\\b|wind-?blown|blown sand|", .KIDPR_DV_NL, "\u044d\u043e\u043b")
.KIDPR_TY_LOESS <- paste0("\\bloess|l\u00f6ss|", .KIDPR_DV_NL, "\u043b[\u0435\u0451]\u0441\u0441")
.KIDPR_TY_PROLUVIAL <- paste0("proluv|", .KIDPR_DV_NL, "\u043f\u0440\u043e\u043b\u044e\u0432")
.KIDPR_TY_GYPSUM_ROCK <- paste0("gypsum|gypsite|anhydrit|", .KIDPR_DV_NL, "\u0433\u0438\u043f\u0441|",
                                .KIDPR_DV_NL, "\u0430\u043d\u0433\u0438\u0434\u0440\u0438\u0442")
.kidpr_ty_substrate <- function(v, t) {
  if (!nrow(t)) return(t)
  if (is.null(t$mark)) t$mark <- rep(NA_character_, nrow(t))
  live <- t$buried == 0L
  nh <- if (is.null(t$no_horizon)) rep(FALSE, nrow(t)) else t$no_horizon
  for (j in which(live & nh)) {
    t$code[j] <- "C"
    t$passed[j] <- TRUE
    t$candidates[[j]] <- character(0)
    t$missing[[j]] <- character(0)
    t$no_horizon[j] <- FALSE
  }
  t$mark[t$mark %in% "\"\""] <- "^^"
  if (is.null(v$pedon)) return(t)
  x <- v$kidpr_x %||% (v$kidpr_x <- .kidpr_profile(v$pedon))
  txt <- function(i, keys) {
    s <- c(vapply(keys, function(k) as.character(.kidpr_txt(x, i, k)), ""), as.character(v$site$parent_material %||% NA))
    s <- s[!is.na(s) & nzchar(s)]
    if (length(s)) tolower(paste(s, collapse = " ")) else NA_character_
  }
  for (j in which(live & t$code %in% "C" & is.na(t$mark) & !t$transitional & !t$small)) {
    i <- t$horizon_idx[j]
    if (is.na(i)) next
    o <- txt(i, c("layer_origin", "rock_origin", "parent_material"))
    vg <- .kidpr_num(x, i, "volcanic_glass_pct")
    mk <- if (.kidpr_dv_match(.KIDPR_DV_ALLUVIUM, o) && !.kidpr_dv_match(.KIDPR_DV_NOT_ALLUVIUM, o)) "~~"
          else if (.kidpr_dv_match(.KIDPR_DV_ASH, o) || isTRUE(vg >= 30)) "\""
          else if (.kidpr_dv_match(.KIDPR_TY_AEOLIAN, o) && !.kidpr_dv_match(.KIDPR_TY_LOESS, o)) "^^"
          else NA_character_
    if (is.na(mk) && .kidpr_dv_match(.KIDPR_TY_PROLUVIAL, o)) next
    if (is.na(mk)) {
      k <- .kidpr_grad(x, i)$step
      mk <- if (is.na(k)) NA_character_ else if (k <= 2) ".." else "="
    }
    if (!is.na(mk)) t$mark[j] <- mk
    else {
      t$features_na[[j]] <- unique(c(as.character(t$features_na[[j]]), "=", ".."))
      t$flag_missing[[j]] <- unique(c(as.character(t$flag_missing[[j]]), .KIDPR_TEXTURE_MISSING))
    }
  }
  for (j in which(live & t$code %in% "R" & t$route %in% "data")) {
    i <- t$horizon_idx[j]
    if (is.na(i)) next
    o <- txt(i, c("rock_origin", "parent_material"))
    carb <- .kidpr_dv_match(.KIDPR_TY_CARB_ROCK, o)
    gyps <- .kidpr_dv_match(.KIDPR_TY_GYPSUM_ROCK, o)
    eff <- .kidpr_eff_any(x, i)$passed
    ca <- if (carb || isTRUE(eff)) TRUE else if (isFALSE(eff)) FALSE else NA
    cs <- if (gyps) TRUE else if (!is.na(o) || isTRUE(ca)) FALSE else NA
    f <- as.character(t$features[[j]]); fna <- as.character(t$features_na[[j]])
    if (isTRUE(ca)) f <- c(f, "ca") else if (is.na(ca)) fna <- c(fna, "ca")
    if (isTRUE(cs)) f <- c(f, "cs") else if (is.na(cs)) fna <- c(fna, "cs")
    t$features[[j]] <- unique(f)
    t$features_na[[j]] <- unique(fna)
    if (is.na(ca) || is.na(cs))
      t$flag_missing[[j]] <- unique(c(as.character(t$flag_missing[[j]]), "rock_origin",
                                      if (is.na(ca)) c("effervescence_class", "caco3_pct")))
  }
  t
}

# The fragments of a TUR (Hugo's decision Q20): from the notation
# (TUR[SEL+ASN]), else from the column turbated_fragments ("SEL;ASN"); with
# neither, unknown (the solonetzic types undecided) when the TUR has an ESP
# of 15 % or more, from na_cmol and cec_cmol or esp_pct, and otherwise not
# the fragments of a formula ("none", which no formula writes).
.kidpr_ty_fragments <- function(v, t) {
  comp <- if (is.null(t$composition)) rep(list(character(0)), nrow(t)) else t$composition
  h <- v$h
  col <- function(k, i) {
    if (is.null(h) || is.null(h[[k]]) || is.na(i) || i < 1L || i > nrow(h)) return(NA)
    h[[k]][i]
  }
  for (j in which(t$code %in% "TUR")) {
    if (length(comp[[j]])) next
    i <- t$horizon_idx[j]
    fr <- col("turbated_fragments", i)
    if (!is.na(fr) && nzchar(trimws(fr))) {
      x <- toupper(trimws(unlist(strsplit(.formula_normalise(fr), "[;+,]+"))))
      comp[[j]] <- x[nzchar(x)]
      next
    }
    na <- suppressWarnings(as.numeric(col("na_cmol", i)))
    cec <- suppressWarnings(as.numeric(col("cec_cmol", i)))
    esp <- if (!is.na(na) && !is.na(cec) && cec > 0) 100 * na / cec else suppressWarnings(as.numeric(col("esp_pct", i)))
    if (!isTRUE(esp >= 15)) comp[[j]] <- "none"
  }
  comp
}

# The match of the profile against the formulas of a division, once per
# view: .match_schemes() with the pick, the intergrades, the nearest
# formulas and every result; NULL when the division has no types.
.kidpr_ty_match <- function(v, div) {
  div <- .kidpr_ty_bare(div)
  .kidpr_dv_memo(v, paste0("types|", div), function() {
    entries <- .kidpr_ty_list(v$type_lists %||% .kidpr_ty_installed(), div)
    if (!length(entries)) return(NULL)
    sch <- .kidpr_ty_parse(entries)
    tab <- .kidpr_ty_table(v, div)
    m <- .match_schemes(tab, sch$specs, vocabulary = sch$vocabularies,
                        surface = .KIDPR_DV_NATURAL, agro = .KIDPR_DV_AGRO, agro_remnant = "thickest",
                        pair_gaps_cm = .KIDPR_TY_GAPS, buried_within_cm = .KIDPR_TY_BURIED_CM)
    m <- .kidpr_ty_checks(m, tab, sch)
    m$division <- div
    m$schemes <- sch
    m$table <- tab
    m$names <- stats::setNames(vapply(entries, function(e) as.character(e$name %||% ""), ""),
                               sch$codes)
    m
  })
}

# Two tests the matcher does not write (Hugo, 2026-11-02), applied to its
# results before the pick is made again: a plain TUR (CT01 to CT13,
# `reject_fragments`) is not a TUR of solonetz fragments (ASN, BSN), and is
# undecided when its fragments are unknown (Q20); the G or Q of CL01 and
# CL02 (`directly_under`) lies directly under the salt horizon or its host
# layer, transitions passed over.
.kidpr_ty_checks <- function(m, tab, sch) {
  rej <- sch$reject_fragments; du <- sch$directly_under
  if (!any(lengths(rej)) && !any(lengths(du))) return(m)
  live <- which(tab$buried == 0L & !tab$small & !tab$transitional &
                  !(if (is.null(tab$no_horizon)) FALSE else tab$no_horizon))
  changed <- FALSE
  set <- function(k, val, note) {
    r <- m$results[[k]]
    if (isFALSE(r$passed) || identical(r$passed, val)) return()
    r$passed <- if (isTRUE(r$passed)) val else if (isFALSE(val)) FALSE else NA
    if (isFALSE(r$passed)) r$distance <- max(1, r$distance)
    r$notes <- unique(c(r$notes, note))
    m$results[[k]] <<- r
    changed <<- TRUE
  }
  for (k in names(rej)[lengths(rej) > 0L]) {
    tur <- live[tab$code[live] %in% "TUR"]
    if (!length(tur)) next
    comp <- tab$composition[tur]
    if (any(vapply(comp, function(c) any(rej[[k]] %in% c), NA)))
      set(k, FALSE, sprintf("the TUR holds fragments of %s: a solonetzic or post-solonetzic soil (Hugo, 2026-11-02)",
                            paste(rej[[k]], collapse = " or ")))
    else if (any(lengths(comp) == 0L))
      set(k, NA, "the fragments of the TUR are not recorded and its ESP is 15 % or more (Hugo, 2026-11-02)")
  }
  for (k in names(du)[lengths(du) > 0L]) {
    s <- live[tab$code[live] %in% c("S", "SS")]
    if (!length(s)) next
    s1 <- s[1L]
    after <- live[live > s1 & tab$horizon_idx[live] != tab$horizon_idx[s1]]
    host <- live[live > s1 & tab$horizon_idx[live] == tab$horizon_idx[s1]]
    ok <- if (length(host) && tab$code[host[1L]] %in% du[[k]]) TRUE
          else if (!length(after)) FALSE
          else if (is.na(tab$code[after[1L]])) { if (any(du[[k]] %in% tab$candidates[[after[1L]]])) NA else FALSE }
          else tab$code[after[1L]] %in% du[[k]]
    if (!isTRUE(ok))
      set(k, if (is.na(ok)) NA else FALSE,
          sprintf("%s does not lie directly under the salt horizon (Hugo, 2026-11-02)", paste(du[[k]], collapse = " or ")))
  }
  if (!changed) return(m)
  pick <- .pick_scheme(m$results)
  for (k in names(pick)) m[[k]] <- pick[[k]]
  m$nearest <- if (!isTRUE(pick$passed)) .nearest_schemes(m$results, 3L) else NULL
  m
}

# The verdict of one type within the pick: TRUE for the type picked; for a
# type ranked before it in the order of trial, its own answer (FALSE, or NA,
# which makes the type picked provisional); FALSE for a type after it (an
# intergrade when its formula matches too). With no type picked, its own
# answer.
.kidpr_ty_verdict <- function(m, code) {
  r <- m$results[[code]]
  if (isTRUE(m$passed)) {
    if (identical(code, m$code)) return(TRUE)
    if (match(code, m$schemes$trial) < match(m$code, m$schemes$trial)) return(r$passed)
    return(FALSE)
  }
  r$passed
}

# The rows (horizon_idx) of the horizons that fill the slots of a match.
.kidpr_ty_layers <- function(matched) {
  if (is.null(matched) || !nrow(matched)) return(integer(0))
  ids <- regmatches(matched$horizon, regexpr("\\(([^()]*)\\)$", matched$horizon))
  ids <- gsub("[()]", "", ids)
  out <- integer(0)
  for (s in unlist(strsplit(ids, ", ", fixed = TRUE))) {
    ab <- suppressWarnings(as.integer(strsplit(s, "-", fixed = TRUE)[[1L]]))
    if (length(ab) == 2L && !anyNA(ab)) out <- c(out, seq.int(ab[1L], ab[2L]))
    else if (length(ab) == 1L && !is.na(ab)) out <- c(out, ab)
  }
  unique(out)
}

# The attributes that would decide an undecided match: those of the
# undecided layers, else the depths (a tie of thickness at the surface).
.kidpr_ty_missing <- function(m) {
  tab <- m$table
  und <- which(is.na(tab$code) & !(tab$passed %in% FALSE) & tab$buried == 0L)
  miss <- unique(unlist(tab$missing[und]))
  # the marks the data leave undecided, and the fragments of a TUR
  if (!is.null(tab$flag_missing)) miss <- unique(c(miss, unlist(tab$flag_missing[tab$buried == 0L])))
  tur <- which(tab$code %in% "TUR" & tab$buried == 0L & lengths(tab$composition) == 0L)
  if (length(tur)) miss <- unique(c(miss, "turbated_fragments"))
  if (!length(miss)) miss <- c("top_cm", "bottom_cm")
  miss
}

# The horizons of the profile as the types read them, from the top
# ("AY-EL-BT-G-C"): sub-horizons merged, transitions, small horizons and
# layers with no horizon left out, an undecided layer "?".
.kidpr_ty_sequence <- function(tab) {
  nh <- if (is.null(tab$no_horizon)) FALSE else tab$no_horizon
  keep <- !(tab$small | tab$transitional) & !(is.na(tab$code) & tab$passed %in% FALSE) & !nh & tab$buried == 0L
  cd <- ifelse(is.na(tab$code[keep]), "?", tab$code[keep])
  if (!length(cd)) return("")
  paste(cd[c(TRUE, cd[-1L] != cd[-length(cd)] | cd[-1L] == "?")], collapse = "-")
}

# One line on a formula and how far the profile is from it: the slots
# missing, the horizons not in the formula, and a horizon not directly under
# the one above it (a gap with no diagnostic horizon).
.kidpr_ty_describe <- function(r) {
  gap <- grep(" across .* with no diagnostic horizon|does not lie directly under the salt|the TUR holds fragments of|fragments of the TUR are not recorded",
              r$notes, value = TRUE)
  parts <- c(if (length(r$missing)) sprintf("missing %s", paste(r$missing, collapse = ", ")),
             if (length(r$extra)) sprintf("not in the formula: %s", paste(r$extra, collapse = ", ")),
             gap)
  sprintf("%s, distance %s%s", r$formula, format(r$distance),
          if (length(parts)) paste0(" (", paste(parts, collapse = "; "), ")") else "")
}


#' Types of the Russian soil classification (internal)
#'
#' The test of the types of the Russian soil classification (Field Guide
#' of 2008, chapter 4), read by \code{\link{classify_kidpr2008}} through
#' the type lists of \code{inst/rules/kidpr2008/types/}. A type is named by
#' the formula of its profile ("O-EL-BEL-BT-C"); the profile is matched
#' against every formula of its division with
#' \code{\link{match_horizon_sequence}}, three-valued, on the horizons the
#' divisions read (\code{\link{identify_horizons}}): fresh litter left out,
#' the profile under a deposit of 40 cm or less, the surface horizons by the
#' thickest-horizon rule, sub-horizons merged, transitions and small
#' horizons left out, an overlay a feature of its layer.
#'
#' Hugo's decisions of 2026-11-02: the match is closed on the horizons the
#' formulas of the division write (a horizon foreign to the division is
#' ignored), BEL only where it is the diagnostic eluvial horizon (CA07, CA08,
#' CA11, CA20, CA21, CD09; elsewhere a BEL neither blocks a formula nor is
#' required by it); a horizon in parentheses is optional, and so is BEL
#' except where it is diagnostic; the g and q marks are expected, not
#' required; the parent material is not tested; "directly under" passes over
#' transitions and sub-horizons, and over up to 15 cm with no diagnostic
#' horizon between AU and BCA. The types are tried most specific first, agro
#' before natural: the first that matches is the type, and another that
#' matches is an intergrade, in the evidence. A few near-identity readings
#' are written on the types they concern (the BF of CB05, CE06 and CE07 is a
#' BHF of either kind; CJ01 takes BCA or CAT; CH02 takes O, T or H; in CB and
#' CE an EL fills the E of a formula); in CD a BCA, like BEL, closes no
#' formula that does not write it. A tie or a missing thickness at the
#' surface, or a layer the data route reads as no horizon between two
#' horizons of a formula, leaves the type undecided.
#'
#' The types of trunks A, B and D (v0.9.342): the buried soil [ABC] of the
#' stratozems is tested within 100 cm; the substrate of trunk D names its
#' types and is tested (the marks of the loose rock C=, C.., C~~, C", C^^,
#' and R, Rca, Rcs), its mark from the data where it is not written (an
#' alluvial, ash or aeolian origin, else the texture); in trunk A a peat
#' horizon across the line 50 cm under the top of the peat body is TT below
#' it, and a T of the notation in AA is TO or TE by its botany; the BH of
#' BB01 and BB03 is a BHF of either kind.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param type The code of a type, as its rule file writes it
#'   (\code{"[CA01]"}).
#' @return A \code{\link{DiagnosticResult}}: TRUE for the type of the
#'   profile; FALSE for another type of the division (one whose formula
#'   matches as well is an intergrade, named in the evidence); NA when an
#'   undecided layer could change the match. The evidence gives the formula,
#'   the horizons that fill its slots, the optional ones absent, the slots
#'   missing, the horizons not in the formula, the horizons left out of the
#'   match, the surface horizon and the edit distance.
#' @keywords internal
#' @name kidpr2008_types
NULL

#' @rdname kidpr2008_types
#' @export
type_formula_kidpr <- function(pedon, type) {
  v <- .kidpr_dv_of(pedon)
  type <- as.character(unlist(type))[1L]
  div <- .kidpr_ty_division(type)
  m <- .kidpr_ty_match(v, div)
  r <- if (is.null(m)) NULL else m$results[[type]]
  if (is.null(r)) {
    return(DiagnosticResult$new(name = "type_formula_kidpr", passed = NA,
                                evidence = list(type = type), missing = "diagnostic_type_formula_kidpr",
                                reference = .KIDPR_TY_REF,
                                notes = sprintf("No formula of the type %s in the rules.", type)))
  }
  p <- .kidpr_ty_verdict(m, type)
  picked <- isTRUE(p)
  notes <- r$notes
  if (isTRUE(r$passed) && !picked && isTRUE(m$passed))
    notes <- c(notes, sprintf("the formula matches too: an intergrade of %s, which names the type (Hugo's decision Q13)",
                              m$code))
  if (picked && length(m$intergrades))
    notes <- c(notes, sprintf("intergrade: the formula of %s matches too (Hugo's decision Q13)",
                              paste(m$intergrades, collapse = ", ")))
  ev <- list(type = type, formula = r$formula, matched = r$matched, absent = r$absent,
             missing_slots = r$missing, extra = r$extra, ignored = r$ignored,
             surface = r$surface, absorbed = r$absorbed, sequence = r$sequence,
             distance = r$distance, undecided = r$undecided)
  if (picked) {
    ev$intergrades <- m$intergrades
    ev$provisional_because <- m$provisional_because
  }
  DiagnosticResult$new(name = "type_formula_kidpr", passed = p,
                       layers = if (picked) .kidpr_ty_layers(r$matched) else integer(0),
                       evidence = ev,
                       missing = if (is.na(p)) .kidpr_ty_missing(m) else character(0),
                       reference = .KIDPR_TY_REF,
                       notes = if (length(notes)) unique(notes) else NA_character_)
}


# The notes of the type level on a result: an intergrade; the candidates
# when the type is undecided; a combination of horizons no formula of the
# division provides for (Q15), with the nearest formulas, in the warnings and
# in the trace (phase "types_nearest").
.kidpr_types_notes <- function(res, view) {
  lv <- res$levels
  if (is.null(lv) || nrow(lv) < 2L || !identical(lv$level[2L], "divisions")) return(res)
  div <- lv$code[2L]
  if (!length(.kidpr_ty_list(view$type_lists %||% .kidpr_ty_installed(), div))) return(res)
  m <- .kidpr_ty_match(view, div)
  if (is.null(m)) return(res)
  label <- function(k) sprintf("%s %s", k, m$results[[k]]$formula)
  notes <- character(0)
  if (nrow(lv) >= 3L) {
    if (length(m$intergrades)) notes <- c(notes, sprintf(
      "Intergrade: the profile also matches the formula of %s; %s names the type, the more specific or the first in the order of trial (Hugo's decision Q13).",
      paste(vapply(m$intergrades, label, ""), collapse = "; "), label(lv$code[3L])))
  } else if (is.na(m$passed)) {
    cands <- vapply(m$candidates, label, "")
    notes <- c(notes, sprintf(
      "The type is undecided%s: an undecided layer could change the match (see $missing_data); the name stops at the division.",
      if (length(cands) == 1L) sprintf(", the formula of %s may match", cands)
      else sprintf(" between %s", paste(cands, collapse = "; "))))
  } else {
    near <- m$nearest
    stop_note <- sprintf("^No class of the types list under %s passed", div)
    res$warnings <- res$warnings[!grepl(stop_note, res$warnings)]
    seq_txt <- .kidpr_ty_sequence(m$table)
    notes <- c(notes, sprintf(
      "Combination not provided by the Field Guide of 2008: no type formula of the %s (%s) matches the horizons %s, so the type is unassigned (Hugo's decision Q15). Nearest formulas: %s.",
      lv$name[2L], div, if (nzchar(seq_txt)) seq_txt else "of the profile",
      paste(vapply(seq_len(nrow(near)), function(i)
        sprintf("%s %s", near$code[i], .kidpr_ty_describe(m$results[[near$code[i]]])), ""), collapse = "; ")))
    res$trace[["types_nearest"]] <- stats::setNames(lapply(seq_len(nrow(near)), function(i) {
      r <- m$results[[near$code[i]]]
      list(code = near$code[i], name = m$names[[near$code[i]]], passed = FALSE,
           formula = r$formula, distance = r$distance, missing_slots = r$missing, extra = r$extra,
           missing = character(0), notes = .kidpr_ty_describe(r))
    }), near$code)
  }
  if (length(notes)) res$warnings <- c(res$warnings, notes)
  res
}
