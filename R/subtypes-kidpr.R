# =============================================================================
# v0.9.345 -- the subtypes of the Russian soil classification (Field Guide of
# 2008, the lists of chapter 4) and the full name of a soil.
#
# Each division has one list of subtypes (26 lists, 558 entries; the
# chemozems CU have none), each the index of a genetic feature (section
# 3.2), sometimes with the horizon it is on (ELf, AUel, Pw ...). A soil is
# named by its type and every subtype of its division's list that passes,
# in the order of the list, comma-separated and in the plural as printed:
# "Podzolistye gleevatye, yazykovatye" (Hugo's decision Q16, 2026-11-02).
# The typical subtype, which the lists leave out (p. 76), is soilKey's
# "[tipichnye]" when every entry is FALSE; when some are undecided the name
# is provisional and the undecided entries are listed. A feature the formula
# of the type already writes (g in the gley types, q in the quasigley ones,
# ELg in CA03, BF in CB05, s and cs in the BCAs,cs of the solonetzes, the
# permafrost sign and @ in the cryozems) is not named again. The list
# decides where a feature applies; a sentence of 3.2 restricts it only when
# it names a single type ([akl] in the brown arid soils, ocr in the peaty
# cryozems). The subtypes qualify a type: a soil whose type is unassigned
# gets none.
#
# The lists are in inst/rules/kidpr2008/subtypes.yaml; the tests are the
# feature tests of R/diagnostics-kidpr-features.R, run through
# .kidpr_features(); the level is the generic additive level of
# R/key-common.R (.add_additive_level()), as the Hungarian subtypes.
# =============================================================================

#' The subtype lists (inst/rules/kidpr2008/subtypes.yaml), cached.
#' @noRd
.kidpr_st_rules <- function() {
  if (is.null(.kidpr_cache$st_rules)) {
    f <- system.file("rules", "kidpr2008", "subtypes.yaml", package = "soilKey")
    if (!nzchar(f)) rlang::abort("The subtype lists of the Russian classification are not installed.")
    .kidpr_cache$st_rules <- yaml::read_yaml(f)
  }
  .kidpr_cache$st_rules
}

#' The subtype list of a division: a list of entries in the order of the
#' book (NULL for a division without one, the chemozems CU).
#' @noRd
.kidpr_st_list <- function(division) {
  d <- gsub("[][]", "", as.character(division)[1L])
  .kidpr_st_rules()$lists[[d]]
}

#' The subtype lists as a table: one row per entry (division, position,
#' key, index, name, name_en, hosts, page, only_types, notes).
#' @noRd
.kidpr_st_table <- function() {
  ls <- .kidpr_st_rules()$lists
  rows <- lapply(names(ls), function(d) {
    e <- ls[[d]]
    data.frame(division = d, position = seq_along(e),
               key = vapply(e, function(x) as.character(x$key), ""),
               index = vapply(e, function(x) as.character(x$index), ""),
               name = vapply(e, function(x) as.character(x$name), ""),
               name_en = vapply(e, function(x) as.character(x$name_en), ""),
               hosts = vapply(e, function(x) paste(unlist(x$hosts), collapse = ","), ""),
               page = vapply(e, function(x) as.integer(x$page), 0L),
               only_types = vapply(e, function(x) paste(unlist(x$only_types), collapse = ","), ""),
               universal = vapply(e, function(x) isTRUE(x$universal), NA),
               notes = vapply(e, function(x) as.character(x$notes %||% ""), ""),
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}


# --- the formula of the type ------------------------------------------------------

# The horizons of a formula as written: one row per token (a token "CG~~
# (CQ~~)" read as both), with its index, the rock flag of a gleyed or
# quasigleyed rock (CG, CQ), the features written on it, the modification
# (BF, BH) and the mark (~~, "). A feature written in parentheses is
# optional (C(ca), Q(s)): the formula does not write it, so it is left out.
.kidpr_st_formula_tokens <- function(formula) {
  if (is.null(formula) || is.na(formula) || !nzchar(formula)) return(list())
  key <- paste0("st_formula:", formula)
  hit <- .kidpr_cache[[key]]
  if (!is.null(hit)) return(hit)
  toks <- .kidpr_split_outside(as.character(formula), "-")
  toks <- unlist(lapply(toks, function(t) strsplit(trimws(t), "\\s+")[[1L]]))
  out <- lapply(toks, function(t) {
    t <- gsub("\\([a-z,]+\\)", "", t)
    r <- tryCatch(.kidpr_parse_one(t), error = function(e) NULL)
    if (is.null(r) || is.na(r$index %||% NA)) return(NULL)
    list(index = as.character(r$index), rockflag = as.character(r$rockflag %||% NA_character_),
         features = as.character(unlist(r$features)), modification = as.character(r$modification %||% NA_character_),
         mark = as.character(r$mark %||% NA_character_))
  })
  out <- Filter(Negate(is.null), out)
  assign(key, out, envir = .kidpr_cache)
  out
}

# The canonical feature codes and the hosts an entry reads: the feature, or
# the parts of a host form (ELg: g on EL), with the hosts of the entry where
# the list narrows them.
.kidpr_st_parts <- function(e) {
  f <- as.character(e$feature %||% e$index)
  if (!is.null(e$test)) return(list())
  parts <- tryCatch(.kidpr_ft_parts(f, .kidpr_st_host(e), e$place), error = function(err) list())
  voc <- .kidpr_vocabulary()
  alias_code <- function(x) if (x %in% names(voc$feature_aliases)) unname(voc$feature_aliases[[x]]) else x
  plain <- is.null(e[["host"]]) && length(parts) == 1L && identical(parts[[1L]]$code, alias_code(f))
  lapply(parts, function(p) list(code = p$code, hosts = if (is.null(p$host)) NULL else .kidpr_ft_hosts(p$host),
                                 full = plain))
}

# Is the feature of an entry written in the formula of the type: on one of
# its hosts (any horizon for a feature with no narrowed host), BF and BH as
# the modification of BHF, TT~~ and TT" as the marks on TT, or, for a
# feature with no narrowed host, by the horizon that is the feature in full
# (subtypes.yaml, in_full: G of g, CR of @ ...), a gleyed or quasigleyed
# rock (CG, CQ) counting as G or Q. ELg (g on EL) is written only by an ELg.
.kidpr_st_in_formula <- function(e, formula) {
  toks <- .kidpr_st_formula_tokens(formula)
  if (!length(toks)) return(FALSE)
  parts <- .kidpr_st_parts(e)
  if (!length(parts)) return(FALSE)
  in_full <- .kidpr_st_rules()$in_full %||% list()
  written <- function(code, hosts, full) {
    for (t in toks) {
      on_host <- is.null(hosts) || identical(hosts, "any") || t$index %in% hosts
      if (code %in% c("BF", "BH")) {
        if (identical(t$modification, code)) return(TRUE)
        next
      }
      if (code == "TT~~" && identical(t$index, "TT") && isTRUE(grepl("~", t$mark))) return(TRUE)
      if (code == "TT\"" && identical(t$index, "TT") && isTRUE(grepl("\"", t$mark))) return(TRUE)
      if (on_host && code %in% t$features) return(TRUE)
      whole <- c(t$index, if (!is.na(t$rockflag)) t$rockflag)
      if (isTRUE(full) && any(vapply(whole, function(h) identical(as.character(in_full[[h]] %||% ""), code), NA))) return(TRUE)
    }
    FALSE
  }
  all(vapply(parts, function(p) written(p$code, p$hosts, p$full), NA))
}


# Is an entry a feature of the whole profile (u, ro, the permafrost sign,
# r, rh, ur, rr, rx, x, TT~~, TT")?
.kidpr_st_profile_feature <- function(e) {
  if (!is.null(e$test)) return(TRUE)
  parts <- tryCatch(.kidpr_ft_parts(as.character(e$feature %||% e$index), e[["host"]], e$place), error = function(err) list())
  meta <- .kidpr_ft_meta()
  length(parts) > 0L && all(vapply(parts, function(p) identical(meta[[p$code]]$place, "profile"), NA))
}

# The context of the soil's own horizons: where the type is keyed on the
# layers over a buried soil (a formula that writes the buried soil as [ABC]
# or {ABC}: the stratozems on a buried soil, the secondary solonchaks, the
# volcanic soils), the buried soil under them (layers in brackets, the FAO
# suffix b) is left out, so that the gleyic horizon of a soil buried under a
# stratozem does not name the stratozem gleyic (the book names the buried
# soil after the stratozem, p. 128). Otherwise the type was keyed on the
# buried soil itself (a soil under a deposit of 40 cm or less) and every
# layer stays.
.kidpr_st_own_context <- function(ctx, formula = NA_character_) {
  if (is.na(formula) || !grepl("[[{]ABC[]}]", formula)) return(ctx)
  bur <- vapply(ctx$main, function(j) isTRUE(.kidpr_buried_des(ctx$x, j)), NA)
  if (!any(bur) || all(bur)) return(ctx)
  first <- which(!bur)[1L]
  drop <- ctx$main[bur & seq_along(bur) > first]
  if (!length(drop)) return(ctx)
  cut <- min(match(drop, ctx$ord))
  own <- list2env(mget(ls(ctx, all.names = TRUE), envir = ctx), parent = emptyenv())
  class(own) <- "kidpr_ft_context"
  own$ord <- ctx$ord[seq_len(cut - 1L)]
  own$main <- own$ord[!own$small[own$ord]]
  own$memo <- new.env(parent = emptyenv())
  own
}


# The hosts an entry passes to the dispatcher: the list's own (`host`), or
# the feature's less the horizons the list names apart (`except`: where the
# list has ELg, g is read on the layers other than EL; Hugo, 2026-11-03);
# NULL for the hosts of the feature.
.kidpr_st_host <- function(e) {
  if (!is.null(e[["host"]])) return(as.character(unlist(e[["host"]])))
  if (is.null(e$except)) return(NULL)
  m <- .kidpr_ft_meta()[[as.character(e$feature %||% e$index)]]
  setdiff(.kidpr_ft_hosts(m$hosts, m$not_on), as.character(unlist(e$except)))
}

# An entry read on the dispatcher at each of its places (CG's crm on or over
# CR), the verdicts joined: named where it holds at one of them.
.kidpr_st_places <- function(ctx, d, div) {
  pl <- as.character(unlist(d$place))
  if (length(pl) <= 1L) return(.kidpr_features(ctx, list(d), division = div))
  rs <- .kidpr_features(ctx, lapply(pl, function(x) utils::modifyList(d, list(place = x))), division = div)
  p <- .kidpr_or(rs$passed)
  use <- if (isTRUE(p)) which(rs$passed %in% TRUE) else if (is.na(p)) which(is.na(rs$passed)) else seq_len(nrow(rs))
  out <- rs[use[1L], , drop = FALSE]
  out$passed <- p
  out$place <- paste(pl, collapse = "|")
  out$layers <- I(list(sort(unique(unlist(rs$layers[use])))))
  out$evidence <- .kidpr_evidence(unlist(rs$evidence[use]))
  out$missing <- I(list(unique(unlist(rs$missing[use]))))
  out
}

# A V or an S laid over a layer of the Russian notation (BCA+V, AU+S) is the
# feature v or s written on it: the vertic or saline subtype.
.kidpr_st_overlay <- function(ctx, code) {
  x <- ctx$x
  if (!isTRUE(x$kidpr)) return(list(passed = FALSE))
  ov <- toupper(code)
  hit <- Filter(function(j) isTRUE(x$des$known[j]) && ov %in% (x$des$overlay[[j]] %||% character(0)), ctx$ord)
  if (!length(hit)) return(list(passed = FALSE))
  hit <- unlist(hit)
  list(passed = TRUE, layers = hit, note = sprintf("%s laid over %s: the feature %s written on it", ov,
                                                   paste(vapply(hit, function(j) .kidpr_ft_lab(ctx, j), ""), collapse = ", "), code))
}

# A deposit of 5-40 cm at the surface written as a horizon, over a buried
# humus or organic horizon (in brackets): a stratified horizon RU, RY or RJ
# (humified) is the feature rh, alluvium C~~ (not humified) the feature r,
# as key 2.1 reads a deposit of 40 cm or less (positions 35 and 38).
.kidpr_st_rdeposit <- function(ctx) {
  x <- ctx$x
  none <- list(kind = NA_character_, layers = integer(0), note = character(0))
  if (!isTRUE(x$kidpr) || !length(ctx$main)) return(none)
  m <- ctx$main
  kind_of <- function(j) {
    if (!isTRUE(x$des$known[j]) || isTRUE(.kidpr_buried_des(x, j))) return(NA_character_)
    ix <- x$des$index[j]
    if (ix %in% c("RU", "RY", "RJ")) "rh" else if (identical(ix, "C") && isTRUE(grepl("~", x$des$mark[j]))) "r" else NA_character_
  }
  k1 <- kind_of(m[1L])
  if (is.na(k1)) return(none)
  n <- 1L
  while (n < length(m) && identical(kind_of(m[n + 1L]), k1)) n <- n + 1L
  rows <- m[seq_len(n)]
  th <- x$bot[rows[n]] - x$top[rows[1L]]
  nx <- if (n < length(m)) m[n + 1L] else NA_integer_
  if (is.na(th) || th < 5 || th > 40 || is.na(nx) || !isTRUE(.kidpr_buried_des(x, nx)) ||
      !isTRUE(.kidpr_ft_buried_humus(ctx, nx))) return(none)
  list(kind = k1, layers = rows, note = sprintf("%s cm of %s over a buried horizon (%s): the feature %s (key 2.1, positions %s)",
                                                format(th), if (k1 == "rh") "humified stratified material" else "alluvium",
                                                .kidpr_ft_lab(ctx, nx), k1, if (k1 == "rh") "34-35" else "34, 38"))
}


# --- the tests of their own --------------------------------------------------------

# [BHF-BAN]>3, the layered-ochric volcanic soils: more than three elementary
# profiles buried in the ash (p. 127), read as more than three runs of BHF or
# BAN horizons separated by other layers, under the run of the surface soil
# (Hugo, 2026-11-02 and the review of 2026-11-03).
.kidpr_st_layered_ochric <- function(ctx) {
  H <- c("BHF", "BAN")
  m <- ctx$main
  if (!length(m)) return(list(passed = FALSE, evidence = "no layers"))
  st <- vapply(m, function(j) .kidpr_ft_is_host(ctx, j, H), NA)
  runs <- function(v) { v <- v %in% TRUE; sum(v & !c(FALSE, utils::head(v, -1L))) }
  # the first run is the surface soil's elementary profile
  sure <- max(runs(st %in% TRUE) - 1L, 0L)
  maybe <- max(runs(!(st %in% FALSE)) - 1L, 0L)
  ev <- sprintf("%d buried run(s) of BHF or BAN under the surface soil%s", sure,
                if (maybe > sure) sprintf(", %d with the undecided layers", maybe) else "")
  if (sure > 3L) return(list(passed = TRUE, evidence = ev))
  if (maybe <= 3L) return(list(passed = FALSE, evidence = ev))
  list(passed = NA, evidence = ev, missing = unique(unlist(ctx$fr$missing[m[is.na(st)]])))
}


# --- the scale of the carbonate profile -----------------------------------------------

# The subtypes of the carbonate profile are one of a scale (default 19 of the
# questions): the nodules (nc) prevail over the pseudomycelium (mc), and
# both over the diffuse carbonates (dc); a list that has the form with the
# migrating carbonates of the humus horizon (AUlc-BCAmc, AUlc-BCAnc,
# AJlc-BCAnc) names it when lc holds, and the form alone only when lc does
# not. `v` holds the verdicts of lc (on the humus horizon of the list's
# compound forms), mc, nc and dc.
.kidpr_st_scale <- function(e, entries, v) {
  idx <- as.character(e$index)
  form <- sub("^.*BCA", "", idx)
  compound <- grepl("lc-BCA", idx, fixed = TRUE)
  forms <- vapply(entries, function(x) if (identical(x$scale, "carbonate")) sub("^.*BCA", "", as.character(x$index)) else "", "")
  comps <- vapply(entries, function(x) identical(x$scale, "carbonate") && grepl("lc-BCA", as.character(x$index), fixed = TRUE), NA)
  higher <- switch(form, nc = character(0), mc = "nc", dc = c("nc", "mc"), character(0))
  p <- v[[form]]
  for (h in higher) p <- .kidpr_and(p, .kidpr_not(v[[h]]))
  if (compound) p <- .kidpr_and(p, v$lc)
  else if (any(comps & forms == form)) p <- .kidpr_and(p, .kidpr_not(v$lc))
  p
}


# --- the level ----------------------------------------------------------------------

# The first letter of a name in lower case (the subtypes follow the type).
.kidpr_st_lower <- function(x) paste0(tolower(substr(x, 1L, 1L)), substr(x, 2L, nchar(x)))

# Add the subtypes to a classify_kidpr2008() result: the division's list run
# through the feature tests (one context for the whole list), the features
# the type's formula writes left out, the single-type restrictions of 3.2,
# the scale of the carbonate profile; then the generic additive level
# (level "subtype", as the Hungarian subtypes), the Russian names on the
# rows, and the full name in Russian in qualifiers$name_native.
.kidpr_add_subtypes <- function(res, pedon, rules, ctx = NULL) {
  lv <- res$levels
  if (is.null(lv) || nrow(lv) < 3L || isTRUE(.kidpr_dv_state$masking)) return(res)
  at <- which(lv$level == "types")
  if (!length(at) || is.na(lv$code[at[1L]])) return(res)
  div <- gsub("[][]", "", lv$code[lv$level == "divisions"][1L])
  entries <- .kidpr_st_list(div)
  if (!length(entries)) return(res)
  type <- lv$code[at[1L]]
  te <- Filter(function(t) identical(as.character(t$code), type), .kidpr_ty_list(rules$types %||% list(), div))
  formula <- if (length(te)) as.character(te[[1L]]$formula) else NA_character_
  ctx <- ctx %||% .kidpr_ft_context(pedon, div)
  keys <- vapply(entries, function(e) as.character(e$key), "")
  in_formula <- vapply(entries, function(e) .kidpr_st_in_formula(e, formula), NA)
  restricted <- vapply(entries, function(e) length(e$only_types) > 0L && !(type %in% unlist(e$only_types)), NA)
  # the dispatcher, once for every entry it reads
  run <- which(!in_formula & !restricted & vapply(entries, function(e) is.null(e$test), NA))
  disp <- lapply(entries[run], function(e)
    list(feature = as.character(e$feature %||% e$index), host = .kidpr_st_host(e), place = e$place))
  # a feature of the whole profile reads the whole profile; a feature of a
  # horizon reads the soil's own horizons, not a buried soil under them
  own <- .kidpr_st_own_context(ctx, formula)
  prof <- vapply(entries[run], function(e) .kidpr_st_profile_feature(e), NA)
  fr <- NULL
  if (length(disp)) {
    fr <- vector("list", length(disp))
    if (any(prof)) {
      fp <- .kidpr_features(ctx, disp[prof], division = div)
      for (j in seq_len(nrow(fp))) fr[[which(prof)[j]]] <- fp[j, , drop = FALSE]
    }
    if (any(!prof)) {
      for (j in which(!prof)) fr[[j]] <- .kidpr_st_places(own, disp[[j]], div)
    }
    fr <- do.call(rbind, fr)
  }
  # the carbonate scale: lc, mc, nc, dc
  scale <- vapply(entries, function(e) identical(e$scale, "carbonate"), NA)
  cv <- NULL
  if (any(scale[!in_formula & !restricted])) {
    lc_host <- unique(unlist(lapply(entries[scale], function(e) {
      i <- as.character(e$index)
      if (grepl("lc-BCA", i, fixed = TRUE)) sub("lc-BCA.*$", "", i)
    })))
    ce <- list(list(feature = "lc", host = if (length(lc_host)) lc_host), list(feature = "mc"),
               list(feature = "nc"), list(feature = "dc"))
    cf <- .kidpr_features(own, ce, division = div)
    cv <- stats::setNames(as.list(cf$passed), c("lc", "mc", "nc", "dc"))
    cev <- stats::setNames(cf$evidence, c("lc", "mc", "nc", "dc"))
    cmiss <- stats::setNames(cf$missing, c("lc", "mc", "nc", "dc"))
  }
  # what the dispatcher does not read: a V or S laid over a host layer (v,
  # s), a stratified horizon or alluvium of 5-40 cm over a buried humus
  # horizon (rh, r); and the kind of BHF the soil has, where the formula
  # writes BF or BH
  ovl <- list()
  for (cd in c("v", "s")) ovl[[cd]] <- .kidpr_st_overlay(own, cd)
  dep <- .kidpr_st_rdeposit(ctx)
  bhf <- NULL
  if (any(in_formula & vapply(entries, function(e) e$index %in% c("BF", "BH"), NA))) {
    bk <- .kidpr_features(own, c("BF", "BH"), division = div)$passed
    bhf <- if (isTRUE(bk[1L])) "BF" else if (isTRUE(bk[2L])) "BH" else if (isFALSE(bk[1L]) && isFALSE(bk[2L])) "neither BF nor BH" else "undecided between BF and BH"
  }
  feats <- vector("list", length(entries))
  extra <- vector("list", length(entries))
  for (k in seq_along(entries)) {
    e <- entries[[k]]
    r <- list(passed = FALSE, evidence = NULL, missing = character(0), notes = character(0))
    route <- NA_character_; layers <- integer(0)
    if (restricted[k]) {
      r$notes <- sprintf("3.2 names this subtype in %s only; the type is %s", paste(unlist(e$only_types), collapse = ", "), type)
    } else if (!is.null(e$test)) {
      r <- switch(e$test, layered_ochric = .kidpr_st_layered_ochric(ctx),
                  list(passed = NA, notes = sprintf("no test '%s'", e$test)))
    } else if (!in_formula[k]) {
      j <- match(k, run)
      row <- fr[j, , drop = FALSE]
      p <- row$passed
      if (isTRUE(scale[k]) && !is.null(cv)) {
        p <- .kidpr_st_scale(e, entries, cv)
        used <- unique(c(sub("^.*BCA", "", e$index), "nc", "mc", if (grepl("lc-BCA", e$index, fixed = TRUE) ||
                                                                       any(vapply(entries, function(x) grepl("lc-BCA", x$index, fixed = TRUE), NA))) "lc"))
        used <- intersect(c("lc", "mc", "nc", "dc"), used)
        r$evidence <- unique(c(row$evidence, unlist(cev[used])))
        r$missing <- if (is.na(p)) unique(unlist(cmiss[used])) else character(0)
        r$notes <- "the subtypes of the carbonate profile are one of a scale: nodules over pseudomycelium over diffuse carbonates; the form with lc where lc holds"
      } else {
        r$evidence <- row$evidence
        r$missing <- unlist(row$missing)
      }
      code <- as.character(e$feature %||% e$index)
      if (code %in% names(ovl) && !isTRUE(p) && isTRUE(ovl[[code]]$passed)) {
        p <- TRUE
        r$evidence <- c(r$evidence, ovl[[code]]$note)
        row$layers <- list(ovl[[code]]$layers)
      }
      if (code %in% c("r", "rh") && !isTRUE(p) && identical(dep$kind, code)) {
        p <- TRUE
        r$evidence <- c(r$evidence, dep$note)
        row$layers <- list(dep$layers)
      }
      r$passed <- p
      route <- row$route
      layers <- unlist(row$layers)
    }
    feats[[k]] <- list(id = keys[k], name = as.character(e$name_en), passed = r)
    extra[[k]] <- list(index = as.character(e$index), name_native = as.character(e$name),
                       feature = as.character(e$feature %||% e$index),
                       hosts = as.character(unlist(e[["hosts"]])), place = e$place %||% NA_character_,
                       route = route, layers = layers, page = e$page,
                       restricted = restricted[k])
  }
  ty <- .kidpr_st_rules()$typical
  res <- .add_additive_level(res, pedon, feats, level = "subtype", after = "types",
                             already = keys[in_formula], typical = ty$name_en %||% "[typical]", typical_en = NA_character_,
                             context = list(division = div))
  # the Russian names on the rows, and the full name in Russian
  lv <- res$levels
  w <- which(lv$level == "subtype")
  if (length(w) && "name_native" %in% names(lv)) {
    lv$name_native[w] <- vapply(lv$code[w], function(cd) {
      if (is.na(cd)) return(as.character(ty$name %||% "[typical]"))
      as.character(entries[[match(cd, keys)]]$name)
    }, "")
    res$levels <- lv
  }
  type_ru <- as.character(lv$name_native[at[1L]] %||% lv$name[at[1L]])
  named <- if (length(w)) lv$code[w] else character(0)
  ru <- if (length(w)) {
    nm <- vapply(seq_along(w), function(i) {
      if (is.na(named[i])) as.character(ty$name) else .kidpr_st_lower(lv$name_native[w[i]])
    }, "")
    paste(type_ru, paste(nm, collapse = ", "))
  } else type_ru
  q <- res$qualifiers %||% list()
  q$subtype_native <- if (length(w)) lv$name_native[w] else character(0)
  q$name_native <- ru
  q$type_formula <- formula
  res$qualifiers <- q
  # the trace: the index, the Russian name, the host and the route of each
  tr <- res$trace
  for (k in seq_along(entries)) {
    id <- keys[k]
    if (is.null(tr$subtype[[id]])) next
    tr$subtype[[id]] <- c(tr$subtype[[id]], extra[[k]])
    if (restricted[k]) tr$subtype[[id]]$status <- "not named"
    if (in_formula[k] && !is.null(bhf) && entries[[k]]$index %in% c("BF", "BH"))
      tr$subtype[[id]]$notes <- c(tr$subtype[[id]]$notes, sprintf(
        "the type's formula writes %s and takes either kind of BHF (D23); the BHF of this soil reads %s",
        entries[[k]]$index, bhf))
  }
  res$trace <- tr
  res
}
