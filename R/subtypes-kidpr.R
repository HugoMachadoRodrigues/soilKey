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
# (BF, BH) and the mark (~~, ").
.kidpr_st_formula_tokens <- function(formula) {
  if (is.null(formula) || is.na(formula) || !nzchar(formula)) return(list())
  key <- paste0("st_formula:", formula)
  hit <- .kidpr_cache[[key]]
  if (!is.null(hit)) return(hit)
  toks <- .kidpr_split_outside(as.character(formula), "-")
  toks <- unlist(lapply(toks, function(t) strsplit(trimws(t), "\\s+")[[1L]]))
  out <- lapply(toks, function(t) {
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
  parts <- tryCatch(.kidpr_ft_parts(f, e[["host"]], e$place), error = function(err) list())
  lapply(parts, function(p) list(code = p$code, hosts = if (is.null(p$host)) NULL else .kidpr_ft_hosts(p$host)))
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
  written <- function(code, hosts) {
    for (t in toks) {
      on_host <- is.null(hosts) || identical(hosts, "any") || t$index %in% hosts
      if (code %in% c("BF", "BH")) {
        if (identical(t$modification, code)) return(TRUE)
        next
      }
      if (code == "TT~~" && identical(t$index, "TT") && isTRUE(grepl("~", t$mark))) return(TRUE)
      if (code == "TT\"" && identical(t$index, "TT") && isTRUE(grepl("\"", t$mark))) return(TRUE)
      if (on_host && code %in% t$features) return(TRUE)
      full <- c(t$index, if (!is.na(t$rockflag)) t$rockflag)
      if (is.null(hosts) && any(vapply(full, function(h) identical(as.character(in_full[[h]] %||% ""), code), NA))) return(TRUE)
    }
    FALSE
  }
  all(vapply(parts, function(p) written(p$code, p$hosts), NA))
}


# Is an entry a feature of the whole profile (u, ro, the permafrost sign,
# r, rh, ur, rr, rx, x, TT~~, TT")?
.kidpr_st_profile_feature <- function(e) {
  if (!is.null(e$test)) return(TRUE)
  parts <- tryCatch(.kidpr_ft_parts(as.character(e$feature %||% e$index), e[["host"]], e$place), error = function(err) list())
  meta <- .kidpr_ft_meta()
  length(parts) > 0L && all(vapply(parts, function(p) identical(meta[[p$code]]$place, "profile"), NA))
}

# The context of the soil's own horizons: a buried soil under them (layers
# in brackets, the FAO suffix b) is left out, so that the gleyic horizon of a
# soil buried under a stratozem does not name the stratozem gleyic (the
# book names the buried soil after the stratozem, p. 128). A profile whose
# every layer is buried (a soil under a deposit) keeps them all.
.kidpr_st_own_context <- function(ctx) {
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


# --- the tests of their own --------------------------------------------------------

# [BHF-BAN]>3, the layered-ochric volcanic soils: more than three elementary
# profiles in the ash (p. 127), read as more than three runs of BHF or BAN
# horizons separated by other layers (soilKey's reading).
.kidpr_st_layered_ochric <- function(ctx) {
  H <- c("BHF", "BAN")
  m <- ctx$main
  if (!length(m)) return(list(passed = FALSE, evidence = "no layers"))
  st <- vapply(m, function(j) .kidpr_ft_is_host(ctx, j, H), NA)
  runs <- function(v) { v <- v %in% TRUE; sum(v & !c(FALSE, utils::head(v, -1L))) }
  sure <- runs(st %in% TRUE)
  maybe <- runs(!(st %in% FALSE))
  ev <- sprintf("%d run(s) of BHF or BAN in the profile%s", sure,
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
.kidpr_add_subtypes <- function(res, pedon, rules) {
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
  ctx <- .kidpr_ft_context(pedon, div)
  keys <- vapply(entries, function(e) as.character(e$key), "")
  in_formula <- vapply(entries, function(e) .kidpr_st_in_formula(e, formula), NA)
  restricted <- vapply(entries, function(e) length(e$only_types) > 0L && !(type %in% unlist(e$only_types)), NA)
  # the dispatcher, once for every entry it reads
  run <- which(!in_formula & !restricted & vapply(entries, function(e) is.null(e$test), NA))
  disp <- lapply(entries[run], function(e) {
    hf <- as.character(e$feature %||% e$index)
    list(feature = hf, host = if (!is.null(e[["host"]])) as.character(unlist(e[["host"]])), place = e$place)
  })
  # a feature of the whole profile reads the whole profile; a feature of a
  # horizon reads the soil's own horizons, not a buried soil under them
  own <- .kidpr_st_own_context(ctx)
  prof <- vapply(entries[run], function(e) .kidpr_st_profile_feature(e), NA)
  fr <- NULL
  if (length(disp)) {
    fr <- vector("list", length(disp))
    if (any(prof)) {
      fp <- .kidpr_features(ctx, disp[prof], division = div)
      for (j in seq_len(nrow(fp))) fr[[which(prof)[j]]] <- fp[j, , drop = FALSE]
    }
    if (any(!prof)) {
      fo <- .kidpr_features(own, disp[!prof], division = div)
      for (j in seq_len(nrow(fo))) fr[[which(!prof)[j]]] <- fo[j, , drop = FALSE]
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
  }
  res$trace <- tr
  res
}
