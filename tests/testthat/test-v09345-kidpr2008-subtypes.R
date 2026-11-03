# v0.9.345 -- the subtypes of the Russian Field Guide of 2008 (the lists of
# chapter 4, one per division) and the full name of a soil: every subtype of
# the division's list that passes, in the order of the list, after the type;
# typical when all are FALSE; provisional when some are undecided; a feature
# the formula of the type writes not named again; a sentence of 3.2 that
# names a single type restricts its subtype to it (Hugo's decision Q16).

# A pedon in the Russian notation from tokens "DESIGNATION:thickness".
.sb_ped <- function(tokens, site = list(), ...) {
  d <- sub(":[0-9.]+$", "", tokens)
  th <- as.numeric(sub("^.*:", "", tokens))
  bot <- cumsum(th)
  h <- data.frame(designation = d, top_cm = c(0, bot[-length(bot)]), bottom_cm = bot, stringsAsFactors = FALSE)
  ex <- list(...)
  for (k in names(ex)) h[[k]] <- ex[[k]]
  PedonRecord$new(site = utils::modifyList(list(id = "ru-subtype", designation_system = "kidpr"), site),
                  horizons = ensure_horizon_schema(data.table::as.data.table(h)))
}
.sb_of <- function(tokens, ...) classify_kidpr2008(.sb_ped(tokens, ...), on_missing = "silent")
.sb_codes <- function(r) r$levels$code[r$levels$level == "subtype"]
.sb_ru <- function(r) r$qualifiers$name_native
.sb_type <- function(r) r$levels$code[r$levels$level == "types"]
.sb_entry <- function(d, key) Filter(function(e) identical(e$key, key), .kidpr_st_list(d))[[1L]]

# Undesignated layers for the data route (as in the tests of the types).
.sb_L <- function(top, bottom, ...) {
  d <- list(top_cm = top, bottom_cm = bottom, munsell_hue_dry = "10YR", munsell_value_dry = 6,
            munsell_chroma_dry = 4, structure_type = "massive", structure_grade = "structureless",
            clay_pct = 20, silt_pct = 40, sand_pct = 40, oc_pct = 0.1, ph_h2o = 6.0,
            clay_films_amount = "none", effervescence_class = "none", secondary_carbonates_form = "none")
  a <- list(...)
  d[names(a)] <- a
  d
}
.sb_P <- function(...) {
  ls <- list(...)
  nm <- unique(unlist(lapply(ls, names)))
  h <- as.data.frame(lapply(stats::setNames(nm, nm), function(k)
    unlist(lapply(ls, function(l) if (is.null(l[[k]])) NA else l[[k]]))), stringsAsFactors = FALSE)
  old <- options(soilKey.warn_unrecognised = FALSE)
  on.exit(options(old))
  PedonRecord$new(site = list(frost_regime = "none", land_use = "forest"), horizons = h)
}
.sb_AU <- function(top, bottom, ...) .sb_L(top, bottom, munsell_value_dry = 3, munsell_chroma_dry = 1,
                                           structure_type = "granular", structure_grade = "moderate",
                                           ph_h2o = 7.2, oc_pct = 4, clay_pct = 15, silt_pct = 50, sand_pct = 35, ...)
.sb_BCA <- function(top, bottom, ...) .sb_L(top, bottom, caco3_pct = 12, effervescence_class = "strong", ph_h2o = 8.2, ...)

.st_mk <- function(des, site = list()) {
  th <- ifelse(des %in% c("r", "rh", "ur", "rr", "rx", "pir"), 10, 20)
  bot <- cumsum(th); top <- bot - th
  h <- data.frame(designation = des, top_cm = top, bottom_cm = bot, stringsAsFactors = FALSE)
  PedonRecord$new(site = utils::modifyList(list(id = "st", designation_system = "kidpr"), site),
                  horizons = ensure_horizon_schema(data.table::as.data.table(h)))
}

# The tokens of a formula as a profile: the optional horizons left out, the
# first of alternatives, a buried soil [ABC] or {ABC} as [AY]-[BT].
.st_tokens <- function(formula) {
  t <- .kidpr_split_outside(formula, "-")
  t <- unlist(lapply(t, function(x) strsplit(trimws(x), "\\s+")[[1L]][1L]))
  t <- t[!grepl("^\\(.*\\)$", t)]
  t <- sub("\\(.*\\)$", "", t)
  t <- t[t != "{ABC}"]
  unlist(lapply(t, function(x) if (x == "[ABC]") c("[AY]", "[BT]") else x))
}

.st_index <- function(tok) {
  vapply(tok, function(s) {
    r <- tryCatch(.kidpr_parse_one(s), error = function(e) NULL)
    if (is.null(r)) NA_character_ else as.character(r$index %||% NA_character_)
  }, "", USE.NAMES = FALSE)
}

# Write feature letters on a token: BT + g -> BTg; BTg + y -> BTg,y.
.st_write <- function(tok, f) {
  if (f %in% c("[hh]", "[e-hf]", "[akl]")) return(paste0(tok, f))
  r <- .kidpr_parse_one(tok)
  if (length(r$features)) paste0(tok, ",", f) else paste0(tok, f)
}

# Where an entry is written: the canonical code of each part, its hosts and
# its place.
.st_plan <- function(e) {
  meta <- .kidpr_ft_meta()
  parts <- .kidpr_ft_parts(as.character(e$feature %||% e$index), .kidpr_st_host(e), e$place[1L])
  lapply(parts, function(p) {
    m <- meta[[p$code]]
    H <- if (is.null(p$host)) .kidpr_ft_hosts(m$hosts, m$not_on) else .kidpr_ft_hosts(p$host)
    list(code = p$code, H = H, place = p$place %||% m$place %||% "on")
  })
}

# Add a host the profile lacks: an organic horizon on top, rock and the
# hydrogenic horizons at the bottom, the rest before the last layer.
.st_add_host <- function(tok, H) {
  h <- H[1L]
  if (h %in% c("O", "T", "TO", "TE", "TJ", "H")) return(c(h, tok))
  # the parent rock: the loose rock C of the formula becomes D (i is read at
  # the top of the parent rock)
  if (h %in% c("C", "D") && .st_index(utils::tail(tok, 1L)) %in% c("C", "D")) return(c(utils::head(tok, -1L), h))
  if (h %in% c("C", "D", "G", "Q", "R")) return(c(tok, h))
  c(utils::head(tok, -1L), h, utils::tail(tok, 1L))
}

# Write an entry on a profile (a vector of tokens): the new tokens, or NULL.
.st_apply <- function(tok, e) {
  if (!is.null(e$test)) {
    if (!identical(e$test, "layered_ochric")) return(NULL)
    return(c(tok[1L], rep(c("BHF", "BAN", "C\""), 5L)))
  }
  for (p in .st_plan(e)) {
    code <- p$code
    ix <- .st_index(tok)
    if (identical(p$place, "profile")) {
      if (code %in% c("TT~~", "TT\"")) {
        k <- which(ix == "TT"); if (!length(k)) return(NULL)
        tok[k[1L]] <- paste0("TT", sub("^TT", "", code)); next
      }
      if (code %in% c("r", "rh", "ur", "rr", "rx")) { tok <- c(code, tok); next }
      if (code == "x") { tok[1L] <- paste0(tok[1L], "+X"); next }
      k <- max(which(!is.na(ix)))
      tok[k] <- .st_write(tok[k], code); next
    }
    H <- p$H
    j <- which(if (identical(H, "any")) !is.na(ix) else ix %in% H)
    if (!length(j)) {
      if (identical(H, "any")) return(NULL)
      tok <- .st_add_host(tok, H); ix <- .st_index(tok)
      j <- which(ix %in% H)
      if (!length(j)) return(NULL)
    }
    j <- j[1L]
    if (code %in% c("BF", "BH")) {
      if (!identical(ix[j], "BHF")) return(NULL)
      tok[j] <- sub("^BHF", code, tok[j]); next
    }
    if (p$place == "over" && j == 1L) { tok <- c(code, tok); next }
    k <- switch(p$place, under = j + 1L, over = j - 1L, j)
    if (k > length(tok)) tok <- c(tok, "C")
    tok[k] <- .st_write(tok[k], code)
  }
  tok
}

# Read the entries put on one profile as .kidpr_add_subtypes() reads them.
.st_read <- function(tok, d, ents, put) {
  site <- if (d == "CJ") list(wind_deflation = TRUE) else list()
  ctx <- .kidpr_ft_context(.st_mk(tok, site), d)
  ok <- stats::setNames(rep(NA, length(put)), put)
  # x is read by the whole classification: an X within 30 cm keys the soil
  # to the chemozems in trunk C after CU
  if (any(vapply(ents[put], function(e) identical(e$key, "x"), NA))) {
    # on the division's example, which keys to it, the X on its top layer
    p <- make_kidpr2008_example(d)
    h <- data.table::copy(p$horizons)
    h$designation[1L] <- paste0(h$designation[1L], "+X")
    p$horizons <- h
    r <- classify_kidpr2008(p, on_missing = "silent")
    ok[as.character(put)] <- "x" %in% r$levels$code[r$levels$level == "subtype"]
    return(ok)
  }
  run <- put[vapply(ents[put], function(e) is.null(e$test) && !identical(e$scale, "carbonate"), NA)]
  for (k in run) {
    e <- ents[[k]]
    ok[as.character(k)] <- .kidpr_st_places(ctx, list(feature = as.character(e$feature %||% e$index),
                                                     host = .kidpr_st_host(e), place = e$place), d)$passed
  }
  for (k in setdiff(put, run)) {
    e <- ents[[k]]
    if (!is.null(e$test)) { ok[as.character(k)] <- .kidpr_st_layered_ochric(ctx)$passed; next }
    lc_host <- unique(unlist(lapply(ents, function(x) if (grepl("lc-BCA", x$index, fixed = TRUE)) sub("lc-BCA.*$", "", x$index))))
    cf <- .kidpr_features(ctx, list(list(feature = "lc", host = lc_host), list(feature = "mc"),
                                              list(feature = "nc"), list(feature = "dc")), division = d)
    ok[as.character(k)] <- .kidpr_st_scale(e, ents, stats::setNames(as.list(cf$passed), c("lc", "mc", "nc", "dc")))
  }
  ok
}

# Every entry of every list: per type of the division, the entries its
# formula does not write and its type may take, several on one profile (one
# modification of BHF and one carbonate form per profile; a deposit at the
# surface and a test of their own on a profile of their own), in passes over
# the types until nothing more is reached. One row per entry: the type and
# the profile that named it (named TRUE), or the last tried.
.st_reach_all <- function(divisions = NULL) {
  rl <- load_rules("kidpr2008")
  L <- .kidpr_st_rules()$lists
  out <- list()
  for (d in divisions %||% names(L)) {
    ents <- L[[d]]
    types <- rl$types[[d]] %||% rl$types[[paste0("[", d, "]")]]
    todo <- seq_along(ents)
    res <- list()
    stuck <- -1L
    for (pass in 1:12) {
      before <- length(todo)
      if (pass %% 2L == 1L) { if (length(todo) == stuck) break; stuck <- length(todo) }
      for (t in types) {
        if (!length(todo)) break
        tok <- .st_tokens(t$formula)
        put <- integer(0); grps <- character(0)
        for (k in todo) {
          e <- ents[[k]]
          if (length(e$only_types) && !(t$code %in% unlist(e$only_types))) next
          if (.kidpr_st_in_formula(e, t$formula)) next
          grp <- if (identical(e$scale, "carbonate")) "carbonate" else if (e$index %in% c("BF", "BH")) "bhf"
                 else if (e$index %in% c("r", "rh", "ur", "rr", "rx")) "deposit" else if (!is.null(e$test)) "test"
                 else if (identical(e$key, "x")) "x" else "plain"
          alone <- grp %in% c("deposit", "test", "x")
          if (alone != (pass %% 2L == 0L)) next
          if (alone && length(put)) next
          if (grp != "plain" && grp %in% grps) next
          nt <- .st_apply(tok, e)
          if (is.null(nt)) next
          tok <- nt; put <- c(put, k); grps <- c(grps, grp)
        }
        if (!length(put)) next
        ok <- .st_read(tok, d, ents, put)
        for (k in put) res[[length(res) + 1L]] <- data.frame(
          division = d, key = ents[[k]]$key, type = t$code, profile = paste(tok, collapse = "-"),
          named = unname(ok[as.character(k)]), stringsAsFactors = FALSE)
        todo <- setdiff(todo, put[ok %in% TRUE])
      }
    }
    for (k in todo) if (!any(vapply(res, function(o) o$key == ents[[k]]$key, NA)))
      res[[length(res) + 1L]] <- data.frame(division = d, key = ents[[k]]$key, type = NA_character_,
                                            profile = NA_character_, named = NA, stringsAsFactors = FALSE)
    res <- do.call(rbind, res)
    keys <- vapply(ents, `[[`, "", "key")
    out[[d]] <- do.call(rbind, lapply(keys, function(kk) {
      o <- res[res$key == kk, , drop = FALSE]
      w <- which(o$named %in% TRUE)
      if (length(w)) o[w[1L], ] else o[nrow(o), ]
    }))
  }
  out <- do.call(rbind, out)
  rownames(out) <- NULL
  out
}


# --- the lists ------------------------------------------------------------------------

test_that("26 lists, 558 entries, in the order and with the counts of the book", {
  L <- .kidpr_st_rules()$lists
  counts <- c(AA = 15L, AB = 10L, BA = 22L, BB = 12L, BC = 14L, DA = 19L, CA = 35L, CB = 30L, CC = 16L,
              CD = 24L, CE = 16L, CF = 17L, CG = 15L, CH = 27L, CI = 27L, CJ = 18L, CK = 16L, CL = 11L,
              CM = 22L, CN = 31L, CO = 22L, CP = 23L, CQ = 30L, CR = 30L, CS = 35L, CT = 21L)
  expect_identical(lengths(L)[names(counts)], counts)
  expect_identical(sum(lengths(L)), 558L)
  expect_null(.kidpr_st_list("CU"))
  expect_identical(.kidpr_st_list("[DA]"), L$DA)
  tab <- .kidpr_st_table()
  expect_identical(nrow(tab), 558L)
  # one key per entry of a list, ASCII; the index and the name as printed
  for (d in names(L)) expect_false(anyDuplicated(tab$key[tab$division == d]) > 0L, info = d)
  expect_true(all(grepl("^[A-Za-z][A-Za-z0-9_]*$", tab$key)))
  expect_true(all(nzchar(tab$index) & nzchar(tab$name) & nzchar(tab$name_en) & nzchar(tab$hosts)))
  expect_true(all(grepl("^[\u0400-\u04ff\\[]", tab$name)))
  expect_false(any(grepl("[A-Za-z]", tab$name[tab$name != "[\u0442\u0438\u043f\u0438\u0447\u043d\u044b\u0435]"])))
  expect_true(all(tab$page >= 79L & tab$page <= 133L))
  # the book's list order: CA starts ao, mr, Th, ELf, [e-hf] and ends with
  # the universal subtypes; the permafrost sign, lost in the text, is read
  expect_identical(utils::head(tab$index[tab$division == "CA"], 5L), c("ao", "mr", "Th", "ELf", "[e-hf]"))
  expect_identical(utils::tail(tab$key[tab$division == "CA"], 8L), c("perm", "pb", "r", "rh", "ur", "rr", "rx", "x"))
  expect_identical(sum(tab$index == "\u22a5"), 10L)
  # the universal lists as printed: CO stops at ur, CQ and CR lack rh and pb
  expect_identical(tab$key[tab$division == "CO" & tab$universal], c("pb", "r", "rh", "ur"))
  expect_identical(tab$key[tab$division == "CQ" & tab$universal], c("r", "ur", "rr", "rx", "x"))
  # every entry is a feature, a host form or a test of its own, on hosts
  for (i in seq_len(nrow(tab))) {
    e <- L[[tab$division[i]]][[tab$position[i]]]
    if (!is.null(e$test)) next
    expect_true(length(.kidpr_ft_parts(as.character(e$feature %||% e$index), e[["host"]], e$place)) > 0L,
                info = paste(tab$division[i], tab$key[i]))
  }
  # the restrictions name types of their own division
  ty <- load_rules("kidpr2008")$types
  for (i in which(nzchar(tab$only_types))) {
    codes <- vapply(.kidpr_ty_list(ty, tab$division[i]), `[[`, "", "code")
    expect_true(all(strsplit(tab$only_types[i], ",")[[1L]] %in% codes), info = tab$key[i])
  }
  expect_setequal(paste(tab$division, tab$key)[nzchar(tab$only_types)],
                  c("AA Tote", "CJ akl", "CA crm", "CA ct", "CH cf", "CG ocr"))
  # CS prints BH and BF with each other's names: named as in 3.2 (Q12)
  cs <- L$CS
  bh <- Filter(function(e) e$key == "BH", cs)[[1L]]
  expect_identical(bh$name, "\u0418\u043b\u043b\u044e\u0432\u0438\u0430\u043b\u044c\u043d\u043e-\u0433\u0443\u043c\u0443\u0441\u043e\u0432\u044b\u0435")
  expect_identical(bh$printed, "\u0418\u043b\u043b\u044e\u0432\u0438\u0430\u043b\u044c\u043d\u043e-\u0436\u0435\u043b\u0435\u0437\u0438\u0441\u0442\u044b\u0435")
})


# --- the book's names -------------------------------------------------------------------

test_that("the book's own names: pp. 60, 67, 129 and 134", {
  skip_on_cran()
  # p. 60: a sod-pale-podzolic gleyic soil, AY-ELf-BEL-BTg-C
  r <- .sb_of(c("AY:15", "ELf:15", "BEL:15", "BTg:40", "C:30"))
  expect_identical(.sb_type(r), "[CA04]")
  expect_identical(.sb_codes(r), c("ELf", "g"))
  expect_identical(.sb_ru(r), paste("\u0414\u0435\u0440\u043d\u043e\u0432\u043e-\u043f\u043e\u0434\u0437\u043e\u043b\u0438\u0441\u0442\u044b\u0435",
                                    "\u043f\u0430\u043b\u0435\u0432\u043e\u043f\u043e\u0434\u0437\u043e\u043b\u0438\u0441\u0442\u044b\u0435,",
                                    "\u0433\u043b\u0435\u0435\u0432\u0430\u0442\u044b\u0435"))
  expect_identical(r$name, "Sod-podzolic pale-podzolic, gleyic")
  expect_length(r$provisional_because, 0L)
  # p. 60: a podzolic soil with a podzol microprofile, O-[e-hf]-EL-BEL-BT-C
  r <- .sb_of(c("O:5", "[e-hf]:1", "EL:14", "BEL:20", "BT:60", "C:50"))
  expect_identical(.sb_type(r), "[CA01]")
  expect_identical(.sb_ru(r), paste("\u041f\u043e\u0434\u0437\u043e\u043b\u0438\u0441\u0442\u044b\u0435",
                                    "\u0441 \u043c\u0438\u043a\u0440\u043e\u043f\u0440\u043e\u0444\u0438\u043b\u0435\u043c \u043f\u043e\u0434\u0437\u043e\u043b\u0430"))
  # p. 60: a podzolised podbur, O-e-BHF-C
  r <- .sb_of(c("O:5", "e:1", "BHF:34", "C:60"))
  expect_identical(.sb_type(r), "[CB01]")
  expect_identical(.sb_ru(r), "\u041f\u043e\u0434\u0431\u0443\u0440\u044b \u043e\u043f\u043e\u0434\u0437\u043e\u043b\u0435\u043d\u043d\u044b\u0435")
  expect_identical(r$name, "Podburs podzolised")
  # p. 67: a contact-bleached podzol, O-BHF-ek-D (with its E)
  ek <- "\u043a\u043e\u043d\u0442\u0430\u043a\u0442\u043d\u043e-\u043e\u0441\u0432\u0435\u0442\u043b\u0435\u043d\u043d\u044b\u0435"
  r <- .sb_of(c("O:5", "E:10", "BHF:25", "ek:4", "D:56"))
  expect_identical(.sb_type(r), "[CB07]")
  expect_identical(.sb_ru(r), paste("\u041f\u043e\u0434\u0437\u043e\u043b\u044b", ek))
  # p. 67: a contact-eluviated podzol, O-BHF-Del-D: the list of the
  # Al-Fe-humus soils has no el, so the contact-eluviated subtype is not
  # named (Hugo, 2026-11-02)
  r <- .sb_of(c("O:5", "E:10", "BHF:25", "ek:4", "Del:16", "D:40"))
  expect_identical(.sb_codes(r), "ek")
  expect_null(r$trace$subtype$el)
  # p. 129: a dark-humus water-accumulative stratozem on a grey gleyic
  # residual-carbonate soil: the stratozem and its subtype; the gleyic
  # horizon of the buried soil does not name the stratozem
  r <- .sb_of(c("RUaq:50", "[AY]:20", "[BTg]:50"))
  expect_identical(.sb_type(r), "[BC05]")
  expect_identical(.sb_codes(r), "aq")
  expect_identical(.sb_ru(r), paste("\u0421\u0442\u0440\u0430\u0442\u043e\u0437\u0435\u043c\u044b",
                                    "\u0442\u0435\u043c\u043d\u043e\u0433\u0443\u043c\u0443\u0441\u043e\u0432\u044b\u0435",
                                    "\u043d\u0430 \u043f\u043e\u0433\u0440\u0435\u0431\u0435\u043d\u043d\u043e\u0439 \u043f\u043e\u0447\u0432\u0435",
                                    "\u0432\u043e\u0434\u043d\u043e-\u0430\u043a\u043a\u0443\u043c\u0443\u043b\u044f\u0442\u0438\u0432\u043d\u044b\u0435"))
  # p. 134: the chemozem keeps the name built from its pollutants; CU has no
  # subtype list
  r <- classify_kidpr2008(make_kidpr2008_example("CU"), on_missing = "silent")
  expect_identical(r$levels$code, c("C", "CU", "[CU:CA04]"))
  expect_identical(r$name, "Chemozems polluted with Cu, Ni, Co; original soil: Sod-podzolic")
  expect_null(r$qualifiers$name_native)
  # Q16's own example: every feature that passes, in the order of the list
  r <- .sb_of(c("O:5", "EL:15", "BELy:15", "BTg:40", "C:30"))
  expect_identical(.sb_ru(r), paste("\u041f\u043e\u0434\u0437\u043e\u043b\u0438\u0441\u0442\u044b\u0435",
                                    "\u0433\u043b\u0435\u0435\u0432\u0430\u0442\u044b\u0435,",
                                    "\u044f\u0437\u044b\u043a\u043e\u0432\u0430\u0442\u044b\u0435"))
})


# --- typical, provisional, the order of the list -----------------------------------------

test_that("typical when every entry is FALSE; provisional, with the undecided listed, when some are NA", {
  skip_on_cran()
  r <- classify_kidpr2008(make_kidpr2008_example("CA"), on_missing = "silent")
  lv <- r$levels
  expect_identical(lv$level, c("trunks", "divisions", "types", "subtype"))
  expect_true(is.na(lv$code[4L]))
  expect_identical(lv$name[4L], "[typical]")
  expect_identical(lv$name_native[4L], "[\u0442\u0438\u043f\u0438\u0447\u043d\u044b\u0435]")
  expect_identical(r$name, "Sod-podzolic [typical]")
  expect_identical(.sb_ru(r), "\u0414\u0435\u0440\u043d\u043e\u0432\u043e-\u043f\u043e\u0434\u0437\u043e\u043b\u0438\u0441\u0442\u044b\u0435 [\u0442\u0438\u043f\u0438\u0447\u043d\u044b\u0435]")
  expect_length(r$provisional_because, 0L)
  expect_true(all(vapply(r$trace$subtype, function(e) e$status %in% c("not named", "in the formula"), NA)))
  # every entry is in the trace, with its index, host, route and evidence
  expect_identical(names(r$trace$subtype), vapply(.kidpr_st_list("CA"), `[[`, "", "key"))
  e <- r$trace$subtype$ELf
  expect_identical(e$index, "ELf")
  expect_identical(e$hosts, "EL")
  expect_identical(e$route, "notation")
  expect_true(nzchar(paste(e$evidence, collapse = "")))
  # without subtypes the classification stops at the type
  r0 <- classify_kidpr2008(make_kidpr2008_example("CA"), on_missing = "silent", subtypes = FALSE)
  expect_identical(r0$levels$level, c("trunks", "divisions", "types"))
  expect_identical(r0$name, "Sod-podzolic")
  # the data route: salts not measured leave the saline subtype undecided;
  # the pseudomycelium of the Bk names the soil cryogenic-mycelial
  p <- .sb_P(.sb_AU(0, 40, designation = "Ah"),
             .sb_BCA(40, 90, designation = "Bk", secondary_carbonates_form = "pseudomycelia"),
             .sb_L(90, 150, designation = "Ck", caco3_pct = 10))
  r <- classify_kidpr2008(p, on_missing = "silent")
  expect_identical(.sb_type(r), "[CI01]")
  expect_identical(.sb_codes(r), "mc")
  expect_identical(r$trace$subtype$mc$route, "data")
  expect_identical(r$provisional_because, "subtype:s")
  expect_true(any(grepl("^Provisional subtype: 1 listed feature\\(s\\) could not be decided \\(s\\)", r$warnings)))
  expect_true("soluble_salts_pct" %in% r$missing_data)
  pd <- .provisional_detail(r)
  expect_identical(pd$level, "subtype")
  expect_identical(pd$code, "s")
  expect_identical(pd$name, "saline")
  # a field observation not recorded is absent: no effervescence recorded in
  # the C is no residual carbonate (0.9.344), and the soil is typical
  ca4 <- function() .sb_P(.sb_L(0, 15, designation = "Ah", munsell_value_dry = 5, munsell_chroma_dry = 2,
                   structure_type = "granular", structure_grade = "moderate", ph_h2o = 5.0, oc_pct = 2,
                   clay_pct = 15, silt_pct = 50, sand_pct = 35),
             .sb_L(15, 35, designation = "E", munsell_value_dry = 7, munsell_chroma_dry = 2, structure_type = "platy",
                   structure_grade = "weak", clay_pct = 10, silt_pct = 50, sand_pct = 40),
             .sb_L(35, 90, designation = "Bt", munsell_hue_dry = "7.5YR", munsell_value_dry = 5, munsell_chroma_dry = 6,
                   structure_type = "blocky", structure_grade = "strong", clay_pct = 30, silt_pct = 40,
                   sand_pct = 30, clay_films_amount = "common"),
             .sb_L(90, 150, designation = "C", ph_h2o = NA, effervescence_class = NA))
  r <- classify_kidpr2008(ca4(), on_missing = "silent")
  expect_identical(.sb_type(r), "[CA04]")
  expect_identical(r$name, "Sod-podzolic [typical]")
  expect_identical(r$trace$subtype$ca$status, "not named")
  expect_length(r$provisional_because, 0L)
  # nothing passes and one is undecided: the name is the type's alone (no
  # frost regime recorded: the permafrost sign undecided, a measurement)
  p <- ca4()
  p$site$frost_regime <- NULL
  r <- classify_kidpr2008(p, on_missing = "silent")
  expect_identical(.sb_type(r), "[CA04]")
  expect_false("subtype" %in% r$levels$level)
  expect_identical(r$name, "Sod-podzolic")
  expect_identical(r$qualifiers$name_native, "\u0414\u0435\u0440\u043d\u043e\u0432\u043e-\u043f\u043e\u0434\u0437\u043e\u043b\u0438\u0441\u0442\u044b\u0435")
  expect_identical(r$provisional_because, "subtype:perm")
  expect_identical(r$trace$subtype$perm$status, "undecided")
  expect_true("frost_regime" %in% r$missing_data)
  # measured, the salts and the gypsum decide: nodules and gypsum named
  p <- .sb_P(.sb_AU(0, 40, designation = "Ah", soluble_salts_pct = 0.05, gypsum_visible_pct = 0),
             .sb_BCA(40, 90, designation = "Bk", secondary_carbonates_form = "soft concretions",
                     soluble_salts_pct = 0.05, gypsum_visible_pct = 2),
             .sb_L(90, 150, designation = "Ck", caco3_pct = 10, soluble_salts_pct = 0.05, gypsum_visible_pct = 0))
  r <- classify_kidpr2008(p, on_missing = "silent")
  expect_identical(.sb_codes(r), c("nc", "cs"))
  expect_length(r$provisional_because, 0L)
  expect_identical(r$name, "Chernozems segregational, gypsum-bearing")
})

test_that("the subtypes are named in the order of the list, whatever the order of the profile", {
  skip_on_cran()
  # tongues on the BEL, gleying in the BT: g (9th) before y (19th)
  r <- .sb_of(c("O:5", "EL:15", "BELy:15", "BTg:40", "C:30"))
  expect_identical(.sb_codes(r), c("g", "y"))
  # where the list has ELg, g is read on the layers other than EL (Hugo,
  # 2026-11-03): gleying in EL alone is ELg alone, in the BT too both
  r <- .sb_of(c("O:5", "ELg:15", "BEL:15", "BT:40", "C:30"))
  expect_identical(.sb_codes(r), "ELg")
  r <- .sb_of(c("O:5", "ELg:15", "BELy:15", "BTg:40", "C:30"))
  expect_identical(.sb_codes(r), c("ELg", "g", "y"))
  pos <- match(.sb_codes(r), vapply(.kidpr_st_list("CA"), `[[`, "", "key"))
  expect_false(is.unsorted(pos))
  expect_identical(r$levels$name_native[r$levels$level == "subtype"],
                   c("\u0413\u043b\u0435\u0435\u043f\u043e\u0434\u0437\u043e\u043b\u0438\u0441\u0442\u044b\u0435",
                     "\u0413\u043b\u0435\u0435\u0432\u0430\u0442\u044b\u0435",
                     "\u042f\u0437\u044b\u043a\u043e\u0432\u0430\u0442\u044b\u0435"))
})


# --- the formula and the restrictions ---------------------------------------------------

test_that("a feature the formula of the type writes is not named again", {
  skip_on_cran()
  # g in the gley types (CA05 writes BELg, BTg and G)
  r <- .sb_of(c("AY:15", "EL:15", "BELg:15", "BTg:40", "G:30", "CG:30"))
  expect_identical(.sb_type(r), "[CA05]")
  expect_identical(r$trace$subtype$g$status, "in the formula")
  expect_true(isTRUE(r$trace$subtype$g$skipped))
  expect_true(is.na(.sb_codes(r)))
  # ELg in CA03, BF in CB05, s and cs in the BCAs,cs of the solonetzes
  r <- .sb_of(c("T:30", "ELg:15", "BELg:15", "BTg:40", "G:30", "CG:30"))
  expect_identical(.sb_type(r), "[CA03]")
  expect_identical(r$trace$subtype$ELg$status, "in the formula")
  r <- .sb_of(c("AY:15", "BF:30", "C:30"))
  expect_identical(.sb_type(r), "[CB05]")
  expect_identical(r$trace$subtype$BF$status, "in the formula")
  expect_identical(r$trace$subtype$BH$status, "not named")
  r <- classify_kidpr2008(make_kidpr2008_example("CK"), on_missing = "silent")
  expect_identical(c(r$trace$subtype$s$status, r$trace$subtype$cs$status), c("in the formula", "in the formula"))
  # the permafrost sign and @ in the cryozems (CR is the cryoturbated horizon)
  expect_true(.kidpr_st_in_formula(.sb_entry("CA", "perm"), "O-CR-C\u22a5"))
  expect_true(.kidpr_st_in_formula(.sb_entry("CB", "cryoturb"), "O-CR-C\u22a5"))
  # q in the quasigley types, fn by F, v by V, ml by ML; a host form on its host
  expect_true(.kidpr_st_in_formula(.sb_entry("CA", "q"), "AH-EL-BTq-BCAq-Q(s)-CQ(s)"))
  expect_true(.kidpr_st_in_formula(.sb_entry("CB", "fn"), "T-Eg-BHFg-F-G-CG"))
  expect_true(.kidpr_st_in_formula(.sb_entry("CI", "v"), "AU-V-C(ca)"))
  expect_true(.kidpr_st_in_formula(.sb_entry("BA", "ml"), "AU-ML-CG~~ (CQ~~)"))
  expect_false(.kidpr_st_in_formula(.sb_entry("CA", "ELg"), "AY-EL-BELg-BTg-G-CG"))
  expect_true(.kidpr_st_in_formula(.sb_entry("CA", "ELg"), "PU-ELg-BTg-G-CG"))
  expect_false(.kidpr_st_in_formula(.sb_entry("CA", "g"), "AY-EL-BEL-BT-C"))
  # a feature the formula does not write is named: g on the BT of CA04
  r <- .sb_of(c("AY:15", "EL:15", "BEL:15", "BTg:40", "C:30"))
  expect_identical(.sb_codes(r), "g")
})

test_that("a sentence of 3.2 that names a single type restricts its subtype; the list decides elsewhere", {
  skip_on_cran()
  # crm: the podzolic type only (p. 63)
  r <- .sb_of(c("AY:15", "EL:15", "crm:5", "BT:40", "C:30"))
  expect_identical(.sb_type(r), "[CA04]")
  expect_identical(r$trace$subtype$crm$status, "not named")
  expect_true(isTRUE(r$trace$subtype$crm$restricted))
  expect_match(r$trace$subtype$crm$notes, "[CA01] only", fixed = TRUE)
  r <- .sb_of(c("O:5", "EL:15", "crm:5", "BT:40", "C:30"))
  expect_identical(.sb_type(r), "[CA01]")
  expect_identical(.sb_codes(r), "crm")
  # ocr: the peaty cryozems (p. 69)
  expect_identical(.sb_codes(.sb_of(c("O:5", "CRocr:30", "C\u22a5:30"))), NA_character_)
  r <- .sb_of(c("T:30", "CRocr:30", "C\u22a5:30"))
  expect_identical(.sb_type(r), "[CG03]")
  expect_identical(.sb_codes(r), "ocr")
  # [akl]: the brown arid soils (p. 63)
  expect_identical(.sb_codes(.sb_of(c("AJ[akl]:10", "BCA:30", "Cca:30"))), NA_character_)
  r <- .sb_of(c("AJ[akl]:10", "BM:20", "BCA:30", "Cca:30"))
  expect_identical(.sb_type(r), "[CJ02]")
  expect_identical(.sb_codes(r), "akl")
  # cf in the gleyzems only in CH; in the cryozems the list decides (Q17)
  expect_identical(.sb_entry("CH", "cf")$only_types, "[CH01]")
  expect_null(.sb_entry("CG", "cf")$only_types)
})

test_that("the carbonate profile is one of a scale; the double el by place; the layered ochric soils", {
  skip_on_cran()
  ch <- function(r) .sb_codes(r)
  expect_identical(ch(.sb_of(c("AUlc:40", "BCAmc:40", "Cca:30"))), "AUlc_BCAmc")
  expect_identical(ch(.sb_of(c("AU:40", "BCAmc:40", "Cca:30"))), "mc")
  expect_identical(ch(.sb_of(c("AU:40", "BCAnc,mc:40", "Cca:30"))), "nc")
  expect_identical(ch(.sb_of(c("AUlc:40", "BCAnc:40", "Cca:30"))), "AUlc_BCAnc")
  expect_identical(ch(.sb_of(c("AU:40", "BCAdc:40", "Cca:30"))), "dc")
  # the double el of CI: in the AU, solodised; under it, eluviated
  expect_identical(ch(.sb_of(c("AUel:40", "BCA:40", "Cca:30"))), "AUel")
  expect_identical(ch(.sb_of(c("AU:40", "BCAel:40", "Cca:30"))), "el")
  # more than three elementary profiles buried in the ash, the surface one
  # not counted (p. 127): four are, three are not
  bur <- function(n) c("AO:8", "BH:10", "BAN:30", "C\":20", rep(c("[BHF]:10", "[BAN]:10", "C\":10"), n))
  r <- .sb_of(bur(4L))
  expect_identical(.sb_type(r), "[BB01]")
  expect_identical(ch(r), "BHF_BAN3")
  expect_true(is.na(ch(.sb_of(bur(3L)))))
  r <- .sb_of(c("AO:8", "BH:10", "BAN:30", "C\":20", "[AO]:5", "[BAN]:10", "C\":10"))
  expect_true(is.na(ch(r)))
})

test_that("overlays, deposits over a buried soil, hosts by analogy and optional features (review of 2026-11-03)", {
  skip_on_cran()
  # a V or an S laid over a host layer is v or s written on it
  r <- .sb_of(c("AU:40", "BCA+V:40", "Cca:30"))
  expect_identical(.sb_type(r), "[CI01]")
  expect_identical(.sb_codes(r), "v")
  expect_match(paste(r$trace$subtype$v$evidence, collapse = " "), "V laid over", fixed = TRUE)
  r <- .sb_of(c("AU:40", "BCA+S:40", "Cca:30"))
  expect_identical(.sb_codes(r), "s")
  # a soil keyed under a thin deposit keeps its buried horizons; the deposit
  # is rh (a stratified horizon, humified) or r (alluvium)
  r <- .sb_of(c("RU:20", "[AU]:30", "[BCAnc]:40", "[Cca]:30"))
  expect_identical(.sb_type(r), "[CI01]")
  expect_identical(.sb_codes(r), c("nc", "rh"))
  r <- .sb_of(c("C~~:20", "[AY]:15", "[EL]:15", "[BEL]:15", "[BTg]:30", "[C]:30"))
  expect_identical(.sb_type(r), "[CA04]")
  expect_identical(.sb_codes(r), c("g", "r"))
  # a deposit over 40 cm is no r or rh
  expect_true(is.na(.kidpr_st_rdeposit(.kidpr_ft_context(.sb_ped(c("RU:45", "[AU]:30", "[BCA]:40")), "CI"))$kind))
  # CG: crm on or over the CR; CO: ox and fn on the gleyed rock, ff on EL or D
  expect_identical(.sb_codes(.sb_of(c("O:5", "crm:5", "CR:30", "C\u22a5:30"))), "crm")
  expect_identical(.sb_codes(.sb_of(c("O:5", "CRcrm:30", "C\u22a5:30"))), "crm")
  expect_identical(.sb_codes(.sb_of(c("O:5", "EL:20", "DGox:40"))), "ox")
  expect_identical(.sb_codes(.sb_of(c("O:5", "EL:20", "DGfn:40"))), "fn")
  expect_identical(.sb_codes(.sb_of(c("O:5", "ELff:20", "D:40"))), "ff")
  # an optional (ca) of the formula does not write ca
  expect_false(.kidpr_st_in_formula(.sb_entry("CR", "ca"), "PB-BI-C(ca)"))
  expect_true(.kidpr_st_in_formula(.sb_entry("CR", "ca"), "PB-BCA-Cca"))
  r <- .sb_of(c("PB:25", "BIca:40", "C:30"))
  expect_identical(.sb_codes(r), "ca")
  # the formula of CB05 writes BF and takes either BHF: the trace says which
  r <- .sb_of(c("AY:15", "BF:30", "C:30"))
  expect_match(paste(r$trace$subtype$BF$notes, collapse = " "), "the BHF of this soil reads BF", fixed = TRUE)
  # the Russian full name and the formula are not printed as qualifiers
  m <- character(0)
  o <- utils::capture.output(m <- utils::capture.output(print(r), type = "message"))
  out <- paste(c(o, m), collapse = "\n")
  expect_match(out, "Qualifiers", fixed = TRUE)
  expect_false(grepl("AY-BF-C", out, fixed = TRUE))
})

test_that("no subtypes without a type; the app's card shows them as the Hungarian ones", {
  skip_on_cran()
  # a profile whose type is unassigned keeps its division and gets none
  r <- .sb_of(c("O:5", "BEL:15", "BT:40", "C:30"))
  expect_identical(r$levels$level[r$levels$level != "trunks"], "divisions")
  expect_false("subtype" %in% r$levels$level)
  expect_null(r$trace$subtype)
  # the card of the national systems lists the subtypes on one row
  for (p in c("shiny", "bslib")) skip_if_not_installed(p)
  d <- system.file("shiny", "classify_app_pro", package = "soilKey")
  if (!nzchar(d) || !dir.exists(d)) d <- file.path("inst", "shiny", "classify_app_pro")
  skip_if_not(dir.exists(file.path(d, "R")))
  env <- new.env(parent = globalenv())
  for (f in list.files(file.path(d, "R"), pattern = "[.]R$", full.names = TRUE)) sys.source(f, envir = env)
  pt <- system.file("i18n", "translations.yaml", package = "soilKey")
  if (!nzchar(pt) || !file.exists(pt)) pt <- file.path("inst", "i18n", "translations.yaml")
  env$.sk_i18n_env$cat <- yaml::read_yaml(pt)
  withr::local_options(soilKey.app_lang = "en")
  r <- .sb_of(c("AY:15", "ELf:15", "BEL:15", "BTg:40", "C:30"))
  html <- as.character(env$pro_national_card(r, "Russia (KiDPR)", edition = "2008"))
  expect_match(html, r$name, fixed = TRUE)
  expect_match(html, "Subtypes", fixed = TRUE)
  expect_match(html, "pale-podzolic, gleyic", fixed = TRUE)
})


# --- every entry ------------------------------------------------------------------------

test_that("every entry of every list is reachable on a profile of a type of its division", {
  skip_on_cran()
  out <- .st_reach_all()
  expect_identical(nrow(out), 558L)
  # the saline and gypsum-bearing subtypes of the alkaline soils: every type
  # of CK writes BCAs,cs, so they are never named (Q16); x, an X within 30
  # cm, keys a soil of trunk C to the chemozems before every division of
  # the trunk but the lithozems and the halomorphic soils
  miss <- paste(out$division, out$key)[!(out$named %in% TRUE)]
  L <- .kidpr_st_rules()$lists
  after_cu <- setdiff(grep("^C", names(L), value = TRUE), c("CP", "CL"))
  with_x <- after_cu[vapply(after_cu, function(d) "x" %in% vapply(L[[d]], `[[`, "", "key"), NA)]
  expect_setequal(miss, c("CK s", "CK cs", paste(with_x, "x")))
  expect_true(all(out$named[out$key == "x" & !(out$division %in% after_cu)] %in% TRUE))
  ty <- load_rules("kidpr2008")$types
  for (k in which(out$division == "CK" & out$key %in% c("s", "cs")))
    expect_true(all(vapply(.kidpr_ty_list(ty, "CK"), function(t)
      .kidpr_st_in_formula(.sb_entry("CK", out$key[k]), t$formula), NA)))
})

test_that("the 27 examples and the canonical fixtures are classified quickly, with their full names", {
  skip_on_cran()
  divs <- eval(formals(make_kidpr2008_example)$division)
  t0 <- proc.time()[["elapsed"]]
  rs <- lapply(divs, function(d) classify_kidpr2008(make_kidpr2008_example(d), on_missing = "silent"))
  t1 <- proc.time()[["elapsed"]] - t0
  expect_lt(t1, 60)
  for (k in seq_along(divs)) {
    r <- rs[[k]]
    has_type <- "types" %in% r$levels$level
    if (divs[k] == "CU" || !has_type) expect_false("subtype" %in% r$levels$level, info = divs[k])
    else {
      expect_true("subtype" %in% r$levels$level, info = divs[k])
      expect_true(startsWith(r$qualifiers$name_native, r$levels$name_native[r$levels$level == "types"]), info = divs[k])
    }
  }
  fx <- grep("^make_.*_canonical$", ls(asNamespace("soilKey")), value = TRUE)
  t0 <- proc.time()[["elapsed"]]
  for (f in fx) expect_s3_class(classify_kidpr2008(get(f)(), on_missing = "silent"), "ClassificationResult")
  expect_lt(proc.time()[["elapsed"]] - t0, 120)
})
