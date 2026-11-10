# =============================================================================
# soilKey -- offline benchmark of the FAO-Unesco legends (1974 and the Revised
# Legend of 1988) on the ISIS reference profiles (v0.9.392).
#
# ISRIC's ISIS gives most of its reference profiles one or two FAO-Unesco
# names (field faoClassifications): one of the Legend of 1974 (publication
# year 1974) and one of the Revised Legend (stored under the year of its
# ISRIC reprint, 1997), written as names in the singular, with a third-level
# subunit of ISRIC on many revised names and a phase on some
# ("[<subunit>-]<unit> <group>[ <phase>]"). soilKey keys each edition by its
# own text (classify_fao(edition = "1974" / "1988")); the label of each
# edition is compared with the class of the same edition.
#
# OFFLINE. This script downloads nothing and queries no service, and it does
# not change the ISIS reader (Hugo's decision Q27). It reads a cache of raw
# ISIS profiles (the list of profiles as the ISIS GraphQL server returns
# them, every field of .isis_profile_fields(), saved with saveRDS), from, in
# this order:
#   1. the first argument (or `cache =`);
#   2. the option soilKey.isis_cache;
#   3. the environment variable SOILKEY_ISIS_CACHE;
#   4. file.path(tools::R_user_dir("soilKey", "cache"), "isis", "profiles.rds").
# Run as a script with no cache found, it says so and exits cleanly; from R,
# fao_isis_benchmark() stops with that message. ISIS data are ISRIC's
# (educational and non-commercial use, source acknowledged: isis_citation());
# they are not shipped with soilKey.
#
# Usage:
#   Rscript fao_isis.R [cache.rds] [report.md]
# The markdown report is written only where the second argument says;
# without it the summary is printed. From R:
#   source(system.file("benchmarks", "fao_isis.R", package = "soilKey"))
#   b <- fao_isis_benchmark(cache = "profiles.rds", out = "report.md")
#
# Scoring. Each label is read by soilKey:::.fao_match_label() in the edition
# of its publication year (1974 -> fao1974; 1997 -> fao1988): exact names
# only, the singular read as the plural of the edition, the 1974 Nitosols
# and the 1988 Nitisols read in the other edition too (flagged), the ISRIC
# subunit cut off and reported apart (never scored: the legend defines no
# third level), the phases matched to the phases of the edition. A label not
# read is unmatched, never guessed; .fao_isis_variants is the explicit list
# of spelling variants tried only then (empty: every label of the cache of
# 2026-10-29 is read as it stands).
#   * Grouping: soilKey's major soil group(ing) equals the label's.
#   * Unit: soilKey's unit equals the label's, over all labels and among
#     those whose grouping agrees.
#   * Agreement carries a Wilson 95 % interval.
#   * Fall-through: soilKey names no grouping (no gate passed: the
#     candidates left undecided, or not classified; there is no residual
#     group, Hugo's decision Q5). It counts as a disagreement.
#   * Provisional: a class tested before the assigned one was left
#     undecided (provisional_because) at that level or above. Stand-in: a
#     test that passed for the assigned class rests on a stand-in (a USDA or
#     WRB reader for a test the FAO data leave undecided). "Firm" = neither.
#   * Phases: for each phase a label names, soilKey's status for it
#     (present, absent, undecided, not shown for the unit); and for each
#     phase soilKey finds present, how many of those labels name it.
# =============================================================================

# Spelling variants of the ISIS FAO labels (regular expression, case ignored
# = replacement), tried only when a label is not read as it stands. None is
# needed for the cache of 2026-10-29.
.fao_isis_variants <- character(0)

.fao_isis_wilson <- function(k, n, z = stats::qnorm(0.975)) {
  if (!n) return(c(NA_real_, NA_real_))
  p <- k / n
  den <- 1 + z^2 / n
  mid <- (p + z^2 / (2 * n)) / den
  half <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / den
  c(max(0, mid - half), min(1, mid + half))
}

fao_isis_cache <- function(cache = NULL, must = TRUE) {
  cands <- c(cache, getOption("soilKey.isis_cache", ""), Sys.getenv("SOILKEY_ISIS_CACHE", ""),
             file.path(tools::R_user_dir("soilKey", "cache"), "isis", "profiles.rds"))
  cands <- path.expand(as.character(cands[!is.na(cands) & nzchar(cands)]))
  hit <- cands[file.exists(cands)]
  if (!length(hit)) {
    msg <- paste0("No ISIS cache found (tried: ", paste(cands, collapse = ", "), "). ",
                  "Give the path of the saved ISIS profiles as the first argument, in the option ",
                  "soilKey.isis_cache or in SOILKEY_ISIS_CACHE; this script downloads nothing.")
    if (must) stop(msg, call. = FALSE)
    message(msg)
    return(NULL)
  }
  hit[1]
}

# TRUE when a stand-in decided a test of a trace entry (a nested list).
.fao_isis_stand_in <- function(x) {
  if (!is.list(x)) return(FALSE)
  s <- x[["stand_in"]]
  if (!is.null(s) && length(s) && !is.list(s) && any(nzchar(as.character(s)))) return(TRUE)
  any(vapply(x, .fao_isis_stand_in, NA))
}

# soilKey's grouping and unit of one result, with their status.
.fao_isis_result <- function(r) {
  na <- list(sk_group = NA_character_, sk_unit = NA_character_, fall_through = NA_character_,
             prov_group = NA, prov_unit = NA, standin_group = NA, standin_unit = NA, candidates = "")
  if (is.null(r)) return(na)
  lv <- r$levels
  pb <- as.character(r$provisional_because %||% character(0))
  out <- na
  out$prov_group <- any(startsWith(pb, "major_groups:"))
  out$prov_unit <- out$prov_group || any(startsWith(pb, "units:"))
  if (!is.data.frame(lv) || !nrow(lv)) {
    und <- sub("^major_groups:", "", grep("^major_groups:", pb, value = TRUE))
    out$fall_through <- if (length(und)) "undecided" else "not_classified"
    out$candidates <- paste(und, collapse = " ")
    out$standin_group <- out$standin_unit <- FALSE
    return(out)
  }
  g <- lv$code[lv$level == "major_groups"][1]
  u <- lv$code[lv$level == "units"][1]
  # the single-unit groups of 1974 (Lithosols, Rankers, Rendzinas) are
  # their own unit
  if (is.na(u) && identical(r$system_id, "fao1974") && g %in% c("I", "U", "E")) u <- g
  out$sk_group <- g
  out$sk_unit <- u
  out$fall_through <- "no"
  out$standin_group <- .fao_isis_stand_in(r$trace$major_groups[[g]]$evidence)
  out$standin_unit <- out$standin_group ||
    (!is.na(u) && .fao_isis_stand_in(r$trace$units[[u]]$evidence))
  out
}

fao_isis_benchmark <- function(cache = NULL, out = NULL, verbose = TRUE) {
  if (!"soilKey" %in% loadedNamespaces() &&
      !suppressPackageStartupMessages(requireNamespace("soilKey", quietly = TRUE)))
    stop("This script needs the soilKey package (0.9.392 or later); install it first.", call. = FALSE)
  ns <- asNamespace("soilKey")
  if (!exists(".fao_match_label", envir = ns, inherits = FALSE))
    stop("This script needs soilKey 0.9.392 or later.", call. = FALSE)
  match_label <- get(".fao_match_label", envir = ns)
  path <- fao_isis_cache(cache)
  raw <- readRDS(path)
  # the FAO records of every profile
  recs <- do.call(rbind, lapply(seq_along(raw), function(i) {
    f <- raw[[i]]$faoClassifications
    if (!length(f)) return(NULL)
    do.call(rbind, lapply(f, function(z) data.frame(
      idx = i, profile = as.character(raw[[i]]$profileCode %||% NA_character_),
      year = as.character(z$publicationYear %||% NA_character_),
      label = as.character(z$faoClassification %||% NA_character_), stringsAsFactors = FALSE)))
  }))
  if (is.null(recs) || !nrow(recs)) stop("The cache holds no FAO-Unesco label.", call. = FALSE)
  m <- match_label(recs$label, recs$year, variants = .fao_isis_variants)
  d <- cbind(recs, m[, setdiff(names(m), "label")])
  d <- d[!is.na(d$edition), , drop = FALSE]
  # classify each profile once per edition it is labelled in
  todo <- unique(d[c("idx", "edition")])
  res <- vector("list", nrow(todo))
  peds <- list()
  for (j in seq_len(nrow(todo))) {
    i <- todo$idx[j]
    if (verbose) message(sprintf("[%d/%d] %s %s", j, nrow(todo), raw[[i]]$profileCode, todo$edition[j]))
    key <- as.character(i)
    if (is.null(peds[[key]]))
      peds[[key]] <- tryCatch(soilKey::isis_profile_to_pedon(raw[[i]]), error = function(e) NA)
    p <- peds[[key]]
    res[[j]] <- if (identical(p, NA)) NULL else
      tryCatch(suppressWarnings(soilKey::classify_fao(p, edition = todo$edition[j], on_missing = "silent")),
               error = function(e) NULL)
  }
  rk <- paste(todo$idx, todo$edition)
  dk <- paste(d$idx, d$edition)
  st <- do.call(rbind, lapply(res, function(r) as.data.frame(.fao_isis_result(r), stringsAsFactors = FALSE)))
  st$error <- vapply(res, is.null, NA)
  d <- cbind(d, st[match(dk, rk), , drop = FALSE])
  d$agree_group <- ifelse(is.na(d$group_code), NA, !is.na(d$sk_group) & d$sk_group == d$group_code)
  d$agree_unit <- ifelse(is.na(d$unit_code), NA, !is.na(d$sk_unit) & d$sk_unit == d$unit_code)
  # phases: soilKey's status of each labelled phase; the phases it finds present
  ph_status <- function(r, ids) {
    if (is.null(r) || !is.data.frame(r$phases)) return(rep(NA_character_, length(ids)))
    p <- r$phases[r$phases$kind == "phase", ]
    p$status[match(ids, p$code)]
  }
  phrows <- list()
  for (k in seq_len(nrow(d))) {
    r <- res[[match(dk[k], rk)]]
    lab <- if (nzchar(d$phases[k])) strsplit(d$phases[k], ", ", fixed = TRUE)[[1]] else character(0)
    present <- if (!is.null(r) && is.data.frame(r$phases))
      r$phases$code[r$phases$kind == "phase" & r$phases$status == "present"] else character(0)
    ids <- unique(c(lab, present))
    if (!length(ids)) next
    phrows[[length(phrows) + 1L]] <- data.frame(edition = d$edition[k], profile = d$profile[k], phase = ids,
                                                labelled = ids %in% lab, status = ph_status(r, ids),
                                                stringsAsFactors = FALSE)
  }
  ph <- if (length(phrows)) do.call(rbind, phrows) else
    data.frame(edition = character(0), profile = character(0), phase = character(0),
               labelled = logical(0), status = character(0))
  editions <- intersect(c("1974", "1988"), unique(d$edition))
  score <- do.call(rbind, lapply(editions, function(e) {
    # a profile soilKey could not run (no horizons in the record) is
    # reported apart, not scored
    x <- d[d$edition == e & !is.na(d$group_code) & !d$error %in% TRUE, , drop = FALSE]
    one <- function(level, a, prov, stdin, sub = rep(TRUE, length(a))) {
      s <- !is.na(a) & sub
      k <- sum(a[s]); n <- sum(s); ci <- .fao_isis_wilson(k, n)
      firm <- s & !prov %in% TRUE & !stdin %in% TRUE
      data.frame(edition = e, level = level, n = n, agree = k, pct = if (n) 100 * k / n else NA_real_,
                 lo = 100 * ci[1], hi = 100 * ci[2], n_firm = sum(firm), agree_firm = sum(a[firm]),
                 pct_firm = if (sum(firm)) 100 * sum(a[firm]) / sum(firm) else NA_real_,
                 provisional = sum(s & prov %in% TRUE), stand_in = sum(s & stdin %in% TRUE),
                 fall_through = sum(s & x$fall_through %in% c("undecided", "not_classified")),
                 stringsAsFactors = FALSE)
    }
    rbind(one("grouping", x$agree_group, x$prov_group, x$standin_group),
          one("unit", x$agree_unit, x$prov_unit, x$standin_unit),
          one("unit | grouping agrees", x$agree_unit, x$prov_unit, x$standin_unit, x$agree_group %in% TRUE))
  }))
  status <- do.call(rbind, lapply(editions, function(e) {
    x <- d[d$edition == e, , drop = FALSE]
    data.frame(edition = e, labels = nrow(x), read_group = sum(!is.na(x$group_code)),
               read_unit = sum(!is.na(x$unit_code)), with_subunit = sum(!is.na(x$subunit)),
               with_phase = sum(nzchar(x$phases)), nitosol_nitisol = sum(grepl("nitosol_nitisol", x$flags)),
               variant = sum(x$matched_by %in% "variant"), error = sum(x$error %in% TRUE),
               classified = sum(x$fall_through %in% "no"),
               undecided = sum(x$fall_through %in% "undecided"),
               not_classified = sum(x$fall_through %in% "not_classified"),
               provisional_group = sum(x$prov_group %in% TRUE & x$fall_through %in% "no"),
               stand_in_group = sum(x$standin_group %in% TRUE), stringsAsFactors = FALSE)
  }))
  phase_score <- do.call(rbind, lapply(editions, function(e) {
    x <- ph[ph$edition == e, , drop = FALSE]
    if (!nrow(x)) return(NULL)
    do.call(rbind, lapply(sort(unique(x$phase)), function(id) {
      y <- x[x$phase == id, , drop = FALSE]
      l <- y[y$labelled, , drop = FALSE]
      data.frame(edition = e, phase = id, labelled = nrow(l),
                 present = sum(l$status %in% "present"), absent = sum(l$status %in% "absent"),
                 undecided = sum(l$status %in% "undecided"), not_shown = sum(l$status %in% "not shown"),
                 sk_present = sum(y$status %in% "present"),
                 sk_present_labelled = sum(y$status %in% "present" & y$labelled), stringsAsFactors = FALSE)
    }))
  }))
  confusion <- lapply(stats::setNames(editions, editions), function(e) {
    x <- d[d$edition == e & !is.na(d$group_code) & !d$error %in% TRUE, , drop = FALSE]
    table(label = x$group_code, soilKey = ifelse(is.na(x$sk_group), "[none]", x$sk_group))
  })
  undecided <- lapply(stats::setNames(editions, editions), function(e) {
    u <- unlist(strsplit(d$candidates[d$edition == e & d$fall_through %in% "undecided"], " ", fixed = TRUE))
    sort(table(u[nzchar(u)]), decreasing = TRUE)
  })
  out_res <- list(records = d, score = score, status = status, phases = phase_score, phase_rows = ph,
                  confusion = confusion, undecided = undecided,
                  version = as.character(utils::packageVersion("soilKey")),
                  n_profiles = length(unique(d$idx)), cache = basename(path))
  md <- .fao_isis_report(out_res)
  if (!is.null(out)) {
    writeLines(md, out, useBytes = TRUE)
    if (verbose) message("Report written to ", out)
  } else if (verbose) {
    cat(md, sep = "\n")
  }
  invisible(out_res)
}

.fao_isis_report <- function(res) {
  f1 <- function(x) ifelse(is.na(x), "n/a", sprintf("%.1f", x))
  sc <- res$score; st <- res$status; ps <- res$phases
  ed_name <- c("1974" = "Legend of 1974 (fao1974)", "1988" = "Revised Legend 1988, ISIS year 1997 (fao1988)")
  lines <- c(
    sprintf("# FAO-Unesco legends on the ISIS reference profiles (soilKey %s)", res$version), "",
    sprintf("Offline run on the cache `%s`: %d profiles with an FAO-Unesco label, %d labels.",
            res$cache, res$n_profiles, nrow(res$records)),
    "Each label is compared with soilKey's class in the same edition. 95 % intervals are Wilson intervals.",
    "A fall-through (no grouping passed; no residual group, Q5) counts as a disagreement.",
    "A profile soilKey could not run (no horizons in the record: Not run) is not scored.", "",
    "## Labels read", "",
    "| Edition | Labels | Grouping read | Unit read | With ISRIC subunit | With phase | Nitosol/Nitisol | Variant | Not run | Classified | Undecided | Not classified | Provisional grouping | Stand-in grouping |",
    "|---|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|")
  for (i in seq_len(nrow(st)))
    lines <- c(lines, sprintf("| %s | %d | %d | %d | %d | %d | %d | %d | %d | %d | %d | %d | %d | %d |",
                              ed_name[[st$edition[i]]], st$labels[i], st$read_group[i], st$read_unit[i],
                              st$with_subunit[i], st$with_phase[i], st$nitosol_nitisol[i], st$variant[i],
                              st$error[i], st$classified[i], st$undecided[i], st$not_classified[i],
                              st$provisional_group[i], st$stand_in_group[i]))
  lines <- c(lines, "", "## Agreement", "",
             "| Edition | Level | n | Agree | % | 95 % CI | Firm n | Agree among firm | % firm | Provisional | Stand-in | Fall-through |",
             "|---|---|--:|--:|--:|---|--:|--:|--:|--:|--:|--:|")
  for (i in seq_len(nrow(sc)))
    lines <- c(lines, sprintf("| %s | %s | %d | %d | %s | %s-%s | %d | %d | %s | %d | %d | %d |",
                              sc$edition[i], sc$level[i], sc$n[i], sc$agree[i], f1(sc$pct[i]), f1(sc$lo[i]),
                              f1(sc$hi[i]), sc$n_firm[i], sc$agree_firm[i], f1(sc$pct_firm[i]),
                              sc$provisional[i], sc$stand_in[i], sc$fall_through[i]))
  lines <- c(lines, "", "Firm: neither provisional (a class tested before it left undecided) nor resting on a stand-in.")
  for (e in names(res$undecided)) {
    u <- res$undecided[[e]]
    lines <- c(lines, "", sprintf("Candidates left undecided in the fall-throughs of %s (labels): %s", e,
                                  if (length(u)) paste(sprintf("%s %d", names(u), as.integer(u)), collapse = ", ") else "none."))
  }
  lines <- c(lines, "", "## Phases", "",
             "For each phase a label names, soilKey's status; and how many labels name a phase soilKey finds present.", "",
             "| Edition | Phase | Labelled | Present | Absent | Undecided | Not shown | soilKey present | of which labelled |",
             "|---|---|--:|--:|--:|--:|--:|--:|--:|")
  if (!is.null(ps)) for (i in seq_len(nrow(ps)))
    lines <- c(lines, sprintf("| %s | %s | %d | %d | %d | %d | %d | %d | %d |", ps$edition[i], ps$phase[i],
                              ps$labelled[i], ps$present[i], ps$absent[i], ps$undecided[i], ps$not_shown[i],
                              ps$sk_present[i], ps$sk_present_labelled[i]))
  for (e in names(res$confusion)) {
    cf <- res$confusion[[e]]
    lines <- c(lines, "", sprintf("## Grouping confusion, %s (rows: label; columns: soilKey)", e), "",
               paste0("| | ", paste(colnames(cf), collapse = " | "), " |"),
               paste0("|---|", paste(rep("--:", ncol(cf)), collapse = "|"), "|"))
    for (i in seq_len(nrow(cf)))
      lines <- c(lines, paste0("| ", rownames(cf)[i], " | ", paste(cf[i, ], collapse = " | "), " |"))
  }
  lines <- c(lines, "", "Source: ISRIC World Soil Reference Collection (ISIS); see `soilKey::isis_citation()`.")
  lines
}

if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a)) b else a

if (sys.nframe() == 0L && !interactive()) {
  a <- commandArgs(trailingOnly = TRUE)
  if (is.null(fao_isis_cache(if (length(a) >= 1L) a[1] else NULL, must = FALSE))) quit(save = "no", status = 0L)
  fao_isis_benchmark(cache = if (length(a) >= 1L) a[1] else NULL,
                     out = if (length(a) >= 2L) a[2] else NULL)
}
