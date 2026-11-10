# =============================================================================
# soilKey -- offline benchmark of the FAO-Unesco legends (1974 and the Revised
# Legend of 1988) on WoSIS profiles (v0.9.396).
#
# ISRIC's WoSIS gives many profiles one FAO-Unesco name: the fields
# faoMajorGroup (the group, in the singular), faoSoilUnit (the unit
# adjective) and faoPublicationYear, which is 1974 (the Legend of 1974) or
# 1997 (the year of ISRIC's reprint of the Revised Legend, read as fao1988,
# Hugo's decision Q1). soilKey keys each profile in the edition of its label
# (classify_fao(edition = "1974" / "1988")) and the label is compared with the
# class of the same edition.
#
# OFFLINE. This script downloads nothing and queries no service. It reads a
# cache folder of WoSIS data saved by the user, from, in this order:
#   1. the first argument (or `cache =`);
#   2. the option soilKey.wosis_fao_cache;
#   3. the environment variable SOILKEY_WOSIS_FAO_CACHE;
#   4. file.path(tools::R_user_dir("soilKey", "cache"), "wosis_fao").
# The folder holds:
#   * listing.rds: the list of profile records as the WoSIS GraphQL server
#     returns them (wosisLatestProfiles: profileId, profileCode, datasetCode,
#     countryName, faoMajorGroupCode, faoMajorGroup, faoSoilUnitCode,
#     faoSoilUnit, faoPublicationYear);
#   * raw/<profileId>.rds: the layers of one profile as the server returns
#     them (wosisLatestLayers, with licence, datasetId and the <field>Values
#     of the properties read), one file per profile;
#   * manifest.csv (optional): one row per profile attempted, with
#     profile_id, status ("ok", "no_layers", "over_max_layers"), downloaded
#     (date and time) and the licences of its layers.
# The layers are read by soilKey:::.wosis_layers_to_horizons(), the same step
# read_wosis_layers_graphql() takes after its download, so a profile is read
# here exactly as the live reader reads it. Run as a script with no cache
# found, it says so and exits cleanly; from R, fao_wosis_benchmark() stops
# with that message.
#
# WoSIS data are ISRIC's, licensed per profile as each data provider
# specified (CC BY or CC BY-NC); they are not shipped with soilKey. Cite them
# as ISRIC asks (soilKey::wosis_citation()): Batjes NH, Calisto L and de Sousa
# LM, 2024. WoSIS-latest: Standardised world soil profile data. The report
# carries the citation with the date of the download.
#
# Usage:
#   Rscript fao_wosis.R [cache_dir] [report.md]
# From R:
#   source(system.file("benchmarks", "fao_wosis.R", package = "soilKey"))
#   b <- fao_wosis_benchmark(cache = "wosis_fao", out = "report.md")
#
# Scoring (Hugo's decisions of 2026-11-10):
#   * The label "<faoSoilUnit> <faoMajorGroup>" is read by
#     soilKey:::.fao_match_label() in the edition of its year: exact names
#     only, a label not read is unmatched, never guessed.
#   * A unit that is no unit of its edition (the national adaptations, e.g.
#     "Chromic Regosol" of 1974, "Plinthic Calcisol" of 1997) is scored at
#     grouping level only and listed as an unmatched unit (Q3).
#   * A record of 1974 whose group code is one of 1988 (SC, KS: the labels
#     mix editions in some datasets) is scored by its 1974 name and reported
#     apart, not in the 1974 totals (Q2).
#   * Profiles of the dataset WD-ISIS are left out: they are scored by the
#     ISIS benchmark (fao_isis.R) (Q7).
#   * All licences are scored (non-commercial research; only aggregate
#     figures are reported); the figures on the CC BY profiles only are given
#     beside them (Q4). A profile's licence is the most restrictive of its
#     layers.
#   * The Legend of 1974 is also given for Mexico and for the rest apart
#     (most of its WoSIS labels come from one Mexican dataset).
#   * Headline (Q6): agreement (soilKey's grouping equals the label's, over
#     all labels; a fall-through counts as a disagreement), coverage
#     (soilKey names a grouping), agreement among covered, and "not
#     contradicted" (agrees, or soilKey stops undecided with the label's
#     grouping among its candidates), each with a Wilson 95 % interval.
#   * Unit: over the labels whose unit is read, and among those whose
#     grouping agrees. Firm: classified, neither provisional nor resting on
#     a stand-in.
# =============================================================================

.fao_wosis_variants <- character(0)

if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a)) b else a

.fao_wosis_wilson <- function(k, n, z = stats::qnorm(0.975)) {
  if (!n) return(c(NA_real_, NA_real_))
  p <- k / n
  den <- 1 + z^2 / n
  mid <- (p + z^2 / (2 * n)) / den
  half <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / den
  c(max(0, mid - half), min(1, mid + half))
}

fao_wosis_cache <- function(cache = NULL, must = TRUE) {
  cands <- c(cache, getOption("soilKey.wosis_fao_cache", ""), Sys.getenv("SOILKEY_WOSIS_FAO_CACHE", ""),
             file.path(tools::R_user_dir("soilKey", "cache"), "wosis_fao"))
  cands <- path.expand(as.character(cands[!is.na(cands) & nzchar(cands)]))
  hit <- cands[file.exists(file.path(cands, "listing.rds")) & dir.exists(file.path(cands, "raw"))]
  if (!length(hit)) {
    msg <- paste0("No WoSIS cache found (tried: ", paste(cands, collapse = ", "), "). ",
                  "Give the folder holding listing.rds and raw/ as the first argument, in the option ",
                  "soilKey.wosis_fao_cache or in SOILKEY_WOSIS_FAO_CACHE; this script downloads nothing.")
    if (must) stop(msg, call. = FALSE)
    message(msg)
    return(NULL)
  }
  hit[1]
}

# TRUE when a stand-in decided a test of a trace entry (a nested list).
.fao_wosis_stand_in <- function(x) {
  if (!is.list(x)) return(FALSE)
  s <- x[["stand_in"]]
  if (!is.null(s) && length(s) && !is.list(s) && any(nzchar(as.character(s)))) return(TRUE)
  any(vapply(x, .fao_wosis_stand_in, NA))
}

# soilKey's grouping and unit of one result, with their status.
.fao_wosis_result <- function(r) {
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
  out$standin_group <- .fao_wosis_stand_in(r$trace$major_groups[[g]]$evidence)
  out$standin_unit <- out$standin_group ||
    (!is.na(u) && .fao_wosis_stand_in(r$trace$units[[u]]$evidence))
  out
}

# One profile record of listing.rds as a one-row data frame.
.fao_wosis_profile_row <- function(p) {
  ch <- function(v) if (is.null(v) || !length(v)) NA_character_ else as.character(v)[1]
  data.frame(profile_id = ch(p$profileId), profile_code = ch(p$profileCode), dataset = ch(p$datasetCode),
             country = ch(p$countryName), lat = suppressWarnings(as.numeric(ch(p$latitude))),
             lon = suppressWarnings(as.numeric(ch(p$longitude))),
             fao_year = ch(p$faoPublicationYear), fao_group_code = ch(p$faoMajorGroupCode),
             fao_group = ch(p$faoMajorGroup), fao_unit_code = ch(p$faoSoilUnitCode),
             fao_unit = ch(p$faoSoilUnit), stringsAsFactors = FALSE)
}

# A PedonRecord from one cached profile (its listing row and its layers).
.fao_wosis_pedon <- function(row, layers) {
  ns <- asNamespace("soilKey")
  L <- ns$.wosis_layers_to_horizons(layers)
  site <- list(id = row$profile_code, lat = row$lat, lon = row$lon, country = row$country,
               wosis_profile_id = row$profile_id, source = "ISRIC WoSIS", dataset = row$dataset,
               licence = attr(L, "licence"))
  p <- attr(L, "provenance")
  soilKey::PedonRecord$new(site = site,
                           horizons = ns$ensure_horizon_schema(data.table::as.data.table(L)),
                           provenance = if (nrow(p)) data.table::as.data.table(p) else NULL)
}

fao_wosis_benchmark <- function(cache = NULL, out = NULL, verbose = TRUE,
                                exclude_datasets = "WD-ISIS") {
  if (!"soilKey" %in% loadedNamespaces() &&
      !suppressPackageStartupMessages(requireNamespace("soilKey", quietly = TRUE)))
    stop("This script needs the soilKey package (0.9.396 or later); install it first.", call. = FALSE)
  ns <- asNamespace("soilKey")
  if (!exists(".wosis_layers_to_horizons", envir = ns, inherits = FALSE))
    stop("This script needs soilKey 0.9.396 or later.", call. = FALSE)
  match_label <- get(".fao_match_label", envir = ns)
  dir <- fao_wosis_cache(cache)
  listing <- readRDS(file.path(dir, "listing.rds"))
  P <- do.call(rbind, lapply(listing, .fao_wosis_profile_row))
  P <- P[!duplicated(P$profile_id), , drop = FALSE]
  man_f <- file.path(dir, "manifest.csv")
  man <- if (file.exists(man_f)) utils::read.csv(man_f, stringsAsFactors = FALSE, colClasses = "character") else NULL
  raw_ids <- sub("\\.rds$", "", list.files(file.path(dir, "raw"), pattern = "\\.rds$"))
  ok_ids <- if (is.null(man)) raw_ids else intersect(man$profile_id[man$status == "ok"], raw_ids)
  accounting <- data.frame(
    item = c("profiles listed with an FAO-Unesco label", "profiles attempted (manifest)",
             "downloaded with layers", "no layers", "more layers than the download reads (skipped)",
             "left out: dataset excluded", "scored"),
    n = NA_integer_, stringsAsFactors = FALSE)
  accounting$n[1] <- nrow(P)
  accounting$n[2] <- if (is.null(man)) NA_integer_ else nrow(man)
  accounting$n[3] <- length(ok_ids)
  accounting$n[4] <- if (is.null(man)) NA_integer_ else sum(man$status == "no_layers")
  accounting$n[5] <- if (is.null(man)) NA_integer_ else sum(man$status == "over_max_layers")
  d <- P[P$profile_id %in% ok_ids, , drop = FALSE]
  excl <- d$dataset %in% exclude_datasets
  accounting$n[6] <- sum(excl)
  d <- d[!excl, , drop = FALSE]
  d$label <- trimws(paste(ifelse(is.na(d$fao_unit), "", d$fao_unit), ifelse(is.na(d$fao_group), "", d$fao_group)))
  m <- match_label(d$label, d$fao_year, variants = .fao_wosis_variants)
  d <- cbind(d, m[, c("edition", "group_code", "group_name", "unit_code", "unit_name", "matched_by", "flags")])
  d <- d[!is.na(d$edition), , drop = FALSE]
  d$code_1988_in_1974 <- d$edition == "1974" & !is.na(d$fao_group_code) &
    !d$fao_group_code %in% ns$.F74_GROUPS$code
  d$unit_given <- !is.na(d$fao_unit) & nzchar(d$fao_unit)
  d$unit_outside <- d$unit_given & !is.na(d$group_code) & is.na(d$unit_code)
  d$mexico <- d$country %in% "Mexico"
  n <- nrow(d)
  d$licence <- NA_character_
  st <- vector("list", n)
  for (i in seq_len(n)) {
    if (verbose && (i %% 250L == 0L || i == n)) message(sprintf("[%d/%d]", i, n))
    lay <- readRDS(file.path(dir, "raw", paste0(d$profile_id[i], ".rds")))
    d$licence[i] <- ns$.wosis_most_restrictive(vapply(lay, function(l) as.character(l$licence %||% NA_character_), ""))
    r <- tryCatch(suppressWarnings(soilKey::classify_fao(.fao_wosis_pedon(d[i, ], lay), edition = d$edition[i],
                                                         on_missing = "silent")),
                  error = function(e) e)
    err <- inherits(r, "error")
    s <- .fao_wosis_result(if (err) NULL else r)
    s$error <- if (err) conditionMessage(r) else NA_character_
    st[[i]] <- as.data.frame(s, stringsAsFactors = FALSE)
  }
  if (n) d <- cbind(d, do.call(rbind, st)) else
    d <- cbind(d, as.data.frame(.fao_wosis_result(NULL), stringsAsFactors = FALSE)[0, ], error = character(0))
  d$cc_by <- ns$.wosis_is_permissive(d$licence)
  accounting$n[7] <- sum(!is.na(d$group_code) & is.na(d$error))
  d$agree_group <- ifelse(is.na(d$group_code), NA, !is.na(d$sk_group) & d$sk_group == d$group_code)
  d$agree_unit <- ifelse(is.na(d$unit_code), NA, !is.na(d$sk_unit) & d$sk_unit == d$unit_code)
  d$candidate_ok <- d$fall_through %in% "undecided" &
    mapply(function(g, cands) !is.na(g) && g %in% strsplit(cands, " ", fixed = TRUE)[[1]], d$group_code, d$candidates)

  subsets <- list(
    list(edition = "1988", subset = "all", keep = function(x) rep(TRUE, nrow(x))),
    list(edition = "1988", subset = "CC BY only", keep = function(x) x$cc_by),
    list(edition = "1974", subset = "all (1974 group codes)", keep = function(x) !x$code_1988_in_1974),
    list(edition = "1974", subset = "CC BY only", keep = function(x) !x$code_1988_in_1974 & x$cc_by),
    list(edition = "1974", subset = "Mexico", keep = function(x) !x$code_1988_in_1974 & x$mexico),
    list(edition = "1974", subset = "outside Mexico", keep = function(x) !x$code_1988_in_1974 & !x$mexico),
    list(edition = "1974", subset = "records with 1988 group codes (apart)", keep = function(x) x$code_1988_in_1974))
  pct <- function(k, n) if (n) 100 * k / n else NA_real_
  ci <- function(k, n) 100 * .fao_wosis_wilson(k, n)
  score <- do.call(rbind, lapply(subsets, function(s) {
    x <- d[d$edition == s$edition & !is.na(d$group_code) & is.na(d$error), , drop = FALSE]
    x <- x[s$keep(x) %in% TRUE, , drop = FALSE]
    nn <- nrow(x)
    cov <- x$fall_through %in% "no"
    ag <- x$agree_group %in% TRUE
    nc <- ag | x$candidate_ok
    ur <- !is.na(x$agree_unit)
    ua <- x$agree_unit %in% TRUE
    firm <- cov & !x$prov_group %in% TRUE & !x$standin_group %in% TRUE
    ci_ag <- ci(sum(ag), nn); ci_cov <- ci(sum(cov), nn); ci_agc <- ci(sum(ag & cov), sum(cov))
    ci_nc <- ci(sum(nc), nn); ci_u <- ci(sum(ua & ur), sum(ur)); ci_ug <- ci(sum(ua & ur & ag), sum(ur & ag))
    data.frame(edition = s$edition, subset = s$subset, n = nn,
               agree = sum(ag), pct = pct(sum(ag), nn), lo = ci_ag[1], hi = ci_ag[2],
               covered = sum(cov), pct_cov = pct(sum(cov), nn), lo_cov = ci_cov[1], hi_cov = ci_cov[2],
               pct_agree_cov = pct(sum(ag & cov), sum(cov)), lo_agree_cov = ci_agc[1], hi_agree_cov = ci_agc[2],
               not_contradicted = sum(nc), pct_nc = pct(sum(nc), nn), lo_nc = ci_nc[1], hi_nc = ci_nc[2],
               n_unit = sum(ur), agree_unit = sum(ua & ur), pct_unit = pct(sum(ua & ur), sum(ur)),
               lo_unit = ci_u[1], hi_unit = ci_u[2],
               n_unit_g = sum(ur & ag), agree_unit_g = sum(ua & ur & ag), pct_unit_g = pct(sum(ua & ur & ag), sum(ur & ag)),
               lo_unit_g = ci_ug[1], hi_unit_g = ci_ug[2],
               n_firm = sum(firm), agree_firm = sum(ag & firm), pct_firm = pct(sum(ag & firm), sum(firm)),
               provisional = sum(cov & x$prov_group %in% TRUE), stand_in = sum(cov & x$standin_group %in% TRUE),
               undecided = sum(x$fall_through %in% "undecided"),
               not_classified = sum(x$fall_through %in% "not_classified"), stringsAsFactors = FALSE)
  }))
  editions <- intersect(c("1988", "1974"), unique(d$edition))
  status <- do.call(rbind, lapply(editions, function(e) {
    x <- d[d$edition == e, , drop = FALSE]
    data.frame(edition = e, labels = nrow(x), read_group = sum(!is.na(x$group_code)),
               unit_given = sum(x$unit_given), read_unit = sum(!is.na(x$unit_code)),
               unit_outside = sum(x$unit_outside), code_1988 = sum(x$code_1988_in_1974),
               nitosol_nitisol = sum(grepl("nitosol_nitisol", x$flags)), variant = sum(x$matched_by %in% "variant"),
               cc_by = sum(x$cc_by), mexico = sum(x$mexico), error = sum(!is.na(x$error)), stringsAsFactors = FALSE)
  }))
  um <- d[d$unit_outside, , drop = FALSE]
  unmatched <- if (nrow(um)) {
    t <- stats::aggregate(list(n = rep(1L, nrow(um))), list(edition = um$edition, label = um$label), sum)
    t[order(t$edition, -t$n, t$label), , drop = FALSE]
  } else data.frame(edition = character(0), label = character(0), n = integer(0))
  rownames(unmatched) <- NULL
  scored <- function(e) d[d$edition == e & !is.na(d$group_code) & is.na(d$error) &
                            !(e == "1974" & d$code_1988_in_1974), , drop = FALSE]
  confusion <- lapply(stats::setNames(editions, editions), function(e) {
    x <- scored(e)
    table(label = x$group_code, soilKey = ifelse(is.na(x$sk_group), "[none]", x$sk_group))
  })
  top_confusions <- lapply(stats::setNames(editions, editions), function(e) {
    x <- scored(e)
    x <- x[!x$agree_group %in% TRUE, , drop = FALSE]
    sort(table(paste(x$group_code, "->", ifelse(is.na(x$sk_group), "[none]", x$sk_group))), decreasing = TRUE)
  })
  undecided <- lapply(stats::setNames(editions, editions), function(e) {
    x <- scored(e)
    u <- unlist(strsplit(x$candidates[x$fall_through %in% "undecided"], " ", fixed = TRUE))
    sort(table(u[nzchar(u)]), decreasing = TRUE)
  })
  when <- if (!is.null(man) && "downloaded" %in% names(man) && any(nzchar(man$downloaded)))
    as.Date(max(substr(man$downloaded[nzchar(man$downloaded)], 1, 10))) else Sys.Date()
  res <- list(records = d, score = score, status = status, unmatched = unmatched, confusion = confusion,
              top_confusions = top_confusions, undecided = undecided, accounting = accounting,
              excluded_datasets = exclude_datasets, downloaded = when,
              citation = soilKey::wosis_citation(when),
              version = as.character(utils::packageVersion("soilKey")), cache = basename(dir))
  md <- .fao_wosis_report(res)
  if (!is.null(out)) {
    writeLines(md, out, useBytes = TRUE)
    if (verbose) message("Report written to ", out)
  } else if (verbose) {
    cat(md, sep = "\n")
  }
  invisible(res)
}

.fao_wosis_report <- function(res) {
  f1 <- function(x) ifelse(is.na(x), "n/a", sprintf("%.1f", x))
  rng <- function(lo, hi) sprintf("%s-%s", f1(lo), f1(hi))
  sc <- res$score; st <- res$status; ac <- res$accounting
  ed_name <- c("1974" = "Legend of 1974 (fao1974)", "1988" = "Revised Legend 1988, WoSIS year 1997 (fao1988)")
  lines <- c(
    sprintf("# FAO-Unesco legends on WoSIS profiles (soilKey %s)", res$version), "",
    sprintf("Offline run on the cache `%s` (WoSIS data downloaded up to %s).", res$cache, format(res$downloaded)),
    "Each label is compared with soilKey's class in the same edition. 95 % intervals are Wilson intervals.",
    "A fall-through (no grouping passed; no residual group) counts as a disagreement.",
    sprintf("Profiles of the dataset(s) %s are left out (scored by the ISIS benchmark).",
            paste(res$excluded_datasets, collapse = ", ")), "",
    "## Profiles", "", "| | n |", "|---|--:|")
  for (i in seq_len(nrow(ac))) lines <- c(lines, sprintf("| %s | %s |", ac$item[i], ifelse(is.na(ac$n[i]), "n/a", ac$n[i])))
  lines <- c(lines, "", "## Labels read", "",
             "| Edition | Labels | Grouping read | Unit given | Unit read | Unit outside the edition (grouping only) | 1988 group code (apart) | Nitosol/Nitisol | CC BY | Mexico | Not run |",
             "|---|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|")
  for (i in seq_len(nrow(st)))
    lines <- c(lines, sprintf("| %s | %d | %d | %d | %d | %d | %d | %d | %d | %d | %d |", ed_name[[st$edition[i]]],
                              st$labels[i], st$read_group[i], st$unit_given[i], st$read_unit[i], st$unit_outside[i],
                              st$code_1988[i], st$nitosol_nitisol[i], st$cc_by[i], st$mexico[i], st$error[i]))
  lines <- c(lines, "", "## Grouping", "",
             "| Edition | Subset | n | Agree | % (95 % CI) | Covered | % (95 % CI) | Agree among covered, % (95 % CI) | Not contradicted | % (95 % CI) | Firm n | Agree among firm | Provisional | Stand-in | Undecided | Not classified |",
             "|---|---|--:|--:|---|--:|---|---|--:|---|--:|--:|--:|--:|--:|--:|")
  for (i in seq_len(nrow(sc)))
    lines <- c(lines, sprintf("| %s | %s | %d | %d | %s (%s) | %d | %s (%s) | %s (%s) | %d | %s (%s) | %d | %d | %d | %d | %d | %d |",
                              sc$edition[i], sc$subset[i], sc$n[i], sc$agree[i], f1(sc$pct[i]), rng(sc$lo[i], sc$hi[i]),
                              sc$covered[i], f1(sc$pct_cov[i]), rng(sc$lo_cov[i], sc$hi_cov[i]),
                              f1(sc$pct_agree_cov[i]), rng(sc$lo_agree_cov[i], sc$hi_agree_cov[i]),
                              sc$not_contradicted[i], f1(sc$pct_nc[i]), rng(sc$lo_nc[i], sc$hi_nc[i]),
                              sc$n_firm[i], sc$agree_firm[i], sc$provisional[i], sc$stand_in[i],
                              sc$undecided[i], sc$not_classified[i]))
  lines <- c(lines, "", paste("Covered: soilKey names a grouping. Not contradicted: the grouping agrees, or soilKey stops",
                                    "undecided with the label's grouping among its candidates. Firm: covered, neither provisional",
                                    "(a class tested before it left undecided) nor resting on a stand-in."),
             "", "## Unit", "",
             "| Edition | Subset | Units read | Agree | % (95 % CI) | Where the grouping agrees | Agree | % (95 % CI) |",
             "|---|---|--:|--:|---|--:|--:|---|")
  for (i in seq_len(nrow(sc)))
    lines <- c(lines, sprintf("| %s | %s | %d | %d | %s (%s) | %d | %d | %s (%s) |", sc$edition[i], sc$subset[i],
                              sc$n_unit[i], sc$agree_unit[i], f1(sc$pct_unit[i]), rng(sc$lo_unit[i], sc$hi_unit[i]),
                              sc$n_unit_g[i], sc$agree_unit_g[i], f1(sc$pct_unit_g[i]), rng(sc$lo_unit_g[i], sc$hi_unit_g[i])))
  for (e in names(res$top_confusions)) {
    tc <- utils::head(res$top_confusions[[e]], 20)
    u <- utils::head(res$undecided[[e]], 15)
    lines <- c(lines, "", sprintf("## Disagreements, %s", e), "",
               sprintf("Most frequent (label -> soilKey): %s.",
                       if (length(tc)) paste(sprintf("%s %d", names(tc), as.integer(tc)), collapse = "; ") else "none"),
               "", sprintf("Candidates left undecided in the fall-throughs: %s.",
                           if (length(u)) paste(sprintf("%s %d", names(u), as.integer(u)), collapse = ", ") else "none"))
  }
  um <- res$unmatched
  lines <- c(lines, "", "## Unmatched units (scored at grouping level only)", "")
  if (nrow(um)) {
    lines <- c(lines, "| Edition | Label | n |", "|---|---|--:|")
    for (i in seq_len(nrow(um))) lines <- c(lines, sprintf("| %s | %s | %d |", um$edition[i], um$label[i], um$n[i]))
  } else lines <- c(lines, "None.")
  for (e in names(res$confusion)) {
    cf <- res$confusion[[e]]
    lines <- c(lines, "", sprintf("## Grouping confusion, %s (rows: label; columns: soilKey)", e), "",
               paste0("| | ", paste(colnames(cf), collapse = " | "), " |"),
               paste0("|---|", paste(rep("--:", ncol(cf)), collapse = "|"), "|"))
    for (i in seq_len(nrow(cf)))
      lines <- c(lines, paste0("| ", rownames(cf)[i], " | ", paste(cf[i, ], collapse = " | "), " |"))
  }
  lines <- c(lines, "", paste0("Source: ", res$citation),
             "WoSIS profiles are licensed per profile by their data providers (CC BY or CC BY-NC); only aggregate figures are given here.")
  lines
}

if (sys.nframe() == 0L && !interactive()) {
  a <- commandArgs(trailingOnly = TRUE)
  if (is.null(fao_wosis_cache(if (length(a) >= 1L) a[1] else NULL, must = FALSE))) quit(save = "no", status = 0L)
  fao_wosis_benchmark(cache = if (length(a) >= 1L) a[1] else NULL,
                      out = if (length(a) >= 2L) a[2] else NULL)
}
