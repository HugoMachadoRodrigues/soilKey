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
#     only, a label not read is unmatched, never guessed. A label of another
#     year is counted and not scored.
#   * A unit that is no unit of its edition (the national adaptations, e.g.
#     "Chromic Regosol" of 1974, "Plinthic Calcisol" of 1997) is scored at
#     grouping level only and listed as an unmatched unit (Q3).
#   * WoSIS codes every Kastanozem of 1974 "KS" and every Solonchak "SC"
#     (the codes of 1988), with names that are valid 1974 names: they are
#     read by name as K and Z and scored with the rest of 1974.
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
#     contradicted": the grouping agrees, or the label's grouping is one of
#     the groupings soilKey left undecided (provisional_because) -- those of
#     a fall-through, or those tested before a provisional class. Beside it,
#     a shuffled-label baseline: the same rate with the labels permuted
#     within the subset (200 permutations, fixed seed; the caller's random
#     number state is restored).
#   * Firm: covered and not provisional (no grouping tested before it left
#     undecided); agreement among firm is reported, with the stand-in count.
#   * Sample weights. When the cache holds a sample (manifest.csv) and the
#     listing tells which profiles have layers, the design is read back:
#     strata are edition x country x group code among the listed profiles
#     with layers, and each sampled profile weighs N_h / n_h (a sample drawn
#     proportionally with a minimum of one per stratum over-represents the
#     small strata). The weighted figures are the headline, the unweighted
#     beside them. Weighted intervals: Wilson intervals on Kish's effective
#     sample size, (sum w)^2 / sum w^2 (an approximation).
#   * Unit: over the labels whose unit is read, and among those whose
#     grouping agrees (unweighted).
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
  ok <- function(x) file.exists(file.path(x, "listing.rds")) & dir.exists(file.path(x, "raw"))
  if (length(cache) && !is.na(cache[1]) && nzchar(cache[1]) && !ok(path.expand(cache[1])))
    stop("The WoSIS cache ", cache[1], " does not exist or holds no listing.rds and raw/; ",
         "this script downloads nothing.", call. = FALSE)
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
  # the groupings left undecided: the candidates of a fall-through, or those
  # tested before a provisional class
  und <- sub("^major_groups:", "", grep("^major_groups:", pb, value = TRUE))
  out$candidates <- paste(und, collapse = " ")
  if (!is.data.frame(lv) || !nrow(lv)) {
    out$fall_through <- if (length(und)) "undecided" else "not_classified"
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
# One profile record of listing.rds as a one-row data frame. has_layers is NA
# when the record does not say (no `layers` field).
.fao_wosis_profile_row <- function(p) {
  ch <- function(v) if (is.null(v) || !length(v)) NA_character_ else as.character(v)[1]
  data.frame(profile_id = ch(p$profileId), profile_code = ch(p$profileCode), dataset = ch(p$datasetCode),
             country = ch(p$countryName), lat = suppressWarnings(as.numeric(ch(p$latitude))),
             lon = suppressWarnings(as.numeric(ch(p$longitude))),
             fao_year = ch(p$faoPublicationYear), fao_group_code = ch(p$faoMajorGroupCode),
             fao_group = ch(p$faoMajorGroup), fao_unit_code = ch(p$faoSoilUnitCode),
             fao_unit = ch(p$faoSoilUnit),
             has_layers = if ("layers" %in% names(p)) length(p$layers) > 0L else NA,
             stringsAsFactors = FALSE)
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

# A weighted proportion with a Wilson interval on Kish's effective sample
# size (an approximation); unweighted when all weights are 1.
.fao_wosis_prop <- function(y, w = rep(1, length(y))) {
  y <- as.logical(y); ok <- !is.na(y); y <- y[ok]; w <- w[ok]
  if (!length(y) || sum(w) <= 0) return(c(n = 0, pct = NA_real_, lo = NA_real_, hi = NA_real_))
  p <- sum(w * y) / sum(w)
  neff <- sum(w)^2 / sum(w^2)
  ci <- .fao_wosis_wilson(p * neff, neff)
  c(n = length(y), pct = 100 * p, lo = 100 * ci[1], hi = 100 * ci[2])
}

# The shuffled-label baseline of "not contradicted": the labels permuted
# within the records, `B` times. Returns the mean and the 2.5 and 97.5 %
# points (in %), with and without weights. The random number state of the
# caller is restored.
.fao_wosis_baseline <- function(x, w, B = 200L, seed = 20261110L) {
  if (!nrow(x)) return(c(mean = NA_real_, lo = NA_real_, hi = NA_real_,
                         mean_w = NA_real_, lo_w = NA_real_, hi_w = NA_real_))
  if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
    old <- get(".Random.seed", envir = globalenv(), inherits = FALSE)
    on.exit(assign(".Random.seed", old, envir = globalenv()), add = TRUE)
  } else {
    on.exit(if (exists(".Random.seed", envir = globalenv(), inherits = FALSE))
      rm(".Random.seed", envir = globalenv()), add = TRUE)
  }
  set.seed(seed)
  g <- x$group_code
  codes <- sort(unique(g))
  cands <- strsplit(x$candidates, " ", fixed = TRUE)
  # M[i, k]: label codes[k] would not be contradicted by record i's result
  M <- vapply(codes, function(cd) (!is.na(x$sk_group) & x$sk_group == cd) |
                vapply(cands, function(cc) cd %in% cc, NA), logical(nrow(x)))
  M <- matrix(M, nrow = nrow(x))
  r <- vapply(seq_len(B), function(b) {
    k <- match(g[sample.int(length(g))], codes)
    v <- M[cbind(seq_len(nrow(x)), k)]
    c(mean(v), sum(w * v) / sum(w))
  }, numeric(2))
  c(mean = 100 * mean(r[1, ]), lo = 100 * unname(stats::quantile(r[1, ], 0.025)),
    hi = 100 * unname(stats::quantile(r[1, ], 0.975)),
    mean_w = 100 * mean(r[2, ]), lo_w = 100 * unname(stats::quantile(r[2, ], 0.025)),
    hi_w = 100 * unname(stats::quantile(r[2, ], 0.975)))
}

fao_wosis_benchmark <- function(cache = NULL, out = NULL, verbose = TRUE,
                                exclude_datasets = "WD-ISIS", permutations = 200L) {
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
  P$edition <- ns$.fao_label_edition(P$fao_year)
  man_f <- file.path(dir, "manifest.csv")
  man <- if (file.exists(man_f)) utils::read.csv(man_f, stringsAsFactors = FALSE, colClasses = "character") else NULL
  raw_ids <- sub("\\.rds$", "", list.files(file.path(dir, "raw"), pattern = "\\.rds$"))
  ok_ids <- if (is.null(man)) raw_ids else intersect(man$profile_id[man$status == "ok"], raw_ids)

  # the sample design, read back: strata edition x country x group code
  # among the listed profiles with layers; weight N_h / n_h
  P$stratum <- paste(P$edition, P$country, P$fao_group_code, sep = " | ")
  weighted <- !is.null(man) && nrow(man) > 0L && !anyNA(P$has_layers)
  if (weighted) {
    frame <- P[P$has_layers %in% TRUE & !is.na(P$edition), , drop = FALSE]
    N_h <- table(frame$stratum)
    n_h <- table(P$stratum[match(man$profile_id, P$profile_id)])
    P$weight <- as.numeric(N_h[P$stratum]) / as.numeric(n_h[P$stratum])
  }
  if (!weighted) P$weight <- 1

  d <- P[P$profile_id %in% ok_ids, , drop = FALSE]
  excl <- d$dataset %in% exclude_datasets
  n_excl <- sum(excl)
  d <- d[!excl, , drop = FALSE]
  n_other_year <- sum(is.na(d$edition))
  d <- d[!is.na(d$edition), , drop = FALSE]
  # a design that cannot be read back for every profile scored is not used
  if (weighted && anyNA(d$weight)) { weighted <- FALSE; d$weight <- 1 }
  d$label <- trimws(paste(ifelse(is.na(d$fao_unit), "", d$fao_unit), ifelse(is.na(d$fao_group), "", d$fao_group)))
  m <- match_label(d$label, d$edition, variants = .fao_wosis_variants)
  d <- cbind(d, m[, c("group_code", "group_name", "unit_code", "unit_name", "matched_by", "flags")])
  d$unit_given <- !is.na(d$fao_unit) & nzchar(d$fao_unit)
  d$unit_outside <- d$unit_given & !is.na(d$group_code) & is.na(d$unit_code)
  d$mexico <- d$country %in% "Mexico"
  n <- nrow(d)
  d$licence <- rep(NA_character_, n)
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
  d <- if (n) cbind(d, do.call(rbind, st)) else
    cbind(d, as.data.frame(.fao_wosis_result(NULL), stringsAsFactors = FALSE)[0, ], error = character(0))
  d$cc_by <- ns$.wosis_is_permissive(d$licence)
  d$agree_group <- ifelse(is.na(d$group_code), NA, !is.na(d$sk_group) & d$sk_group == d$group_code)
  d$agree_unit <- ifelse(is.na(d$unit_code), NA, !is.na(d$sk_unit) & d$sk_unit == d$unit_code)
  d$in_candidates <- mapply(function(g, cands) !is.na(g) && g %in% strsplit(cands, " ", fixed = TRUE)[[1]],
                            d$group_code, d$candidates)
  d$covered <- d$fall_through %in% "no"
  d$not_contradicted <- d$agree_group %in% TRUE | d$in_candidates
  d$firm <- d$covered & !d$prov_group %in% TRUE

  listed_other <- sum(is.na(P$edition) & !is.na(P$fao_year))
  accounting <- data.frame(
    item = c("profiles listed with an FAO-Unesco label", "of which with a year other than 1974 or 1997 (not scored)",
             "profiles attempted (manifest)", "downloaded with layers", "no layers",
             "more than 40 layers (not downloaded)", "left out: dataset excluded",
             "left out: year other than 1974 or 1997", "scored"),
    n = c(nrow(P), listed_other,
          if (is.null(man)) NA_integer_ else nrow(man), length(ok_ids),
          if (is.null(man)) NA_integer_ else sum(man$status == "no_layers"),
          if (is.null(man)) NA_integer_ else sum(man$status == "over_max_layers"),
          n_excl, n_other_year, sum(!is.na(d$group_code) & is.na(d$error))),
    stringsAsFactors = FALSE)

  subsets <- list(
    list(edition = "1988", subset = "all", keep = function(x) rep(TRUE, nrow(x))),
    list(edition = "1988", subset = "CC BY only", keep = function(x) x$cc_by),
    list(edition = "1974", subset = "all", keep = function(x) rep(TRUE, nrow(x))),
    list(edition = "1974", subset = "CC BY only", keep = function(x) x$cc_by),
    list(edition = "1974", subset = "Mexico", keep = function(x) x$mexico),
    list(edition = "1974", subset = "outside Mexico", keep = function(x) !x$mexico))
  scorable <- function(e) d[d$edition == e & !is.na(d$group_code) & is.na(d$error), , drop = FALSE]
  score <- do.call(rbind, lapply(subsets, function(s) {
    x <- scorable(s$edition)
    x <- x[s$keep(x) %in% TRUE, , drop = FALSE]
    one <- function(w, tag) {
      ag <- .fao_wosis_prop(x$agree_group %in% TRUE, w)
      cv <- .fao_wosis_prop(x$covered, w)
      ac <- .fao_wosis_prop((x$agree_group %in% TRUE)[x$covered], w[x$covered])
      nc <- .fao_wosis_prop(x$not_contradicted, w)
      fm <- .fao_wosis_prop((x$agree_group %in% TRUE)[x$firm], w[x$firm])
      v <- c(ag, cv[-1], ac, nc[-1], fm)
      names(v) <- c("n", "agree", "agree_lo", "agree_hi", "cov", "cov_lo", "cov_hi",
                    "n_cov", "agree_cov", "agree_cov_lo", "agree_cov_hi", "nc", "nc_lo", "nc_hi",
                    "n_firm", "agree_firm", "agree_firm_lo", "agree_firm_hi")
      as.data.frame(as.list(v))
    }
    bl <- .fao_wosis_baseline(x, x$weight, B = permutations)
    u <- one(rep(1, nrow(x)), "u")
    w <- one(x$weight, "w")
    names(w) <- paste0("w_", names(w))
    ur <- !is.na(x$agree_unit); ua <- x$agree_unit %in% TRUE; agr <- x$agree_group %in% TRUE
    un <- .fao_wosis_prop(ua[ur]); ug <- .fao_wosis_prop(ua[ur & agr])
    cbind(data.frame(edition = s$edition, subset = s$subset, stringsAsFactors = FALSE), u, w,
          data.frame(k_agree = sum(agr), k_cov = sum(x$covered), k_nc = sum(x$not_contradicted),
                     k_firm_agree = sum(agr & x$firm), covered_wrong = sum(x$covered & !agr),
                     firm_wrong = sum(x$firm & !agr), provisional = sum(x$covered & x$prov_group %in% TRUE),
                     stand_in = sum(x$covered & x$standin_group %in% TRUE),
                     undecided = sum(x$fall_through %in% "undecided"),
                     not_classified = sum(x$fall_through %in% "not_classified"),
                     base = bl[["mean"]], base_lo = bl[["lo"]], base_hi = bl[["hi"]],
                     base_w = bl[["mean_w"]], base_w_lo = bl[["lo_w"]], base_w_hi = bl[["hi_w"]],
                     n_unit = un[["n"]], k_unit = sum(ua & ur), unit = un[["pct"]], unit_lo = un[["lo"]],
                     unit_hi = un[["hi"]], n_unit_g = ug[["n"]], k_unit_g = sum(ua & ur & agr),
                     unit_g = ug[["pct"]], unit_g_lo = ug[["lo"]], unit_g_hi = ug[["hi"]],
                     stringsAsFactors = FALSE))
  }))
  editions <- intersect(c("1988", "1974"), unique(d$edition))
  status <- do.call(rbind, lapply(editions, function(e) {
    x <- d[d$edition == e, , drop = FALSE]
    data.frame(edition = e, labels = nrow(x), read_group = sum(!is.na(x$group_code)),
               unit_given = sum(x$unit_given), read_unit = sum(!is.na(x$unit_code)),
               unit_outside = sum(x$unit_outside),
               code_differs = sum(!is.na(x$group_code) & !is.na(x$fao_group_code) & x$group_code != x$fao_group_code),
               nitosol_nitisol = sum(grepl("nitosol_nitisol", x$flags)), variant = sum(x$matched_by %in% "variant"),
               cc_by = sum(x$cc_by), mexico = sum(x$mexico), error = sum(!is.na(x$error)), stringsAsFactors = FALSE)
  }))
  by_group <- do.call(rbind, lapply(editions, function(e) {
    x <- scorable(e)
    if (!nrow(x)) return(NULL)
    do.call(rbind, lapply(sort(unique(x$group_code)), function(g) {
      y <- x[x$group_code == g, , drop = FALSE]
      ag <- y$agree_group %in% TRUE
      data.frame(edition = e, group = g, wosis_code = paste(sort(unique(stats::na.omit(y$fao_group_code))), collapse = ", "),
                 n = nrow(y), agree = sum(ag), covered = sum(y$covered),
                 agree_cov = if (sum(y$covered)) 100 * sum(ag & y$covered) / sum(y$covered) else NA_real_,
                 not_contradicted = sum(y$not_contradicted), stringsAsFactors = FALSE)
    }))
  }))
  um <- d[d$unit_outside, , drop = FALSE]
  unmatched <- if (nrow(um)) {
    t <- stats::aggregate(list(n = rep(1L, nrow(um))), list(edition = um$edition, label = um$label), sum)
    t[order(t$edition, -t$n, t$label), , drop = FALSE]
  } else data.frame(edition = character(0), label = character(0), n = integer(0))
  rownames(unmatched) <- NULL
  confusion <- lapply(stats::setNames(editions, editions), function(e) {
    x <- scorable(e)
    table(label = x$group_code, soilKey = ifelse(is.na(x$sk_group), "[none]", x$sk_group))
  })
  top_confusions <- lapply(stats::setNames(editions, editions), function(e) {
    x <- scorable(e)
    x <- x[!x$agree_group %in% TRUE, , drop = FALSE]
    sort(table(paste(x$group_code, "->", ifelse(is.na(x$sk_group), "[none]", x$sk_group))), decreasing = TRUE)
  })
  covered_wrong <- lapply(stats::setNames(editions, editions), function(e) {
    x <- scorable(e)
    x <- x[x$covered & !x$agree_group %in% TRUE, , drop = FALSE]
    sort(table(paste(x$group_code, "->", x$sk_group)), decreasing = TRUE)
  })
  undecided <- lapply(stats::setNames(editions, editions), function(e) {
    x <- scorable(e)
    u <- unlist(strsplit(x$candidates[x$fall_through %in% "undecided"], " ", fixed = TRUE))
    sort(table(u[nzchar(u)]), decreasing = TRUE)
  })
  when <- if (!is.null(man) && "downloaded" %in% names(man) && any(nzchar(man$downloaded)))
    as.Date(max(substr(man$downloaded[nzchar(man$downloaded)], 1, 10))) else Sys.Date()
  res <- list(records = d, score = score, status = status, by_group = by_group, unmatched = unmatched,
              confusion = confusion, top_confusions = top_confusions, covered_wrong = covered_wrong,
              undecided = undecided, accounting = accounting, weighted = weighted,
              permutations = permutations, excluded_datasets = exclude_datasets, downloaded = when,
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
  pc <- function(p, lo, hi) sprintf("%s (%s-%s)", f1(p), f1(lo), f1(hi))
  ac <- res$accounting; st <- res$status
  sc <- res$score[res$score$n > 0, , drop = FALSE]
  empty <- res$score[res$score$n == 0, , drop = FALSE]
  ed_name <- c("1974" = "Legend of 1974 (fao1974)", "1988" = "Revised Legend 1988, WoSIS year 1997 (fao1988)")
  lines <- c(
    sprintf("# FAO-Unesco legends on WoSIS profiles (soilKey %s)", res$version), "",
    sprintf("Offline run on the cache `%s` (WoSIS data downloaded up to %s).", res$cache, format(res$downloaded)),
    "Each label is compared with soilKey's class in the same edition.",
    "A fall-through (no grouping passed; no residual group) counts as a disagreement.",
    sprintf("Profiles of the dataset(s) %s are left out (scored by the ISIS benchmark).",
            paste(res$excluded_datasets, collapse = ", ")),
    "WoSIS codes the Kastanozems of 1974 KS and the Solonchaks SC; their names are 1974 names, read as K and Z.",
    if (res$weighted) paste("The figures are weighted by the sample design (N_h / n_h over the strata edition x country x group code",
                            "of the listed profiles with layers; the sample was drawn proportionally with a minimum of one per stratum).",
                            "Weighted intervals are Wilson intervals on Kish's effective sample size (an approximation).",
                            "The unweighted figures follow.")
    else "No sample design in the cache: the figures are unweighted, with Wilson 95 % intervals.",
    "", "## Profiles", "", "| | n |", "|---|--:|")
  for (i in seq_len(nrow(ac))) lines <- c(lines, sprintf("| %s | %s |", ac$item[i], ifelse(is.na(ac$n[i]), "n/a", ac$n[i])))
  lines <- c(lines, "", "## Labels read", "",
             "| Edition | Labels | Grouping read | Unit given | Unit read | Unit outside the edition (grouping only) | WoSIS code differs from the name's | Nitosol/Nitisol | CC BY | Mexico | Not run |",
             "|---|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|")
  for (i in seq_len(nrow(st)))
    lines <- c(lines, sprintf("| %s | %d | %d | %d | %d | %d | %d | %d | %d | %d | %d |", ed_name[[st$edition[i]]],
                              st$labels[i], st$read_group[i], st$unit_given[i], st$read_unit[i], st$unit_outside[i],
                              st$code_differs[i], st$nitosol_nitisol[i], st$cc_by[i], st$mexico[i], st$error[i]))
  tab <- function(title, pre) {
    g <- function(nm) sc[[paste0(pre, nm)]]
    out <- c("", title, "",
             "| Edition | Subset | n | Agree, % (95 % CI) | Covered, % (95 % CI) | Agree among covered, % (95 % CI) | Not contradicted, % (95 % CI) | Shuffled-label baseline, % (2.5-97.5) | Firm n | Agree among firm, % (95 % CI) |",
             "|---|---|--:|---|---|---|---|---|--:|---|")
    for (i in seq_len(nrow(sc))) {
      b <- if (pre == "w_") pc(sc$base_w[i], sc$base_w_lo[i], sc$base_w_hi[i]) else pc(sc$base[i], sc$base_lo[i], sc$base_hi[i])
      out <- c(out, sprintf("| %s | %s | %d | %s | %s | %s | %s | %s | %d | %s |", sc$edition[i], sc$subset[i], as.integer(sc$n[i]),
                            pc(g("agree")[i], g("agree_lo")[i], g("agree_hi")[i]),
                            pc(g("cov")[i], g("cov_lo")[i], g("cov_hi")[i]),
                            pc(g("agree_cov")[i], g("agree_cov_lo")[i], g("agree_cov_hi")[i]),
                            pc(g("nc")[i], g("nc_lo")[i], g("nc_hi")[i]), b, as.integer(sc$n_firm[i]),
                            pc(g("agree_firm")[i], g("agree_firm_lo")[i], g("agree_firm_hi")[i])))
    }
    out
  }
  if (res$weighted) lines <- c(lines, tab("## Grouping, weighted (headline)", "w_"))
  lines <- c(lines, tab(if (res$weighted) "## Grouping, unweighted" else "## Grouping", ""))
  lines <- c(lines, "", "Counts (unweighted):", "",
             "| Edition | Subset | Agree | Covered | Covered but wrong | Not contradicted | Firm | Firm but wrong | Provisional | Stand-in | Undecided | Not classified |",
             "|---|---|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|")
  for (i in seq_len(nrow(sc)))
    lines <- c(lines, sprintf("| %s | %s | %d | %d | %d | %d | %d | %d | %d | %d | %d | %d |", sc$edition[i], sc$subset[i],
                              sc$k_agree[i], sc$k_cov[i], sc$covered_wrong[i], sc$k_nc[i], as.integer(sc$n_firm[i]),
                              sc$firm_wrong[i], sc$provisional[i], sc$stand_in[i], sc$undecided[i], sc$not_classified[i]))
  for (i in seq_len(nrow(empty)))
    lines <- c(lines, "", sprintf("No profile of %s is in the subset \"%s\" (none remains once %s is left out).",
                                  empty$edition[i], empty$subset[i], paste(res$excluded_datasets, collapse = ", ")))
  lines <- c(lines, "", paste("Covered: soilKey names a grouping. Not contradicted: the grouping agrees, or the label's grouping",
                                    "is one soilKey left undecided (the candidates of a fall-through, or a grouping tested before a",
                                    sprintf("provisional class). Shuffled-label baseline: the same rate with the labels permuted within the subset (%d permutations, fixed seed).", res$permutations),
                                    "Firm: covered and not provisional (no grouping tested before it left undecided)."),
             "", "## Unit (unweighted)", "",
             "| Edition | Subset | Units read | Agree | % (95 % CI) | Where the grouping agrees | Agree | % (95 % CI) |",
             "|---|---|--:|--:|---|--:|--:|---|")
  for (i in seq_len(nrow(sc)))
    lines <- c(lines, sprintf("| %s | %s | %d | %d | %s | %d | %d | %s |", sc$edition[i], sc$subset[i],
                              as.integer(sc$n_unit[i]), sc$k_unit[i], pc(sc$unit[i], sc$unit_lo[i], sc$unit_hi[i]),
                              as.integer(sc$n_unit_g[i]), sc$k_unit_g[i], pc(sc$unit_g[i], sc$unit_g_lo[i], sc$unit_g_hi[i])))
  bg <- res$by_group
  if (!is.null(bg) && nrow(bg)) {
    lines <- c(lines, "", "## By group of the label (unweighted)", "",
               "| Edition | Group | WoSIS code | n | Agree | Covered | Agree among covered, % | Not contradicted |",
               "|---|---|---|--:|--:|--:|--:|--:|")
    for (i in seq_len(nrow(bg)))
      lines <- c(lines, sprintf("| %s | %s | %s | %d | %d | %d | %s | %d |", bg$edition[i], bg$group[i], bg$wosis_code[i],
                                bg$n[i], bg$agree[i], bg$covered[i], f1(bg$agree_cov[i]), bg$not_contradicted[i]))
  }
  for (e in names(res$top_confusions)) {
    tc <- utils::head(res$top_confusions[[e]], 20)
    cw <- utils::head(res$covered_wrong[[e]], 15)
    u <- utils::head(res$undecided[[e]], 15)
    lines <- c(lines, "", sprintf("## Disagreements, %s", e), "",
               sprintf("Most frequent (label -> soilKey): %s.",
                       if (length(tc)) paste(sprintf("%s %d", names(tc), as.integer(tc)), collapse = "; ") else "none"),
               "", sprintf("Covered but wrong (label -> soilKey): %s.",
                           if (length(cw)) paste(sprintf("%s %d", names(cw), as.integer(cw)), collapse = "; ") else "none"),
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
    lines <- c(lines, "", sprintf("## Grouping confusion, %s (rows: label; columns: soilKey; unweighted)", e), "",
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
