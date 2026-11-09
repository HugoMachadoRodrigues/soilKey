# =============================================================================
# soilKey -- offline benchmark of the German KA5 (2005) on the ISIS reference
# profiles from Germany (v0.9.379).
#
# ISRIC's ISIS holds 24 profiles from Germany. For most of them the local
# classification gives a German name, in the usage before the KA5 (the KA4,
# the DBG systematics of 1998: "Schwarzerde", "Typischer Gley", "Acker-"
# names). soilKey keys the KA5 (classify_ka5()). The ISIS designations are
# FAO style (Ah1, E, Bt), so soilKey reads the horizons from the data.
#
# OFFLINE. This script downloads nothing and queries no service. It reads a
# cache of raw ISIS profiles (the list of profiles as the ISIS GraphQL server
# returns them, every field of .isis_profile_fields(), saved with saveRDS),
# from, in this order:
#   1. the first argument;
#   2. the environment variable SOILKEY_ISIS_CACHE;
#   3. file.path(tools::R_user_dir("soilKey", "cache"), "isis", "profiles.rds").
# It stops if none exists. ISIS data are ISRIC's (educational and
# non-commercial use, source acknowledged: isis_citation()); they are not
# shipped with soilKey.
#
# Usage:
#   Rscript ka5_isis.R [cache.rds] [report.md]
# The markdown report is written only where the second argument says;
# without it the summary is printed. From R:
#   source(system.file("benchmarks", "ka5_isis.R", package = "soilKey"))
#   b <- ka5_isis_benchmark(cache = "profiles.rds", out = "report.md")
#
# Scoring. Each label is read by soilKey:::.ka5_match_label(): the names of
# the KA5 key and the older names of inst/rules/ka5/legacy_names.yaml, exactly
# (case, umlaut spelling and the spacing of hyphens alike), with the leading modifiers Acker-,
# Typischer and Primaerer. Only when a label is not read are the spelling
# variants of .ka5_isis_variants tried, an explicit list kept here; such
# matches are flagged in the report. A label not read is unmatched, never
# guessed. A level is scored on the profiles whose label reaches it: the class
# on every read label, the type on the labels that name a type or subtype, the
# subtype only where both the label and soilKey give one. soilKey stopping
# above a level counts as a disagreement at that level (the type, the class).
# Agreement carries a Wilson 95 % interval. The class soilKey assigns at a
# level is provisional when a test at that level or above was left undecided
# (provisional_because); varieties are not scored.
# =============================================================================

.ka5_isis_levels <- c("classes", "types", "subtypes")

# The forms of the ISIS labels the legacy table does not hold (regular
# expression, case ignored = replacement), tried only when a label is not
# read; older names with no printed source, kept here and not in
# legacy_names.yaml (Hugo, 2026-11-08):
#   * a soil "like a Schwarzerde" (umlaut or ae), read as the class
#     Schwarzerden (T) only, its type not scored;
#   * a Braunerde "of low base content" (with or without "mit", dative or
#     genitive), read as "basenarme Braunerde": type BB, the variety dy noted
#     and not scored (the base content could also be mesotroph, p. 265).
.ka5_isis_variants <- c("^Schwarzerde(\u00e4|ae)hnlicher Boden$" = "Schwarzerden",
                        "^Braunerde (mit )?geringe[mn] Basengehalt(e?s)?$" = "basenarme Braunerde")

.ka5_isis_wilson <- function(k, n, z = stats::qnorm(0.975)) {
  if (!n) return(c(NA_real_, NA_real_))
  p <- k / n
  den <- 1 + z^2 / n
  mid <- (p + z^2 / (2 * n)) / den
  half <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / den
  c(max(0, mid - half), min(1, mid + half))
}

ka5_isis_cache <- function(cache = NULL) {
  cands <- c(cache, Sys.getenv("SOILKEY_ISIS_CACHE", ""),
             file.path(tools::R_user_dir("soilKey", "cache"), "isis", "profiles.rds"))
  cands <- path.expand(cands[!is.na(cands) & nzchar(cands)])
  hit <- cands[file.exists(cands)]
  if (!length(hit))
    stop("No ISIS cache found (tried: ", paste(cands, collapse = ", "), "). ",
         "Give the path of the saved ISIS profiles as the first argument or in ",
         "SOILKEY_ISIS_CACHE; this script downloads nothing.", call. = FALSE)
  hit[1]
}

ka5_isis_benchmark <- function(cache = NULL, out = NULL, verbose = TRUE) {
  if (!"soilKey" %in% loadedNamespaces() &&
      !suppressPackageStartupMessages(requireNamespace("soilKey", quietly = TRUE)))
    stop("This script needs the soilKey package (0.9.379 or later); install it first.", call. = FALSE)
  ns <- asNamespace("soilKey")
  if (!exists(".ka5_match_label", envir = ns, inherits = FALSE))
    stop("This script needs soilKey 0.9.379 or later.", call. = FALSE)
  match_label <- get(".ka5_match_label", envir = ns)
  code_at <- get(".ka5_code_at", envir = ns)
  path <- ka5_isis_cache(cache)
  raw <- readRDS(path)
  de <- Filter(function(p) identical(p$countryCode, "DE") &&
                 grepl("^DE0[0-9]{2}$", p$profileCode %||% ""), raw)
  if (!length(de)) stop("The cache holds no ISIS profile from Germany.", call. = FALSE)
  de <- de[order(vapply(de, `[[`, "", "profileCode"))]
  codes <- vapply(de, `[[`, "", "profileCode")
  lab <- vapply(de, function(p) {
    lc <- p$localClassifications
    v <- unlist(lapply(lc, function(z) z$classificationName))
    if (length(v)) paste(v, collapse = " | ") else NA_character_
  }, "")
  m <- match_label(lab, variants = .ka5_isis_variants)
  m$profile <- codes
  rules <- soilKey::load_rules("ka5")
  sk <- lapply(seq_along(de), function(i) {
    if (verbose) message(sprintf("[%d/%d] %s", i, length(de), codes[i]))
    p <- tryCatch(soilKey::isis_profile_to_pedon(de[[i]]), error = function(e) NULL)
    if (is.null(p)) return(NULL)
    tryCatch(suppressWarnings(soilKey::classify_ka5(p, rules = rules, on_missing = "silent")),
             error = function(e) NULL)
  })
  depth <- stats::setNames(seq_along(.ka5_isis_levels), .ka5_isis_levels)
  above <- list(classes = c("divisions", "classes"), types = c("divisions", "classes", "types"),
                subtypes = c("divisions", "classes", "types", "subtypes"))
  rows <- lapply(seq_along(de), function(i) {
    r <- sk[[i]]
    lv <- if (!is.null(r) && is.data.frame(r$levels)) r$levels else data.frame(level = character(0), code = character(0), name = character(0))
    pb <- as.character(r$provisional_because %||% character(0))
    pb_lv <- sub(":.*$", "", pb)
    out <- data.frame(profile = codes[i], error = is.null(r), stringsAsFactors = FALSE)
    for (L in .ka5_isis_levels) {
      j <- match(L, lv$level)
      out[[paste0("sk_", L)]] <- if (is.na(j)) NA_character_ else lv$code[j]
      out[[paste0("undecided_", L)]] <- any(pb_lv %in% above[[L]])
    }
    out$undecided_classes_list <- paste(sub("^classes:", "", grep("^classes:", pb, value = TRUE)), collapse = " ")
    out$sk_division <- { j <- match("divisions", lv$level); if (is.na(j)) NA_character_ else lv$code[j] }
    lm <- lv[lv$level %in% c("divisions", "classes", "types", "subtypes"), , drop = FALSE]
    out$sk_name <- if (nrow(lm)) paste(lm$code[nrow(lm)], lm$name[nrow(lm)]) else NA_character_
    out$sk_variety <- paste(lv$code[!lv$level %in% c("divisions", "classes", "types", "subtypes")], collapse = " ")
    out$sk_undecided <- paste(pb, collapse = "; ")
    out
  })
  d <- cbind(m, do.call(rbind, rows)[, -1])
  for (L in .ka5_isis_levels) {
    lab_L <- ifelse(is.na(d$level) | depth[d$level] < depth[[L]], NA_character_, code_at(d$code, L))
    d[[paste0("lab_", L)]] <- lab_L
    skL <- d[[paste0("sk_", L)]]
    a <- ifelse(is.na(lab_L), NA, !is.na(skL) & skL == lab_L)
    if (L == "subtypes") a[is.na(skL)] <- NA   # scored only where both sides have one
    d[[paste0("agree_", L)]] <- a
    d[[paste0("prov_", L)]] <- d[[paste0("undecided_", L)]]
  }
  score <- do.call(rbind, lapply(.ka5_isis_levels, function(L) {
    a <- d[[paste0("agree_", L)]]; s <- !is.na(a)
    k <- sum(a[s]); n <- sum(s); ci <- .ka5_isis_wilson(k, n)
    firm <- s & !d[[paste0("prov_", L)]]
    data.frame(level = L, labelled = sum(!is.na(d[[paste0("lab_", L)]])), n = n, agree = k,
               pct = if (n) 100 * k / n else NA_real_, lo = 100 * ci[1], hi = 100 * ci[2],
               n_firm = sum(firm), agree_firm = sum(a[firm]),
               stopped_above = sum(s & is.na(d[[paste0("sk_", L)]])),
               provisional = sum(d[[paste0("prov_", L)]][s]), stringsAsFactors = FALSE)
  }))
  status <- do.call(rbind, lapply(.ka5_isis_levels, function(L) {
    ok <- !d$error
    data.frame(level = L, classified = sum(ok), reached = sum(!is.na(d[[paste0("sk_", L)]][ok])),
               undecided = sum(d[[paste0("undecided_", L)]][ok]), stringsAsFactors = FALSE)
  }))
  und <- unlist(strsplit(d$undecided_classes_list[!d$error], " ", fixed = TRUE))
  undecided_classes <- sort(table(und[nzchar(und)]), decreasing = TRUE)
  res <- list(profiles = d, score = score, status = status,
              unmatched = d[!is.na(d$label) & is.na(d$code), c("profile", "label")],
              unlabelled = d$profile[is.na(d$label)],
              variants = d[!is.na(d$matched_by) & d$matched_by == "variant", c("profile", "part", "code", "name")],
              readings = d[!is.na(d$reading) & d$reading, c("profile", "part", "code", "name")],
              undecided_classes = undecided_classes,
              version = as.character(utils::packageVersion("soilKey")), n_germany = length(de),
              cache = basename(path))
  md <- .ka5_isis_report(res)
  if (!is.null(out)) {
    writeLines(md, out, useBytes = TRUE)
    if (verbose) message("Report written to ", out)
  } else if (verbose) {
    cat(md, sep = "\n")
  }
  invisible(res)
}

.ka5_isis_report <- function(res) {
  f1 <- function(x) ifelse(is.na(x), "n/a", sprintf("%.1f", x))
  d <- res$profiles; sc <- res$score; st <- res$status
  lines <- c(
    sprintf("# German KA5 (2005) on the ISIS profiles from Germany (soilKey %s)", res$version),
    "",
    sprintf("Offline run on the cache `%s`: %d profiles from Germany, %d with a local name, %d of them read as a KA5 class, type or subtype.",
            res$cache, res$n_germany, sum(!is.na(d$label)), sum(!is.na(d$code))),
    "The local names are older German usage, read through inst/rules/ka5/legacy_names.yaml; the designations are FAO style, so soilKey reads the horizons from the data.",
    "A level is scored on the profiles whose label reaches it; the subtype only where soilKey gives one too. 95 % intervals are Wilson intervals.",
    "",
    "## Agreement",
    "",
    "| Level | Labelled | n | Agree | % | 95 % CI | Firm n | Agree among firm | soilKey stops above | Provisional |",
    "|---|--:|--:|--:|--:|---|--:|--:|--:|--:|")
  for (i in seq_len(nrow(sc)))
    lines <- c(lines, sprintf("| %s | %d | %d | %d | %s | %s-%s | %d | %d | %d | %d |",
                              sc$level[i], sc$labelled[i], sc$n[i], sc$agree[i], f1(sc$pct[i]), f1(sc$lo[i]), f1(sc$hi[i]),
                              sc$n_firm[i], sc$agree_firm[i], sc$stopped_above[i], sc$provisional[i]))
  lines <- c(lines, "", "## How far soilKey keys (all profiles classified)", "",
             "| Level | Classified | Reached | Undecided at or above |", "|---|--:|--:|--:|")
  for (i in seq_len(nrow(st)))
    lines <- c(lines, sprintf("| %s | %d | %d | %d |", st$level[i], st$classified[i], st$reached[i], st$undecided[i]))
  uc <- res$undecided_classes
  lines <- c(lines, "", "Classes left undecided (profiles, of all those classified): ",
             if (length(uc)) paste(sprintf("%s %d", names(uc), as.integer(uc)), collapse = ", ") else "none.",
             sprintf("Profiles that could not be classified (no horizons): %d.", sum(d$error)))
  lines <- c(lines, "", "## Labels read through a spelling variant (.ka5_isis_variants)", "")
  lines <- c(lines, if (nrow(res$variants)) sprintf("- %s: \"%s\" read as %s %s", res$variants$profile, res$variants$part,
                                                     res$variants$code, res$variants$name) else "None.")
  lines <- c(lines, "", "## Labels read through a soilKey reading (readings.yaml)", "")
  lines <- c(lines, if (nrow(res$readings)) sprintf("- %s: \"%s\" read as %s %s", res$readings$profile, res$readings$part,
                                                     res$readings$code, res$readings$name) else "None.")
  lines <- c(lines, "", "## Labels not read", "")
  lines <- c(lines, if (nrow(res$unmatched)) sprintf("- %s: \"%s\"", res$unmatched$profile, res$unmatched$label) else "None.",
             sprintf("Profiles with no local name: %d.", length(res$unlabelled)))
  lines <- c(lines, "", "## Profiles", "",
             "| Profile | Label | Read as | soilKey | Class | Type | Subtype | Undecided at type |",
             "|---|---|---|---|---|---|---|---|")
  yn <- function(x) ifelse(is.na(x), "", ifelse(x, "yes", "no"))
  for (i in seq_len(nrow(d)))
    lines <- c(lines, sprintf("| %s | %s | %s | %s | %s | %s | %s | %s |", d$profile[i],
                              ifelse(is.na(d$label[i]), "(none)", d$label[i]),
                              ifelse(is.na(d$code[i]), "", paste(d$code[i], d$name[i])),
                              ifelse(d$error[i] | is.na(d$sk_name[i]), "(none)", d$sk_name[i]),
                              yn(d$agree_classes[i]), yn(d$agree_types[i]), yn(d$agree_subtypes[i]),
                              yn(d$prov_types[i])))
  lines <- c(lines, "", "Source: ISRIC World Soil Reference Collection (ISIS); see `soilKey::isis_citation()`.")
  lines
}

if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a)) b else a

if (sys.nframe() == 0L && !interactive()) {
  a <- commandArgs(trailingOnly = TRUE)
  ka5_isis_benchmark(cache = if (length(a) >= 1L) a[1] else NULL,
                     out = if (length(a) >= 2L) a[2] else NULL)
}
