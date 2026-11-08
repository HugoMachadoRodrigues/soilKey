# =============================================================================
# soilKey -- offline benchmark of the Chinese Soil Taxonomy (2001) on the
# ISIS reference profiles from China (v0.9.378).
#
# ISRIC's ISIS holds 51 profiles from China (CN001-CN051). Their local
# classification gives the 1991 first-proposal name and, beside it, a name in
# the Chinese Soil Taxonomy ("Haplic latosol; Hapli-Udic Ferrosol"), mostly at
# group level. The labels were given by ISSAS, probably on the Chinese
# edition, so the group is the fair target; soilKey keys the English edition
# of 2001 (classify_cst2001()).
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
#   Rscript cst2001_isis.R [cache.rds] [report.md]
# The markdown report is written only where the second argument says;
# without it the summary is printed. From R:
#   source(system.file("benchmarks", "cst2001_isis.R", package = "soilKey"))
#   b <- cst2001_isis_benchmark(cache = "profiles.rds", out = "report.md")
#
# Scoring. The CST part of each label is matched to a taxon of the key by
# soilKey:::.cst_match_label() (singular or plural, the printed aliases, and
# names one letter off a single taxon, flagged as typos). A level is scored
# only on the profiles whose label reaches it (the order on every matched
# label, the subgroup on the one subgroup label). Agreement carries a Wilson
# 95 % interval. The class soilKey assigns at a level is provisional when a
# class tested before it at that level or above could not be decided, rests
# on a reading the book leaves open, or rests on a stand-in of a group key.
# =============================================================================

.cst_isis_levels <- c("orders", "suborders", "groups", "subgroups")

.cst_isis_wilson <- function(k, n, z = stats::qnorm(0.975)) {
  if (!n) return(c(NA_real_, NA_real_))
  p <- k / n
  den <- 1 + z^2 / n
  mid <- (p + z^2 / (2 * n)) / den
  half <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / den
  c(max(0, mid - half), min(1, mid + half))
}

cst2001_isis_cache <- function(cache = NULL) {
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

cst2001_isis_benchmark <- function(cache = NULL, out = NULL, verbose = TRUE) {
  if (!"soilKey" %in% loadedNamespaces())
    suppressPackageStartupMessages(requireNamespace("soilKey"))
  ns <- asNamespace("soilKey")
  if (!exists(".cst_match_label", envir = ns, inherits = FALSE))
    stop("This script needs soilKey 0.9.378 or later.", call. = FALSE)
  match_label <- get(".cst_match_label", envir = ns)
  code_at <- get(".cst_code_at", envir = ns)
  path <- cst2001_isis_cache(cache)
  raw <- readRDS(path)
  cn <- Filter(function(p) identical(p$countryCode, "CN") &&
                 grepl("^CN0[0-9]{2}$", p$profileCode %||% ""), raw)
  if (!length(cn)) stop("The cache holds no ISIS profile from China.", call. = FALSE)
  cn <- cn[order(vapply(cn, `[[`, "", "profileCode"))]
  codes <- vapply(cn, `[[`, "", "profileCode")
  lab <- vapply(cn, function(p) {
    lc <- p$localClassifications
    v <- unlist(lapply(lc, function(z) z$classificationName))
    if (length(v)) paste(v, collapse = " | ") else NA_character_
  }, "")
  m <- match_label(lab)
  m$profile <- codes
  rules <- soilKey::load_rules("cst2001")
  sk <- lapply(seq_along(cn), function(i) {
    if (verbose) message(sprintf("[%d/%d] %s", i, length(cn), codes[i]))
    p <- tryCatch(soilKey::isis_profile_to_pedon(cn[[i]]), error = function(e) NULL)
    if (is.null(p)) return(NULL)
    tryCatch(suppressWarnings(soilKey::classify_cst2001(p, rules = rules, on_missing = "silent")),
             error = function(e) NULL)
  })
  # soilKey's class and its status at each level
  depth <- stats::setNames(seq_along(.cst_isis_levels), .cst_isis_levels)
  rows <- lapply(seq_along(cn), function(i) {
    r <- sk[[i]]
    lv <- if (!is.null(r) && is.data.frame(r$levels)) r$levels else data.frame(level = character(0), code = character(0), name = character(0))
    pb <- as.character(r$provisional_because %||% character(0))
    w <- as.character(r$warnings %||% character(0))
    open_codes <- sub("^Provisional: ([A-N][0-9.]*) .*$", "\\1", grep("^Provisional: [A-N][0-9.]* .* rests on", w, value = TRUE))
    stand_codes <- sub("^Stand-in: ([A-N][0-9.]*) .*$", "\\1", grep("^Stand-in: [A-N][0-9.]* ", w, value = TRUE))
    lvl_of <- function(cd) lv$level[match(cd, lv$code)]
    out <- data.frame(profile = codes[i], error = is.null(r), stringsAsFactors = FALSE)
    for (L in .cst_isis_levels) {
      j <- match(L, lv$level)
      out[[paste0("sk_", L)]] <- if (is.na(j)) NA_character_ else lv$code[j]
      out[[paste0("skname_", L)]] <- if (is.na(j)) NA_character_ else lv$name[j]
      upto <- .cst_isis_levels[seq_len(depth[[L]])]
      pb_lv <- sub(":.*$", "", pb)
      out[[paste0("undecided_", L)]] <- any(pb_lv %in% upto)
      out[[paste0("open_", L)]] <- any(lvl_of(open_codes) %in% upto)
      out[[paste0("standin_", L)]] <- any(lvl_of(stand_codes) %in% upto)
    }
    out$sk_name <- if (nrow(lv)) paste(lv$code[nrow(lv)], lv$name[nrow(lv)]) else NA_character_
    out$alternative <- if (!is.null(r) && is.data.frame(r$candidates) && nrow(r$candidates))
      paste(r$candidates$code, r$candidates$name, collapse = "; ") else NA_character_
    out
  })
  d <- cbind(m, do.call(rbind, rows)[, -1])
  for (L in .cst_isis_levels) {
    d[[paste0("lab_", L)]] <- ifelse(is.na(d$level) | depth[d$level] < depth[[L]], NA_character_,
                                     code_at(d$code, L))
    d[[paste0("agree_", L)]] <- ifelse(is.na(d[[paste0("lab_", L)]]), NA,
                                       !is.na(d[[paste0("sk_", L)]]) & d[[paste0("sk_", L)]] == d[[paste0("lab_", L)]])
    d[[paste0("prov_", L)]] <- d[[paste0("undecided_", L)]] | d[[paste0("open_", L)]] | d[[paste0("standin_", L)]]
  }
  # the scores
  score <- do.call(rbind, lapply(.cst_isis_levels, function(L) {
    a <- d[[paste0("agree_", L)]]; s <- !is.na(a)
    k <- sum(a[s]); n <- sum(s); ci <- .cst_isis_wilson(k, n)
    pv <- d[[paste0("prov_", L)]][s]
    firm <- s & !d[[paste0("prov_", L)]]
    data.frame(level = L, n = n, agree = k, pct = if (n) 100 * k / n else NA_real_,
               lo = 100 * ci[1], hi = 100 * ci[2],
               n_firm = sum(firm), agree_firm = sum(a[firm]),
               stopped_above = sum(s & is.na(d[[paste0("sk_", L)]])),
               provisional = sum(pv), stringsAsFactors = FALSE)
  }))
  status <- do.call(rbind, lapply(.cst_isis_levels, function(L) {
    ok <- !d$error
    data.frame(level = L, classified = sum(ok),
               undecided_before = sum(d[[paste0("undecided_", L)]][ok]),
               open_reading = sum(d[[paste0("open_", L)]][ok]),
               stand_in = sum(d[[paste0("standin_", L)]][ok]),
               any_provisional = sum(d[[paste0("prov_", L)]][ok]), stringsAsFactors = FALSE)
  }))
  ord_names <- vapply(rules$orders, function(e) as.character(e$name), "")
  names(ord_names) <- vapply(rules$orders, function(e) as.character(e$code), "")
  so <- !is.na(d$lab_orders)
  conf <- table(label = factor(ord_names[d$lab_orders[so]], levels = ord_names),
                soilKey = factor(ifelse(is.na(d$sk_orders[so]), "[none]", ord_names[d$sk_orders[so]]),
                                 levels = c(ord_names, "[none]")))
  conf <- conf[rowSums(conf) > 0, colSums(conf) > 0, drop = FALSE]
  res <- list(profiles = d, score = score, status = status, confusion = conf,
              unmatched = d[is.na(d$code), c("profile", "label")],
              typos = d[!is.na(d$matched_by) & d$matched_by == "typo", c("profile", "part", "code", "name")],
              version = as.character(utils::packageVersion("soilKey")), n_china = length(cn),
              cache = basename(path))
  md <- .cst_isis_report(res)
  if (!is.null(out)) {
    writeLines(md, out, useBytes = TRUE)
    if (verbose) message("Report written to ", out)
  } else if (verbose) {
    cat(md, sep = "\n")
  }
  invisible(res)
}

.cst_isis_report <- function(res) {
  f1 <- function(x) ifelse(is.na(x), "n/a", sprintf("%.1f", x))
  d <- res$profiles; sc <- res$score; st <- res$status
  lines <- c(
    sprintf("# Chinese Soil Taxonomy (2001) on the ISIS profiles from China (soilKey %s)", res$version),
    "",
    sprintf("Offline run on the cache `%s`: %d profiles from China, %d with a CST name matched to a taxon of the key, %d without.",
            res$cache, res$n_china, sum(!is.na(d$code)), sum(is.na(d$code))),
    "The labels were given by ISSAS, probably on the Chinese edition; soilKey keys the English edition of 2001.",
    "A level is scored on the profiles whose label reaches it. 95 % intervals are Wilson intervals.",
    "",
    "## Agreement",
    "",
    "| Level | n | Agree | % | 95 % CI | Firm n | Agree among firm | soilKey stops above | Provisional |",
    "|---|--:|--:|--:|---|--:|--:|--:|--:|")
  for (i in seq_len(nrow(sc)))
    lines <- c(lines, sprintf("| %s | %d | %d | %s | %s-%s | %d | %d | %d | %d |",
                              sc$level[i], sc$n[i], sc$agree[i], f1(sc$pct[i]), f1(sc$lo[i]), f1(sc$hi[i]),
                              sc$n_firm[i], sc$agree_firm[i], sc$stopped_above[i], sc$provisional[i]))
  lines <- c(lines, "", "## Provisional classes (all profiles classified)", "",
             "Cumulative: a class is counted at a level when the reason lies at that level or above.", "",
             "| Level | Classified | Class undecided before | Open reading | Stand-in | Any |",
             "|---|--:|--:|--:|--:|--:|")
  for (i in seq_len(nrow(st)))
    lines <- c(lines, sprintf("| %s | %d | %d | %d | %d | %d |", st$level[i], st$classified[i],
                              st$undecided_before[i], st$open_reading[i], st$stand_in[i], st$any_provisional[i]))
  cf <- res$confusion
  lines <- c(lines, "", "## Order confusion (rows: label; columns: soilKey)", "",
             paste0("| | ", paste(colnames(cf), collapse = " | "), " |"),
             paste0("|---|", paste(rep("--:", ncol(cf)), collapse = "|"), "|"))
  for (i in seq_len(nrow(cf)))
    lines <- c(lines, paste0("| ", rownames(cf)[i], " | ", paste(cf[i, ], collapse = " | "), " |"))
  lines <- c(lines, "", "## Labels matched as typos", "")
  lines <- c(lines, if (nrow(res$typos)) sprintf("- %s: \"%s\" read as %s %s", res$typos$profile, res$typos$part,
                                                  res$typos$code, res$typos$name) else "None.")
  lines <- c(lines, "", "## Labels with no CST taxon", "")
  lines <- c(lines, if (nrow(res$unmatched)) sprintf("- %s: \"%s\"", res$unmatched$profile, res$unmatched$label) else "None.")
  lines <- c(lines, "", "## Profiles", "",
             "| Profile | Label (CST part) | Label code | soilKey | Order | Suborder | Group | Subgroup | Provisional at group |",
             "|---|---|---|---|---|---|---|---|---|")
  yn <- function(x) ifelse(is.na(x), "", ifelse(x, "yes", "no"))
  for (i in seq_len(nrow(d)))
    lines <- c(lines, sprintf("| %s | %s | %s | %s | %s | %s | %s | %s | %s |", d$profile[i],
                              ifelse(is.na(d$part[i]), "(none)", d$part[i]), ifelse(is.na(d$code[i]), "", d$code[i]),
                              ifelse(d$error[i] | is.na(d$sk_name[i]), "(none)", d$sk_name[i]),
                              yn(d$agree_orders[i]), yn(d$agree_suborders[i]), yn(d$agree_groups[i]),
                              yn(d$agree_subgroups[i]), yn(d$prov_groups[i])))
  lines <- c(lines, "", "Source: ISRIC World Soil Reference Collection (ISIS); see `soilKey::isis_citation()`.")
  lines
}

if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a)) b else a

if (sys.nframe() == 0L && !interactive()) {
  a <- commandArgs(trailingOnly = TRUE)
  cst2001_isis_benchmark(cache = if (length(a) >= 1L) a[1] else NULL,
                         out = if (length(a) >= 2L) a[2] else NULL)
}
