# =============================================================================
# soilKey -- offline benchmark of the South African soil classification of
# 1991 (za1991) on the South African profiles of the Africa Soil Profiles
# database (AfSP 1.2, ISRIC) (v0.9.393; the second figure of the 1977 humic
# A, v0.9.394).
#
# AfSP gives most South African profiles their local classification (field
# LocalCls of AfSP012Qry_Profiles.dbf): a binomial of 1977 ("<Form>
# <Series>"), a form and family of 1991 ("<Form> Form, <Family> Family") or a
# form named alone. soilKey keys each profile with classify_za1991(); the
# form of the label is compared with soilKey's form, and the family of a 1991
# label with soilKey's family.
#
# OFFLINE. This script downloads nothing and queries no service. It reads
# the AfSP DBF files (AfSP012Qry_Profiles.dbf and AfSP012Qry_Layers.dbf;
# reading the layers takes a few minutes) from the directory given, in this
# order:
#   1. the first argument (or `afsp_dir =`);
#   2. the option soilKey.afsp_dir;
#   3. the environment variable SOILKEY_AFSP_DIR.
# Run as a script with no directory found, it says so and exits cleanly; from
# R, za1991_afsp_benchmark() stops with that message. AfSP is ISRIC's (CC
# BY-NC 4.0; Leenaars et al. 2014); none of its profiles or labels is shipped
# with soilKey.
#
# Usage:
#   Rscript za1991_afsp.R [afsp_dir] [report.md]
# The markdown report is written only where the second argument says;
# without it the summary is printed. From R:
#   source(system.file("benchmarks", "za1991_afsp.R", package = "soilKey"))
#   b <- za1991_afsp_benchmark(afsp_dir = "AfSP/extracted", out = "report.md")
# `pedons =` (a list from soilKey::load_afsp_pedons(dir, countries = "ZA"))
# skips the reading of the layers.
#
# Scoring (Hugo's decisions of 2026-11-10). Each label is read by
# soilKey:::.za_match_label(): exact form and family names only, the era
# from the shape of the label (not the survey year): a binomial is "1977",
# "<Form> Form, <Family> Family" is "1991", a form alone is "form only".
# A label not read is unmatched, never guessed; .za_afsp_variants is the
# explicit list of misspellings tried only then, and such matches are
# flagged.
#   * Form: soilKey's form equals the label's. A binomial of 1977 is scored
#     as its 1991 namesake (some forms were redefined in 1991). A profile
#     where soilKey names no form (a form test undecided, the land class
#     rock and lithosols, or no form at all) counts as a disagreement.
#   * Agreement carries a Wilson 95 % interval; overall and by era.
#   * Decided: soilKey names a form. Coverage = decided / scored; the
#     agreement among decided profiles; the provisional share among them (a
#     form tested before the assigned one left undecided, or a family left
#     undecided, or Lichtenburg).
#   * Candidates: where soilKey stops with forms undecided
#     (provisional_because "forms:<code>"), how often the label's form is
#     among them; beside it the median size of the lists, the hit rate with
#     the labels shuffled across those profiles (a baseline: 1000
#     permutations, fixed seed, the caller's random state kept) and the hit
#     rate by size of the list.
#   * Family: scored only where the label is a 1991 label naming a family
#     of its form and soilKey reaches a family. Series of 1977 are not
#     scored.
#   * The humic A of 1977 (A1, Hugo's decision of 2026-11-10): a second
#     figure beside the main one, which is not changed. For a label from a
#     survey before 1991 or written as a 1977 binomial (a label with
#     neither is not read so), soilKey's humic form with a humic A of 450
#     mm or less is scored as its orthic counterpart (Inanda as Hutton,
#     Magwa as Clovelly, Kranskop as Griffin, Sweetwater as Oakleaf, Nomanci
#     as Glenrosa; soilKey:::.za_humic_1977()): the humic A of 1977 had to
#     be thicker than 450 mm, the thin one came in 1991. Lusiki is left out
#     (no single orthic counterpart: Valsrivier or Swartland by what lies
#     below). Flagged in the report.
# =============================================================================

# Misspellings of the AfSP labels (regular expression, case ignored =
# replacement), tried only when the form of a label is not read as it
# stands (Hugo's decision, 2026-11-10). A trailing comma is kept and read.
.za_afsp_variants <- c("^Avanlon\\b" = "Avalon", "^Houhoek\\b" = "Houwhoek",
                       "^Houwbroek\\b" = "Houwhoek", "^Kraanskop\\b" = "Kranskop")

# Family names of 1991 labels that are not read (Hugo, 2026-11-10: text
# only, no family misspelling list). Reported beside the name when present.
.za_afsp_family_notes <- c(Glengazi = "a 1977 series name", Cleveland = "a 1977 series name",
                           Greendal = "looks like a misspelling of Greendale (Rg1000); not read")

# The baseline of the candidate hit rate: the labels of the profiles that
# stop shuffled across them, `B` times with a fixed seed; the random state
# of the caller is kept. `lab` the label forms, `cand` a list of the
# candidate codes of each profile.
.za_afsp_cand_baseline <- function(lab, cand, B = 1000L, seed = 1000L) {
  n <- length(lab)
  if (!n) return(c(mean = NA_real_, lo = NA_real_, hi = NA_real_))
  old <- if (exists(".Random.seed", envir = .GlobalEnv))
    get(".Random.seed", envir = .GlobalEnv) else NULL
  on.exit({
    if (!is.null(old)) assign(".Random.seed", old, envir = .GlobalEnv)
    else if (exists(".Random.seed", envir = .GlobalEnv))
      rm(".Random.seed", envir = .GlobalEnv)
  }, add = TRUE)
  set.seed(seed)
  r <- vapply(seq_len(B), function(b) {
    l <- lab[sample.int(n)]
    mean(vapply(seq_len(n), function(i) l[i] %in% cand[[i]], NA))
  }, 0)
  q <- stats::quantile(r, c(0.025, 0.975), names = FALSE)
  c(mean = mean(r), lo = q[1], hi = q[2])
}

.za_afsp_wilson <- function(k, n, z = stats::qnorm(0.975)) {
  if (!n) return(c(NA_real_, NA_real_))
  p <- k / n
  den <- 1 + z^2 / n
  mid <- (p + z^2 / (2 * n)) / den
  half <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / den
  c(max(0, mid - half), min(1, mid + half))
}

za1991_afsp_dir <- function(afsp_dir = NULL, must = TRUE) {
  cands <- c(afsp_dir, getOption("soilKey.afsp_dir", ""), Sys.getenv("SOILKEY_AFSP_DIR", ""))
  cands <- path.expand(as.character(cands[!is.na(cands) & nzchar(cands)]))
  hit <- cands[file.exists(file.path(cands, "AfSP012Qry_Profiles.dbf"))]
  if (!length(hit)) {
    msg <- paste0("No AfSP directory found (tried: ",
                  if (length(cands)) paste(cands, collapse = ", ") else "none given", "). ",
                  "Give the directory holding AfSP012Qry_Profiles.dbf and AfSP012Qry_Layers.dbf as the ",
                  "first argument, in the option soilKey.afsp_dir or in SOILKEY_AFSP_DIR; ",
                  "this script downloads nothing.")
    if (must) stop(msg, call. = FALSE)
    message(msg)
    return(NULL)
  }
  hit[1]
}

# The labels of the South African profiles: ProfileID, LocalCls, survey year.
za1991_afsp_labels <- function(afsp_dir) {
  if (!requireNamespace("foreign", quietly = TRUE))
    stop("This script needs the package foreign to read the AfSP DBF files.", call. = FALSE)
  prof <- foreign::read.dbf(file.path(afsp_dir, "AfSP012Qry_Profiles.dbf"), as.is = TRUE)
  prof <- prof[prof$Country %in% "ZA", , drop = FALSE]
  yr <- suppressWarnings(as.integer(prof$T_Year))
  yr[!is.na(yr) & yr < 0] <- NA_integer_
  data.frame(id = as.character(prof$ProfileID), label = as.character(prof$LocalCls),
             year = yr, stringsAsFactors = FALSE)
}

# soilKey's form and family of one result, with their status.
.za_afsp_result <- function(r) {
  na <- list(sk_form = NA_character_, sk_family = NA_character_, outcome = NA_character_,
             prov_form = NA, prov_family = NA, candidates = "", family_partial = NA_character_)
  if (is.null(r)) return(na)
  lv <- r$levels
  pb <- as.character(r$provisional_because %||% character(0))
  cand <- sub("^forms:", "", grep("^forms:", pb, value = TRUE))
  out <- na
  form <- if (is.data.frame(lv) && nrow(lv)) lv$code[lv$level == "forms"][1] else NA_character_
  if (is.na(form)) {
    out$outcome <- if (length(cand)) "undecided" else
      if (!is.null(r$qualifiers$za_land_class)) "land_class" else "no_form"
    out$candidates <- paste(cand, collapse = " ")
    return(out)
  }
  out$sk_form <- form
  out$outcome <- "decided"
  out$sk_family <- lv$code[lv$level == "families"][1]
  out$prov_form <- length(cand) > 0L
  out$prov_family <- any(startsWith(pb, "families:"))
  out$candidates <- paste(cand, collapse = " ")
  fam <- r$qualifiers$za_family
  if (is.list(fam) && !is.null(fam$partial)) out$family_partial <- as.character(fam$partial)[1]
  out
}

za1991_afsp_benchmark <- function(afsp_dir = NULL, out = NULL, pedons = NULL, labels = NULL,
                                  verbose = TRUE) {
  if (!"soilKey" %in% loadedNamespaces() &&
      !suppressPackageStartupMessages(requireNamespace("soilKey", quietly = TRUE)))
    stop("This script needs the soilKey package (0.9.394 or later); install it first.", call. = FALSE)
  ns <- asNamespace("soilKey")
  if (!exists(".za_match_label", envir = ns, inherits = FALSE) || !exists(".za_humic_1977", envir = ns, inherits = FALSE))
    stop("This script needs soilKey 0.9.394 or later.", call. = FALSE)
  match_label <- get(".za_match_label", envir = ns)
  humic_1977 <- get(".za_humic_1977", envir = ns)
  form_name <- get(".za_form_name", envir = ns)
  if (is.null(labels) || is.null(pedons)) {
    path <- za1991_afsp_dir(afsp_dir)
    if (is.null(labels)) labels <- za1991_afsp_labels(path)
    if (is.null(pedons)) {
      if (verbose) message("Reading the AfSP layers (a few minutes) ...")
      pedons <- soilKey::load_afsp_pedons(path, countries = "ZA", verbose = verbose)
    }
  }
  if (is.null(labels$year)) labels$year <- NA_integer_
  lab <- labels$label
  lab[!is.na(lab) & trimws(lab) %in% c("", "NA", "-9999")] <- NA_character_
  labels$label <- lab
  ids <- vapply(pedons, function(p) as.character(p$site$id %||% NA_character_), "")
  m <- match_label(labels$label, variants = .za_afsp_variants)
  d <- cbind(labels[c("id", "year")], m)
  d$has_label <- !is.na(d$label)
  d$read <- !is.na(d$form_code)
  d$pedon <- match(d$id, ids)
  todo <- which(d$read & !is.na(d$pedon))
  res <- vector("list", nrow(d))
  h77 <- data.frame(sk_form_1977 = rep(NA_character_, nrow(d)), humic_mm = NA_real_, humic_thin = FALSE,
                    stringsAsFactors = FALSE)
  for (j in seq_along(todo)) {
    k <- todo[j]
    if (verbose && (j %% 50L == 0L || j == length(todo))) message(sprintf("[%d/%d]", j, length(todo)))
    # res[k] <- list(): assigning NULL with [[ ]] would drop the element
    res[k] <- list(tryCatch(suppressWarnings(soilKey::classify_za1991(pedons[[d$pedon[k]]], on_missing = "silent")),
                            error = function(e) NULL))
    if (!is.null(res[[k]])) {
      hm <- humic_1977(res[[k]], pedons[[d$pedon[k]]])
      h77$sk_form_1977[k] <- hm$form; h77$humic_mm[k] <- hm$humic_mm; h77$humic_thin[k] <- hm$mapped
    }
  }
  st <- do.call(rbind, lapply(res, function(r) as.data.frame(.za_afsp_result(r), stringsAsFactors = FALSE)))
  d <- cbind(d, st, h77)
  d$error <- seq_len(nrow(d)) %in% todo & vapply(res, is.null, NA)
  d$scored <- seq_len(nrow(d)) %in% todo & !d$error
  d$agree_form <- ifelse(d$scored, !is.na(d$sk_form) & d$sk_form == d$form_code, NA)
  d$cand_hit <- ifelse(d$scored & nzchar(d$candidates),
                       vapply(seq_len(nrow(d)), function(i)
                         d$form_code[i] %in% strsplit(d$candidates[i], " ", fixed = TRUE)[[1]], NA), NA)
  # the humic A of 1977 (A1): labels from a survey before 1991 or in the
  # 1977 binomial shape
  d$before_1991 <- (!is.na(d$year) & d$year < 1991) | d$era %in% "1977"
  d$mapped_1977 <- d$scored & d$before_1991 & d$humic_thin
  d$form_1977 <- ifelse(d$mapped_1977, d$sk_form_1977, d$sk_form)
  d$agree_1977 <- ifelse(d$scored, !is.na(d$form_1977) & d$form_1977 == d$form_code, NA)
  d$family_scored <- d$scored & d$era %in% "1991" & !is.na(d$family_code) & !is.na(d$sk_family)
  d$agree_family <- ifelse(d$family_scored, d$sk_family == d$family_code, NA)
  pct <- function(k, n) if (n) 100 * k / n else NA_real_
  score_row <- function(group, x) {
    n <- nrow(x); k <- sum(x$agree_form); ci <- .za_afsp_wilson(k, n)
    dec <- x[x$outcome %in% "decided", , drop = FALSE]
    nd <- nrow(dec); kd <- sum(dec$agree_form); cid <- .za_afsp_wilson(nd, n); cia <- .za_afsp_wilson(kd, nd)
    und <- x[x$outcome %in% "undecided", , drop = FALSE]
    data.frame(group = group, n = n, agree = k, pct = pct(k, n), lo = 100 * ci[1], hi = 100 * ci[2],
               decided = nd, coverage = pct(nd, n), cov_lo = 100 * cid[1], cov_hi = 100 * cid[2],
               agree_decided = kd, pct_decided = pct(kd, nd), dec_lo = 100 * cia[1], dec_hi = 100 * cia[2],
               provisional = sum(dec$prov_form | dec$prov_family | dec$sk_form %in% "Lc"),
               provisional_form = sum(dec$prov_form | dec$sk_form %in% "Lc"),
               undecided = nrow(und), cand_hit = sum(und$cand_hit %in% TRUE),
               land_class = sum(x$outcome %in% "land_class"), no_form = sum(x$outcome %in% "no_form"),
               stringsAsFactors = FALSE)
  }
  sc <- d[d$scored, , drop = FALSE]
  era_name <- c("1977" = "1977 binomial (Form Series)", "1991" = "1991 (Form, Family)", form_only = "Form named alone")
  era_all <- c(era_name, unrecognised = "Shape not recognised")
  eras <- intersect(names(era_name), unique(sc$era))
  score <- rbind(score_row("All", sc),
                 do.call(rbind, lapply(eras, function(e) score_row(era_name[[e]], sc[sc$era == e, , drop = FALSE]))),
                 score_row("Read by a listed misspelling", sc[sc$matched_by %in% "variant", , drop = FALSE]))
  s77 <- sc
  s77$agree_form <- s77$agree_1977
  mp <- sc[sc$mapped_1977, , drop = FALSE]
  h77res <- list(score = score_row("All, the humic A of 1977 (surveys before 1991 or 1977 binomials)", s77),
                     mapped = nrow(mp), gained = sum(mp$agree_1977 & !mp$agree_form),
                     lost = sum(!mp$agree_1977 & mp$agree_form),
                     changes = if (nrow(mp)) {
                       t <- as.data.frame(table(label = mp$form_code, soilKey = mp$sk_form, read_1977 = mp$form_1977),
                                          stringsAsFactors = FALSE)
                       t <- t[t$Freq > 0, , drop = FALSE]
                       t[order(-t$Freq, t$label), , drop = FALSE]
                     } else NULL)
  status <- data.frame(
    profiles = nrow(d), labelled = sum(d$has_label), read = sum(d$read),
    variant = sum(d$matched_by %in% "variant"), unmatched = sum(d$has_label & !d$read),
    no_pedon = sum(d$read & is.na(d$pedon)), error = sum(d$error), scored = sum(d$scored),
    stringsAsFactors = FALSE)
  era_lab <- ifelse(is.na(d$era), "unrecognised", d$era)
  by_era <- as.data.frame.matrix(table(factor(era_lab[d$has_label], levels = c(names(era_name), "unrecognised")),
                                       factor(ifelse(d$read[d$has_label], "read", "unmatched"),
                                              levels = c("read", "unmatched"))))
  # the survey years of each era (the era is read from the shape, not the year)
  years <- lapply(stats::setNames(eras, eras), function(e) {
    y <- sc$year[sc$era == e]
    c(n = length(y), with_year = sum(!is.na(y)),
      min = if (any(!is.na(y))) min(y, na.rm = TRUE) else NA_integer_,
      max = if (any(!is.na(y))) max(y, na.rm = TRUE) else NA_integer_,
      before_1991 = sum(y < 1991, na.rm = TRUE), from_1991 = sum(y >= 1991, na.rm = TRUE))
  })
  # the survey years of the labels read in the 1991 shape
  y91 <- d$year[d$read & d$era %in% "1991"]
  era91 <- c(n = length(y91), with_year = sum(!is.na(y91)), before_1991 = sum(y91 < 1991, na.rm = TRUE))
  # candidates of the profiles that stop: list size, baseline, hit by size
  und <- sc[sc$outcome %in% "undecided", , drop = FALSE]
  cl <- lapply(und$candidates, function(z) strsplit(z, " ", fixed = TRUE)[[1]])
  size <- lengths(cl)
  bins <- cut(size, c(0, 5, 20, 40, Inf), labels = c("1-5", "6-20", "21-40", "over 40"))
  by_size <- data.frame(size = levels(bins), n = as.integer(table(bins)),
                        hit = as.integer(tapply(und$cand_hit %in% TRUE, bins, sum)), stringsAsFactors = FALSE)
  by_size$hit[is.na(by_size$hit)] <- 0L
  dis_all <- sc[sc$outcome %in% "decided" & !sc$agree_form, , drop = FALSE]
  candidates <- list(n = nrow(und), hit = sum(und$cand_hit %in% TRUE),
                     median_size = if (nrow(und)) stats::median(size) else NA_real_,
                     baseline = .za_afsp_cand_baseline(und$form_code, cl), by_size = by_size,
                     decided_disagree = nrow(dis_all),
                     decided_disagree_hit = sum(vapply(seq_len(nrow(dis_all)), function(i)
                       dis_all$form_code[i] %in% strsplit(dis_all$candidates[i], " ", fixed = TRUE)[[1]], NA)))
  # family
  f91 <- sc[sc$era %in% "1991", , drop = FALSE]
  fs <- f91[f91$family_scored, , drop = FALSE]
  fci <- .za_afsp_wilson(sum(fs$agree_family), nrow(fs))
  family <- data.frame(labels_1991 = nrow(f91), family_read = sum(!is.na(f91$family_code)),
                       family_unread = sum(!is.na(f91$family_unread)),
                       form_agrees = sum(!is.na(f91$family_code) & f91$agree_form),
                       sk_family = nrow(fs), agree = sum(fs$agree_family), pct = pct(sum(fs$agree_family), nrow(fs)),
                       lo = 100 * fci[1], hi = 100 * fci[2],
                       scored_form_agrees = sum(fs$agree_form), agree_form_agrees = sum(fs$agree_family & fs$agree_form),
                       stringsAsFactors = FALSE)
  # per form of the label
  per_form <- do.call(rbind, lapply(split(sc, sc$form_code), function(x) {
    got <- ifelse(is.na(x$sk_form), "[no form named]", x$sk_form)
    other <- got[!x$agree_form]
    top <- if (length(other)) names(sort(table(other), decreasing = TRUE))[1] else ""
    data.frame(form = x$form_code[1], name = x$form_name[1], n = nrow(x), agree = sum(x$agree_form),
               decided = sum(x$outcome %in% "decided"), agree_decided = sum(x$agree_form & x$outcome %in% "decided"),
               undecided = sum(x$outcome %in% "undecided"), cand_hit = sum(x$cand_hit %in% TRUE & x$outcome %in% "undecided"),
               top_other = top, top_other_n = if (nzchar(top)) sum(other == top) else 0L, stringsAsFactors = FALSE)
  }))
  per_form <- per_form[order(-per_form$n, per_form$form), , drop = FALSE]
  rownames(per_form) <- NULL
  # confusions among decided profiles (label form -> soilKey form)
  dis <- sc[sc$outcome %in% "decided" & !sc$agree_form, , drop = FALSE]
  confusion <- if (nrow(dis)) {
    t <- as.data.frame(table(label = dis$form_code, soilKey = dis$sk_form), stringsAsFactors = FALSE)
    t <- t[t$Freq > 0, , drop = FALSE]
    t[order(-t$Freq, t$label, t$soilKey), , drop = FALSE]
  } else data.frame(label = character(0), soilKey = character(0), Freq = integer(0))
  rownames(confusion) <- NULL
  # the forms left undecided in the profiles that stop
  u <- unlist(strsplit(sc$candidates[sc$outcome %in% "undecided"], " ", fixed = TRUE))
  undecided <- sort(table(u[nzchar(u)]), decreasing = TRUE)
  out_res <- list(records = d, score = score, status = status, by_era = by_era, years = years,
                  family = family, era91 = era91, candidates = candidates,
                  family_unread = sort(unique(d$family_unread[d$scored & !is.na(d$family_unread)])),
                  per_form = per_form, confusion = confusion, undecided = undecided, humic_1977 = h77res,
                  era_name = era_all, form_name = form_name,
                  version = as.character(utils::packageVersion("soilKey")))
  md <- .za_afsp_report(out_res)
  if (!is.null(out)) {
    writeLines(md, out, useBytes = TRUE)
    if (verbose) message("Report written to ", out)
  } else if (verbose) {
    cat(md, sep = "\n")
  }
  invisible(out_res)
}

.za_afsp_report <- function(res) {
  f1 <- function(x) ifelse(is.na(x), "n/a", sprintf("%.1f", x))
  ci <- function(lo, hi) sprintf("%s-%s", f1(lo), f1(hi))
  fname <- function(code) { n <- res$form_name(code); ifelse(is.na(n), code, n) }
  st <- res$status; sc <- res$score; fm <- res$family
  e91 <- res$era91
  era91_txt <- if (!e91[["n"]]) "No label is in the 1991 shape." else
    if (e91[["with_year"]] == e91[["n"]] && e91[["before_1991"]] == e91[["n"]])
      sprintf(paste("All %d labels in the 1991 shape come from surveys before 1991, so they were converted to",
                    "the 1991 system later, by the compilers."), e91[["n"]]) else
      sprintf("Of the %d labels in the 1991 shape, %d have a survey year and %d of them are before 1991.",
              e91[["n"]], e91[["with_year"]], e91[["before_1991"]])
  lines <- c(
    sprintf("# South African soil classification 1991 on the AfSP profiles (soilKey %s)", res$version), "",
    sprintf(paste("Offline run on the South African profiles of the Africa Soil Profiles database 1.2 (ISRIC):",
                  "%d profiles, %d with a local label (LocalCls), %d labels read to a form, %d scored."),
            st$profiles, st$labelled, st$read, st$scored),
    paste("The form of each label is compared with the form of `classify_za1991()`. 95 % intervals are Wilson intervals.",
          "A profile where soilKey names no form counts as a disagreement in the main figure."),
    paste("A binomial of 1977 is scored at the form level as its 1991 namesake; some forms were redefined in 1991,",
          "so part of the disagreement on the 1977 labels may come from the change of definition, not from soilKey."),
    "The era of a label is read from its shape, not from the survey year.",
    era91_txt, "",
    "## Labels read", "",
    "| Profiles | Labelled | Read to a form | of which by a listed misspelling | Unmatched | Read, no profile built | Not run | Scored |",
    "|--:|--:|--:|--:|--:|--:|--:|--:|",
    sprintf("| %d | %d | %d | %d | %d | %d | %d | %d |", st$profiles, st$labelled, st$read, st$variant,
            st$unmatched, st$no_pedon, st$error, st$scored), "",
    "Labels by shape:", "",
    "| Era (shape) | Read | Unmatched |", "|---|--:|--:|")
  be <- res$by_era
  for (e in rownames(be))
    lines <- c(lines, sprintf("| %s | %d | %d |", res$era_name[[e]], be[e, "read"], be[e, "unmatched"]))
  unm <- res$records[res$records$has_label & !res$records$read, , drop = FALSE]
  lines <- c(lines, "", sprintf("Unmatched labels (never guessed): %d.", nrow(unm)),
             "Read, no profile built: the AfSP loader keeps only profiles with a WRB 2006 group.")
  v <- res$records[res$records$matched_by %in% "variant", , drop = FALSE]
  if (nrow(v)) {
    vt <- table(v$form_name)
    lines <- c(lines, sprintf("Read by the listed misspellings (flagged): %s.",
                              paste(sprintf("%s %d", names(vt), as.integer(vt)), collapse = ", ")))
  }
  if (length(res$years)) {
    lines <- c(lines, "", "Survey years of the scored labels, by shape:", "",
               "| Era (shape) | Scored | With a year | Earliest | Latest | Before 1991 | 1991 or later |",
               "|---|--:|--:|--:|--:|--:|--:|")
    for (e in names(res$years)) {
      y <- res$years[[e]]
      lines <- c(lines, sprintf("| %s | %d | %d | %s | %s | %d | %d |", res$era_name[[e]], y[["n"]], y[["with_year"]],
                                ifelse(is.na(y[["min"]]), "n/a", y[["min"]]), ifelse(is.na(y[["max"]]), "n/a", y[["max"]]),
                                y[["before_1991"]], y[["from_1991"]]))
    }
  }
  lines <- c(lines, "", "## Form agreement", "",
             "| Labels | n | Agree | % | 95 % CI | Decided | Coverage % | 95 % CI | Agree among decided | % decided | 95 % CI | Provisional (decided) | Undecided | Label's form among the undecided | Rock and lithosols | No form at all (nothing undecided) |",
             "|---|--:|--:|--:|---|--:|--:|---|--:|--:|---|--:|--:|--:|--:|--:|")
  for (i in seq_len(nrow(sc)))
    lines <- c(lines, sprintf("| %s | %d | %d | %s | %s | %d | %s | %s | %d | %s | %s | %d | %d | %d | %d | %d |",
                              sc$group[i], sc$n[i], sc$agree[i], f1(sc$pct[i]), ci(sc$lo[i], sc$hi[i]),
                              sc$decided[i], f1(sc$coverage[i]), ci(sc$cov_lo[i], sc$cov_hi[i]),
                              sc$agree_decided[i], f1(sc$pct_decided[i]), ci(sc$dec_lo[i], sc$dec_hi[i]),
                              sc$provisional[i], sc$undecided[i], sc$cand_hit[i], sc$land_class[i], sc$no_form[i]))
  a <- sc[sc$group == "All", ]
  lines <- c(lines, "",
             paste("Decided: soilKey names a form. Provisional (decided): a form tested before the one named, or a",
                   "family of it, was left undecided, or the form is Lichtenburg;",
                   sprintf("%d of %d decided (%s %%), %d of them at the form level.", a$provisional, a$decided,
                           f1(if (a$decided) 100 * a$provisional / a$decided else NA), a$provisional_form)),
             paste("Undecided: soilKey stops with forms undecided (provisional_because `forms:<code>`);",
                   sprintf("the label's form is among them for %d of %d (%s %%).", a$cand_hit, a$undecided,
                           f1(if (a$undecided) 100 * a$cand_hit / a$undecided else NA))))
  cd <- res$candidates
  if (cd$n) {
    bl <- cd$baseline
    lines <- c(lines,
               sprintf(paste("Baseline: the candidate lists hold a median of %s forms; with the labels shuffled across",
                             "the %d profiles that stop (1000 permutations, fixed seed) the hit rate is %s %% (95 %% range %s-%s)."),
                       format(cd$median_size), cd$n, f1(100 * bl[["mean"]]), f1(100 * bl[["lo"]]), f1(100 * bl[["hi"]])),
               "", "| Candidate forms left undecided | Profiles | Label's form among them | % |", "|---|--:|--:|--:|")
    bs <- cd$by_size
    for (i in seq_len(nrow(bs)))
      lines <- c(lines, sprintf("| %s | %d | %d | %s |", bs$size[i], bs$n[i], bs$hit[i],
                                f1(if (bs$n[i]) 100 * bs$hit[i] / bs$n[i] else NA)))
    lines <- c(lines, "")
  }
  lines <- c(lines, sprintf(paste("Not in the rate: %d of the %d decided disagreements have the label's form among the forms",
                                  "left undecided before the one assigned."), cd$decided_disagree_hit, cd$decided_disagree))
  h7 <- res$humic_1977
  if (!is.null(h7)) {
    s7 <- h7$score
    lines <- c(lines, "", "## Second figure: the humic A of 1977 (flagged)", "",
               paste("The key is not changed and the main figure above stands. The humic A of 1977 had to be thicker",
                     "than 450 mm; the thin humic A came with the second edition (1991). Here, for labels from a survey",
                     "before 1991 or written as a 1977 binomial (a label with neither is not read so), soilKey's humic",
                     "form with a humic A of 450 mm or less is scored as its orthic counterpart (Inanda as Hutton,",
                     "Magwa as Clovelly, Kranskop as Griffin, Sweetwater as Oakleaf, Nomanci as Glenrosa). Lusiki is",
                     "left out: it has no single orthic counterpart (Valsrivier or Swartland by what lies below the",
                     "pedocutanic B). The organic carbon is not corrected."), "",
               "| Labels | n | Agree | % | 95 % CI | Decided | Coverage % | Agree among decided | % decided | 95 % CI |",
               "|---|--:|--:|--:|---|--:|--:|--:|--:|---|",
               sprintf("| %s | %d | %d | %s | %s | %d | %s | %d | %s | %s |", s7$group, s7$n, s7$agree, f1(s7$pct),
                       ci(s7$lo, s7$hi), s7$decided, f1(s7$coverage), s7$agree_decided, f1(s7$pct_decided),
                       ci(s7$dec_lo, s7$dec_hi)), "",
               sprintf("Profiles read so: %d; agreement gained %d, lost %d.", h7$mapped, h7$gained, h7$lost))
    if (!is.null(h7$changes) && nrow(h7$changes)) {
      lines <- c(lines, "", "| Label | soilKey (1991) | Read with the humic A of 1977 | n |", "|---|---|---|--:|")
      ch <- h7$changes
      for (i in seq_len(nrow(ch)))
        lines <- c(lines, sprintf("| %s | %s | %s | %d |", fname(ch$label[i]), fname(ch$soilKey[i]),
                                  fname(ch$read_1977[i]), ch$Freq[i]))
    }
  }
  lines <- c(lines, "", "## Family agreement (1991 labels only)", "",
             paste("Scored only where the label names a family of its form (inst/rules/za1991/families.yaml) and",
                   "soilKey reaches a family; the series of the 1977 binomials are not scored."), "",
             "| 1991 labels scored | Family read | Family name not of its form | Form agrees (family read) | soilKey reaches a family | Agree | % | 95 % CI | of which form agrees | Agree where form agrees |",
             "|--:|--:|--:|--:|--:|--:|--:|---|--:|--:|",
             sprintf("| %d | %d | %d | %d | %d | %d | %s | %s | %d | %d |", fm$labels_1991, fm$family_read,
                     fm$family_unread, fm$form_agrees, fm$sk_family, fm$agree, f1(fm$pct), ci(fm$lo, fm$hi),
                     fm$scored_form_agrees, fm$agree_form_agrees))
  if (length(res$family_unread)) {
    nt <- .za_afsp_family_notes[res$family_unread]
    lines <- c(lines, "", sprintf("Family names not read under their form: %s.",
                                  paste(ifelse(is.na(nt), res$family_unread, paste0(res$family_unread, " (", nt, ")")),
                                        collapse = ", ")))
  }
  pf <- res$per_form
  lines <- c(lines, "", "## By form of the label", "",
             "| Form | n | Agree | Decided | Agree among decided | Undecided | Label's form among the undecided | Most frequent other answer |",
             "|---|--:|--:|--:|--:|--:|--:|---|")
  for (i in seq_len(nrow(pf)))
    lines <- c(lines, sprintf("| %s (%s) | %d | %d | %d | %d | %d | %d | %s |", pf$name[i], pf$form[i], pf$n[i],
                              pf$agree[i], pf$decided[i], pf$agree_decided[i], pf$undecided[i], pf$cand_hit[i],
                              if (nzchar(pf$top_other[i])) sprintf("%s (%d)",
                                ifelse(pf$top_other[i] == "[no form named]", "no form named (undecided or none)", fname(pf$top_other[i])),
                                pf$top_other_n[i]) else ""))
  cf <- res$confusion
  lines <- c(lines, "", "## Confusions among decided profiles (label -> soilKey)", "")
  if (nrow(cf)) {
    lines <- c(lines, "| Label | soilKey | n |", "|---|---|--:|")
    for (i in seq_len(min(nrow(cf), 25L)))
      lines <- c(lines, sprintf("| %s | %s | %d |", fname(cf$label[i]), fname(cf$soilKey[i]), cf$Freq[i]))
  } else lines <- c(lines, "None.")
  u <- res$undecided
  lines <- c(lines, "", sprintf("Forms left undecided in the profiles that stop (profiles): %s",
                                if (length(u)) paste(sprintf("%s %d", fname(names(u)), as.integer(u)), collapse = ", ") else "none."))
  lines <- c(lines, "", paste("Source: Leenaars, J. G. B., van Oostrum, A. J. M. and Ruiperez Gonzalez, M. (2014).",
                              "Africa Soil Profiles Database, Version 1.2. ISRIC Report 2014/01. ISRIC, Wageningen (CC BY-NC 4.0)."))
  lines
}

if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a)) b else a

if (sys.nframe() == 0L && !interactive()) {
  a <- commandArgs(trailingOnly = TRUE)
  if (is.null(za1991_afsp_dir(if (length(a) >= 1L) a[1] else NULL, must = FALSE))) quit(save = "no", status = 0L)
  za1991_afsp_benchmark(afsp_dir = if (length(a) >= 1L) a[1] else NULL,
                        out = if (length(a) >= 2L) a[2] else NULL)
}
