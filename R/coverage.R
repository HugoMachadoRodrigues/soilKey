# =============================================================
# Honest taxonomic-completeness measurement
# =============================================================
# `coverage_report()` reports, for a classification system, exactly which
# canonical taxa/qualifiers the package's rule base registers and which it
# does not -- an auditable replacement for hand-maintained "100% coverage"
# claims. Comparison is by NAME, never by code: soilKey's internal
# great-group codes diverge from the SoilTaxonomy 13th-edition codes for 34
# great groups (e.g. Hydrudands/Melanudands are swapped), so a code-set diff
# would be meaningless. Names are the stable, authoritative key.

#' Subgroup full-names registered in the USDA subgroup rule base.
#'
#' Reads every \code{inst/rules/usda/subgroups/<order>.yaml} and returns the
#' union of subgroup \code{name} fields (e.g. \code{"Typic Hapludands"}).
#'
#' @return Character vector of registered subgroup names (lower-cased, trimmed).
#' @keywords internal
.coverage_registered_usda_subgroups <- function() {
  dir <- system.file("rules", "usda", "subgroups", package = "soilKey")
  if (!nzchar(dir) || !dir.exists(dir)) return(character(0))
  files <- list.files(dir, pattern = "\\.yaml$", full.names = TRUE)
  nms <- character(0)
  for (f in files) {
    y <- yaml::read_yaml(f)$subgroups
    if (is.null(y)) next
    for (gg in y) for (e in gg) if (!is.null(e$name)) nms <- c(nms, e$name)
  }
  unique(tolower(trimws(nms)))
}

#' USDA subgroup coverage (registered vs canonical KST 13th edition).
#' @keywords internal
.coverage_usda_subgroup <- function() {
  codes <- kst13_codes()
  sg    <- codes[nchar(codes$code) == 4L, , drop = FALSE]
  ord_name <- stats::setNames(codes$name[nchar(codes$code) == 1L],
                              codes$code[nchar(codes$code) == 1L])
  sg$order <- ord_name[substr(sg$code, 1, 1)]
  sg$key   <- tolower(trimws(sg$name))

  reg <- .coverage_registered_usda_subgroups()
  sg$covered <- sg$key %in% reg

  by_order <- do.call(rbind, lapply(split(sg, sg$order), function(d) {
    data.frame(group = d$order[1], canonical_n = nrow(d),
               covered_n = sum(d$covered), missing_n = sum(!d$covered),
               pct = round(100 * mean(d$covered), 1), stringsAsFactors = FALSE)
  }))
  by_order <- by_order[order(-by_order$pct, by_order$group), ]
  rownames(by_order) <- NULL

  overall <- data.frame(
    system = "usda", level = "subgroup",
    canonical_n = nrow(sg), registered_n = length(reg),
    covered_n = sum(sg$covered), missing_n = sum(!sg$covered),
    pct = round(100 * mean(sg$covered), 1), stringsAsFactors = FALSE)

  list(overall = overall, by_group = by_order,
       missing = sort(sg$name[!sg$covered]),
       extra   = sort(setdiff(reg, sg$key)))
}

#' Is a \code{qual_*} function a genuine implementation (not an unconditional
#' \code{passed = NA} stub)? A real qualifier either delegates to
#' \code{.q_presence()} or assigns \code{passed} from a computation.
#' @keywords internal
.qualifier_is_implemented <- function(name) {
  fn_name <- paste0("qual_", tolower(name))
  if (!exists(fn_name, where = asNamespace("soilKey"))) return(NA)  # no function
  b <- paste(deparse(body(get(fn_name, asNamespace("soilKey")))), collapse = " ")
  isTRUE(grepl("\\.q_presence", b) ||
         grepl("passed\\s*(<-|=)\\s*(?!NA[,)[:space:]])", b, perl = TRUE))
}

#' WRB 2022 qualifier coverage (canonical vs genuinely-implemented \code{qual_*}).
#'
#' "covered" means a \code{qual_*} function exists AND has a real
#' implementation (not an unconditional \code{passed = NA} stub). Stubs are
#' reported separately so the headline is honest rather than counting inert
#' functions. (Specifier-prefixed forms such as \emph{Endogleyic} are derived
#' by the specifier engine from their base qualifier and are not canonical
#' qualifier names, so they never enter this count.)
#' @keywords internal
.coverage_wrb_qualifiers <- function() {
  wc <- wrb2022_canonical()
  pq <- as.character(wc$pq[[ncol(wc$pq)]])
  sq <- as.character(wc$sq[[ncol(wc$sq)]])
  qdf <- rbind(
    data.frame(group = "principal",     name = unique(pq), stringsAsFactors = FALSE),
    data.frame(group = "supplementary", name = unique(sq), stringsAsFactors = FALSE))
  qdf <- qdf[!duplicated(qdf$name), , drop = FALSE]
  # Classify each canonical qualifier: implemented (real standalone qual_),
  # specifier_derived (an Epi-/Endo-/Bathy-/... form whose base qualifier is
  # implemented -- delivered by the specifier engine, not a standalone fn),
  # genuine_stub (exists but inert), or no_function. The first two are
  # deliverable, so "covered".
  spec_pref <- "^(Epi|Endo|Bathy|Ano|Ortho|Kato|Amphi|Panto|Supra|Thapto|Poly|Proto|Hyper|Hypo)"
  qdf$cat <- vapply(qdf$name, function(nm) {
    r <- .qualifier_is_implemented(nm)
    if (isTRUE(r)) return("implemented")
    if (is.na(r))  return("no_function")
    base <- sub(spec_pref, "", nm, ignore.case = TRUE)
    if (nzchar(base) && base != nm && isTRUE(.qualifier_is_implemented(base)))
      return("specifier_derived")
    "genuine_stub"
  }, character(1))
  qdf$covered <- qdf$cat %in% c("implemented", "specifier_derived")
  qdf$stub    <- qdf$cat == "genuine_stub"

  by_group <- do.call(rbind, lapply(split(qdf, qdf$group), function(d) {
    data.frame(group = d$group[1], canonical_n = nrow(d),
               covered_n = sum(d$covered), missing_n = sum(!d$covered),
               pct = round(100 * mean(d$covered), 1), stringsAsFactors = FALSE)
  }))
  rownames(by_group) <- NULL

  overall <- data.frame(
    system = "wrb2022", level = "qualifier",
    canonical_n = nrow(qdf), registered_n = sum(qdf$covered),
    covered_n = sum(qdf$covered), missing_n = sum(!qdf$covered),
    specifier_derived_n = sum(qdf$cat == "specifier_derived"),
    pct = round(100 * mean(qdf$covered), 1), stringsAsFactors = FALSE)

  list(overall = overall, by_group = by_group,
       missing = sort(qdf$name[!qdf$covered]),                  # the genuine gaps
       stubs   = sort(qdf$name[qdf$cat == "genuine_stub"]),     # exist but inert (fibric/hemic/sapric)
       extra   = character(0))
}

#' Names registered under a USDA level YAML directory (great-groups / suborders).
#' @keywords internal
.coverage_registered_usda_level <- function(subdir, yaml_key) {
  dir <- system.file("rules", "usda", subdir, package = "soilKey")
  if (!nzchar(dir) || !dir.exists(dir)) return(character(0))
  nms <- character(0)
  for (f in list.files(dir, pattern = "\\.yaml$", full.names = TRUE)) {
    y <- yaml::read_yaml(f)[[yaml_key]]
    for (grp in y) for (e in grp) if (!is.null(e$name)) nms <- c(nms, e$name)
  }
  unique(tolower(trimws(nms)))
}

#' USDA coverage at the great-group / suborder level (by NAME vs KST 13ed).
#' @keywords internal
.coverage_usda_named_level <- function(code_len, subdir, yaml_key, level) {
  codes <- kst13_codes()
  lev   <- codes[nchar(codes$code) == code_len, , drop = FALSE]
  ord_name <- stats::setNames(codes$name[nchar(codes$code) == 1L],
                              codes$code[nchar(codes$code) == 1L])
  lev$order <- ord_name[substr(lev$code, 1, 1)]
  lev$key   <- tolower(trimws(lev$name))
  reg <- .coverage_registered_usda_level(subdir, yaml_key)
  lev$covered <- lev$key %in% reg

  by_order <- do.call(rbind, lapply(split(lev, lev$order), function(d)
    data.frame(group = d$order[1], canonical_n = nrow(d), covered_n = sum(d$covered),
               missing_n = sum(!d$covered), pct = round(100 * mean(d$covered), 1),
               stringsAsFactors = FALSE)))
  by_order <- by_order[order(-by_order$pct, by_order$group), ]; rownames(by_order) <- NULL
  overall <- data.frame(system = "usda", level = level, canonical_n = nrow(lev),
                        registered_n = length(reg), covered_n = sum(lev$covered),
                        missing_n = sum(!lev$covered), pct = round(100 * mean(lev$covered), 1),
                        stringsAsFactors = FALSE)
  list(overall = overall, by_group = by_order,
       missing = sort(lev$name[!lev$covered]), extra = sort(setdiff(reg, lev$key)))
}

#' SiBCS 5 registered class counts. There is no external canonical code-set to
#' diff against (unlike KST 13ed for USDA), so this honestly reports the
#' registered class counts per level with that caveat -- not a percentage.
#' @keywords internal
.coverage_sibcs <- function() {
  levels <- c(order = "ordens", suborder = "subordens",
              great_group = "grandes_grupos", subgroup = "subgrupos")
  rows <- lapply(names(levels), function(lv)
    data.frame(group = lv,
               registered_n = length(.coverage_registered_sibcs_level(levels[[lv]])),
               stringsAsFactors = FALSE))
  by_group <- do.call(rbind, rows); rownames(by_group) <- NULL
  overall <- data.frame(system = "sibcs", level = "all",
                        canonical_n = NA_integer_, registered_n = sum(by_group$registered_n),
                        covered_n = NA_integer_, missing_n = NA_integer_, pct = NA_real_,
                        stringsAsFactors = FALSE)
  list(overall = overall, by_group = by_group, missing = character(0), extra = character(0),
       note = "No external canonical SiBCS 5 class list; registered counts only.")
}

#' Distinct leaf names registered at a SiBCS level (from the merged rule base).
#' @keywords internal
.coverage_registered_sibcs_level <- function(yaml_key) {
  block <- load_rules("sibcs5")[[yaml_key]]
  nms <- character(0)
  collect <- function(x) {
    if (!is.list(x)) return(invisible())
    if (!is.null(x$name) && is.character(x$name)) nms[[length(nms) + 1L]] <<- x$name
    else for (el in x) collect(el)
  }
  collect(block)
  unique(tolower(trimws(unlist(nms))))
}

#' Honest taxonomic-completeness report
#'
#' Measures, by NAME, exactly which canonical taxa/qualifiers the package's
#' deterministic rule base registers, replacing hand-maintained coverage
#' claims with an auditable, reproducible diff. For \code{"usda_subgroup"} the
#' canonical reference is the Soil Taxonomy 13th-edition subgroup set from
#' \code{\link{kst13_codes}}; for \code{"wrb_qualifiers"} it is the WRB 2022
#' principal + supplementary qualifier set from \code{\link{wrb2022_canonical}}.
#'
#' @param system Which axis to measure. USDA taxon levels against the Soil
#'   Taxonomy 13th-edition code set (\code{\link{kst13_codes}}):
#'   \code{"usda_subgroup"} (default), \code{"usda_great_group"},
#'   \code{"usda_suborder"}. WRB 2022 qualifiers against
#'   \code{\link{wrb2022_canonical}}: \code{"wrb_qualifiers"} -- here "covered"
#'   means the \code{qual_*} function exists \emph{and} is a genuine
#'   implementation (not an unconditional \code{passed = NA} stub), and the
#'   inert ones are returned in \code{$stubs}. \code{"sibcs"} has no external
#'   canonical class list, so it honestly reports registered class counts per
#'   level only (no percentage).
#' @param write If \code{TRUE}, also write a Markdown summary to
#'   \code{report_dir}. Default \code{FALSE}.
#' @param report_dir Directory for the Markdown report when \code{write = TRUE}.
#'   Defaults to \code{inst/benchmarks/reports} inside the installed package.
#'
#' @return Invisibly, a list with \code{$overall} (one-row data frame:
#'   \code{system}, \code{level}, \code{canonical_n}, \code{registered_n},
#'   \code{covered_n}, \code{missing_n}, \code{pct}), \code{$by_group} (per
#'   order, or per principal/supplementary), \code{$missing} (canonical names
#'   not registered), \code{$extra} (registered names absent from the canonical
#'   set), and -- for \code{"wrb_qualifiers"} -- \code{$stubs} (functions that
#'   exist but are inert). A compact summary is printed as a side effect.
#'
#' @examples
#' cov <- coverage_report("usda_subgroup")
#' cov$overall
#' head(cov$missing)
#'
#' @export
coverage_report <- function(system = c("usda_subgroup", "usda_great_group",
                                       "usda_suborder", "wrb_qualifiers", "sibcs"),
                            write = FALSE, report_dir = NULL) {
  system <- match.arg(system)
  res <- switch(system,
    usda_subgroup    = .coverage_usda_subgroup(),
    usda_great_group = .coverage_usda_named_level(3L, "great-groups", "great_groups", "great_group"),
    usda_suborder    = .coverage_usda_named_level(2L, "suborders", "suborders", "suborder"),
    wrb_qualifiers   = .coverage_wrb_qualifiers(),
    sibcs            = .coverage_sibcs())

  o <- res$overall
  cli::cli_h2(sprintf("Coverage: %s %s", o$system, o$level))
  if (is.na(o$pct)) {
    cli::cli_alert_info(sprintf("%d classes registered. %s",
                                o$registered_n, res$note %||% ""))
  } else {
    cli::cli_alert_info(sprintf(
      "%d / %d canonical %ss registered (%.1f%%); %d missing.",
      o$covered_n, o$canonical_n, o$level, o$pct, o$missing_n))
    if (length(res$stubs))
      cli::cli_alert_warning(sprintf("%d exist but are inert (stubs): %s",
                                     length(res$stubs), paste(res$stubs, collapse = ", ")))
  }
  print(res$by_group, row.names = FALSE)

  if (isTRUE(write)) {
    report_dir <- report_dir %||%
      system.file("benchmarks", "reports", package = "soilKey")
    if (nzchar(report_dir) && dir.exists(report_dir)) {
      path <- file.path(report_dir, sprintf("coverage_%s.md", system))
      writeLines(.coverage_markdown(system, res), path)
      cli::cli_alert_success(sprintf("Wrote %s", path))
    } else {
      cli::cli_alert_warning("report_dir not found; skipped writing.")
    }
  }
  invisible(res)
}

#' Render a coverage result as Markdown.
#' @keywords internal
.coverage_markdown <- function(system, res) {
  o <- res$overall
  if (is.na(o$pct)) {           # registered-counts-only (e.g. SiBCS, no canonical)
    lines <- c(sprintf("# Coverage report -- %s %s", o$system, o$level), "",
               sprintf("%d classes registered. %s", o$registered_n, res$note %||% ""),
               "", "## By group", "", "| group | registered |", "|---|---:|")
    for (i in seq_len(nrow(res$by_group)))
      lines <- c(lines, sprintf("| %s | %d |", res$by_group$group[i],
                                res$by_group$registered_n[i]))
    return(lines)
  }
  lines <- c(
    sprintf("# Coverage report -- %s %s", o$system, o$level),
    "",
    sprintf("Measured by NAME against the canonical reference set. %d of %d canonical %ss are registered (**%.1f%%**); %d missing.",
            o$covered_n, o$canonical_n, o$level, o$pct, o$missing_n),
    "",
    "## By group", "",
    "| group | canonical | covered | missing | pct |",
    "|---|---:|---:|---:|---:|")
  for (i in seq_len(nrow(res$by_group))) {
    g <- res$by_group[i, ]
    lines <- c(lines, sprintf("| %s | %d | %d | %d | %.1f%% |",
                              g$group, g$canonical_n, g$covered_n, g$missing_n, g$pct))
  }
  if (length(res$stubs %||% character(0))) {
    lines <- c(lines, "",
               sprintf("## Exist but inert (stubs) (%d)", length(res$stubs)),
               "", paste0("- ", res$stubs))
  }
  lines <- c(lines, "", sprintf("## Missing (%d)", length(res$missing)), "",
             if (length(res$missing)) paste0("- ", res$missing) else "_none_")
  if (length(res$extra)) {
    lines <- c(lines, "",
               sprintf("## Registered but non-canonical (%d)", length(res$extra)),
               "", paste0("- ", res$extra))
  }
  lines
}
