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

#' WRB 2022 qualifier coverage (registered \code{qual_*} vs canonical).
#' @keywords internal
.coverage_wrb_qualifiers <- function() {
  wc <- wrb2022_canonical()
  pq <- as.character(wc$pq[[ncol(wc$pq)]])
  sq <- as.character(wc$sq[[ncol(wc$sq)]])
  qdf <- rbind(
    data.frame(group = "principal",     name = unique(pq), stringsAsFactors = FALSE),
    data.frame(group = "supplementary", name = unique(sq), stringsAsFactors = FALSE))
  qdf <- qdf[!duplicated(qdf$name), , drop = FALSE]
  qdf$covered <- vapply(qdf$name,
    function(nm) exists(paste0("qual_", tolower(nm)), where = asNamespace("soilKey")),
    logical(1))

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
    pct = round(100 * mean(qdf$covered), 1), stringsAsFactors = FALSE)

  list(overall = overall, by_group = by_group,
       missing = sort(qdf$name[!qdf$covered]), extra = character(0))
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
#' @param system Which axis to measure: \code{"usda_subgroup"} (default) or
#'   \code{"wrb_qualifiers"}.
#' @param write If \code{TRUE}, also write a Markdown summary to
#'   \code{report_dir}. Default \code{FALSE}.
#' @param report_dir Directory for the Markdown report when \code{write = TRUE}.
#'   Defaults to \code{inst/benchmarks/reports} inside the installed package.
#'
#' @return Invisibly, a list with \code{$overall} (one-row data frame:
#'   \code{system}, \code{level}, \code{canonical_n}, \code{registered_n},
#'   \code{covered_n}, \code{missing_n}, \code{pct}), \code{$by_group} (per
#'   order, or per principal/supplementary), \code{$missing} (canonical names
#'   not registered) and \code{$extra} (registered names absent from the
#'   canonical set). A compact summary is printed as a side effect.
#'
#' @examples
#' cov <- coverage_report("usda_subgroup")
#' cov$overall
#' head(cov$missing)
#'
#' @export
coverage_report <- function(system = c("usda_subgroup", "wrb_qualifiers"),
                            write = FALSE, report_dir = NULL) {
  system <- match.arg(system)
  res <- switch(system,
    usda_subgroup  = .coverage_usda_subgroup(),
    wrb_qualifiers = .coverage_wrb_qualifiers())

  o <- res$overall
  cli::cli_h2(sprintf("Coverage: %s %s", o$system, o$level))
  cli::cli_alert_info(sprintf(
    "%d / %d canonical %ss registered (%.1f%%); %d missing.",
    o$covered_n, o$canonical_n, o$level, o$pct, o$missing_n))
  print(res$by_group, row.names = FALSE)

  if (isTRUE(write)) {
    report_dir <- report_dir %||%
      system.file("benchmarks", "reports", package = "soilKey")
    if (nzchar(report_dir) && dir.exists(report_dir)) {
      path <- file.path(report_dir, sprintf("coverage_%s_v09113.md", system))
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
  lines <- c(lines, "", sprintf("## Missing (%d)", length(res$missing)), "",
             if (length(res$missing)) paste0("- ", res$missing) else "_none_")
  if (length(res$extra)) {
    lines <- c(lines, "",
               sprintf("## Registered but non-canonical (%d)", length(res$extra)),
               "", paste0("- ", res$extra))
  }
  lines
}
