# =============================================================================
# soilKey -- Report rendering (HTML)
#
# Promised in ARCHITECTURE.md sec. 10:
#
#   report(list(res_wrb, res_sibcs), file = "perfil_042_report.html")
#
# This file implements the HTML path. The PDF path lives in
# `report-pdf.R` and shares the same data-assembly helpers
# (`.assemble_report_data()`, `.html_escape()`, ...).
#
# Design principles:
#
#   1. The HTML output is fully self-contained (one file, inline CSS,
#      no external network requests). A pedologist can email the file
#      to a colleague or attach it to a laudo without any other
#      artefacts.
#   2. Zero hard dependencies beyond base R. (We deliberately do NOT
#      depend on rmarkdown / htmltools for HTML so that the function
#      works in a minimal install.)
#   3. The renderer accepts a single `ClassificationResult`, a list of
#      `ClassificationResult`s (one per system), or a `PedonRecord` --
#      same single entry-point.
#   4. The output mirrors what `print(ClassificationResult)` shows in
#      the console: name, evidence grade, qualifiers, key trace,
#      ambiguities, missing data, warnings -- plus, when a PedonRecord
#      is provided, the horizons table and the per-attribute provenance.
# =============================================================================


# ---- public S3 generic ------------------------------------------------------


#' Render a soilKey classification report
#'
#' Produces a pedologist-facing report from one or more
#' \code{\link{ClassificationResult}} objects, optionally including the
#' source \code{\link{PedonRecord}}. The HTML output is fully
#' self-contained (single file, inline CSS); the PDF output goes through
#' \code{rmarkdown::render()} and therefore requires a working LaTeX
#' install (or one of the alternative engines accepted by
#' \code{rmarkdown}).
#'
#' This is an S3 generic with methods for \code{ClassificationResult},
#' \code{list}, and \code{PedonRecord}. Most users call \code{report()}
#' directly with a list of three results
#' (\code{list(classify_wrb2022(p), classify_sibcs(p), classify_usda(p))})
#' to get a cross-system one-pager.
#'
#' @param x    A \code{ClassificationResult}, a list of
#'             \code{ClassificationResult}s, or a \code{PedonRecord}
#'             (in which case all three keys are run automatically).
#' @param file Output path. The format is inferred from the extension
#'             (\code{.html} or \code{.pdf}) unless \code{format} is
#'             given explicitly.
#' @param format One of \code{"auto"}, \code{"html"}, \code{"pdf"}.
#' @param pedon Optional \code{PedonRecord}; when provided, its
#'             horizons table and provenance log are included.
#' @param title Optional report title.
#' @param ...  Passed to method-specific renderers.
#' @return     The output path, invisibly.
#' @export
report <- function(x,
                   file,
                   format = c("auto", "html", "pdf"),
                   pedon  = NULL,
                   title  = NULL,
                   ...) {
  format <- match.arg(format)
  if (missing(file) || is.null(file) || !nzchar(file))
    stop("`file` is required and must be a non-empty path.")
  if (format == "auto") {
    ext <- tolower(tools::file_ext(file))
    format <- switch(ext,
                     "html" = "html",
                     "htm"  = "html",
                     "pdf"  = "pdf",
                     stop(sprintf(
                       "Cannot infer format from extension '%s'; pass format = \"html\" or \"pdf\".",
                       ext)))
  }
  switch(format,
         "html" = report_html(x, file = file, pedon = pedon,
                                title = title, ...),
         "pdf"  = report_pdf( x, file = file, pedon = pedon,
                                title = title, ...))
}


# ---- helpers ----------------------------------------------------------------


#' Look up the package version, falling back gracefully when soilKey
#' is not installed (e.g. during interactive development with
#' `sys.source()`).
#'
#' @keywords internal
.soilkey_version <- function() {
  v <- tryCatch(utils::packageVersion("soilKey"),
                  error = function(e) NULL)
  if (is.null(v)) "dev" else as.character(v)
}
#' Internal helper: .html_escape


#' @keywords internal
.html_escape <- function(x) {
  if (is.null(x)) return("")
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- gsub("&",  "&amp;",  x, fixed = TRUE)
  x <- gsub("<",  "&lt;",   x, fixed = TRUE)
  x <- gsub(">",  "&gt;",   x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  x
}
#' Internal helper: .normalise_results

#' @keywords internal
.normalise_results <- function(x, pedon = NULL) {
  if (inherits(x, "PedonRecord")) {
    if (is.null(pedon)) pedon <- x
    out <- list()
    out$wrb   <- tryCatch(classify_wrb2022(pedon, on_missing = "silent"),
                            error = function(e) NULL)
    out$sibcs <- tryCatch(classify_sibcs(pedon, include_familia = TRUE),
                            error = function(e) NULL)
    out$usda  <- tryCatch(classify_usda(pedon),
                            error = function(e) NULL)
    res <- Filter(Negate(is.null), out)
  } else if (inherits(x, "ClassificationResult")) {
    res <- list(x)
  } else if (is.list(x)) {
    bad <- !vapply(x, inherits, logical(1), "ClassificationResult")
    if (any(bad))
      stop("All list elements must be ClassificationResult objects.")
    res <- x
  } else {
    stop("`x` must be a PedonRecord, a ClassificationResult, or a list of them.")
  }
  list(results = res, pedon = pedon)
}

#' Grade -> CSS class
#' @keywords internal
.grade_class <- function(g) {
  if (is.null(g) || is.na(g)) return("grade grade-na")
  switch(as.character(g),
         "A" = "grade grade-a",
         "B" = "grade grade-b",
         "C" = "grade grade-c",
         "D" = "grade grade-d",
         "grade grade-na")
}

#' Render the head section with embedded CSS.
#' @keywords internal
.html_head <- function(title) {
  paste0(
    '<!DOCTYPE html>\n',
    '<html lang="en"><head>\n',
    '<meta charset="utf-8">\n',
    '<meta name="viewport" content="width=device-width, initial-scale=1">\n',
    '<title>', .html_escape(title), '</title>\n',
    '<style>\n',
    'body{font-family:-apple-system,BlinkMacSystemFont,"Segoe UI",Roboto,Helvetica,Arial,sans-serif;',
    'max-width:880px;margin:2rem auto;padding:0 1.5rem;color:#222;line-height:1.55;}\n',
    'h1{border-bottom:3px solid #FF6B35;padding-bottom:.4rem;font-size:1.8rem;}\n',
    'h2{margin-top:2rem;color:#3a3a3a;font-size:1.35rem;border-left:4px solid #FF6B35;padding-left:.6rem;}\n',
    'h3{margin-top:1.4rem;color:#555;font-size:1.05rem;}\n',
    '.system-card{background:#fafafa;border:1px solid #e3e3e3;border-radius:8px;',
    'padding:1rem 1.2rem;margin-bottom:1rem;}\n',
    '.system-card .name{font-size:1.15rem;font-weight:600;color:#1c1c1c;}\n',
    '.grade{display:inline-block;padding:.15rem .55rem;border-radius:4px;font-weight:600;',
    'font-size:.85rem;letter-spacing:.05em;}\n',
    '.grade-a{background:#1f9e3f;color:white;}\n',
    '.grade-b{background:#82c341;color:white;}\n',
    '.grade-c{background:#f0ad4e;color:white;}\n',
    '.grade-d{background:#d9534f;color:white;}\n',
    '.grade-na{background:#999;color:white;}\n',
    'table{border-collapse:collapse;width:100%;margin:.6rem 0;font-size:.92rem;}\n',
    'th,td{border:1px solid #ddd;padding:.4rem .55rem;text-align:left;}\n',
    'th{background:#f4f4f4;font-weight:600;}\n',
    '.muted{color:#888;font-size:.88rem;}\n',
    '.passed{color:#1f9e3f;font-weight:600;}\n',
    '.failed{color:#a94442;}\n',
    '.indeterminate{color:#8a6d3b;}\n',
    '.trace{font-family:"JetBrains Mono",ui-monospace,Menlo,Consolas,monospace;font-size:.85rem;}\n',
    '.qualifiers{margin:.4rem 0;}\n',
    '.qual-pill{display:inline-block;background:#eef3ff;color:#1a4d99;border:1px solid #c8d6f0;',
    'padding:.1rem .45rem;border-radius:3px;margin:0 .25rem .25rem 0;font-size:.85rem;}\n',
    '.qual-pill.suppl{background:#fff3e0;color:#7a4a00;border-color:#f0d6a8;}\n',
    'footer{margin-top:3rem;padding-top:1rem;border-top:1px solid #eee;color:#999;font-size:.85rem;}\n',
    '@media print{body{max-width:none;margin:0;}h2{page-break-after:avoid;}',
    '.system-card{page-break-inside:avoid;}}\n',
    '</style>\n',
    '</head>\n<body>\n'
  )
}

#' Render the per-result card (one per classification system).
#' @keywords internal
.html_classification_card <- function(res) {
  qual_principal <- res$qualifiers$principal     %||% character()
  qual_suppl     <- res$qualifiers$supplementary %||% character()

  qual_html <- c(
    if (length(qual_principal) > 0)
      vapply(qual_principal, function(q)
        sprintf('<span class="qual-pill">%s</span>', .html_escape(q)),
        character(1)),
    if (length(qual_suppl) > 0)
      vapply(qual_suppl, function(q)
        sprintf('<span class="qual-pill suppl">%s</span>', .html_escape(q)),
        character(1))
  )

  trace_rows <- if (length(res$trace) == 0) {
    "<tr><td colspan=\"4\" class=\"muted\">(no trace)</td></tr>"
  } else {
    paste0(vapply(seq_along(res$trace), function(i) {
      t <- res$trace[[i]]
      # v0.9.11: tolerate atomic / non-list trace entries (some
      # diagnostics return a bare logical when no metadata is
      # available). Promote to a minimal list so the field accesses
      # below are uniform.
      if (!is.list(t)) {
        t <- list(passed = if (is.logical(t)) t else NA,
                  code = NA_character_, name = NA_character_,
                  missing = character(0))
      }
      passed <- t$passed
      sym <- if (isTRUE(passed))      'class="passed">PASSED'
             else if (isFALSE(passed)) 'class="failed">failed'
             else                      'class="indeterminate">indeterminate'
      sprintf(
        '<tr><td>%d</td><td><code>%s</code></td><td>%s</td><td><span %s</span>%s</td></tr>',
        i,
        .html_escape(t$code %||% "?"),
        .html_escape(t$name %||% ""),
        sym,
        if (!isTRUE(passed) && length(t$missing %||% character()) > 0)
          sprintf(' <span class="muted">(%d attrs missing)</span>',
                    length(t$missing))
        else "")
    }, character(1)), collapse = "\n")
  }

  ambig_html <- if (length(res$ambiguities) == 0) {
    ""
  } else {
    paste0(
      "<h3>Ambiguities</h3><ul>",
      paste(vapply(res$ambiguities, function(a)
        sprintf('<li><b>%s</b>: %s</li>',
                  .html_escape(a$rsg_code %||% "?"),
                  .html_escape(a$reason   %||% "")),
        character(1)), collapse = "\n"),
      "</ul>"
    )
  }

  missing_html <- if (length(res$missing_data %||% character()) == 0) {
    ""
  } else {
    sprintf("<h3>Missing data that would refine the result</h3><p>%s</p>",
              paste(.html_escape(res$missing_data), collapse = ", "))
  }

  warn_html <- if (length(res$warnings %||% character()) == 0) {
    ""
  } else {
    paste0(
      "<h3>Warnings</h3><ul>",
      paste(vapply(res$warnings, function(w)
        sprintf("<li>%s</li>", .html_escape(w)),
        character(1)), collapse = "\n"),
      "</ul>"
    )
  }

  prior_html <- if (is.null(res$prior_check)) {
    ""
  } else {
    sprintf("<p class=\"muted\">Spatial-prior check: <b>%s</b></p>",
              .html_escape(res$prior_check$status %||% "not run"))
  }

  paste0(
    '<div class="system-card">\n',
    sprintf('<div class="name">%s</div>\n', .html_escape(res$name %||% "(unnamed)")),
    sprintf('<div class="muted">System: %s &middot; RSG/Order: <code>%s</code> &middot; Evidence grade: <span class="%s">%s</span></div>\n',
              .html_escape(res$system %||% "?"),
              .html_escape(res$rsg_or_order %||% "?"),
              .grade_class(res$evidence_grade),
              .html_escape(res$evidence_grade %||% "NA")),
    if (length(qual_html) > 0)
      sprintf('<div class="qualifiers">%s</div>\n', paste(qual_html, collapse = "")),
    prior_html,
    '<h3>Key trace</h3>\n',
    sprintf('<table class="trace"><thead><tr><th>#</th><th>Code</th><th>Name</th><th>Result</th></tr></thead><tbody>%s</tbody></table>\n',
              trace_rows),
    ambig_html,
    missing_html,
    warn_html,
    '</div>\n'
  )
}

#' Render the horizons table from a PedonRecord.
#' @keywords internal
#' @param pedon A \code{\link{PedonRecord}}.
.html_horizons_table <- function(pedon) {
  if (is.null(pedon) || is.null(pedon$horizons) || nrow(pedon$horizons) == 0) {
    return("")
  }
  cols <- intersect(
    c("top_cm", "bottom_cm", "designation",
      "munsell_hue_moist", "munsell_value_moist", "munsell_chroma_moist",
      "clay_pct", "silt_pct", "sand_pct",
      "ph_h2o", "oc_pct", "cec_cmol", "bs_pct"),
    names(pedon$horizons)
  )
  if (length(cols) == 0) return("")

  h <- as.data.frame(pedon$horizons)[, cols, drop = FALSE]
  header <- paste0("<th>", .html_escape(cols), "</th>", collapse = "")
  rows <- vapply(seq_len(nrow(h)), function(i) {
    cells <- vapply(h[i, , drop = TRUE], function(v) {
      if (is.numeric(v) && !is.na(v))
        sprintf("%.4g", v)
      else if (is.na(v))
        "<span class=\"muted\">--</span>"
      else
        .html_escape(v)
    }, character(1))
    paste0("<tr>",
             paste0("<td>", cells, "</td>", collapse = ""),
             "</tr>")
  }, character(1))

  paste0(
    "<h2>Horizons</h2>\n",
    sprintf("<table><thead><tr>%s</tr></thead><tbody>%s</tbody></table>\n",
              header,
              paste(rows, collapse = "\n"))
  )
}

#' Render a provenance summary from a PedonRecord.
#' @keywords internal
#' @param pedon A \code{\link{PedonRecord}}.
.html_provenance_table <- function(pedon) {
  if (is.null(pedon) || is.null(pedon$provenance) ||
        nrow(pedon$provenance) == 0) {
    return("")
  }
  by_source <- as.data.frame(table(source = pedon$provenance$source))
  by_source <- by_source[order(-by_source$Freq), , drop = FALSE]
  rows <- vapply(seq_len(nrow(by_source)), function(i) {
    sprintf("<tr><td><code>%s</code></td><td>%d</td></tr>",
              .html_escape(by_source$source[i]),
              by_source$Freq[i])
  }, character(1))
  paste0(
    "<h2>Provenance summary</h2>\n",
    sprintf("<table><thead><tr><th>Source</th><th>n</th></tr></thead><tbody>%s</tbody></table>\n",
              paste(rows, collapse = "\n"))
  )
}

#' Render the cross-system summary table when multiple results are provided.
#' @keywords internal
.html_summary_table <- function(results) {
  if (length(results) < 2) return("")
  rows <- vapply(results, function(r) {
    sprintf(
      paste0("<tr><td>%s</td><td>%s</td>",
             "<td><span class=\"%s\">%s</span></td></tr>"),
      .html_escape(r$system %||% "?"),
      .html_escape(r$name   %||% "(unnamed)"),
      .grade_class(r$evidence_grade),
      .html_escape(r$evidence_grade %||% "NA")
    )
  }, character(1))
  paste0(
    "<h2>Cross-system summary</h2>\n",
    paste0("<table><thead><tr><th>System</th><th>Name</th>",
             "<th>Grade</th></tr></thead>",
             sprintf("<tbody>%s</tbody></table>\n",
                       paste(rows, collapse = "\n")))
  )
}

#' Render site metadata header.
#' @keywords internal
#' @param pedon A \code{\link{PedonRecord}}.
.html_site_header <- function(pedon) {
  if (is.null(pedon) || is.null(pedon$site)) return("")
  s <- pedon$site
  bits <- list()
  if (!is.null(s$id))               bits[["ID"]]    <- s$id
  if (!is.null(s$lat) && !is.null(s$lon))
    bits[["Coords"]] <- sprintf("%.4f, %.4f", s$lat, s$lon)
  if (!is.null(s$country))          bits[["Country"]] <- s$country
  if (!is.null(s$parent_material))  bits[["Parent material"]] <- s$parent_material
  if (!is.null(s$elevation_m))      bits[["Elevation (m)"]] <- s$elevation_m
  if (!is.null(s$slope_pct))        bits[["Slope (%)"]]    <- s$slope_pct
  if (!is.null(s$date))             bits[["Date"]]         <- s$date
  if (length(bits) == 0) return("")
  rows <- vapply(seq_along(bits), function(i)
    sprintf("<tr><th>%s</th><td>%s</td></tr>",
              .html_escape(names(bits)[i]),
              .html_escape(bits[[i]])),
    character(1))
  paste0("<h2>Site</h2>\n",
           "<table><tbody>",
           paste(rows, collapse = "\n"),
           "</tbody></table>\n")
}


# ---- public renderer --------------------------------------------------------


#' Render a soilKey classification report as self-contained HTML
#'
#' See \code{\link{report}} for the generic. This function writes a
#' single-file HTML report with inline CSS (no external network
#' requests, no `htmltools` dependency) so it can be emailed or
#' archived as-is.
#'
#' @param x      A \code{ClassificationResult}, list of results, or
#'               \code{PedonRecord}.
#' @param file   Output \code{.html} path.
#' @param pedon  Optional \code{PedonRecord}.
#' @param title  Report title.
#' @param ...    Currently unused.
#' @return       The output path, invisibly.
#' @export
report_html <- function(x,
                        file,
                        pedon = NULL,
                        title = NULL,
                        ...) {
  norm <- .normalise_results(x, pedon = pedon)
  results <- norm$results
  pedon   <- norm$pedon

  if (is.null(title)) {
    pedon_id <- if (!is.null(pedon) && !is.null(pedon$site$id))
                  pedon$site$id else "soilKey report"
    title <- paste0("soilKey -- ", pedon_id)
  }

  body <- paste0(
    .html_head(title),
    sprintf("<h1>%s</h1>\n", .html_escape(title)),
    sprintf("<p class=\"muted\">Generated %s by soilKey v%s</p>\n",
              format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
              .soilkey_version()),
    .html_site_header(pedon),
    .html_summary_table(results),
    "<h2>Classification results</h2>\n",
    paste(vapply(results, .html_classification_card, character(1)),
            collapse = "\n"),
    .html_horizons_table(pedon),
    .html_provenance_table(pedon),
    "<footer>\n",
    "Report rendered by <a href=\"https://github.com/HugoMachadoRodrigues/soilKey\">soilKey</a>. ",
    "The taxonomic key was executed deterministically from versioned YAML rules; ",
    "no language model was used in the classification step.\n",
    "</footer>\n",
    "</body></html>\n"
  )

  dir.create(dirname(normalizePath(file, mustWork = FALSE)),
             recursive = TRUE, showWarnings = FALSE)
  writeLines(body, file, useBytes = TRUE)
  invisible(file)
}
