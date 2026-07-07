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
#' @param include_family When \code{x} is a \code{PedonRecord} (so the
#'             three keys are run here), passes through to
#'             \code{\link{classify_usda}} to append the USDA family
#'             (5th category) to the subgroup. Default \code{FALSE} keeps
#'             the output byte-identical to earlier versions.
#' @param specifiers When \code{x} is a \code{PedonRecord}, passes through
#'             to \code{\link{classify_wrb2022}} to attach WRB depth
#'             specifiers (Epi-/Endo-/...) to depth-anchored qualifiers.
#'             Default \code{FALSE}. Both flags are ignored when \code{x}
#'             is already a (list of) \code{ClassificationResult}.
#' @param lang Report language; \code{"en"} (default) or \code{"pt"}
#'             (Brazilian Portuguese).
#' @param ...  Passed to method-specific renderers.
#' @return     The output path, invisibly.
#' @examples
#' pedon <- make_ferralsol_canonical()
#' out <- file.path(tempdir(), "soilkey_report.html")
#' report(pedon, file = out, pedon = pedon)
#' file.exists(out)
#' @export
report <- function(x,
                   file,
                   format = c("auto", "html", "pdf"),
                   pedon  = NULL,
                   title  = NULL,
                   include_family = FALSE,
                   specifiers     = FALSE,
                   lang   = c("en", "pt"),
                   ...) {
  format <- match.arg(format)
  lang   <- match.arg(lang)
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
                                title = title,
                                include_family = include_family,
                                specifiers = specifiers, lang = lang, ...),
         "pdf"  = report_pdf( x, file = file, pedon = pedon,
                                title = title,
                                include_family = include_family,
                                specifiers = specifiers, lang = lang, ...))
}


# ---- helpers ----------------------------------------------------------------


#' Look up the package version, falling back gracefully when soilKey
#' is not installed (e.g. during interactive development with
#' `sys.source()`).
#'
#' @noRd
.soilkey_version <- function() {
  v <- tryCatch(utils::packageVersion("soilKey"),
                  error = function(e) NULL)
  if (is.null(v)) "dev" else as.character(v)
}
#' Internal helper: .html_escape


#' @noRd
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

#' @noRd
.normalise_results <- function(x, pedon = NULL,
                                 include_family = FALSE, specifiers = FALSE) {
  if (inherits(x, "PedonRecord")) {
    if (is.null(pedon)) pedon <- x
    out <- list()
    out$wrb   <- tryCatch(classify_wrb2022(pedon, on_missing = "silent",
                                             specifiers = specifiers),
                            error = function(e) NULL)
    out$sibcs <- tryCatch(classify_sibcs(pedon, include_familia = TRUE),
                            error = function(e) NULL)
    out$usda  <- tryCatch(classify_usda(pedon, include_family = include_family),
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
#' @noRd
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
#' @noRd
.html_head <- function(title) {
  paste0(
    '<!DOCTYPE html>\n',
    '<html lang="en"><head>\n',
    '<meta charset="utf-8">\n',
    '<meta name="viewport" content="width=device-width, initial-scale=1">\n',
    '<title>', .html_escape(title), '</title>\n',
    '<style>\n',
    ':root{color-scheme:light;}\n',
    'body{font-family:-apple-system,BlinkMacSystemFont,"Segoe UI",Roboto,Helvetica,Arial,sans-serif;',
    'max-width:880px;margin:2rem auto;padding:0 1.5rem;color:#222;background:#ffffff;line-height:1.55;}\n',
    'td{background:#ffffff;}\n',
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
    # v0.9.168: branded header, locator map, and per-profile page breaks.
    '.report-header{display:flex;align-items:center;gap:.9rem;border-bottom:3px solid #B5652E;',
    'padding-bottom:.55rem;margin-bottom:.2rem;}\n',
    '.report-header img{height:50px;width:auto;}\n',
    '.report-header .rh-title{font-size:1.7rem;font-weight:700;color:#3a2a1e;line-height:1.15;}\n',
    '.map-card{margin:1rem 0;border:1px solid #e3ddd0;border-radius:8px;overflow:hidden;}\n',
    '.map-card img{display:block;width:100%;height:auto;}\n',
    '.map-card .cap{padding:.4rem .7rem;background:#faf6ef;color:#6b5c4d;font-size:.85rem;}\n',
    '.profile-page{margin-top:1.6rem;padding-top:.4rem;}\n',
    '.profile-page h2:first-child{border-left:none;padding-left:0;color:#4A3226;',
    'border-bottom:2px solid #e3ddd0;}\n',
    '@media print{body{max-width:none;margin:0;}h2{page-break-after:avoid;}',
    '.system-card,.map-card{page-break-inside:avoid;}',
    '.profile-page{page-break-before:always;}}\n',
    '</style>\n',
    '</head>\n<body>\n'
  )
}

#' Render the per-result card (one per classification system).
#' @noRd
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

  # v0.9.165: normalise the system-dependent trace (flat for WRB, nested phases
  # for SiBCS/USDA) into one ordered table, then render the decision steps.
  # `info` rows (family attributes / bare labels) are not key steps, so they
  # are dropped here; assigned-taxon rows render like a passing step. WRB output
  # is byte-identical to the previous flat rendering.
  tt <- .flatten_key_trace(res$trace)
  tt <- tt[tt$status != "info", , drop = FALSE]
  trace_rows <- if (nrow(tt) == 0) {
    sprintf("<tr><td colspan=\"4\" class=\"muted\">%s</td></tr>",
            .report_msg("report.no_trace"))
  } else {
    paste0(vapply(seq_len(nrow(tt)), function(i) {
      st  <- tt$status[i]
      sym <- if (st %in% c("passed", "selected"))
               paste0('class="passed">', .report_msg("report.trace_passed"))
             else if (st == "failed")
               paste0('class="failed">', .report_msg("report.trace_failed"))
             else
               paste0('class="indeterminate">', .report_msg("report.trace_indeterminate"))
      sprintf(
        '<tr><td>%d</td><td><code>%s</code></td><td>%s</td><td><span %s</span>%s</td></tr>',
        i,
        .html_escape(if (nzchar(tt$code[i])) tt$code[i] else "?"),
        .html_escape(tt$name[i]),
        sym,
        if (st %in% c("failed", "indeterminate") && tt$n_missing[i] > 0)
          sprintf(.report_msg("report.attrs_missing"), tt$n_missing[i])
        else "")
    }, character(1)), collapse = "\n")
  }

  ambig_html <- if (length(res$ambiguities) == 0) {
    ""
  } else {
    paste0(
      sprintf("<h3>%s</h3><ul>", .report_msg("report.ambiguities")),
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
    sprintf(.report_msg("report.missing_data"),
              paste(.html_escape(res$missing_data), collapse = ", "))
  }

  warn_html <- if (length(res$warnings %||% character()) == 0) {
    ""
  } else {
    paste0(
      sprintf("<h3>%s</h3><ul>", .report_msg("report.warnings")),
      paste(vapply(res$warnings, function(w)
        sprintf("<li>%s</li>", .html_escape(w)),
        character(1)), collapse = "\n"),
      "</ul>"
    )
  }

  prior_html <- if (is.null(res$prior_check)) {
    ""
  } else {
    sprintf(.report_msg("report.spatial_prior_check"),
              .html_escape(res$prior_check$status %||% "not run"))
  }

  paste0(
    '<div class="system-card">\n',
    sprintf('<div class="name">%s</div>\n', .html_escape(res$name %||% "(unnamed)")),
    sprintf(.report_msg("report.card_meta"),
              .html_escape(res$system %||% "?"),
              .html_escape(res$rsg_or_order %||% "?"),
              .grade_class(res$evidence_grade),
              .html_escape(res$evidence_grade %||% "NA")),
    if (length(qual_html) > 0)
      sprintf('<div class="qualifiers">%s</div>\n', paste(qual_html, collapse = "")),
    prior_html,
    sprintf('<h3>%s</h3>\n', .report_msg("report.key_trace")),
    sprintf(.report_msg("report.trace_table"),
              trace_rows),
    ambig_html,
    missing_html,
    warn_html,
    '</div>\n'
  )
}

#' Render the horizons table from a PedonRecord.
#' @noRd
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
    sprintf("<h2>%s</h2>\n", .report_msg("report.horizons")),
    sprintf("<table><thead><tr>%s</tr></thead><tbody>%s</tbody></table>\n",
              header,
              paste(rows, collapse = "\n"))
  )
}

#' Render a provenance summary from a PedonRecord.
#' @noRd
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
    sprintf("<h2>%s</h2>\n", .report_msg("report.provenance_summary")),
    sprintf(.report_msg("report.provenance_table"),
              paste(rows, collapse = "\n"))
  )
}

#' Render the cross-system summary table when multiple results are provided.
#' @noRd
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
    sprintf("<h2>%s</h2>\n", .report_msg("report.cross_system_summary")),
    paste0(.report_msg("report.summary_table_head"),
             sprintf("<tbody>%s</tbody></table>\n",
                       paste(rows, collapse = "\n")))
  )
}

#' Render the spectral preprocessing sequence, if one was recorded.
#'
#' The Pro app stores the applied Vis-NIR treatment pipeline in
#' \code{pedon$spectra$preprocessing$steps} (an ordered character vector).
#' Returns an empty string when no spectrum/pipeline is present.
#' @noRd
.html_spectra_preprocess <- function(pedon) {
  pp <- tryCatch(pedon$spectra$preprocessing, error = function(e) NULL)
  steps <- pp$steps
  if (is.null(steps) || !length(steps)) return("")
  seq_html <- paste(vapply(as.character(steps), .html_escape, character(1)),
                    collapse = ' <span class="seq-arrow">&rarr;</span> ')
  paste0(
    sprintf("<h2>%s</h2>\n", .report_msg("report.spectra_preprocess")),
    sprintf("<p>%s</p>\n", .report_msg("report.spectra_preprocess_desc")),
    sprintf('<p class="spectra-seq">%s</p>\n', seq_html))
}

#' Render site metadata header.
#' @noRd
#' @param pedon A \code{\link{PedonRecord}}.
.html_site_header <- function(pedon) {
  if (is.null(pedon) || is.null(pedon$site)) return("")
  s <- pedon$site
  bits <- list()
  if (!is.null(s$id))               bits[[.report_msg("report.site_id")]]    <- s$id
  if (!is.null(s$lat) && !is.null(s$lon))
    bits[[.report_msg("report.site_coords")]] <- sprintf("%.4f, %.4f", s$lat, s$lon)
  if (!is.null(s$country))          bits[[.report_msg("report.site_country")]] <- s$country
  if (!is.null(s$parent_material))  bits[[.report_msg("report.site_parent_material")]] <- s$parent_material
  if (!is.null(s$elevation_m))      bits[[.report_msg("report.site_elevation")]] <- s$elevation_m
  if (!is.null(s$slope_pct))        bits[[.report_msg("report.site_slope")]]    <- s$slope_pct
  if (!is.null(s$date))             bits[[.report_msg("report.site_date")]]         <- s$date
  if (length(bits) == 0) return("")
  rows <- vapply(seq_along(bits), function(i)
    sprintf("<tr><th>%s</th><td>%s</td></tr>",
              .html_escape(names(bits)[i]),
              .html_escape(bits[[i]])),
    character(1))
  paste0(sprintf("<h2>%s</h2>\n", .report_msg("report.site")),
           "<table><tbody>",
           paste(rows, collapse = "\n"),
           "</tbody></table>\n")
}


# ---- v0.9.168: branded header, locator map, multi-profile support -----------

#' Branded report header: the soilKey logo (data: URI) beside the title.
#' @noRd
.html_report_header <- function(title, logo_uri) {
  logo_html <- if (nzchar(logo_uri %||% ""))
    sprintf('<img src="%s" alt="soilKey">', logo_uri) else ""
  sprintf('<div class="report-header">%s<div class="rh-title">%s</div></div>\n',
          logo_html, .html_escape(title))
}

#' A locator-map card (self-contained base64 image + caption). Empty when no
#' finite coordinate is available.
#' @noRd
.html_map_card <- function(map_uri, caption) {
  if (is.null(map_uri) || !nzchar(map_uri)) return("")
  sprintf(paste0('<div class="map-card"><img src="%s" alt="%s">',
                 '<div class="cap">%s</div></div>\n'),
          map_uri, .html_escape(caption), .html_escape(caption))
}

#' TRUE when x is a (non-empty) list of PedonRecords -> a multi-profile report.
#' @noRd
.report_multi_pedons <- function(x) {
  is.list(x) && !inherits(x, "PedonRecord") && length(x) >= 1L &&
    all(vapply(x, inherits, logical(1), "PedonRecord"))
}

#' Overview table for a multi-profile report: one row per profile with its
#' coordinates and the name it received in each system.
#' @noRd
.html_multi_summary <- function(pedons, per_results) {
  name_for <- function(res, sys) {
    r <- Find(function(z) identical(tolower(z$system %||% ""), sys) ||
                grepl(sys, tolower(z$system %||% ""), fixed = TRUE), res)
    if (is.null(r)) "--" else (r$name %||% "--")
  }
  rows <- vapply(seq_along(pedons), function(i) {
    p <- pedons[[i]]; res <- per_results[[i]]
    coord <- if (!is.null(p$site$lat) && !is.null(p$site$lon))
      sprintf("%.3f, %.3f", as.numeric(p$site$lat), as.numeric(p$site$lon)) else "--"
    sprintf("<tr><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>",
            .html_escape(p$site$id %||% sprintf("profile %d", i)),
            .html_escape(coord),
            .html_escape(name_for(res, "wrb")),
            .html_escape(name_for(res, "sibcs")),
            .html_escape(name_for(res, "usda")))
  }, character(1))
  paste0(
    sprintf("<h2>%s</h2>\n", .report_msg("report.profiles_overview")),
    sprintf(.report_msg("report.overview_table"),
            paste(rows, collapse = "\n")))
}

#' Render a multi-profile HTML report: a first-page overview (map of all points
#' + summary table) followed by one page per profile.
#' @noRd
.report_html_multi <- function(pedons, file, title, include_family,
                               specifiers) {
  if (is.null(title))
    title <- sprintf(.report_msg("report.n_profiles_title"), length(pedons))
  logo    <- .report_logo_data_uri()
  map_uri <- .report_map_data_uri(pedons)
  per <- lapply(pedons, function(p)
    .normalise_results(p, pedon = p, include_family = include_family,
                       specifiers = specifiers)$results)

  pages <- vapply(seq_along(pedons), function(i) {
    p <- pedons[[i]]; res <- per[[i]]
    paste0(
      '<section class="profile-page">\n',
      sprintf("<h2>%s</h2>\n",
              .html_escape(p$site$id %||% sprintf("profile %d", i))),
      .html_site_header(p),
      paste(vapply(res, .html_classification_card, character(1)),
            collapse = "\n"),
      .html_horizons_table(p),
      "</section>\n")
  }, character(1))

  body <- paste0(
    .html_head(title),
    .html_report_header(title, logo),
    sprintf(.report_msg("report.generated_by"),
            format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
            .soilkey_version()),
    .html_map_card(map_uri, .report_msg("report.map_caption")),
    .html_multi_summary(pedons, per),
    sprintf("<h2>%s</h2>\n", .report_msg("report.per_profile_reports")),
    paste(pages, collapse = "\n"),
    "<footer>\n", .report_msg("report.footer"), "</footer>\n",
    "</body></html>\n")

  dir.create(dirname(normalizePath(file, mustWork = FALSE)),
             recursive = TRUE, showWarnings = FALSE)
  writeLines(body, file, useBytes = TRUE)
  invisible(file)
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
#' @param include_family,specifiers Passed through to the keys when
#'               \code{x} is a \code{PedonRecord}; see \code{\link{report}}.
#' @param lang   Report language; \code{"en"} (default) or \code{"pt"}
#'               (Brazilian Portuguese).
#' @param ...    Currently unused.
#' @return       The output path, invisibly.
#' @export
report_html <- function(x,
                        file,
                        pedon = NULL,
                        title = NULL,
                        include_family = FALSE,
                        specifiers = FALSE,
                        lang = c("en", "pt"),
                        ...) {
  lang <- match.arg(lang)
  old_lang <- getOption("soilKey.report_lang")
  options(soilKey.report_lang = lang)
  on.exit(options(soilKey.report_lang = old_lang), add = TRUE)
  # A list of PedonRecords -> multi-profile report (overview map + one page each).
  if (.report_multi_pedons(x))
    return(.report_html_multi(x, file = file, title = title,
                              include_family = include_family,
                              specifiers = specifiers))
  norm <- .normalise_results(x, pedon = pedon,
                             include_family = include_family,
                             specifiers = specifiers)
  results <- norm$results
  pedon   <- norm$pedon

  if (is.null(title)) {
    pedon_id <- if (!is.null(pedon) && !is.null(pedon$site$id))
                  pedon$site$id else "soilKey report"
    title <- paste0("soilKey -- ", pedon_id)
  }

  body <- paste0(
    .html_head(title),
    .html_report_header(title, .report_logo_data_uri()),
    sprintf(.report_msg("report.generated_by"),
              format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
              .soilkey_version()),
    .html_site_header(pedon),
    .html_map_card(.report_map_data_uri(pedon),
                   .report_msg("report.map_caption")),
    .html_summary_table(results),
    sprintf("<h2>%s</h2>\n", .report_msg("report.classification_results")),
    paste(vapply(results, .html_classification_card, character(1)),
            collapse = "\n"),
    .html_horizons_table(pedon),
    .html_spectra_preprocess(pedon),
    .html_provenance_table(pedon),
    "<footer>\n",
    .report_msg("report.footer"),
    "</footer>\n",
    "</body></html>\n"
  )

  dir.create(dirname(normalizePath(file, mustWork = FALSE)),
             recursive = TRUE, showWarnings = FALSE)
  writeLines(body, file, useBytes = TRUE)
  invisible(file)
}
