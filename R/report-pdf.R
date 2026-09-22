# =============================================================================
# soilKey -- Report rendering (PDF)
#
# Companion to `report-html.R`. The PDF path goes through a temporary
# Rmd file rendered by `rmarkdown::render(... output_format =
# "pdf_document")`, which in turn requires a working LaTeX install
# (or an alternative engine accepted by rmarkdown, such as `tinytex`).
#
# We chose this approach over a "render HTML and shell to wkhtmltopdf"
# pipeline because:
#
#   - rmarkdown is already in Suggests (used by the vignettes), so
#     adding the PDF path costs nothing in dependencies.
#   - LaTeX-rendered PDFs handle long key traces and tables better
#     than HTML-to-PDF at typical paper sizes.
#   - Pedologists who archive laudos in PDF generally already have a
#     TeX install for tables/figures.
#
# When neither rmarkdown nor LaTeX is available, the function
# degrades gracefully with an actionable error pointing to
# `tinytex::install_tinytex()`.
# =============================================================================


#' Render a soilKey classification report as PDF
#'
#' See \code{\link{report}} for the generic dispatcher. This function
#' assembles a temporary `.Rmd` file with the same content as
#' \code{\link{report_html}} (site, cross-system summary, classification
#' cards, horizons, provenance) and renders it via
#' \code{rmarkdown::render()}.
#'
#' @param x      A \code{ClassificationResult}, list of results, or
#'               \code{PedonRecord}.
#' @param file   Output \code{.pdf} path.
#' @param pedon  Optional \code{PedonRecord}.
#' @param title  Report title.
#' @param include_family,specifiers Passed through to the keys when
#'               \code{x} is a \code{PedonRecord}; see \code{\link{report}}.
#' @param lang   Report language, \code{"en"} (default) or \code{"pt"}
#'               (Brazilian Portuguese).
#' @param ...    Passed to \code{rmarkdown::render()}.
#' @return       The output path, invisibly.
#' @export
report_pdf <- function(x,
                       file,
                       pedon = NULL,
                       title = NULL,
                       include_family = FALSE,
                       specifiers     = FALSE,
                       lang = c("en", "pt"),
                       ...) {
  lang <- match.arg(lang)
  old_lang <- getOption("soilKey.report_lang")
  options(soilKey.report_lang = lang)
  on.exit(options(soilKey.report_lang = old_lang), add = TRUE)
  if (!requireNamespace("rmarkdown", quietly = TRUE))
    stop("Package 'rmarkdown' is required for PDF reports.\n",
         "  install.packages('rmarkdown')")

  # LaTeX-safe image assets (absolute, forward-slashed, temp-copied).
  latex_path <- function(p)
    if (nzchar(p %||% "")) gsub("\\\\", "/", normalizePath(p, mustWork = FALSE)) else ""
  logo_path <- ""
  logo_src  <- system.file("shiny", "classify_app_pro", "www", "logo.png",
                           package = "soilKey")
  if (nzchar(logo_src) && file.exists(logo_src)) {
    lt <- tempfile(fileext = ".png")
    if (file.copy(logo_src, lt, overwrite = TRUE)) {
      logo_path <- latex_path(lt); on.exit(unlink(lt), add = TRUE)
    }
  }
  map_path <- ""
  map_tmp  <- tempfile(fileext = ".png")
  on.exit(unlink(map_tmp), add = TRUE)   # cleaned up AFTER render, not before
  make_map <- function(pedons)
    if (.report_map_png(pedons, map_tmp)) latex_path(map_tmp) else ""

  if (.report_multi_pedons(x)) {
    if (is.null(title))
      title <- sprintf(.report_msg("report.n_profiles_title"), length(x))
    map_path <- make_map(x)
    rmd <- .build_report_rmd_multi(x, title = title, logo_path = logo_path,
                                   map_path = map_path,
                                   include_family = include_family,
                                   specifiers = specifiers)
  } else {
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
    map_path <- make_map(pedon)
    rmd <- .build_report_rmd(results, pedon = pedon, title = title,
                             logo_path = logo_path, map_path = map_path)
  }

  tmp_rmd <- tempfile(fileext = ".Rmd")
  writeLines(rmd, tmp_rmd, useBytes = TRUE)
  on.exit(unlink(tmp_rmd), add = TRUE)

  out_dir <- dirname(normalizePath(file, mustWork = FALSE))
  out_fn  <- basename(file)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  out <- tryCatch(
    rmarkdown::render(
      tmp_rmd,
      output_format = rmarkdown::pdf_document(toc       = FALSE,
                                               number_sections = FALSE,
                                               latex_engine    = "xelatex"),
      output_file   = out_fn,
      output_dir    = out_dir,
      quiet         = TRUE,
      ...
    ),
    error = function(e) {
      msg <- conditionMessage(e)
      if (grepl("(?i)pdflatex|xelatex|LaTeX|tinytex", msg)) {
        stop("PDF rendering failed -- a LaTeX engine appears to be missing.\n",
             "  Install one with `tinytex::install_tinytex()` or by",
             " installing TeX Live / MacTeX system-wide.\n",
             "  Underlying error: ", msg, call. = FALSE)
      }
      stop(e)
    }
  )

  invisible(out)
}


# ---- internal: build an Rmd from the assembled data --------------------------
#' Internal helper: .escape_latex


#' @noRd
.escape_latex <- function(x) {
  if (is.null(x)) return("")
  x <- as.character(x)
  x[is.na(x)] <- ""
  # Replace order matters: backslash first, then the rest.
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  x <- gsub("([&%$#_{}])", "\\\\\\1", x)
  x <- gsub("~",  "\\\\textasciitilde{}",  x)
  x <- gsub("\\^", "\\\\textasciicircum{}", x)
  x
}
#' Internal helper: .rmd_header

#' @noRd
#' @param logo_path Optional PNG path placed in the running page header (so the
#'   soilKey logo appears at the top of every page, including multi-profile
#'   reports). LaTeX-safe (forward slashes, no spaces) paths only.
.rmd_header <- function(title, logo_path = NULL) {
  logo_inc <- if (!is.null(logo_path) && nzchar(logo_path)) paste0(
    "  - \\usepackage{graphicx}\n",
    "  - \\usepackage{fancyhdr}\n",
    "  - \\pagestyle{fancy}\n",
    "  - \\fancyhf{}\n",
    "  - \\setlength{\\headheight}{24pt}\n",
    "  - \\addtolength{\\topmargin}{-12pt}\n",
    sprintf("  - \\fancyhead[L]{\\includegraphics[height=0.7cm]{%s}}\n",
            logo_path),
    "  - \\fancyhead[R]{\\small\\textbf{soilKey}}\n",
    "  - \\fancyfoot[C]{\\small\\thepage}\n",
    "  - \\renewcommand{\\headrulewidth}{0.4pt}\n",
    # Redefine the 'plain' page style (used by the title page) so the logo
    # header also appears on page 1.
    sprintf(paste0("  - \\fancypagestyle{plain}{\\fancyhf{}",
                   "\\fancyhead[L]{\\includegraphics[height=0.7cm]{%s}}",
                   "\\fancyhead[R]{\\small\\textbf{soilKey}}",
                   "\\fancyfoot[C]{\\small\\thepage}",
                   "\\renewcommand{\\headrulewidth}{0.4pt}}\n"),
            logo_path)) else ""
  paste0(
    "---\n",
    "title: \"", title, "\"\n",
    "date: \"", format(Sys.time(), "%Y-%m-%d"), "\"\n",
    "output:\n",
    "  pdf_document:\n",
    "    toc: false\n",
    "    number_sections: false\n",
    "  html_document:\n",
    "    toc: false\n",
    "    self_contained: true\n",
    "geometry: margin=2cm\n",
    "fontsize: 10pt\n",
    "header-includes:\n",
    "  - \\usepackage{longtable}\n",
    "  - \\usepackage{booktabs}\n",
    "  - \\usepackage{xcolor}\n",
    logo_inc,
    "---\n\n",
    "```{r setup, include=FALSE}\n",
    "knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE)\n",
    "```\n\n"
  )
}
#' Internal helper: .rmd_classification_block

#' @noRd
.rmd_classification_block <- function(res) {
  qual_principal <- res$qualifiers$principal     %||% character()
  qual_suppl     <- res$qualifiers$supplementary %||% character()

  # v0.9.165: normalise the system-dependent trace (flat for WRB, nested phases
  # for SiBCS/USDA) into one ordered table before rendering. Dropping `info`
  # rows keeps the trace to genuine decision steps; assigned-taxon rows render
  # like a passing step. WRB output is byte-identical to the previous rendering.
  tt <- .flatten_key_trace(res$trace)
  tt <- tt[tt$status != "info", , drop = FALSE]
  trace_lines <- if (nrow(tt) == 0) {
    paste0("_", .report_msg("report.no_trace"), "_")
  } else {
    paste(vapply(seq_len(nrow(tt)), function(i) {
      st  <- tt$status[i]
      sym <- if (st %in% c("passed", "selected"))
               paste0("**", .report_msg("report.trace_passed"), "**")
             else if (st == "failed") .report_msg("report.trace_failed")
             else                      .report_msg("report.trace_indeterminate")
      sprintf("%2d. `%-3s` %-20s -- %s%s",
                i,
                if (nzchar(tt$code[i])) tt$code[i] else "?",
                tt$name[i],
                sym,
                if (st %in% c("failed", "indeterminate") && tt$n_missing[i] > 0)
                  sprintf(.report_msg("report.attrs_missing_pdf"),
                            tt$n_missing[i])
                else "")
    }, character(1)), collapse = "  \n")
  }

  paste0(
    "## ", res$system %||% "?", "\n\n",
    "**", res$name %||% "(unnamed)", "**\n\n",
    sprintf("* %s: `", .report_msg("report.rsg_or_order")), res$rsg_or_order %||% "?", "`\n",
    sprintf("* %s: **", .report_msg("report.evidence_grade")), res$evidence_grade %||% "NA", "**\n",
    if (length(qual_principal) > 0)
      paste0(sprintf("* %s: ", .report_msg("report.principal_qualifiers")),
               paste(qual_principal, collapse = ", "), "\n"),
    if (length(qual_suppl) > 0)
      paste0(sprintf("* %s: ", .report_msg("report.supplementary_qualifiers")),
               paste(qual_suppl, collapse = ", "), "\n"),
    sprintf("\n### %s\n\n", .report_msg("report.key_trace")),
    trace_lines,
    "\n\n",
    if (length(res$ambiguities) > 0)
      paste0(sprintf("### %s\n\n", .report_msg("report.ambiguities")),
               paste(vapply(res$ambiguities, function(a)
                 sprintf("- **%s**: %s",
                           a$rsg_code %||% "?",
                           a$reason   %||% ""),
                 character(1)), collapse = "\n"),
               "\n\n")
      else "",
    if (length(res$missing_data %||% character()) > 0)
      paste0(sprintf("### %s\n\n", .report_msg("report.missing_data_heading")),
               paste(res$missing_data, collapse = ", "),
               "\n\n")
      else "",
    if (length(res$warnings %||% character()) > 0)
      paste0(sprintf("### %s\n\n", .report_msg("report.warnings")),
               paste(vapply(res$warnings, function(w)
                 sprintf("- %s", w),
                 character(1)), collapse = "\n"),
               "\n\n")
      else ""
  )
}
#' Internal helper: .rmd_summary_block

#' @noRd
.rmd_summary_block <- function(results) {
  if (length(results) < 2) return("")
  rows <- vapply(results, function(r)
    sprintf("| %s | %s | %s |",
              r$system          %||% "?",
              r$name            %||% "(unnamed)",
              r$evidence_grade  %||% "NA"),
    character(1))
  paste0(
    sprintf("## %s\n\n", .report_msg("report.cross_system_summary")),
    sprintf("| %s | %s | %s |\n",
              .report_msg("report.system"),
              .report_msg("report.name"),
              .report_msg("report.grade")),
    "|---|---|---|\n",
    paste(rows, collapse = "\n"),
    "\n\n"
  )
}
#' Internal helper: .rmd_horizons_block

#' @noRd
#' @param pedon A \code{\link{PedonRecord}}.
#' @param label Knitr chunk label; must be unique across the document (a
#'   multi-profile report renders one horizons block per profile).
.rmd_horizons_block <- function(pedon, label = "horizons") {
  if (is.null(pedon) || is.null(pedon$horizons) ||
        nrow(pedon$horizons) == 0) {
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

  paste0(
    sprintf("## %s\n\n", .report_msg("report.horizons")),
    sprintf("```{r %s}\n", label),
    "knitr::kable(",
    paste(deparse(h), collapse = ""),
    ", booktabs = TRUE, format.args = list(big.mark = ''))\n",
    "```\n\n"
  )
}
#' Internal helper: .rmd_site_block

#' @noRd
#' @param pedon A \code{\link{PedonRecord}}.
.rmd_site_block <- function(pedon) {
  if (is.null(pedon) || is.null(pedon$site)) return("")
  s <- pedon$site
  bits <- list()
  if (!is.null(s$id))              bits[[.report_msg("report.site_id")]]    <- s$id
  if (!is.null(s$lat) && !is.null(s$lon))
    bits[[.report_msg("report.coords")]] <- sprintf("%.4f, %.4f", s$lat, s$lon)
  if (!is.null(s$country))         bits[[.report_msg("report.country")]]         <- s$country
  if (!is.null(s$parent_material)) bits[[.report_msg("report.parent_material")]] <- s$parent_material
  if (!is.null(s$elevation_m))     bits[[.report_msg("report.elevation_m")]]     <- s$elevation_m
  if (!is.null(s$slope_pct))       bits[[.report_msg("report.slope_pct")]]       <- s$slope_pct
  if (!is.null(s$date))            bits[[.report_msg("report.date")]]            <- s$date
  if (length(bits) == 0) return("")
  rows <- vapply(seq_along(bits), function(i)
    sprintf("| %s | %s |", names(bits)[i], bits[[i]]),
    character(1))
  paste0(
    sprintf("## %s\n\n", .report_msg("report.site")),
    sprintf("| %s | %s |\n",
              .report_msg("report.field"),
              .report_msg("report.value")),
    "|---|---|\n",
    paste(rows, collapse = "\n"),
    "\n\n"
  )
}
#' Internal helper: .build_report_rmd

#' @noRd
.build_report_rmd <- function(results, pedon, title,
                              logo_path = NULL, map_path = NULL) {
  map_md <- if (!is.null(map_path) && nzchar(map_path))
    sprintf("![%s](%s)\n\n", .report_msg("report.map_caption"), map_path)
    else ""
  paste0(
    .rmd_header(title, logo_path),
    sprintf(.report_msg("report.pdf_footer"),
              format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
              .soilkey_version()),
    .rmd_site_block(pedon),
    map_md,
    .rmd_summary_block(results),
    sprintf("# %s\n\n", .report_msg("report.classification_results")),
    paste(vapply(results, .rmd_classification_block, character(1)),
            collapse = "\n"),
    .rmd_horizons_block(pedon)
  )
}

#' Multi-profile PDF: a first-page overview (map + summary table) then one page
#' per profile, separated by \\newpage.
#' @noRd
.build_report_rmd_multi <- function(pedons, title, logo_path, map_path,
                                    include_family, specifiers) {
  per <- lapply(pedons, function(p)
    .normalise_results(p, pedon = p, include_family = include_family,
                       specifiers = specifiers)$results)
  name_for <- function(res, sys) {
    r <- Find(function(z) grepl(sys, tolower(z$system %||% ""), fixed = TRUE),
              res)
    if (is.null(r)) "--" else (r$name %||% "--")
  }
  ov_rows <- vapply(seq_along(pedons), function(i) {
    p <- pedons[[i]]
    coord <- if (!is.null(p$site$lat) && !is.null(p$site$lon))
      sprintf("%.3f, %.3f", as.numeric(p$site$lat), as.numeric(p$site$lon))
      else "--"
    sprintf("| %s | %s | %s | %s | %s |",
            p$site$id %||% sprintf("profile %d", i), coord,
            name_for(per[[i]], "wrb"), name_for(per[[i]], "sibcs"),
            name_for(per[[i]], "usda"))
  }, character(1))
  overview <- paste0(
    if (!is.null(map_path) && nzchar(map_path))
      sprintf("![%s](%s)\n\n", .report_msg("report.map_caption"), map_path)
      else "",
    sprintf("# %s\n\n", .report_msg("report.profiles_overview")),
    "| Perfil | Coord | WRB 2022 | SiBCS 5 | USDA ST 13 |\n",
    "|---|---|---|---|---|\n",
    paste(ov_rows, collapse = "\n"), "\n\n")
  pages <- vapply(seq_along(pedons), function(i) {
    p <- pedons[[i]]
    paste0(
      "\\newpage\n\n",
      sprintf("# %s\n\n", p$site$id %||% sprintf("profile %d", i)),
      .rmd_site_block(p),
      paste(vapply(per[[i]], .rmd_classification_block, character(1)),
            collapse = "\n"),
      .rmd_horizons_block(p, label = sprintf("hz_%d", i)))
  }, character(1))
  paste0(
    .rmd_header(title, logo_path),
    sprintf(.report_msg("report.pdf_footer"),
              format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
              .soilkey_version()),
    overview,
    paste(pages, collapse = "\n"))
}
