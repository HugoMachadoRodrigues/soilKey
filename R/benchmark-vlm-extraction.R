# =============================================================================
# v0.9.66 -- Phase 1: VLM extraction benchmark.
#
# Question we want to answer before deciding whether to fine-tune:
#   "Is the vanilla Gemma 4 e2b / e4b + pedologist persona good enough on
#    real soilKey extraction tasks, or do we need few-shot / LoRA?"
#
# Three tasks, three metrics:
#
#   1. Munsell-from-photo:   Delta-E 2000 perceptual color distance
#                            (lower is better; <= 2.3 = imperceptible)
#   2. Horizons-from-text:   precision + recall over horizon count;
#                            per-attribute match rate over numeric fields
#   3. Site-from-fieldsheet: field-level Intersection-over-Union +
#                            value accuracy on matched fields
#
# Each task has a directory of paired (input, golden_json) fixtures
# under inst/fixtures/vlm_extraction/<task>/. Fixtures are either
# bundled (synthetic) OR added by the user (real photos / PDFs).
# =============================================================================


# ---- Fixture loader -------------------------------------------------------

#' Locate the soilKey VLM-extraction fixture directory
#'
#' Resolves to `system.file("fixtures", "vlm_extraction", ...)` after
#' install, or to `inst/fixtures/vlm_extraction/` in a development
#' checkout. Errors when neither is found.
#'
#' @keywords internal
.vlm_fixtures_dir <- function(subdir = NULL) {
  base <- system.file("fixtures", "vlm_extraction", package = "soilKey")
  if (!nzchar(base) || !dir.exists(base)) {
    base <- file.path("inst", "fixtures", "vlm_extraction")
  }
  if (!is.null(subdir)) base <- file.path(base, subdir)
  base
}


#' List the paired (input, golden) fixtures available for one task
#'
#' Each task directory holds matched files: an `input` (`.txt` for
#' horizons / site, `.jpg`/`.png` for munsell) and a `golden.json`
#' with the ground-truth answer. The pairing rule is filename-stem.
#'
#' @param task One of `"munsell"`, `"horizons"`, `"site"`.
#' @param fixtures_dir Optional override (default uses bundled).
#'
#' @return data.frame with columns `id`, `input_path`, `golden_path`
#'   (one row per fixture).
#' @export
list_vlm_fixtures <- function(task = c("munsell", "horizons", "site"),
                                 fixtures_dir = NULL) {
  task <- match.arg(task)
  base <- fixtures_dir %||% .vlm_fixtures_dir(task)
  if (!dir.exists(base)) {
    return(data.frame(id = character(0), input_path = character(0),
                        golden_path = character(0), stringsAsFactors = FALSE))
  }
  golden <- list.files(base, pattern = "\\.golden\\.json$",
                          full.names = TRUE)
  if (length(golden) == 0L) {
    return(data.frame(id = character(0), input_path = character(0),
                        golden_path = character(0), stringsAsFactors = FALSE))
  }
  ids <- sub("\\.golden\\.json$", "", basename(golden))
  ext_map <- list(munsell  = c("jpg", "jpeg", "png", "webp"),
                   horizons = c("txt", "md", "pdf"),
                   site     = c("txt", "md", "jpg", "png"))
  exts <- ext_map[[task]]
  input_paths <- vapply(ids, function(id) {
    cands <- file.path(base, paste0(id, ".", exts))
    hit   <- cands[file.exists(cands)]
    if (length(hit) == 0L) NA_character_ else hit[1L]
  }, character(1L))
  keep <- !is.na(input_paths)
  data.frame(
    id          = ids[keep],
    input_path  = input_paths[keep],
    golden_path = golden[keep],
    stringsAsFactors = FALSE
  )
}


# ---- Synthetic fixture generator -----------------------------------------

#' Generate a synthetic horizons-extraction fixture from a real pedon
#'
#' Renders a `PedonRecord$horizons` table back into a Markdown-style
#' description (the input the VLM will see) and emits the original
#' structured horizon table as the golden answer. This lets us scale
#' the horizons-task fixture set from any pedon source we already
#' have a loader for (BDsolos, FEBR, KSSL, LUCAS, ...).
#'
#' Useful as a *unit-test* fixture: the VLM should be able to round-
#' trip its own description into structured JSON. Limitation: the
#' description is template-rendered (uniform style); does not exercise
#' truly natural-language variation. Pair with hand-curated real-PDF
#' fixtures.
#'
#' @param pedon A `[PedonRecord]`.
#' @param fixture_id Filename stem (no extension) that the input + golden
#'   files will share.
#' @param out_dir Directory to write `<fixture_id>.txt` and
#'   `<fixture_id>.golden.json`. Default: bundled horizons fixtures dir.
#'
#' @return Invisibly, the named list `(input_path, golden_path)`.
#' @export
make_synthetic_horizons_fixture <- function(pedon,
                                                 fixture_id,
                                                 out_dir = NULL) {
  if (!inherits(pedon, "PedonRecord")) {
    stop("'pedon' must be a PedonRecord.")
  }
  if (!is.character(fixture_id) || length(fixture_id) != 1L ||
        !nzchar(fixture_id)) {
    stop("'fixture_id' must be a non-empty character scalar.")
  }
  out_dir <- out_dir %||% .vlm_fixtures_dir("horizons")
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  h <- as.data.frame(pedon$horizons)
  if (nrow(h) == 0L) stop("pedon has no horizons.")

  # Render Markdown description (one section per horizon).
  lines <- c(
    "# Descricao do perfil",
    "",
    sprintf("Local: %s, %s.", pedon$site$state %||% "?",
              pedon$site$municipality %||% "?"),
    sprintf("Identificacao: %s.", pedon$site$id %||% "?"),
    ""
  )
  for (i in seq_len(nrow(h))) {
    sect <- sprintf("## Horizonte %s (%g a %g cm)",
                      h$designation[i] %||% "?",
                      h$top_cm[i]    %||% NA,
                      h$bottom_cm[i] %||% NA)
    body <- character(0)
    if (!is.null(h$munsell_hue_moist) && !is.na(h$munsell_hue_moist[i])) {
      body <- c(body, sprintf("Cor Munsell umida: %s %g/%g.",
                                  h$munsell_hue_moist[i],
                                  h$munsell_value_moist[i] %||% NA,
                                  h$munsell_chroma_moist[i] %||% NA))
    }
    if (!is.null(h$clay_pct) && !is.na(h$clay_pct[i])) {
      body <- c(body, sprintf("Argila %g %%.", h$clay_pct[i]))
    }
    if (!is.null(h$silt_pct) && !is.na(h$silt_pct[i])) {
      body <- c(body, sprintf("Silte %g %%.", h$silt_pct[i]))
    }
    if (!is.null(h$sand_pct) && !is.na(h$sand_pct[i])) {
      body <- c(body, sprintf("Areia %g %%.", h$sand_pct[i]))
    }
    if (!is.null(h$ph_h2o) && !is.na(h$ph_h2o[i])) {
      body <- c(body, sprintf("pH em agua: %.1f.", h$ph_h2o[i]))
    }
    if (!is.null(h$oc_pct) && !is.na(h$oc_pct[i])) {
      body <- c(body, sprintf("Carbono organico: %.2f %%.", h$oc_pct[i]))
    }
    lines <- c(lines, sect, "", body, "")
  }

  golden <- list(
    horizons = lapply(seq_len(nrow(h)), function(i) {
      row <- as.list(h[i, , drop = FALSE])
      Filter(function(v) length(v) > 0L && !is.na(v), row)
    })
  )

  in_path  <- file.path(out_dir, paste0(fixture_id, ".txt"))
  out_path <- file.path(out_dir, paste0(fixture_id, ".golden.json"))

  writeLines(lines, in_path, useBytes = TRUE)
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("'jsonlite' is required to write golden JSON fixtures.")
  }
  writeLines(jsonlite::toJSON(golden, pretty = TRUE, auto_unbox = TRUE,
                                  na = "null"),
               out_path, useBytes = TRUE)
  invisible(list(input_path = in_path, golden_path = out_path))
}


# ---- Metric: Munsell Delta-E 2000 ----------------------------------------

#' Pairwise perceptual color distance between two Munsell triplets
#'
#' Prefers the Nickerson Color Difference Index (operates directly on
#' HVC, well-known in pedology and the Munsell renotation literature)
#' via `munsellinterpol::NickersonColorDifference`. Falls back to a
#' CIE Lab Euclidean distance (ΔE 1976) computed via
#' `munsellinterpol::MunsellToLab` when Nickerson is unavailable.
#' Returns `NA_real_` when either Munsell triplet is unparseable.
#'
#' Approximate Nickerson scale (matches Δ Lab roughly 1:1 for Munsell
#' value 4 chromas 1-8): `< 2` = visually equivalent;
#' `2-5` = noticeable but small; `> 10` = clearly different colors.
#'
#' @param hue1,value1,chroma1 First Munsell color (e.g. `"5YR", 4, 6`).
#' @param hue2,value2,chroma2 Second Munsell color.
#'
#' @return Numeric scalar (Nickerson or Lab distance), or `NA_real_`.
#' @keywords internal
.munsell_delta_e <- function(hue1, value1, chroma1,
                                hue2, value2, chroma2) {
  if (!requireNamespace("munsellinterpol", quietly = TRUE)) {
    return(NA_real_)
  }
  if (any(is.na(c(hue1, value1, chroma1, hue2, value2, chroma2)))) {
    return(NA_real_)
  }
  hvc1 <- sprintf("%s %s/%s", hue1, value1, chroma1)
  hvc2 <- sprintf("%s %s/%s", hue2, value2, chroma2)
  # Prefer Nickerson Color Difference Index (Munsell-domain).
  ncd <- tryCatch(
    munsellinterpol::NickersonColorDifference(hvc1, hvc2),
    error = function(e) NULL,
    warning = function(w) NULL
  )
  if (!is.null(ncd) && is.numeric(ncd) && length(ncd) >= 1L &&
        is.finite(as.numeric(ncd)[1L])) {
    return(as.numeric(ncd)[1L])
  }
  # Fallback: ΔE 1976 (Euclidean Lab distance).
  lab1 <- tryCatch(munsellinterpol::MunsellToLab(hvc1), error = function(e) NULL)
  lab2 <- tryCatch(munsellinterpol::MunsellToLab(hvc2), error = function(e) NULL)
  if (is.null(lab1) || is.null(lab2)) return(NA_real_)
  sqrt(sum((as.numeric(lab1) - as.numeric(lab2))^2))
}


#' Mean Delta-E 2000 between predicted and golden Munsell horizons
#'
#' Pairs predicted horizons to golden horizons by index (assumes the
#' ordering by depth is consistent, the same convention soilKey uses
#' throughout). Returns the mean over the min(length) horizons; pads
#' missing predictions with `NA` (penalised separately via the
#' coverage rate).
#'
#' @keywords internal
.metric_munsell_deltaE <- function(pred, golden) {
  pred_h   <- pred$horizons   %||% list()
  golden_h <- golden$horizons %||% list()
  n <- min(length(pred_h), length(golden_h))
  if (n == 0L) {
    return(list(mean_delta_e = NA_real_,
                n_compared = 0L,
                coverage = if (length(golden_h) == 0L) NA_real_ else 0))
  }
  des <- vapply(seq_len(n), function(i) {
    p <- pred_h[[i]]; g <- golden_h[[i]]
    .munsell_delta_e(
      p$munsell_hue_moist,    p$munsell_value_moist,    p$munsell_chroma_moist,
      g$munsell_hue_moist,    g$munsell_value_moist,    g$munsell_chroma_moist
    )
  }, numeric(1L))
  list(
    mean_delta_e = mean(des, na.rm = TRUE),
    n_compared   = sum(!is.na(des)),
    coverage     = n / max(1L, length(golden_h))
  )
}


# ---- Metric: horizons precision/recall + attribute match -----------------

#' Precision / recall on horizon count + numeric attribute match rate
#'
#' Counts how many predicted horizons line up with a golden horizon
#' under the depth-overlap heuristic (>=80 % overlap of [top, bottom]
#' interval) and what fraction of numeric attributes agree within a
#' small tolerance. The overlap heuristic gives partial credit when
#' the model splits / merges adjacent horizons.
#'
#' @keywords internal
.metric_horizons_overlap <- function(pred, golden,
                                        numeric_tol = 0.10) {
  pred_h   <- pred$horizons   %||% list()
  golden_h <- golden$horizons %||% list()
  n_g <- length(golden_h); n_p <- length(pred_h)
  if (n_g == 0L) return(list(precision = NA_real_, recall = NA_real_,
                                attr_match_rate = NA_real_,
                                n_pred = n_p, n_golden = 0L))

  matched_g <- logical(n_g); matched_p <- logical(n_p)
  attr_total <- 0L; attr_match <- 0L
  for (i in seq_len(n_g)) {
    g <- golden_h[[i]]
    g_top <- as.numeric(g$top_cm    %||% NA)
    g_bot <- as.numeric(g$bottom_cm %||% NA)
    if (is.na(g_top) || is.na(g_bot) || g_bot <= g_top) next
    g_span <- g_bot - g_top
    best_j <- NA_integer_; best_overlap <- 0
    for (j in seq_len(n_p)) {
      if (matched_p[j]) next
      p <- pred_h[[j]]
      p_top <- as.numeric(p$top_cm    %||% NA)
      p_bot <- as.numeric(p$bottom_cm %||% NA)
      if (is.na(p_top) || is.na(p_bot) || p_bot <= p_top) next
      overlap <- max(0, min(g_bot, p_bot) - max(g_top, p_top))
      if (overlap / g_span > best_overlap) {
        best_overlap <- overlap / g_span; best_j <- j
      }
    }
    if (!is.na(best_j) && best_overlap >= 0.80) {
      matched_g[i] <- TRUE; matched_p[best_j] <- TRUE
      # Attribute-level match
      p <- pred_h[[best_j]]
      for (key in c("clay_pct", "silt_pct", "sand_pct", "ph_h2o",
                      "oc_pct",   "cec_cmol", "bs_pct")) {
        g_raw <- g[[key]]
        p_raw <- p[[key]]
        gv <- if (length(g_raw) == 0L) NA_real_ else
                  suppressWarnings(as.numeric(g_raw))
        pv <- if (length(p_raw) == 0L) NA_real_ else
                  suppressWarnings(as.numeric(p_raw))
        if (length(gv) == 1L && is.finite(gv)) {
          attr_total <- attr_total + 1L
          if (length(pv) == 1L && is.finite(pv) &&
                abs(gv - pv) <= numeric_tol * max(1, abs(gv))) {
            attr_match <- attr_match + 1L
          }
        }
      }
    }
  }
  list(
    precision = if (n_p > 0L) sum(matched_p) / n_p else NA_real_,
    recall    = sum(matched_g) / n_g,
    attr_match_rate = if (attr_total > 0L) attr_match / attr_total else NA_real_,
    n_pred    = n_p,
    n_golden  = n_g
  )
}


# ---- Metric: site fields IoU ---------------------------------------------

#' Field-level Intersection-over-Union + value accuracy
#'
#' For site metadata: how many of the golden fields appear in the
#' prediction (recall), how many predicted fields appear in golden
#' (precision), and -- for the matched fields -- what fraction agree
#' on value. Numeric values use `numeric_tol`; character uses exact
#' (case-insensitive, trimmed) match.
#'
#' @keywords internal
.metric_site_iou <- function(pred, golden, numeric_tol = 0.05) {
  ps <- pred$site   %||% list()
  gs <- golden$site %||% list()
  pk <- names(ps); gk <- names(gs)
  inter <- intersect(pk, gk); union_keys <- union(pk, gk)
  iou <- if (length(union_keys) == 0L) NA_real_ else length(inter) / length(union_keys)
  recall    <- if (length(gk) == 0L) NA_real_ else length(inter) / length(gk)
  precision <- if (length(pk) == 0L) NA_real_ else length(inter) / length(pk)

  v_ok <- 0L; v_total <- 0L
  for (k in inter) {
    pv <- ps[[k]]; gv <- gs[[k]]
    if (is.numeric(gv) || (is.character(gv) && !is.na(suppressWarnings(as.numeric(gv))))) {
      pn <- suppressWarnings(as.numeric(pv))
      gn <- suppressWarnings(as.numeric(gv))
      if (is.finite(pn) && is.finite(gn)) {
        v_total <- v_total + 1L
        if (abs(pn - gn) <= numeric_tol * max(1, abs(gn))) {
          v_ok <- v_ok + 1L
        }
      }
    } else {
      v_total <- v_total + 1L
      if (identical(tolower(trimws(as.character(pv))),
                      tolower(trimws(as.character(gv))))) {
        v_ok <- v_ok + 1L
      }
    }
  }
  list(iou = iou, precision = precision, recall = recall,
       value_accuracy = if (v_total > 0L) v_ok / v_total else NA_real_,
       n_pred = length(pk), n_golden = length(gk),
       n_matched = length(inter))
}


# ---- Top-level benchmark --------------------------------------------------

#' Provider-agnostic VLM extraction benchmark (Phase 1)
#'
#' Runs each (provider, model) pair against every fixture for every
#' selected task and reports per-fixture and per-(provider, task)
#' aggregate metrics. Mock providers (`MockVLMProvider`) are accepted
#' for unit testing.
#'
#' @param providers Named list of provider specifications. Each entry
#'   is one of: a pre-built ellmer Chat object; a MockVLMProvider;
#'   a list `(name = ..., model = ...)` forwarded to
#'   [vlm_provider()].
#' @param tasks Subset of `c("munsell", "horizons", "site")`.
#' @param fixtures_dir Optional override; default = bundled fixtures.
#' @param max_per_task Cap fixtures per task (useful for smoke tests).
#' @param verbose Logical (default TRUE); print per-fixture progress.
#'
#' @return List with
#'   \describe{
#'     \item{`predictions`}{long data.frame: provider, task, fixture,
#'           ok, error, raw_pred, golden, metric_*}
#'     \item{`summary`}{data.frame: provider x task aggregates}
#'   }
#'
#' @section What this does NOT measure:
#'   - Latency / cost per request (use the provider's own telemetry).
#'   - End-to-end classification accuracy (run
#'     `benchmark_bdsolos_sibcs()` for that).
#'   - VLM hallucination outside the schema (the JSON validator catches
#'     that as a parse failure, counted as `ok = FALSE`).
#'
#' @examples
#' \dontrun{
#' # Compare local Gemma e2b vs e4b vs Claude:
#' bench <- benchmark_vlm_extraction(
#'   providers = list(
#'     gemma_e2b = list(name = "ollama", model = "gemma4:e2b"),
#'     gemma_e4b = list(name = "ollama", model = "gemma4:e4b"),
#'     claude    = list(name = "anthropic")
#'   ),
#'   tasks = c("horizons", "site"),     # skip Munsell if no photo fixtures
#'   max_per_task = 5
#' )
#' bench$summary
#' }
#' @seealso [list_vlm_fixtures()], [make_synthetic_horizons_fixture()],
#'   [extract_horizons_from_pdf()].
#' @export
benchmark_vlm_extraction <- function(providers,
                                         tasks        = c("horizons", "site", "munsell"),
                                         fixtures_dir = NULL,
                                         max_per_task = NULL,
                                         verbose      = TRUE) {
  tasks <- match.arg(tasks, several.ok = TRUE)
  if (!is.list(providers) || length(providers) == 0L ||
        is.null(names(providers)) ||
        any(!nzchar(names(providers)))) {
    stop("'providers' must be a non-empty named list.")
  }

  rows <- list()
  for (task in tasks) {
    fxs <- list_vlm_fixtures(task, fixtures_dir = fixtures_dir)
    if (!is.null(max_per_task) && nrow(fxs) > max_per_task) {
      fxs <- fxs[seq_len(max_per_task), , drop = FALSE]
    }
    if (nrow(fxs) == 0L) {
      if (isTRUE(verbose)) {
        cli::cli_alert_warning("Task {.field {task}}: no fixtures found.")
      }
      next
    }
    for (pname in names(providers)) {
      pspec <- providers[[pname]]
      provider <- .resolve_provider(pspec)
      for (k in seq_len(nrow(fxs))) {
        fx <- fxs[k, ]
        if (isTRUE(verbose)) {
          cli::cli_alert_info("[{.field {pname}}] task={task} fixture={fx$id}")
        }
        out <- .run_one_extraction(provider, task, fx)
        metric <- .compute_metric(task, out$pred, out$golden)
        rows[[length(rows) + 1L]] <- data.frame(
          provider   = pname,
          task       = task,
          fixture    = fx$id,
          ok         = isTRUE(out$ok),
          error      = out$error %||% NA_character_,
          metric_1   = metric[[1L]] %||% NA_real_,
          metric_2   = metric[[2L]] %||% NA_real_,
          metric_3   = metric[[3L]] %||% NA_real_,
          metric_1_name = names(metric)[1L] %||% NA_character_,
          metric_2_name = names(metric)[2L] %||% NA_character_,
          metric_3_name = names(metric)[3L] %||% NA_character_,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  predictions <- if (length(rows) > 0L) do.call(rbind, rows) else
                    data.frame(provider = character(0), task = character(0),
                                stringsAsFactors = FALSE)

  summary <- if (nrow(predictions) > 0L) {
    df <- predictions
    df$ok_num <- as.integer(df$ok)
    parts <- split(df, list(df$provider, df$task), drop = TRUE)
    do.call(rbind, lapply(parts, function(d) {
      data.frame(
        provider = d$provider[1L],
        task     = d$task[1L],
        n        = nrow(d),
        ok_rate  = mean(d$ok_num, na.rm = TRUE),
        metric_1_mean = mean(d$metric_1, na.rm = TRUE),
        metric_2_mean = mean(d$metric_2, na.rm = TRUE),
        metric_3_mean = mean(d$metric_3, na.rm = TRUE),
        metric_1_name = d$metric_1_name[1L],
        metric_2_name = d$metric_2_name[1L],
        metric_3_name = d$metric_3_name[1L],
        stringsAsFactors = FALSE
      )
    }))
  } else NULL
  if (!is.null(summary)) rownames(summary) <- NULL

  list(predictions = predictions, summary = summary)
}


# ---- Internal helpers ----------------------------------------------------

# Build / accept a provider object from a spec.
.resolve_provider <- function(spec) {
  if (inherits(spec, "MockVLMProvider")) return(spec)
  if (inherits(spec, "Chat")) return(spec)
  if (is.list(spec) && !is.null(spec$name)) {
    return(do.call(vlm_provider, spec))
  }
  stop("provider spec must be a Chat object, MockVLMProvider, or list(name=..., model=...).")
}


# Single (provider, fixture) extraction call. Loads input + golden,
# routes to the right extract_* helper, returns parsed JSON or error.
.run_one_extraction <- function(provider, task, fx) {
  golden <- tryCatch(jsonlite::fromJSON(fx$golden_path, simplifyVector = FALSE),
                       error = function(e) NULL)
  if (is.null(golden)) {
    return(list(ok = FALSE, error = "golden JSON unreadable",
                pred = NULL, golden = NULL))
  }
  out <- tryCatch({
    if (identical(task, "horizons")) {
      txt <- paste(readLines(fx$input_path, warn = FALSE,
                                encoding = "UTF-8"), collapse = "\n")
      ped <- PedonRecord$new(
        site = list(id = fx$id, country = "BR"),
        horizons = ensure_horizon_schema(
          data.table::data.table(top_cm = numeric(0), bottom_cm = numeric(0))
        )
      )
      extract_horizons_from_pdf(ped, pdf_text = txt, provider = provider,
                                  overwrite = TRUE)
      list(horizons = lapply(seq_len(nrow(ped$horizons)), function(i) {
        as.list(ped$horizons[i, ])
      }))
    } else if (identical(task, "site")) {
      ext <- tolower(tools::file_ext(fx$input_path))
      if (ext %in% c("txt", "md")) {
        # Text-mode site fixture: bypass extract_site_from_fieldsheet
        # (which is photo-only) and use the same prompt + schema +
        # validate_or_retry contract directly.
        txt <- paste(readLines(fx$input_path, warn = FALSE,
                                  encoding = "UTF-8"), collapse = "\n")
        schema_json <- load_schema("site")
        rendered <- load_prompt("extract_site_from_text",
                                  vars = list(schema_json = schema_json,
                                                 document_text = txt))
        res <- validate_or_retry(provider, rendered, "site",
                                    max_retries = 3L, image = NULL)
        # Schema wraps every field in {value, confidence, source_quote}.
        # Unwrap so the predicted site is flat -- matching the
        # canonical PedonRecord$site shape and the golden fixtures.
        extracted <- res$data$site %||% res$data
        flat <- lapply(extracted, function(v) {
          if (is.list(v) && !is.null(v$value)) v$value else v
        })
        flat <- Filter(function(v) length(v) > 0L && !all(is.na(unlist(v))),
                          flat)
        list(site = flat)
      } else {
        ped <- PedonRecord$new(
          site = list(id = fx$id, country = "BR"),
          horizons = ensure_horizon_schema(
            data.table::data.table(top_cm = numeric(0), bottom_cm = numeric(0))
          )
        )
        extract_site_from_fieldsheet(ped, image_path = fx$input_path,
                                         provider = provider, overwrite = TRUE)
        list(site = ped$site)
      }
    } else if (identical(task, "munsell")) {
      ped <- PedonRecord$new(
        site = list(id = fx$id, country = "BR"),
        horizons = ensure_horizon_schema(
          data.table::data.table(top_cm = numeric(0), bottom_cm = numeric(0))
        )
      )
      extract_munsell_from_photo(ped, image_path = fx$input_path,
                                    provider = provider, overwrite = TRUE)
      list(horizons = lapply(seq_len(nrow(ped$horizons)), function(i) {
        as.list(ped$horizons[i, ])
      }))
    } else NULL
  }, error = function(e) e)
  if (inherits(out, "error")) {
    return(list(ok = FALSE, error = conditionMessage(out),
                pred = NULL, golden = golden))
  }
  list(ok = TRUE, error = NA_character_, pred = out, golden = golden)
}


# Pick the right metric helper for the task, return a 3-slot named list
# (NAs for unused slots).
.compute_metric <- function(task, pred, golden) {
  if (is.null(pred) || is.null(golden)) {
    return(list(NA_real_, NA_real_, NA_real_))
  }
  if (identical(task, "munsell")) {
    m <- .metric_munsell_deltaE(pred, golden)
    list(mean_delta_e = m$mean_delta_e, coverage = m$coverage,
         n_compared   = m$n_compared)
  } else if (identical(task, "horizons")) {
    m <- .metric_horizons_overlap(pred, golden)
    list(precision = m$precision, recall = m$recall,
         attr_match = m$attr_match_rate)
  } else if (identical(task, "site")) {
    m <- .metric_site_iou(pred, golden)
    list(iou = m$iou, value_accuracy = m$value_accuracy,
         recall = m$recall)
  } else list(NA_real_, NA_real_, NA_real_)
}
