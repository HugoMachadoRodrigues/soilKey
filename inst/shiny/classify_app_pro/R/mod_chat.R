# =============================================================================
# soilKey Pro -- "Talk to soilKey Pro" chat (v0.9.176).
#
# Replaces the old Photo tab's Mock/Local/Cloud provider selector with a single
# conversational assistant:
#
#   * With a FREE Groq API key (GROQ_API_KEY) it chats with an open model via
#     ellmer, chosen from what Groq offers (utils_groq.R) -- no paid key,
#     nothing runs on the user's machine.
#   * With NO key it still answers, from a built-in SCRIPTED assistant grounded
#     in the current pedon and its deterministic classification, so the demo is
#     never dead.
#
# The model NEVER classifies: soilKey's deterministic keys do that, and the
# assistant only explains the result. This module is text-only -- photo ->
# Munsell / site extraction lives in the standalone Photo tab (mod_photo.R,
# restored in v0.9.181).
# =============================================================================

# The text model is chosen at use time from Groq's own list of models available
# to the key (.groq_model("text"), utils_groq.R). It used to be the fixed
# "llama-3.3-70b-versatile"; when Groq retired it every reply silently fell back
# to the built-in summary while the status still read "connected". An explicit
# options(soilKey.groq_text_model=) or $GROQ_TEXT_MODEL still wins while Groq
# offers it.

# Resolve the Groq key: the in-app field wins, else the GROQ_API_KEY env var.
.chat_groq_key <- function(field) {
  k <- if (!is.null(field) && nzchar(trimws(field))) trimws(field) else ""
  if (nzchar(k)) k else Sys.getenv("GROQ_API_KEY", "")
}

# Build an ellmer Groq chat (or NULL if no key / not available). api_key is
# deprecated in ellmer >= 0.4 in favour of credentials, so suppress that note.
.chat_make_groq <- function(key, model, system_prompt) {
  if (!nzchar(key) || !requireNamespace("ellmer", quietly = TRUE)) return(NULL)
  pa <- .groq_text_params(model)
  tryCatch(
    suppressWarnings(ellmer::chat_groq(
      system_prompt = system_prompt, model = model, api_key = key,
      params = ellmer::params(temperature = pa$temperature, max_tokens = pa$max_tokens),
      api_args = .groq_api_args(model), echo = "none")),
    error = function(e) NULL)
}

# One question to a chat, without holding the R process while Groq answers
# (seconds per reply, every other session on the instance waited). A promise of
# the reply text or of the error, resolved either way so the caller decides.
# Calls fail fast at Groq's limits: app.R sets options(ellmer_max_tries = 1),
# or ellmer would wait out Groq's Retry-After and try again; the user is told
# to try again in a minute instead.
.chat_ask_async <- function(chat, msg) {
  p <- tryCatch(chat$chat_async(msg),
                error = function(e) promises::promise_resolve(e))
  promises::then(p,
                 onFulfilled = function(x) if (inherits(x, "condition")) x
                                           else as.character(x),
                 onRejected  = function(e) e)
}

# The evidence the assistant may explain from. Without it the model was handed
# only the three class names and had to invent the reasons: asked why a profile
# is a Ferralsol and not an Acrisol, models answered with "redoximorphic
# features" the profile was not recorded to have, or "higher-activity clays"
# for Acrisols (the opposite of the definition). The right answer was in the
# key all along -- Ferralsols come before Acrisols in the WRB key, so Acrisols
# are never tested -- and now the model is given it.

# The horizons as recorded, one labelled line each, numbered as the key traces
# number them. Labels on every value: given a pipe table instead, a model read
# the Al saturation column as base saturation. Short labels, explained once:
# every token here is paid on every question, against Groq's per-minute limits.
.chat_horizon_lines <- function(h, max_extra = 8L, budget = 4500L, full = TRUE) {
  if (is.null(h) || !nrow(h)) return(NULL)
  v <- function(col, i) {
    x <- if (col %in% names(h)) h[[col]][i] else NA
    if (length(x) != 1L || is.na(x) || !nzchar(as.character(x))) return(NA_character_)
    if (is.numeric(x)) format(round(x, 2), trim = TRUE) else as.character(x)
  }
  colour <- function(what, hue, val, chr) {
    if (all(is.na(c(hue, val, chr)))) return(NULL)
    paste0(what, " ", if (!is.na(hue)) paste0(hue, " "),
           ifelse(is.na(val), "?", val), "/", ifelse(is.na(chr), "?", chr))
  }
  one <- function(fmt, x) if (is.na(x)) NULL else sprintf(fmt, x)
  single <- c(coarse_fragments_pct = "coarse fragments %s%%", clay_pct = "clay %s%%",
              silt_pct = "silt %s%%", sand_pct = "sand %s%%", ph_h2o = "pH H2O %s",
              ph_kcl = "pH KCl %s", oc_pct = "OC %s%%", n_total_pct = "N %s%%",
              cec_cmol = "CEC %s", ecec_cmol = "ECEC %s",
              bs_pct = "BS %s%%", al_sat_pct = "Al sat %s%%",
              fe_dcb_pct = "Fe DCB %s%%", caco3_pct = "CaCO3 %s%%",
              ec_dS_m = "EC %s dS/m", bulk_density_g_cm3 = "BD %s",
              consistence_moist = "consistence %s", clay_films_amount = "clay films %s")
  cations <- c(ca_cmol = "Ca", mg_cmol = "Mg", k_cmol = "K", na_cmol = "Na", al_cmol = "Al")
  field_only <- c("consistence_moist", "clay_films_amount", "n_total_pct",
                  "bulk_density_g_cm3", "coarse_fragments_pct")
  shown <- c("top_cm", "bottom_cm", "designation", names(single), names(cations),
             paste0("munsell_", c("hue", "value", "chroma"), "_moist"),
             paste0("munsell_", c("hue", "value", "chroma"), "_dry"),
             paste0("structure_", c("grade", "size", "type")))
  # anything else recorded, by its column name (ids and free-text notes aside)
  extra <- setdiff(names(h), shown)
  extra <- extra[!grepl("(^|_)id$|notes|source|method", extra)]
  lines <- vapply(seq_len(nrow(h)), function(i) {
    cat_v <- vapply(names(cations), v, character(1), i = i)
    parts <- c(
      colour("moist", v("munsell_hue_moist", i), v("munsell_value_moist", i),
             v("munsell_chroma_moist", i)),
      if (full) colour("dry", v("munsell_hue_dry", i), v("munsell_value_dry", i),
                       v("munsell_chroma_dry", i)),
      if (full) { st <- stats::na.omit(c(v("structure_grade", i), v("structure_size", i),
                                         v("structure_type", i)))
                  if (length(st)) paste("structure", paste(st, collapse = " ")) },
      unlist(lapply(if (full) names(single) else setdiff(names(single), field_only),
                    function(k) one(single[[k]], v(k, i)))),
      if (any(!is.na(cat_v)))
        paste("exch", paste(cations[!is.na(cat_v)], cat_v[!is.na(cat_v)],
                            collapse = " ")),
      if (full) utils::head(unlist(lapply(extra, function(k) {
        x <- v(k, i); if (is.na(x)) NULL else paste(k, x)
      })), max_extra))
    sprintf("  #%d %s %s-%s cm: %s", i, ifelse(is.na(v("designation", i)), "?",
                                                v("designation", i)),
            v("top_cm", i), v("bottom_cm", i), paste(parts, collapse = "; "))
  }, character(1))
  # A long profile (WoSIS has them with 40 layers) would crowd out the rest:
  # first the field description, structure and extra columns go, then the
  # deepest horizons, said so.
  if (full && sum(nchar(lines)) > budget)
    return(.chat_horizon_lines(h, max_extra, budget, full = FALSE))
  if (sum(nchar(lines)) > budget) {
    keep <- max(1L, sum(cumsum(nchar(lines)) <= budget))
    lines <- c(lines[seq_len(keep)],
               sprintf("  (%d deeper horizons not shown here)", length(lines) - keep))
  }
  c(paste0("Horizons as recorded (the key traces refer to them by #; moist/dry = ",
           "Munsell colour; BS = base saturation at pH 7; Al sat = Al saturation; ",
           "OC = organic carbon; CEC (pH 7), ECEC and exch(angeable) cations in ",
           "cmolc/kg; BD = bulk density, g/cm3):"), lines)
}

# A field of a node of the engine's evidence. Works on its R6 DiagnosticResult
# objects as on plain lists (and `[[` avoids the partial matching of `$`).
.chat_get <- function(node, k) {
  if (!is.list(node) && !is.environment(node)) return(NULL)
  tryCatch(node[[k]], error = function(e) NULL)
}

.chat_status <- function(p) {
  if (isTRUE(p)) "met" else if (identical(p, FALSE)) "not met" else "could not be checked"
}

.chat_text1 <- function(s) length(s) == 1L && !is.na(s) && nzchar(s)

# Horizon numbers as runs: 1:5 -> "#1-#5", c(1, 3, 4) -> "#1, #3-#4".
.chat_runs <- function(x) {
  x <- sort(unique(as.integer(x)))
  if (!length(x)) return("")
  br <- c(0L, which(diff(x) != 1L), length(x))
  paste(vapply(seq_len(length(br) - 1L), function(k) {
    r <- x[(br[k] + 1L):br[k + 1L]]
    if (length(r) == 1L) paste0("#", r) else paste0("#", r[1], "-#", r[length(r)])
  }, character(1)), collapse = ", ")
}

# The same sources, in fewer tokens.
.chat_short_ref <- function(ref) {
  ref <- sub("IUSS Working Group WRB \\(2022\\)", "WRB 2022", ref)
  ref <- sub("Embrapa \\(2018\\), SiBCS 5a ed\\.", "SiBCS 5", ref)
  ref <- sub("USDA Soil Survey Staff \\(2022\\), KST 13th ed\\.", "KST 13", ref)
  trimws(ref)
}

# Units of the per-horizon values the tests record. Without them the model
# guessed, and wrote the CEC per kg clay as "cmolc/kg per % clay".
.CHAT_UNITS <- c(cec_per_clay = "cmolc/kg clay", ecec_per_clay = "cmolc/kg clay",
                 ecec_per_kg_clay = "cmolc/kg clay",
                 ta_cmolc_per_kg_clay = "cmolc/kg clay",
                 thickness = "cm", thickness_cm = "cm", clay_pct = "%",
                 bs_pct = "%", al_sat_pct = "%", oc_pct = "%", caco3_pct = "%")

# The per-horizon values a test computed, with the limit it applied: the value
# named like the test (cec_per_clay, thickness), else the only one it recorded
# (bs_pct for SiBCS eutrófico). Tests keep them in $details or $evidence$layers.
# A test that records whether its limit is inclusive (the CEC per clay: WRB
# "< 16", USDA "16 or less") says so, so the two read differently.
.chat_values <- function(node, label) {
  d <- .chat_get(node, "details")
  if (!is.list(d) || !length(d)) d <- .chat_get(.chat_get(node, "evidence"), "layers")
  if (!is.list(d) || !length(d) || !all(vapply(d, is.list, logical(1)))) return(NULL)
  one <- function(x, k) {
    y <- .chat_get(x, k)
    if (is.numeric(y) && length(y) == 1L && !is.na(y)) y else NA_real_
  }
  fields <- unique(unlist(lapply(d, function(x)
    names(x)[vapply(x, function(y) is.numeric(y) && length(y) == 1L, logical(1))])))
  fields <- setdiff(fields, c("idx", "threshold", "passed", "result"))
  f <- if (label %in% fields) label else if (length(fields) == 1L) fields else return(NULL)
  v   <- vapply(d, one, numeric(1), k = f)
  idx <- vapply(d, one, numeric(1), k = "idx")
  ok  <- !is.na(v) & !is.na(idx)
  if (!any(ok)) return(NULL)
  th  <- unique(stats::na.omit(vapply(d, one, numeric(1), k = "threshold")))
  inc <- unique(unlist(lapply(d, function(x) {
    y <- .chat_get(x, "inclusive"); if (is.logical(y) && length(y) == 1L) y })))
  unit <- if (f %in% names(.CHAT_UNITS)) paste0(" ", .CHAT_UNITS[[f]]) else ""
  lim <- if (length(th) != 1L) NULL
         else if (identical(inc, FALSE)) paste0(" (must be below ", th, ")")
         else if (identical(inc, TRUE)) paste0(" (must be ", th, " or less)")
         else paste0(" (limit ", th, ")")
  paste0(if (!identical(f, label)) paste0(f, " "), "values ",
         paste(sprintf("#%d=%s", as.integer(idx[ok]), signif(v[ok], 3)), collapse = ", "),
         unit, lim)
}

# A criterion as evaluated, with the criteria under it.
.chat_criteria <- function(node, label, depth = 1L, max_depth = 3L) {
  passed <- .chat_get(node, "passed")
  if (length(passed) != 1L || depth > max_depth) return(character(0))
  lay  <- .chat_get(node, "layers")
  note <- .chat_get(node, "notes")
  ref  <- .chat_get(node, "reference")
  vals <- .chat_values(node, label)
  line <- paste0(strrep("  ", depth), "- ", label, ": ", .chat_status(passed),
    if (isTRUE(passed) && is.numeric(lay) && length(lay))
      paste0(" in horizons ", .chat_runs(lay)),
    if (length(vals)) paste0("; ", vals),
    # why it failed, unless the note is a changelog entry ("v0.9.61: ...")
    if (!isTRUE(passed) && .chat_text1(note) && !grepl("^v[0-9]", note))
      paste0(" (", note, ")"),
    if (.chat_text1(ref)) paste0(" [", .chat_short_ref(ref), "]"))
  kids <- .chat_get(node, "evidence")
  if (!is.list(kids) || !length(names(kids))) return(line)
  c(line, unlist(lapply(names(kids)[nzchar(names(kids))], function(n)
    .chat_criteria(kids[[n]], n, depth + 1L, max_depth))))
}

# Class names of one level of a key, in key order, from the rule files.
.chat_key_order <- function(system, level) {
  y <- tryCatch(yaml::read_yaml(system.file("rules", system, "key.yaml",
                                            package = "soilKey")),
                error = function(e) NULL)
  vapply(y[[level]] %||% list(), function(x) as.character(x$name %||% ""),
         character(1))
}

# A key as it was walked: each class tested with its outcome, the criteria
# behind the class assigned, and the classes after it, which are never tested.
.chat_key_trace <- function(tested, title, key_order) {
  tested <- Filter(is.list, tested %||% list())
  if (!length(tested)) return(NULL)
  lines <- paste0(title, ", classes in the order tested:")
  for (i in seq_along(tested)) {
    x <- tested[[i]]
    miss <- as.character(unlist(.chat_get(x, "missing")))
    lines <- c(lines, sprintf("  %d. %s: %s%s", i,
      .chat_get(x, "name") %||% .chat_get(x, "code") %||% "?",
      .chat_status(.chat_get(x, "passed")),
      if (length(miss)) paste0(" (missing ", paste(utils::head(miss, 2), collapse = ", "),
                               if (length(miss) > 2L) ", ...", ")")
      else ""))
  }
  last <- tested[[length(tested)]]
  nm <- .chat_get(last, "name")
  if (isTRUE(.chat_get(last, "passed")) && .chat_text1(nm)) {
    lines <- c(lines, sprintf("  Criteria behind %s, as evaluated:", nm))
    for (ev in .chat_get(last, "evidence") %||% list())
      lines <- c(lines, .chat_criteria(ev, .chat_get(ev, "test_name") %||% nm, 2L, 4L))
    pos <- match(nm, key_order)
    if (!is.na(pos) && pos < length(key_order))
      lines <- c(lines, sprintf("  Classes after %s in this key, therefore never tested: %s.",
                                nm, paste(key_order[-seq_len(pos)], collapse = ", ")))
  }
  lines
}

# The words a set of names shares at the start or the end ("Latossolos
# Vermelhos Distróficos ...", "... Hapludox"), said once instead of on each.
.chat_common_words <- function(x) {
  w <- strsplit(x, " ", fixed = TRUE)
  n <- min(lengths(w))
  if (length(x) < 2L || n < 2L) return(list(common = NULL, rest = x))
  same <- function(f) length(unique(vapply(w, f, character(1)))) == 1L
  pre <- 0L
  while (pre < n - 1L && same(function(z) z[pre + 1L])) pre <- pre + 1L
  suf <- 0L
  while (suf < n - 1L - pre && same(function(z) z[length(z) - suf])) suf <- suf + 1L
  if (pre + suf == 0L) return(list(common = NULL, rest = x))
  w1 <- w[[1]]
  list(common = paste(c(w1[seq_len(pre)], "...",
                        if (suf) w1[(length(w1) - suf + 1L):length(w1)]), collapse = " "),
       rest = vapply(w, function(z) paste(z[(pre + 1L):(length(z) - suf)], collapse = " "),
                     character(1)))
}

# The candidates of a lower level, on one line, the one assigned always kept.
.chat_level_line <- function(cands, label, max_n = 10L) {
  cands <- Filter(is.list, cands %||% list())
  if (!length(cands)) return(NULL)
  nm <- vapply(cands, function(x) as.character(.chat_get(x, "name") %||% "?"),
               character(1), USE.NAMES = FALSE)
  sh <- .chat_common_words(nm)
  if (!is.null(sh$common)) label <- sprintf("%s (%s)", label, sh$common)
  s <- vapply(seq_along(cands), function(j) {
    x <- cands[[j]]
    p <- .chat_get(x, "passed")
    miss <- as.character(unlist(.chat_get(x, "missing")))
    paste0(sh$rest[j], " (", .chat_status(p),
           if (is.na(p[1]) && length(miss))
             paste0(": missing ", paste(utils::head(miss, 3), collapse = ", ")),
           ")")
  }, character(1), USE.NAMES = FALSE)
  if (length(s) > max_n)
    s <- c(utils::head(s, max_n - 1L),
           sprintf("[%d more]", length(s) - max_n), s[length(s)])
  last <- cands[[length(cands)]]
  why <- if (isTRUE(.chat_get(last, "passed")))
    unlist(lapply(.chat_get(last, "evidence") %||% list(), function(ev)
      .chat_criteria(ev, .chat_get(ev, "test_name") %||% "criteria", 2L, 3L)))
  c(paste0("  ", label, " tested in key order: ", paste(s, collapse = "; "), "."), why)
}

# The qualifiers in the WRB name, each with the rule soilKey applied to it.
# Eutric is the one models got wrong: in WRB 2022 it compares exchangeable
# bases with exchangeable Al, it is no longer base saturation at pH 7.
.chat_wrb_qualifiers <- function(r, pedon) {
  q  <- .chat_get(r, "qualifiers")
  nm <- as.character(c(.chat_get(q, "principal"), .chat_get(q, "supplementary")))
  if (!length(nm)) return(NULL)
  ns <- asNamespace("soilKey")
  # Epic, Endic and Dorsic are defined by the RSG's own horizon (v0.9.217)
  # and take its code, as the resolver passes it.
  rsg <- tryCatch({
    rs <- get0("load_rules", envir = ns, inherits = FALSE)("wrb2022")$rsgs
    hit <- Filter(function(x) identical(x$name, .chat_get(r, "rsg_or_order")), rs)
    if (length(hit)) hit[[1]]$code
  }, error = function(e) NULL)
  c("  WRB qualifiers in the name, as evaluated:",
    vapply(nm, function(x) {
      if (identical(x, "Haplic"))
        return("  - Haplic: no other principal qualifier applies")
      fn  <- get0(paste0("qual_", tolower(x)), envir = ns, inherits = FALSE)
      res <- if (is.function(fn)) tryCatch(
        if ("rsg_code" %in% names(formals(fn))) fn(pedon, rsg_code = rsg) else fn(pedon),
        error = function(e) NULL)
      lay <- .chat_get(res, "layers")
      ref <- .chat_get(res, "reference")
      paste0("  - ", x, ": met",
             if (is.numeric(lay) && length(lay))
               paste0(" in horizons ", .chat_runs(lay)),
             if (.chat_text1(ref)) paste0(" [", .chat_short_ref(ref), "]"))
    }, character(1), USE.NAMES = FALSE))
}

# The evidence behind the three classifications, each system on its own so a
# failure in one leaves the others.
.chat_key_evidence <- function(res, pedon) {
  safe <- function(expr) tryCatch(expr, error = function(e) NULL)
  wrb <- safe(c(
    .chat_key_trace(.chat_get(res$wrb, "trace"), "WRB 2022 key",
                    .chat_key_order("wrb2022", "rsgs")),
    .chat_wrb_qualifiers(res$wrb, pedon)))
  sib <- safe({
    t <- .chat_get(res$sibcs, "trace")
    c(.chat_key_trace(t$ordens, "SiBCS 5 key (orders)",
                      .chat_key_order("sibcs5", "ordens")),
      .chat_level_line(t$subordens, "SiBCS suborders"),
      .chat_level_line(t$grandes_grupos, "SiBCS great groups"),
      .chat_level_line(t$subgrupos, "SiBCS subgroups"))
  })
  usd <- safe({
    t <- .chat_get(res$usda, "trace")
    c(.chat_key_trace(t$orders, "USDA Soil Taxonomy key (orders)",
                      .chat_key_order("usda", "orders")),
      .chat_level_line(t$suborders, "USDA suborders"),
      .chat_level_line(t$great_groups, "USDA great groups"),
      .chat_level_line(t$subgroups, "USDA subgroups"))
  })
  c(wrb, sib, usd)
}

# A compact, deterministic description of the current pedon + its classification
# ($text, fed to Groq AND shown by the scripted fallback), and the evidence
# behind it ($evidence, for Groq only: too long for a chat bubble). Reports what
# classify_all() found; it never asks a model to classify.
.chat_pedon_context <- function(pedon, settings = NULL) {
  if (is.null(pedon)) return(NULL)
  st <- tryCatch(settings, error = function(e) NULL)
  res <- tryCatch(.sk_with_session_opts(soilKey::classify_all(
    pedon, on_missing = "silent",
    include_familia = isTRUE(st$include_familia),
    include_family  = isTRUE(st$include_family),
    specifiers      = isTRUE(st$specifiers))),
    error = function(e) NULL)
  h <- tryCatch(as.data.frame(pedon$horizons), error = function(e) NULL)
  site <- pedon$site %||% list()
  # the rest of the site record (parent material, country, source, ...), the
  # one-value fields only; the long citation and licence texts stay out
  skip <- c("id", "lat", "lon", "crs", "citation", "attribution", "licence",
            "licence_short", "accessed", "wosis_profile_id")
  extra <- site[setdiff(names(site), skip)]
  extra <- extra[vapply(extra, function(x)
    is.atomic(x) && length(x) == 1L && !is.na(x) && nzchar(as.character(x)), logical(1))]
  lines <- c(
    sprintf("Site id: %s", site$id %||% "(unnamed)"),
    if (!is.null(site$lat) && !is.null(site$lon))
      sprintf("Location: lat %s, lon %s", site$lat, site$lon),
    if (length(extra))
      paste("Site:", paste(names(extra), vapply(extra, as.character, character(1)),
                           collapse = "; ")),
    if (!is.null(h) && nrow(h))
      sprintf("Horizons (%d): %s", nrow(h),
              paste(sprintf("%s %s-%s cm",
                            h$designation %||% "?", h$top_cm %||% "?",
                            h$bottom_cm %||% "?"), collapse = "; ")))
  say <- function(r, label) {
    if (is.null(r)) return(NULL)
    g <- r$evidence_grade %||% NA
    sprintf("%s: %s (%s; evidence grade %s)", label,
            r$name %||% "?", r$rsg_or_order %||% "?",
            if (is.na(g)) "none: no horizon has a soil property, the class was reached by elimination" else g)
  }
  cls <- c(say(res$wrb, "WRB 2022"), say(res$sibcs, "SiBCS 5"),
           say(res$usda, "USDA ST"))
  missing <- tryCatch({
    md <- unique(unlist(lapply(res[c("wrb", "sibcs", "usda")],
                               function(r) if (!is.null(r)) r$missing_data)))
    if (length(md)) paste("Missing data:", paste(md, collapse = ", ")) else NULL
  }, error = function(e) NULL)
  evidence <- tryCatch(c(.chat_horizon_lines(h), .chat_key_evidence(res, pedon)),
                       error = function(e) NULL)
  list(text     = paste(c(lines, cls, missing), collapse = "\n"),
       evidence = if (length(evidence)) paste(evidence, collapse = "\n"),
       results  = res)
}

# A question to the live model, start to finish. A model retired since Groq's
# list was last read is replaced (the list is read again, the conversation
# carried over) and asked once more. A model at its per-minute limit is NOT
# replaced: the user is asked to wait a minute. Until v0.9.209 another model
# (gpt-oss-120b) answered in its place, and in live use it got SiBCS rules
# wrong where Qwen had them right. A promise of list(reply, backend, msg):
# reply is the text, "rate_limited", or an error condition; backend is the one
# to keep (it changes after a retirement).
.chat_converse_async <- function(b, msg, system_prompt) {
  settle <- function(r, b) list(
    reply   = if (inherits(r, "error") && .groq_rate_limited(r)) "rate_limited" else r,
    backend = b, msg = msg)
  promises::then(.chat_ask_async(b$chat, msg), function(r) {
    if (inherits(r, "error") && .groq_model_gone(r)) {
      .groq_forget_models()
      model <- .groq_model("text", b$key)
      chat  <- if (!is.na(model)) .chat_make_groq(b$key, model, system_prompt)
      if (!is.null(chat)) {
        chat$set_turns(b$chat$get_turns())
        b <- list(chat = chat, model = model, key = b$key)
        return(promises::then(.chat_ask_async(chat, msg), function(r2) settle(r2, b)))
      }
    }
    settle(r, b)
  })
}

# What the live model is told: the instructions, the classification and the
# evidence behind it.
.chat_system_prompt <- function(ctx, lang = NULL) {
  txt <- ctx$text
  # the one-line horizon list and the missing-data list repeat what the
  # evidence gives in full
  if (length(ctx$evidence))
    txt <- paste(grep("^Horizons \\(|^Missing data:", strsplit(txt %||% "", "\n")[[1]],
                      value = TRUE, invert = TRUE), collapse = "\n")
  paste0(i18n("chat.system_prompt", lang = lang),
         if (!is.null(ctx)) paste0("\n\n### Current pedon & deterministic classification\n",
                                   txt)
         else paste0("\n\n### Current pedon\nNo profile has been loaded or built yet. ",
                     "If the user asks about \"this profile\", tell them to load an ",
                     "example or build one in the Pedon tab first."),
         if (length(ctx$evidence))
           paste0("\n\n### Evidence used by the keys\n", ctx$evidence))
}

# Scripted (no-key) assistant: a keyword intent router over the current pedon
# context, so the demo answers real questions without any model.
.chat_scripted_reply <- function(msg, ctx) {
  if (is.null(ctx)) return(i18n("chat.scripted_need_pedon"))
  m <- tolower(msg %||% "")
  r <- ctx$results
  grade <- function(x) {
    if (is.null(x)) return("?")
    g <- x$evidence_grade %||% NA
    if (is.na(g)) i18n("ui.no_measured_data") else g
  }
  if (grepl("horizon|camada|perfil|profile", m) && !grepl("wrb|sibcs|usda", m))
    return(paste0(i18n("chat.scripted_horizons"), "\n\n", ctx$text))
  if (grepl("\\bwrb\\b|world reference", m) && !is.null(r$wrb))
    return(sprintf("**WRB 2022:** %s\n\n%s: %s (%s)", r$wrb$name,
                   i18n("chat.reference_group"), r$wrb$rsg_or_order, grade(r$wrb)))
  if (grepl("sibcs|embrapa|brasil", m) && !is.null(r$sibcs))
    return(sprintf("**SiBCS 5:** %s (%s)", r$sibcs$name, grade(r$sibcs)))
  if (grepl("usda|soil taxonomy|order|great group", m) && !is.null(r$usda))
    return(sprintf("**USDA ST:** %s (%s)", r$usda$name, grade(r$usda)))
  if (grepl("missing|falta|why|por que|porque|grade|evid", m))
    return(paste0(i18n("chat.scripted_missing"), "\n\n", ctx$text))
  if (grepl("munsell|colou?r|\\bcor\\b", m))
    return(i18n("chat.scripted_photo_hint"))
  # default: full three-system summary + an offer to enable the live model
  paste0(i18n("chat.scripted_summary"), "\n\n", ctx$text, "\n\n",
         i18n("chat.scripted_addkey_hint"))
}

# One chat bubble (markdown -> HTML when commonmark is available).
.chat_bubble <- function(role, text) {
  cls <- if (identical(role, "user")) "sk-bubble sk-bubble-user"
         else "sk-bubble sk-bubble-bot"
  body <- if (requireNamespace("commonmark", quietly = TRUE))
    shiny::HTML(commonmark::markdown_html(text %||% ""))
  else shiny::HTML(gsub("\n", "<br/>", htmltools::htmlEscape(text %||% "")))
  shiny::div(class = "sk-bubble-row",
             shiny::div(class = cls, body))
}


# The chat now lives in a right-side slide-out DRAWER available on every tab
# (mounted in app.R), not a nav tab. This builds the drawer's inner content: a
# header, the transcript, and the composer. No API-key field and no photo upload
# -- the key comes from GROQ_API_KEY (else the grounded scripted assistant).
chat_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "sk-assistant-inner",
    shiny::div(
      class = "sk-assistant-head",
      shiny::div(
        shiny::span(class = "sk-assistant-title",
                    shiny::tags$img(src = "logo.png", class = "sk-assistant-logo",
                                    alt = "soilKey"), " ",
                    i18n("chat.drawer_title")),
        shiny::uiOutput(ns("backend_status"), inline = TRUE)),
      shiny::tags$button(
        id = "sk_assistant_close", class = "sk-assistant-close",
        type = "button", `aria-label` = i18n("chat.close"),
        shiny::icon("xmark"))),
    shiny::div(id = ns("log"), class = "sk-chat-log",
               role = "log", `aria-live` = "polite",
               shiny::uiOutput(ns("messages"))),
    shiny::div(
      class = "sk-chat-composer",
      shiny::textAreaInput(ns("msg"), NULL, width = "100%", rows = 2,
                           placeholder = i18n("chat.placeholder")),
      bslib::tooltip(
        bslib::input_task_button(ns("send"), i18n("chat.send"),
                               icon = shiny::icon("paper-plane"),
                               label_busy = i18n("chat.thinking"),
                               type = "primary"),
        i18n("chat.tip_send"))),
    shiny::div(class = "sk-assistant-foot small text-muted",
               i18n("chat.grounding_note"))
  )
}


chat_server <- function(id, rv, settings) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    history  <- shiny::reactiveVal(list())
    chat_obj <- shiny::reactiveVal(NULL)
    chat_sig <- shiny::reactiveVal("")
    chat_sys <- shiny::reactiveVal("")

    add <- function(role, text) {
      history(c(history(), list(list(role = role, text = text))))
      session$sendCustomMessage("sk_chat_scroll", ns("log"))
    }

    # A persistent Groq chat, rebuilt when the pedon / key / model changes so its
    # system prompt always reflects the current context (kept across turns while
    # those are stable, preserving conversation history).
    get_backend <- function() {
      key   <- .chat_groq_key(input$groq_key)
      model <- if (nzchar(key)) .groq_model("text", key) else NA_character_
      if (nzchar(key) && is.na(model))      # Groq offers nothing suitable now
        return(list(kind = "scripted", chat = NULL))
      sig   <- paste(rv$pedon$site$id %||% "none", model, nzchar(key), sep = "|")
      if (nzchar(key) &&
          (!identical(sig, chat_sig()) || is.null(chat_obj()))) {
        ctx <- .chat_pedon_context(rv$pedon, tryCatch(settings(), error = function(e) NULL))
        chat_sys(.chat_system_prompt(ctx))
        chat_obj(.chat_make_groq(key, model, chat_sys()))
        chat_sig(sig)
      }
      if (nzchar(key)) list(kind = "groq", chat = chat_obj(), model = model, key = key)
      else list(kind = "scripted", chat = NULL)
    }

    # The status names the model actually in use, read from Groq's list, rather
    # than claiming "connected" because a key exists. Re-checked when the key
    # changes and after a failed call (model_check).
    model_check <- shiny::reactiveVal(0L)
    output$backend_status <- shiny::renderUI({
      model_check()
      key <- .chat_groq_key(input$groq_key)
      if (!nzchar(key))
        return(shiny::div(class = "small mb-2 alert alert-light py-1 px-2 border",
                          shiny::icon("robot"), " ", i18n("chat.backend_scripted")))
      model <- .groq_model("text", key)
      if (is.na(model))
        return(shiny::div(class = "small mb-2 alert alert-warning py-1 px-2",
                          shiny::icon("triangle-exclamation"), " ",
                          i18n("chat.backend_unavailable")))
      shiny::div(class = "small mb-2", style = "color:#3f6024;",
                 shiny::icon("circle-check"), " ", i18n("chat.backend_groq", model))
    })
    # render eagerly so the status shows even while the settings sidebar starts
    # collapsed (otherwise the output stays suspended/pending until first shown)
    shiny::outputOptions(output, "backend_status", suspendWhenHidden = FALSE)

    output$messages <- shiny::renderUI({
      h <- history()
      if (!length(h))
        return(shiny::div(class = "text-muted p-3 text-center",
                          i18n("chat.empty")))
      shiny::tagList(
        lapply(h, function(m) .chat_bubble(m$role, m$text)),
        # while the live model answers
        if (identical(chat_task$status(), "running"))
          .chat_bubble("assistant", paste0("*", i18n("chat.thinking"), "*")))
    })

    # ---- send a text message ---------------------------------------------
    # The live model answers through an ExtendedTask: the reply is awaited
    # without holding the R process, so other sessions are not frozen for the
    # seconds each answer takes. The Send button stays busy meanwhile, and a
    # second message waits its turn.
    chat_task <- shiny::ExtendedTask$new(function(b, msg, sys)
      .chat_converse_async(b, msg, sys))
    bslib::bind_task_button(chat_task, "send")
    asked <- shiny::reactiveVal("")
    shiny::observeEvent(input$send, {
      msg <- trimws(input$msg %||% "")
      if (!nzchar(msg)) return()
      add("user", msg)
      shiny::updateTextAreaInput(session, "msg", value = "")
      backend <- get_backend()
      if (identical(backend$kind, "scripted") || is.null(backend$chat)) {
        ctx <- .chat_pedon_context(rv$pedon, tryCatch(settings(), error = function(e) NULL))
        add("assistant", .chat_scripted_reply(msg, ctx))
        return()
      }
      asked(msg)
      chat_task$invoke(backend, msg, chat_sys())
    })
    shiny::observeEvent(chat_task$status(), {
      if (!chat_task$status() %in% c("success", "error")) return()
      out <- tryCatch(chat_task$result(), error = function(e) list(reply = e))
      # a retired model was replaced: keep the new chat for the next question
      b <- out$backend
      if (!is.null(b$chat) && !identical(b$chat, chat_obj())) {
        chat_obj(b$chat)
        chat_sig(paste(rv$pedon$site$id %||% "none", b$model, TRUE, sep = "|"))
        model_check(model_check() + 1L)
      }
      reply <- out$reply
      if (identical(reply, "rate_limited")) return(add("assistant", i18n("chat.rate_limited")))
      if (inherits(reply, "error") || is.null(reply) || !nzchar(reply)) {
        ctx <- .chat_pedon_context(rv$pedon, tryCatch(settings(), error = function(e) NULL))
        return(add("assistant", paste0(i18n("chat.groq_failed"), "\n\n",
                                       .chat_scripted_reply(out$msg %||% asked(), ctx))))
      }
      add("assistant", reply)
    })
    shiny::observeEvent(chat_task$status(), {
      if (identical(chat_task$status(), "running"))
        session$sendCustomMessage("sk_chat_scroll", ns("log"))
    })
  })
}
