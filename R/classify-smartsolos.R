# =============================================================================
# v0.9.54 -- SmartSolosExpert REST API client (cross-validation eixo).
#
# Glauber Vaz's PROLOG implementation of the SiBCS classifier is exposed
# by Embrapa's AgroAPI as a REST endpoint. This module wraps it so soilKey
# users can cross-validate the local classifier against an authoritative
# Embrapa-hosted reference.
#
# Endpoint (Swagger):
#   POST  https://api.cnptia.embrapa.br/smartsolos/expert/v1/classification
#   POST  https://api.cnptia.embrapa.br/smartsolos/expert/v1/verification
#
# Auth:
#   Authorization: Bearer <token>
#   Token comes from registration at agroapi.cnptia.embrapa.br
#   Set via env var AGROAPI_TOKEN (or argument api_key=).
# =============================================================================


# ---- Mapping tables: soilKey horizon column -> SmartSolos field --------

#' SmartSolos drainage class scale (DRENAGEM, 1-8)
#'
#' SiBCS / Embrapa drainage scale used by the SmartSolosExpert API:
#' 1 excessivamente drenado .. 8 muito mal drenado.
#' soilKey does not have a canonical drainage column yet; user supplies
#' via \code{drenagem} argument when known.
#'
#' @keywords internal
.SMARTSOLOS_DRAINAGE_SCALE <- c(
  "excessivamente drenado"      = 1L,
  "fortemente drenado"          = 2L,
  "acentuadamente drenado"      = 3L,
  "bem drenado"                 = 4L,
  "moderadamente drenado"       = 5L,
  "imperfeitamente drenado"     = 6L,
  "mal drenado"                 = 7L,
  "muito mal drenado"           = 8L
)


#' Map a soilKey \code{structure_grade} string to the SmartSolos integer
#' (\code{ESTRUTURA_GRAU}: 1=fraca, 2=moderada, 3=forte).
#' @keywords internal
.smartsolos_struct_grade <- function(x) {
  if (is.null(x) || is.na(x)) return(NA_integer_)
  s <- tolower(trimws(as.character(x)))
  out <- switch(s,
    "weak"        = 1L, "fraca"     = 1L, "fraco" = 1L,
    "moderate"    = 2L, "moderada"  = 2L, "moderado" = 2L,
    "strong"      = 3L, "forte"     = 3L,
    NA_integer_)
  out
}


#' Map \code{structure_size} (very fine .. very coarse) to SmartSolos
#' \code{ESTRUTURA_TAMANHO} (1..5).
#' @keywords internal
.smartsolos_struct_size <- function(x) {
  if (is.null(x) || is.na(x)) return(NA_integer_)
  s <- tolower(trimws(as.character(x)))
  switch(s,
    "very fine"  = 1L, "muito pequena"   = 1L, "muito pequeno"   = 1L,
    "fine"       = 2L, "pequena"         = 2L, "pequeno"         = 2L,
    "medium"     = 3L, "media"           = 3L, "media"           = 3L,
    "coarse"     = 4L, "grande"          = 4L,
    "very coarse"= 5L, "muito grande"    = 5L,
    NA_integer_)
}


#' Map \code{structure_type} to SmartSolos \code{ESTRUTURA_TIPO} (1..6).
#' @keywords internal
.smartsolos_struct_type <- function(x) {
  if (is.null(x) || is.na(x)) return(NA_integer_)
  s <- tolower(trimws(as.character(x)))
  if (grepl("granular", s)) return(1L)
  if (grepl("subangular", s)) return(3L)
  if (grepl("angular", s))    return(2L)
  if (grepl("prism",     s))  return(4L)
  if (grepl("column",    s) || grepl("colunar", s)) return(5L)
  if (grepl("lamin",     s))  return(6L)
  NA_integer_
}


#' Map \code{clay_films_amount} (few/common/many) to SmartSolos
#' \code{CEROSIDADE_QUANTIDADE} (1..3).
#' @keywords internal
.smartsolos_clay_films_amt <- function(x) {
  if (is.null(x) || is.na(x)) return(NA_integer_)
  s <- tolower(trimws(as.character(x)))
  switch(s,
    "few"        = 1L, "pouca" = 1L, "poucas" = 1L,
    "common"     = 2L, "comum" = 2L,
    "many"       = 3L, "muita" = 3L, "muitas" = 3L, "abundante" = 3L,
    NA_integer_)
}


#' Map \code{clay_films_strength} to SmartSolos \code{CEROSIDADE_GRAU} (1..3).
#' @keywords internal
.smartsolos_clay_films_strength <- function(x) {
  if (is.null(x) || is.na(x)) return(NA_integer_)
  s <- tolower(trimws(as.character(x)))
  switch(s,
    "weak"   = 1L, "fraca" = 1L, "fraco" = 1L,
    "moderate" = 2L, "moderada" = 2L, "moderado" = 2L,
    "strong" = 3L, "forte" = 3L,
    NA_integer_)
}


#' Convert one PedonRecord to the SmartSolosExpert request payload
#'
#' Builds the JSON-serialisable list expected by the
#' \code{POST /classification} (or \code{/verification}) endpoint.
#' Missing soilKey horizon attributes are sent as \code{NA} (the API
#' tolerates partial data and replies with \code{NULL} for taxonomic
#' levels that cannot be resolved).
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param drenagem Optional integer 1..8 (SiBCS drainage class) or
#'        a string in
#'        \code{c("excessivamente drenado", ..., "muito mal drenado")}.
#' @param reference_sibcs Optional named list with the user's
#'        reference SiBCS classification (\code{ordem, subordem,
#'        gde_grupo, subgrupo}) for use with the
#'        \code{/verification} endpoint.
#' @keywords internal
.smartsolos_pedon_to_payload <- function(pedon,
                                            drenagem = NULL,
                                            reference_sibcs = NULL) {
  if (!inherits(pedon, "PedonRecord")) {
    stop(".smartsolos_pedon_to_payload(): expected a PedonRecord.")
  }
  drenagem_int <- if (is.null(drenagem) || is.na(drenagem)) {
    NA_integer_
  } else if (is.numeric(drenagem)) {
    as.integer(drenagem)
  } else {
    v <- .SMARTSOLOS_DRAINAGE_SCALE[tolower(trimws(as.character(drenagem)))]
    if (length(v) == 0L || is.na(v)) NA_integer_ else unname(as.integer(v))
  }
  h <- pedon$horizons
  to_int <- function(x) if (is.null(x) || is.na(x)) NA_integer_ else as.integer(round(x))
  to_num <- function(x) if (is.null(x) || is.na(x)) NA_real_    else as.numeric(x)
  horizontes <- vector("list", nrow(h))
  for (i in seq_len(nrow(h))) {
    # Texture: convert percent -> grams/kg integer (SiBCS canonical)
    clay_g_kg <- to_int(h$clay_pct[i] * 10)
    silt_g_kg <- to_int(h$silt_pct[i] * 10)
    sand_total_g_kg <- to_int(h$sand_pct[i] * 10)
    # SmartSolos has separate AREIA_GROS / AREIA_FINA; if we only have total
    # sand_pct, split 50/50.
    areia_grossa <- if (is.na(sand_total_g_kg)) NA_integer_ else sand_total_g_kg %/% 2L
    areia_fina   <- if (is.na(sand_total_g_kg)) NA_integer_ else sand_total_g_kg - areia_grossa
    # OC: percent -> g/kg
    oc_g_kg <- to_num(h$oc_pct[i] * 10)
    horizontes[[i]] <- list(
      SIMB_HORIZ           = if (is.na(h$designation[i])) NA else as.character(h$designation[i]),
      LIMITE_SUP           = to_int(h$top_cm[i]),
      LIMITE_INF           = to_int(h$bottom_cm[i]),
      COR_UMIDA_MATIZ      = if (is.na(h$munsell_hue_moist[i])) NA else
                                as.character(h$munsell_hue_moist[i]),
      COR_UMIDA_VALOR      = to_num(h$munsell_value_moist[i]),
      COR_UMIDA_CROMA      = to_num(h$munsell_chroma_moist[i]),
      COR_SECA_MATIZ       = if (is.na(h$munsell_hue_dry[i])) NA else
                                as.character(h$munsell_hue_dry[i]),
      COR_SECA_VALOR       = to_num(h$munsell_value_dry[i]),
      COR_SECA_CROMA       = to_num(h$munsell_chroma_dry[i]),
      ESTRUTURA_GRAU       = .smartsolos_struct_grade(h$structure_grade[i]),
      ESTRUTURA_TAMANHO    = .smartsolos_struct_size(h$structure_size[i]),
      ESTRUTURA_TIPO       = .smartsolos_struct_type(h$structure_type[i]),
      CEROSIDADE_GRAU      = .smartsolos_clay_films_strength(h$clay_films_strength[i]),
      CEROSIDADE_QUANTIDADE= .smartsolos_clay_films_amt(h$clay_films_amount[i]),
      CONSISTENCIA_SECO    = NA_integer_,  # not in soilKey schema yet
      AREIA_GROS           = areia_grossa,
      AREIA_FINA           = areia_fina,
      SILTE                = silt_g_kg,
      ARGILA               = clay_g_kg,
      PH_AGUA              = to_num(h$ph_h2o[i]),
      PH_KCL               = to_num(h$ph_kcl[i]),
      C_ORG                = oc_g_kg,
      CA_TROC              = to_num(h$ca_cmol[i]),
      MG_TROC              = to_num(h$mg_cmol[i]),
      K_TROC               = to_num(h$k_cmol[i]),
      NA_TROC              = to_num(h$na_cmol[i]),
      AL_TROC              = to_num(h$al_cmol[i]),
      # H_TROC: H+Al if cec, bs, al available; else NA
      H_TROC               = NA_real_,
      P_ASSIM              = to_int(h$p_mehlich3_mg_kg[i])
    )
  }
  ref <- reference_sibcs %||% list()
  list(
    items = list(list(
      ID_PONTO   = as.character(pedon$site$id %||% "soilkey-pedon"),
      DRENAGEM   = drenagem_int,
      HORIZONTES = horizontes,
      ORDEM      = ref$ordem      %||% "",
      SUBORDEM   = ref$subordem   %||% "",
      GDE_GRUPO  = ref$gde_grupo  %||% "",
      SUBGRUPO   = ref$subgrupo   %||% ""
    ))
  )
}


#' Convert a SmartSolosExpert response to a soilKey ClassificationResult
#' @keywords internal
.smartsolos_response_to_result <- function(resp, pedon, endpoint) {
  if (!is.list(resp) || is.null(resp$items) || length(resp$items) == 0L) {
    stop(".smartsolos_response_to_result(): empty 'items' in response.")
  }
  it <- resp$items[[1L]]
  ordem <- it$ORDEM     %||% NA_character_
  sub   <- it$SUBORDEM  %||% NA_character_
  gg    <- it$GDE_GRUPO %||% NA_character_
  sg    <- it$SUBGRUPO  %||% NA_character_
  parts <- c(ordem, sub, gg, sg)
  parts <- parts[!is.na(parts) & nzchar(parts)]
  display <- if (length(parts) > 0L) paste(parts, collapse = " ") else NA_character_
  trace_combined <- list(
    smartsolos_endpoint = endpoint,
    smartsolos_response = it
  )
  if (endpoint == "verification" && !is.null(resp$items_bd)) {
    trace_combined$smartsolos_user_reference <- resp$items_bd[[1L]]
    trace_combined$smartsolos_summary        <- resp$summary
  }
  warnings <- character(0)
  if (endpoint == "verification" && !is.null(resp$summary)) {
    s <- resp$summary
    if (isTRUE(s$L0 > 0L)) {
      warnings <- c(warnings,
                     sprintf("SmartSolos /verification: ORDEM mismatch (L0=%d).",
                              s$L0))
    }
  }
  ClassificationResult$new(
    system         = "SiBCS 5a edicao (SmartSolosExpert API)",
    name           = display %||% "(unclassified)",
    rsg_or_order   = ordem,
    qualifiers     = list(
      subordem  = sub,
      gde_grupo = gg,
      subgrupo  = sg
    ),
    trace          = trace_combined,
    ambiguities    = list(),
    missing_data   = character(0),
    evidence_grade = "B",   # external classifier, not directly traceable
    prior_check    = NULL,
    warnings       = warnings
  )
}


# ---- Public API --------------------------------------------------------

#' Classify a PedonRecord via Embrapa's SmartSolosExpert REST API
#'
#' Sends a soilKey \code{\link{PedonRecord}} to the SmartSolosExpert
#' REST endpoint maintained by Embrapa (Glauber Vaz's PROLOG-based
#' implementation of the SiBCS classifier) and returns the resulting
#' four-level classification (Ordem / Subordem / Grande Grupo /
#' Subgrupo) wrapped in a soilKey
#' \code{\link{ClassificationResult}}.
#'
#' This is an **external classifier** -- the package does not host or
#' replicate the PROLOG rules. The function exists so soilKey users
#' can cross-validate the local classifier against an authoritative
#' Embrapa-hosted reference. Use the \code{"verification"} endpoint to
#' compare against your own user-supplied reference classification
#' (the API returns a per-level match \code{summary} with counters
#' \code{L0..L4}).
#'
#' Authentication: register a free AgroAPI account at
#' \url{https://www.agroapi.cnptia.embrapa.br/portal/}, subscribe to
#' the SmartSolosExpert API and generate an access token. Pass it via
#' the \code{AGROAPI_TOKEN} environment variable or the
#' \code{api_key} argument.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param api_key Bearer token. Defaults to
#'        \code{Sys.getenv("AGROAPI_TOKEN")}. Required unless
#'        \code{post_fn} is supplied (test injection).
#' @param endpoint One of \code{"classification"} (default; classify
#'        only) or \code{"verification"} (classify + compare against
#'        user-supplied \code{reference_sibcs}).
#' @param drenagem Optional drainage class. Integer 1..8 or
#'        Portuguese string (\code{"bem drenado"} etc.).
#' @param reference_sibcs Optional named list (\code{ordem,
#'        subordem, gde_grupo, subgrupo}) used by the
#'        \code{"verification"} endpoint as the user's reference.
#' @param base_url Override base URL. Default
#'        \code{"https://api.cnptia.embrapa.br/smartsolos/expert/v1"}.
#' @param timeout_seconds HTTP timeout (default 30).
#' @param post_fn Internal: function with signature
#'        \code{function(payload) -> response_list} for unit
#'        tests. When supplied, the network is bypassed.
#' @param verbose If \code{TRUE} (default), emits a one-line summary.
#' @return A \code{\link{ClassificationResult}} with
#'         \code{system = "SiBCS 5a edicao (SmartSolosExpert API)"}
#'         and the four taxonomic levels in
#'         \code{rsg_or_order} (Ordem) and \code{qualifiers}
#'         (Subordem / GdeGrupo / Subgrupo). Verification-mode
#'         responses additionally carry \code{trace$smartsolos_summary}
#'         (the per-level match counters \code{L0..L4}).
#'
#' @examples
#' \dontrun{
#' Sys.setenv(AGROAPI_TOKEN = "<your token>")
#' res <- classify_via_smartsolos_api(make_argissolo_canonical())
#' res$rsg_or_order      # "ARGISSOLO"
#' res$qualifiers
#' #> $subordem  "VERMELHO"
#' #> $gde_grupo "Distrofico"
#' #> $subgrupo  "tipico"
#' }
#' @seealso \code{\link{classify_sibcs}} for the local PROLOG-free
#'          classifier; \code{\link{compare_smartsolos}} for a
#'          side-by-side comparison helper.
#' @export
classify_via_smartsolos_api <- function(pedon,
                                          api_key         = Sys.getenv("AGROAPI_TOKEN"),
                                          endpoint        = c("classification",
                                                                "verification"),
                                          drenagem        = NULL,
                                          reference_sibcs = NULL,
                                          base_url        = "https://api.cnptia.embrapa.br/smartsolos/expert/v1",
                                          timeout_seconds = 30,
                                          post_fn         = NULL,
                                          verbose         = TRUE) {
  endpoint <- match.arg(endpoint)
  if (!inherits(pedon, "PedonRecord")) {
    stop("classify_via_smartsolos_api(): 'pedon' must be a PedonRecord.")
  }
  payload <- .smartsolos_pedon_to_payload(pedon,
                                            drenagem = drenagem,
                                            reference_sibcs = reference_sibcs)

  if (is.null(post_fn)) {
    if (!requireNamespace("httr", quietly = TRUE)) {
      stop("Package 'httr' is required for classify_via_smartsolos_api(). ",
           "Install with `install.packages(\"httr\")`.")
    }
    if (!requireNamespace("jsonlite", quietly = TRUE)) {
      stop("Package 'jsonlite' is required for classify_via_smartsolos_api(). ",
           "Install with `install.packages(\"jsonlite\")`.")
    }
    if (is.null(api_key) || !nzchar(api_key)) {
      stop("classify_via_smartsolos_api(): no API token. ",
           "Set AGROAPI_TOKEN env var or pass api_key=. Get one at ",
           "https://www.agroapi.cnptia.embrapa.br/portal/")
    }
    url <- paste0(base_url, "/", endpoint)
    body_json <- jsonlite::toJSON(payload, auto_unbox = TRUE,
                                    null = "null", na = "null")
    if (isTRUE(verbose)) {
      cli::cli_alert_info(sprintf(
        "POST %s (pedon id = %s, %d horizon(s))",
        url, pedon$site$id %||% "?", length(payload$items[[1L]]$HORIZONTES)
      ))
    }
    resp <- httr::POST(
      url,
      httr::add_headers(
        Authorization  = paste("Bearer", api_key),
        `Content-Type` = "application/json",
        Accept         = "application/json"
      ),
      body = body_json,
      encode = "raw",
      httr::timeout(timeout_seconds)
    )
    sc <- httr::status_code(resp)
    if (sc != 200L) {
      stop(sprintf("SmartSolosExpert API HTTP %d: %s",
                    sc, substr(httr::content(resp, "text", encoding = "UTF-8"),
                                1L, 500L)))
    }
    parsed <- httr::content(resp, "parsed", encoding = "UTF-8")
  } else {
    parsed <- post_fn(payload)
  }

  out <- .smartsolos_response_to_result(parsed, pedon, endpoint)
  if (isTRUE(verbose)) {
    cli::cli_alert_success(sprintf(
      "SmartSolos -> %s",
      paste(c(out$rsg_or_order, out$qualifiers$subordem,
                out$qualifiers$gde_grupo, out$qualifiers$subgrupo),
              collapse = " / ")
    ))
  }
  out
}


#' Cross-validate the local SiBCS classifier against the SmartSolosExpert API
#'
#' Runs both \code{\link{classify_sibcs}} (local) and
#' \code{\link{classify_via_smartsolos_api}} (remote PROLOG via
#' Embrapa AgroAPI) on the same \code{\link{PedonRecord}} and tabulates
#' agreement at each of the four SiBCS categorical levels.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param ... Forwarded to \code{\link{classify_via_smartsolos_api}}.
#' @return A list with \code{local} and \code{remote}
#'         \code{ClassificationResult}s plus a one-row
#'         \code{agreement} data.frame with columns
#'         \code{ordem, subordem, gde_grupo, subgrupo, n_match}.
#'
#' @examples
#' \dontrun{
#' Sys.setenv(AGROAPI_TOKEN = "<your token>")
#' cmp <- compare_smartsolos(make_argissolo_canonical())
#' cmp$agreement
#' }
#' @export
compare_smartsolos <- function(pedon, ...) {
  if (!inherits(pedon, "PedonRecord")) {
    stop("compare_smartsolos(): 'pedon' must be a PedonRecord.")
  }
  local  <- classify_sibcs(pedon, on_missing = "silent")
  remote <- classify_via_smartsolos_api(pedon, ...)

  # Pull local levels from the rich trace structure
  loc_trace <- local$trace
  loc_ordem <- toupper(local$rsg_or_order %||% NA_character_)
  loc_sub   <- toupper(loc_trace$subordem_assigned$name        %||% NA_character_)
  loc_gg    <- toupper(loc_trace$grande_grupo_assigned$name    %||% NA_character_)
  loc_sg    <- toupper(loc_trace$subgrupo_assigned$name        %||% NA_character_)

  rem_ordem <- toupper(remote$rsg_or_order %||% NA_character_)
  rem_sub   <- toupper(remote$qualifiers$subordem  %||% NA_character_)
  rem_gg    <- toupper(remote$qualifiers$gde_grupo %||% NA_character_)
  rem_sg    <- toupper(remote$qualifiers$subgrupo  %||% NA_character_)

  match_at <- function(a, b) {
    if (is.na(a) || is.na(b) || !nzchar(a) || !nzchar(b)) return(NA)
    grepl(b, a, fixed = TRUE) || grepl(a, b, fixed = TRUE)
  }
  agreement <- data.frame(
    point_id  = as.character(pedon$site$id %||% NA_character_),
    ordem     = match_at(loc_ordem, rem_ordem),
    subordem  = match_at(loc_sub,   rem_sub),
    gde_grupo = match_at(loc_gg,    rem_gg),
    subgrupo  = match_at(loc_sg,    rem_sg),
    stringsAsFactors = FALSE
  )
  agreement$n_match <- sum(c(agreement$ordem, agreement$subordem,
                               agreement$gde_grupo, agreement$subgrupo),
                              na.rm = TRUE)
  list(local = local, remote = remote, agreement = agreement)
}
