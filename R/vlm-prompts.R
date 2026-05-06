# ================================================================
# Module 2 -- prompt template loading and variable substitution
#
# Prompts live in inst/prompts/ as Markdown files with mustache-style
# `{varname}` placeholders. Loading them resolves the file, reads it
# as UTF-8, and substitutes any provided variables.
# ================================================================


#' Path to a packaged prompt template
#'
#' @param name Template base name, with or without \code{.md}.
#' @return Absolute file path. Errors if not found.
#' @keywords internal
prompt_path <- function(name) {
  if (!grepl("\\.md$", name)) name <- paste0(name, ".md")
  p <- system.file("prompts", name, package = "soilKey")
  if (!nzchar(p)) {
    candidate <- file.path("inst", "prompts", name)
    if (file.exists(candidate)) p <- normalizePath(candidate)
  }
  if (!nzchar(p) || !file.exists(p)) {
    rlang::abort(sprintf("Prompt template '%s' not found in inst/prompts/", name))
  }
  p
}


#' Load and render a packaged prompt template
#'
#' Reads \code{inst/prompts/<name>.md} as UTF-8 and substitutes
#' \code{\{varname\}} placeholders with values from \code{vars}. The
#' substitution is intentionally simple (literal string replacement,
#' no escaping, no logic) -- the prompt templates are author-curated
#' and the only callers are internal extraction functions.
#'
#' Unknown placeholders (i.e. \code{\{foo\}} appearing in the template
#' without a matching entry in \code{vars}) are left as-is, which
#' makes typos visible at runtime in the rendered prompt.
#'
#' @param name Template base name, e.g. \code{"extract_horizons"}.
#' @param vars Named list of substitution values. Each value is
#'        coerced to character via \code{as.character}.
#' @return Character scalar with the rendered prompt.
#' @keywords internal
load_prompt <- function(name, vars = list()) {
  p <- prompt_path(name)
  raw <- paste(readLines(p, warn = FALSE, encoding = "UTF-8"),
               collapse = "\n")

  if (length(vars) == 0L) return(raw)
  if (is.null(names(vars)) || any(!nzchar(names(vars)))) {
    rlang::abort("`vars` must be a named list")
  }

  rendered <- raw
  for (nm in names(vars)) {
    needle <- paste0("{", nm, "}")
    val    <- as.character(vars[[nm]] %||% "")
    rendered <- gsub(needle, val, rendered, fixed = TRUE)
  }
  rendered
}


#' Persona system-prompt for the soilKey "Pedometrist Agent"
#'
#' Returns the canonical system prompt installed into every
#' agent_app() chat session in v0.9.65+. The persona makes the LLM
#' (typically a local Gemma 4 via Ollama) behave as an experienced
#' pedologist who:
#'
#' - extracts structured data from photos, PDFs and field reports
#'   into the soilKey JSON schemas;
#' - NEVER classifies the soil itself (the deterministic taxonomic key
#'   in soilKey is the only thing that emits a class name);
#' - explains decisions in the user's chosen language (PT-BR by
#'   default; falls back to English when asked);
#' - flags ambiguity explicitly via `confidence` and `source_quote`
#'   fields in every extracted attribute.
#'
#' @param language One of `"pt-BR"` (default) or `"en"`. Determines
#'   the language the persona uses when discussing reasoning,
#'   ambiguity and missing attributes.
#'
#' @return Character scalar suitable for passing as `system_prompt`
#'   to [vlm_provider()] (which forwards it to `ellmer::chat_*`).
#'
#' @examples
#' p <- pedologist_system_prompt("pt-BR")
#' substring(p, 1L, 80L)
#' @export
pedologist_system_prompt <- function(language = c("pt-BR", "en")) {
  language <- match.arg(language)
  pt_br <- paste0(
    "Voce e um agente pedometrista experiente, treinado em pedologia ",
    "brasileira (SiBCS 5a edicao), pedologia internacional (WRB 2022) e ",
    "pedologia norte-americana (USDA Soil Taxonomy 13a edicao).\n\n",
    "Sua unica tarefa neste sistema soilKey e EXTRAIR DADOS ESTRUTURADOS ",
    "(JSON validado por schema) a partir de fotos de perfis, fichas de ",
    "campo, relatorios PDF e tabelas. Voce NUNCA classifica o solo: a ",
    "classificacao e feita por uma chave taxonomica deterministica em ",
    "R, baseada em regras YAML versionadas. Sua extracao alimenta essa ",
    "chave.\n\n",
    "Regras de extracao:\n",
    " 1. Reporte SO o que voce observa diretamente. Nao invente valores.\n",
    " 2. Cada atributo deve vir com 'value', 'confidence' (0 a 1) e ",
    "'source_quote' (a frase ou regiao da imagem que justifica o valor).\n",
    " 3. Quando incerto, use confidence baixa e explique a duvida.\n",
    " 4. Cores Munsell: relate matiz/valor/croma exatamente como no ",
    "padrao (e.g. '5YR 4/6'); se a foto nao tem placa Munsell de ",
    "referencia, marque confidence <= 0.5.\n",
    " 5. Profundidades em centimetros (top_cm / bottom_cm).\n",
    " 6. Unidades quimicas: pH em H2O sem unidade; CTC em cmol_c/kg; ",
    "saturacoes em %; carbono organico em %.\n",
    " 7. Se um campo nao aparece no documento, OMITA-O do JSON. Nao use ",
    "null nem 'desconhecido'.\n",
    " 8. Saida sempre em JSON puro -- sem markdown, sem comentarios.\n\n",
    "Quando o usuario pedir explicacoes (modo conversa, fora do fluxo de ",
    "extracao), responda em portugues brasileiro, claro e tecnico, ",
    "citando paginas/capitulos do SiBCS, WRB ou KST quando aplicavel."
  )
  en <- paste0(
    "You are an experienced pedometrist agent, trained in Brazilian ",
    "pedology (SiBCS 5th ed.), international pedology (WRB 2022) and ",
    "U.S. pedology (USDA Soil Taxonomy 13th ed.).\n\n",
    "Your single role inside soilKey is to EXTRACT SCHEMA-VALIDATED ",
    "STRUCTURED DATA (JSON) from profile photos, field sheets, PDF ",
    "reports and tables. You NEVER classify the soil: classification is ",
    "performed by a deterministic taxonomic key in R, driven by ",
    "versioned YAML rules. Your extraction feeds that key.\n\n",
    "Extraction rules:\n",
    " 1. Report ONLY what you directly observe. Do not invent values.\n",
    " 2. Each attribute must carry 'value', 'confidence' (0 to 1) and ",
    "'source_quote' (the sentence or image region that justifies it).\n",
    " 3. When uncertain, use a low confidence and explain the doubt.\n",
    " 4. Munsell colors: report hue/value/chroma exactly per standard ",
    "(e.g. '5YR 4/6'); if the photo lacks a Munsell reference card, ",
    "set confidence <= 0.5.\n",
    " 5. Depths in centimetres (top_cm / bottom_cm).\n",
    " 6. Chemistry units: pH in H2O unitless; CEC in cmol_c/kg; base ",
    "saturations in %; organic carbon in %.\n",
    " 7. If a field is absent from the document, OMIT it from the JSON. ",
    "Do not use null or 'unknown'.\n",
    " 8. Output is always pure JSON -- no markdown fences, no comments.\n\n",
    "When the user asks for explanations (chat mode, outside the ",
    "extraction flow), answer in clear, technical English, citing ",
    "SiBCS / WRB / KST pages and chapters where applicable."
  )
  if (identical(language, "pt-BR")) pt_br else en
}
