# =============================================================
# SiBCS 5a edicao -- 5o nivel categorico (Familias) -- Cap 18
# =============================================================
#
# O 5o nivel difere fundamentalmente dos niveis 1-4: ao inves de
# uma chave deterministica "primeiro que passa", cada perfil
# recebe N adjetivos compostos simultaneos, organizados em
# dimensoes ortogonais. Cada ordem do SiBCS usa um subconjunto
# dessas dimensoes.
#
# As dimensoes (Cap 18, pp 281-288):
#
#  Dimensao                    Helper                  Aplicabilidade
#  -------------------------   ---------------------   ----------------
#  Grupamento textural         familia_grupamento_     Todas, exceto RQ
#                              textural
#  Subgrupamento textural      familia_subgrupamento_  Substitui o
#                              textural                grupamento em
#                                                       solos arenosos
#                                                       (E, Lp, RY, RR,
#                                                       RQ, e SGs are/
#                                                       espessar de PA,
#                                                       PV, T, S, F)
#  Distribuicao de cascalhos   familia_distribuicao_   Todas (se cascalho
#                              cascalhos                > 80 g/kg)
#  Constituicao esqueletica    familia_constituicao_   Todas (se >35% e
#                              esqueletica              <90% > 2cm)
#  Tipo de A diagnostico       familia_tipo_horizonte_ Todas, exceto onde
#                              superficial             ja em nivel mais
#                                                       alto
#  Prefixos epi/meso/endo      familia_prefixo_        v0.7.14.B
#                              profundidade
#  Saturacao de bases          familia_saturacao_      v0.7.14.B
#                              bases
#  Saturacao por aluminio      familia_saturacao_      v0.7.14.B
#                              aluminio (alico)
#  Mineralogia (areia)         familia_mineralogia_    v0.7.14.C
#                              areia
#  Mineralogia (argila Lat)    familia_mineralogia_    v0.7.14.C
#                              argila_latossolo
#  Atividade da argila         familia_atividade_      v0.7.14.C
#                              argila
#  Teor de oxidos de ferro     familia_oxidos_ferro    v0.7.14.C
#  Propriedades andicas        familia_andico          v0.7.14.C
#  Material subjacente         familia_material_       v0.7.14.D (Org)
#  (Organossolos)              subjacente
#  Espessura material organico familia_espessura_      v0.7.14.D (Org)
#  > 100 cm                    organica_alta
#  Lenhosidade (Organossolos)  familia_lenhosidade     v0.7.14.D (Org)
#
# =============================================================


# ---- FamilyAttribute R6 class ------------------------------------------

#' Classe S4-like para atributos de Familia (5o nivel SiBCS)
#'
#' Estrutura categorica (em vez de booleana) que representa um
#' adjetivo composto da Familia. \code{value} eh o adjetivo
#' atribuido (string) ou \code{NULL} quando a dimensao nao se
#' aplica ou nao foi possivel determinar.
#'
#' @field name Nome da dimensao (e.g. "grupamento_textural").
#' @field value Adjetivo atribuido (e.g. "argilosa") ou NULL.
#' @field evidence Lista nomeada com valores intermediarios.
#' @field missing Vetor de colunas necessarias mas indisponiveis.
#' @field reference String com referencia bibliografica.
#'
#' @export
FamilyAttribute <- R6::R6Class("FamilyAttribute",
  public = list(
    name = NULL,
    value = NULL,
    evidence = NULL,
    missing = NULL,
    reference = NULL,
    initialize = function(name,
                            value = NULL,
                            evidence = list(),
                            missing = character(0),
                            reference = "") {
      self$name <- name
      self$value <- value
      self$evidence <- evidence
      self$missing <- missing
      self$reference <- reference
    },
    print = function(...) {
      cat("<FamilyAttribute>\n")
      cat("  name:    ", self$name, "\n", sep = "")
      cat("  value:   ",
          if (is.null(self$value)) "<NULL>" else self$value, "\n", sep = "")
      if (length(self$missing) > 0L) {
        cat("  missing: ", paste(self$missing, collapse = ", "),
            "\n", sep = "")
      }
      invisible(self)
    }
  )
)


# ---- Helpers internos -----------------------------------------------------

# Calcula media ponderada (por espessura) das colunas de textura nas
# camadas dentro de uma faixa de profundidade.
.weighted_avg_in_depth <- function(h, col, max_depth_cm = 200,
                                       min_depth_cm = 0) {
  vals <- h[[col]]
  tops <- h$top_cm
  bots <- h$bottom_cm
  ok <- !is.na(vals) & !is.na(tops) & !is.na(bots) &
          tops >= min_depth_cm & tops < max_depth_cm
  if (!any(ok)) return(NA_real_)
  v <- vals[ok]
  t <- pmax(tops[ok], min_depth_cm)
  b <- pmin(bots[ok], max_depth_cm)
  thk <- pmax(b - t, 0)
  if (sum(thk) == 0) return(NA_real_)
  sum(v * thk, na.rm = TRUE) / sum(thk)
}


# ---- Dimensao 1: Grupamento textural (Cap 1, p 46) -------------------------

#' Familia: grupamento textural (Cap 1, p 46)
#'
#' Retorna o grupamento textural do solo na secao de controle.
#' Classes (em g kg-1):
#' \itemize{
#'   \item arenosa: areia + areia franca, i.e. (sand_pct - clay_pct) > 70
#'   \item media: clay < 35 e sand > 15, exceto arenosa
#'   \item argilosa: clay entre 35 e 60
#'   \item muito_argilosa: clay > 60
#'   \item siltosa: clay < 35 e sand < 15
#' }
#'
#' Aplicavel a todas as ordens do SiBCS, exceto Neossolos
#' Quartzarenicos (RQ), nas quais o subgrupamento eh mais
#' apropriado.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_depth_cm Profundidade da secao de controle (default
#'        200 cm).
#' @return \code{\link{FamilyAttribute}}.
#' @references Embrapa (2018), SiBCS 5a ed., Cap 1, p. 46-47;
#'             Cap 18, p. 281.
#' @export
familia_grupamento_textural <- function(pedon, max_depth_cm = 200) {
  h <- pedon$horizons
  clay <- .weighted_avg_in_depth(h, "clay_pct", max_depth_cm = max_depth_cm)
  sand <- .weighted_avg_in_depth(h, "sand_pct", max_depth_cm = max_depth_cm)
  if (is.na(clay) || is.na(sand)) {
    miss <- character(0)
    if (is.na(clay)) miss <- c(miss, "clay_pct")
    if (is.na(sand)) miss <- c(miss, "sand_pct")
    return(FamilyAttribute$new(
      name = "grupamento_textural", value = NULL,
      evidence = list(reason = "textura insuficiente"),
      missing = miss,
      reference = "Embrapa (2018), SiBCS 5a ed., Cap 1, p 46"
    ))
  }
  # Convert to g/kg basis (clay_pct is in %, so threshold 35 == 350 g/kg).
  classe <- if (sand - clay > 70) "arenosa"
            else if (clay > 60) "muito_argilosa"
            else if (clay >= 35 && clay <= 60) "argilosa"
            else if (clay < 35 && sand > 15) "media"
            else if (clay < 35 && sand < 15) "siltosa"
            else NULL  # zona nao classificavel (raro)
  FamilyAttribute$new(
    name = "grupamento_textural", value = classe,
    evidence = list(clay_pct_avg = clay, sand_pct_avg = sand,
                      max_depth_cm = max_depth_cm),
    missing = character(0),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 1, p 46-47"
  )
}


# ---- Dimensao 2: Subgrupamento textural (Cap 18, p 283) -------------------

#' Familia: subgrupamento textural (Cap 18, p 283; em validacao)
#'
#' Subgrupamento textural mais detalhado, aplicavel em
#' substituicao ao grupamento para Espodossolos, Latossolos
#' psamiticos, Neossolos Fluvicos Psamiticos, Neossolos
#' Regoliticos, Neossolos Quartzarenicos, e SGs arenicos /
#' espessarenicos de Argissolos / Luvissolos / Planossolos /
#' Plintossolos. Tambem em solos com textura arenosa e/ou media.
#'
#' Classes (em g kg-1; referidas a media ponderada da secao de controle):
#' \itemize{
#'   \item muito_arenosa: classe textural areia (sand >= 85)
#'   \item arenosa-media: classe textural areia franca (sand >= 70 e
#'     <= 91; clay <= 15)
#'   \item media-arenosa: francoarenosa, sand > 52
#'   \item media-argilosa: franco-argiloarenosa (clay 20-35, sand >=
#'     45)
#'   \item media-siltosa: clay < 35 e sand > 15, excluindo as 4
#'     classes acima
#'   \item siltosa: clay < 35 e sand < 15
#'   \item argilosa: clay 35-60
#'   \item muito_argilosa: clay > 60
#' }
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_depth_cm Profundidade da secao de controle (default
#'        200 cm).
#' @return \code{\link{FamilyAttribute}}.
#' @references Embrapa (2018), SiBCS 5a ed., Cap 18, p. 283.
#' @export
familia_subgrupamento_textural <- function(pedon, max_depth_cm = 200) {
  h <- pedon$horizons
  clay <- .weighted_avg_in_depth(h, "clay_pct", max_depth_cm = max_depth_cm)
  sand <- .weighted_avg_in_depth(h, "sand_pct", max_depth_cm = max_depth_cm)
  silt <- .weighted_avg_in_depth(h, "silt_pct", max_depth_cm = max_depth_cm)
  if (is.na(clay) || is.na(sand)) {
    miss <- character(0)
    if (is.na(clay)) miss <- c(miss, "clay_pct")
    if (is.na(sand)) miss <- c(miss, "sand_pct")
    return(FamilyAttribute$new(
      name = "subgrupamento_textural", value = NULL,
      evidence = list(reason = "textura insuficiente"),
      missing = miss,
      reference = "Embrapa (2018), SiBCS 5a ed., Cap 18, p 283"
    ))
  }
  classe <- if (clay > 60) "muito_argilosa"
            else if (clay >= 35 && clay <= 60) "argilosa"
            else if (sand >= 85 && clay <= 10) "muito_arenosa"
            else if (sand >= 70 && sand < 91 && clay <= 15) "arenosa-media"
            else if (clay < 20 && sand > 52) "media-arenosa"
            else if (clay >= 20 && clay <= 35 && sand >= 45) "media-argilosa"
            else if (clay < 35 && sand < 15) "siltosa"
            else if (clay < 35 && sand >= 15) "media-siltosa"
            else NULL
  FamilyAttribute$new(
    name = "subgrupamento_textural", value = classe,
    evidence = list(clay_pct_avg = clay, sand_pct_avg = sand,
                      silt_pct_avg = silt, max_depth_cm = max_depth_cm),
    missing = character(0),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 18, p 283"
  )
}


# ---- Dimensao 3: Distribuicao de cascalhos (Cap 1, p 47-48) ---------------

#' Familia: distribuicao de cascalhos no perfil (Cap 1, p 47-48)
#'
#' Utiliza coarse_fragments_pct (% volume de cascalhos 2 mm a 2 cm
#' relativo a terra fina) como modificador do grupamento textural.
#'
#' Classes (Santos et al., 2015; valores em g kg-1):
#' \itemize{
#'   \item pouco_cascalhenta: 8% <= cascalho < 15%
#'   \item cascalhenta: 15% <= cascalho <= 50%
#'   \item muito_cascalhenta: cascalho > 50%
#' }
#'
#' Aplica-se a TODAS as classes que apresentam cascalho > 80 g/kg
#' (8% do volume) na secao de controle.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_depth_cm Profundidade da secao de controle (default 200).
#' @return \code{\link{FamilyAttribute}}.
#' @references Embrapa (2018), SiBCS 5a ed., Cap 1, p 47-48; Cap 18,
#'             p 284.
#' @export
familia_distribuicao_cascalhos <- function(pedon, max_depth_cm = 200) {
  h <- pedon$horizons
  cf <- .weighted_avg_in_depth(h, "coarse_fragments_pct",
                                  max_depth_cm = max_depth_cm)
  if (is.na(cf)) {
    return(FamilyAttribute$new(
      name = "distribuicao_cascalhos", value = NULL,
      evidence = list(reason = "coarse_fragments_pct nao informado"),
      missing = "coarse_fragments_pct",
      reference = "Embrapa (2018), SiBCS 5a ed., Cap 1, p 47-48"
    ))
  }
  classe <- if (cf < 8) NULL  # nao se aplica
            else if (cf < 15) "pouco_cascalhenta"
            else if (cf <= 50) "cascalhenta"
            else "muito_cascalhenta"
  FamilyAttribute$new(
    name = "distribuicao_cascalhos", value = classe,
    evidence = list(coarse_fragments_pct_avg = cf,
                      max_depth_cm = max_depth_cm),
    missing = character(0),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 1, p 47-48"
  )
}


# ---- Dimensao 4: Constituicao esqueletica (Cap 1, p 48) -------------------

#' Familia: constituicao esqueletica (Cap 1, p 48)
#'
#' Solo com mais de 35\% e menos de 90\% do volume constituido
#' por material mineral com diametro > 2 cm. Acima de 90\%, eh
#' considerado tipo de terreno (nao classificavel).
#'
#' O schema atual nao distingue cascalho (2 mm-2 cm) de calhaus
#' (> 2 cm). Como aproximacao conservadora, esta funcao retorna
#' "esqueletica" quando \code{coarse_fragments_pct} esta no
#' intervalo (35\%, 90\%). Refinamento futuro requer adicionar
#' uma coluna distinta para fragmentos > 2 cm.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_depth_cm Profundidade da secao de controle (default 200).
#' @return \code{\link{FamilyAttribute}}.
#' @references Embrapa (2018), SiBCS 5a ed., Cap 1, p 48; Cap 18,
#'             p 284.
#' @export
familia_constituicao_esqueletica <- function(pedon, max_depth_cm = 200) {
  h <- pedon$horizons
  cf <- .weighted_avg_in_depth(h, "coarse_fragments_pct",
                                  max_depth_cm = max_depth_cm)
  if (is.na(cf)) {
    return(FamilyAttribute$new(
      name = "constituicao_esqueletica", value = NULL,
      evidence = list(reason = "coarse_fragments_pct nao informado"),
      missing = "coarse_fragments_pct",
      reference = "Embrapa (2018), SiBCS 5a ed., Cap 1, p 48"
    ))
  }
  classe <- if (cf > 35 && cf < 90) "esqueletica" else NULL
  FamilyAttribute$new(
    name = "constituicao_esqueletica", value = classe,
    evidence = list(coarse_fragments_pct_avg = cf,
                      max_depth_cm = max_depth_cm,
                      note = "v0.7.14.A: aproximado via coarse_fragments_pct;",
                      note2 = "fragmentos > 2 cm nao distinguidos no schema"),
    missing = character(0),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 1, p 48"
  )
}


# ---- Dimensao 5: Tipo de horizonte diagnostico superficial (Cap 2) --------

#' Familia: tipo de horizonte diagnostico superficial (Cap 2)
#'
#' Retorna o tipo do horizonte A (ou H/O) presente, em ordem de
#' precedencia: \code{histico} > \code{chernozemico} >
#' \code{humico} > \code{proeminente} > \code{moderado} >
#' \code{fraco}. Se nenhum diagnostico passa, retorna NULL.
#'
#' Aplica-se a TODAS as classes de solo, exceto para aquelas que
#' ja consideram o tipo de A em nivel categorico mais alto (e.g.
#' Chernossolos, Organossolos, Neossolos Litolicos Humicos /
#' Histicos).
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @return \code{\link{FamilyAttribute}}.
#' @references Embrapa (2018), SiBCS 5a ed., Cap 2 (p 49-54);
#'             Cap 18, p 284.
#' @export
familia_tipo_horizonte_superficial <- function(pedon) {
  hi <- horizonte_histico(pedon)
  ch <- horizonte_A_chernozemico(pedon)
  hu <- horizonte_A_humico(pedon)
  pr <- horizonte_A_proeminente(pedon)
  mo <- horizonte_A_moderado(pedon)
  fr <- horizonte_A_fraco(pedon)
  classe <- if (isTRUE(hi$passed)) "histico"
            else if (isTRUE(ch$passed)) "chernozemico"
            else if (isTRUE(hu$passed)) "humico"
            else if (isTRUE(pr$passed)) "proeminente"
            else if (isTRUE(mo$passed)) "moderado"
            else if (isTRUE(fr$passed)) "fraco"
            else NULL
  miss <- unique(c(hi$missing %||% character(0),
                    ch$missing %||% character(0),
                    hu$missing %||% character(0),
                    pr$missing %||% character(0),
                    mo$missing %||% character(0),
                    fr$missing %||% character(0)))
  FamilyAttribute$new(
    name = "tipo_horizonte_superficial", value = classe,
    evidence = list(histico = isTRUE(hi$passed),
                      chernozemico = isTRUE(ch$passed),
                      humico = isTRUE(hu$passed),
                      proeminente = isTRUE(pr$passed),
                      moderado = isTRUE(mo$passed),
                      fraco = isTRUE(fr$passed)),
    missing = miss,
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 2"
  )
}


# ---- Motor classify_sibcs_familia -----------------------------------------

# Mapa estatico: ordem (1a letra do codigo SG) -> dimensoes aplicaveis
# nessa ordem. v0.7.14.A inclui apenas as dimensoes ja implementadas;
# mais dimensoes serao adicionadas ao registry conforme implementadas.
#
# Convencoes:
#  - "use_subgrupamento": ordem usa subgrupamento textural em vez do
#                         grupamento (E, RQ, RR; outras ordens podem
#                         tambem usar para SGs arenico/espessarenico,
#                         tratado depois com sg_code).
#  - "skip_tipo_A":       ordem ja usa o tipo de A em nivel mais alto
#                         (M chernozemico, O = histico).
.familia_dimensoes_por_ordem <- function() {
  # Convencoes:
  #  - use_subgrupamento: ordem usa subgrupamento textural em vez do
  #      grupamento (E, RQ, RR; outras ordens podem tambem usar para
  #      SGs arenico/espessarenico, tratado depois com sg_code).
  #  - skip_tipo_A: ordem ja usa o tipo de A em nivel mais alto
  #      (M chernozemico, O = histico).
  #  - skip_sat_bases: ordem ja usa V em nivel mais alto (e.g.
  #      ordens com classes Distrofico/Eutrofico nos GGs: L, C, P,
  #      N, R [exceto RQ], T, S [parcial], F, V).
  #  - skip_alico: ordem ja usa carater alitico em nivel mais alto
  #      (e.g. P*al, L*al, N*al, T*al, S*al, F*al).
  list(
    "P" = list(use_subgrupamento = FALSE, skip_tipo_A = FALSE,
                  skip_sat_bases = TRUE, skip_alico = TRUE),
    "L" = list(use_subgrupamento = FALSE, skip_tipo_A = FALSE,
                  skip_sat_bases = TRUE, skip_alico = TRUE),
    "C" = list(use_subgrupamento = FALSE, skip_tipo_A = FALSE,
                  skip_sat_bases = TRUE, skip_alico = TRUE),
    "M" = list(use_subgrupamento = FALSE, skip_tipo_A = TRUE,
                  skip_sat_bases = TRUE, skip_alico = FALSE),
    "E" = list(use_subgrupamento = TRUE,  skip_tipo_A = FALSE,
                  skip_sat_bases = FALSE, skip_alico = FALSE),
    "G" = list(use_subgrupamento = FALSE, skip_tipo_A = FALSE,
                  skip_sat_bases = FALSE, skip_alico = FALSE),
    "N" = list(use_subgrupamento = FALSE, skip_tipo_A = FALSE,
                  skip_sat_bases = TRUE, skip_alico = TRUE),
    "R" = list(use_subgrupamento = FALSE, skip_tipo_A = FALSE,
                  skip_sat_bases = TRUE, skip_alico = FALSE),
    "T" = list(use_subgrupamento = FALSE, skip_tipo_A = FALSE,
                  skip_sat_bases = TRUE, skip_alico = TRUE),
    "S" = list(use_subgrupamento = FALSE, skip_tipo_A = FALSE,
                  skip_sat_bases = TRUE, skip_alico = TRUE),
    "F" = list(use_subgrupamento = FALSE, skip_tipo_A = FALSE,
                  skip_sat_bases = TRUE, skip_alico = TRUE),
    "V" = list(use_subgrupamento = FALSE, skip_tipo_A = FALSE,
                  skip_sat_bases = FALSE, skip_alico = FALSE),
    "O" = list(use_subgrupamento = FALSE, skip_tipo_A = TRUE,
                  skip_sat_bases = TRUE, skip_alico = FALSE)
  )
}


#' Classifica um perfil no 5o nivel categorico do SiBCS (Familia)
#'
#' Aplica as dimensoes pertinentes a ordem do solo e devolve uma
#' lista nomeada de \code{\link{FamilyAttribute}}. O label
#' textual da Familia eh formado adicionando-se cada \code{value}
#' nao-nulo apos a designacao do 4o nivel, separados por
#' virgulas (Cap 18, p 281).
#'
#' Esta funcao NAO eh uma chave determinista: cada perfil recebe
#' SIMULTANEAMENTE todos os adjetivos pertinentes (multi-rotulo).
#'
#' @section Status v0.7.14.A:
#' Implementadas 5 dimensoes -- grupamento textural, subgrupamento
#' textural, distribuicao de cascalhos, constituicao esqueletica,
#' tipo de horizonte superficial. Outras dimensoes (prefixos epi/
#' meso/endo, saturacao de bases, alico, mineralogia, atividade da
#' argila, oxidos de ferro, andico, especificos de Organossolos)
#' adicionadas em sub-commits subsequentes.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param ordem_code Codigo da ordem (1 letra: "P", "L", ...). Se
#'        \code{NULL}, sera derivado de \code{sg_code}.
#' @param sg_code Codigo do subgrupo do 4o nivel (e.g. "PVdAr").
#'        Opcional; usado para ajustes especificos por SG (e.g.
#'        forcar subgrupamento textural em arenicos/espessarenicos).
#' @param max_depth_cm Profundidade da secao de controle (default
#'        200 cm).
#' @return Lista nomeada de \code{\link{FamilyAttribute}}.
#' @references Embrapa (2018), SiBCS 5a ed., Cap 18, pp 281-288.
#' @export
classify_sibcs_familia <- function(pedon,
                                       ordem_code   = NULL,
                                       sg_code      = NULL,
                                       max_depth_cm = 200) {
  if (is.null(ordem_code) && !is.null(sg_code)) {
    ordem_code <- substr(sg_code, 1L, 1L)
  }
  if (is.null(ordem_code)) {
    rlang::abort("ordem_code ou sg_code deve ser fornecido")
  }
  cfg_map <- .familia_dimensoes_por_ordem()
  cfg <- cfg_map[[ordem_code]] %||% list(use_subgrupamento = FALSE,
                                            skip_tipo_A = FALSE,
                                            skip_sat_bases = FALSE,
                                            skip_alico = FALSE)

  # Override: SGs arenicos / espessarenicos sempre usam subgrupamento
  if (!is.null(sg_code) && grepl("(Ar|Ea)$", sg_code)) {
    cfg$use_subgrupamento <- TRUE
  }

  out <- list()

  # Dimensao 1 / 2: textura -- mutuamente exclusivas
  if (isTRUE(cfg$use_subgrupamento)) {
    out$subgrupamento_textural <-
      familia_subgrupamento_textural(pedon, max_depth_cm = max_depth_cm)
  } else {
    out$grupamento_textural <-
      familia_grupamento_textural(pedon, max_depth_cm = max_depth_cm)
  }

  # Dimensao 3: cascalhos
  out$distribuicao_cascalhos <-
    familia_distribuicao_cascalhos(pedon, max_depth_cm = max_depth_cm)

  # Dimensao 4: esqueletica
  out$constituicao_esqueletica <-
    familia_constituicao_esqueletica(pedon, max_depth_cm = max_depth_cm)

  # Dimensao 5: tipo de A diagnostico
  if (!isTRUE(cfg$skip_tipo_A)) {
    out$tipo_horizonte_superficial <-
      familia_tipo_horizonte_superficial(pedon)
  }

  # Dimensao 6 (v0.7.14.B): saturacao por bases (V)
  if (!isTRUE(cfg$skip_sat_bases)) {
    out$saturacao_bases <- familia_saturacao_bases(pedon)
  }

  # Dimensao 7 (v0.7.14.B): saturacao por aluminio (alico)
  if (!isTRUE(cfg$skip_alico)) {
    out$saturacao_aluminio <- familia_saturacao_aluminio(pedon)
  }

  out
}


# ---- v0.7.14.B: prefixos epi/meso/endo + saturacoes ----------------------

# Helper interno: recebe vetor de top_cm onde um atributo ocorre,
# devolve "epi" / "meso" / "endo" / NULL (atributo nao ocorre).
.classifica_prefixo_profundidade <- function(tops_cm) {
  tops_cm <- tops_cm[!is.na(tops_cm)]
  if (length(tops_cm) == 0L) return(NULL)
  topo_min <- min(tops_cm)
  if (topo_min < 50)          "epi"
  else if (topo_min < 100)    "meso"
  else                          "endo"
}


#' Familia: prefixo de profundidade epi-/meso-/endo- (Cap 18, p 284-285)
#'
#' Classifica a profundidade onde um diagnostico ocorre em
#' um dos tres prefixos:
#' \itemize{
#'   \item \code{epi-}: topo da primeira camada que satisfaz < 50 cm
#'   \item \code{meso-}: topo da primeira camada em [50, 100) cm
#'   \item \code{endo-}: topo da primeira camada em >= 100 cm
#' }
#'
#' Wrapper generico para ser usado com qualquer
#' \code{\link{DiagnosticResult}}. Retorna NULL se o diagnostico
#' nao passou ou se nao ha camadas identificadas.
#'
#' @param diag Um \code{\link{DiagnosticResult}} com \code{layers}
#'        (indices de horizontes que satisfazem o atributo).
#' @param horizons \code{data.table} de horizontes do pedon.
#' @return String "epi" / "meso" / "endo" ou NULL.
#' @references Embrapa (2018), SiBCS 5a ed., Cap 18, p 284-285.
#' @export
familia_prefixo_profundidade <- function(diag, horizons) {
  if (!isTRUE(diag$passed)) return(NULL)
  layers <- diag$layers %||% integer(0)
  if (length(layers) == 0L) return(NULL)
  tops <- horizons$top_cm[layers]
  .classifica_prefixo_profundidade(tops)
}


#' Familia: saturacao por bases (Cap 18, p 285)
#'
#' Retorna \code{"eutrofico"} (V >= 50\%) ou \code{"distrofico"}
#' (V < 50\%) baseado na media ponderada de \code{bs_pct} na
#' secao de controle. Pode ser combinado com prefixos
#' epi-/meso-/endo- via \code{familia_prefixo_profundidade}.
#'
#' Aplicavel a todas as classes que ainda nao consideram saturacao
#' por bases em nivel categorico mais alto (p.ex. Latossolos
#' Eutroficos / Distroficos ja a consideram).
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_depth_cm Profundidade da secao de controle (default
#'        150 cm; p. 31 do SiBCS define a secao de controle dos
#'        Argissolos / Latossolos como 0-150 cm de B).
#' @param threshold Limiar de eutrofico (default 50\%).
#' @return \code{\link{FamilyAttribute}}.
#' @references Embrapa (2018), SiBCS 5a ed., Cap 1, p 31; Cap 18,
#'             p 285.
#' @export
familia_saturacao_bases <- function(pedon, max_depth_cm = 150,
                                       threshold = 50) {
  h <- pedon$horizons
  v <- .weighted_avg_in_depth(h, "bs_pct", max_depth_cm = max_depth_cm)
  if (is.na(v)) {
    return(FamilyAttribute$new(
      name = "saturacao_bases", value = NULL,
      evidence = list(reason = "bs_pct nao informado"),
      missing = "bs_pct",
      reference = "Embrapa (2018), SiBCS 5a ed., Cap 1, p 31"
    ))
  }
  classe <- if (v >= threshold) "eutrofico" else "distrofico"
  FamilyAttribute$new(
    name = "saturacao_bases", value = classe,
    evidence = list(bs_pct_avg = v, threshold = threshold,
                      max_depth_cm = max_depth_cm),
    missing = character(0),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 1, p 31; Cap 18, p 285"
  )
}


#' Familia: saturacao por aluminio -- "alico" (Cap 18, p 285)
#'
#' Aplica o termo "alico" quando, em qualquer camada do horizonte
#' B (ou C, na ausencia de B):
#' \itemize{
#'   \item al_sat_pct >= 50\% (saturacao por Al = 100*Al/(S+Al)),
#'   \item E al_cmol > 0.5 cmolc/kg.
#' }
#' Quando aplicavel, o prefixo de profundidade (epi-/meso-/endo-)
#' eh determinado pelo topo da primeira camada que satisfaz, e
#' concatenado ao adjetivo: "epialico", "mesoalico", "endoalico".
#'
#' Aplicavel a classes cujo carater alumınico nao tenha sido
#' considerado em nivel categorico mais alto (p.ex. Argissolos
#' Aluminicos ja o usam).
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_al_sat Default 50.
#' @param min_al_cmol Default 0.5.
#' @return \code{\link{FamilyAttribute}} com \code{value} igual a
#'         \code{"epialico"} / \code{"mesoalico"} / \code{"endoalico"}
#'         ou NULL.
#' @references Embrapa (2018), SiBCS 5a ed., Cap 18, p 285.
#' @export
familia_saturacao_aluminio <- function(pedon,
                                          min_al_sat = 50,
                                          min_al_cmol = 0.5) {
  h <- pedon$horizons
  layers_b <- which(!is.na(h$designation) &
                       grepl("^B|^C", h$designation))
  if (length(layers_b) == 0L) {
    return(FamilyAttribute$new(
      name = "saturacao_aluminio", value = NULL,
      evidence = list(reason = "no B/C horizons"),
      missing = "designation",
      reference = "Embrapa (2018), SiBCS 5a ed., Cap 18, p 285"
    ))
  }
  als <- h$al_sat_pct[layers_b]
  alc <- h$al_cmol[layers_b]
  miss <- character(0)
  if (all(is.na(als))) miss <- c(miss, "al_sat_pct")
  if (all(is.na(alc))) miss <- c(miss, "al_cmol")
  if (length(miss) > 0L) {
    return(FamilyAttribute$new(
      name = "saturacao_aluminio", value = NULL,
      evidence = list(reason = "Al insuficiente"),
      missing = miss,
      reference = "Embrapa (2018), SiBCS 5a ed., Cap 18, p 285"
    ))
  }
  passes <- !is.na(als) & !is.na(alc) &
              als >= min_al_sat & alc > min_al_cmol
  if (!any(passes)) {
    return(FamilyAttribute$new(
      name = "saturacao_aluminio", value = NULL,
      evidence = list(reason = "nenhuma camada B/C atinge alico"),
      missing = character(0),
      reference = "Embrapa (2018), SiBCS 5a ed., Cap 18, p 285"
    ))
  }
  passing_layers <- layers_b[passes]
  prefixo <- .classifica_prefixo_profundidade(h$top_cm[passing_layers])
  classe <- if (is.null(prefixo)) "alico" else paste0(prefixo, "alico")
  FamilyAttribute$new(
    name = "saturacao_aluminio", value = classe,
    evidence = list(passing_layers = passing_layers,
                      al_sat_pct = als[passes],
                      al_cmol    = alc[passes],
                      prefixo    = prefixo),
    missing = character(0),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 18, p 285"
  )
}


#' Constroi label textual de Familia a partir de \code{classify_sibcs_familia}
#'
#' Concatena os \code{value} nao-nulos como string separada por
#' virgulas, conforme orientado no Cap 18, p 281: "as caracteristicas
#' utilizadas para identificacao do 5o nivel categorico devem ser
#' acrescentadas apos a designacao do 4o nivel categorico e separadas
#' desta e entre si por virgula".
#'
#' @param familia Lista de \code{\link{FamilyAttribute}}, retorno de
#'        \code{\link{classify_sibcs_familia}}.
#' @return String com adjetivos compostos separados por ", ", ou
#'         vazia se nenhum adjetivo se aplica.
#' @export
familia_label <- function(familia) {
  vals <- vapply(familia, function(fa) fa$value %||% NA_character_,
                   character(1))
  vals <- vals[!is.na(vals)]
  paste(vals, collapse = ", ")
}
