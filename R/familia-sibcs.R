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
  #  - skip_alico: ordem ja usa carater alitico em nivel mais alto.
  #  - mineralogia_areia: ordem se beneficia da mineralogia da areia.
  #      Aplicavel a Cambissolos, Chernossolos, Gleissolos, Luvissolos,
  #      Neossolos (excepto Quartzarenicos), Nitossolos, Planossolos,
  #      Plintossolos, Vertissolos (Cap 18, p 286).
  #  - mineralogia_argila_lat: somente Latossolos (e opcionalmente
  #      Argissolos, Cambissolos, Plintossolos com info da argila).
  #  - skip_atividade_argila: ordem ja usa atividade da argila em
  #      nivel mais alto (Latossolos = Tb por definicao; Chernossolos /
  #      Luvissolos / Vertissolos = Ta por definicao).
  #  - skip_oxidos_ferro: ordem ja usa Fe2O3 em nivel mais alto
  #      (Latossolos *f / Argissolos *f / Cambissolos *f / Nitossolos
  #      *f / Plintossolos *f / Vertissolos sem Fe).
  #  - andico: aplicavel para Cambissolos Histicos e Organossolos
  #      Folicos (sera filtrado por sg_code no motor).
  list(
    "P" = list(use_subgrupamento = FALSE, skip_tipo_A = FALSE,
                  skip_sat_bases = TRUE, skip_alico = TRUE,
                  mineralogia_areia = FALSE,
                  mineralogia_argila_lat = FALSE,
                  skip_atividade_argila = FALSE,
                  skip_oxidos_ferro = FALSE,
                  andico = FALSE),
    "L" = list(use_subgrupamento = FALSE, skip_tipo_A = FALSE,
                  skip_sat_bases = TRUE, skip_alico = TRUE,
                  mineralogia_areia = FALSE,
                  mineralogia_argila_lat = TRUE,
                  skip_atividade_argila = TRUE,
                  skip_oxidos_ferro = FALSE,
                  andico = FALSE),
    "C" = list(use_subgrupamento = FALSE, skip_tipo_A = FALSE,
                  skip_sat_bases = TRUE, skip_alico = TRUE,
                  mineralogia_areia = TRUE,
                  mineralogia_argila_lat = FALSE,
                  skip_atividade_argila = FALSE,
                  skip_oxidos_ferro = FALSE,
                  andico = TRUE),  # Cambissolos Histicos
    "M" = list(use_subgrupamento = FALSE, skip_tipo_A = TRUE,
                  skip_sat_bases = TRUE, skip_alico = FALSE,
                  mineralogia_areia = TRUE,
                  mineralogia_argila_lat = FALSE,
                  skip_atividade_argila = TRUE,
                  skip_oxidos_ferro = FALSE,
                  andico = FALSE),
    "E" = list(use_subgrupamento = TRUE,  skip_tipo_A = FALSE,
                  skip_sat_bases = FALSE, skip_alico = FALSE,
                  mineralogia_areia = FALSE,
                  mineralogia_argila_lat = FALSE,
                  skip_atividade_argila = FALSE,
                  skip_oxidos_ferro = TRUE,
                  andico = FALSE),
    "G" = list(use_subgrupamento = FALSE, skip_tipo_A = FALSE,
                  skip_sat_bases = FALSE, skip_alico = FALSE,
                  mineralogia_areia = TRUE,
                  mineralogia_argila_lat = FALSE,
                  skip_atividade_argila = FALSE,
                  skip_oxidos_ferro = TRUE,
                  andico = FALSE),
    "N" = list(use_subgrupamento = FALSE, skip_tipo_A = FALSE,
                  skip_sat_bases = TRUE, skip_alico = TRUE,
                  mineralogia_areia = TRUE,
                  mineralogia_argila_lat = FALSE,
                  skip_atividade_argila = FALSE,
                  skip_oxidos_ferro = FALSE,
                  andico = FALSE),
    "R" = list(use_subgrupamento = FALSE, skip_tipo_A = FALSE,
                  skip_sat_bases = TRUE, skip_alico = FALSE,
                  mineralogia_areia = TRUE,
                  mineralogia_argila_lat = FALSE,
                  skip_atividade_argila = FALSE,
                  skip_oxidos_ferro = FALSE,
                  andico = FALSE),
    "T" = list(use_subgrupamento = FALSE, skip_tipo_A = FALSE,
                  skip_sat_bases = TRUE, skip_alico = TRUE,
                  mineralogia_areia = TRUE,
                  mineralogia_argila_lat = FALSE,
                  skip_atividade_argila = TRUE,
                  skip_oxidos_ferro = TRUE,
                  andico = FALSE),
    "S" = list(use_subgrupamento = FALSE, skip_tipo_A = FALSE,
                  skip_sat_bases = TRUE, skip_alico = TRUE,
                  mineralogia_areia = TRUE,
                  mineralogia_argila_lat = FALSE,
                  skip_atividade_argila = FALSE,
                  skip_oxidos_ferro = TRUE,
                  andico = FALSE),
    "F" = list(use_subgrupamento = FALSE, skip_tipo_A = FALSE,
                  skip_sat_bases = TRUE, skip_alico = TRUE,
                  mineralogia_areia = TRUE,
                  mineralogia_argila_lat = FALSE,
                  skip_atividade_argila = FALSE,
                  skip_oxidos_ferro = FALSE,
                  andico = FALSE),
    "V" = list(use_subgrupamento = FALSE, skip_tipo_A = FALSE,
                  skip_sat_bases = FALSE, skip_alico = FALSE,
                  mineralogia_areia = TRUE,
                  mineralogia_argila_lat = FALSE,
                  skip_atividade_argila = TRUE,
                  skip_oxidos_ferro = TRUE,
                  andico = FALSE),
    "O" = list(use_subgrupamento = FALSE, skip_tipo_A = TRUE,
                  skip_sat_bases = TRUE, skip_alico = FALSE,
                  mineralogia_areia = FALSE,
                  mineralogia_argila_lat = FALSE,
                  skip_atividade_argila = TRUE,
                  skip_oxidos_ferro = TRUE,
                  andico = TRUE)  # Organossolos Folicos
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
                                            skip_alico = FALSE,
                                            mineralogia_areia = FALSE,
                                            mineralogia_argila_lat = FALSE,
                                            skip_atividade_argila = FALSE,
                                            skip_oxidos_ferro = FALSE,
                                            andico = FALSE)

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

  # Dimensao 8 (v0.7.14.C): mineralogia da fracao areia
  if (isTRUE(cfg$mineralogia_areia)) {
    out$mineralogia_areia <- familia_mineralogia_areia(
      pedon, max_depth_cm = max_depth_cm
    )
  }

  # Dimensao 9 (v0.7.14.C): mineralogia da fracao argila (Latossolos)
  if (isTRUE(cfg$mineralogia_argila_lat)) {
    out$mineralogia_argila <- familia_mineralogia_argila_latossolo(
      pedon, max_depth_cm = max_depth_cm
    )
  }

  # Dimensao 10 (v0.7.14.C): atividade da argila
  if (!isTRUE(cfg$skip_atividade_argila)) {
    out$atividade_argila <- familia_atividade_argila(pedon)
  }

  # Dimensao 11 (v0.7.14.C): teor de oxidos de ferro
  if (!isTRUE(cfg$skip_oxidos_ferro)) {
    out$oxidos_ferro <- familia_oxidos_ferro(pedon)
  }

  # Dimensao 12 (v0.7.14.C): propriedades andicas
  # Sempre testavel quando ordem permite, MAS so retornara value
  # nao-nulo para subgrupos histicos (Cambissolos / Organossolos Folicos)
  if (isTRUE(cfg$andico)) {
    out$andico <- familia_andico(pedon)
  }

  # Dimensoes 13-15 (v0.7.14.D): especificas de Organossolos
  if (ordem_code == "O") {
    out$material_subjacente <-
      familia_organossolo_material_subjacente(pedon,
                                                 max_depth_cm = max_depth_cm)
    out$espessura_organica <- familia_organossolo_espessura(pedon)
    out$lenhosidade <- familia_organossolo_lenhosidade(pedon)
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


# ---- v0.7.14.C: mineralogia + atividade da argila + oxidos Fe + andico ---

#' Familia: mineralogia da fracao areia (Cap 18, p 286)
#'
#' Identifica predominio de minerais facilmente alteraveis na
#' fracao areia (>= 0,05 mm) na secao de controle. Classes:
#' \itemize{
#'   \item \code{micacea}: \code{sand_mica_pct >= 15} (% volume).
#'   \item \code{anfibolitica}: \code{sand_amphibole_pct >= 15}.
#'   \item \code{feldspatica}: \code{sand_feldspar_pct >= 15}.
#' }
#'
#' Quando os percentuais especificos estao ausentes, busca a
#' coluna \code{sand_mineralogy} (atalho qualitativo, valores
#' aceitos: "micacea", "anfibolitica", "feldspatica").
#'
#' Aplicavel a Cambissolos, Chernossolos, Gleissolos, Luvissolos,
#' Neossolos (excepto Quartzarenicos), Nitossolos, Planossolos,
#' Plintossolos e Vertissolos.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_depth_cm Profundidade da secao de controle (default 200).
#' @param threshold Limiar de % volume (default 15).
#' @return \code{\link{FamilyAttribute}}.
#' @references Embrapa (2018), SiBCS 5a ed., Cap 18, p 286.
#' @export
familia_mineralogia_areia <- function(pedon, max_depth_cm = 200,
                                          threshold = 15) {
  h <- pedon$horizons
  pcts <- list(
    micacea     = .weighted_avg_in_depth(h, "sand_mica_pct",
                                              max_depth_cm = max_depth_cm),
    anfibolitica = .weighted_avg_in_depth(h, "sand_amphibole_pct",
                                              max_depth_cm = max_depth_cm),
    feldspatica  = .weighted_avg_in_depth(h, "sand_feldspar_pct",
                                              max_depth_cm = max_depth_cm)
  )
  # Tenta % explicito primeiro
  qualifying <- vapply(pcts, function(p) !is.na(p) && p >= threshold,
                          logical(1))
  classe <- NULL
  if (any(qualifying)) {
    # Se mais de um qualifica, retorna o de maior valor
    vals <- unlist(pcts)
    vals[!qualifying] <- -Inf
    classe <- names(vals)[which.max(vals)]
  } else {
    # Fallback: coluna qualitativa sand_mineralogy
    sm <- h$sand_mineralogy
    if (!is.null(sm)) {
      tops <- h$top_cm
      bots <- h$bottom_cm
      ok <- !is.na(sm) & !is.na(tops) & !is.na(bots) &
              tops < max_depth_cm
      if (any(ok)) {
        candidates <- unique(sm[ok])
        candidates <- candidates[candidates %in%
                                     c("micacea", "anfibolitica",
                                       "feldspatica")]
        if (length(candidates) >= 1) classe <- candidates[1]
      }
    }
  }
  miss <- character(0)
  if (all(vapply(pcts, is.na, logical(1)))) {
    if (is.null(classe)) {
      miss <- c("sand_mica_pct", "sand_amphibole_pct",
                  "sand_feldspar_pct", "sand_mineralogy")
    }
  }
  FamilyAttribute$new(
    name = "mineralogia_areia", value = classe,
    evidence = list(pcts = pcts, threshold = threshold,
                      max_depth_cm = max_depth_cm),
    missing = miss,
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 18, p 286"
  )
}


#' Familia: mineralogia da fracao argila para Latossolos
#' (Cap 18, p 286-287)
#'
#' Classifica via Ki = SiO2/(Al2O3) e Kr = SiO2/(Al2O3 + Fe2O3)
#' molares (helpers \code{\link{compute_ki}} / \code{\link{compute_kr}}):
#' \itemize{
#'   \item \code{caulinitico}: Ki > 0.75 e Kr > 0.75
#'   \item \code{caulinitico-oxidico}: Ki > 0.75 e Kr <= 0.75
#'   \item \code{gibsitico-oxidico}: Ki <= 0.75 e Kr <= 0.75
#'   \item \code{oxidico}: Kr <= 0.75 (predominio Fe2O3 + Al2O3)
#' }
#'
#' Aplicavel principalmente para Latossolos; tambem pode ser
#' usado em Argissolos, Cambissolos e Plintossolos quando ha
#' informacao de mineralogia da argila pelo menos semiquantitativa.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_depth_cm Profundidade da secao de controle (default 200).
#' @return \code{\link{FamilyAttribute}}.
#' @references Embrapa (2018), SiBCS 5a ed., Cap 18, p 286-287.
#' @export
familia_mineralogia_argila_latossolo <- function(pedon,
                                                    max_depth_cm = 200) {
  h <- pedon$horizons
  sio2 <- .weighted_avg_in_depth(h, "sio2_sulfuric_pct",
                                    max_depth_cm = max_depth_cm)
  al2o3 <- .weighted_avg_in_depth(h, "al2o3_sulfuric_pct",
                                     max_depth_cm = max_depth_cm)
  fe2o3 <- .weighted_avg_in_depth(h, "fe2o3_sulfuric_pct",
                                     max_depth_cm = max_depth_cm)
  miss <- character(0)
  if (is.na(sio2)) miss <- c(miss, "sio2_sulfuric_pct")
  if (is.na(al2o3)) miss <- c(miss, "al2o3_sulfuric_pct")
  if (is.na(fe2o3)) miss <- c(miss, "fe2o3_sulfuric_pct")
  if (length(miss) > 0L) {
    return(FamilyAttribute$new(
      name = "mineralogia_argila", value = NULL,
      evidence = list(reason = "Ki/Kr nao computavel"),
      missing = miss,
      reference = "Embrapa (2018), SiBCS 5a ed., Cap 18, p 286-287"
    ))
  }
  ki <- compute_ki(sio2, al2o3)
  kr <- compute_kr(sio2, al2o3, fe2o3)
  classe <- if (is.na(ki) || is.na(kr)) NULL
            else if (ki > 0.75 && kr > 0.75) "caulinitico"
            else if (ki > 0.75 && kr <= 0.75) "caulinitico-oxidico"
            else if (ki <= 0.75 && kr <= 0.75) "gibsitico-oxidico"
            else NULL
  FamilyAttribute$new(
    name = "mineralogia_argila", value = classe,
    evidence = list(sio2_sulfuric_pct = sio2,
                      al2o3_sulfuric_pct = al2o3,
                      fe2o3_sulfuric_pct = fe2o3,
                      ki = ki, kr = kr),
    missing = character(0),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 18, p 286-287"
  )
}


#' Familia: subgrupamento de atividade da fracao argila (Cap 18, p 287)
#'
#' Classifica pela CTC da fracao argila T = (cec_cmol * 100 / clay_pct):
#' \itemize{
#'   \item \code{Tmb}: T < 8 cmolc/kg argila (muito baixa)
#'   \item \code{Tmob}: 8 <= T < 17 (moderadamente baixa)
#'   \item \code{Tm}: 17 <= T < 27 (media)
#'   \item \code{Tmoa}: 27 <= T < 40 (moderadamente alta)
#'   \item \code{Tma}: T >= 40 (muito alta)
#' }
#'
#' Considerada na maior parte do horizonte B (ou C, na ausencia de B).
#' Nao aplicavel a solos de classe textural areia ou areia franca
#' (clay < 15 g kg-1 = 1,5\%).
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_depth_cm Profundidade da secao de controle (default 150).
#' @return \code{\link{FamilyAttribute}}.
#' @references Embrapa (2018), SiBCS 5a ed., Cap 18, p 287.
#' @export
familia_atividade_argila <- function(pedon, max_depth_cm = 150) {
  h <- pedon$horizons
  cec <- .weighted_avg_in_depth(h, "cec_cmol",
                                   max_depth_cm = max_depth_cm)
  clay <- .weighted_avg_in_depth(h, "clay_pct",
                                    max_depth_cm = max_depth_cm)
  miss <- character(0)
  if (is.na(cec)) miss <- c(miss, "cec_cmol")
  if (is.na(clay)) miss <- c(miss, "clay_pct")
  if (length(miss) > 0L) {
    return(FamilyAttribute$new(
      name = "atividade_argila", value = NULL,
      evidence = list(reason = "CEC ou clay nao informado"),
      missing = miss,
      reference = "Embrapa (2018), SiBCS 5a ed., Cap 18, p 287"
    ))
  }
  if (clay < 1.5) {  # ~ areia / areia franca: nao aplicavel
    return(FamilyAttribute$new(
      name = "atividade_argila", value = NULL,
      evidence = list(reason = "textura areia/areia franca",
                        clay_pct = clay),
      missing = character(0),
      reference = "Embrapa (2018), SiBCS 5a ed., Cap 18, p 287"
    ))
  }
  T_argila <- cec * 100 / clay
  classe <- if (T_argila < 8) "Tmb"
            else if (T_argila < 17) "Tmob"
            else if (T_argila < 27) "Tm"
            else if (T_argila < 40) "Tmoa"
            else "Tma"
  FamilyAttribute$new(
    name = "atividade_argila", value = classe,
    evidence = list(cec_cmol_avg = cec, clay_pct_avg = clay,
                      T_argila_cmol_kg = T_argila),
    missing = character(0),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 18, p 287"
  )
}


#' Familia: teor de oxidos de ferro (Cap 1, p 42)
#'
#' Classifica pelo teor de Fe2O3 (g/kg de solo, equivalente a
#' fe2o3_sulfuric_pct * 10) na maior parte do horizonte B:
#' \itemize{
#'   \item \code{hipoferrico}: < 80 g/kg (= < 8\%)
#'   \item \code{mesoferrico}: 80 - 180 g/kg ([8\%, 18\%))
#'   \item \code{ferrico}: 180 - 360 g/kg ([18\%, 36\%))
#'   \item \code{perferrico}: >= 360 g/kg (>= 36\%)
#' }
#'
#' Aplicavel a Argissolos, Cambissolos, Chernossolos, Latossolos,
#' Neossolos Litolicos, Neossolos Regoliticos, Nitossolos e
#' Plintossolos. Quando o atributo ja foi considerado em nivel
#' categorico mais alto (e.g. Latossolos Eutroferricos /
#' Distroferricos / Acriferricos), o motor de Familia pula esta
#' dimensao.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_depth_cm Profundidade da secao de controle (default 150).
#' @return \code{\link{FamilyAttribute}}.
#' @references Embrapa (2018), SiBCS 5a ed., Cap 1, p 42.
#' @export
familia_oxidos_ferro <- function(pedon, max_depth_cm = 150) {
  h <- pedon$horizons
  b_layers <- which(!is.na(h$designation) & grepl("^B", h$designation))
  if (length(b_layers) == 0L) {
    return(FamilyAttribute$new(
      name = "oxidos_ferro", value = NULL,
      evidence = list(reason = "no B horizons"),
      missing = "designation",
      reference = "Embrapa (2018), SiBCS 5a ed., Cap 1, p 42"
    ))
  }
  fe_avg <- .weighted_avg_in_depth(h, "fe2o3_sulfuric_pct",
                                      max_depth_cm = max_depth_cm)
  if (is.na(fe_avg)) {
    return(FamilyAttribute$new(
      name = "oxidos_ferro", value = NULL,
      evidence = list(reason = "fe2o3_sulfuric_pct nao informado"),
      missing = "fe2o3_sulfuric_pct",
      reference = "Embrapa (2018), SiBCS 5a ed., Cap 1, p 42"
    ))
  }
  classe <- if (fe_avg < 8) "hipoferrico"
            else if (fe_avg < 18) "mesoferrico"
            else if (fe_avg < 36) "ferrico"
            else "perferrico"
  FamilyAttribute$new(
    name = "oxidos_ferro", value = classe,
    evidence = list(fe2o3_sulfuric_pct_avg = fe_avg,
                      max_depth_cm = max_depth_cm),
    missing = character(0),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 1, p 42"
  )
}


#' Familia: propriedades andicas (Cap 1, p 42-43)
#'
#' Aplica o termo "andico" quando, em qualquer horizonte:
#' \itemize{
#'   \item densidade do solo <= 0,9 g/cm3, E
#'   \item retencao de fosfato >= 85\%, E
#'   \item Alo + 0.5 * Feo >= 2\% (oxalato extraivel)
#' }
#'
#' Aplicavel para Cambissolos Histicos e Organossolos Folicos
#' (Cap 18 p 287), em fase de validacao.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_db Densidade maxima (default 0.9 g/cm3).
#' @param min_pret Retencao minima de fosfato (default 85\%).
#' @param min_aloxfeox Limite de Alo + 0.5*Feo (default 2\%).
#' @return \code{\link{FamilyAttribute}} com \code{value} =
#'         \code{"andico"} ou NULL.
#' @references Embrapa (2018), SiBCS 5a ed., Cap 1, p 42-43;
#'             Cap 18, p 287.
#' @export
familia_andico <- function(pedon, max_db = 0.9, min_pret = 85,
                              min_aloxfeox = 2) {
  h <- pedon$horizons
  db <- h$bulk_density_g_cm3
  pret <- h$phosphate_retention_pct
  alox <- h$al_ox_pct
  feox <- h$fe_ox_pct
  miss <- character(0)
  if (all(is.na(db))) miss <- c(miss, "bulk_density_g_cm3")
  if (all(is.na(pret))) miss <- c(miss, "phosphate_retention_pct")
  if (all(is.na(alox))) miss <- c(miss, "al_ox_pct")
  if (all(is.na(feox))) miss <- c(miss, "fe_ox_pct")
  if (length(miss) > 0L) {
    return(FamilyAttribute$new(
      name = "andico", value = NULL,
      evidence = list(reason = "criterios andicos nao computaveis"),
      missing = miss,
      reference = "Embrapa (2018), SiBCS 5a ed., Cap 1, p 42-43"
    ))
  }
  passes <- !is.na(db) & db <= max_db &
              !is.na(pret) & pret >= min_pret &
              !is.na(alox) & !is.na(feox) &
              (alox + 0.5 * feox) >= min_aloxfeox
  classe <- if (any(passes)) "andico" else NULL
  FamilyAttribute$new(
    name = "andico", value = classe,
    evidence = list(passing_layers = which(passes),
                      max_db = max_db, min_pret = min_pret,
                      min_aloxfeox = min_aloxfeox),
    missing = character(0),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 1, p 42-43"
  )
}


# ---- v0.7.14.D: Organossolos especificos (Cap 18, p 287-288) -------------

# Helper: identifica camadas orgânicas no perfil via designation
# H/O (horizontes organicos no SiBCS Cap 14).
.organic_layers <- function(h) {
  is_org <- !is.na(h$designation) & grepl("^[HO]", h$designation)
  which(is_org)
}


#' Familia: material subjacente em Organossolos (Cap 18, p 287)
#'
#' Identifica a textura da primeira camada nao-organica abaixo das
#' camadas organicas, na secao de controle. Retorna o grupamento
#' textural daquele material como adjetivo (e.g. "arenoso",
#' "argiloso").
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_depth_cm Profundidade da secao de controle (default 200).
#' @return \code{\link{FamilyAttribute}}.
#' @references Embrapa (2018), SiBCS 5a ed., Cap 18, p 287.
#' @export
familia_organossolo_material_subjacente <- function(pedon,
                                                       max_depth_cm = 200) {
  h <- pedon$horizons
  if (nrow(h) == 0L) {
    return(FamilyAttribute$new(
      name = "material_subjacente", value = NULL,
      evidence = list(reason = "no horizons"),
      missing = character(0),
      reference = "Embrapa (2018), SiBCS 5a ed., Cap 18, p 287"
    ))
  }
  ord <- order(h$top_cm, na.last = TRUE)
  org <- .organic_layers(h)
  # Primeira camada nao-organica DEPOIS da ultima organica
  org_in_order <- ord[ord %in% org]
  if (length(org_in_order) == 0L) {
    return(FamilyAttribute$new(
      name = "material_subjacente", value = NULL,
      evidence = list(reason = "nenhuma camada organica encontrada"),
      missing = character(0),
      reference = "Embrapa (2018), SiBCS 5a ed., Cap 18, p 287"
    ))
  }
  pos_last_org <- max(which(ord %in% org))
  candidates <- ord[seq_len(length(ord))][-seq_len(pos_last_org)]
  candidates <- candidates[!is.na(h$top_cm[candidates]) &
                              h$top_cm[candidates] < max_depth_cm]
  candidates <- setdiff(candidates, org)
  if (length(candidates) == 0L) {
    return(FamilyAttribute$new(
      name = "material_subjacente", value = NULL,
      evidence = list(reason = "no material mineral subjacente na SC"),
      missing = character(0),
      reference = "Embrapa (2018), SiBCS 5a ed., Cap 18, p 287"
    ))
  }
  i <- candidates[1]
  clay <- h$clay_pct[i]
  sand <- h$sand_pct[i]
  if (is.na(clay) || is.na(sand)) {
    return(FamilyAttribute$new(
      name = "material_subjacente", value = NULL,
      evidence = list(reason = "textura mineral subjacente NA",
                        layer = i),
      missing = c("clay_pct", "sand_pct"),
      reference = "Embrapa (2018), SiBCS 5a ed., Cap 18, p 287"
    ))
  }
  classe <- if (sand - clay > 70) "arenoso"
            else if (clay > 60) "muito_argiloso"
            else if (clay >= 35) "argiloso"
            else if (clay < 35 && sand < 15) "siltoso"
            else "medio"
  FamilyAttribute$new(
    name = "material_subjacente", value = classe,
    evidence = list(layer = i, clay_pct = clay, sand_pct = sand),
    missing = character(0),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 18, p 287"
  )
}


#' Familia: espessura > 100 cm de material organico em Organossolos
#' (Cap 18, p 287)
#'
#' Retorna \code{"espesso"} quando a soma das espessuras de
#' camadas organicas a partir da superficie excede 100 cm
#' (Cap 18 p 287: "Organossolos com mais de 100 cm de material
#' organico a partir da sua superficie").
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_cm Default 100 cm.
#' @return \code{\link{FamilyAttribute}}.
#' @references Embrapa (2018), SiBCS 5a ed., Cap 18, p 287.
#' @export
familia_organossolo_espessura <- function(pedon, min_cm = 100) {
  h <- pedon$horizons
  org <- .organic_layers(h)
  if (length(org) == 0L) {
    return(FamilyAttribute$new(
      name = "espessura_organica", value = NULL,
      evidence = list(reason = "no organic layers"),
      missing = character(0),
      reference = "Embrapa (2018), SiBCS 5a ed., Cap 18, p 287"
    ))
  }
  thk <- sum(pmax(h$bottom_cm[org] - h$top_cm[org], 0), na.rm = TRUE)
  classe <- if (thk > min_cm) "espesso" else NULL
  FamilyAttribute$new(
    name = "espessura_organica", value = classe,
    evidence = list(thickness_cm = thk, threshold_cm = min_cm,
                      organic_layers = org),
    missing = character(0),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 18, p 287"
  )
}


#' Familia: lenhosidade em Organossolos (Cap 18, p 288)
#'
#' Classifica a presenca de galhos / fragmentos de troncos > 2 cm em
#' camadas organicas, "a semelhanca do utilizado para qualificar
#' as classes de pedregosidade" (Cap 18 p 288):
#' \itemize{
#'   \item \code{lenhoso}: 10\% <= woody_fragments < 30\%
#'   \item \code{muito_lenhoso}: 30\% <= woody_fragments <= 50\%
#'   \item \code{extremamente_lenhoso}: woody_fragments > 50\%
#' }
#' (Limites adotados a partir das classes de pedregosidade,
#' Santos et al. 2015.)
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @return \code{\link{FamilyAttribute}}.
#' @references Embrapa (2018), SiBCS 5a ed., Cap 18, p 288.
#' @export
familia_organossolo_lenhosidade <- function(pedon) {
  h <- pedon$horizons
  org <- .organic_layers(h)
  if (length(org) == 0L) {
    return(FamilyAttribute$new(
      name = "lenhosidade", value = NULL,
      evidence = list(reason = "no organic layers"),
      missing = character(0),
      reference = "Embrapa (2018), SiBCS 5a ed., Cap 18, p 288"
    ))
  }
  wf <- h$woody_fragments_pct[org]
  if (all(is.na(wf))) {
    return(FamilyAttribute$new(
      name = "lenhosidade", value = NULL,
      evidence = list(reason = "woody_fragments_pct nao informado"),
      missing = "woody_fragments_pct",
      reference = "Embrapa (2018), SiBCS 5a ed., Cap 18, p 288"
    ))
  }
  max_wf <- max(wf, na.rm = TRUE)
  classe <- if (max_wf > 50) "extremamente_lenhoso"
            else if (max_wf >= 30) "muito_lenhoso"
            else if (max_wf >= 10) "lenhoso"
            else NULL
  FamilyAttribute$new(
    name = "lenhosidade", value = classe,
    evidence = list(woody_fragments_pct_max = max_wf,
                      organic_layers = org),
    missing = character(0),
    reference = "Embrapa (2018), SiBCS 5a ed., Cap 18, p 288"
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
