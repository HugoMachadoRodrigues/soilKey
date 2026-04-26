# ================================================================
# SiBCS 5a edicao -- Diagnosticos de horizonte (v0.2 scaffold)
#
# v0.2 scaffold scope:
#   - B_latossolico : delegated to WRB ferralic() (the SiBCS criteria
#                       align with WRB ferralic for the core CTC/argila
#                       and ki/kr cut-offs; the SiBCS-specific tests
#                       on Ki/Kr, B textural exclusion, and uniformity
#                       are scheduled for v0.7).
#   - B_textural    : delegated to WRB argic() (clay-increase rules are
#                       essentially identical; SiBCS-specific gradiente
#                       textural cut-offs and exclusoes are v0.7).
#
# All other SiBCS diagnostics (B nitico, B espodico, B incipiente,
# A chernozemico, A humico, hidromorfia, plintita, vertico, planico,
# horizonte hortico, horizonte O hidromorfico, ...) are scheduled
# for v0.7 when the SiBCS parallel key is filled out properly.
# ================================================================


#' Horizonte B latossolico (SiBCS 5a edicao)
#'
#' v0.2 scaffold delegating ao WRB \code{\link{ferralic}}. As criterios
#' de B latossolico do SiBCS 5 incluem ainda Ki <= 2,2 e Kr <= 1,7
#' (relacoes silica/alumina), uniformidade textural, e exclusao de
#' gradiente textural significativo -- todos agendados para v0.7.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param ... Passed to \code{\link{ferralic}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @references Embrapa (2018). \emph{Sistema Brasileiro de Classificacao
#'   de Solos}, 5a edicao. Embrapa Solos, Brasilia. Capitulo sobre
#'   horizontes diagnosticos -- B latossolico.
#' @export
B_latossolico <- function(pedon, ...) {
  res <- ferralic(pedon, ...)
  res$name      <- "B_latossolico"
  res$reference <- paste0("Embrapa (2018), SiBCS 5a ed., B latossolico ",
                            "-- delegacao ao WRB ferralic() em v0.2 scaffold")
  res$notes     <- "v0.2 scaffold delegates to WRB ferralic; Ki/Kr e exclusoes em v0.7"
  res
}


#' Horizonte B textural (SiBCS 5a edicao)
#'
#' v0.2 scaffold delegating ao WRB \code{\link{argic}}. SiBCS exige
#' gradiente textural quantitativo proximo do WRB argic; diferencas
#' em criterios de profundidade e estrutura sao v0.7.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param ... Passed to \code{\link{argic}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @references Embrapa (2018), SiBCS 5a ed., B textural.
#' @export
B_textural <- function(pedon, ...) {
  res <- argic(pedon, ...)
  res$name      <- "B_textural"
  res$reference <- paste0("Embrapa (2018), SiBCS 5a ed., B textural ",
                            "-- delegacao ao WRB argic() em v0.2 scaffold")
  res$notes     <- "v0.2 scaffold delegates to WRB argic; refinamentos SiBCS em v0.7"
  res
}
