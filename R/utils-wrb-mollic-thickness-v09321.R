# =============================================================================
# v0.9.321 -- criterion 6 of the WRB 2022 mollic and umbric horizons, the
# thickness, with item a.
#
# Mollic horizon (3.1.20, p. 53): "6. a thickness of one of the following:
# a. >= 10 cm if directly overlying continuous rock, technic hard material or
# a cryic, petrocalcic, petroduric, petrogypsic or petroplinthic horizon; or
# b. >= 20 cm." Umbric horizon (3.1.39, p. 70): the same, with "a cryic,
# petroduric or petroplinthic horizon". Until v0.9.321 only item b was read,
# so a mollic or umbric horizon of 10 to < 20 cm on rock was not one.
# =============================================================================

# Per layer, TRUE / FALSE / NA: the layer is one that item 6a names.
#   continuous rock  an R layer (.WRB_ROCK_DESIGNATION, as continuous_rock()
#                    reads it); undecided where no designation is recorded;
#   technic hard material, cryic and petro* horizons
#                    the layers their diagnostics find. Where a diagnostic is
#                    undecided (most profiles: no CaCO3, gypsum or soil
#                    temperature recorded), the layer is read from its own
#                    description: a petro* horizon is "at least moderately
#                    cemented" (all four), so a layer recorded as not or
#                    weakly cemented is none, and one with no cementation
#                    class but a designation is one only with the suffix m
#                    ("pedogenic cementation ... at least moderately
#                    cemented", Annex 1, Table 10.2, p. 227); a cryic horizon
#                    is permafrost, suffix f (same table, p. 226), read with
#                    the pattern of cryic_conditions(). A layer with neither
#                    a cementation class nor a designation is undecided.
# Returns list(any = the 6a status, hard = rock, technic hard material or a
# petro* horizon, which are not soil material of a mollic or umbric horizon).
.wrb_mollic_6a_layers <- function(pedon, umbric = FALSE) {
  h <- pedon$horizons
  n <- nrow(h)
  d <- as.character(h$designation %||% rep(NA_character_, n))
  has_d <- !is.na(d) & nzchar(trimws(d))
  rock <- ifelse(has_d, grepl(.WRB_ROCK_DESIGNATION, d), NA)
  cem <- tolower(trimws(as.character(h$cementation_class %||% rep(NA_character_, n))))
  master <- sub("^[0-9]*", "", d)
  cemented <- ifelse(!is.na(cem) & nzchar(cem),
                     ifelse(cem %in% c("none", "weakly"), FALSE, NA),
                     ifelse(has_d, ifelse(grepl("^[A-Z]+[a-z]*m", master), NA, FALSE), NA))
  frozen <- ifelse(has_d, ifelse(grepl("^[A-Z][a-z]*f($|[0-9])|^Cf|perma", master), NA, FALSE), NA)
  per_layer <- function(fun, undecided) {
    r <- tryCatch(fun(pedon), error = function(e) NULL)
    if (is.null(r) || is.na(r$passed)) return(undecided)
    seq_len(n) %in% (if (isTRUE(r$passed)) r$layers else integer(0))
  }
  petro <- if (umbric) list(petroduric, petroplinthic)
           else list(petrocalcic, petroduric, petrogypsic, petroplinthic)
  hard <- Reduce(.any3v, c(list(rock, per_layer(technic_hard_material, rep(NA, n))),
                           lapply(petro, per_layer, undecided = cemented)))
  list(any = as.logical(.any3v(hard, per_layer(cryic_horizon, frozen))),
       hard = as.logical(hard))
}

# Criterion 6 over the run of mollic or umbric layers (.wrb_mollic_run()):
# the horizon is the sure layers `run$layers`, possibly continued through the
# undecided ones of `run$maybe`; it cannot continue into rock, technic hard
# material or a petro* horizon. TRUE when the sure layers meet 6a or 6b,
# NA when a longer horizon through undecided layers might, FALSE otherwise.
# Nothing described directly below a horizon of 10 to < 20 cm leaves 6a
# undecided.
.wrb_mollic_criterion6 <- function(pedon, run, min_thickness = 20,
                                   min_on_barrier = 10, umbric = FALSE) {
  h <- pedon$horizons
  thk <- function(x) sum(pmax(h$bottom_cm[x] - h$top_cm[x], 0), na.rm = TRUE)
  L <- run$layers
  U <- setdiff(run$maybe, L)
  six <- NULL
  layer_status <- function() {
    if (is.null(six)) six <<- .wrb_mollic_6a_layers(pedon, umbric = umbric)
    six
  }
  # the run cannot go on into a hard layer
  if (length(U)) {
    hard <- layer_status()$hard
    stop_at <- which(hard[U] %in% TRUE)
    if (length(stop_at)) U <- U[seq_len(stop_at[1L] - 1L)]
  }
  directly_below <- function(x) {
    b <- max(h$bottom_cm[x], na.rm = TRUE)
    which(!is.na(h$top_cm) & abs(h$top_cm - b) < 1e-6)
  }
  meets <- function(x) {
    t <- thk(x)
    if (t >= min_thickness) return(TRUE)
    if (t < min_on_barrier || !length(x)) return(FALSE)
    j <- directly_below(x)
    if (!length(j)) return(NA)
    .wrb_any3(layer_status()$any[j])
  }
  sure <- meets(L)
  longer <- if (length(U)) lapply(seq_along(U), function(k) meets(c(L, U[seq_len(k)])))
            else list()
  passed <- if (isTRUE(sure)) TRUE
            else if (is.na(sure) || any(vapply(longer, function(x) !isFALSE(x), NA))) NA
            else FALSE
  list(passed = passed, layers = if (isTRUE(passed)) L else integer(0),
       thickness_cm = thk(L), by_6a = isTRUE(sure) && thk(L) < min_thickness,
       undecided_below = U,
       layer_below = if (length(L)) directly_below(L) else integer(0))
}
