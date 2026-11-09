# =============================================================================
# v0.9.384 -- the FAO-Unesco soil legend in soilKey: the user function
# classify_fao(pedon, edition), the registry id fao1988 (classify_fao1988(),
# a thin wrapper) and the key to the 28 major soil groupings of the Revised
# Legend (FAO 1988, World Soil Resources Report 60; the text of the ISRIC
# reprint of 1997, Technical Paper 20), chapter X, pp. 75-89. The soil units
# (0.9.385), the phases (0.9.386) and the Legend of 1974 (fao1974, from
# 0.9.389) follow in later parts.
#
# The gates read the 16 diagnostic horizons of 0.9.382
# (R/diagnostics-fao88-horizons.R) and the 27 diagnostic properties of
# 0.9.383 (R/diagnostics-fao88-properties.R); the predicates below combine
# them where a gate reads more than one diagnostic in one place (the organic
# layers of the Histosols, the plinthite under an albic E of the
# Plinthosols, the E over a slowly permeable horizon of the Planosols, the
# CEC and base saturation of the argic B ...). The criteria are paraphrased;
# no text of the book is shipped. The numbers are those of
# inst/rules/fao1988/thresholds.yaml, the readings those of
# inst/rules/fao1988/readings.yaml.
#
# Conventions: three values (a measurement not recorded leaves a criterion
# undecided, a field observation not recorded is absent); depths from the
# literal soil surface (Q9); the key read as printed, the definitions of
# chapter VI not added (Q6); no residual grouping after the Cambisols: a soil
# no gate takes is "[not classified]" in brackets, with the gates that
# failed (Q5).
# =============================================================================

# The 28 major soil groupings in key order, with the key's page.
.F88_GROUPINGS <- data.frame(stringsAsFactors = FALSE,
  code = c("HS", "AT", "LP", "VR", "FL", "SC", "GL", "AN", "AR", "RG", "PZ", "PT", "FR", "PL",
           "SN", "GR", "CH", "KS", "PH", "PD", "GY", "CL", "NT", "AL", "AC", "LV", "LX", "CM"),
  name = c("Histosols", "Anthrosols", "Leptosols", "Vertisols", "Fluvisols", "Solonchaks",
           "Gleysols", "Andosols", "Arenosols", "Regosols", "Podzols", "Plinthosols",
           "Ferralsols", "Planosols", "Solonetz", "Greyzems", "Chernozems", "Kastanozems",
           "Phaeozems", "Podzoluvisols", "Gypsisols", "Calcisols", "Nitisols", "Alisols",
           "Acrisols", "Luvisols", "Lixisols", "Cambisols"),
  page = c("75", "75", "76", "77", "77", "78", "78", "79", "80", "80", "81", "81", "82", "82",
           "83", "83", "84", "84", "84", "85", "85", "86", "86", "87", "87", "88", "88", "89"))


# --- the key-level predicates --------------------------------------------------------------

# The DiagnosticResult of a key-level predicate: `layers` in the context's
# depth order, mapped back to the rows of pedon$horizons.
.f88_key_result <- function(x, id, value, page, layers = integer(0), missing = character(0),
                            flags = character(0), notes = character(0), evidence = list()) {
  value <- if (length(value) == 1L) as.logical(value) else NA
  flags <- unique(as.character(flags[!is.na(flags) & nzchar(flags)]))
  if (x$n && all(is.na(x$top) | is.na(x$bot))) {
    value <- NA
    missing <- c("top_cm", "bottom_cm", missing)
  }
  layers <- sort(unique(as.integer(layers[!is.na(layers)])))
  DiagnosticResult$new(
    name = paste0(id, "_fao88"),
    passed = value,
    layers = x$idx[layers],
    evidence = c(evidence, list(edition = "1988", page = page, flags = flags,
                                stand_in = character(0), lower_grade = FALSE)),
    missing = if (is.na(value)) unique(as.character(missing)) else character(0),
    reference = sprintf("FAO (1988), Soil Map of the World, Revised Legend (ISRIC reprint 1997), ch. X, p. %s", page),
    notes = if (length(c(notes, flags))) paste(unique(c(notes, flags)), collapse = "; ") else NA_character_)
}

# Rows of a predicate's DiagnosticResult in the context's order.
.f88_ctx_rows <- function(x, r) {
  i <- match(r$layers %||% integer(0), x$idx)
  i[!is.na(i)]
}

# The per-layer answer of a property predicate (evidence$values, in the
# context's order).
.f88_values <- function(r, n) {
  v <- r$evidence$values
  if (is.data.frame(v) && nrow(v) == n) as.logical(v$value) else rep(NA, n)
}

# Organic layers (H or O): organic soil materials (p. 39; .f88_organic());
# where the carbon leaves that undecided, a layer designated H or O counts,
# flagged (soilKey's reading).
.f88_organic_layers <- function(x) {
  org <- .f88_organic(x)
  v <- org$value
  ho <- !is.na(x$master) & grepl("^(H|O)", x$master) & !x$rock
  use <- is.na(v) & ho
  v[use] <- TRUE
  list(value = v, flags = ifelse(use, "organic_layer_from_designation", NA_character_),
       missing = org$missing)
}

# Histosols (p. 75): an H or O horizon 40 cm or more thick (60 cm or more
# when mainly sphagnum or moss, sphagnum_fibres_pct 75 or more, or with a
# bulk density below 0.1), from the surface down or taken cumulatively within
# the upper 80 cm; thinner when it rests on rock, or on fragmental material
# (coarse_fragments_pct 90 or more, soilKey's number) with organic matter in
# its interstices: then every layer above it is organic (soilKey's reading,
# no minimum printed).
histosols_gate_fao88 <- function(pedon) {
  x <- .f88_ctx(pedon)
  pg <- "75"
  if (!x$n) return(.f88_key_result(x, "histosols_gate", FALSE, pg))
  ol <- .f88_organic_layers(x)
  org <- ol$value
  sph <- .f88_num(x, "sphagnum_fibres_pct"); bd <- .f88_num(x, "bulk_density_g_cm3")
  light <- (!is.na(sph) & sph >= 75) | (!is.na(bd) & bd < 0.1)
  cf <- .f88_num(x, "coarse_fragments_pct")
  th <- pmax(x$bot - x$top, 0); th[is.na(th)] <- 0
  deepest <- suppressWarnings(max(x$bot, na.rm = TRUE)); if (!is.finite(deepest)) deepest <- 0
  on_rock <- any(x$rock)
  test <- function(o) {
    need <- if (any(light & o)) 60 else 40
    # from the surface down
    first <- which(!is.na(x$top))[1L]
    run <- Filter(function(r) r$layers[1L] == first, .f88_run_list(x, o))
    a <- length(run) > 0L && run[[1L]]$thickness >= need
    # cumulative within 80 cm
    cum <- sum(pmax(pmin(x$bot, 80) - pmax(x$top, 0), 0)[o & !is.na(x$top) & !is.na(x$bot)])
    b <- cum >= need
    # thinner on rock or on fragmental material filled with organic matter
    base <- which(x$rock | (!is.na(cf) & cf >= 90 & o))[1L]
    c <- !is.na(base) && base > 1L && isTRUE(x$top[base] > 0) && all(o[seq_len(base - 1L)])
    list(any = a || b || c, a = a, b = b, c = c, cum = cum, need = need)
  }
  sure <- test(org %in% TRUE)
  maybe <- test(!(org %in% FALSE))
  # a description that stops above 80 cm leaves the cumulative path open
  open <- !on_rock && deepest < 80 && !sure$any &&
    (maybe$cum + (80 - deepest)) >= maybe$need
  value <- if (sure$any) TRUE else if (maybe$any || open) NA else FALSE
  lay <- which(org %in% TRUE)
  .f88_key_result(x, "histosols_gate", value, pg,
                  layers = if (isTRUE(value)) lay else which(!(org %in% FALSE)),
                  missing = c(ol$missing, if (open) c("top_cm", "bottom_cm")),
                  flags = c(.f88_flags(ol$flags, lay), if (isTRUE(sure$c) && !sure$a && !sure$b) "organic_layers_on_rock_thinner"),
                  evidence = list(organic_cm_within_80 = sure$cum, thickness_needed_cm = sure$need,
                                  from_surface = sure$a, cumulative = sure$b, on_rock = sure$c))
}

# Anthrosols (p. 75): a profound modification or burial of the original
# horizons by people. Read on the field record (soilKey's reading): a fimic
# A; anthropic layers (artefacts, anthropic material, a fill, midden or
# plaggen layer_origin; .f88_anthropic()) from the surface 50 cm or more
# thick, the thickness the units of the key use (ATc, ATu); or deep
# cultivation recorded at the site (deep_cultivation_kind, or
# mechanical_mixing_depth_cm of 50 or more). Not recorded: absent.
anthrosols_gate_fao88 <- function(pedon) {
  x <- .f88_ctx(pedon)
  pg <- "75"
  if (!x$n) return(.f88_key_result(x, "anthrosols_gate", FALSE, pg))
  fim <- .f88_status(.f88_hz_fimic_a(x))
  an <- .f88_anthropic(x)
  first <- which(!is.na(x$top))[1L]
  run <- Filter(function(r) r$layers[1L] == first, .f88_run_list(x, an$value %in% TRUE))
  thick <- length(run) > 0L && run[[1L]]$thickness >= 50
  dk <- .f88_site(x, "deep_cultivation_kind")
  dk <- !is.na(dk) && nzchar(trimws(as.character(dk))) && !grepl("^(none|no)$", tolower(trimws(as.character(dk))))
  md <- suppressWarnings(as.numeric(.f88_site(x, "mechanical_mixing_depth_cm")))
  deep <- dk || (!is.na(md) && md >= 50)
  value <- .fao_or(fim, thick, deep)
  if (is.na(value)) value <- NA
  .f88_key_result(x, "anthrosols_gate", value, pg,
                  layers = if (thick) run[[1L]]$layers else integer(0),
                  missing = if (is.na(value)) .f88_hz_fimic_a(x)$missing %||% "p2o5_citric_mg_kg",
                  flags = c(if (thick) "anthropic_layers_50cm_from_surface",
                            if (deep) "deep_cultivation_recorded",
                            if (!any(an$recorded) && !deep) "anthropic_modification_not_recorded_absent"),
                  evidence = list(fimic_a = fim, anthropic_layers_50cm = thick, deep_cultivation = deep))
}

# Leptosols (p. 76): within 30 cm, continuous hard rock, highly calcareous
# material (CaCO3 equivalent above 40 %) or a continuous cemented layer; or
# less than 20 % fine earth over 75 cm. The highly calcareous material is a
# layer that is not an A, E, B, H or O (a C, or an undesignated layer below
# the first, flagged) starting within 30 cm (soilKey's reading: the
# material that limits the depth). The fine earth is 100 minus
# coarse_fragments_pct, not recorded read as none, flagged (a field
# observation), rock counting as no fine earth; a description that stops
# above 75 cm decides when the bounds do.
leptosols_gate_fao88 <- function(pedon) {
  x <- .f88_ctx(pedon)
  pg <- "76"
  if (!x$n) return(.f88_key_result(x, "leptosols_gate", FALSE, pg))
  rock <- continuous_hard_rock_fao88(pedon, max_top_cm = 30)
  cc <- .f88_caco3(x)
  m <- x$master
  cmat <- !x$rock & !is.na(x$top) & x$top <= 30 &
    ((!is.na(m) & grepl("^C", m) & !grepl("^C/B", m)) | (is.na(m) & seq_len(x$n) > 1L))
  calc <- .f88_any(.f88_cmp(cc$value, ">", 40)[cmat])
  if (!any(cmat)) calc <- FALSE
  cem <- .f88_cemented(x)
  cemv <- cem$value & !x$rock & !is.na(x$top) & x$top <= 30
  cemented <- any(cemv)
  # fine earth over 0-75 cm
  cf <- .f88_num(x, "coarse_fragments_pct")
  cf_na <- is.na(cf) & !x$rock
  fe <- ifelse(x$rock, 0, 100 - ifelse(is.na(cf), 0, cf))
  ov <- pmax(pmin(x$bot, 75) - pmax(x$top, 0), 0); ov[is.na(ov)] <- 0
  rtop <- suppressWarnings(min(x$top[x$rock], na.rm = TRUE))
  described <- sum(ov[!x$rock]) + if (is.finite(rtop) && rtop < 75) 75 - rtop else 0
  lower <- sum((fe * ov)[!x$rock]) / 75
  upper <- lower + max(75 - described, 0) * 100 / 75
  fine <- if (upper < 20) TRUE else if (lower >= 20) FALSE else NA
  value <- .fao_or(rock$passed, calc, cemented, fine)
  lay <- c(.f88_ctx_rows(x, rock), which(cemv), if (isTRUE(calc)) which(cmat & .f88_cmp(cc$value, ">", 40) %in% TRUE))
  .f88_key_result(x, "leptosols_gate", value, pg, layers = lay,
                  missing = unique(c(rock$missing, if (is.na(calc)) "caco3_pct", if (is.na(fine)) c("top_cm", "bottom_cm"))),
                  flags = c(rock$evidence$flags, .f88_flags(cc$flags, which(cmat)),
                            if (any(cf_na & ov > 0)) "coarse_fragments_not_recorded_none",
                            if (any(cmat & is.na(m))) "undesignated_layer_read_as_material"),
                  evidence = list(rock = rock$passed, highly_calcareous = calc, cemented = cemented,
                                  fine_earth_pct_0_75 = c(lower = lower, upper = upper)))
}

# Vertisols (p. 77): after mixing the upper 18 cm, 30 % clay or more in all
# horizons to 50 cm at least (a soil on rock above 50 cm fails); cracks from
# the surface 1 cm wide or more to 50 cm (cracks_width_cm with
# cracks_depth_cm, or cracks recorded in a layer reaching 50 cm), waived on
# an irrigated site, flagged; and intersecting slickensides (any recorded,
# flagged slickensides_read_as_intersecting) or wedge-shaped or
# parallelepiped aggregates in a layer between 25 and 100 cm. Cracks,
# slickensides and structure are field observations: not recorded, absent.
vertisols_gate_fao88 <- function(pedon) {
  x <- .f88_ctx(pedon)
  pg <- "77"
  if (!x$n) return(.f88_key_result(x, "vertisols_gate", FALSE, pg))
  clay <- .f88_num(x, "clay_pct")
  mix <- .fao_mixed(x$h, "clay_pct", 18)
  rtop <- suppressWarnings(min(x$top[x$rock], na.rm = TRUE))
  shallow <- is.finite(rtop) && rtop < 50
  below <- .fao_in_window(x$h, .f88_cmp(clay, ">=", 30), 18, 50, "throughout")
  c1 <- if (shallow) FALSE else .fao_and(.f88_cmp(mix$value, ">=", 30), below$value)
  # cracks
  cw <- .f88_num(x, "cracks_width_cm"); cdp <- .f88_num(x, "cracks_depth_cm")
  rec <- !is.na(cw) | !is.na(cdp)
  wide <- !is.na(cw) & cw >= 1
  reach <- (!is.na(cdp) & cdp >= 50) | (wide & !is.na(x$bot) & x$bot >= 50)
  c2 <- if (any(wide) && any(reach)) TRUE
        else if (!any(rec)) FALSE
        else if (any(wide) && all(is.na(cdp))) NA else FALSE
  irr <- isTRUE(.f88_site_lgl(x, "irrigated"))
  c2_flag <- character(0)
  if (!isTRUE(c2) && irr) { c2 <- TRUE; c2_flag <- "cracks_waived_irrigated" }
  if (!any(rec) && !irr) c2_flag <- "cracks_not_recorded_absent"
  # slickensides or wedge-shaped / parallelepiped aggregates, 25-100 cm
  sl <- .f88_values(slickensides_fao88(pedon), x$n)
  st <- .f88_chr(x, "structure_type")
  wedge <- !is.na(st) & grepl("wedge|parallelepiped|lenticular|cunei", st)
  win <- !is.na(x$top) & !is.na(x$bot) & x$top < 100 & x$bot > 25 & !x$rock
  c3 <- any((sl %in% TRUE | wedge) & win)
  value <- .fao_and(c1, c2, c3)
  .f88_key_result(x, "vertisols_gate", value, pg,
                  layers = which((sl %in% TRUE | wedge) & win),
                  missing = unique(c(mix$missing, below$missing, if (is.na(c2)) "cracks_depth_cm")),
                  flags = c(c2_flag, if (any(sl %in% TRUE & win)) "slickensides_read_as_intersecting"),
                  evidence = list(clay_mixed_18cm = mix$value, clay_30_to_50cm = c1, cracks = c2,
                                  slickensides_or_wedges = c3))
}

# "No diagnostic horizons other than ...": TRUE when none of the 16
# horizons of chapter IV outside `allowed` is present, NA when one of them
# is undecided, FALSE when one is present (buried horizons and those starting
# below 125 cm are not diagnostic, p. 42, as the horizon predicates read
# them). `allowed` holds horizon ids (.F88_HZ$id), "a_horizon" (any A:
# mollic, umbric, ochric or fimic), "argic_b_calcareous" (an argic B every
# layer of which is calcareous, the Calcisols) and "argic_b_permeated" (an
# argic B every layer of which is calcareous or gypsiferous, the Gypsisols;
# soilKey's reading of "permeated with gypsum or calcium carbonate").
only_horizons_fao88 <- function(pedon, allowed = character(0)) {
  x <- .f88_ctx(pedon)
  pg <- "42"
  allowed <- as.character(unlist(allowed))
  if ("a_horizon" %in% allowed) allowed <- c(allowed, "mollic_a", "umbric_a", "ochric_a", "fimic_a")
  cond <- intersect(allowed, c("argic_b_calcareous", "argic_b_permeated"))
  others <- setdiff(.F88_HZ$id, allowed)
  res <- lapply(others, function(id) get(paste0(id, "_fao88"), mode = "function")(pedon))
  names(res) <- others
  st <- vapply(res, function(r) { p <- r$passed; if (length(p) == 1L) as.logical(p) else NA }, NA)
  cond_flags <- character(0)
  if (length(cond) && "argic_b" %in% others) {
    r <- res$argic_b
    L <- .f88_ctx_rows(x, r)
    calc <- .f88_values(calcareous_fao88(pedon), x$n)
    ok <- if (cond[1L] == "argic_b_permeated") .f88_or(calc, .f88_values(gypsiferous_fao88(pedon), x$n)) else calc
    all_ok <- if (length(L)) .f88_all(ok[L]) else NA
    st[["argic_b"]] <- .fao_and(st[["argic_b"]], .f88_not(all_ok))
    cond_flags <- paste0("argic_b_allowed_when_", sub("argic_b_", "", cond[1L]))
  }
  present <- names(st)[st %in% TRUE]
  undecided <- names(st)[is.na(st)]
  value <- if (length(present)) FALSE else if (length(undecided)) NA else TRUE
  lay <- unlist(lapply(res[present], function(r) .f88_ctx_rows(x, r)))
  .f88_key_result(x, "only_horizons", value, pg, layers = lay,
                  missing = unique(unlist(lapply(res[undecided], function(r) r$missing))),
                  flags = cond_flags,
                  notes = c(if (length(present)) sprintf("other diagnostic horizons present: %s", paste(present, collapse = ", ")),
                            if (length(undecided)) sprintf("undecided: %s", paste(undecided, collapse = ", "))),
                  evidence = list(allowed = allowed, present = present, undecided = undecided))
}

# Coarser than sandy loam to `to_cm` at least (the Arenosols, p. 80; the
# coarse textured materials of the Gleysols, p. 78, read with the same test:
# Q10 and soilKey's reading): every mineral layer overlapping 0 to `to_cm`
# is coarser than sandy loam (.fao_coarser_than_sandy_loam()); H and O
# layers are not read; a soil on rock above `to_cm` fails.
coarse_texture_fao88 <- function(pedon, to_cm = 100) {
  x <- .f88_ctx(pedon)
  pg <- "80"
  if (!x$n) return(.f88_key_result(x, "coarse_texture", FALSE, pg))
  rtop <- suppressWarnings(min(x$top[x$rock], na.rm = TRUE))
  if (is.finite(rtop) && rtop < to_cm)
    return(.f88_key_result(x, "coarse_texture", FALSE, pg, notes = sprintf("rock above %s cm", to_cm)))
  org <- .f88_organic_layers(x)$value
  mineral <- !(org %in% TRUE) & !(!is.na(x$master) & grepl("^(H|O)", x$master))
  w <- .fao_in_window(x$h, .fao_coarser_than_sandy_loam(x$h), 0, to_cm, "throughout", subset = mineral)
  .f88_key_result(x, "coarse_texture", w$value, pg, layers = w$layers %||% integer(0),
                  missing = c(w$missing, if (is.na(w$value)) c("clay_pct", "silt_pct")),
                  flags = w$flags, notes = w$note,
                  evidence = list(to_cm = to_cm))
}

# Less than `lt` % coarse fragments in all subhorizons within `to_cm` (the
# Arenosols, p. 80): coarse_fragments_pct not recorded is none, flagged (a
# field observation).
coarse_fragments_fao88 <- function(pedon, lt = 35, to_cm = 100) {
  x <- .f88_ctx(pedon)
  pg <- "80"
  if (!x$n) return(.f88_key_result(x, "coarse_fragments", FALSE, pg))
  cf <- .f88_num(x, "coarse_fragments_pct")
  sel <- !x$rock & !is.na(x$top) & x$top < to_cm
  bad <- which(sel & !is.na(cf) & cf >= lt)
  value <- !length(bad)
  .f88_key_result(x, "coarse_fragments", value, pg, layers = bad,
                  flags = if (any(sel & is.na(cf))) "coarse_fragments_not_recorded_none",
                  evidence = list(lt = lt, to_cm = to_cm))
}

# A mollic A whose moist chroma is `max` or less (or more than `gt`) in
# every layer from the top of the A (the mineral soil surface, as the
# mollic A of 0.9.382 measures it) to `depth` cm below it: the Greyzems,
# Chernozems and Kastanozems (pp. 83-84).
mollic_chroma_fao88 <- function(pedon, max = NULL, gt = NULL, depth = 15) {
  x <- .f88_ctx(pedon)
  pg <- "83-84"
  if (!x$n) return(.f88_key_result(x, "mollic_chroma", FALSE, pg))
  mo <- .f88_status(.f88_hz_dark_a(x, umbric = FALSE))
  a <- .f88_a_run(x)
  if (isFALSE(mo) || is.null(a))
    return(.f88_key_result(x, "mollic_chroma", FALSE, pg, notes = "no mollic A"))
  t0 <- x$top[a$first]
  cm <- .f88_colours(x)$cm
  ok <- if (!is.null(max)) .f88_cmp(cm, "<=", max) else .f88_cmp(cm, ">", gt)
  sel <- !x$rock & !is.na(x$top) & !is.na(x$bot) & x$bot > t0 & x$top < t0 + depth
  v <- ok[sel]
  ch <- if (any(v %in% FALSE)) FALSE else if (length(v) && all(v %in% TRUE)) TRUE else NA
  value <- .fao_and(mo, ch)
  .f88_key_result(x, "mollic_chroma", value, pg, layers = which(sel),
                  missing = c(if (is.na(ch)) "munsell_chroma_moist", if (is.na(mo)) .f88_hz_dark_a(x, FALSE)$missing),
                  evidence = list(mollic_a = mo, chroma = ch, max = max, gt = gt, depth = depth))
}

# Uncoated silt and sand grains on structural ped surfaces (the Greyzems,
# p. 83), in a layer starting within 125 cm (p. 42): bleached_ped_coatings
# as recorded; else uncoated_grains_pct above 0, flagged; else absent (a
# field observation).
uncoated_grains_fao88 <- function(pedon) {
  x <- .f88_ctx(pedon)
  pg <- "83"
  if (!x$n) return(.f88_key_result(x, "uncoated_grains", FALSE, pg))
  b <- .f88_lgl(x, "bleached_ped_coatings")
  ug <- .f88_num(x, "uncoated_grains_pct")
  use <- is.na(b) & !is.na(ug)
  v <- ifelse(!is.na(b), b, ifelse(use, ug > 0, FALSE))
  v[x$rock] <- FALSE
  sel <- !is.na(x$top) & x$top <= 125
  hit <- which(v %in% TRUE & sel)
  .f88_key_result(x, "uncoated_grains", length(hit) > 0L, pg, layers = hit,
                  flags = c(if (any(use[hit])) "uncoated_grains_pct_for_bleached_coatings",
                            if (!length(hit) && all(is.na(b) & is.na(ug))) "bleached_ped_coatings_not_recorded_absent"))
}

# Base saturation (NH4OAc) of `min` % or more throughout `top`-`bottom` cm
# (the Phaeozems, "throughout the upper 125 cm", p. 84), every mineral layer
# read; H and O layers are not read, flagged where present (soilKey's
# reading); a description that stops above the base leaves it undecided.
base_saturation_fao88 <- function(pedon, min = 50, top = 0, bottom = 125) {
  x <- .f88_ctx(pedon)
  pg <- "84"
  if (!x$n) return(.f88_key_result(x, "base_saturation", FALSE, pg))
  bs <- .f88_bs(x)
  org <- !is.na(x$master) & grepl("^(H|O)", x$master)
  w <- .fao_in_window(x$h, .f88_cmp(bs$value, ">=", min), top, bottom, "throughout", subset = !org)
  sel <- w$layers %||% integer(0)
  .f88_key_result(x, "base_saturation", w$value, pg, layers = sel,
                  missing = c(w$missing, if (is.na(w$value)) "bs_pct"),
                  flags = c(w$flags, .f88_flags(bs$flags), if (any(org)) "organic_layers_not_read_for_base_saturation"),
                  notes = w$note, evidence = list(min = min, window = c(top, bottom)))
}

# Plinthosols (p. 81): a horizon 15 cm or more thick with 25 % plinthite or
# more (plinthite_fao88()) within 50 cm; or within 125 cm where it lies
# below an albic E, or below a layer with stagnic properties starting within
# 50 cm, or where the soil shows gleyic properties within 100 cm (soilKey's
# reading of the last clause). "Within" by the rule of 2026-11-09.
plinthosols_gate_fao88 <- function(pedon) {
  x <- .f88_ctx(pedon)
  pg <- "81"
  if (!x$n) return(.f88_key_result(x, "plinthosols_gate", FALSE, pg))
  pl <- plinthite_fao88(pedon, min_pct = 25, min_thickness_cm = 15)
  pv <- .f88_values(pl, x$n)
  a <- .fao_within(x$h, pv, 50)
  runs <- .f88_run_list(x, !(pv %in% FALSE))
  ae <- .f88_member(x, .f88_hz_albic_e(x)$units)
  sg <- .f88_stagnic_v(x)$value
  gl <- gleyic_properties_fao88(pedon, max_top_cm = 100)$passed
  deep <- rep(FALSE, x$n)
  for (r in runs) {
    above <- which(!is.na(x$bot) & x$bot <= r$top + 0.5)
    e_above <- .f88_any(ae[above])
    s_above <- .f88_any(sg[above[!is.na(x$top[above]) & x$top[above] <= 50]])
    if (!length(above)) { e_above <- FALSE; s_above <- FALSE }
    cond <- .fao_or(e_above, s_above, gl)
    deep[r$layers] <- .fao_and(pv[r$layers], cond)
  }
  b <- .fao_within(x$h, deep, 125)
  value <- .fao_or(a$value, b$value)
  .f88_key_result(x, "plinthosols_gate", value, pg,
                  layers = c(a$layers, b$layers),
                  missing = unique(c(pl$missing, a$missing, b$missing)),
                  flags = c(pl$evidence$flags, a$flags, b$flags),
                  evidence = list(within_50 = a$value, within_125_under_e_stagnic_or_gleyic = b$value,
                                  gleyic_within_100 = gl))
}

# Planosols (p. 82): an E horizon (a layer designated E, or an albic E)
# with stagnic properties in at least part of it, abruptly over a slowly
# permeable horizon within 125 cm. The slowly permeable horizon is the layer
# right below the E with an abrupt textural change (p. 34), or a fragipan
# (suffix x) under an abrupt or clear boundary, flagged, or a measured
# ksat_mm_h below 4 under such a boundary, flagged (default 14; the 4 mm/h
# is soilKey's number). The natric and spodic B are excluded in the key file.
planosols_gate_fao88 <- function(pedon) {
  x <- .f88_ctx(pedon)
  pg <- "82"
  if (!x$n) return(.f88_key_result(x, "planosols_gate", FALSE, pg))
  ae <- .f88_member(x, .f88_hz_albic_e(x)$units)
  is_e <- (!is.na(x$master) & grepl("^E", x$master)) | (ae %in% TRUE)
  is_e[x$rock] <- FALSE
  sg <- .f88_stagnic_v(x)$value
  ab <- .f88_abrupt_v(x)$value
  sharp <- .fao_boundary_sharp(x$h)$value
  ks <- .f88_num(x, "ksat_mm_h")
  out <- FALSE; lay <- integer(0); flags <- character(0)
  for (r in .f88_run_list(x, is_e)) {
    k <- max(r$layers) + 1L
    if (k > x$n || x$rock[k] || is.na(x$top[k]) || x$top[k] > 125) next
    stag <- .f88_any(sg[r$layers])
    frag <- !is.na(x$desg[k]) && grepl("^[A-Z]+(/[A-Z]+)?[a-z0-9]*x", x$desg[k]) && sharp[max(r$layers)]
    kslow <- !is.na(ks[k]) && ks[k] < 4 && sharp[max(r$layers)]
    slow <- .fao_or(ab[k], frag, kslow)
    s <- .fao_and(stag, slow)
    if (isTRUE(s)) {
      out <- TRUE; lay <- c(r$layers, k)
      flags <- c(if (frag && !isTRUE(ab[k])) "fragipan_from_designation", if (kslow && !isTRUE(ab[k])) "ksat_below_4mm_h")
      break
    }
    if (is.na(s)) { out <- if (isFALSE(out)) NA else out; lay <- c(lay, r$layers, k) }
  }
  .f88_key_result(x, "planosols_gate", out, pg, layers = lay,
                  missing = c("clay_pct", "rh_value", "mottle_colors"), flags = flags)
}

# Podzoluvisols (p. 85): an argic B whose upper boundary is irregular or
# broken from deep tonguing of the E into it (tonguing_fao88(), p. 41) or
# from iron nodules larger than 2 cm (nodule_size_max_cm) in its upper
# layer or the layer above, their enriched, redder exteriors taken as met,
# flagged (as for the ferric properties, 0.9.383). The boundary topography
# is not required besides (soilKey's reading).
podzoluvisols_gate_fao88 <- function(pedon) {
  x <- .f88_ctx(pedon)
  pg <- "85"
  if (!x$n) return(.f88_key_result(x, "podzoluvisols_gate", FALSE, pg))
  ar <- argic_b_fao88(pedon)
  tg <- tonguing_fao88(pedon, kind = "albic")
  ns <- .f88_num(x, "nodule_size_max_cm")
  L <- .f88_ctx_rows(x, ar)
  nod <- FALSE
  if (length(L)) {
    i <- min(L)
    zone <- c(if (i > 1L) i - 1L, i)
    nod <- any(!is.na(ns[zone]) & ns[zone] > 2)
  }
  value <- .fao_and(ar$passed, .fao_or(tg$passed, nod))
  .f88_key_result(x, "podzoluvisols_gate", value, pg, layers = L,
                  missing = unique(c(ar$missing, tg$missing)),
                  flags = c(tg$evidence$flags, if (nod) "nodule_exteriors_taken_as_met"),
                  evidence = list(argic_b = ar$passed, tonguing = tg$passed, nodules_over_2cm = nod))
}

# Nitisols (p. 86): an argic B; a clay content that does not fall by more
# than 20 % (relative) from its maximum within 150 cm (default 13: no layer
# below the maximum and within 150 cm under 0.8 x the maximum); gradual to
# diffuse boundaries between the A and the B (the lower boundary of every
# layer from the A down to the top of the argic B; boundary_distinctness not
# recorded, absent, as the boundaries of 2026-11-09); nitic properties in
# some subhorizon within 125 cm.
nitisols_gate_fao88 <- function(pedon) {
  x <- .f88_ctx(pedon)
  pg <- "86"
  if (!x$n) return(.f88_key_result(x, "nitisols_gate", FALSE, pg))
  ar <- argic_b_fao88(pedon)
  L <- .f88_ctx_rows(x, ar)
  if (isFALSE(ar$passed) || !length(L))
    return(.f88_key_result(x, "nitisols_gate", if (is.na(ar$passed)) NA else FALSE, pg,
                           missing = ar$missing, notes = "no argic B"))
  clay <- .f88_num(x, "clay_pct")
  w <- !x$rock & !is.na(x$top) & x$top < 150 & seq_len(x$n) >= min(L)
  cw <- clay[w]
  idx <- which(w)
  dec <- if (anyNA(cw) || !length(cw)) NA else {
    k <- which.max(cw)
    !any(cw[k:length(cw)] < 0.8 * cw[k])
  }
  rtop <- suppressWarnings(min(x$top[x$rock], na.rm = TRUE))
  deepest <- suppressWarnings(max(x$bot[!x$rock], na.rm = TRUE))
  open <- !(is.finite(rtop) && rtop <= 150) && is.finite(deepest) && deepest < 150
  if (isTRUE(dec) && open) dec <- NA
  # boundaries from the A down to the argic B
  bd <- .field_term(.fao_col(x$h, "boundary_distinctness", "character"))
  a <- .f88_a_run(x)
  from <- if (is.null(a)) NA_integer_ else a$first
  zone <- if (is.na(from) || min(L) <= from) integer(0) else seq.int(from, min(L) - 1L)
  gd <- !is.na(bd[zone]) & grepl("gradual|diffuse|difusa", bd[zone])
  bnd <- if (!length(zone)) FALSE else all(gd)
  nit <- nitic_properties_fao88(pedon, max_top_cm = 125)
  value <- .fao_and(ar$passed, dec, bnd, nit$passed)
  .f88_key_result(x, "nitisols_gate", value, pg, layers = L,
                  missing = unique(c(ar$missing, if (is.na(dec)) c("clay_pct", "bottom_cm"), nit$missing)),
                  flags = c(if (any(is.na(bd[zone]))) "boundary_distinctness_not_recorded_absent",
                            if (open) "description_ends_above_150cm"),
                  evidence = list(argic_b = ar$passed, clay_no_decrease_20pct = dec,
                                  gradual_boundaries = bnd, nitic = nit$passed))
}

# The layers of the argic B for "the B horizon" of the Alisols, Acrisols,
# Luvisols and Lixisols (pp. 87-88; plan section 4.2): the layers of the
# upper argic B (p. 42), starting within `to_cm`, sure ones first.
.f88_argic_layers <- function(x, to_cm = 125) {
  r <- .f88_hz_argic_natric(x, natric = FALSE)
  u <- Filter(function(u) !is.na(u$top) && u$top <= to_cm, r$units)
  if (!length(u)) return(integer(0))
  sure <- Filter(function(z) isTRUE(z$status), u)
  pick <- if (length(sure)) sure else u
  pick <- pick[[which.min(vapply(pick, function(z) z$top, 0))]]
  L <- pick$layers
  L[!is.na(x$top[L]) & x$top[L] <= to_cm]
}

# The CEC (NH4OAc) per kg clay of the argic B (AL, AC, LV, LX): the
# thickness-weighted mean over its layers starting within `to_cm`, each
# clipped at `to_cm` (soilKey's reading of "an argic B which has a CEC of
# ...": one value for the horizon, so that the four groupings share the
# argic soils without a gap); undecided when a layer lacks CEC or clay.
argic_cec_clay_fao88 <- function(pedon, min = NULL, lt = NULL, to_cm = 125) {
  x <- .f88_ctx(pedon)
  pg <- "87-88"
  if (!x$n) return(.f88_key_result(x, "argic_cec_clay", FALSE, pg))
  L <- .f88_argic_layers(x, to_cm)
  if (!length(L)) return(.f88_key_result(x, "argic_cec_clay", FALSE, pg, notes = "no argic B"))
  cc <- .fao_cec_clay(x$h, "nh4oac", "clay")
  v <- cc$value[L]
  th <- pmax(pmin(x$bot[L], to_cm) - x$top[L], 0)
  mean_v <- if (anyNA(v) || anyNA(th) || sum(th) <= 0) NA_real_ else sum(v * th) / sum(th)
  value <- if (is.na(mean_v)) NA else if (!is.null(min)) round(mean_v, 6) >= min else round(mean_v, 6) < lt
  .f88_key_result(x, "argic_cec_clay", value, pg, layers = L,
                  missing = if (is.na(value)) c("cec_cmol", "clay_pct"),
                  flags = .f88_flags(cc$flags, L),
                  evidence = list(cec_per_kg_clay = mean_v, min = min, lt = lt))
}

# The base saturation (NH4OAc) of the argic B (AL, AC, LV, LX): below `lt`
# % in at least some part of it within `to_cm` (AL, AC), or `min` % or more
# throughout it to `to_cm` (LV, LX), on the layers of .f88_argic_layers().
argic_bs_fao88 <- function(pedon, lt = NULL, min = NULL, to_cm = 125) {
  x <- .f88_ctx(pedon)
  pg <- "87-88"
  if (!x$n) return(.f88_key_result(x, "argic_bs", FALSE, pg))
  L <- .f88_argic_layers(x, to_cm)
  if (!length(L)) return(.f88_key_result(x, "argic_bs", FALSE, pg, notes = "no argic B"))
  bs <- .f88_bs(x)
  v <- bs$value[L]
  value <- if (!is.null(lt)) .f88_any(.f88_cmp(v, "<", lt)) else .f88_all(.f88_cmp(v, ">=", min))
  .f88_key_result(x, "argic_bs", value, pg, layers = L,
                  missing = if (is.na(value)) "bs_pct",
                  flags = .f88_flags(bs$flags, L),
                  evidence = list(bs = v, lt = lt, min = min))
}


# --- the classifier -----------------------------------------------------------------------

#' Classify a pedon in the FAO-Unesco soil legend
#'
#' Keys a \code{\link{PedonRecord}} through the FAO-Unesco legend of the Soil
#' Map of the World. \strong{Experimental}: this release keys the 28 major
#' soil groupings of the Revised Legend (FAO 1988, World Soil Resources
#' Report 60; the text of the ISRIC reprint of 1997, Technical Paper 20),
#' chapter X, pp. 75-89, in the key's order (Histosols, Anthrosols,
#' Leptosols, Vertisols, Fluvisols, Solonchaks, Gleysols, Andosols,
#' Arenosols, Regosols, Podzols, Plinthosols, Ferralsols, Planosols,
#' Solonetz, Greyzems, Chernozems, Kastanozems, Phaeozems, Podzoluvisols,
#' Gypsisols, Calcisols, Nitisols, Alisols, Acrisols, Luvisols, Lixisols,
#' Cambisols), on the 16 diagnostic horizons and 27 diagnostic properties of
#' chapters IV and V read on the data. The soil units, the phases and the
#' Legend of 1974 follow in later releases; \code{edition = "1974"} stops
#' with a message until then.
#'
#' The tests are three-valued: a measurement not recorded leaves its
#' criterion undecided, a field observation not recorded is absent. A
#' grouping tested before the assigned one that the data leave undecided
#' makes the result provisional (\code{provisional_because}). The key has no
#' residual grouping after the Cambisols: a soil that no gate takes is
#' \code{"[Not classified in the FAO-Unesco Revised Legend (1988)]"}, in
#' brackets because the name is soilKey's, with the gates that failed in the
#' warnings; when no gate passes but some are undecided, the name lists them
#' in brackets as the candidates. Depths are read from the soil surface, H
#' and O horizons included. The readings where the text is silent are in
#' \code{system.file("rules", "fao1988", "readings.yaml", package =
#' "soilKey")}.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param edition \code{"1988"} (the Revised Legend, default) or
#'   \code{"1974"} (not yet available).
#' @param rules Optional pre-loaded rule set (\code{load_rules("fao1988")}).
#' @param on_missing One of \code{"warn"} (default), \code{"silent"},
#'        \code{"error"}.
#' @param gapfill Opt-in within-pedon gap-fill, as in
#'        \code{\link{classify_usda}}.
#' @return A \code{\link{ClassificationResult}} with \code{system_id}
#'   \code{"fao1988"}; \code{name} and \code{rsg_or_order} hold the major
#'   soil grouping as printed ("Luvisols"); \code{levels} has its code
#'   ("LV"), name, page of the key and soilKey's Portuguese gloss
#'   (\code{name_pt}); the key trace and \code{provisional_because}.
#' @references FAO (1988). FAO/Unesco Soil Map of the World, Revised Legend,
#'   with corrections and updates. World Soil Resources Report 60, FAO,
#'   Rome. Reprinted with updates as Technical Paper 20, ISRIC, Wageningen,
#'   1997.
#' @seealso \code{\link{classify_fao1988}}, \code{\link{make_fao_example}},
#'   \code{\link{soilkey_systems}}.
#' @examples
#' res <- classify_fao(make_fao_example("LV"), on_missing = "silent")
#' res$name
#' res$levels
#' @export
classify_fao <- function(pedon, edition = c("1988", "1974"), rules = NULL,
                         on_missing = c("warn", "silent", "error"),
                         gapfill = FALSE) {
  edition <- .fao_edition(match.arg(as.character(edition)[1L], c("1988", "1974", "88", "74")))
  on_missing <- match.arg(on_missing)
  if (edition == "1974") {
    rlang::abort(paste(
      "The FAO-Unesco Legend of 1974 is not yet available in soilKey: its key follows in a later release.",
      "Use edition = \"1988\" for the Revised Legend."))
  }
  .classify_fao1988_impl(pedon, rules = rules, on_missing = on_missing, gapfill = gapfill)
}

#' Classify a pedon in the FAO-Unesco Revised Legend (1988)
#'
#' The registry's classifier of \code{fao1988} (aliases \code{fao},
#' \code{fao88}, \code{revised_legend}): \code{classify_fao(pedon, edition =
#' "1988")}. See \code{\link{classify_fao}}.
#'
#' @inheritParams classify_fao
#' @return A \code{\link{ClassificationResult}}, as \code{\link{classify_fao}}.
#' @seealso \code{\link{classify_fao}}, \code{\link{make_fao_example}}.
#' @examples
#' classify_fao1988(make_fao_example("CM"), on_missing = "silent")$name
#' @export
classify_fao1988 <- function(pedon, rules = NULL,
                             on_missing = c("warn", "silent", "error"),
                             gapfill = FALSE) {
  on_missing <- match.arg(on_missing)
  classify_fao(pedon, edition = "1988", rules = rules, on_missing = on_missing, gapfill = gapfill)
}

.FAO88_NOT_CLASSIFIED <- "[Not classified in the FAO-Unesco Revised Legend (1988)]"

.classify_fao1988_impl <- function(pedon, rules = NULL, on_missing = "warn", gapfill = FALSE) {
  rules <- rules %||% load_rules("fao1988")
  res <- classify_hierarchical("fao1988", pedon, rules = rules,
                               on_missing = on_missing, gapfill = gapfill)
  lv <- res$levels
  tr <- res$trace$major_groups %||% list()
  if (is.data.frame(lv) && nrow(lv)) {
    ent <- lapply(seq_len(nrow(lv)), function(i)
      Find(function(z) identical(as.character(z$code), lv$code[i]), rules[[lv$level[i]]]))
    lv$name_pt <- vapply(ent, function(e) as.character(e$name_pt %||% NA_character_), "")
    lv$page <- vapply(ent, function(e) as.character(e$page %||% NA_character_), "")
    res$levels <- lv
    return(res)
  }
  # nothing passed: no residual grouping in the key (Q5)
  st <- vapply(tr, function(e) { p <- e$passed; if (length(p) == 1L) as.logical(p) else NA }, NA)
  und <- names(st)[is.na(st)]
  nm <- .F88_GROUPINGS$name[match(und, .F88_GROUPINGS$code)]
  # drop the engine's generic warning; ours says it in the key's terms
  res$warnings <- res$warnings[!grepl("^No class of the major_groups key passed|^Provisional: [0-9]+ class", res$warnings)]
  if (length(und)) {
    res$name <- sprintf("[Undecided in the FAO-Unesco Revised Legend (1988): %s, or not classified]",
                        paste(sprintf("%s %s", und, nm), collapse = ", "))
    res$warnings <- c(res$warnings, sprintf(paste(
      "No major soil grouping passed; the data leave %s undecided (the candidates).",
      "The key has no residual grouping after the Cambisols (Hugo's decision Q5): if they all fail,",
      "the soil is not classified in the 1988 legend."), paste(und, collapse = ", ")))
  } else {
    res$name <- .FAO88_NOT_CLASSIFIED
    res$warnings <- c(res$warnings, sprintf(paste(
      "Not classified: none of the %d gates of the 1988 key passed (%s), and the key has no",
      "residual grouping after the Cambisols (Hugo's decision Q5); see $trace for the test that",
      "failed in each gate."), length(st), paste(names(st), collapse = ", ")))
  }
  res
}


# --- examples ------------------------------------------------------------------------------

#' Example pedons for the FAO-Unesco legend
#'
#' Small synthetic profiles written for the tests and examples, one per
#' major soil grouping of the Revised Legend (FAO 1988), built from the
#' key's numbers (no profile of the book is copied). Each keys to its own
#' grouping without an undecided gate before it. With \code{near_miss =
#' TRUE}, the same profile with one printed number of its gate crossed (a
#' Luvisol with a base saturation of 45 \% in its lower B, a Histosol with
#' 35 cm of peat), which keys elsewhere. They are not real profiles.
#'
#' @param code The code of a major soil grouping of 1988 ("HS", "AT", "LP",
#'   "VR", "FL", "SC", "GL", "AN", "AR", "RG", "PZ", "PT", "FR", "PL", "SN",
#'   "GR", "CH", "KS", "PH", "PD", "GY", "CL", "NT", "AL", "AC", "LV", "LX",
#'   "CM").
#' @param edition \code{"1988"}; the examples of 1974 follow with its key.
#' @param near_miss \code{TRUE} for the near miss of the grouping.
#' @return A \code{\link{PedonRecord}}.
#' @seealso \code{\link{classify_fao}}.
#' @examples
#' p <- make_fao_example("PH")
#' p$horizons[, c("designation", "top_cm", "bottom_cm", "bs_pct")]
#' @export
make_fao_example <- function(code = c("HS", "AT", "LP", "VR", "FL", "SC", "GL", "AN", "AR", "RG", "PZ", "PT", "FR", "PL",
                                      "SN", "GR", "CH", "KS", "PH", "PD", "GY", "CL", "NT", "AL", "AC", "LV",
                                      "LX", "CM"),
                             edition = c("1988", "1974"), near_miss = FALSE) {
  code <- match.arg(code)
  edition <- .fao_edition(match.arg(as.character(edition)[1L], c("1988", "1974")))
  if (edition == "1974")
    rlang::abort("The examples of the FAO-Unesco Legend of 1974 follow with its key in a later release.")
  .fao88_example(code, isTRUE(near_miss))
}

# The registry's example function for the app (make_<id>_example).
make_fao1988_example <- function(code, near_miss = FALSE) {
  make_fao_example(code, "1988", near_miss)
}

.fao88_example <- function(code, nm) {
  site <- list(id = paste0("fao1988-", code, if (nm) "-near-miss"), country = "XX",
               designation_system = "fao2006", drainage_class = "well drained",
               parent_material = "granite")
  # what the earlier gates read, so that they are decided: no salts, no
  # gypsum, no carbonate, no coarse fragments, a full base status, no
  # spodic chemistry
  base <- list(ec_dS_m = 0.4, caso4_pct = 0, caco3_pct = 0, coarse_fragments_pct = 0,
               fe_pyro_pct = 0.05, al_pyro_pct = 0.05, fe_dcb_pct = 1.5, al_dcb_pct = 0.3,
               al_ox_pct = 0.1, fe_ox_pct = 0.2, munsell_hue_moist = "10YR",
               structure_grade = "moderate", structure_type = "subangular blocky",
               boundary_distinctness = "clear", ph_h2o = 6.5, na_cmol = 0.1)
  mk <- function(h, s = list()) {
    h <- utils::modifyList(base, h)
    n <- max(lengths(h))
    h <- lapply(h, function(v) if (length(v) == 1L) rep(v, n) else v)
    PedonRecord$new(site = utils::modifyList(site, s), horizons = ensure_horizon_schema(
      data.table::as.data.table(data.frame(h, stringsAsFactors = FALSE))))
  }
  # a loamy soil with an argic B, set per grouping
  argic <- function(cec, bs, bs_low = bs, extra = list()) {
    utils::modifyList(list(
      designation = c("A", "E", "Bt1", "Bt2", "BC"), top_cm = c(0, 15, 35, 70, 110),
      bottom_cm = c(15, 35, 70, 110, 160),
      clay_pct = c(15, 14, 32, 34, 28), silt_pct = c(35, 36, 30, 28, 30), sand_pct = c(50, 50, 38, 38, 42),
      oc_pct = c(1.2, 0.5, 0.4, 0.3, 0.2), cec_cmol = c(8, 6, cec * 0.32, cec * 0.34, cec * 0.28),
      bs_pct = c(bs, bs, bs, bs_low, bs_low), clay_films_amount = c(NA, NA, "common", "common", "few"),
      munsell_value_moist = c(4, 5, 4, 4, 5), munsell_chroma_moist = c(3, 3, 4, 4, 4),
      munsell_value_dry = c(6, 7, 6, 6, 6), munsell_chroma_dry = c(3, 3, 4, 4, 4),
      ph_h2o = if (bs < 50) 5.0 else 6.5), extra)
  }
  switch(code,
    HS = mk(list(
      designation = c("H1", "H2", "Cg"), top_cm = c(0, 25, if (nm) 35 else 45),
      bottom_cm = c(25, if (nm) 35 else 45, 130),
      oc_pct = c(35, 30, 1), clay_pct = c(NA, NA, 30), silt_pct = c(NA, NA, 40), sand_pct = c(NA, NA, 30),
      bulk_density_g_cm3 = c(0.2, 0.25, 1.4), bs_pct = c(40, 40, 60), cec_cmol = c(80, 70, 15),
      munsell_hue_moist = c("10YR", "10YR", "5GY"), munsell_value_moist = c(2, 2, 5),
      munsell_chroma_moist = c(1, 1, 1), ph_h2o = c(4.5, 4.6, 6.0),
      structure_grade = c(NA, NA, "structureless"), structure_type = c(NA, NA, "massive")),
      s = list(drainage_class = "very poorly drained")),
    AT = mk(list(
      designation = c("A", "C", "2Bw", "2C"), top_cm = c(0, 25, if (nm) 45 else 60, 100),
      bottom_cm = c(25, if (nm) 45 else 60, 100, 140),
      layer_origin = c("urban fill", "urban fill", NA, NA), artefacts_pct = c(25, 30, 0, 0),
      clay_pct = c(18, 18, 22, 20), silt_pct = c(40, 40, 40, 40), sand_pct = c(42, 42, 38, 40),
      oc_pct = c(1.5, 0.6, 0.4, 0.2), cec_cmol = c(12, 10, 10, 9), bs_pct = c(80, 80, 75, 75),
      munsell_value_moist = c(4, 4, 4, 5), munsell_chroma_moist = c(2, 2, 5, 4),
      munsell_hue_moist = c("10YR", "10YR", "7.5YR", "10YR"),
      structure_grade = c("weak", "structureless", "moderate", "structureless"),
      structure_type = c("granular", "massive", "subangular blocky", "massive"))),
    LP = mk(list(
      designation = c("A", "C", "R"), top_cm = c(0, 15, if (nm) 32 else 28),
      bottom_cm = c(15, if (nm) 32 else 28, 60),
      clay_pct = c(18, 16, NA), silt_pct = c(40, 40, NA), sand_pct = c(42, 44, NA),
      oc_pct = c(1.5, 0.4, NA), cec_cmol = c(12, 9, NA), bs_pct = c(80, 85, NA),
      munsell_value_moist = c(4, 5, NA), munsell_chroma_moist = c(3, 4, NA),
      structure_grade = c("weak", "structureless", NA), structure_type = c("granular", "massive", NA))),
    VR = mk(list(
      designation = c("A", "Bss1", "Bss2", "C"), top_cm = c(0, 20, 60, 110), bottom_cm = c(20, 60, 110, 150),
      clay_pct = c(if (nm) 27 else 45, 50, 52, 48), silt_pct = c(30, 28, 28, 30),
      sand_pct = c(if (nm) 43 else 25, 22, 20, 22),
      oc_pct = c(1.4, 0.8, 0.5, 0.3), cec_cmol = c(40, 42, 44, 40), bs_pct = c(90, 92, 95, 95),
      slickensides = c(NA, "many", "common", NA), cracks_width_cm = c(2, 1.5, NA, NA),
      cracks_depth_cm = c(70, 70, NA, NA), ph_h2o = 7.0,
      munsell_value_moist = c(3, 3, 4, 5), munsell_chroma_moist = c(2, 2, 2, 3),
      structure_grade = c("strong", "strong", "moderate", "weak"),
      structure_type = c("granular", "wedge-shaped", "angular blocky", "massive"))),
    FL = mk(list(
      designation = c("A", "C1", "C2", "C3", "C4"), top_cm = c(0, 15, 40, 70, 100),
      bottom_cm = c(15, 40, 70, 100, 140),
      clay_pct = c(20, 12, 25, 10, 22), silt_pct = c(50, 30, 50, 25, 50), sand_pct = c(30, 58, 25, 65, 28),
      oc_pct = c(1.5, 0.3, if (nm) 0.3 else 0.9, 0.15, if (nm) 0.1 else 0.6),
      stratification_pattern = if (nm) NA else c(NA, "stratified", "stratified", "stratified", "stratified"),
      cec_cmol = c(14, 8, 16, 6, 14), bs_pct = c(85, 85, 90, 90, 90),
      munsell_value_moist = c(4, 5, 4, 5, 4), munsell_chroma_moist = c(3, 4, 3, 4, 3),
      structure_grade = c("weak", "structureless", "structureless", "structureless", "structureless"),
      structure_type = c("granular", "massive", "massive", "single grain", "massive")),
      s = list(parent_material = "alluvium")),
    SC = mk(list(
      designation = c("Az", "Bw", "C"), top_cm = c(0, 20, 60), bottom_cm = c(20, 60, 130),
      ec_dS_m = c(if (nm) 14 else 20, 12, 8), clay_pct = c(22, 24, 20), silt_pct = c(45, 45, 45),
      sand_pct = c(33, 31, 35), oc_pct = c(0.8, 0.4, 0.2), cec_cmol = c(15, 16, 14),
      bs_pct = c(100, 100, 100), ph_h2o = c(8.0, 8.0, 8.1),
      munsell_value_moist = c(5, 5, 6), munsell_chroma_moist = c(3, 5, 4),
      munsell_hue_moist = c("10YR", "7.5YR", "10YR"),
      structure_grade = c("weak", "moderate", "structureless"),
      structure_type = c("platy", "subangular blocky", "massive"))),
    GL = mk(list(
      designation = c("A", "Bw", "Cg"), top_cm = c(0, 20, if (nm) 55 else 45),
      bottom_cm = c(20, if (nm) 55 else 45, 130),
      clay_pct = c(25, 28, 30), silt_pct = c(45, 42, 40), sand_pct = c(30, 30, 30),
      oc_pct = c(1.5, 0.5, 0.3), cec_cmol = c(18, 18, 19), bs_pct = c(70, 70, 75),
      munsell_hue_moist = c("10YR", "7.5YR", "5GY"), munsell_value_moist = c(4, 4, 5),
      munsell_chroma_moist = c(2, 5, 1),
      structure_grade = c("weak", "moderate", "structureless"),
      structure_type = c("granular", "subangular blocky", "massive"))),
    AN = mk(list(
      designation = c("A", "Bw", "C"), top_cm = c(0, 25, if (nm) 30 else 60),
      bottom_cm = c(25, if (nm) 30 else 60, 130),
      clay_pct = c(15, 14, 10), silt_pct = c(45, 45, 40), sand_pct = c(40, 41, 50),
      oc_pct = c(6, 2, 0.5), al_ox_pct = c(3, 3, if (nm) 0.3 else 2.5),
      fe_ox_pct = c(1.5, 1.5, if (nm) 0.2 else 1.2), bulk_density_g_cm3 = c(0.7, 0.75, if (nm) 1.2 else 0.8),
      phosphate_retention_pct = c(92, 90, if (nm) 20 else 88), volcanic_glass_pct = c(10, 10, if (nm) 2 else 10), cec_cmol = c(30, 25, 15), bs_pct = c(30, 25, 25),
      ph_h2o = 5.5, munsell_value_moist = c(2, 3, 4), munsell_chroma_moist = c(2, 3, 4),
      munsell_value_dry = c(3, 4, 5), munsell_chroma_dry = c(2, 3, 4),
      structure_grade = c("moderate", "weak", "structureless"),
      structure_type = c("granular", "subangular blocky", "massive")),
      s = list(parent_material = "volcanic ash")),
    AR = mk(list(
      designation = c("A", "C1", "C2"), top_cm = c(0, 20, 80), bottom_cm = c(20, 80, 150),
      clay_pct = c(4, 3, if (nm) 12 else 3), silt_pct = c(6, 5, if (nm) 18 else 4),
      sand_pct = c(90, 92, if (nm) 70 else 93),
      oc_pct = c(0.6, 0.2, 0.1), cec_cmol = c(3, 2, 2), bs_pct = c(60, 60, 60),
      munsell_value_moist = c(4, 5, 6), munsell_chroma_moist = c(3, 4, 4),
      structure_grade = c("weak", "structureless", "structureless"),
      structure_type = c("granular", "single grain", "single grain"))),
    RG = mk(list(
      designation = c("A", "C1", "C2"), top_cm = c(0, 15, 60), bottom_cm = c(15, 60, 130),
      clay_pct = c(18, 18, 17), silt_pct = c(40, 40, 40), sand_pct = c(42, 42, 43),
      oc_pct = c(0.9, 0.3, 0.2), cec_cmol = c(10, 9, 9), bs_pct = c(80, 85, 85),
      caco3_pct = c(0, 3, 4), carbonate_soft_pct = c(0, if (nm) 6 else 1, if (nm) 6 else 1),
      ph_h2o = c(7.0, 7.8, 8.0),
      munsell_value_moist = c(4, 5, 5), munsell_chroma_moist = c(3, 4, 4),
      structure_grade = c("weak", "structureless", "structureless"),
      structure_type = c("granular", "massive", "massive"))),
    PZ = mk(list(
      designation = c("A", "E", "Bhs", "Bs", "C"), top_cm = c(0, 10, 30, 45, 80),
      bottom_cm = c(10, 30, 45, 80, 130),
      clay_pct = c(4, 3, 6, 5, 3), silt_pct = c(8, 7, 8, 8, 5), sand_pct = c(88, 90, 86, 87, 92),
      oc_pct = c(2, 0.4, 2.5, 1.0, 0.2), fe_pyro_pct = c(0.05, 0.02, 0.8, 0.6, 0.05),
      al_pyro_pct = c(0.05, 0.02, 0.8, 0.6, 0.05), fe_dcb_pct = c(0.2, 0.1, 1.0, 0.8, 0.2),
      al_dcb_pct = c(0.1, 0.05, 0.8, 0.6, 0.1),
      cec_ph82_cmol = c(8, 1, if (nm) 0.2 else 6, if (nm) 0.15 else 3, 0.5),
      cec_cmol = c(6, 1, 5, 3, 0.5), bs_pct = c(20, 15, 10, 15, 20), ph_h2o = c(4.2, 4.3, 4.5, 4.8, 5.0),
      munsell_hue_moist = c("10YR", "10YR", "5YR", "7.5YR", "10YR"),
      munsell_value_moist = c(3, 6, 3, 4, 6), munsell_chroma_moist = c(1, 2, 3, 6, 4),
      munsell_value_dry = c(4, 7, 4, 5, 7), munsell_chroma_dry = c(1, 2, 3, 6, 4),
      structure_grade = c("weak", "structureless", "weak", "weak", "structureless"),
      structure_type = c("granular", "single grain", "subangular blocky", "subangular blocky", "single grain"))),
    PT = mk(list(
      designation = c("A", "Bv1", "Bv2", "C"), top_cm = c(0, 20, 50, 100), bottom_cm = c(20, 50, 100, 150),
      clay_pct = c(25, 35, 38, 35), silt_pct = c(25, 20, 18, 20), sand_pct = c(50, 45, 44, 45),
      oc_pct = c(1.5, 0.5, 0.3, 0.2), plinthite_pct = c(0, if (nm) 20 else 35, if (nm) 20 else 40, 10),
      cec_cmol = c(6, 6, 6, 5), bs_pct = c(30, 25, 20, 20), ph_h2o = 5.0,
      munsell_hue_moist = c("10YR", "2.5YR", "2.5YR", "5YR"), munsell_value_moist = c(4, 5, 5, 5),
      munsell_chroma_moist = c(3, 6, 6, 6), mottle_colors = c(NA, "10R 4/8", "10R 4/8", NA),
      redoximorphic_features_pct = c(0, 35, 40, 10),
      structure_grade = c("weak", "weak", "structureless", "structureless"),
      structure_type = c("granular", "subangular blocky", "massive", "massive"))),
    FR = mk(list(
      designation = c("A", "Bo1", "Bo2", "Bo3"), top_cm = c(0, 20, 60, 110), bottom_cm = c(20, 60, 110, 180),
      clay_pct = c(45, 55, 58, 58), silt_pct = c(8, 7, 6, 6), sand_pct = c(47, 38, 36, 36),
      oc_pct = c(2, 0.8, 0.5, 0.3), cec_cmol = c(8, if (nm) 9.6 else 6, if (nm) 10 else 5.5, if (nm) 10 else 5),
      bs_pct = c(20, 15, 12, 10), exch_acidity_kcl_cmol = c(1, if (nm) 2 else 0.8, if (nm) 2 else 0.6, if (nm) 2 else 0.5),
      ca_cmol = c(1, if (nm) 4 else 0.5, if (nm) 4 else 0.4, if (nm) 4 else 0.3),
      mg_cmol = c(0.4, if (nm) 1.5 else 0.3, if (nm) 1.5 else 0.2, if (nm) 1.5 else 0.2), k_cmol = c(0.1, 0.1, 0.1, 0.1),
      na_cmol = c(0.02, 0.02, 0.02, 0.02),
      weatherable_minerals_pct = 1, water_dispersible_clay_pct = c(20, 2, 1, 1), rock_structure_pct = 0,
      ph_h2o = 4.8, munsell_hue_moist = c("5YR", "2.5YR", "2.5YR", "2.5YR"),
      munsell_value_moist = c(3, 3, 3, 3), munsell_chroma_moist = c(3, 6, 6, 6),
      boundary_distinctness = c("clear", "diffuse", "diffuse", NA),
      structure_grade = c("moderate", "weak", "weak", "weak"),
      structure_type = c("granular", "granular", "granular", "granular"))),
    PL = mk(list(
      designation = c("A", "Eg", "Btg", "C"), top_cm = c(0, 15, 40, 90), bottom_cm = c(15, 40, 90, 140),
      clay_pct = c(15, 12, if (nm) 22 else 40, 30), silt_pct = c(40, 40, 30, 35), sand_pct = c(45, 48, if (nm) 48 else 30, 35),
      oc_pct = c(1.2, 0.4, 0.4, 0.2), cec_cmol = c(8, 5, if (nm) 7 else 14, 10), bs_pct = c(60, 55, 70, 75),
      munsell_hue_moist = c("10YR", "10YR", "10YR", "10YR"), munsell_value_moist = c(4, 6, 5, 5),
      munsell_chroma_moist = c(2, 1, 2, 3), munsell_value_dry = c(5, 7, 6, 6), munsell_chroma_dry = c(2, 1, 2, 3),
      mottle_colors = c(NA, "7.5YR 5/6", "7.5YR 5/8", NA), redoximorphic_features_pct = c(0, 10, 15, 0),
      clay_films_amount = c(NA, NA, "common", NA), rh_value = c(NA, 17, NA, NA),
      boundary_distinctness = c("clear", "abrupt", "gradual", NA),
      structure_grade = c("weak", "weak", "strong", "structureless"),
      structure_type = c("granular", "platy", "prismatic", "massive"))),
    SN = mk(list(
      designation = c("A", "E", "Btn", "BCn", "C"), top_cm = c(0, 10, 20, 60, 100),
      bottom_cm = c(10, 20, 60, 100, 140),
      clay_pct = c(15, 13, 35, 30, 25), silt_pct = c(40, 42, 35, 38, 40), sand_pct = c(45, 45, 30, 32, 35),
      oc_pct = c(1.5, 0.6, 0.6, 0.3, 0.2), cec_cmol = c(10, 8, 20, 18, 15), bs_pct = c(80, 85, 100, 100, 100),
      esp_pct = c(3, 5, if (nm) 14 else 25, if (nm) 12 else 20, 15), ph_h2o = c(7.0, 7.5, 8.8, 9.0, 8.8),
      clay_films_amount = c(NA, NA, "common", "few", NA),
      munsell_value_moist = c(4, 5, 4, 5, 5), munsell_chroma_moist = c(2, 2, 3, 3, 4),
      structure_grade = c("weak", "weak", "strong", "moderate", "structureless"),
      structure_type = c("granular", "platy", "columnar", "prismatic", "massive"))),
    GR = mk(list(
      designation = c("A1", "A2", "Bt", "BC", "C"), top_cm = c(0, 15, 35, 80, 110),
      bottom_cm = c(15, 35, 80, 110, 150),
      clay_pct = c(20, 21, 32, 28, 24), silt_pct = c(55, 55, 48, 50, 52), sand_pct = c(25, 24, 20, 22, 24),
      oc_pct = c(3.5, 2, 0.6, 0.4, 0.2), cec_cmol = c(30, 26, 26, 22, 18), bs_pct = c(75, 70, 70, 75, 80),
      bleached_ped_coatings = c(FALSE, TRUE, TRUE, FALSE, FALSE), clay_films_amount = c(NA, NA, "common", "few", NA),
      munsell_value_moist = c(2, 3, 4, 4, 5), munsell_chroma_moist = c(if (nm) 3 else 1, if (nm) 3 else 2, 3, 3, 4),
      munsell_value_dry = c(4, 5, 6, 6, 6), munsell_chroma_dry = c(2, 2, 3, 3, 4), ph_h2o = 6.2,
      structure_grade = c("strong", "moderate", "moderate", "weak", "structureless"),
      structure_type = c("granular", "subangular blocky", "subangular blocky", "subangular blocky", "massive"))),
    CH = mk(list(
      designation = c("A1", "A2", "Bk", "Ck"), top_cm = c(0, 25, 50, 90), bottom_cm = c(25, 50, 90, 150),
      clay_pct = c(25, 25, 24, 22), silt_pct = c(55, 55, 56, 58), sand_pct = c(20, 20, 20, 20),
      oc_pct = c(3, 1.8, 0.6, 0.3), cec_cmol = c(35, 30, 24, 20), bs_pct = c(95, 98, 100, 100),
      caco3_pct = c(0, 1, 22, 12), ph_h2o = c(7.0, 7.3, 8.0, 8.2),
      munsell_value_moist = c(2, 3, 5, 6), munsell_chroma_moist = c(if (nm) 3 else 1, if (nm) 3 else 2, 3, 3),
      munsell_value_dry = c(4, 5, 7, 7), munsell_chroma_dry = c(2, 2, 3, 3),
      structure_grade = c("strong", "moderate", "weak", "structureless"),
      structure_type = c("granular", "granular", "subangular blocky", "massive"))),
    KS = mk(list(
      designation = c("A1", "A2", "Bk", "Ck"), top_cm = c(0, 20, 40, 90), bottom_cm = c(20, 40, 90, 150),
      clay_pct = c(22, 22, 22, 20), silt_pct = c(50, 50, 52, 54), sand_pct = c(28, 28, 26, 26),
      oc_pct = c(1.8, 1.0, 0.4, 0.2), cec_cmol = c(25, 22, 18, 15), bs_pct = c(95, 98, 100, 100),
      caco3_pct = c(0, 1, if (nm) 14 else 20, if (nm) 10 else 12), ph_h2o = c(7.2, 7.5, 8.1, 8.3),
      munsell_value_moist = c(3, 3, 5, 6), munsell_chroma_moist = c(3, 3, 3, 4),
      munsell_value_dry = c(5, 5, 7, 7), munsell_chroma_dry = c(3, 3, 3, 4),
      structure_grade = c("moderate", "moderate", "weak", "structureless"),
      structure_type = c("granular", "subangular blocky", "subangular blocky", "massive"))),
    PH = mk(list(
      designation = c("A1", "A2", "Bw", "C"), top_cm = c(0, 20, 40, 90), bottom_cm = c(20, 40, 90, 140),
      clay_pct = c(24, 25, 27, 25), silt_pct = c(45, 45, 43, 45), sand_pct = c(31, 30, 30, 30),
      oc_pct = c(3, 1.8, 0.5, 0.2), cec_cmol = c(28, 25, 20, 17),
      bs_pct = c(70, 65, 60, if (nm) 45 else 60), ph_h2o = 6.0,
      munsell_hue_moist = c("10YR", "10YR", "7.5YR", "10YR"),
      munsell_value_moist = c(2, 3, 4, 5), munsell_chroma_moist = c(2, 2, 5, 4),
      munsell_value_dry = c(4, 5, 6, 6), munsell_chroma_dry = c(2, 2, 5, 4),
      structure_grade = c("strong", "moderate", "moderate", "structureless"),
      structure_type = c("granular", "subangular blocky", "subangular blocky", "massive"))),
    PD = mk(argic(30, 60, extra = list(
      tongue_vol_pct = c(NA, NA, if (nm) 14 else 25, NA, NA), tongue_width_mm = c(NA, NA, 30, NA, NA),
      tongue_depth_cm = c(NA, NA, 30, NA, NA)))),
    GY = mk(list(
      designation = c("A", "By", "C"), top_cm = c(0, 20, 70), bottom_cm = c(20, 70, 130),
      clay_pct = c(18, 15, 14), silt_pct = c(40, 40, 40), sand_pct = c(42, 45, 46),
      oc_pct = c(0.8, 0.3, 0.1), caso4_pct = c(2, if (nm) 4 else 25, if (nm) 2 else 8),
      ec_dS_m = c(2.5, 3, 3), cec_cmol = c(10, 8, 8), bs_pct = c(100, 100, 100), ph_h2o = c(7.5, 7.6, 7.6),
      caco3_pct = c(3, 3, 3),
      munsell_value_moist = c(5, 6, 6), munsell_chroma_moist = c(3, 3, 3),
      structure_grade = c("weak", "weak", "structureless"),
      structure_type = c("granular", "subangular blocky", "massive"))),
    CL = mk(list(
      designation = c("A", "Bk", "Ck"), top_cm = c(0, 20, 70), bottom_cm = c(20, 70, 130),
      clay_pct = c(20, 20, 18), silt_pct = c(45, 45, 45), sand_pct = c(35, 35, 37),
      oc_pct = c(0.9, 0.4, 0.2), caco3_pct = c(5, if (nm) 14 else 25, if (nm) 12 else 15),
      cec_cmol = c(14, 12, 10), bs_pct = c(100, 100, 100), ph_h2o = c(8.0, 8.2, 8.3),
      munsell_value_moist = c(5, 6, 6), munsell_chroma_moist = c(3, 3, 3),
      structure_grade = c("weak", "weak", "structureless"),
      structure_type = c("granular", "subangular blocky", "massive"))),
    NT = mk(list(
      designation = c("A", "AB", "Bt1", "Bt2", "Bt3"), top_cm = c(0, 15, 30, 70, 120),
      bottom_cm = c(15, 30, 70, 120, 170),
      clay_pct = c(40, 48, 60, 62, if (nm) 45 else 58), silt_pct = c(20, 18, 15, 14, 16),
      sand_pct = c(40, 34, 25, 24, if (nm) 39 else 26),
      oc_pct = c(2, 1.2, 0.6, 0.4, 0.2), cec_cmol = c(12, 12, 12, 12, 11), bs_pct = c(40, 35, 30, 30, 30),
      fe_dcb_pct = 9, fe_ox_pct = c(0.8, 0.7, 0.6, 0.5, 0.4), ph_h2o = 5.2,
      clay_films_amount = c(NA, NA, "many", "many", "common"), shiny_ped_faces = c(NA, NA, "many", "many", "common"),
      boundary_distinctness = c("gradual", "gradual", "diffuse", "diffuse", NA),
      munsell_hue_moist = c("2.5YR", "2.5YR", "10R", "10R", "10R"),
      munsell_value_moist = c(3, 3, 3, 3, 3), munsell_chroma_moist = c(4, 4, 6, 6, 6),
      structure_grade = c("moderate", "strong", "strong", "strong", "moderate"),
      structure_type = c("granular", "angular blocky", "polyhedral", "polyhedral", "polyhedral"))),
    AL = mk(argic(if (nm) 22 else 30, 30)),
    AC = mk(argic(16, if (nm) 55 else 30)),
    LV = mk(argic(30, 60, bs_low = if (nm) 45 else 60)),
    LX = mk(argic(if (nm) 25 else 16, 60)),
    CM = mk(list(
      designation = c("A", "Bw", "C"), top_cm = c(0, 20, if (nm) 34 else 60),
      bottom_cm = c(20, if (nm) 34 else 60, 130),
      clay_pct = c(20, 22, 20), silt_pct = c(40, 40, 40), sand_pct = c(40, 38, 40),
      oc_pct = c(1.2, 0.5, 0.2), cec_cmol = c(12, 12, 11), bs_pct = c(70, 70, 75),
      munsell_hue_moist = c("10YR", "7.5YR", "10YR"),
      munsell_value_moist = c(4, 4, 5), munsell_chroma_moist = c(3, 6, 4),
      structure_grade = c("weak", "moderate", "structureless"),
      structure_type = c("granular", "subangular blocky", "massive")))
  )
}
