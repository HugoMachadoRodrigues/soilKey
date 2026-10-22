# =============================================================================
# v0.9.275 -- a scenario for the NPDB records that lack what the Canadian key
# asks of the Chernozems: the dry colour of the A horizons, estimated from the
# moist colour by a model fitted on the NPDB itself, and the soil climate,
# estimated from the climate normals of Agriculture and Agri-Food Canada at
# each pedon. Each estimate is a range; benchmark_npdb() classifies a pedon at
# the ends of its ranges and counts a class as decided only where they agree.
# =============================================================================

.AAFC_NORMALS_URL <- "https://agriculture.canada.ca/imagery-images/rest/services/climate_normals/ImageServer"

# The variables the scenario reads: monthly mean air temperature (all
# months), monthly minimum and maximum temperature and precipitation (April
# to October, the growing season of the AAFC normals).
.aafc_normals_vars <- function() {
  c(sprintf("ta_m%02d", 1:12), sprintf("tn_m%02d", 4:10), sprintf("tx_m%02d", 4:10),
    sprintf("ac_m%02d", 4:10))
}

#' Climate normals of Agriculture and Agri-Food Canada at given points
#'
#' Queries the "Derived Normal Climate Data" of Agriculture and Agri-Food
#' Canada (30-year normals on a 5 km grid, Open Government Licence - Canada)
#' at each point: the monthly mean air temperature of every month, and the
#' monthly minimum and maximum temperature and the precipitation from April
#' to October (the growing season of these normals). The values come from the
#' AAFC map service (one request per variable and per thousand points); no
#' file is downloaded. Needs the \pkg{httr} and \pkg{jsonlite} packages and
#' an internet connection.
#'
#' @param lat,lon Decimal degrees (WGS 84).
#' @param period One of \code{"1961-1990"}, \code{"1971-2000"},
#'   \code{"1981-2010"}, \code{"1991-2020"}.
#' @param id Optional identifiers, returned as the first column.
#' @param verbose Print progress.
#' @return A data frame with \code{id}, \code{lat}, \code{lon} and one column
#'   per variable (\code{ta_m01} to \code{ta_m12}, \code{tn_m04} to
#'   \code{tn_m10}, \code{tx_m04} to \code{tx_m10}, \code{ac_m04} to
#'   \code{ac_m10}; degrees Celsius and mm), with the period and the source
#'   as attributes. Points outside the grid get \code{NA}.
#' @references Agriculture and Agri-Food Canada. Derived Normal Climate Data.
#'   Open Government Portal (dataset 3a060b8f-e662-4a60-b297-3bed859ffc8a),
#'   Open Government Licence - Canada.
#' @seealso \code{\link{npdb_soil_climate}}, \code{\link{npdb_scenario}}
#' @examples
#' \dontrun{
#' nm <- fetch_aafc_normals(lat = 52.13, lon = -106.67)
#' npdb_soil_climate(nm)
#' }
#' @export
fetch_aafc_normals <- function(lat, lon, period = "1961-1990", id = NULL, verbose = TRUE) {
  period <- match.arg(period, c("1961-1990", "1971-2000", "1981-2010", "1991-2020"))
  for (pkg in c("httr", "jsonlite"))
    if (!requireNamespace(pkg, quietly = TRUE))
      stop("fetch_aafc_normals() needs the '", pkg, "' package.", call. = FALSE)
  lat <- as.numeric(lat); lon <- as.numeric(lon)
  if (length(lat) != length(lon)) stop("fetch_aafc_normals(): `lat` and `lon` differ in length.",
                                       call. = FALSE)
  if (is.null(id)) id <- seq_along(lat)
  get_json <- function(resp) jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"))
  q <- httr::GET(paste0(.AAFC_NORMALS_URL, "/query"), httr::timeout(120),
                 query = list(where = sprintf("tYears='%s'", period),
                              outFields = "OBJECTID,tType", returnGeometry = "false",
                              resultRecordCount = 2000, f = "json"))
  httr::stop_for_status(q)
  att <- get_json(q)$features$attributes
  ids <- stats::setNames(att$OBJECTID, att$tType)
  vars <- .aafc_normals_vars()
  if (!all(vars %in% names(ids)))
    stop("fetch_aafc_normals(): the service no longer lists ",
         paste(setdiff(vars, names(ids)), collapse = ", "), call. = FALSE)
  ok <- !is.na(lat) & !is.na(lon)
  out <- matrix(NA_real_, length(lat), length(vars), dimnames = list(NULL, vars))
  idx <- which(ok)
  batches <- split(idx, ceiling(seq_along(idx) / 1000))
  sample_one <- function(rows, oid) {
    g <- list(points = lapply(rows, function(i) c(lon[i], lat[i])),
              spatialReference = list(wkid = 4326))
    for (attempt in 1:4) {
      r <- tryCatch(httr::POST(paste0(.AAFC_NORMALS_URL, "/getSamples"), encode = "form",
             httr::timeout(120),
             body = list(geometry = jsonlite::toJSON(g, auto_unbox = TRUE, digits = NA),
                         geometryType = "esriGeometryMultipoint",
                         mosaicRule = sprintf('{"mosaicMethod":"esriMosaicLockRaster","lockRasterIds":[%d]}', oid),
                         returnFirstValueOnly = "true", f = "json")),
             error = function(e) e)
      if (!inherits(r, "error") && httr::status_code(r) == 200L) {
        d <- get_json(r)
        if (is.null(d$error)) {
          v <- rep(NA_real_, length(rows))
          s <- d$samples
          if (length(s)) v[s$locationId + 1L] <- suppressWarnings(as.numeric(s$value))
          return(v)
        }
      }
      Sys.sleep(2 * attempt)
    }
    stop("fetch_aafc_normals(): the AAFC service did not answer.", call. = FALSE)
  }
  for (v in vars) {
    if (isTRUE(verbose)) message("[aafc] ", v, " (", period, ")")
    for (b in batches) out[b, v] <- sample_one(b, ids[[v]])
  }
  res <- data.frame(id = id, lat = lat, lon = lon, out, stringsAsFactors = FALSE)
  attr(res, "period") <- period
  attr(res, "source") <- "Agriculture and Agri-Food Canada, Derived Normal Climate Data (Open Government Licence - Canada)"
  res
}

# Extraterrestrial radiation (MJ m-2 day-1) on a day of the year at a
# latitude (FAO-56, eq. 21-25).
.extraterrestrial_radiation <- function(lat_deg, doy) {
  phi <- lat_deg * pi / 180
  dr <- 1 + 0.033 * cos(2 * pi * doy / 365)
  delta <- 0.409 * sin(2 * pi * doy / 365 - 1.39)
  ws <- acos(pmin(1, pmax(-1, -tan(phi) * tan(delta))))
  (24 * 60 / pi) * 0.0820 * dr * (ws * sin(phi) * sin(delta) + cos(phi) * cos(delta) * sin(ws))
}

# Day length in hours on a day of the year at a latitude.
.day_length <- function(lat_deg, doy) {
  phi <- lat_deg * pi / 180
  delta <- 0.409 * sin(2 * pi * doy / 365 - 1.39)
  24 / pi * acos(pmin(1, pmax(-1, -tan(phi) * tan(delta))))
}

#' Soil climate of the Canadian system from climate normals
#'
#' Estimates, from monthly climate normals (as returned by
#' \code{\link{fetch_aafc_normals}}), what the Canadian System of Soil
#' Classification reads as the soil climate (Ch 2, p. 16, criterion 7 of the
#' chernozemic A; the soil temperature classes and moisture subclasses of its
#' Tables 3 and 4, after the map Soil Climates of Canada, Clayton et al.
#' 1977):
#' \itemize{
#'   \item the mean annual air temperature (MAAT, the mean of the twelve
#'     monthly means) and, for the mean annual soil temperature (MAST), the
#'     range from the MAAT to the MAAT plus \code{mast_offset} degrees: in
#'     Canada the soil under snow is warmer than the air;
#'   \item the climatic moisture index (CMI) of Table 4, the growing-season
#'     precipitation as a percentage of the water crops would use: here the
#'     precipitation over the potential evapotranspiration (PET) of the
#'     months of the growing season (April to October) whose mean
#'     temperature is above 5 degrees. The PET is computed in two ways: by
#'     Thornthwaite (monthly mean temperatures and day length), the central
#'     estimate, which matches the zones the book gives the Chernozem great
#'     groups on the NPDB (Brown subarid to semiarid, Dark Brown semiarid,
#'     Black and Dark Gray subhumid); and by Hargreaves and Samani (monthly
#'     minimum and maximum temperatures and extraterrestrial radiation),
#'     the other end of the range (on the NPDB mostly drier, sometimes
#'     wetter);
#'   \item the moisture subclass from each index (Table 4): perhumid above
#'     84, humid 74-84, subhumid 59-73, semiarid 46-58, subarid 25-45, arid
#'     below 25.
#' }
#'
#' @param normals A data frame with \code{id}, \code{lat} and the columns of
#'   \code{\link{fetch_aafc_normals}}.
#' @param mast_offset The range added to the MAAT for the MAST, in degrees
#'   (two numbers, read from the lower to the higher).
#' @return A data frame: \code{id}, \code{maat}, \code{mast_lo},
#'   \code{mast_hi}, \code{p_gs} (mm), \code{gs_months}, \code{pet_gs} and
#'   \code{cmi} and \code{moisture_subclass} (Thornthwaite),
#'   \code{pet_gs_hargreaves}, \code{cmi_hargreaves} and
#'   \code{moisture_subclass_hargreaves}.
#' @references Soil Classification Working Group (1998). The Canadian System
#'   of Soil Classification, 3rd ed., Tables 3 and 4. Clayton, J.S. et al.
#'   (1977). Soils of Canada. Thornthwaite, C.W. (1948). Geographical Review
#'   38: 55-94. Hargreaves, G.H., Samani, Z.A. (1985). Applied Engineering in
#'   Agriculture 1: 96-99. Allen, R.G. et al. (1998). FAO Irrigation and
#'   Drainage Paper 56.
#' @export
npdb_soil_climate <- function(normals, mast_offset = c(0, 4)) {
  if (!is.numeric(mast_offset) || length(mast_offset) != 2L || anyNA(mast_offset))
    stop("npdb_soil_climate(): `mast_offset` must be two numbers.", call. = FALSE)
  mast_offset <- sort(mast_offset)
  need <- c("id", "lat", .aafc_normals_vars())
  miss <- setdiff(need, names(normals))
  if (length(miss)) stop("npdb_soil_climate(): missing columns ", paste(miss, collapse = ", "),
                         call. = FALSE)
  ta <- as.matrix(normals[, sprintf("ta_m%02d", 1:12)])
  maat <- rowMeans(ta)
  mid <- c(15, 46, 74, 105, 135, 166, 196, 227, 258, 288, 319, 349)
  days <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  # Thornthwaite: heat index over the twelve months
  heat <- rowSums((pmax(ta, 0) / 5)^1.514)
  expo <- 6.75e-7 * heat^3 - 7.71e-5 * heat^2 + 1.792e-2 * heat + 0.49239
  months <- 4:10
  pet_t <- pet_h <- p <- matrix(0, nrow(normals), length(months))
  for (k in seq_along(months)) {
    m <- months[k]; mm <- sprintf("%02d", m)
    t <- ta[, m]
    grow <- !is.na(t) & t > 5
    et <- ifelse(t >= 26.5, -415.85 + 32.24 * t - 0.43 * t^2,
                 16 * (10 * pmax(t, 0) / pmax(heat, 1e-9))^expo)
    pet_t[, k] <- ifelse(grow, et * .day_length(normals$lat, mid[m]) / 12 * days[m] / 30, 0)
    rng <- pmax(normals[[paste0("tx_m", mm)]] - normals[[paste0("tn_m", mm)]], 0)
    ra <- .extraterrestrial_radiation(normals$lat, mid[m])
    pet_h[, k] <- ifelse(grow, 0.0023 * 0.408 * ra * pmax(t + 17.8, 0) * sqrt(rng) * days[m], 0)
    p[, k] <- ifelse(grow, normals[[paste0("ac_m", mm)]], 0)
  }
  p_gs <- rowSums(p)
  index <- function(pet) { e <- rowSums(pet); ifelse(!is.na(e) & e > 0, round(100 * p_gs / e), NA_real_) }
  subclass <- function(cmi) as.character(cut(cmi, c(-Inf, 24.5, 45.5, 58.5, 73.5, 84.5, Inf),
    labels = c("arid", "subarid", "semiarid", "subhumid", "humid", "perhumid")))
  cmi_t <- index(pet_t); cmi_h <- index(pet_h)
  data.frame(id = normals$id, maat = maat, mast_lo = maat + mast_offset[1],
             mast_hi = maat + mast_offset[2], p_gs = p_gs,
             gs_months = rowSums(ta[, months, drop = FALSE] > 5),
             pet_gs = rowSums(pet_t), cmi = cmi_t, moisture_subclass = subclass(cmi_t),
             pet_gs_hargreaves = rowSums(pet_h), cmi_hargreaves = cmi_h,
             moisture_subclass_hargreaves = subclass(cmi_h), stringsAsFactors = FALSE)
}

# Rows of a pedon that are A horizons (an A master in the Canadian notation,
# or an A or E in another one): the horizons whose dry colour the key reads.
.npdb_a_rows <- function(pedon) {
  x <- .cssc_horizons(pedon)
  a <- .cssc_is(x$parsed, "A", weak = "any") %in% TRUE
  other <- substr(.cssc_other_masters(x$h$designation %||% rep(NA_character_, nrow(x$h)),
                                      x$notation$ok), 1L, 1L)
  which(a | other %in% c("A", "E"))
}

#' Dry colour of A horizons from their moist colour
#'
#' Fits, on the A horizons of a set of pedons that record both colours, a
#' linear model of the dry Munsell value on the moist value and of the dry
#' chroma on the moist chroma, and estimates its error by cross-validation
#' grouped by pedon (the horizons of a pedon stay in one fold): the range of
#' the out-of-fold errors that holds
#' \code{level} of them (from its lower to its upper tail). The pedons are
#' dealt to the folds in turn, in the order given, so the result does not
#' depend on the random number generator. On the NPDB the
#' error is close to one value unit, so the estimate decides the dry value of
#' 5.5 of the chernozemic A for most dark A horizons but seldom the limits of
#' 3.5 and 4.5 between the Black, Dark Brown and Brown Chernozems.
#'
#' @param pedons A list of \code{\link{PedonRecord}}.
#' @param level Share of the cross-validated errors the range holds.
#' @param folds Number of folds.
#' @return A list of class \code{npdb_dry_colour_model}: \code{value} and
#'   \code{chroma} (each with \code{coef}, \code{band} = the error range,
#'   \code{rmse} out of fold, \code{n} horizons), \code{level}.
#' @seealso \code{\link{npdb_scenario}}
#' @export
npdb_dry_colour_model <- function(pedons, level = 0.8, folds = 10L) {
  rows <- do.call(rbind, lapply(seq_along(pedons), function(i) {
    p <- pedons[[i]]
    a <- .npdb_a_rows(p); h <- p$horizons
    if (!length(a)) return(NULL)
    data.frame(id = as.character(i), vm = h$munsell_value_moist[a],
               vd = h$munsell_value_dry[a], cm = h$munsell_chroma_moist[a],
               cd = h$munsell_chroma_dry[a], stringsAsFactors = FALSE)
  }))
  fit <- function(y, x) {
    d <- rows[!is.na(rows[[y]]) & !is.na(rows[[x]]), c("id", x, y)]
    names(d) <- c("id", "x", "y")
    if (nrow(d) < 20L) stop("npdb_dry_colour_model(): fewer than 20 A horizons record both colours.",
                            call. = FALSE)
    ids <- unique(d$id)
    k <- min(as.integer(folds), length(ids))
    f <- stats::setNames(rep(seq_len(k), length.out = length(ids)), ids)
    err <- unlist(lapply(seq_len(k), function(j) {
      tr <- d[f[d$id] != j, ]; te <- d[f[d$id] == j, ]
      m <- stats::lm(y ~ x, tr)
      te$y - stats::predict(m, te)
    }))
    m <- stats::lm(y ~ x, d)
    tail <- (1 - level) / 2
    cf <- stats::coef(m); cf[is.na(cf)] <- 0    # a constant moist colour has no slope
    list(coef = cf, band = unname(stats::quantile(err, c(tail, 1 - tail), na.rm = TRUE)),
         rmse = sqrt(mean(err^2)), n = nrow(d))
  }
  structure(list(value = fit("vd", "vm"), chroma = fit("cd", "cm"), level = level),
            class = "npdb_dry_colour_model")
}

#' print method for npdb_dry_colour_model
#' @param x The model.
#' @param ... Unused.
#' @return \code{x}, invisibly.
#' @keywords internal
#' @export
print.npdb_dry_colour_model <- function(x, ...) {
  for (k in c("value", "chroma")) {
    m <- x[[k]]
    cat(sprintf("dry %s = %.2f + %.2f x moist %s  (n = %d A horizons; error out of fold: RMSE %.2f, %d%% within %+.2f to %+.2f)\n",
                k, m$coef[[1]], m$coef[[2]], k, m$n, m$rmse, round(100 * x$level), m$band[1], m$band[2]))
  }
  invisible(x)
}

#' A scenario for the NPDB: estimated dry colour and soil climate
#'
#' Adds to NPDB pedons what the records lack and the Canadian key reads for
#' the Chernozems (and the Gray Brown Luvisols and some subgroups), each as a
#' range, for \code{\link{benchmark_npdb}}:
#' \itemize{
#'   \item the dry value and chroma of the A horizons that record a moist
#'     colour and no dry one, from \code{dry_colour} (a
#'     \code{\link{npdb_dry_colour_model}}), with the range of its error;
#'   \item the mean annual soil temperature, as the range of
#'     \code{\link{npdb_soil_climate}} (from the air temperature to 4 degrees
#'     above it; kept as a range only where it crosses 0 or 8 degrees, the
#'     limits the key reads), and the moisture subclass by Thornthwaite, with
#'     the subclass by Hargreaves and every subclass in between where they
#'     differ.
#' }
#' The pedons hold the central estimate (the middle of the soil temperature
#' range) in the usual fields (\code{munsell_value_dry},
#' \code{munsell_chroma_dry}, \code{site$mean_annual_soil_temp_C},
#' \code{site$soil_moisture_subclass}), and the Thornthwaite moisture index
#' in \code{site$npdb_cmi}; recorded values are never replaced. The ranges
#' are kept in \code{site$npdb_scenario} (a second call adds to them); a
#' pedon with no \code{site$id}, or one not in \code{climate}, gets no
#' climate. \code{benchmark_npdb}
#' classifies such a pedon at every combination of the ends of its ranges
#' and counts a class as decided only where they all give it; otherwise the
#' result is provisional.
#'
#' @param pedons A list of \code{\link{PedonRecord}} from
#'   \code{\link{load_npdb_pedons}}.
#' @param climate A data frame from \code{\link{npdb_soil_climate}} (its
#'   \code{id} matching \code{site$id}), or \code{NULL}.
#' @param dry_colour A \code{\link{npdb_dry_colour_model}}, or \code{NULL}.
#' @return The pedons (copies), with the estimates.
#' @examples
#' \dontrun{
#' peds <- load_npdb_pedons("path/to/npdb_v3_csv")
#' nm <- fetch_aafc_normals(vapply(peds, function(p) p$site$lat, 0),
#'                          vapply(peds, function(p) p$site$lon, 0),
#'                          id = names(peds))
#' sc <- npdb_scenario(peds, climate = npdb_soil_climate(nm),
#'                     dry_colour = npdb_dry_colour_model(peds))
#' benchmark_npdb(sc, level = "order", set_aside = character(0))
#' }
#' @export
npdb_scenario <- function(pedons, climate = NULL, dry_colour = NULL) {
  if (!is.null(dry_colour) && !inherits(dry_colour, "npdb_dry_colour_model"))
    stop("npdb_scenario(): `dry_colour` must come from npdb_dry_colour_model().", call. = FALSE)
  if (!is.null(climate)) climate <- climate[!duplicated(climate$id), , drop = FALSE]
  out <- lapply(pedons, function(p) {
    q <- p$clone(deep = TRUE)
    h <- data.table::copy(q$horizons)
    sc <- list()
    if (!is.null(dry_colour)) {
      a <- .npdb_a_rows(q)
      rows <- a[is.na(h$munsell_value_dry[a]) & !is.na(h$munsell_value_moist[a])]
      if (length(rows)) {
        cv <- dry_colour$value$coef; cc <- dry_colour$chroma$coef
        dv <- pmin(pmax(cv[[1]] + cv[[2]] * h$munsell_value_moist[rows], 0), 10)
        data.table::set(h, i = rows, j = "munsell_value_dry", value = as.numeric(dv))
        crow <- rows[is.na(h$munsell_chroma_dry[rows]) & !is.na(h$munsell_chroma_moist[rows])]
        if (length(crow)) {
          dc <- pmax(cc[[1]] + cc[[2]] * h$munsell_chroma_moist[crow], 0)
          data.table::set(h, i = crow, j = "munsell_chroma_dry", value = as.numeric(dc))
        }
        sc$dry_value_rows <- rows; sc$dry_value_band <- dry_colour$value$band
        sc$dry_chroma_rows <- crow; sc$dry_chroma_band <- dry_colour$chroma$band
      }
    }
    if (!is.null(climate)) {
      pid <- q$site$id
      k <- if (length(pid) == 1L && !is.na(pid) && nzchar(as.character(pid)))
             match(.npdb_id_chr(pid), .npdb_id_chr(climate$id)) else NA_integer_
      if (!is.na(k)) {
        cl <- as.list(climate[k, , drop = FALSE])
        val <- function(v) if (is.null(v) || length(v) != 1L) NA else v
        unset <- function(v) is.null(v) || all(is.na(v))
        lo <- val(cl$mast_lo); hi <- val(cl$mast_hi)
        if (unset(q$site$mean_annual_soil_temp_C) && !is.na(lo) && !is.na(hi)) {
          q$site$mean_annual_soil_temp_C <- (lo + hi) / 2
          # the key reads the soil temperature at 0 and 8 degrees only
          sc["mast_range"] <- list(if (any(min(lo, hi) < c(0, 8) & max(lo, hi) >= c(0, 8)))
                                     sort(c(lo, hi)))
        }
        sub <- tolower(as.character(val(cl$moisture_subclass)))
        if (unset(q$site$soil_moisture_subclass) && !is.na(sub)) {
          q$site$soil_moisture_subclass <- sub
          q$site$npdb_cmi <- val(cl$cmi)
          sc["moisture_alt"] <- list(NULL)
          cl$moisture_subclass <- sub
          alt <- tolower(as.character(val(cl$moisture_subclass_hargreaves)))
          if (!is.na(alt) && !identical(alt, cl$moisture_subclass)) {
            # the other end and every subclass in between (the key reads
            # "subhumid" exactly, for some Solonetzic subgroups)
            scale <- c("arid", "subarid", "semiarid", "subhumid", "humid", "perhumid")
            ends <- match(c(cl$moisture_subclass, alt), scale)
            if (!anyNA(ends)) {
              between <- scale[seq(ends[1], ends[2])]
              sc$moisture_alt <- setdiff(between, cl$moisture_subclass)
            }
          }
        }
      }
    }
    q$horizons <- h
    # a second call adds to the ranges kept by the first, and replaces those
    # of what it sets again
    if (length(sc)) {
      merged <- utils::modifyList(q$site$npdb_scenario %||% list(), sc)
      q$site$npdb_scenario <- if (length(merged)) merged else NULL
    }
    q
  })
  names(out) <- names(pedons)
  out
}

# Identifiers compared as text (a number without scientific notation).
.npdb_id_chr <- function(v) {
  if (is.numeric(v)) format(v, scientific = FALSE, trim = TRUE) else as.character(v)
}

# The versions of a scenario pedon to classify: the central one first, then
# one per combination of the ends of its ranges (dry value, dry chroma, soil
# temperature) and of the moisture subclasses between the two PET methods.
.npdb_scenario_versions <- function(p) {
  sc <- p$site$npdb_scenario
  if (is.null(sc)) return(list(p))
  dims <- list()
  if (length(sc$dry_value_rows)) dims$value <- 1:2
  if (length(sc$dry_chroma_rows)) dims$chroma <- 1:2
  if (length(sc$mast_range)) dims$mast <- 1:2
  if (length(sc$moisture_alt)) dims$moisture <- seq_len(length(sc$moisture_alt) + 1L)
  if (!length(dims)) return(list(p))
  grid <- expand.grid(dims)
  # with the moisture alone, its first value is the central version itself
  if (identical(names(dims), "moisture")) grid <- grid[grid$moisture > 1L, , drop = FALSE]
  c(list(p), lapply(seq_len(nrow(grid)), function(g) {
    q <- p$clone(deep = TRUE)
    h <- data.table::copy(q$horizons)
    if (!is.null(grid$value)) {
      r <- sc$dry_value_rows
      v <- pmin(pmax(h$munsell_value_dry[r] + sc$dry_value_band[grid$value[g]], 0), 10)
      data.table::set(h, i = r, j = "munsell_value_dry", value = as.numeric(v))
    }
    if (!is.null(grid$chroma)) {
      r <- sc$dry_chroma_rows
      v <- pmax(h$munsell_chroma_dry[r] + sc$dry_chroma_band[grid$chroma[g]], 0)
      data.table::set(h, i = r, j = "munsell_chroma_dry", value = as.numeric(v))
    }
    if (!is.null(grid$mast)) q$site$mean_annual_soil_temp_C <- sc$mast_range[grid$mast[g]]
    if (!is.null(grid$moisture) && grid$moisture[g] > 1L)
      q$site$soil_moisture_subclass <- sc$moisture_alt[grid$moisture[g] - 1L]
    q$horizons <- h
    q
  }))
}
