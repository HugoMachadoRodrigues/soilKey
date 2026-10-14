# The country table of the visitor counter of soilKey Pro (v0.9.261):
# inst/shiny/classify_app_pro/data/countries.csv, with the ISO 3166 code, a
# point inside the country for the map, and its name in English and in
# Portuguese.
#
#   * Points: the largest polygon of each country in the `maps` world database
#     (public domain, from the CIA World Data Bank II), joined to ISO codes by
#     maps::iso.alpha(), and a point on its surface (sf::st_point_on_surface()).
#   * Names: the Unicode CLDR names that Node's Intl.DisplayNames gives for
#     "en" and "pt-BR".
#
# Run from the root of the repository with R, sf, maps and Node installed:
#   Rscript data-raw/visitor_countries.R
suppressMessages({ library(sf); library(maps) })
sf::sf_use_s2(FALSE)
w <- sf::st_as_sf(maps::map("world", fill = TRUE, plot = FALSE))
w$region <- sub(":.*$", "", w$ID)
w$iso2 <- suppressWarnings(maps::iso.alpha(w$region, n = 2))
# Namibia's code is "NA", which maps' table holds as a missing value
w$iso2[w$region == "Namibia"] <- "NA"
w <- w[!is.na(w$iso2), ]
# one point per country, in the largest polygon of all its features
one <- lapply(split(seq_len(nrow(w)), w$iso2), function(i) {
  g <- sf::st_cast(sf::st_geometry(w[i, ]), "POLYGON", warn = FALSE)
  g <- g[which.max(as.numeric(sf::st_area(g)))]
  suppressWarnings(sf::st_coordinates(sf::st_point_on_surface(sf::st_make_valid(g))))[1, 1:2]
})
big <- data.frame(iso2 = names(one), stringsAsFactors = FALSE)
pt <- do.call(rbind, one)
codes <- sort(unique(c(big$iso2, maps::iso3166$a2, "NA")))
codes <- codes[grepl("^[A-Z]{2}$", codes)]
js <- sprintf(paste0(
  "const en = new Intl.DisplayNames(['en'], {type: 'region'});",
  "const pt = new Intl.DisplayNames(['pt-BR'], {type: 'region'});",
  "for (const c of %s) console.log([c, en.of(c), pt.of(c)].join('\\t'));"),
  jsonlite::toJSON(codes))
nm <- utils::read.delim(text = system2("node", c("-e", shQuote(js)), stdout = TRUE),
                        header = FALSE, col.names = c("iso2", "name_en", "name_pt"),
                        quote = "", encoding = "UTF-8", na.strings = character(0))
out <- merge(nm, data.frame(iso2 = big$iso2, lon = round(pt[, 1], 2), lat = round(pt[, 2], 2)),
             by = "iso2", all.x = TRUE)
out <- out[order(out$iso2), c("iso2", "lon", "lat", "name_en", "name_pt")]
utils::write.csv(out, "inst/shiny/classify_app_pro/data/countries.csv", row.names = FALSE,
                 fileEncoding = "UTF-8", na = "")
cat(nrow(out), "countries,", sum(!is.na(out$lon)), "with a point\n")
