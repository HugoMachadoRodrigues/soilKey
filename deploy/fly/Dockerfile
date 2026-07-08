# =============================================================================
# soilKey Pro -- production image for the public Shiny app (soilkeypro.com).
#
# Differs from the repo-root Dockerfile (which is a reproducible R REPL image):
# this one installs the FULL runtime dependency set the Pro app touches across
# every tab and, by default, LAUNCHES the app on 0.0.0.0:8080 instead of an R
# REPL. Built and run by Fly.io (see deploy/fly/fly.toml).
#
#   Build context = repository root.
#   fly deploy --config deploy/fly/fly.toml .
# =============================================================================

FROM rocker/r-ver:4.4.0

LABEL org.opencontainers.image.source="https://github.com/HugoMachadoRodrigues/soilKey"
LABEL org.opencontainers.image.description="soilKey Pro -- public Shiny app (WRB 2022 / SiBCS 5 / USDA ST 13)"
LABEL org.opencontainers.image.licenses="MIT"

# System libraries for terra/sf (GDAL/GEOS/PROJ), magick, pdftools, RSQLite,
# fonts for plotly/leaflet static bits, and pandoc for the HTML report.
RUN apt-get update && apt-get install -y --no-install-recommends \
        libcurl4-openssl-dev \
        libssl-dev \
        libxml2-dev \
        libgdal-dev \
        libgeos-dev \
        libproj-dev \
        libudunits2-dev \
        libpoppler-cpp-dev \
        libmagick++-dev \
        libfontconfig1-dev \
        libfreetype6-dev \
        libpng-dev \
        libtiff5-dev \
        libjpeg-dev \
        libsqlite3-dev \
        libsodium-dev \
        pandoc \
        git \
    && rm -rf /var/lib/apt/lists/*

# pak resolves system + R deps and pulls Posit Package Manager binaries for
# jammy, so this stays fast despite the long package list.
RUN R -e "install.packages('pak', repos = 'https://cloud.r-project.org')"

WORKDIR /opt/soilKey

# Copy only what's needed to install the package (better layer caching).
COPY DESCRIPTION NAMESPACE ./
COPY R           ./R
COPY data        ./data
COPY man         ./man
COPY inst        ./inst
COPY tests       ./tests
COPY vignettes   ./vignettes
COPY NEWS.md README.md LICENSE ./

# Full runtime set for the Pro app:
#   * hard requirement to even start (run_classify_app / .pro_require):
#       shiny, bslib, DT, plotly, shinyWidgets, leaflet
#   * UI plumbing: htmltools, htmlwidgets, jsonlite, commonmark
#   * spatial (Map / Spatial tabs, SoilGrids demo raster): terra, sf
#   * classification engine (best accuracy): aqp, SoilTaxonomy, mpspline2
#   * Spectra tab (OSSL gap-fill + Savitzky-Golay): prospectr, pls
#   * Vis-NIR -> Munsell: munsellinterpol
#   * Photo VLM image handling (optional, mock by default): magick, pdftools,
#       base64enc, httr, jsonvalidate
#   * HTML report: rmarkdown, knitr  (PDF/LaTeX intentionally omitted -> the
#       report module falls back to HTML)
#   * misc used by modules: DBI, RSQLite, maps, withr
#
# ellmer (the live Assistant + online Photo VLM client) is installed separately
# below -- it postdates this image's frozen CRAN snapshot. NOT installed:
#   * foreign -- ships with R as a "recommended" package (already present);
#       listing it explicitly makes pak's solver conflict on its pinned version.
RUN R -e "pak::pkg_install(c( \
      'local::./', \
      'R6','data.table','yaml','cli','rlang', \
      'shiny','bslib','DT','plotly','shinyWidgets','leaflet', \
      'htmltools','htmlwidgets','jsonlite','commonmark', \
      'terra','sf', \
      'aqp','SoilTaxonomy','mpspline2', \
      'prospectr','pls', \
      'munsellinterpol', \
      'magick','pdftools','base64enc','httr','jsonvalidate', \
      'rmarkdown','knitr', \
      'DBI','RSQLite','maps','withr' \
    ))"

# ellmer (LLM client for the Assistant chat + online Photo VLM) postdates the
# frozen snapshot above, so install it from the current Posit PM binary repo.
RUN R -e "options(repos = c(PPM = 'https://packagemanager.posit.co/cran/__linux__/jammy/latest')); \
  pak::pkg_install('ellmer')"

# Build-time smoke check: the package loads and every hard-required app package
# (incl. ellmer for the live Assistant) is importable. Fails the build early if
# the dependency set is wrong.
RUN R -e "library(soilKey); \
  stopifnot(is.function(soilKey::run_classify_app)); \
  need <- c('shiny','bslib','DT','plotly','shinyWidgets','leaflet','ellmer'); \
  ok <- vapply(need, requireNamespace, logical(1), quietly = TRUE); \
  if (!all(ok)) stop('missing: ', paste(need[!ok], collapse=', ')); \
  cat('soilKey', as.character(packageVersion('soilKey')), 'app deps OK\n')"

EXPOSE 8080

# Launch the Pro app, bound to all interfaces. Honour $PORT when the platform
# injects it (Cloud Run sets PORT; Fly uses the fixed internal_port); default
# to 8080 otherwise.
CMD ["R", "--no-save", "-e", "port <- as.integer(Sys.getenv('PORT', '8080')); soilKey::run_classify_app(ui = 'pro', port = port, host = '0.0.0.0', launch.browser = FALSE)"]
