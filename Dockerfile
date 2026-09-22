# =============================================================================
# soilKey -- reproducible R container.
#
# Build:
#   docker build -t soilkey:latest .
#
# Run interactively (R REPL with soilKey loaded):
#   docker run --rm -it soilkey:latest R -e 'library(soilKey); sessionInfo()'
#
# Run the Shiny classification app on http://localhost:3838:
#   docker run --rm -it -p 3838:3838 soilkey:latest \
#     R -e 'soilKey::run_classify_app(host = "0.0.0.0", port = 3838L,
#                                       launch.browser = FALSE)'
#
# Mount your soil_data/ folder (LUCAS, ESDB, BDsolos):
#   docker run --rm -it -v $(pwd)/soil_data:/data soilkey:latest
#
# Image size target: ~ 1.5 GB (rocker/r-ver base + GDAL stack + soilKey).
# =============================================================================

FROM rocker/r-ver:4.4.0

LABEL org.opencontainers.image.source="https://github.com/HugoMachadoRodrigues/soilKey"
LABEL org.opencontainers.image.description="Automated soil profile classification per WRB 2022, SiBCS 5 and USDA Soil Taxonomy 13"
LABEL org.opencontainers.image.licenses="MIT"

# System dependencies for terra (GDAL/GEOS/PROJ), sf, httr, jsonlite,
# pdftools, magick, foreign-mdb readers.
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
        pandoc \
        git \
    && rm -rf /var/lib/apt/lists/*

# Pin remotes / pak for fast and reproducible installation.
RUN R -e "install.packages('pak', repos = 'https://cloud.r-project.org')"

# Copy the soilKey source (only what's needed to install).
WORKDIR /opt/soilKey
COPY DESCRIPTION NAMESPACE ./
COPY R           ./R
COPY man         ./man
COPY inst        ./inst
COPY tests       ./tests
COPY vignettes   ./vignettes
COPY NEWS.md README.md LICENSE ./

# Install soilKey + the Suggests we want available by default in the
# image (terra for spatial lookups, foreign for VAT decoding, pls for
# OSSL training, munsellinterpol for Vis-NIR -> Munsell, shiny+DT for
# the Shiny app).
RUN R -e "pak::pkg_install(c('local::./', 'terra', 'foreign', 'pls', \
                              'munsellinterpol', 'shiny', 'DT', \
                              'data.table', 'cli', 'rlang', 'R6', 'yaml'))"

# Smoke check during the build: load + run a tiny end-to-end example.
RUN R -e "library(soilKey); cat('soilKey ', as.character(packageVersion('soilKey')), ' OK\n', sep = '')"

EXPOSE 3838

# Default: drop into an R REPL with soilKey loaded.
CMD ["R", "--no-save", "-e", "library(soilKey); cat('soilKey loaded. Type ?soilKey or run_classify_app() to begin.\\n'); options(prompt = 'soilKey> '); invisible(readLines('stdin'))"]
