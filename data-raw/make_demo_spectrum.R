# Builds inst/shiny/classify_app_pro/www/demo_spectrum.csv -- a SYNTHETIC
# Vis-NIR spectrum for the Pro app's Spectra tab, one row per horizon of
# make_ferralsol_canonical().
#
# Nothing here was measured. Each row is a smooth continuum plus Gaussian
# absorption features placed where a kaolinitic, hematite-rich Ferralsol has
# them, so the demo behaves like the soil it illustrates (a red Munsell colour,
# not a grey one):
#
#   * a steep hematite edge below ~590 nm and a broad Fe3+ band near 880 nm
#   * O-H / water bands near 1400 and 1900 nm
#   * the kaolinite doublet at 2165 / 2205 nm
#
# Surface horizons are darkened for organic matter; deeper ones are brighter and
# redder. A little noise is added so the curve does not look drawn with a ruler.
#
# Run from the package root:  Rscript data-raw/make_demo_spectrum.R

set.seed(20260922)
wl <- seq(350, 2500, by = 5)
n  <- nrow(soilKey::make_ferralsol_canonical()$horizons)

gauss <- function(centre, width, depth) depth * exp(-0.5 * ((wl - centre) / width)^2)

one <- function(k) {
  depth_frac <- (k - 1) / max(1, n - 1)              # 0 at the surface, 1 at depth
  continuum  <- 0.20 + 0.18 * (1 - exp(-(wl - 350) / 450)) - 0.03 * (wl - 350) / 2150
  hematite   <- 0.21 / (1 + exp((wl - 590) / 18))     # low blue/green, red edge ~590 nm
  r <- continuum - hematite -
       gauss(880, 90, 0.045 + 0.02 * depth_frac) -     # Fe3+ crystal-field band
       gauss(1415, 25, 0.035) - gauss(1915, 35, 0.060) - # O-H and water
       gauss(2165, 12, 0.020) - gauss(2207, 14, 0.035)   # kaolinite doublet
  r <- r * (0.78 + 0.22 * depth_frac)                  # darker where organic matter is
  r <- r + stats::rnorm(length(wl), 0, 0.0015)
  round(pmin(pmax(r, 0.01), 0.95), 4)
}

m <- t(vapply(seq_len(n), one, numeric(length(wl))))
colnames(m) <- wl
utils::write.csv(m, "inst/shiny/classify_app_pro/www/demo_spectrum.csv", row.names = FALSE)
cat("wrote", nrow(m), "synthetic spectra x", ncol(m), "wavelengths\n")
