# Predict Munsell hue / value / chroma from Vis-NIR reflectance spectra

Combines
[`predict_xyz_from_spectra`](https://hugomachadorodrigues.github.io/soilKey/reference/predict_xyz_from_spectra.md)
with the Munsell renotation interpolation in munsellinterpol (CRAN,
GPL). Returns hue (e.g. `"7.5YR"`), value (0..10) and chroma (0..20) per
sample, plus the soilKey fields

## Usage

``` r
predict_munsell_from_spectra(spectra, wavelengths, round_chip = TRUE)
```

## Arguments

- spectra:

  Reflectance values, in 0..1 or 0..100. A numeric vector (one sample),
  a numeric matrix (rows = samples, cols = wavelengths) or a data.frame.

- wavelengths:

  Numeric vector of the wavelengths (in nm) corresponding to the columns
  of `spectra`. Must cover at least 400-700 nm; values outside 380-780
  are ignored.

- round_chip:

  If `TRUE` (default), snaps the predicted HVC to the nearest standard
  Munsell chip grid via
  [`munsellinterpol::roundHVC()`](https://rdrr.io/pkg/munsellinterpol/man/round.html).
  `FALSE` returns continuous HVC (useful for further numeric work).

## Value

A data.frame with columns `munsell_hue_moist`, `munsell_value_moist`,
`munsell_chroma_moist`, `munsell_string` (e.g. `"7.5YR 4/6"`), `X`, `Y`,
`Z`, one row per sample.

## Details

The Munsell renotation is defined under *Illuminant C*, while the
colorimetry here is computed under D65, so the conversion adapts D65 -\>
C. It calls `munsellinterpol::XYZtoMunsell(XYZ, white =)`
(munsellinterpol \>= 3.4-0), which performs that chromatic adaptation
internally, and falls back to the numerically identical XYZ -\>
CIELAB(D65) -\> `LabToMunsell()` route on older versions. Feeding D65
chromaticities straight to `xyYtoMunsell()` (with no `white`) would bias
every colour toward green-yellow (a perfect neutral would return Chroma
~ 0.65 rather than 0); this routine avoids that. The D65 reference white
is derived from the same bundled CIE table the colorimetry integrates
against (so a constant-reflectance spectrum maps to an exact neutral,
and a perfect reflecting diffuser to Munsell value 10), and the
conversion is vectorised over all rows of `spectra` at once.
`munsell_hue_moist`, `munsell_value_moist`, `munsell_chroma_moist` ready
to write into a
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
via the pedon's `add_measurement` method (see also
[`fill_munsell_from_spectra`](https://hugomachadorodrigues.github.io/soilKey/reference/fill_munsell_from_spectra.md)).

This is the v0.9.47 unblock for the v0.9.35 Argissolo Vermelho / Amarelo
/ Vermelho-Amarelo color-confusion case: when a user has Vis-NIR spectra
(which Embrapa's BDsolos / FEBR do not include but the OSSL does), the
Munsell hue can be recovered physically without waiting for the
surveyor's morphological description.

## Examples

``` r
if (FALSE) { # \dontrun{
# White reflector across the visible: should map to a near-neutral
# high-value Munsell color.
wl <- seq(380, 780, by = 5)
R  <- rep(0.9, length(wl))
predict_munsell_from_spectra(R, wavelengths = wl)
} # }
```
