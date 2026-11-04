# Attach a pedon to the references of the French Referentiel pedologique 2008

Attaches a
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
to the references of the *Referentiel pedologique* 2008 (AFES; Baize and
Girard coord.; Editions Quae, 2009). **Experimental**: this release
tests the 71 references of the first twenty-one groups of references
(GER): alocrisols (2), andosols (6), anthroposols (5), arenosols (1),
brunisols (2), the solums dominated by calcium or magnesium (7:
rendosols, calcosols, dolomitosols, rendisols, calcisols, magnesisols,
calcarisols), chernosols (3), colluviosols (1), cryosols (2),
ferrallitisols and oxydisols (7), ferruginosols (4), fersialsols (4),
fluviosols (4), grisols (3), gypsosols (2), histosols (5),
leptismectisols (1), lithosols (1), luvisols (6), nitosols (1) and
organosols (4). The 39 references of the other 13 GER come in later
releases: a profile that shows their reference horizons gets them among
the candidates as not implemented yet, and its name is provisional. The
horizons of the intertropical chapters (F, OX, RT, ND, FE, OXc, OXm) are
read only in the intertropical domain, and the results of those chapters
and of the nitosols say that the book calls them a first version (Q18);
a fluviosol needs a recorded alluvial origin and is provisional while
the position in the valley and the water table are not recorded (Q14).
The histosols read their predominant H horizon between 40 and 120 cm,
the luvisols their texture contrast by the bands of the chapter, the
organosols the S/CEC of their Aho as a whole.

## Usage

``` r
classify_rp2008(
  pedon,
  rules = NULL,
  on_missing = c("warn", "silent", "error"),
  gapfill = FALSE
)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- rules:

  Optional pre-loaded rules (`load_rules("rp2008")`).

- on_missing:

  One of `"warn"` (default), `"silent"`, `"error"`.

- gapfill:

  Opt-in within-pedon gap-fill, as in
  [`classify_usda`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_usda.md).

## Value

A
[`ClassificationResult`](https://hugomachadorodrigues.github.io/soilKey/reference/ClassificationResult.md)
with `system_id` `"rp2008"`: the main name in `name` ("\[non rattache\]"
when nothing attaches), the GER label in `rsg_or_order`, the references
in the name in `levels` (`level` "references", `code`, `name`,
`name_native`, `name_en`, `ger`, `attachment`: simple, imperfect,
simple_or_imperfect, double), every reference that may attach in
`candidates` (with the columns of `ClassificationResult`, `group` the
GER, plus `name_en`, `implemented` and `precedence`, the book's
statements that put a reference of the name before it or exclude it),
the evaluation of every reference, the horizon frame and the rapid key
in `trace` (with `trace$position`, the depth of the first ND, OXm or OXc
of a ferrallitisol or oxydisol and its proximal or distal type), and
`provisional_because` (`"references:<code>"`: the references or GER that
could take the name once their data are known).

## Details

The RP is not a key: a solum attaches to one, two or no reference,
simply or imperfectly (pp. XIV, XVII). Every reference is tested every
time: its sequences of reference horizons, matched on the horizon frame
(the RP notation where the profile is written in it, else the horizons
identified on the data, `identify_horizons(pedon, "rp2008")`), and the
side tests of its chapter (depths, thicknesses, forbidden horizons,
saturation, carbonates, permafrost, colluvial or anthropic materials).
Each test is evaluated strictly and with the tolerances of p. 64 (depth
and thickness 10 to 200 cm, pH +/- 0.2, CEC +/- 1, S/CEC +/- 5, organic
carbon 8 +/- 2 and 30 +/- 5; any other threshold strict): a reference
met only within tolerance, or by an imperfect case its chapter names
(calcosol leptique, rendosol pachique, arenosol redoxique ...), is an
imperfect attachment, never a simple one. The main name follows the
book's explicit statements of precedence, then the degree, then the
order of the rapid key (pp. 66-68); a double attachment is written only
for the pairs the book prescribes (calcisol-calcosol, the anthroposols
transformes); with no reference, the name is "\[non rattache\]" and the
three nearest references are listed. The choice of one name is a
convention of soilKey, and `$candidates` lists every reference that may
attach the solum with its degree and the criteria that fail, pass only
within tolerance or are undecided.

Names are French, as printed, in the singular (`name`,
`levels$name_native`), with an English gloss (`levels$name_en`); the GER
is a label (`rsg_or_order`, `levels$ger`), not a level. The qualifiers
and the types come later. The readings where the text is silent are
listed in
`system.file("rules", "rp2008", "readings.yaml", package = "soilKey")`.

## References

Baize D., Girard M.-C. (coord.) (2009). Referentiel pedologique 2008.
AFES, Editions Quae, Versailles.

## See also

[`make_rp2008_example`](https://hugomachadorodrigues.github.io/soilKey/reference/make_rp2008_example.md),
[`soilkey_systems`](https://hugomachadorodrigues.github.io/soilKey/reference/soilkey_systems.md),
[`within_tolerance`](https://hugomachadorodrigues.github.io/soilKey/reference/within_tolerance.md).

## Examples

``` r
res <- classify_rp2008(make_rp2008_example("calcosols"), on_missing = "silent")
res$name
#> [1] "calcosol"
res$candidates[, c("code", "degree", "in_name")]
#>        code degree in_name
#> 1 calcosols simple    TRUE
```
