# The humus form of the Referentiel pedologique 2008

Attaches the epipedon ("episolum humifere") of a pedon to a humus form
of Annex 1 of the French Referentiel pedologique 2008 (pp. 327-355): the
four functional types (mull, moder, mor, amphimus) and their forms. The
horizons are read as
[`classify_rp2008`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_rp2008.md)
reads them (the RP notation where written, else the data), then, in
order: H horizons at the top leave the form to the histosols (p. 351);
an OHta makes a tangel, an An an anmoor; O horizons directly over an X,
R or D give the forms without fine mineral earth (pp. 348-349);
`site$land_use` or a ploughed L gives the open-land forms (p. 337);
otherwise the key of Figure 2 (p. 354) is walked on the O and A
horizons, and its outcome is made hydromorphic when the first mineral
layer is a G or bears g (p. 350).

## Usage

``` r
humus_form_rp(pedon)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

## Value

A list: `form` (the code of the form, NA when undecided), `name` (its
name as printed), `type` (mull, moder, mor or amphimus, NA when
undecided or for the anmoor), `label` (the name of the form when
decided, else of the type when decided, else NA: what follows "a" in the
name of the solum), `group` (aerated, hydromorphic, no_fine_earth,
open_land, histic or NA), `decided`, `route` ("horizons", "recorded" or
"none"), `candidates` (the codes of the forms still possible),
`no_form_possible` (TRUE when the horizons may also fit no form),
`recorded` (the recorded `site$humus_form`), `conflict` (the recorded
form disagrees with the horizons), `qualifiers` (a data frame:
`qualifier`, `group`, `value`, `degree` (pass, imperfect, undecided),
`page`, `note`), `path` and `answers` (the walk of the key), `missing`
(the attributes that would decide it) and `notes`.

## Details

The key reads the kind of the O horizons (OL, OLn, OLv, OF, OH: the
notation, or `fine_organic_matter_pct`), their zoogenic character (the
suffixes zo and noz, or `o_zoogenic`), their lateral cover
(`lateral_cover_pct`, or the parentheses of the RP notation), the
thickness of the OH and the biological structure of the A
(`biogenic_aggregates_pct`, else the structure). Every test is
three-valued: what is not recorded leaves the form undecided among its
candidates. A recorded `site$humus_form` decides when the horizons leave
the form undecided (even outside the forms they leave possible, the
conflict reported); when the horizons decide another form, they prevail
and the conflict is reported.

## References

Baize D., Girard M.-C. (coord.) (2009). Referentiel pedologique 2008.
AFES, Editions Quae, Versailles. Annex 1, pp. 327-355.

## See also

[`classify_rp2008`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_rp2008.md).

## Examples

``` r
humus_form_rp(make_rp2008_example("organosols_holorganiques"))$label
#> [1] "tangel"
```
