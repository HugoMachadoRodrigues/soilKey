# WRB 2022 colour qualifiers — audit (v0.9.131, Fix D slice 3)

Chromic / Rhodic / Xanthic checked against the verbatim WRB 2022 PDF (Ch 5).

## Chromic (p130)

> a layer, ≥ 30 cm thick, between 25 and 150 cm, that shows evidence of soil
> formation as defined in criterion 3 of the cambic horizon and that has, in
> ≥ 90 % of its exposed area, a Munsell hue redder than 7.5YR and a chroma > 4,
> both moist, and that **does not meet the Rhodic qualifier**.

Old code: hue + chroma only. **Missing**: the ≥ 30 cm thickness, the
soil-formation evidence, and (importantly) the **not-Rhodic exclusion** — so
Chromic and Rhodic could co-occur (the canonical Ferralsol carried both). All
three added; hue test moved to `.munsell_hue_units` (< 15 = redder than 7.5YR).

## Rhodic (p145)

> a layer, ≥ 30 cm thick, between 25 and 150 cm, that shows evidence of soil
> formation (cambic criterion 3) and that has hue redder than 5YR moist, value
> < 4 moist, and a dry value not more than one unit higher than the moist value.

Old code: hue + value < 4 only. **Missing**: the ≥ 30 cm thickness, the
soil-formation evidence, and the value-dry clause. All added (the value-dry
clause refine-when-present).

## Xanthic (p151)

> a ferralic horizon with a subhorizon ≥ 30 cm thick (≤ 75 cm from the ferralic
> top) with hue 7.5YR or yellower, value ≥ 4, chroma ≥ 5 (moist).

Old code's hue regex (`7.5YR|10YR|2.5Y|5Y`) missed 7.5Y/10Y; widened to
`.munsell_hue_units ≥ 15`. Added the ≥ 30 cm thickness.

## Effect on the canonical Ferralsol

| | before | after |
|---|---|---|
| Rhodic | yes | yes |
| Chromic | yes (wrong) | **no** (Chromic excludes Rhodic) |
| name | …Rhodic Chromic Ferralsol | …Rhodic Ferralsol |

The FR fixture passes cambic criterion 3 (via the AB→BA chroma increase), so the
new soil-formation requirement does not drop it.

## Verification

- Verbatim WRB 2022 PDF (Ch 5, p130 / p145 / p151).
- Full suite **5655 pass / 0 fail**; +6 unit tests; FR canonical name updated
  (Chromic dropped, validated as more-correct).
- `R CMD check --as-cran`: codoc OK.

## Fix D progress

Slices shipped: base-status (v0.9.129), texture (v0.9.130), colour (v0.9.131).
Remaining families: depth/contact, intensity specifiers, chemical, organic,
andic/spodic, technic/anthropic, Tier-3 extras.
