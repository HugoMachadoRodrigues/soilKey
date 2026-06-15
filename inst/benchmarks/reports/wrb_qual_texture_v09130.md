# WRB 2022 texture qualifiers — audit (v0.9.130, Fix D slice 2)

Five texture qualifiers checked against the verbatim WRB 2022 PDF (Ch 5).

| qualifier | WRB 2022 (verbatim) | code | verdict |
|---|---|---|---|
| **Clayic** | texture class clay / sandy clay / silty clay, ≥ 30 cm in 100 cm | `clay ≥ 60%` | **BUG — fixed** |
| Arenic | texture class sand / loamy sand, ≥ 30 cm | delegates to `arenic_texture` | OK |
| Loamic | loam / sandy loam / clay loam / sandy clay loam / silty clay loam, ≥ 30 cm | `clay 8–40 & silt ≥ 15` proxy | OK (proxy) |
| Siltic | silt-rich classes, ≥ 30 cm | `clay < 35 & silt ≥ 50` proxy | OK (proxy) |
| Skeletic | ≥ 40 % coarse fragments | `coarse_fragments_pct ≥ 40` | OK |

## The Clayic bug

WRB 2022 Clayic = the **clay, sandy clay, or silty clay** texture classes. On
the texture triangle those are:

- clay: clay ≥ 40 %
- silty clay: clay ≥ 40 %, silt ≥ 40 %
- sandy clay: clay ≥ 35 %, sand ≥ 45 %

i.e. `clay ≥ 40 OR (clay ≥ 35 AND sand ≥ 45)`. The code required `clay ≥ 60 %`,
so it **under-fired** for every soil with 40–60 % clay (a large fraction of
clayey soils, including most Ferralsols/Nitisols). Fixed to the proper
texture-class test.

## Verification

- Verbatim WRB 2022 PDF (Ch 5, p130).
- Full suite **5642 pass / 0 fail**; +5 unit tests.
- Canonical fixtures unaffected (clay values fall outside the new 40–60 % band
  at Clayic-eligible depths, so no name changed).

## Fix D progress

Slices shipped: base-status (v0.9.129), texture (v0.9.130). Remaining families
to audit against the PDF: depth/contact (Lithic/Leptic/Densic/Petric/…),
intensity specifiers (Hyper/Hypo/Proto for salic/sodic/calcic/gypsic/natric),
chemical (Magnesic/Carbonatic/Chloridic/Sulfatic/Toxic/…), organic
(Fibric/Floatic/Ombric/Rheic/…), andic/spodic, colour/morphology, and
technic/anthropic.
