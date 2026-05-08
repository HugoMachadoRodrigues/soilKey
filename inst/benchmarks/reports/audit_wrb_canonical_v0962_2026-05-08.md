# soilKey WRB 2022 audit vs canonical (NCSS-tech)

**Date**: 2026-05-08  
**soilKey version**: 0.9.64  
**Canonical source**: NCSS-tech/SoilTaxonomy, **Reference**: IUSS Working Group WRB (2022). ## Coverage summary

| Element                | Canonical | Implemented | Missing |
|------------------------|----------:|------------:|--------:|
| Reference Soil Groups  |        32 |          32 |       0 |
| Principal qualifiers   |       131 |         131 |       0 |
| Supplementary qualif.  |       170 |         170 |       0 |

## Reference Soil Groups

### Implemented

- `ACRISOLS`
- `ALISOLS`
- `ANDOSOLS`
- `ANTHROSOLS`
- `ARENOSOLS`
- `CALCISOLS`
- `CAMBISOLS`
- `CHERNOZEMS`
- `CRYOSOLS`
- `DURISOLS`
- `FERRALSOLS`
- `FLUVISOLS`
- `GLEYSOLS`
- `GYPSISOLS`
- `HISTOSOLS`
- `KASTANOZEMS`
- `LEPTOSOLS`
- `LIXISOLS`
- `LUVISOLS`
- `NITISOLS`
- `PHAEOZEMS`
- `PLANOSOLS`
- `PLINTHOSOLS`
- `PODZOLS`
- `REGOSOLS`
- `RETISOLS`
- `SOLONCHAKS`
- `SOLONETZ`
- `STAGNOSOLS`
- `TECHNOSOLS`
- `UMBRISOLS`
- `VERTISOLS`

### Missing (canonical NOT in soilKey)

(none -- all 32 canonical RSGs are implemented)

## Principal qualifiers

Total canonical: 131  
Implemented (heuristic match in R/qualifiers-wrb*.R): 131 (100.0%)

### Missing principal qualifiers (top 50)


## Supplementary qualifiers

Total canonical: 170  
Implemented (heuristic match): 170 (100.0%)

### Missing supplementary qualifiers (top 50)


## Caveats

- Heuristic matching: a qualifier is 'implemented' if its name
  appears (case-insensitive, word-boundary) anywhere in the
  WRB-related R sources. False positives possible if the name
  collides with an unrelated identifier; false negatives if the
  qualifier was implemented under a different identifier.
- Canonical RSG names are singular ALL-CAPS in WRB 2022 text;
  soilKey uses plural Title Case. The detector tries both.
- 'Missing' here means 'not detected by the heuristic'. Manual
  review needed to confirm a real coverage gap before opening
  v0.9.63 implementation tickets.
