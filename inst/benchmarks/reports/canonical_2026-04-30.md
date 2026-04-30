# soilKey -- canonical fixtures benchmark (offline)

**Run:** 2026-04-30 15:20:15 EDT &middot; **Package version:** 0.9.9 &middot; **Fixtures:** 31

This is the network-free benchmark over the canonical fixtures
shipped under `inst/extdata/`. Each fixture is a real published
profile (WRB 2022 didactic exemplars, ISRIC ISMC monoliths, Soil
Atlas of Europe), tagged with its known target RSG / SiBCS order /
USDA order. The full-WoSIS run (see `run_wosis_benchmark()`)
produces the paper-grade numbers; this offline run is the
release-time sanity check.

## Top-1 agreement

| System | n | match | top-1 |
|---|---:|---:|---:|
| WRB 2022   | 31 | 31 | 1.000 |
| SiBCS 5    | 20 | 15 | 0.750 |
| USDA ST 13 | 31 | 23 | 0.742 |

## Evidence-grade distribution

**WRB 2022**

  - A: 31

**SiBCS 5**

  - A: 31

**USDA ST 13**

  - A: 31

## Per-fixture results

| Fixture      | Target WRB    | Assigned WRB  | OK   | Target SiBCS  | Assigned SiBCS | OK   | Target USDA   | Assigned USDA | OK   |
|---|---|---|:---:|---|---|:---:|---|---|:---:|
| acrisol      | Acrisols      | Acrisols      | OK   | Argissolos    | Argissolos    | OK   | Ultisols      | Ultisols      | OK   |
| alisol       | Alisols       | Alisols       | OK   | Argissolos    | Argissolos    | OK   | Ultisols      | Ultisols      | OK   |
| andosol      | Andosols      | Andosols      | OK   | Cambissolos   | Organossolos  | MISS | Andisols      | Andisols      | OK   |
| anthrosol    | Anthrosols    | Anthrosols    | OK   | NA            | Neossolos     | .    | Inceptisols   | Mollisols     | MISS |
| arenosol     | Arenosols     | Arenosols     | OK   | Neossolos     | Neossolos     | OK   | Entisols      | Entisols      | OK   |
| calcisol     | Calcisols     | Calcisols     | OK   | NA            | Neossolos     | .    | Aridisols     | Aridisols     | OK   |
| cambisol     | Cambisols     | Cambisols     | OK   | Cambissolos   | Cambissolos   | OK   | Inceptisols   | Inceptisols   | OK   |
| chernozem    | Chernozems    | Chernozems    | OK   | Chernossolos  | Neossolos     | MISS | Mollisols     | Mollisols     | OK   |
| cryosol      | Cryosols      | Cryosols      | OK   | NA            | Cambissolos   | .    | Gelisols      | Gelisols      | OK   |
| durisol      | Durisols      | Durisols      | OK   | NA            | Neossolos     | .    | Aridisols     | Aridisols     | OK   |
| ferralsol    | Ferralsols    | Ferralsols    | OK   | Latossolos    | Latossolos    | OK   | Oxisols       | Oxisols       | OK   |
| fluvisol     | Fluvisols     | Fluvisols     | OK   | Neossolos     | Luvissolos    | MISS | Entisols      | Alfisols      | MISS |
| gleysol      | Gleysols      | Gleysols      | OK   | Gleissolos    | Gleissolos    | OK   | Entisols      | Inceptisols   | MISS |
| gypsisol     | Gypsisols     | Gypsisols     | OK   | NA            | Neossolos     | .    | Aridisols     | Aridisols     | OK   |
| histosol     | Histosols     | Histosols     | OK   | Organossolos  | Organossolos  | OK   | Histosols     | Histosols     | OK   |
| kastanozem   | Kastanozems   | Kastanozems   | OK   | Chernossolos  | Neossolos     | MISS | Mollisols     | Mollisols     | OK   |
| leptosol     | Leptosols     | Leptosols     | OK   | Neossolos     | Neossolos     | OK   | Entisols      | Entisols      | OK   |
| lixisol      | Lixisols      | Lixisols      | OK   | Argissolos    | Argissolos    | OK   | Alfisols      | Alfisols      | OK   |
| luvisol      | Luvisols      | Luvisols      | OK   | Luvissolos    | Luvissolos    | OK   | Alfisols      | Alfisols      | OK   |
| nitisol      | Nitisols      | Nitisols      | OK   | Nitossolos    | Argissolos    | MISS | Alfisols      | Ultisols      | MISS |
| phaeozem     | Phaeozems     | Phaeozems     | OK   | Chernossolos  | Chernossolos  | OK   | Mollisols     | Mollisols     | OK   |
| planosol     | Planosols     | Planosols     | OK   | Planossolos   | Planossolos   | OK   | Alfisols      | Alfisols      | OK   |
| plinthosol   | Plinthosols   | Plinthosols   | OK   | Plintossolos  | Plintossolos  | OK   | Oxisols       | Inceptisols   | MISS |
| podzol       | Podzols       | Podzols       | OK   | Espodossolos  | Espodossolos  | OK   | Spodosols     | Spodosols     | OK   |
| retisol      | Retisols      | Retisols      | OK   | NA            | Neossolos     | .    | Alfisols      | Inceptisols   | MISS |
| solonchak    | Solonchaks    | Solonchaks    | OK   | NA            | Neossolos     | .    | Aridisols     | Aridisols     | OK   |
| solonetz     | Solonetz      | Solonetz      | OK   | NA            | Luvissolos    | .    | Aridisols     | Alfisols      | MISS |
| stagnosol    | Stagnosols    | Stagnosols    | OK   | NA            | Cambissolos   | .    | Inceptisols   | Inceptisols   | OK   |
| technosol    | Technosols    | Technosols    | OK   | NA            | Neossolos     | .    | Entisols      | Mollisols     | MISS |
| umbrisol     | Umbrisols     | Umbrisols     | OK   | NA            | Cambissolos   | .    | Inceptisols   | Inceptisols   | OK   |
| vertisol     | Vertisols     | Vertisols     | OK   | Vertissolos   | Vertissolos   | OK   | Vertisols     | Vertisols     | OK   |

## Notes

- A '.' in a target column indicates the fixture has no canonical
  target in that system (e.g. Solonchak / Solonetz / Calcisol have
  no direct SiBCS analogue in the 5ª edição).
- Cross-system targets follow Schad (2023) Annex Table 1 (WRB <->
  USDA) and the SiBCS 5ª ed. Annex A correspondence guide.
- Sub-level (Subgroup / Família) concordance is not tested here --
  only the highest categorical level (RSG / Ordem / Order). Sub-
  level concordance is reserved for the WoSIS run.

---

_Report emitted by `run_canonical_benchmark()` in_
_`inst/benchmarks/run_wosis_benchmark.R`._
