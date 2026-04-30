# soilKey -- canonical fixtures benchmark (offline)

**Run:** 2026-04-30 15:39:37 EDT &middot; **Package version:** 0.9.9 &middot; **Fixtures:** 31

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
| SiBCS 5    | 20 | 20 | 1.000 |
| USDA ST 13 | 31 | 31 | 1.000 |

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
| acrisol      | Acrisols                   | Acrisols      | OK   | Argissolos                 | Argissolos    | OK   | Ultisols                   | Ultisols      | OK   |
| alisol       | Alisols                    | Alisols       | OK   | Argissolos                 | Argissolos    | OK   | Ultisols                   | Ultisols      | OK   |
| andosol      | Andosols                   | Andosols      | OK   | Cambissolos                | Cambissolos   | OK   | Andisols                   | Andisols      | OK   |
| anthrosol    | Anthrosols                 | Anthrosols    | OK   | .                          | Neossolos     | .    | Inceptisols / Mollisols / Alfisols | Mollisols     | OK   |
| arenosol     | Arenosols                  | Arenosols     | OK   | Neossolos                  | Neossolos     | OK   | Entisols                   | Entisols      | OK   |
| calcisol     | Calcisols                  | Calcisols     | OK   | .                          | Cambissolos   | .    | Aridisols                  | Aridisols     | OK   |
| cambisol     | Cambisols                  | Cambisols     | OK   | Cambissolos                | Cambissolos   | OK   | Inceptisols                | Inceptisols   | OK   |
| chernozem    | Chernozems                 | Chernozems    | OK   | Chernossolos               | Chernossolos  | OK   | Mollisols                  | Mollisols     | OK   |
| cryosol      | Cryosols                   | Cryosols      | OK   | .                          | Cambissolos   | .    | Gelisols                   | Gelisols      | OK   |
| durisol      | Durisols                   | Durisols      | OK   | .                          | Neossolos     | .    | Aridisols                  | Aridisols     | OK   |
| ferralsol    | Ferralsols                 | Ferralsols    | OK   | Latossolos                 | Latossolos    | OK   | Oxisols                    | Oxisols       | OK   |
| fluvisol     | Fluvisols                  | Fluvisols     | OK   | Neossolos                  | Neossolos     | OK   | Entisols                   | Entisols      | OK   |
| gleysol      | Gleysols                   | Gleysols      | OK   | Gleissolos                 | Gleissolos    | OK   | Entisols / Inceptisols     | Inceptisols   | OK   |
| gypsisol     | Gypsisols                  | Gypsisols     | OK   | .                          | Neossolos     | .    | Aridisols                  | Aridisols     | OK   |
| histosol     | Histosols                  | Histosols     | OK   | Organossolos               | Organossolos  | OK   | Histosols                  | Histosols     | OK   |
| kastanozem   | Kastanozems                | Kastanozems   | OK   | Chernossolos               | Chernossolos  | OK   | Mollisols                  | Mollisols     | OK   |
| leptosol     | Leptosols                  | Leptosols     | OK   | Neossolos                  | Neossolos     | OK   | Entisols                   | Entisols      | OK   |
| lixisol      | Lixisols                   | Lixisols      | OK   | Argissolos                 | Argissolos    | OK   | Alfisols                   | Alfisols      | OK   |
| luvisol      | Luvisols                   | Luvisols      | OK   | Luvissolos                 | Luvissolos    | OK   | Alfisols                   | Alfisols      | OK   |
| nitisol      | Nitisols                   | Nitisols      | OK   | Nitossolos                 | Nitossolos    | OK   | Alfisols / Ultisols / Oxisols / Inceptisols | Ultisols      | OK   |
| phaeozem     | Phaeozems                  | Phaeozems     | OK   | Chernossolos               | Chernossolos  | OK   | Mollisols                  | Mollisols     | OK   |
| planosol     | Planosols                  | Planosols     | OK   | Planossolos                | Planossolos   | OK   | Alfisols                   | Alfisols      | OK   |
| plinthosol   | Plinthosols                | Plinthosols   | OK   | Plintossolos               | Plintossolos  | OK   | Oxisols / Ultisols / Inceptisols | Inceptisols   | OK   |
| podzol       | Podzols                    | Podzols       | OK   | Espodossolos               | Espodossolos  | OK   | Spodosols                  | Spodosols     | OK   |
| retisol      | Retisols                   | Retisols      | OK   | .                          | Neossolos     | .    | Alfisols / Inceptisols / Spodosols | Inceptisols   | OK   |
| solonchak    | Solonchaks                 | Solonchaks    | OK   | .                          | Cambissolos   | .    | Aridisols                  | Aridisols     | OK   |
| solonetz     | Solonetz                   | Solonetz      | OK   | .                          | Luvissolos    | .    | Aridisols / Alfisols / Mollisols | Alfisols      | OK   |
| stagnosol    | Stagnosols                 | Stagnosols    | OK   | .                          | Cambissolos   | .    | Inceptisols                | Inceptisols   | OK   |
| technosol    | Technosols                 | Technosols    | OK   | .                          | Neossolos     | .    | Entisols                   | Entisols      | OK   |
| umbrisol     | Umbrisols                  | Umbrisols     | OK   | .                          | Cambissolos   | .    | Inceptisols                | Inceptisols   | OK   |
| vertisol     | Vertisols                  | Vertisols     | OK   | Vertissolos                | Vertissolos   | OK   | Vertisols                  | Vertisols     | OK   |

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
