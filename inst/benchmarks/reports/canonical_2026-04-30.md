# soilKey -- canonical fixtures benchmark (offline)

**Run:** 2026-04-30 14:43:37 EDT &middot; **Package version:** 0.9.8 &middot; **Fixtures:** 31

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
| WRB 2022   | 31 | 26 | 0.839 |
| SiBCS 5    | 20 | 13 | 0.650 |
| USDA ST 13 | 31 | 8 | 0.258 |

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
| acrisol      | Acrisols      | Acrisols      | OK   | Argissolos    | Argissolos    | OK   | Ultisols      | Entisols      | MISS |
| alisol       | Alisols       | Alisols       | OK   | Argissolos    | Argissolos    | OK   | Ultisols      | Entisols      | MISS |
| andosol      | Andosols      | Umbrisols     | MISS | Cambissolos   | Organossolos  | MISS | Andisols      | Entisols      | MISS |
| anthrosol    | Anthrosols    | Anthrosols    | OK   | NA            | Neossolos     | .    | Inceptisols   | Entisols      | MISS |
| arenosol     | Arenosols     | Arenosols     | OK   | Neossolos     | Neossolos     | OK   | Entisols      | Entisols      | OK   |
| calcisol     | Calcisols     | Calcisols     | OK   | NA            | Neossolos     | .    | Aridisols     | Entisols      | MISS |
| cambisol     | Cambisols     | Cambisols     | OK   | Cambissolos   | Cambissolos   | OK   | Inceptisols   | Entisols      | MISS |
| chernozem    | Chernozems    | Cambisols     | MISS | Chernossolos  | Neossolos     | MISS | Mollisols     | Entisols      | MISS |
| cryosol      | Cryosols      | Cryosols      | OK   | NA            | Cambissolos   | .    | Gelisols      | Gelisols      | OK   |
| durisol      | Durisols      | Durisols      | OK   | NA            | Neossolos     | .    | Aridisols     | Entisols      | MISS |
| ferralsol    | Ferralsols    | Ferralsols    | OK   | Latossolos    | Latossolos    | OK   | Oxisols       | Entisols      | MISS |
| fluvisol     | Fluvisols     | Fluvisols     | OK   | Neossolos     | Luvissolos    | MISS | Entisols      | Entisols      | OK   |
| gleysol      | Gleysols      | Cambisols     | MISS | Gleissolos    | Gleissolos    | OK   | Entisols      | Entisols      | OK   |
| gypsisol     | Gypsisols     | Gypsisols     | OK   | NA            | Neossolos     | .    | Aridisols     | Entisols      | MISS |
| histosol     | Histosols     | Histosols     | OK   | Organossolos  | Organossolos  | OK   | Histosols     | Histosols     | OK   |
| kastanozem   | Kastanozems   | Kastanozems   | OK   | Chernossolos  | Neossolos     | MISS | Mollisols     | Entisols      | MISS |
| leptosol     | Leptosols     | Leptosols     | OK   | Neossolos     | Neossolos     | OK   | Entisols      | Entisols      | OK   |
| lixisol      | Lixisols      | Lixisols      | OK   | Argissolos    | Argissolos    | OK   | Alfisols      | Entisols      | MISS |
| luvisol      | Luvisols      | Luvisols      | OK   | Luvissolos    | Luvissolos    | OK   | Alfisols      | Entisols      | MISS |
| nitisol      | Nitisols      | Nitisols      | OK   | Nitossolos    | Argissolos    | MISS | Alfisols      | Entisols      | MISS |
| phaeozem     | Phaeozems     | Phaeozems     | OK   | Chernossolos  | Chernossolos  | OK   | Mollisols     | Entisols      | MISS |
| planosol     | Planosols     | Stagnosols    | MISS | Planossolos   | Planossolos   | OK   | Alfisols      | Entisols      | MISS |
| plinthosol   | Plinthosols   | Plinthosols   | OK   | Plintossolos  | Argissolos    | MISS | Oxisols       | Entisols      | MISS |
| podzol       | Podzols       | Podzols       | OK   | Espodossolos  | Espodossolos  | OK   | Spodosols     | Spodosols     | OK   |
| retisol      | Retisols      | Retisols      | OK   | NA            | Neossolos     | .    | Alfisols      | Entisols      | MISS |
| solonchak    | Solonchaks    | Solonchaks    | OK   | NA            | Neossolos     | .    | Aridisols     | Entisols      | MISS |
| solonetz     | Solonetz      | Solonetz      | OK   | NA            | Luvissolos    | .    | Aridisols     | Entisols      | MISS |
| stagnosol    | Stagnosols    | Stagnosols    | OK   | NA            | Cambissolos   | .    | Inceptisols   | Entisols      | MISS |
| technosol    | Technosols    | Technosols    | OK   | NA            | Neossolos     | .    | Entisols      | Entisols      | OK   |
| umbrisol     | Umbrisols     | Umbrisols     | OK   | NA            | Cambissolos   | .    | Inceptisols   | Entisols      | MISS |
| vertisol     | Vertisols     | Cambisols     | MISS | Vertissolos   | Argissolos    | MISS | Vertisols     | Entisols      | MISS |

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
