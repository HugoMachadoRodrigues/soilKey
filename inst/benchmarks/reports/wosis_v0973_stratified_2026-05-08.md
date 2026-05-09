# WoSIS stratified benchmark report -- v0.9.72 -- 2026-05-08

**Source:** `load_wosis_stratified_sample()` (130 pedons, 5 per RSG x 26 RSGs)

## Top-1 accuracy ladder

| Configuration | Accuracy |
|---|---:|
| baseline (no opt-ins) | 22/130 (16.9%) |
| +aqp engine | 21/130 (16.2%) |
| +aqp + ECEC + tex-morph (v0.9.69-70) | 21/130 (16.2%) |
| +full v0.9.69-72 stack (g/f/v inferences) | 21/130 (16.2%) |

## Per-RSG recall (baseline vs full v0.9.72 stack)

| RSG | n | baseline | +full | delta |
|---|---:|---:|---:|---:|
| Acrisol | 5 | 1 | 1 | +0 |
| Andosol | 5 | 0 | 0 | +0 |
| Arenosol | 5 | 4 | 4 | +0 |
| Calcisol | 5 | 2 | 2 | +0 |
| Cambisol | 5 | 3 | 3 | +0 |
| Chernozem | 5 | 0 | 0 | +0 |
| Cryosol | 5 | 0 | 0 | +0 |
| Ferralsol | 5 | 0 | 0 | +0 |
| Fluvisol | 5 | 2 | 0 | -2 |
| Gleysol | 5 | 0 | 0 | +0 |
| Gypsisol | 5 | 0 | 0 | +0 |
| Histosol | 5 | 5 | 5 | +0 |
| Kastanozem | 5 | 0 | 0 | +0 |
| Leptosol | 5 | 1 | 4 | +3 |
| Luvisol | 5 | 0 | 0 | +0 |
| Nitisol | 5 | 0 | 0 | +0 |
| Phaeozem | 5 | 0 | 0 | +0 |
| Planosol | 5 | 0 | 0 | +0 |
| Plinthosol | 5 | 0 | 0 | +0 |
| Podzol | 5 | 0 | 0 | +0 |
| Regosol | 5 | 4 | 2 | -2 |
| Solonchak | 5 | 0 | 0 | +0 |
| Solonetz | 5 | 0 | 0 | +0 |
| Stagnosol | 5 | 0 | 0 | +0 |
| Umbrisol | 5 | 0 | 0 | +0 |
| Vertisol | 5 | 0 | 0 | +0 |
