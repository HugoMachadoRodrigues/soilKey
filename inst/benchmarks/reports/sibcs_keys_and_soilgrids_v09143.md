# SiBCS keys verification + BDsolos coord-sign fix + SoilGrids measurement (v0.9.143)

Two deferred fronts, advanced carefully: (2) verify the SiBCS taxonomic keys
beyond Argissolos, and (3) measure the SoilGrids-by-coordinate gap-fill on a
coordinate-bearing dataset (BDsolos). Front 3 turned up — and fixed — a real
coordinate-sign bug; the SoilGrids lift itself is honestly negative.

## Front 2 — SiBCS keys verified faithful (no code change)

Extends the Argissolos key verification (faithful at subordem/GG/subgroup) to a
second order and a structural cross-check of all thirteen.

- **Cambissolos (Cap 6) verified faithful**: the YAML has 4 subordens —
  Hísticos (2 GGs), Húmicos (4), Flúvicos (8), Háplicos (12) — matching the
  verbatim Cap 6 exactly (subordens 1-4; Flúvicos 3.1-3.8 = 8 GGs; Háplicos
  4.1-4.12 = 12 GGs).
- **All 13 orders structurally sensible**: 44 subordens / 938 subgroups loaded;
  per-order subordem counts (P=5, C=4, M=4, L=4, …) match the chapters and are
  already regression-guarded by the per-order `test-sibcs-*` files.

Conclusion: two orders deeply verified (Argissolos + Cambissolos) plus the
all-13 structural cross-check confirm the keys are faithful. As the project has
found repeatedly, the Redape subgrupo ceiling (27.1%) is data-limited, not
key-limited — a full key-by-key re-audit is low-yield. No code change.

## Front 3 — BDsolos coordinate-sign bug FIXED

**The bug:** the BDsolos CSV records the hemisphere as a full Portuguese word
("Sul" / "Oeste"), but `.bdsolos_dms_to_decimal` matched only the single letter
(S/W/O). So the southern/western sign was never applied and **every Brazilian
coordinate was mirrored into the N/E hemisphere** (e.g. an RJ profile at
−21.52, −41.78 became +21.52, +41.78 — in the Red Sea). The deterministic key
ignores coordinates, so classification was unaffected, but SoilGrids / spatial
priors / mapping all queried the wrong location.

**The fix:** negate for any hemisphere starting with S (Sul), O (Oeste) or W.
Verified: RJ profile now resolves to −21.519, −41.779. Classification
byte-identical (the BDsolos confusion + Redape guards are unchanged, as the key
does not use coordinates). +9 unit tests (test-v09143).

## Front 3 — SoilGrids measurement (honest = slightly negative)

With coordinates now correct, the SoilGrids depth-fill was measured on a 40-pedon
sample of BDsolos profiles that have coordinates AND a missing CEC horizon (the
hardest, most data-poor pedons), classify_sibcs ON vs OFF against the reference
order:

| | order accuracy |
|---|---|
| OFF | 25.0% (10/40) |
| SoilGrids-ON | 22.5% (9/40) |
| changed | 8 |

**Slightly negative (−1 pedon).** Unlike EU-LUCAS (0→60%, where the pedons were
nearly empty so SoilGrids supplied almost everything), BDsolos profiles already
carry the key attributes (clay 88%); filling the residual gaps from a coarse 250 m
grid perturbs the deterministic key's decisions more than it helps. This is
consistent with the v0.9.120 / v0.9.140 finding: gap-fill is a data-recovery /
opt-in facility, not an accuracy lever on these reference data. SoilGrids stays
off by default and opt-in (`gapfill = list(method = "soilgrids")`).

## Gate

- BDsolos coord-sign fix: classification byte-identical (BDsolos confusion +
  Redape order guards unchanged); +9 unit tests; full suite green; `--as-cran`.
- Front 2: verification only (no code change); keys confirmed faithful.

This effectively completes the SiBCS Phase-3 audit (atributos, surface +
subsurface horizons, B textural, and the taxonomic keys) and the three deferred
fronts (calcic morphology v0.9.142; keys verification + coord fix + SoilGrids
measurement here).
