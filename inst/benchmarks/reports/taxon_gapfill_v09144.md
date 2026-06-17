# Non-circular predicted-taxon gap-fill (v0.9.144)

Closes the deferred item *"gap-fill não-circular por táxon-predito"*. This is the
first gap-fill method in the package that produces a **net positive** accuracy
lift on reference data that already carries the key attributes — the regime where
the SoilGrids depth-fill (v0.9.143) was slightly *negative*.

## The method

Two new exported functions in `R/gapfill.R`:

1. **`build_taxon_profiles(pedons, ref_field, attrs)`** — for each taxon (the
   first word of the reference label, normalised by `.taxon_key`: lowercase,
   accent-free, de-pluralised), averages each continuous attribute across the
   calibration pedons into the six standard depth slices (0-5 … 100-200 cm).
   Returns `taxon -> attr -> numeric(6)`.

2. **`gapfill_by_predicted_taxon(pedon, taxon_profiles, system, …)`** — classifies
   the pedon with **no fill** to obtain a *provisional* taxon, then fills the
   missing cells from `taxon_profiles[[<that taxon>]]` via the shared
   `.interp_depth_profile()`. Filled cells are written `source = "inferred_prior"`
   (evidence grade C). Wired into the `.classify_apply_gapfill` dispatcher as
   `gapfill = list(method = "taxon", taxon_profiles = <...>)`; default off →
   byte-identical.

### Why it is non-circular

The fill is keyed on the **model's own prediction**, not the reference label.
Combined with calibrating the profiles on a set *disjoint* from the pedons being
filled (a train split), no reference label of a filled pedon ever informs its own
fill. The profile supplies taxon-typical *structure* (e.g. an Argissolo's Bt clay
bulge); the deterministic key then re-decides on the completed profile.

## Measurement — BDsolos-RJ, 2-fold cross-validated

722 RJ pedons with a SiBCS reference label, split 50/50 by index. For each fold,
profiles are built on the train half and the test half is classified twice
(fill-OFF vs predicted-taxon-ON), scored on the reference **order**. The two
folds are reciprocal (train↔test swapped), so every pedon is scored exactly once
as a held-out test case.

| fold | n | OFF | taxon-ON | Δ | changed |
|---|---|---|---|---|---|
| A (train odd / test even) | 360 | 123 | 126 | +3 | 65 |
| B (train even / test odd)  | 360 | 100 | 110 | +10 | 50 |
| **combined** | **720** | **223 (31.0%)** | **236 (32.8%)** | **+13 (+1.8 pp)** | **115** |

**Both folds positive.** 115/720 pedons changed classification; the net is +13.

### Confirmation on Redape (FEBR national reference, n=94)

The same non-circular 2-fold protocol was repeated on the Redape GeoTab set (94
pedons with a SiBCS reference label, profiles built per train half, the model's
prediction driving the fill on the held-out half):

| fold | n | OFF | taxon-ON | Δ | changed |
|---|---|---|---|---|---|
| A | 47 | 33 | 34 | +1 | 1 |
| B | 47 | 27 | 27 |  0 | 1 |
| **combined** | **94** | **60 (63.8%)** | **61 (64.9%)** | **+1 (+1.1 pp)** | **2** |

Same direction as BDsolos: a small **positive** lift (+1.1 pp), both folds
non-negative, with very few pedons disturbed (2/94). Redape carries richer
profiles than BDsolos-RJ (its OFF order accuracy is already 63.8%), so the
residual gaps the prior can act on are fewer — hence a smaller but still positive
delta. Across the two reference sets the predicted-taxon prior is consistently
non-negative and modestly positive, unlike the SoilGrids prior.

This contrasts with:

- **SoilGrids depth-fill (v0.9.143):** −1 pedon on the same family of data — a
  coarse 250 m spatial average perturbs the key more than it helps when the pedon
  already has its key attributes.
- **EU-LUCAS SoilGrids (v0.9.50/64):** 0 → 60%, but there the pedons were nearly
  empty, so the fill supplied almost everything.

The predicted-taxon prior wins where SoilGrids loses because it injects
*taxon-shaped* depth structure rather than a location average — exactly the
information the key uses to discriminate orders.

### Honesty / limits

- +1.8 pp (BDsolos-RJ) and +1.1 pp (Redape) are **modest** lifts, cross-validated
  on two independent reference sets. Real improvements, not ceiling-busters.
- The provisional-taxon step costs one extra classify per pedon.
- Still opt-in and off by default; the default classification path is unchanged
  and byte-identical (gate below).

## Gate

- **44 canonical fixtures byte-identical** (default path unchanged — gap-fill is
  opt-in).
- **+22 unit tests** (`test-v09144-taxon-gapfill.R`): `.taxon_key` normalisation;
  6-slice averaging; blank/NA-label skipping; fill-from-prediction + caller
  never mutated + grade-C provenance; no-op on unknown taxon; dispatcher routing
  `method = "taxon"`; unknown-method error lists `taxon`.
- Full suite green; `R CMD check --as-cran`.
- No schema change (uses the existing continuous gap-fill attribute set), so
  `pedon-schema.json` is untouched.
