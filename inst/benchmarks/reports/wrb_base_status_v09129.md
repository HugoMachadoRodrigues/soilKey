# WRB 2022 base-status qualifiers (v0.9.129, Fix D part 1)

The WRB qualifier audit (Fix D) opened with the most-used qualifiers, and the
first finding was a **criterion-level** divergence, not a threshold typo: the
base-status family implemented the obsolete **WRB 2014 base-saturation**
criterion instead of the **WRB 2022 exchangeable-Al-vs-bases** criterion.

## Verbatim WRB 2022 (Ch 5, p131-133)

| qualifier | WRB 2022 criterion | old code |
|---|---|---|
| Dystric | exch. Al > bases in **half or more** of 20-100 cm | `bs_pct < 50` throughout |
| Eutric | exch. bases ≥ Al in the **major part** | `bs_pct ≥ 50` throughout |
| Hyperdystric | Al > bases **throughout** AND Al > 4×bases (Al-sat > 80 %) in major part | `bs_pct < 5` throughout |
| Hypereutric | bases ≥ Al throughout AND bases ≥ 4×Al (Al-sat ≤ 20 %) in major part | `bs_pct ≥ 80` throughout |

Mineral layers use `al_sat_pct` (primary) or `al_cmol` vs the summed base
cations; organic layers (`oc_pct ≥ 20`) use the Histosol pH branch
(pH_water 5.5, or 4.5/6.5 for the Hyper- variants). The Epi-/Endo- variants
apply the same criterion restricted to 20-50 / 50-100 cm.

## Strict policy (user decision)

No base-saturation fallback: where no exchangeable-Al datum is present the
result is **NA**, not a guess from `bs_pct`. This is more faithful but reduces
coverage on profiles that carry only base saturation.

## Why this matters — the variable-charge showcase

The canonical Ferralsol has base **saturation** of 24 % (against its pH7 CEC of
8.0) — "Dystric" by the 2014 rule. But its **effective** exchange (ECEC ≈ 2.6)
is base-dominated: bases ≈ 1.9 > exchangeable Al ≈ 0.7. WRB 2022 keys base
status on Al-vs-bases, so it is **Eutric**. This decoupling of base status from
base saturation for variable-charge soils is precisely what the 2014→2022
redefinition was made for. The canonical Cambisol (bases ≫ 4×Al throughout)
keys as the more-specific **Hypereutric**.

## Verification

- Verbatim WRB 2022 PDF (Ch 5, p131-133) — the authoritative ground truth (WRB
  has no machine-readable criteria in-package; KSSL is USDA-labelled and blind
  to WRB).
- Full suite **5621 pass / 0 fail**; +21 unit tests (`test-v09129-...`).
- The unit tests that encoded the old base-saturation criterion, and the CM/FR
  canonical-fixture expectations, were updated to the WRB 2022 results
  (validated as more-correct, with the rationale in each test).
- `coverage_report("wrb_qualifiers")` stub-detector taught the new delegation
  (E3 pattern) → coverage stays **229/234**.
- The FEBR-WRB benchmark is **not** locally re-runnable (the upstream FEBR repo
  was retired), so the change rests on verbatim correctness + the canonical
  fixtures, as for the Phase 2 WRB diagnostic fixes.

## Scope note (Fix D continues)

This is the first slice of the qualifier audit. ~170 `qual_*` carry hardcoded
thresholds; the remaining families (texture, depth specifiers, intensity
specifiers, etc.) are still to be audited against the WRB 2022 PDF.
