# Redape subgroup: argissólico relação-textural tightening (v0.9.146)

User asked whether the Redape SiBCS **subgroup** accuracy (pinned at 27.1% across
every version) could be raised. A decomposition of the errors, then one
principled predicate tightening, lifts it to **32.9%** — without touching order
or great-group classification.

## Decomposition of the 27.1% ceiling (n=85 in-scope)

| origin of the error | n | % | code-fixable? |
|---|---|---|---|
| correct | 23 | 27% | — |
| order wrong (upstream) | 27 | 32% | no (= the order-63.8% ceiling, data-limited) |
| order ok, great-group wrong (upstream) | 22 | 26% | no (upstream) |
| order+GG ok, **subgroup wrong** | 13 | 15% | **partially** |

So **~79% of the subgroup miss is upstream (order/GG) or data-absent**, not a
subgroup-logic problem. Of the 13 truly subgroup-level errors, **9 were
over-fires** with one dominant pattern: the **`argissólico`** subgroup firing on
Latossolos the reference calls *típico*.

## Root cause (verified on the over-firing pedons)

`carater_argiluvico()` — the `argissólico` discriminator — passed via
`B_textural`, which unions the **argic** clay-increase (ratio ≥ 1.4). That is
*looser* than the SiBCS **relação textural** (Cap 1 item h: ratio > 1.5 / 1.7 /
1.8 by the A-clay band). The over-firing Latossolos showed the gradual latossolic
gradient, e.g.:

| pedon | clay A→B | argiluvico (old) | relação textural |
|---|---|---|---|
| PE_006 Escada | 38 → 59 | TRUE | FALSE (≈1.55 < 1.7) |
| PE_033 Sirinhaém | 35 → 55 | TRUE | FALSE |
| PE_035 Jaboatão | 44 → 56 | TRUE | FALSE |
| PE_038 Rio Formoso | 37 → 48 | TRUE | FALSE |

The reference pedologists correctly call these *típico*; the relação textural
correctly rejects them. So the fix is **more faithful to the verbatim**, not a
tune.

## The change

`carater_argiluvico()` additionally requires `test_ratio_textural_sibcs()` to
pass **when clay is recorded** (refine-when-present → byte-identical without
clay). Because `carater_argiluvico` appears only in `subgrupos/*.yaml` (never in
order or great-group keys), the change is **contained to the subgroup level**.

## Result & gate

- **Redape subgroup 27.1% → 32.9% (23 → 28 / 85, +5 pedons, +5.8 pp).**
- **Order and great-group byte-identical**: Redape order 63.8%, GG 42.4%
  unchanged; **BDsolos-RJ order 30.97% (223/720) unchanged** (matches the
  v0.9.144 baseline exactly). BDsolos carries no subgroup references, so Redape
  is the only subgroup-scoreable set.
- 44 canonical fixtures byte-identical (full suite green). One unit-test fixture
  updated as *validated-more-correct*: a 1.67-ratio gradient relabelled to a
  genuine 2.3-ratio textural B (the test's stated intent).
- `R CMD check --as-cran` = 1 NOTE (CRAN-incoming, benign).

## Honest scope

This is a **small, real** SiBCS accuracy gain (+5.8 pp on n=85). The bulk of the
Redape subgroup ceiling remains upstream order/GG error and data-absent *típico*
defaults (missing structure/cerosidade/mottle morphology in the reference set) —
neither code-fixable. This was the genuinely subgroup-level, code-fixable slice.
