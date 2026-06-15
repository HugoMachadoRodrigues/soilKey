# SiBCS B textural relação-textural (item h) — v0.9.138

**Phase 3 follow-up.** Implements the verbatim Embrapa (2018) SiBCS Cap 2 p.56
item (h) — the proportional B/A textural ratio — that was deferred twice (v0.9.134,
v0.9.136) as "high-risk rewire of the dominant Argissolo/Latossolo diagnostic".

## Method

1. Focused 2-agent Workflow: (A) trace `argic → B_textural` and the key
   precedence that decides Argissolos vs Latossolos vs Nitossolos vs Planossolos;
   (B) characterize the verbatim (h) ratio + footnote-4 control section and the
   concrete delta vs the WRB argic absolute clay-increase.
2. **Hand band-analysis** of (h) vs argic across A-clay ranges.
3. Implement (h) faithfully, UNION into `B_textural`, **measure** on BDsolos RJ +
   Redape vs the captured baseline, ship only if non-regressing.

## The decisive finding — (h) is a near-subset of argic

`argic` (WRB 2022, the current `B_textural`) uses absolute thresholds:
A<15% → +6 pp; 15–50% → ratio ≥1.4; ≥50% → +20 pp.
SiBCS (h) uses A-clay-keyed ratios: <15% → >1.80; 15–40% → >1.70; >40% → >1.50.

Band-by-band, **argic is more permissive than (h) in every band except very sandy
A**:

| A clay | argic | (h) | more permissive |
|---|---|---|---|
| <7.5% | +6 pp | ratio >1.80 (smaller abs. jump) | **(h)** |
| 7.5–15% | +6 pp | ratio >1.80 | argic |
| 15–40% | ratio ≥1.40 | ratio >1.70 | **argic** |
| 40–50% | ratio ≥1.40 | ratio >1.50 | **argic** |
| ≥50% | +20 pp | ratio >1.50 (⇒ ≥+25 pp) | **argic** |

The agent's `delta_vs_argic` independently struggled to construct an
"(h)-passes-argic-fails" case, confirming the subset relationship. The only real
divergence: **very sandy A (clay <~7.5%)**, e.g. A=5% / B=10% — argic fails (+5 pp
< 6) but (h) passes (ratio 2.0 > 1.80).

## Implementation (minimal, union-only)

- New internal `test_ratio_textural_sibcs(h)` (R/utils-diagnostic-tests.R) —
  the (h) ratio over the footnote-4 control section (A<15cm → 30cm B window;
  A≥15cm → 2×A-thickness window; B excludes BC; thickness-weighted means).
- `B_textural` UNIONs `h_ratio$layers` into `argic`'s result **only when (h)
  passes**. Since this can only flip FALSE→TRUE (add a sandy-A pass), it is
  byte-identical on every argic-passing soil.

## Gate results — measured benchmark-neutral

| metric | baseline (v0.9.137) | (h)-union (v0.9.138) |
|---|---|---|
| BDsolos RJ order acc | 0.4141 | **0.4141** |
| BDsolos Latossolos recall | 17/114 | 17/114 |
| BDsolos Argissolos recall | 175/240 | 175/240 |
| BDsolos Cambissolos recall | 16/90 | 16/90 |
| BDsolos Neossolos recall | 44/57 | 44/57 |
| Redape order acc | 0.6383 | **0.6383** |
| Redape subgrupo acc | 0.2706 | **0.2706** |

**The premise is measured-refuted:** `B_textural=argic` was not under-firing on
(h) — argic already covered (h) except for sandy-A edge cases absent from these
datasets. The union is a *correctness* improvement (item (h) is now genuinely
implemented per the verbatim, including the footnote-4 control section) at **zero
accuracy cost and zero regression risk**.

- 44 canonical fixtures byte-identical (the union never removes an argic pass);
  +5 lock-in tests (test-v09138); full suite green; `--as-cran` codoc OK.

## Still deferred (honest)

- **(i) cerosidade** sub-rules (i.1–i.4) — the verbatim path for ratio-1.4–1.7
  mid-clay soils that lack the (h) ratio; needs clay-skin morphology
  (`clay_films_amount`/`_strength`) which is data-sparse in the benchmarks. Note
  this is the band where argic is *over*-permissive vs SiBCS (admits 1.4–1.7
  soils without cerosidade) — a future tightening, not a loosening.
- **(f)** E-horizon and **(j)** lithologic-discontinuity paths — designation-aware
  but not dedicated tests.
