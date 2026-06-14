# Humult criterion 1 — KSSL before/after gate (v0.9.126, Fix A)

Re-enabling KST 13ed key **HB criterion 1** (≥ 0.9 % organic carbon, weighted
average, in the upper 15 cm of the argillic/kandic horizon) in
`humult_qualifying_usda`, validated on **2895** real KSSL+NASIS pedons that carry
a `reference_usda_subgroup`, classified at subgroup level **before** (v0.9.125,
criterion-2-only) and **after** (criterion 1 restored).

## The window-anchoring decision

Criterion 1 was deferred in v0.9.113–125 because a naïve implementation
(window = upper 15 cm starting at `argillic_within_usda`'s reported top)
inherited a top-detection artifact: `argic()`'s deliberate **"min-above"**
heuristic (v0.9.23, added to catch gradual FEBR Hapludalfs clay increases such
as 13→15→21→27→31) can admit a transitional B that has *no* clay increase over
the horizon directly above it. That inflates the OC window with low-carbon
subsoil and produces a **false-positive Humult**.

The fix anchors the 15 cm window at the **illuvial onset** — the shallowest
argic/kandic layer whose clay exceeds the horizon immediately above it — instead
of the diagnostic's reported top. This removes the artifact **without touching
the high-risk `argic()` core** (which drives Argissolos/Luvisols/Acrisols across
all three systems).

## Result

| metric | value |
|---|---:|
| evaluated pedons | 2895 |
| changed predictions | **1** |
| improved (now matches reference) | 0 |
| **worsened (was-correct → now-wrong) at subgroup level** | **0** |
| subgroup exact-match before | 3.45 % |
| subgroup exact-match after | 3.45 % |

Determinism was verified first (identical code, same session 0/400 and
cross-session 0/2895), so the single diff is wholly attributable to the change.

## The one changed pedon (id 6495) — book-correct

| | label |
|---|---|
| reference | `typic hapludults` (Udult) |
| before (v0.9.125) | `typic rhodudults` (Udult — wrong great group) |
| after (v0.9.126) | `typic haplohumults` (Humult) |

Horizons: Ap 0–10 (clay 22.5, OC 4.28); **Bt1 10–20 (clay 32.0, OC 1.15)**;
**Bt2 20–38 (clay 62.2, OC 0.87)**; Bt3–Bt5 below. The argillic onset is Bt1
(clay 32.0 > Ap 22.5), so the upper 15 cm of the argillic is **10–25 cm**, where
the weighted-average OC is `(1.15·10 + 0.87·5)/15 = 1.06 %` ≥ 0.9 %. By the
verbatim KST 13ed key HB this profile **is** a Humult; the reference's
"Hapludults" label is the outlier (reference noise — differing OC data). Both
before and after already disagreed with the reference's great group, so there is
**no previously-correct → wrong flip** at the gate's decision level.

The previously-deferred false-positive (KSSL 1828: A 6.7 → E 16.6 → B 15.8 → Bt
20.5; `argic()` admits the no-increase B) now correctly stays **out** of
Humults: the onset moves to the true Bt and the upper-15 cm OC is 0.33 % < 0.9 %.

## Verification

- KSSL gate: **0 worsened** (above).
- 44 canonical fixtures: **byte-identical** (no expected label changed).
- Full suite: **5590 pass / 0 fail** (100 warnings + 23 skips are the known
  data-dependent benchmark tests).
- `R CMD check --as-cran`: unchanged (2 vignette-env WARNINGs + 1 NOTE).
