# KSSL+NASIS v0.9.26 multi-level benchmark — 2026-05-03

A/B comparison of v0.9.25 vs v0.9.26 on the same KSSL+NASIS sample
(n=865), identical quality filter. v0.9.26 adds the per-system
threshold infrastructure (system parameter on test_clay_increase_argic
and argic) but keeps argillic_usda routed to the WRB threshold set.
Therefore the benchmark numbers should be -- and are -- identical.

## Results

| Level         | v0.9.25 | v0.9.26 | Delta |
|---------------|---:|---:|---:|
| **Order**     | 37.23 % | 37.23 % | 0.00 pp |
| **Suborder**  | 17.84 % | 17.84 % | 0.00 pp |
| **Great Group** | 10.34 % | 10.34 % | 0.00 pp |
| **Subgroup**  | 4.97 %  | 4.97 %  | 0.00 pp |

## Why no behavioral change

The v0.9.26 release is intentionally regression-safe. The motivation
(narrowing the haplargids -> haplocambids and argiustolls -> hapludolls
GG misses by relaxing the argic clay-increase thresholds to KST 13ed
specs) was implemented as the new \code{system} parameter, but
empirically routing \code{argillic_usda} to the looser thresholds
WITHOUT also implementing the KST 13ed clay-illuviation test (clay
films / oriented clays / lamellae) produced a NET REGRESSION:

| Level | v0.9.25 (WRB thresh) | v0.9.26 (KST thresh, no clay-films) | Delta |
|---|---:|---:|---:|
| Order        | 37.23 % | 35.95 % | -1.28 pp |
| Suborder     | 17.84 % | 16.92 % | -0.92 pp |
| Great Group  | 10.34 % |  9.99 % | -0.35 pp |
| Subgroup     |  4.97 % |  4.62 % | -0.35 pp |

The looser thresholds without clay-films verification produce many
false-positive argillic detections, which then mis-route genuinely
non-argillic profiles to argillic-bearing Orders.

## Roadmap (v0.9.27+)

The hypothesis is that the looser KST thresholds, paired with a
clay-illuviation test (probably the NASIS pediagfeatures `argillic`
flag), will produce a NET POSITIVE lift at Great Group level by
closing the haplargids -> haplocambids / argiustolls -> hapludolls
gap. This is the v0.9.27 work item.

## Reproducibility

Same as the v0.9.25 report. Re-running the benchmark after a `git
checkout v0.9.25` should produce identical numbers (modulo small
bootstrap CI variation).
