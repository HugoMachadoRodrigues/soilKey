# External / closure gap-fill — measured on the local SiBCS benchmarks (v0.9.140)

Extends gap-fill beyond the v0.9.120 within-pedon depth interpolation with
(a) a definitional-closure fill and (b) a method dispatcher that makes the
existing external SoilGrids fill reachable from the classifiers. Then **measures**
the lift on the local SiBCS benchmarks (Redape, BDsolos RJ) — the honest result is
**data recovery, not accuracy**.

## The gap structure diagnostic (why closures, not statistical PTFs)

Per-horizon missingness on the two local SiBCS datasets:

| dataset | clay NA but sand+silt present | bs NA but cec+Ca+Mg present | al_sat measured |
|---|---|---|---|
| Redape (94 pedons, 444 hz) | **0** | 1 | **0%** (al_cmol 96%) |
| BDsolos RJ (722 pedons, 2572 hz) | **0** | 1 | 86% |

The gaps are **whole-horizon**: clay is NA only when sand AND silt are also NA;
base saturation is NA only when CEC and the exchangeable bases are also NA. So a
within-horizon proxy is essentially never available where the target is missing —
a statistical PTF calibrated on those proxies would have nothing to read. The one
real opening is **al_sat**, which Redape never reports yet is definitionally
derivable from the measured exchange complex (Al + bases).

## What was built

- **`gapfill_derive_horizon()`** — fills cells that are exact closures of other
  measured columns in the same horizon: the texture third (clay/silt/sand);
  `ecec = sum(bases) + al`; `al_sat = 100·al/ecec`; `bs = 100·sum(bases)/cec`.
  Requires Ca+Mg present before trusting `sum(bases)`. Writes
  `source = "inferred_prior"` (grade C; never displaces a measured value).
- **Method dispatcher** for the classifiers' `gapfill=` argument:
  `gapfill = list(method = c("interp" | "derive" | "soilgrids"), ...)`. This makes
  the pre-existing `apply_soilgrids_depth_prior()` (the LUCAS 0→60% external fill)
  reachable from `classify_*`/`benchmark_*` for the first time. `gapfill = TRUE`
  and `gapfill = <character>` keep their v0.9.120 interp meaning (back-compat);
  `gapfill = FALSE` (default) stays byte-identical.

## Measured ON/OFF (the honest result — NEUTRAL)

| benchmark | OFF | derive ON | Δ |
|---|---|---|---|
| Redape order | 0.6383 | 0.6383 | **+0.0000** |
| Redape grande-grupo | 0.4235 | 0.4235 | **+0.0000** |
| Redape subgrupo | 0.2706 | 0.2706 | **+0.0000** |
| BDsolos RJ order | 0.4141 | 0.4155 | +0.0014 (+1 pedon) |

`derive` fills **851 cells on Redape and 1,574 on BDsolos** (mostly al_sat) — yet
accuracy is flat. The recovered attributes are decision-redundant for the keys:
e.g. `al_sat` (Redape 0→96% recovered) does not move `carater_alitico`, which
already keys on the measured `V < 50` branch. This matches the v0.9.120 finding
(within-pedon interp measured KSSL-neutral-to-negative) and the project-wide
theme: **the accuracy ceiling on these reference datasets is whole-horizon
missingness + label noise, not derivable gaps.**

## The one real lever, and why it is unmeasurable here

The only fill that can touch the **whole-horizon** gaps (BDsolos CEC 40%, clay 88%)
is a **coordinate-based external prior** — SoilGrids-by-coordinate, which lifted
the EU-LUCAS WRB benchmark 0→60% (v0.9.50/64). It is now reachable via
`gapfill = list(method = "soilgrids", ...)`, but:

- **Redape has 0/94 coordinates** → SoilGrids cannot run on it at all.
- **BDsolos has 561/722 coordinates** → measurable in principle, but it is a live
  per-pedon ISRIC REST call (network, not CI-safe, 561 requests).
- A **per-taxon-mean** fill (the other option discussed) is **methodologically
  circular** for a benchmark: filling a profile from its reference taxon's mean
  profile leaks the answer into the input. A non-circular variant would have to
  fill from a *predicted* (fill-free) taxon and iterate — out of scope here.

So the external lever is delivered as an **opt-in capability** (off by default,
offline-testable via `depth_profiles=`), not a measured accuracy claim on the
SiBCS data.

## Verdict

Gap-fill remains a **data-recovery / opt-in** facility, not an accuracy lever on
the available local reference data — now with two more methods (`derive`,
`soilgrids`) reachable from the classifiers. 44 fixtures byte-identical with
`gapfill = FALSE`; the existing within-pedon interp tests are unchanged;
+9 lock-in tests (test-v09140).
