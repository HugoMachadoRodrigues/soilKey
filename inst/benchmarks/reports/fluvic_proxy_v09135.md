# Fluvic-material proxy fix (v0.9.135)

The user asked to enable the verbatim SiBCS/WRB fluvic "AND/OR" by tightening the
`oc_irregular` proxy. Doing so surfaced a richer result.

## The two proxies were both too loose

`test_fluvic_stratification` combined two proxies:
- `texture_alternates = any(abs(diff(clay)) >= 8)` — fired on ANY clay change,
  so a monotone A->Bt increase (a normal Argissolo) read as "stratified".
- `oc_irregular = any(diff(oc) > 0.1)` — fired on any tiny OC bump.

Under the original AND both were masked. The fixes:
- **texture**: require an erratic clay REVERSAL (an interior peak/valley, both
  adjacent swings >= 8%) — a depositional signature, not a pedogenic trend.
- **OC**: require a substantial reversal (deeper >= shallower + 0.2% AND
  >= 1.25x) AND exclude increases INTO a spodic illuvial horizon (Bh/Bs/Bhs) —
  podzolization is pedogenic, which the SiBCS criterion ("nao relacionada a
  processos pedogeneticos") excludes.

## Why the OR is still deferred

With `passed = texture OR oc`, an erratic-OC-only Chernozem keys as a Neossolo
Fluvico (recall -> 0 on Redape), because the package's SiBCS key reaches the
Neossolos branch before the stronger orders for that profile. That is a
key-ordering issue (Neossolos should be the residual order). Until it is fixed,
`passed = texture AND oc` is kept — and the tightened proxies already improve
accuracy under AND.

## Measured impact (kept as AND)

| metric | before | after |
|---|---|---|
| BDsolos RJ Argissolo recall | 166 | **175** |
| BDsolos RJ Argissolo -> Neossolo | 60 | **50** |
| Redape SiBCS order accuracy | 59.6 % | **63.8 %** |
| Chernossolo / Latossolo recall | — | preserved |

## Verification

- Full suite **5692 pass / 0 fail**; +4 unit tests; two benchmark
  regression-guards (`test-v0983`, `test-v0981`) updated to the new (better)
  numbers, each documented as an improvement.
- `R CMD check --as-cran`: codoc OK.

## Follow-up

Enable the fluvic OR after fixing the SiBCS key so Neossolos is the residual
order (checked after Chernossolos and the other strong orders).
