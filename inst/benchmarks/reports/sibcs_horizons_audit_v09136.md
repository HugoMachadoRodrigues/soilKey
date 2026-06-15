# SiBCS diagnostic-horizon audit — v0.9.136

**Phase 3, slice 2.** Audit of the SiBCS *diagnostic horizons* (Embrapa 2018,
Cap 2) against the verbatim manual, same pattern as the *atributos* slice
(v0.9.134). Surface A horizons covered here; subsurface B horizons and the
order→subgrupo keys remain for later slices.

## Method

1. Multi-agent Workflow (`sibcs-horizons-audit`, 9 agents): per-group agents
   compared the horizon-function code against the verbatim Cap 2 criteria
   supplied in-prompt → adversarial-refute pass.
2. **Hand re-confirmation against the manual PDF** (the Phase-2 meta-lesson:
   the adversarial-verify Workflow degrades without machine ground truth — SiBCS
   has none in-package — so every flag was re-checked by reading the printed
   pages directly: p.50–53 surface A, p.54–59 B textural/latossólico).
3. Fix only confirmed verbatim contradictions; each *refine-when-present*
   (tighten only on recorded data) → byte-identical when the field is absent.
4. Gate: 44 canonical fixtures + SiBCS benchmarks (Redape order, BDsolos RJ
   confusion) + full suite + `R CMD check --as-cran`.

## Workflow flags → verdicts (4 flagged, 4 acted, 1 sub-claim refuted)

| Function | Flag | Verdict | Action |
|---|---|---|---|
| `horizonte_A_humico` | colour value/chroma ≤4 (p.51) never checked | **CONFIRMED** | added colour gate |
| `horizonte_A_chernozemico` | structure allows *weak* grade; p.50(a) requires moderate/strong | **CONFIRMED** | require moderate/strong |
| `horizonte_A_chernozemico` | flat 18 cm vs p.51(e) conditional 10/18+⅓/25 | **CONFIRMED** | solum-depth conditional |
| `horizonte_A_antropico` | (a) missing artefacts gate; (b) "inverted AND/OR" | (a) **CONFIRMED** / (b) **REFUTED** | added artefacts gate; AND kept |
| `B_textural` | relação-textural ratio (p.56 h) not keyed on A clay | **CONFIRMED (gap)** | **deferred** (see below) |

### The refuted sub-claim (verbatim caught it)

The workflow claimed `horizonte_A_antropico` inverts SiBCS's AND/OR, asserting
the manual reads "thickness ≥20 **OR** P ≥30". The verbatim (p.53) reads:

> a) Espessura maior ou igual a 20 cm; **e**
> b) Conteúdo de P extraível (Mehlich-1) ≥ 30 mg kg⁻¹.

The connector is **"e" (AND)**. `hortic` already enforces both as an AND, so it
is correct. Only the *artefacts* omission was real (artefacts are "de presença
obrigatória"). Fixing the agent's hallucinated logic flip would have introduced
a regression — re-reading the page prevented it.

## Fixes (all in `R/diagnostics-horizons-sibcs.R`)

1. **A húmico colour** — `color_ok <- all(value_moist ≤4) && all(chroma_moist ≤4)`
   over the A run; all-NA → TRUE (byte-identical).
2. **A chernozêmico structure** — `is.na(grade) || grepl("moder|strong|forte", grade)`
   (was `!grepl("massive|grain|loose", grade)`, which let *weak* through).
3. **A chernozêmico thickness** — conditional on solum (A+B) depth and lithic
   contact: ≥10 over rock / ≥18 ∧ >⅓·solum if solum<75 / ≥25 if solum≥75;
   `min_thickness_cm` (18) is the fallback only when solum is indeterminable.
4. **A antrópico artefacts** — when `artefacts_pct` is recorded and zero in the
   diagnostic layers → FALSE; absent → defer to `hortic` (byte-identical).

## Deferred (honest)

- **`B_textural` relação-textural (h).** The verbatim ratio thresholds
  (>1.50 / >1.70 / >1.80, keyed on A-horizon clay >400 / 150–400 / <150 g/kg,
  with the p.56 footnote-4 control-section rules) are NOT enforced — `B_textural`
  delegates wholesale to the WRB `argic` clay-increase (the manual itself notes
  the criterion "é derivado de argillic horizon"). `argic` captures the
  clay-increase essence but not the SiBCS pure-ratio alternative path. Because
  `B_textural` governs the dominant Argissolo/Latossolo split, re-wiring it is
  high regression risk (cf. the v0.9.135 fluvic OR cascade) → its own gated slice.
- **A húmico / A chernozêmico thickness parity.** A húmico criterion (a) says
  "espessura mínima como a descrita para o A chernozêmico"; it still uses a flat
  18 cm rather than the new conditional. Pre-existing simplification; the
  `proeminente` path already inherits the conditional via `chernozemico`.

## Gate results

- **44 canonical fixtures: byte-identical** (the canonical chernozem fixture —
  Ah 0–100 cm, strong/strong/moderate grade, value ≤3, Bk–Ck solum 140 cm — passes
  the moderate/strong gate and the 25 cm-deep-solum thickness).
- **Redape order accuracy: 63.8% unchanged** (test-v0981 guard 60–66%).
- **BDsolos RJ confusion: byte-identical** (test-v0983 exact matrix).
  Benchmark pedons carry no measured `structure_grade`/`artefacts_pct`, and their
  chernic candidates still pass thickness → refine-when-present holds.
- Full suite green; `+9` lock-in tests (test-v09136); `--as-cran` codoc OK.
