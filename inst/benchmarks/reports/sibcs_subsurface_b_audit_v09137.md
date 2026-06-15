# SiBCS subsurface-B diagnostic-horizon audit — v0.9.137

**Phase 3, slice 3.** Audit of the SiBCS *subsurface B* diagnostic horizons
(Embrapa 2018, Cap 2, p.59-74) against the verbatim manual, after the surface-A
slice (v0.9.136).

## Method

1. Multi-agent Workflow (`sibcs-subsurface-b-audit`, 12 agents, 3 groups):
   per-group agents compared the B-horizon functions (and their delegated WRB
   cores: spodic/albic/plinthic/petroplinthic/gleyic/calcic/thionic/vertic_horizon)
   against verbatim Cap 2 criteria supplied in-prompt → adversarial-refute pass.
2. **Hand re-confirmation against the manual PDF** (Phase-2 meta-lesson — SiBCS
   has no machine ground truth, so every flag was re-checked by reading the
   printed pages p.59-74 directly).
3. Fix confirmed verbatim contradictions; each *refine-when-present*.
4. Gate: 44 canonical fixtures + SiBCS benchmarks (Redape order, BDsolos RJ
   confusion) + full suite + `R CMD check --as-cran`.

## Workflow flags → verdicts (9 confirmed; 7 fixed, 1 refuted sub-claim, 2 deferred)

| # | Function | Flag | Verdict | Action |
|---|---|---|---|---|
| 1 | `B_nitico` | structure GRADE + cerosidade GRADE not tested (p.62c) | **CONFIRMED** | grade gates added |
| 2 | `B_nitico` | flat 30 cm misses the ≥15 cm lithic-contact exception (p.62a) | **CONFIRMED** | conditional thickness |
| 3 | `B_nitico` | non-verbatim `fe_dcb≥8` ferric short-circuit (p.62d) | **CONFIRMED** | **removed** (measured neutral) |
| 4 | `B_incipiente` | missing duripa/petrocalcico/fragipa/plintita/glei exclusions (p.60a) | **CONFIRMED** | 5 exclusions added |
| 5 | `horizonte_vertico` | cracks 0.5 cm vs SiBCS ≥1 cm (p.73) | **CONFIRMED** | `min_crack_width_cm=1.0` |
| 6 | `horizonte_sulfurico` | missing jarosite OR-path (p.72-73) | **CONFIRMED** | jarosite path wired |
| 6b| `horizonte_sulfurico` | "sulfidic-S 0.01% is 5× below SiBCS 0.05%" | **REFUTED** | none (sulfate ≠ sulfide-S) |
| 7 | `horizonte_calcico`/`calcic` | missing "+50 g/kg vs subjacent" clause (p.71) | **CONFIRMED** | **deferred** (shared core) |
| 8 | `B_planico` | colour paths (b)/(c) + permeability missing (p.66) | **CONFIRMED** | **deferred** (schema-blocked) |
| 9 | `horizonte_E_albico` | WRB albic vs SiBCS albico colour drift (p.66) | **delegation** | **deferred** (manual cites albic) |

### The refuted sub-claim

The workflow read `thionic`'s `min_sulfidic_s = 0.01` (% sulfidic **sulfide**-S)
as "5× below the SiBCS 0.05%". But the SiBCS 0.05% (p.72-73 c) is **water-soluble
sulfate** — a different analyte. The sulfidic-S path and the sulfate path are
distinct alternatives; no threshold change was warranted.

### The ferric short-circuit — measured, not assumed

`B_nitico` carried a v0.9.10 `fe_dcb_pct >= 8` path added on the premise that
high-activity ferric Nitossolos were lost to Argissolos. It is **not in the
verbatim criterion (d)**. Removal was **measured benchmark-neutral**:

| | BDsolos RJ Argissolo→Argissolo | BDsolos RJ Nitossolo→Argissolo | Redape order acc |
|---|---|---|---|
| ferri kept | 175 | 4 | 63.8% |
| ferri removed | 175 | 4 | 63.8% |

Ferric Nitossolos are oxidic → low-activity clay → already pass the low-activity
path, so the short-circuit was inert. The canonical Nitossolo fixture still
classifies as Nitossolos. Removed to restore verbatim fidelity.

## Deferred (honest)

- **`calcic` core +50-subjacent clause.** Verbatim calcic (p.71, and equally WRB
  3.1.5 / USDA KST) requires ≥50 g/kg more CaCO3 than an underlying layer; the
  shared `calcic()` core tests only the absolute ≥15%. Fixing it changes WRB
  Calcisols and USDA Calcids → needs the KSSL n=2895 + WRB cross-system gate →
  its own slice, not bundled into a SiBCS-horizon audit.
- **`B_planico` colour paths (b)/(c) + slow permeability.** Schema-blocked: no
  mottle-colour fields (only `mottle_morphology`, qualitative) and no
  horizon-level permeability/drainage field.
- **`horizonte_E_albico`.** The manual (p.67) states E álbico "é derivado de
  albic horizon"; delegating to the WRB albic core is sanctioned. The 1-chroma
  drift the workflow cited is a WRB-2022-albic-vs-FAO-1974-albic difference;
  rewriting risks the Planossolo/Espodossolo benchmarks — deferred.

## Gate results

- **44 canonical fixtures: byte-identical**, except the canonical SiBCS Vertissolo
  fixture, whose shrink-swell cracks were widened from 0.6–0.8 cm to verbatim-valid
  ≥1.2 cm (a textbook Vertissolo cracks well past 1 cm). The shared WRB/USDA
  `make_vertisol_canonical` fixture is untouched (byte-identical).
- **Redape order accuracy 63.8% unchanged**; **BDsolos RJ confusion byte-identical**.
- Full suite green; `+9` lock-in tests (test-v09137); `--as-cran` codoc OK.
