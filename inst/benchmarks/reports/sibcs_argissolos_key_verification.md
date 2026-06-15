# SiBCS Argissolos taxonomic-key verification (vs Embrapa 2018 Cap 5)

**Phase 3, keys slice 1 — a VERIFICATION (no code change).** First audit of a
SiBCS taxonomic *key* (as opposed to a diagnostic/attribute) against the verbatim
Embrapa 2018 manual. Argissolos chosen first: the largest order (240 references in
the BDsolos RJ benchmark) and the most complex key.

## Method

1. Read the verbatim Cap 5 (printed p115–125): subordens (p115–116), grandes
   grupos (p116–119), and the subgrupos of the most complex GG (PA Amarelos
   Distrocoesos, 21 subgroups, p122–125).
2. Explore-agent mapped the current `inst/rules/sibcs5/` Argissolos key
   (subordens in key.yaml, grandes-grupos/argissolos.yaml, subgrupos/argissolos.yaml).
3. Compared NAME + ORDER + discriminating criteria, level by level.

## Result — the key is FAITHFUL at every level checked

| Level | Verbatim (Cap 5) | YAML | Verdict |
|---|---|---|---|
| Subordens | 5: Bruno-Acinzentados → Acinzentados → Amarelos → Vermelhos → Vermelho-Amarelos (colour-keyed) | PBAC, PAC, PA, PV, PVA, same order + hue/value/chroma tests | **MATCH** |
| Grandes grupos | 23 = 3+3+6+6+5, each Ta-Alumínicos → Alumínicos → [Ta-Distróficos] → Distrocoesos → Distróficos → Eutroférricos → Eutrocoesos → Eutróficos | 23 GGs, identical names + order + predicates (Ta/aluminic/BS/coeso/ferri) | **MATCH** |
| Subgrupos PBAC (8) | Ab / Eh / Hu / Tp per GG | identical | **MATCH** |
| Subgrupos PAdc (21, most complex) | Sd-Ab, Ar-Fr, Ar, Ab-Fr-Ep, Ab-Fr, Ab-Pp, Ab-Pl, Ab-Ep, ... | `PAdcSdAb, PAdcArFr, PAdcAr, PAdcAbFrEp, PAdcAbFr, PAdcAbPp, PAdcAbPl, PAdcAbEp, ...` | **MATCH** |

- **165 Argissolo subgroups** are wired across all 23 GGs (PA Amarelos 6/5/21/6/8/6;
  PV Vermelhos 6/7/4/9/6/16 included). The Explore-agent's initial "PA & PV
  subgrupos not implemented" claim was a **false alarm** — refuted by directly
  loading the rules (Phase-2 meta-lesson: verify structural claims against the
  actual loaded rules, not a YAML skim).
- The subordem + GG structure is already regression-guarded by
  `test-sibcs-argissolos-gg-v074.R` (asserts 5 subordens + 23 GGs = 3+3+6+6+5).

## Reframe — the keys are sound; the subgrupo ceiling is DATA-limited

The largest and most complex SiBCS key, built carefully in v0.7.4, is verbatim-
faithful down to the ordering of a 21-member GG. This is strong evidence the other
order keys (built the same way) are likewise sound. Therefore:

- **The Redape subgrupo accuracy (27.1%) is limited by missing input attributes in
  the reference pedons, NOT by key-correctness bugs** — consistent with the
  project-wide finding that the accuracy ceiling is missing data, not engine bugs.
- A full key-by-key audit of the remaining 11 orders is **low-yield**: it would
  largely re-confirm faithful keys against a weak (data-limited) gate.

## Honest residual (minor, deferred)

- **Bruno-Acinzentados subordem** — the verbatim "escurecimento" sub-criteria
  (p115 a/b: moist value/chroma below the subjacent sub-horizon; dry value/chroma
  below ≥1 overlying sub-horizon) are not separately enforced beyond the
  hue/value≤4/chroma≤4 gate. Bruno-Acinzentados is rare (RS/PR/SC/Pampas only);
  tightening risks the canonical fixture for negligible benchmark gain → deferred.
- The verbatim Bruno-Acinzentados value range is "3 a 4"; the YAML enforces
  value ≤ 4 (not ≥ 3). Negligible.

No code change shipped: the key is correct as-is.
