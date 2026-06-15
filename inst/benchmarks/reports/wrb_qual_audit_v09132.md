# WRB 2022 qualifier audit — batch 1 (v0.9.132, Fix D slice 4)

A multi-agent Workflow audited ~120 WRB qualifier predicates: 7 agents (one per
family) compared each `qual_*` body against the verbatim WRB 2022 Ch 5 criteria
(extracted by the main loop from the authoritative PDF and provided in-prompt as
the ONLY ground truth), followed by an adversarial-refutation pass. **Every
flag was then re-confirmed by hand against the PDF** — the meta-lesson from
Phase 2 is that even adversarial-verify degrades without machine ground truth
(indeed one verifier cited "WRB typically ≥80%" from model knowledge, which the
hand check rejected).

## Confirmed and fixed (11)

| qualifier | WRB 2022 | was | fix |
|---|---|---|---|
| Geric | (bases+Al) < 6 cmol/kg **clay** | ≤ 1.5 cmol/kg fine earth + spurious ΔpH | per-clay, < 6, ΔpH removed |
| Sodic | ≥ 15 % (Na+Mg) AND ≥ 6 % Na | only ≥ 6 % Na | added (Na+Mg) ≥ 15 % |
| Eutrosilic | Σ bases ≥ 15 cmol/kg | BS ≥ 50 % | Σ base cations ≥ 15 |
| Pellic | value ≤ 3 | value ≤ 4 | ≤ 3 |
| Aceric | 3.5 ≤ pH < 5 | pH ≤ 5 | added lower bound |
| Carbonic | ≥ 5 % OC, ≥ 10 cm | ≥ 6 %, no thickness | 5 %, ≥ 10 cm |
| Columnic | columnar, ≥ 15 cm | columnar/column/**prism** | drop prism, ≥ 15 cm |
| Magnesic | Ca/Mg < 1, ≥ 30 cm | any layer | ≥ 30 cm |
| Thixotropic | within 50 cm | within 100 cm | 50 cm |
| Hyperorganic | organic ≥ 200 cm thick | organic layer ≤ 100 cm | ≥ 200 cm |
| Placic | Fe-cement ≥ weakly, 0.1–2.5 cm | strongly/indurated only | ≥ weakly, ≥ 0.1 cm |

## Deferred (verified, not fixed in this slice)

- **Schema-blocked**: Isopteric (BD/particle-size), Mochipic (saturation-days),
  Glacic (ice %), Aceric jarosite, Hydric undried-water (could refine).
- **Proxy / not a clear threshold bug**: Raptic (material-origin exclusions),
  Hyposalic (no verbatim — may be a package extension), Grumic
  (blocky-alternative), Mazic (rupture-resistance clause), Urbic / Evapocrustic
  (thickness refinement).
- **Uncertain verbatim**: Hyperskeletic (the ≥ 90 % coarse threshold may be
  correct; the agent's "≥ 80 %" was model knowledge, rejected).

## Verification

- Verbatim WRB 2022 PDF (Ch 5, p126–151) — the sole ground truth.
- Full suite **5656 pass / 0 fail**; +16 unit tests; 2 old-criterion unit tests
  updated (Eutrosilic, Hyperorganic).
- `R CMD check --as-cran`: codoc OK.

## Fix D progress

Slices: base-status (v0.9.129), texture (v0.9.130), colour (v0.9.131), audit
batch 1 (v0.9.132). The multi-agent audit covered all remaining families; a
small set of deferred refinements (above) remains, plus the schema-blocked
items that need new fields.
