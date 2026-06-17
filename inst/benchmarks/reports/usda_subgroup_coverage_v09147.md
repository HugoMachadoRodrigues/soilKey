# USDA subgroup coverage +35 (criteria-exact slice) — v0.9.147

Closes the user's request to raise USDA subgroup coverage. This is a
**completeness** slice, explicitly **not** an accuracy change: every new rule is
appended before its block's `Typic` default with first-match semantics, so an
already-specific classification can never change — only a former `Typic` can
refine. The deterministic key is byte-identical on every already-classified
pedon (44 canonical fixtures unchanged; KSSL 0 worsened).

## Method (the v0.9.121/123 discipline, automated by a verification workflow)

1. **Enumerate** the 712 missing subgroups; keep the **194 "generatable-now"**
   (every modifier word already appears in the registered predicate vocabulary);
   drop the **8 known over-firers** (alfic/fluventic/psammentic/vertic/aquic/
   oxyaquic/aeric/fluvaquentic) → **62 strict candidates**.
2. **Verify** each of the 62 with a 9-agent workflow (one per modifier family)
   against the vendored `ST_criteria_13th`: map every modifier to the EXACT
   existing predicate + params, copied from a sibling subgroup; reject any whose
   semantics do not match. → **35 safe, 27 excluded**.
3. **Generate**: insert each as `- { code, name, tests: { all_of: [...] } }`
   before the block's `Typic` default, minting a fresh non-colliding code
   (soilKey's internal codes diverge from kst13, so the kst13 code could not be
   reused).
4. **Gate**: KSSL+NASIS n=3860 before/after (0 worsened) + 44 fixtures
   byte-identical + full suite + `--as-cran`.

## Result

USDA subgroup coverage **73.8% → 75.1% (2003 → 2038 / 2715)**. The 35:

- **13 Fragiaquic** (`fragipan_usda` + `aquic_subgroup_usda`) — Palexeralfs,
  Haploxeralfs, Paleudalfs, Glossudalfs, Hapludalfs, Dystroxerepts, Eutrudepts,
  Dystrudepts, Kanhapludults, Paleudults, Hapludults.
- **13 Humic** — 8 Oxisols (`humic_oxisol_usda`: Sombriustox, Kandiustox,
  Sombriperox, Kandiperox, Sombriudox, Acrudox, Eutrudox, Kandiudox), 4
  aquic Inceptisols (`humic_inceptisol_usda`: Fragiaquepts, Densiaquepts,
  Gelaquepts, Cryaquepts), 1 Andisol (`humic_andisol_usda`: Durustands).
- **5 Gypsic** (`gypsic_subgroup_usda`) — Calciustepts, Haplustepts,
  Haploxerepts, Calciustolls, Haplusterts.
- **3 Spodic** (`spodic_subgroup_usda`) — Humicryepts, Dystrocryepts, Dystrudepts.
- **Argic Petrocalcids** (`argillic_within_usda` 100 cm), **Aridic Leptic
  Haplusterts** (`smr_aridic_usda` + `leptic_vertic_usda`), **Plinthic
  Quartzipsamments** (`plinthic_subgroup_usda` 100 cm).

## Honesty

- This raises the **coverage statistic**, not classification accuracy. The
  KSSL gate fires on only 1 pedon (the US sample barely contains these Oxisol /
  intergrade subgroups), so the safety guarantee comes from criteria-exact
  predicate mapping + append-before-default, not from the dataset exercising them
  (the same caveat documented at v0.9.123).
- **27 candidates were excluded, not mis-mapped** (the careful half of the work):
- **Aridic Leptic Natrustalfs** — TRAP: 'Leptic' in Natr- great groups = soluble-salt criterion (visible gypsum/soluble-salt crystals within 40 cm), NOT the lithic/contact 'Leptic' of leptic_vertic_usda
- **Aridic Leptic Natrustolls** — TRAP (same as Natrustalfs): 'Leptic' here = visible gypsum/soluble-salt crystals within 40 cm, a soluble-salt criterion, NOT a lithic/contact depth
- **Haploplaggic Udipsamments** — No existing predicate for the haploplaggic/plaggen modifier
- **Humic Fragiaqualfs** — Differentia is the 'value moist<=3, dry<=5 in upper 18 cm' color test
- **Humic Frasiwassents** — Color-value (V<=3 moist / <=5 dry, upper 18 cm) differentia
- **Humic Epiaquepts** — 'Humic Epiaquepts' is not a valid KST-13 subgroup (no matching differentia entry)
- **Humic Endoaquepts** — Needs all_of[color-value, base-sat<50%]
- **Humic Dystrustepts** — Color-value-only differentia
- **Humic Fragixerepts** — Differentia is an any_of[epipedon, color-value]
- **Humic Dystroxerepts** — Color-value-only differentia; no registered dark-color-value predicate
- **Humic Lithic Dystroxerepts** — Multi-word modifier needing all_of[lithic_contact_usda, color-value]
- **Humic Haploxerepts** — Color-value-only differentia; no registered dark-color-value predicate
- **Humic Lithic Haploxerepts** — all_of[lithic_contact_usda, color-value]; the color-value modifier has no registered predicate
- **Humic Fragiudepts** — any_of[epipedon, color-value]; the color-value branch has no registered predicate, so humic_inceptisol_usda alone would under-match
- **Humic Densiudepts** — any_of[epipedon, color-value]; the color-value branch has no registered predicate
- **Humic Eutrudepts** — Color-value-only differentia; no registered dark-color-value predicate
- **Humic Lithic Eutrudepts** — all_of[lithic_contact_usda, color-value]; the color-value modifier has no registered predicate
- **Humic Lithic Dystrudepts** — all_of[lithic_contact_usda, color-value]; the color-value modifier has no registered predicate
- **Leptic Natrustalfs** — TRAP (v0
- **Leptic Natralbolls** — Same trap: salt-crystal Leptic with no matching predicate
- **Leptic Natrustolls** — Same trap: no predicate for "visible crystals of gypsum and/or more soluble salts within 40 cm"
- **Leptic Natrudolls** — Same trap: salt-crystal Leptic with no matching predicate
- **Plinthic Petraquepts** — Same-word-different-meaning trap: 'Plinthic' in Petraquepts means plinthite continuous-phase OR >=50% by volume (KABC), not the >=5% used by plinthic_subgroup_usda/plinth_subgroup_usda
- **Sodic Hydraquents** — Semantic mismatch (CRITICAL TRAP)
- **Sodic Psammaquents** — Same CRITICAL-TRAP semantic mismatch as Sodic Hydraquents
- **Sodic Endoaquents** — Same CRITICAL-TRAP semantic mismatch
- **Spodic Paleudults** — Code collision: soilKey codes diverge from kst13 (documented gotcha) and the supplied code HCEB is ALREADY registered in inst/rules/usda/subgroups/ultisols
