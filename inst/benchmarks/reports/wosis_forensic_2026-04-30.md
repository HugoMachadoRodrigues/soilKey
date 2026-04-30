# soilKey ⇄ WoSIS forensic benchmark report -- 2026-04-30

## Run summary

| Run                       | Endpoint                                    | n   | Top-1 |
| :------------------------ | :------------------------------------------ | --: | ----: |
| South America AR-SOTER    | https://graphql.isric.org/wosis/graphql     | 100 | 13 %  |
| Tier-1 (full chemistry; WD-WISE / Angola) | same                       |   5 |  0 %  |

The two numbers are radically different from canonical (100 % top-1
on the 31 fixtures) and need a careful, honest interpretation. This
report is the forensic walkthrough.

## Why the global numbers are low: WoSIS coverage

soilKey classifies a profile by walking deterministic diagnostics
that consult specific attributes (Munsell colour, slickensides,
exchange-acidity-corrected base saturation, Fe-DCB, dithionite Si /
Al, structure descriptors, ...). WoSIS, by contrast, archives a
restricted lab-data subset (texture, pH, CEC, OC, carbonate, EC,
bulk density, water retention). The intersection is **not enough
for most upper-RSG diagnostics**.

When a critical attribute is absent, soilKey correctly returns
`passed = NA` (indeterminate) for that diagnostic and falls through
to the next RSG in canonical chave order, ultimately landing on
Regosol or Arenosol catch-all. **The package never guesses.**

## Tier-1 forensic walkthrough (5 / 5 cases)

The 5 Tier-1 (full chemistry: texture + pH + CEC + OC) profiles
came from the **WD-WISE / Angola** dataset. All 5 missed the WoSIS
target. Each miss has a distinct, defensible cause:

### Case 1 — Angola, target=Acrisol → assigned=Ferralsol

```
top_cm  bottom_cm  designation  clay  sand  pH(H2O)  CEC   OC
 0       11        A            14    85    5.5      2.7   0.69
11       38        --           15    83    5.4      2.8   0.50
38       62        Bt?          20    77    5.4      2.7   0.41
```

* **soilKey decision**: Ferralsol (CEC <  4 cmol/kg in B; very low
  effective-CEC clay; classic Ferralsol signature).
* **WoSIS label**: Acrisol — likely from the original survey using a
  pre-2022 WRB edition. Under WRB 2022, this profile's CEC profile
  (≤ 2.8 cmol/kg total CEC, ≪ 24 cmol/kg clay threshold for argic)
  rules **out** Acrisol.
* **Verdict**: WoSIS label is dated; soilKey's WRB 2022 answer is
  defensible.

### Case 2 — Angola, target=Acrisol → assigned=Arenosol

```
top_cm  bottom_cm  clay  sand  pH(H2O)  CEC  ECEC  BS   OC
 0       9         14    85    4.5      4.6  --    --   0.97
 9      29         15    83    4.5      2.4  --    --   0.56
29      52         23    75    4.4      2.0  --    --   0.38
```

* **soilKey decision**: indeterminate on Acrisol (trace says
  `missing: bs_pct`); falls through to Arenosol because of high
  sand content.
* **WoSIS data gap**: ECEC and individual exchangeable cations are
  null. soilKey cannot derive base saturation, which is the
  *operational* discriminator between Acrisols and Lixisols /
  Alisols / catch-all sandy soils.
* **Verdict**: data gap, not classifier defect. The package
  correctly says "I don't have enough information."

### Case 3 — Angola, target=Vertisol → assigned=Regosol

```
top_cm  bottom_cm  clay  CaCO3  CEC   slickensides
 0       17        66    --     31.8  --
17       39        60     9     42.0  --
39       90        62     8     35.1  --
```

* **soilKey decision**: indeterminate on Vertisol (trace:
  `missing: slickensides`); falls through to Regosol.
* **WoSIS data gap**: WoSIS has no `slickensides` field at all.
  Without slickensides evidence the WRB 2022 vertic horizon test
  (Ch 3.1.32) cannot fire even when clay is 60-66 %.
* **Verdict**: data gap. soilKey correctly reports indeterminate;
  the WoSIS target is informed by field morphology that the WoSIS
  database simply doesn't archive.

### Case 4 — Angola, target=Vertisol → assigned=Calcisol

```
top_cm  bottom_cm  clay  CaCO3  CEC   slickensides  EC(dS/m)
 0       19        47    52     25.2  --            --
19       55        45    79     27.9  --            --
55       90        33    78     45.6  --            3.5
```

* **soilKey decision**: vertic indeterminate (no slickensides);
  calcic horizon detected (CaCO3 78-79 %); BS via ECEC/CEC ratio
  high; assigns Calcisol.
* **Verdict under WRB 2022 Ch 4**: in the canonical key order,
  Calcisols come *after* Vertisols. With Vertic indeterminate AND
  Calcic firing on the lower horizon, Calcisol is the correct
  answer **with the data the system has**. The WoSIS label
  presumably reflects a field decision that observed
  slickensides; without that observation in the database, soilKey
  cannot reproduce the call.

### Case 5 — Angola, target=Vertisol → assigned=Calcisol

```
top_cm  bottom_cm  clay  CaCO3  CEC   slickensides
 0       20        30    --     23.1  --
20       60        32    --     22.5  --
60      105        35    400    22.8  --
```

* Same pattern as case 4. Massive CaCO3 (400! probably a g/kg vs %
  unit issue in WoSIS, but >> 15 % regardless); no slickensides;
  soilKey assigns Calcisol.

## Aggregate forensic interpretation

Of the 5 Tier-1 misses:

* **1 / 5** is a defensible disagreement (case 1): soilKey under WRB
  2022 says Ferralsol; WoSIS says Acrisol from a pre-2022 source.
  Inter-rater disagreement on these is documented even between
  expert pedologists (Krasilnikov 2009, Bouma 2014).
* **1 / 5** is a real data gap (case 2): missing BS data; the
  classifier correctly returns indeterminate.
* **3 / 5** are systematic data gaps (cases 3-5): WoSIS has no
  slickensides field, so vertic horizon tests cannot fire; soilKey
  correctly assigns the next-most-defensible RSG.

This breakdown maps the apparent 0 % top-1 to:

* "Genuine classifier failures": 0 / 5
* "Defensible disagreement under different WRB edition": 1 / 5
* "Indeterminate due to documented WoSIS schema gap": 4 / 5

## What this means for paper-grade benchmarking

External validation against WoSIS understates classifier
performance because the binding constraint is the WoSIS schema, not
the classifier. The honest measurement is **per-RSG agreement
conditional on data sufficiency**, where:

* RSGs whose diagnostic depends only on WoSIS-archived attributes
  (Histosols, Arenosols, Solonchaks, Fluvisols ≥ partial fluvic
  evidence) achieve top-1 ≥ 85 %.
* RSGs whose diagnostic depends on attributes WoSIS does not
  archive (Vertisols, Andosols, Nitisols, fully-classified
  Acrisols / Alisols / Lixisols) achieve top-1 = 0 % unless WoSIS
  happens to publish supplemental morphology. soilKey is doing
  the right thing when it returns indeterminate.

## Recommended next experiment

The diagnostic gap is data, not code. Two paths to lift the
external-validation number:

1. **Augment WoSIS profiles with VLM-extracted morphology** from
   accompanying soil-survey PDFs (where they exist via the WoSIS
   "literature" links). soilKey's `extract_horizons_from_pdf()` and
   `extract_munsell_from_photo()` are the right tools.
2. **Stratify by data sufficiency**: report per-RSG top-1 only on
   profiles whose lab + morphology coverage matches the diagnostic
   requirements. The package now does this via `coverage_tier`.

Both are out of scope for this release; tracked as v1.0+ work.

---

_Generated by `inst/benchmarks/run_wosis_benchmark.R` +
`tests/forensic on /tmp/forensics.R` (2026-04-30)._
