# SiBCS atributos audit — Phase 3 slice 1 (v0.9.134)

With the authoritative Embrapa 2018 manual (SiBCS 5th ed.) provided by the
user, a multi-agent Workflow audited the SiBCS *atributos diagnósticos* against
the verbatim Cap 1 criteria (extracted by the main loop from the manual, p30–47,
and provided in-prompt as the sole ground truth), followed by an
adversarial-refutation pass and a **hand re-confirmation against the manual**.

## Confirmed and fixed (4)

| atributo | Embrapa 2018 (Cap 1) | was | fix |
|---|---|---|---|
| `carater_acrico` | (bases+Al) ≤ 1.5 cmol/kg clay AND (pH-KCl ≥ 5.0 **OR** ΔpH ≥ 0) | ΔpH ≥ 0 only | added pH-KCl ≥ 5.0 OR-branch |
| `carater_alitico` | Al ≥ 4 AND (Al-sat ≥ 50 % **OR** V < 50 %) | all three AND-ed | OR the two saturations |
| `luvissolo_cromico` | crit (c) only for hues 2.5Y–5Y | catch-all else (any hue) | restricted to 2.5Y/5Y |
| `carater_argiluvico` | B/A ≥ 1.4 **AND** prismatic/blocky-moderate structure | clay ratio only | added structure clause (refine-when-present) |

## Verified correct — no change

atividade-argila (Ta ≥ 27), eutrofico/distrofico, carater_eutrico (pH ≥ 5.7 +
S ≥ 2.0), carbonatico (≥ 15 %)/hipocarbonatico (5–15 %),
sodico (PST ≥ 15)/solodico (6–15)/salico (CE ≥ 7)/salino (4–7),
plintico/concrecionario/litoplintico/redoxico/planico, vertico, fluvic colour
branches.

## Rejected by hand re-confirmation (Phase-2 lesson)

`mudanca_textural_abrupta` — an agent claimed a missing "3rd case (A/E ≥ 400 g/kg
→ +220 g/kg)". The manual (p31) gives only **two** cases; its "220 → 420"
example is the +200 g/kg rule (420 − 220 = 200), not a third increment. The
code's 2-case logic is correct.

## Deferred

`carater_fluvico` / shared `fluvic_material`: the SiBCS/WRB criterion is verbatim
an OR (stratified texture AND/OR irregular OC-with-depth), but the package's
`oc_irregular` proxy (any +0.1 % OC increase with depth) is too permissive to
stand alone in an OR — switching to OR over-fired fluvic across all three
systems (56 cascading test failures). Kept as AND until the proxy is tightened
to a genuine erratic-OC pattern.

## Verification

- Verbatim Embrapa 2018 manual (Cap 1, p30–47) — the sole ground truth.
- Full suite **5684 pass / 0 fail**; +8 unit tests; 44 canonical fixtures
  byte-identical.
- `R CMD check --as-cran`: codoc OK.

## Phase 3 scope note

This is the first SiBCS slice (the Cap 1 *atributos*). The diagnostic horizons
(B textural / latossólico / nítico / espódico; A chernozêmico / húmico /
proeminente / hístico / antrópico) and the order→subgroup keys remain to be
audited against later chapters of the manual.
