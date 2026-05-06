# Munsell-from-photo fixtures

Each fixture is a **pair**:

```
fixture_id.jpg          (or .png / .jpeg / .webp)
fixture_id.golden.json
```

The image must be a real photo of a soil profile (or a single horizon)
with at least one Munsell colour reference card visible — without the
card, `pedologist_system_prompt()` instructs the agent to fall back to
`confidence <= 0.5`, which would dominate the benchmark.

Golden JSON schema (mirror of what `extract_munsell_from_photo()`
produces):

```json
{
  "horizons": [
    {
      "designation": "Bt1",
      "top_cm": 30,
      "bottom_cm": 80,
      "munsell_hue_moist": "5YR",
      "munsell_value_moist": 4,
      "munsell_chroma_moist": 6
    },
    ...
  ]
}
```

## Where to get fixtures

We do not ship Munsell photo fixtures bundled with the package: real
profile photos are large, often subject to publication / institutional
licences, and lose value when stripped of the original metadata. The
two recommended sources for this benchmark:

1. **Your own field photos** with a colour-checker card visible,
   labelled by hand against the profile description.
2. **Embrapa BDsolos image archive** — most BDsolos perfis (the same
   ~9 000 we benchmark in `benchmark_bdsolos_sibcs()`) include scanned
   photo plates. Pair each plate with the BDsolos surveyor-recorded
   Munsell triplet as the golden answer.

Drop the resulting `<id>.jpg` + `<id>.golden.json` files into this
directory and re-run `benchmark_vlm_extraction(tasks = "munsell")`.

## Metric

`benchmark_vlm_extraction()` reports the **mean CIE Delta-E 2000**
between predicted and golden colours over matched horizons. ΔE
benchmarks (CIE 1976 perceptual scale):

| ΔE 2000 | Interpretation               |
|---------|------------------------------|
| < 1     | Imperceptible to most viewers |
| 1 – 2   | Perceptible only on close inspection |
| 2 – 5   | Clearly perceptible          |
| > 5     | Clearly different colours    |

For Munsell, ΔE ~5 is roughly one chroma step at value 4.
