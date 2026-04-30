# Melanic epipedon (USDA Soil Taxonomy, 13th edition)

A thick, very dark, andic, organic-rich surface horizon associated with
volcanic-ash-derived soils in cool, humid environments. Diagnostic for
the Melanists / Melanudands great groups of Andisols.

## Usage

``` r
melanic_epipedon_usda(pedon)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## Details

KST 13ed required characteristics (Ch. 3, pp 15-16):

- Upper boundary at or within 30 cm of the mineral soil surface (or
  organic layer with andic properties);

- Cumulative thickness \>= 30 cm within 40 cm with all of:

  - Andic soil properties throughout;

  - Color value \<= 2.5 moist AND chroma \<= 2 throughout;

  - Melanic index \<= 1.70 (deferred – specialized lab measurement);

  - OC \>= 6 percent (weighted) AND \>= 4 percent (each layer).

Implementation notes (v0.8.x):

- Andic soil properties are tested via `andic_properties_usda` (v0.9;
  for v0.8 we approximate with bulk_density \<= 0.9 g/cm3 AND
  phosphate_retention \>= 85%).

- Melanic index (= 100 / (OC \* 100 + 1) per KST appendix) is deferred –
  requires UV-Vis spectroscopy.

## References

Soil Survey Staff (2022), KST 13ed, Ch. 3, pp 15-16.
