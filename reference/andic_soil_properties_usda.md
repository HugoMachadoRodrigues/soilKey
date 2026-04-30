# Andic soil properties (USDA, KST 13ed Ch 3, p 32)

Soil materials with one or both of the following:

- bulk_density \<= 0.90 g/cm3 AND Al + 0.5\*Fe (oxalate) \>= 2.0% AND
  phosphate_retention \>= 85%; OR

- Al + 0.5\*Fe (oxalate) \>= 0.4% AND phosphate_retention \>= 25% AND
  volcanic_glass_pct varying with the texture-class proxy (deferred –
  requires fine-earth fraction analysis).

## Usage

``` r
andic_soil_properties_usda(pedon)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## Details

Implementation (v0.8.6): primary "humic-andic" branch (bd \<= 0.9 +
Al+0.5Fe \>= 2 + Pret \>= 85). The vitric-andic branch (lower Al+Fe but
high glass content) is partially captured.

## References

Soil Survey Staff (2022), KST 13ed, Ch. 3, p 32.
