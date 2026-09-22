# Build the canonical Stagnosol fixture

Synthetic Stagnosol: redoximorphic features in a perched layer (Bg,
15-50 cm; redox 25%) but the deeper subsoil is well-drained (BC redox
2%, C redox 0). The decay-with-depth contrast is what distinguishes
stagnic from gleyic. By construction
[`stagnic_properties`](https://hugomachadorodrigues.github.io/soilKey/reference/stagnic_properties.md)
passes and
[`gleyic_properties`](https://hugomachadorodrigues.github.io/soilKey/reference/gleyic_properties.md)
also passes (the surface redox qualifies for both); the WRB key tests
Stagnosols (#16) and Gleysols (#9), so a real Stagnosol-typed fixture
lands at Gleysols if both pass – the criteria differ in depth pattern,
which is enough for the diagnostic functions but not for key precedence
in v0.3. This is documented in the test as known overlap; v0.4 will add
a stronger discriminator.

## Usage

``` r
make_stagnosol_canonical()
```

## Value

A
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).
