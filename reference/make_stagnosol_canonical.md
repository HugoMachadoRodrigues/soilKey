# Build the canonical Stagnosol fixture

Synthetic Stagnosol: redoximorphic features in a perched layer (Bg,
15-50 cm; redox 25%) but the deeper subsoil is well-drained (BC redox
2%, C redox 0). Since v0.9.312 what tells stagnic from gleyic is the
recorded perched water table and the Bg's matrix, which has no
reductimorphic colour (WRB 2022 3.2.6 and 3.2.14, as Hugo reads them):
[`stagnic_properties`](https://hugomachadorodrigues.github.io/soilKey/reference/stagnic_properties.md)
passes,
[`gleyic_properties`](https://hugomachadorodrigues.github.io/soilKey/reference/gleyic_properties.md)
does not, and the grey Bg covers more than one third of 0-60 cm (the
Stagnosols key, Ch 4, p. 108).

## Usage

``` r
make_stagnosol_canonical()
```

## Value

A
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).
