# Oxic horizon (USDA Soil Taxonomy)

v0.9.420: the oxic horizon of Keys to Soil Taxonomy (13th ed., 2022, Ch.
3, p. 28), in place of the WRB ferralic horizon it delegated to. A
subsurface horizon of adjoining layers, 30 cm or more thick, in which
every layer:

- has a texture class of sandy loam or finer;

- has an apparent CEC of 16 cmol(+) or less per kg clay (NH4OAc, pH 7),
  the clay being the measured clay or 3 x (water at 1500 kPa - organic
  carbon), whichever is higher, the estimate only where the water is
  measured by mass (audit reading L25);

- has less than 10% weatherable minerals in the 0.05-0.2 mm fraction
  (`weatherable_minerals_pct`; never recorded in most data, and then
  treated as met, as the evidence says: audit reading L21);

- has rock structure in less than 5% of its volume
  (`rock_structure_pct`, a field observation: absent when not recorded);

- has no andic soil properties (a layer that
  `andic_soil_properties_usda()` finds andic is not oxic);

and whose upper boundary has a clay increase from the horizon above of
less than 3% (absolute), 20% (relative) or 8% (absolute) (by the clay of
the horizon above), unless the boundary is diffuse (15 cm or more). The
layer at the mineral soil surface is not part of it. Values are rounded
to the precision of the limit (endnote 1, p. 70).

## Usage

``` r
oxic_usda(pedon, ...)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- ...:

  Ignored (kept for the earlier signature).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md)
(`name = "oxic_usda"`).

## References

Soil Survey Staff (2022). *Keys to Soil Taxonomy*, 13th edition.
USDA-NRCS. Chapter 3, oxic horizon (p. 28).
