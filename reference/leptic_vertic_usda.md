# Leptic Subgroup helper (Vertisols, KST 13ed Ch 16)

The Vertisol *Leptic* subgroups have a densic, lithic, or paralithic
contact, a duripan, or a petrocalcic horizon within `max_top_cm` of the
surface. Detection is deliberately conservative (under-fire before
over-fire): a root-restricting designation (`R`/`Cr`/`Cd`), a duripan
(`duripan_pct > 0` or strongly/indurated cementation), or a petrocalcic
horizon (strongly/indurated cementation with CaCO3 \>= 15%). This is the
USDA “shallow contact” sense of *leptic*, distinct from both the WRB
coarse-fragment `leptic` and the gypsic/soluble-salt *Leptic* of certain
Gypsids and Natr- great groups.

## Usage

``` r
leptic_vertic_usda(pedon, max_top_cm = 100)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- max_top_cm:

  Depth window in cm (default 100).
