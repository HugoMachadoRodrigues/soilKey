# Chernozem RSG gate (strengthened, WRB 2022 Ch 4, p 111)

WRB-canonical: chernic horizon AND, starting \\= 50 cm below the lower
limit of the mollic horizon and (if a petrocalcic horizon is present)
above it, a layer with protocalcic properties \\= 5 cm thick OR a calcic
horizon AND base saturation \\= 50% from the surface to the protocalcic
/ calcic layer throughout.

## Usage

``` r
chernozem_strict(pedon, min_bs = 50, max_top_cm = 50)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_bs:

  Numeric threshold or option (see Details).

- max_top_cm:

  Numeric threshold or option (see Details).

## Details

v0.3.4 strengthens the previous v0.2 chernozem (which only required
mollic + chernic_color) by adding the protocalcic / calcic gate and the
BS \\= 50% requirement.

Note: the v0.2
[`chernozem()`](https://hugomachadorodrigues.github.io/soilKey/reference/chernozem.md)
diagnostic remains available as a less-strict variant;
`chernozem_strict()` is what the v0.3.4 key.yaml uses for the CH RSG.
