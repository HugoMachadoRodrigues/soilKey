# Build the canonical Phaeozem fixture

Synthetic humid-temperate Phaeozem on non-calcareous loess: mollic
(chroma 2, value 2-3) and high BS, but no secondary carbonates anywhere
– typical of more leached / less-arid steppe-forest transition. By
construction:

- [`mollic`](https://hugomachadorodrigues.github.io/soilKey/reference/mollic.md):
  PASSES.

- [`phaeozem`](https://hugomachadorodrigues.github.io/soilKey/reference/phaeozem.md):
  PASSES.

- [`chernozem`](https://hugomachadorodrigues.github.io/soilKey/reference/chernozem.md),
  [`kastanozem`](https://hugomachadorodrigues.github.io/soilKey/reference/kastanozem.md):
  FAIL (no carbonates).

## Usage

``` r
make_phaeozem_canonical()
```

## Value

A
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).
