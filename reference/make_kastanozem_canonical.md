# Build the canonical Kastanozem fixture

Synthetic continental-semiarid Kastanozem on loess-like substrate:
mollic surface (chroma 3, value 3) – dark enough for mollic but not dark
enough for Chernozem (chroma 3 \> 2 in the upper 20 cm); secondary
carbonates accumulating in the Bk. By construction:

- [`mollic`](https://hugomachadorodrigues.github.io/soilKey/reference/mollic.md):
  PASSES.

- [`kastanozem`](https://hugomachadorodrigues.github.io/soilKey/reference/kastanozem.md):
  PASSES.

- [`chernozem`](https://hugomachadorodrigues.github.io/soilKey/reference/chernozem.md),
  [`phaeozem`](https://hugomachadorodrigues.github.io/soilKey/reference/phaeozem.md):
  FAIL.

## Usage

``` r
make_kastanozem_canonical()
```

## Value

A
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).
