# Build the canonical Lixisol fixture

Synthetic Mediterranean / sub-tropical Lixisol on weathered calcareous
parent material: argic horizon at Bt1 with low-activity clay (CEC/clay ~
20) but high base saturation (BS ~ 70%) thanks to carbonate-buffered
weathering. By construction:

- [`argic`](https://hugomachadorodrigues.github.io/soilKey/reference/argic.md):
  PASSES on Bt1.

- [`lixisol`](https://hugomachadorodrigues.github.io/soilKey/reference/lixisol.md):
  PASSES (CEC low, BS high).

- [`acrisol`](https://hugomachadorodrigues.github.io/soilKey/reference/acrisol.md),
  [`alisol`](https://hugomachadorodrigues.github.io/soilKey/reference/alisol.md),
  [`luvisol`](https://hugomachadorodrigues.github.io/soilKey/reference/luvisol.md):
  FAIL.

## Usage

``` r
make_lixisol_canonical()
```

## Value

A
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).
