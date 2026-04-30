# Build the canonical Alisol fixture

Synthetic humid-tropical Alisol on weathered shale: argic horizon at Bt1
with high-activity clay (CEC/clay ~ 34) AND high Al saturation (Al sat ~
70%); the canonical "young weathering on a 2:1 clay parent that has not
yet released enough Al into the precipitate-stabilised pool". By
construction:

- [`argic`](https://hugomachadorodrigues.github.io/soilKey/reference/argic.md):
  PASSES on Bt1.

- [`alisol`](https://hugomachadorodrigues.github.io/soilKey/reference/alisol.md):
  PASSES (CEC high, Al sat high).

- [`acrisol`](https://hugomachadorodrigues.github.io/soilKey/reference/acrisol.md),
  [`lixisol`](https://hugomachadorodrigues.github.io/soilKey/reference/lixisol.md),
  [`luvisol`](https://hugomachadorodrigues.github.io/soilKey/reference/luvisol.md):
  FAIL.

## Usage

``` r
make_alisol_canonical()
```

## Value

A
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).
