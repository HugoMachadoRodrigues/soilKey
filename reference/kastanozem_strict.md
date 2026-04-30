# Kastanozem RSG gate (strengthened, WRB 2022 Ch 4, p 112)

Same structure as
[`chernozem_strict`](https://hugomachadorodrigues.github.io/soilKey/reference/chernozem_strict.md)
but using the mollic horizon (no chernic gate) and starting \\= 70 cm of
mineral soil surface.

## Usage

``` r
kastanozem_strict(pedon, min_bs = 50, max_top_cm = 70)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_bs:

  Numeric threshold or option (see Details).

- max_top_cm:

  Numeric threshold or option (see Details).
