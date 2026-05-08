# Mawic qualifier (mw): moss-fibre-dominant peat

WRB 2022 Ch 5 (Histosols): "Containing \>= 40% by volume moss fibres in
organic material \>= 40 cm thick within 100 cm."

## Usage

``` r
qual_mawic(pedon)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

## Details

Implementation: any horizon with `fiber_content_unrubbed_pct` \\= 40 AND
`layer_origin` matches "moss" pattern, OR fall back to `histic_horizon`
OK + fibre threshold (the moss- specific test is over-permissive without
explicit moss flag).
