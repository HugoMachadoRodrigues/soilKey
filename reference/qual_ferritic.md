# Ferritic qualifier (fr): high free-Fe in fine earth

WRB 2022 Ch 5 (Nitisols / Ferralsols): "Containing layers with \>= 18%
Fe2O3 (or 12.6% Fe) in fine earth, averaged over upper 100 cm or to a
contact / petroplinthic / pisoplinthic / R."

## Usage

``` r
qual_ferritic(pedon)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

## Details

Implementation: weighted mean of `fe_dcb_pct` (DCB-extractable Fe2O3,
the canonical Fe-pool for Ferralic / Nitic chemistry) over the upper 100
cm.
