# Annotate KSSL/NASIS pedons with a derived WRB Reference Soil Group

Applies
[`usda_to_wrb_rsg`](https://hugomachadorodrigues.github.io/soilKey/reference/usda_to_wrb_rsg.md)
to each pedon's USDA classification (preserved as
`site$reference_usda` + `site$reference_usda_suborder` by
[`load_kssl_pedons_gpkg`](https://hugomachadorodrigues.github.io/soilKey/reference/load_kssl_pedons_gpkg.md))
and writes the result to `site$reference_wrb_from_usda` – a "best-guess"
expected WRB label for benchmark comparison.

## Usage

``` r
annotate_wrb_from_usda(pedons)
```

## Arguments

- pedons:

  List of
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
  objects.

## Value

The same list, with `site$reference_wrb_from_usda` populated where USDA
classification is present.

## Details

Pedons that already have `site$reference_wrb` populated (e.g.\\ from
external sources) are left untouched.
