# SoilGrids -\> WRB code lookup table

Maps the integer raster values used by the SoilGrids 2.0 "MostProbable
WRB" layer to soilKey's two-letter RSG codes (the codes used in
`inst/rules/wrb2022/key.yaml`).

## Usage

``` r
soilgrids_wrb_lut()
```

## Value

Named character vector: names are integer-as-character (`"1"`, `"2"`,
...), values are RSG codes.

## Details

The numeric values follow the order used by ISRIC; users with a
different convention can override this via the `lut` argument to
[`spatial_prior_soilgrids`](https://hugomachadorodrigues.github.io/soilKey/reference/spatial_prior_soilgrids.md).
