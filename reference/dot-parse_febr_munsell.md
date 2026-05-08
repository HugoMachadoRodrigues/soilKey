# Parse a single Brazilian-style Munsell color string into hue/value/chroma

Handles the FEBR / SiBCS-canonical `"<matiz> <valor>/<croma>"` format
with PT-BR comma-decimal in any numeric component (e.g. `"2,5YR 3/6"`
-\> hue `"2.5YR"`, value 3, chroma 6; `"10YR 5,5/3,5"` -\> hue `"10YR"`,
value 5.5, chroma 3.5).

## Usage

``` r
.parse_febr_munsell(s)
```

## Details

Returns `c(hue = NA_character_, value = NA_real_, chroma = NA_real_)`
when the input is empty / unparseable.
