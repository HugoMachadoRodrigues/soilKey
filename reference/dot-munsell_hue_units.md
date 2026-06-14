# Munsell hue as a continuous position on the red-to-yellow scale.

Maps a soil Munsell hue name (e.g. "7.5YR") to a numeric position in
Munsell hue units, increasing toward yellow: 2.5R = 0, 5R = 2.5, ...,
10R = 7.5, 2.5YR = 10, ..., 10YR = 17.5, 2.5Y = 20, ..., 10Y = 27.5.
Adjacent hue pages differ by 2.5 units. Returns `NA` for neutral (N) or
unparseable hues. A LOWER value is redder; a HIGHER value is yellower.
Used to evaluate "\\\ge\\ 2.5 units redder/yellower" in WRB 2022
cambic-horizon criterion 3.

## Usage

``` r
.munsell_hue_units(hue)
```

## Arguments

- hue:

  Character Munsell hue (single value or vector).
