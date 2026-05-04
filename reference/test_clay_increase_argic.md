# Test the argic / argillic clay-increase criterion

Tests every horizon in the profile against the clay-increase rules of
either WRB 2022 (default, `system = "wrb2022"`) or USDA Soil Taxonomy
13th edition (`system = "usda"`). The two systems use the SAME
structural rule (three brackets keyed on overlying eluvial clay percent)
but DIFFERENT thresholds:

## Usage

``` r
test_clay_increase_argic(h, system = c("wrb2022", "usda"))
```

## Arguments

- h:

  Horizons data.table (canonical schema).

- system:

  One of `"wrb2022"` (default) or `"usda"`. Selects the threshold set.

## Value

Sub-test result list.

## Details

|                  |                        |                        |
|------------------|------------------------|------------------------|
| **Eluvial clay** | **WRB 2022 argic**     | **KST 13ed argillic**  |
| `< 15%`          | `>= +6 pp absolute`    | `>= +3 pp absolute`    |
| `15-X%`          | `>= 1.4x ratio` (X=50) | `>= 1.2x ratio` (X=40) |
| `>= X%`          | `>= +20 pp absolute`   | `>= +8 pp absolute`    |

KST 13ed thresholds are taken from Chapter 3, "Argillic horizon" (p. 4);
WRB 2022 thresholds from Chapter 3.1.3, "Argic horizon" (p. 36). v0.9.26
introduces the per-system switch – earlier versions used WRB thresholds
for both systems, which under-detected the argillic horizon in KSSL
profiles where clay increase is in the 1.2-1.4 ratio band or +3 to +6 pp
absolute band.

Returns the indices of horizons that satisfy as argic candidates.

## References

IUSS Working Group WRB (2022), Chapter 3.1.3, Argic horizon, criteria
2.a.iv-vi (p. 36); Soil Survey Staff (2022), Keys to Soil Taxonomy 13th
ed., Chapter 3, Argillic horizon (p. 4).
