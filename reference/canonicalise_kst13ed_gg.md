# Canonicalise a USDA Great Group label to a KST 13ed-compatible key

Maps both obsolete (pre-KST 13ed) and modern Great Group names to a
single canonical key, so that direct equality between predicted and
reference Great Group names ignores edition-driven renaming. Names that
have no known mapping pass through unchanged.

## Usage

``` r
canonicalise_kst13ed_gg(gg)
```

## Arguments

- gg:

  Character vector of Great Group names (lower case, no whitespace).

## Value

Character vector of canonical keys. Unmapped names pass through. NA
stays NA. Empty input returns empty vector.

## Details

Examples of the canonicalisation (each pair is rendered equivalent):

- `"haplaquolls"` (KST 8) === `"endoaquolls"` (KST 13ed)

- `"pellusterts"` (KST 8) === `"hapluderts"` (KST 13ed)

- `"camborthids"` (KST 8) === `"haplocambids"` (KST 13ed)

- `"vitrandepts"` (KST 8) === `"vitrudands"` (KST 13ed)

## References

Soil Survey Staff (2022), Keys to Soil Taxonomy 13ed, Ch 4 (Order keys);
previous editions for the obsolete names.
