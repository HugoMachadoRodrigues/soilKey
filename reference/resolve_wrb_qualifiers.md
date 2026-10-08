# Resolve WRB 2022 qualifiers for a Reference Soil Group

Walks the RSG's lists of principal and supplementary qualifiers (WRB
2022 Chapter 4, in `inst/rules/wrb2022/qualifiers.yaml`) and tests each
qualifier against the pedon, following the rules for naming soils of
Chapter 2.2: in a slash group (`"Rhodic/Xanthic"`) only the first
qualifier that applies is used; a qualifier made redundant by another is
left out (Eutric when Calcaric or Dolomitic applies); Haplic applies
only where the RSG lists it and no other principal qualifier does.

## Usage

``` r
resolve_wrb_qualifiers(pedon, rsg_code, rules = NULL, specifiers = FALSE)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- rsg_code:

  Two-letter RSG code (e.g. `"FR"` for Ferralsols).

- rules:

  Optional pre-loaded rules list (saves I/O when many RSGs are tested).

- specifiers:

  If `TRUE`, auto-attach WRB Ch 5 depth specifiers
  (Epi-/Endo-/Bathy-/Amphi-/Panto-/Kato-) to depth-anchored qualifiers
  based on the feature's actual depth. Default `FALSE` leaves names
  byte-identical to earlier versions.

## Value

A list with `principal` and `supplementary` (character vectors, in the
order they are written in the name), `trace` and `trace_supplementary`.

## Details

Both vectors come back in the order of the name. Principal qualifiers
are written right to left, "the uppermost qualifier in the list is
placed closest to the name of the RSG", so `principal` reads the matched
list backwards. Supplementary qualifiers open with the texture
qualifiers (top to bottom of the profile when specifiers make several
apply), then follow the alphabetical order of the qualifier, not of the
subqualifier. Until v0.9.217 both kept the order of the old lists.
